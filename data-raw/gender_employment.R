library(tidyverse)
library(readr)

## Download the data from https://stats.oecd.org/Index.aspx?DataSetCode=IDD.
## On the rightmost menu click on Gender -> Employment -> Indicators of gender equality in employment
## Click on Export -> Text File (CSV) -> Download

all_gender_indicators <-
  readr::read_csv("./data-raw/raw_gender_employment.csv")

indicators <- paste0("EMP", c(1:3, 5:6, 8))

gender_indicators <-
  all_gender_indicators %>%
  dplyr::select(Country, IND, Sex, `Age Group`, Time, Value) %>%
  dplyr::mutate_if(is_integer, as.character) %>%
  dplyr::filter(IND %in% indicators,
                `Age Group` == "Total",
                Sex != "All persons") %>%
  dplyr::rename(Year = Time)

# These are the PISA waves
vector_to_fill <- seq(2000, 2015, 3)

# Functions that fills.
vector_filler <- function(vector_to_fill, vector_to_fill_from) {

  # If the vector to search for is empty, return NA repeated n times.
  if (purrr::is_empty(vector_to_fill_from)) {
    empty_years <- rep(NA, length(vector_to_fill)) %>%
      stats::setNames(vector_to_fill)

    return(empty_years)
  }

  # Function returns a list with information on which years to use to fill from
  which_inside_which <- function(vector_to_fill, vector_to_fill_from) {

    names_to_fill_from <- names(vector_to_fill_from)

    # Years which are present in both vectors (those we will keep as is)
    already_filled <- vector_to_fill %in% names_to_fill_from

    already_filled_from <- names_to_fill_from %in% vector_to_fill

    # Values we need to actually fill
    yrs_to_subtract <- vector_to_fill[!already_filled]

    # Values available to fill the yrs_to_subtract vector
    yrs_to_loop <- as.numeric(names(vector_to_fill_from))

    list(years_filled = already_filled,
         years_filled_from = already_filled_from,
         years_to_subtract = yrs_to_subtract,
         years_to_loop = yrs_to_loop)
  }

  indexes <- which_inside_which(vector_to_fill, vector_to_fill_from)

  indices_to_match <-
    purrr::map(indexes$years_to_subtract, function(.x) {
      subtraction <- .x - indexes$years_to_loop # subtract each year to be filled from all available years
      abs_subtr <- abs(subtraction)
      abs_subtr == min(abs_subtr) # Check which year is the closest to the year to be filled in
    })

  # Subset the closest year and calculate the mean in case there is more than one value
  new_values <-
    purrr::map_dbl(indices_to_match, ~ mean(vector_to_fill_from[.x])) %>%
    stats::setNames(indexes$years_to_subtract) %>%
    `c`(vector_to_fill_from[indexes$years_filled_from])

  correct_order <-
    names(new_values) %>%
    as.numeric() %>%
    order()

  new_values[correct_order]
}

country_names <- unique(gender_indicators$Country)
unique_gender_indicator <- unique(gender_indicators$IND)

# Loop through each country, and then through each inequality indicator and then through each sex
# and apply the function from above. This will return three nested lists. One for country
# with 6 indicators for each country and then for male and female for each indicators
all_indicators <-
  purrr::map(country_names, function(country_name) {
    purrr::map(unique_gender_indicator, function(gender_ind) {
      purrr::map(c("Men", "Women"), function(actual_gender) {

      specific_country <- with(gender_indicators,
                               gender_indicators[IND == gender_ind &
                                                 Country == country_name &
                                                 Sex == actual_gender,
                                                 c("Country", "Year", "Value")])

      vector_to_fill_from <- with(specific_country, stats::setNames(Value, Year))

      vector_filler(vector_to_fill, vector_to_fill_from)

      })
    })
  }) %>%
  stats::setNames(country_names)


reduce_df <-
  all_indicators %>%
  purrr::map(function(country) {
    purrr::map(country, ~ {
      .x %>%
        setNames(c("male", "female")) %>%
        tibble::enframe() %>%
        tidyr::unnest() %>%
        dplyr::mutate(year = rep(vector_to_fill, 2))
    })
  })

gender_employment <-
  purrr::map(reduce_df, ~ {
  setNames(.x, unique_gender_indicator) %>%
    tibble::enframe(name = "gender_indicator") %>%
    tidyr::unnest()
  }) %>%
  tibble::enframe() %>%
  tidyr::unnest() %>%
  setNames(c("country", "gender_indicator", "gender", "value", "year"))

# Data base ready

readr::write_csv(all_gender_indicators, "./data-raw/raw_gender_employment.csv")
devtools::use_data(gender_employment, overwrite = TRUE)
