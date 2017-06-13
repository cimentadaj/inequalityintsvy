library(tidyverse)
library(readr)

## Download the data from https://stats.oecd.org/Index.aspx?DataSetCode=IDD.
## click on Export -> Text File (CSV) - > Download
##### All income inequality indicators
all_gender_indicators <-
  readr::read_csv("./data-raw/raw_gender_employment.csv")

# stats::setNames(unique(all_ineq_indicators$Measure), unique(all_ineq_indicators$MEASURE))
# The above code gives you all the measured available with its code.
indicators <- paste0("EMP", c(1:3, 5:6, 8))

# EMP1
# Labour force participation rate

# EMP2
# Employment/population ratio

# EMP3
# Unemployment rate

# EMP5
# Share of employed in part-time employment

# EMP6
# Share of employed in involuntary part-time employment

# EMP8
# Share of employed in temporary employment

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

# Loop through each country, and then through each inequality indicator and apply the function from above.
# This will return a list for each country containing all inequality indicators.
all_indicators <-
  purrr::map(country_names, function(country_name) {
    purrr::map(unique_gender_indicator, function(gender_ind) {

      specific_country <- with(gender_indicators,
                               gender_indicators[IND == gender_ind &
                                                 Country == country_name,
                                                 c("Country", "Year", "Value")])

      vector_to_fill_from <- with(specific_country, stats::setNames(Value, Year))

      vector_filler(vector_to_fill, vector_to_fill_from)

    })
  }) %>%
  stats::setNames(country_names)

# Take that list, for each country, cbind it and the data frame it.
# Finally, row bind all of those data frames.
# You left off here! Something is wrong with the vector_filler function because it's picking some values twice
# Check running lines 127 and 128
reduce_df <-
  all_indicators %>%
  purrr::map(~ tibble(.x)) %>%
  purrr::reduce(rbind)

colnames(reduce_df) <- unique_gender_indicator
# Data base sort of ready

gender_indicators <-
  reduce_df %>%
  tibble::as_tibble() %>%
  dplyr::mutate(year = as.character(rep(vector_to_fill, length(country_names))),
                country = rep(country_names, each = length(vector_to_fill))) %>%
  tidyr::gather(indicators, value, GINI:S90S10)

write_csv(all_ineq_indicators, "./data-raw/raw_gender_indicators.csv")
devtools::use_data(gender_indicators, overwrite = TRUE)
