## In this script I donwload several income inequality indicators for all available years
## from 2000 to 2015. I do this in order to have income inequality indicators for each PISA wave,
## that is, seq(2000, 2015, 3).

# Whenever there's no information for the PISA year, I take the closest year available.

library(tidyverse)
library(readr)

## Download the data from https://stats.oecd.org/Index.aspx?DataSetCode=IDD.
## click on Export -> Text File (CSV) - > Download
##### All income inequality indicators
all_ineq_indicators <-
  readr::read_csv("./data/IDD_26042017182325929.csv")

# stats::setNames(unique(all_ineq_indicators$Measure), unique(all_ineq_indicators$MEASURE))
# The above code gives you all the measured available with its code.
indicators <- c("GINI", "GINIB", "GINIG", "PALMA", paste0("P90P", c("10", "50")),
                "P50P10", "S80S20", "S90S10")

income_inequality <-
  all_ineq_indicators %>%
  dplyr::select(Country, Year, AGE, Value, MEASURE, Measure, Unit, METHODO) %>%
  dplyr::mutate_if(is_integer, as.character) %>%
  dplyr::filter(MEASURE %in% indicators)

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

country_names <- unique(income_inequality$Country)
inequality_indicator <- unique(income_inequality$MEASURE)[-3]
# Excluding GINIG which is not found in the data

# Loop through each country, and then through each inequality indicator and apply the function from above.
# This will return a list for each country containing all inequality indicators.
all_indicators <-
  purrr::map(country_names, function(country_name) {
    purrr::map(inequality_indicator, function(inequality_indicator) {

      specific_country <- with(income_inequality,
                               income_inequality[MEASURE == inequality_indicator &
                                                   Country == country_name &
                                                   METHODO == "METH2011" &
                                                   AGE == "TOT", c("Country", "Year", "Value")])

      vector_to_fill_from <- with(specific_country, stats::setNames(Value, Year))

      vector_filler(vector_to_fill, vector_to_fill_from)

    })
  }) %>%
  stats::setNames(country_names)

# Take that list, for each country, cbind it and the data frame it.
# Finally, row bind all of those data frames.
reduce_df <-
  all_indicators %>%
  purrr::map(~ reduce(.x, function(x, y) as.data.frame(cbind(x, y)))) %>%
  purrr::reduce(rbind)

colnames(reduce_df) <- inequality_indicator
# Data base sort of ready

inequality_data <-
  reduce_df %>%
  tibble::as_tibble() %>%
  dplyr::mutate(year = as.character(rep(vector_to_fill, length(country_names))),
         country = rep(country_names, each = length(vector_to_fill))) %>%
  tidyr::gather(indicators, value, GINI:S90S10)

save(inequality_data, file = "./data/inequality_data.rda")

# Everything ready!
