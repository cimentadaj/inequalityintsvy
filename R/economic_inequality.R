#' Economic inequality indicators for all available PISA waves (2000 to 2015 every three years)
#'
#' This datasets contains 9 economic inequality indicators for over 30 countries
#' for all available PISA waves. It is taken from the OECD website. For years
#' where no data was available, the closest year was taken. For more detail please
#' check out the 'economic_inequality.R' script from the Github repository.
#'
#'
#' @format A tibble with 1,776 rows and 4 columns:
#' \describe{
#'   \item{GINI}{Gini (disposable income, post taxes and transfers)}
#'   \item{GINIB}{Gini (market income, before taxes and transfers)}
#'   \item{GINIG}{Gini (gross income, before taxes)}
#'   \item{PALMA}{Palma ratio}
#'   \item{P90P10}{P90/P10 disposable income decile ratio}
#'   \item{P90P50}{P90/P50 disposable income decile ratio}
#'   \item{P50P10}{P50/P10 disposable income decile ratio}
#'   \item{S80S20}{S80/S20 disposable income quintile share}
#'   \item{S90S10}{S90S10	S90/S10 disposable income decile share}
#' }
#' @source \url{https://stats.oecd.org/Index.aspx?DataSetCode=IDD}
#'
#'
"economic_inequality"
