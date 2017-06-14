#' Gender unemployment inequality indicators for all available PISA waves (2000 to 2015 every three years)
#'
#' This datasets contains 6 gender unemployment inequality indicators for over 30 countries
#' divided by gender for all available PISA waves. It is taken from the OECD website. For years
#' where no data was available, the closest year was taken. Download the data from
#' https://stats.oecd.org/Index.aspx?DataSetCode=IDD. On the rightmost menu click on
#' Gender -> Employment -> Indicators of gender equality in employment. For more detail please
#' check out the 'gender_employment.R' script from the Github repository.
#'
#'
#' @format A tibble with 3,240 rows and 5 columns:
#'
#' \describe{
#'   \item{EMP1}{Labour force participation rate}
#'   \item{EMP2}{Employment/population ratio}
#'   \item{EMP3}{Unemployment rate}
#'   \item{EMP5}{Share of employed in part-time employment}
#'   \item{EMP6}{Share of employed in involuntary part-time employment}
#'   \item{EMP8}{Share of employed in temporary employment}
#' }
#' @source \url{https://stats.oecd.org/Index.aspx?DataSetCode=IDD}
#'
#'
"gender_employment"
