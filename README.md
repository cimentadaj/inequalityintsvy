# Economic inequality indicators for all available PISA waves (2000 to 2015 every three years)

This package contains a a dataset with 9 economic inequality indicators for over 30 countries for all available PISA waves.
It is taken from the OECD website. For years where no data was available, the closest year available was taken.
For more detail please check out the 'generating_database.R' script from the Github repository.

The indicators available in the dataset are:
Format

GINI- Gini (disposable income, post taxes and transfers)

GINIB - Gini (market income, before taxes and transfers)

GINIG - Gini (gross income, before taxes)

PALMA- Palma ratio

P90P10 - P90/P10 disposable income decile ratio

P90P50 - P90/P50 disposable income decile ratio

P50P10 - P50/P10 disposable income decile ratio

S80S20 - S80/S20 disposable income quintile share

S90S10 - 	S90/S10 disposable income decile share

The data comes from: https://stats.oecd.org/Index.aspx?DataSetCode=IDD

You can install the package with

```{r}
devtools::install_github("cimentadaj/inequalityintsvy")
```