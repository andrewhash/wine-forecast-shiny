# Australian Wine Forecast Explorer (ADS 506, Module 5)

Shiny app comparing TSLM, ETS and ARIMA forecasts for Australian wine sales by varietal.

## Run locally
1. Put `AustralianWines.csv` in `data/`.
2. In R: `install.packages(c("shiny", "bslib", "fpp3"))`
3. Open `app.R` in RStudio and click **Run App**.

## Deploy (shinyapps.io)
```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")  # from shinyapps.io > Account > Tokens
rsconnect::deployApp(appFiles = c("app.R", "about.md", "data/AustralianWines.csv"),
                     appName = "wine-forecast")
```

## Reproducibility
`renv::init()` then `renv::snapshot()` to create `renv.lock`.

