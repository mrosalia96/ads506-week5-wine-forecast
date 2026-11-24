# ADS 506 Week 5 — Australian Wine Forecast (Shiny)

Shiny app comparing **TSLM**, **ETS**, and **ARIMA** on `data/AustralianWines.csv`.

## Run

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "lubridate", "tsibble",
  "fable", "fabletools", "feasts", "ggplot2", "scales", "DT", "purrr"
))
shiny::runApp()  # run from this directory
```

## LLM use

UI/deploy boilerplate drafted with **Cursor**; forecasting follows the Module 5 `fable` workflow.