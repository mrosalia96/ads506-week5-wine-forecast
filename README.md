# ADS 506 — Week 5 Australian Wine Forecast Shiny App

Interactive comparison of **TSLM**, **ETS**, and **ARIMA** forecasts for Australian wine sales (`AustralianWines.csv`).

## Run locally

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "lubridate", "tsibble",
  "fable", "fabletools", "feasts", "ggplot2", "scales", "DT", "purrr", "ggtime"
))
setwd("path/to/ads506-week5-wine-forecast")
shiny::runApp()
```

## App highlights

- Monthly `tsibble` keyed by `Varietal`
- History window, training cutoff, and forecast horizon controls
- Side-by-side forecasts with **80% and 95%** prediction intervals
- Training vs validation **RMSE / MAE / MAPE** table
- Human-readable model specs (`ETS(M,N,M)`, `ARIMA(p,d,q)(P,D,Q)[12]`, …)
- STL seasonal decomposition tab
- Optional **log(Sales)** transform (re-fits all three models)

## LLM use

Layout, Docker packaging, and Quarto boilerplate were drafted with **Cursor**. Forecasting logic follows the Module 5 suggested workflow (`fable::model()` with TSLM, ETS, ARIMA).

## Deploy (Cloudish)

From this directory:

```bash
tar --exclude='.git' --exclude='context.tar.gz' -czf context.tar.gz .
# Upload via Cloudish API (see course announcement); port 3838, Dockerfile included.
```