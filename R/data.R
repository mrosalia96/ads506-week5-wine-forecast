#' Load Australian wine sales and return a monthly tsibble keyed by `Varietal`.
load_wine_tsibble <- function(path = "data/AustralianWines.csv") {
  if (!file.exists(path) && file.exists(file.path("..", path))) {
    path <- file.path("..", path)
  }

  raw <- read.csv(
    path,
    check.names = FALSE,
    na.strings = c("*", "", "NA"),
    stringsAsFactors = FALSE
  )

  raw |>
    tidyr::pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
    dplyr::mutate(
      Varietal = trimws(Varietal),
      Varietal = dplyr::if_else(
        tolower(Varietal) == "sparkling",
        "Sparkling",
        tools::toTitleCase(tolower(Varietal))
      ),
      Month = tsibble::yearmonth(lubridate::parse_date_time(Month, "b-y")),
      Sales = as.numeric(Sales)
    ) |>
    dplyr::arrange(Varietal, Month) |>
    dplyr::group_by(Varietal) |>
    dplyr::mutate(
      Sales = if (any(is.na(Sales))) {
        stats::approx(
          x = as.numeric(Month),
          y = Sales,
          xout = as.numeric(Month),
          rule = 2
        )$y
      } else {
        Sales
      }
    ) |>
    dplyr::ungroup() |>
    tsibble::as_tsibble(index = Month, key = Varietal)
}

format_accuracy_tbl <- function(acc) {
  acc |>
    dplyr::select(Varietal, .model, Window, RMSE, MAE, MAPE) |>
    dplyr::mutate(
      dplyr::across(c(RMSE, MAE), ~ round(.x, 1)),
      MAPE = round(MAPE, 2)
    )
}

model_spec_tbl <- function(fit) {
  model_cols <- setdiff(names(fit), "Varietal")
  rows <- vector("list", nrow(fit) * length(model_cols))
  idx <- 0L

  for (col in model_cols) {
    for (i in seq_len(nrow(fit))) {
      mdl <- fit[[col]][[i]]
      if (inherits(mdl, "NULL model")) {
        next
      }
      idx <- idx + 1L
      rows[[idx]] <- tibble::tibble(
        Varietal = fit$Varietal[i],
        .model = col,
        Specification = fabletools::model_sum(mdl)
      )
    }
  }

  dplyr::bind_rows(rows)
}

fit_wine_models <- function(train, use_log = FALSE) {
  target <- if (use_log) rlang::expr(log(Sales)) else rlang::expr(Sales)

  fabletools::model(
    train,
    TSLM = fable::TSLM(!!target ~ trend() + season()),
    ETS = fable::ETS(!!target),
    ARIMA = fable::ARIMA(!!target, stepwise = TRUE, approximation = TRUE)
  )
}