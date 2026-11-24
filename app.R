library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(ggplot2)
library(scales)
library(DT)

source("R/data.R", local = TRUE)

wine_full <- load_wine_tsibble()
varietal_choices <- sort(unique(wine_full$Varietal))
global_min <- min(wine_full$Month)
global_max <- max(wine_full$Month)
default_train_end <- as.Date(lubridate::ymd("1992-12-01"))

line_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#6c1d45",
  base_font = font_google("Source Sans 3")
)

ui <- page_navbar(
  title = "Australian Wine Forecast Studio",
  theme = line_theme,
  id = "tabs",
  sidebar = sidebar(
    width = 320,
    selectizeInput(
      "varietals",
      "Varietal(s)",
      choices = varietal_choices,
      selected = c("Sparkling", "Fortified"),
      multiple = TRUE,
      options = list(placeholder = "Choose up to three varietals")
    ),
    dateRangeInput(
      "date_range",
      "History window",
      start = as.Date(global_min),
      end = as.Date(global_max),
      min = as.Date(global_min),
      max = as.Date(global_max),
      format = "M yyyy",
      separator = " – "
    ),
    dateInput(
      "train_end",
      "Training ends (inclusive)",
      value = as.Date(default_train_end),
      min = as.Date(global_min),
      max = as.Date(global_max) - months(12)
    ),
    numericInput(
      "horizon",
      "Forecast horizon (months)",
      value = 12,
      min = 1,
      max = 36,
      step = 1
    ),
    checkboxInput("use_log", "Model log(Sales)", value = FALSE),
    checkboxGroupInput(
      "models",
      "Models to compare",
      choices = c("TSLM", "ETS", "ARIMA"),
      selected = c("TSLM", "ETS", "ARIMA")
    ),
    actionButton("run_models", "Update forecasts", class = "btn-primary w-100"),
    tags$hr(),
    tags$p(
      class = "text-muted small mb-0",
      "Training window ends on the date above; later months in the history window are used for validation."
    )
  ),
  nav_panel(
    "Overview",
    layout_columns(
      col_widths = c(12, 12),
      card(
        card_header("Monthly sales"),
        card_body(plotOutput("overview_plot", height = "360px"))
      ),
      card(
        card_header("Seasonal decomposition (training data)"),
        card_body(plotOutput("decomp_plot", height = "360px"))
      )
    )
  ),
  nav_panel(
    "Forecasts",
    layout_columns(
      col_widths = c(12, 6, 6),
      card(
        card_header("Comparative forecasts with 80% & 95% intervals"),
        card_body(plotOutput("forecast_plot", height = "420px"))
      ),
      card(
        card_header("Validation accuracy (RMSE, MAE, MAPE)"),
        card_body(DTOutput("accuracy_tbl"))
      ),
      card(
        card_header("Model specifications"),
        card_body(DTOutput("spec_tbl"))
      )
    )
  ),
  nav_panel(
    "About",
    card(
      card_body(
        tags$h4("Data"),
        tags$p(
          "Monthly Australian domestic wine sales by variety, Jan 1980 – Dec 1994 ",
          tags$code("AustralianWines.csv"),
          " (fable package)."
        ),
        tags$h4("Modeling workflow"),
        tags$ul(
          tags$li("Convert to a monthly tsibble keyed by ", tags$code("Varietal")),
          tags$li("Fit TSLM, ETS, and ARIMA on the training window with ", tags$code("fable::model()")),
          tags$li("Score training vs validation with ", tags$code("fabletools::accuracy()")),
          tags$li("Optional log transform refits all three models on ", tags$code("log(Sales)"))
        ),
        tags$h4("LLM assistance"),
        tags$p(
          "Boilerplate for the Shiny layout, Docker packaging, and Quarto narrative was drafted with ",
          "Cursor (Composer). Model logic follows the Module 5 suggested workflow and ",
          tags$code("fable"),
          " documentation; I reviewed and can explain every modeling step."
        ),
        tags$h4("Reproduce the default story"),
        tags$ul(
          tags$li("Varietals: Sparkling, Fortified"),
          tags$li("Training ends: Dec 1992"),
          tags$li("Horizon: 12 months"),
          tags$li("Models: TSLM, ETS, ARIMA (level scale)")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  analysis <- reactiveVal(NULL)

  observeEvent(input$run_models, {
    req(length(input$varietals) > 0, length(input$models) > 0)

    if (length(input$varietals) > 3) {
      showNotification("Please select at most three varietals for responsive loading.", type = "warning")
      return()
    }

    train_end <- tsibble::yearmonth(input$train_end)
    range_start <- tsibble::yearmonth(input$date_range[1])
    range_end <- tsibble::yearmonth(input$date_range[2])

    if (train_end >= range_end) {
      showNotification("Training end must be before the end of the history window.", type = "error")
      return()
    }

    filtered <- wine_full |>
      dplyr::filter(
        Varietal %in% input$varietals,
        Month >= range_start,
        Month <= range_end
      )

    train <- filtered |> dplyr::filter(Month <= train_end)
    validation <- filtered |> dplyr::filter(Month > train_end)

    if (nrow(validation) < 6) {
      showNotification("Leave at least six validation months after the training cutoff.", type = "error")
      return()
    }

    withProgress(message = "Fitting models", value = 0, {
      incProgress(0.2, detail = "TSLM / ETS / ARIMA")
      fit <- fit_wine_models(train, use_log = isTRUE(input$use_log))
      incProgress(0.5, detail = "Forecasting")

      h_val <- validation |>
        dplyr::group_by(Varietal) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::pull(n) |>
        min()

      fc_val <- fabletools::forecast(fit, h = h_val)

      fc_future <- fabletools::forecast(fit, h = input$horizon)

      fc_val <- fc_val |> dplyr::filter(.model %in% input$models)
      fc_future <- fc_future |> dplyr::filter(.model %in% input$models)

      if (isTRUE(input$use_log)) {
        fc_val <- fc_val |> fabletools::response_trans(Sales, inv = exp)
        fc_future <- fc_future |> fabletools::response_trans(Sales, inv = exp)
      }

      acc_train <- fabletools::accuracy(fit) |>
        dplyr::filter(.model %in% input$models) |>
        dplyr::mutate(Window = "Training")

      acc_val <- fabletools::accuracy(fc_val, validation) |>
        dplyr::filter(.model %in% input$models) |>
        dplyr::mutate(Window = "Validation")

      specs <- model_spec_tbl(fit) |>
        dplyr::filter(.model %in% input$models)

      analysis(list(
        train = train,
        validation = validation,
        train_end = train_end,
        fit = fit,
        fc_val = fc_val,
        fc_future = fc_future,
        accuracy = dplyr::bind_rows(acc_train, acc_val),
        specs = specs
      ))
      incProgress(1)
    })
  }, ignoreInit = FALSE)

  output$overview_plot <- renderPlot({
    req(analysis())
    dat <- analysis()$train |>
      dplyr::bind_rows(analysis()$validation) |>
      dplyr::mutate(
        Split = dplyr::if_else(Month <= analysis()$train_end, "Training", "Validation")
      )

    ggplot(dat, aes(x = Month, y = Sales, colour = Split)) +
      geom_line(linewidth = 0.7) +
      geom_vline(
        xintercept = as.numeric(analysis()$train_end) + 0.5,
        linetype = "dashed",
        colour = "grey40"
      ) +
      facet_wrap(~Varietal, scales = "free_y", ncol = 1) +
      scale_y_continuous(labels = comma) +
      scale_colour_manual(values = c(Training = "#0d6efd", Validation = "#dc3545")) +
      labs(
        x = NULL,
        y = "Thousands of litres",
        colour = NULL,
        caption = "Dashed line = training / validation split"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$decomp_plot <- renderPlot({
    req(analysis())
    train <- analysis()$train

    season_comp <- train |>
      fabletools::model(STL = feasts::STL(Sales)) |>
      fabletools::components() |>
      dplyr::filter(.model == "season")

    ggplot(season_comp, aes(x = Month, y = season, colour = Varietal)) +
      geom_line(linewidth = 0.7) +
      labs(x = NULL, y = "Seasonal component", colour = "Varietal") +
      theme_minimal(base_size = 13)
  })

  output$forecast_plot <- renderPlot({
    req(analysis())
    res <- analysis()
    hist <- dplyr::bind_rows(res$train, res$validation)
    fc_all <- dplyr::bind_rows(res$fc_val, res$fc_future)

    p <- fabletools::autoplot(fc_all, hist, level = c(80, 95), alpha = 0.18) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(
        x = NULL,
        y = "Sales (thousands of litres)",
        caption = "Grey history; coloured lines with ribbons = 80% and 95% forecast intervals"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(legend.position = "top")

    if (length(unique(fc_all$Varietal)) > 1) {
      p <- p + ggplot2::facet_wrap(~Varietal, scales = "free_y", ncol = 1)
    }

    p
  })

  output$accuracy_tbl <- renderDT({
    req(analysis())
    format_accuracy_tbl(analysis()$accuracy) |>
      datatable(
        rownames = FALSE,
        options = list(pageLength = 12, dom = "t"),
        caption = "Training = in-sample fit; Validation = holdout after training cutoff"
      )
  })

  output$spec_tbl <- renderDT({
    req(analysis())
    analysis()$specs |>
      datatable(rownames = FALSE, options = list(pageLength = 12, dom = "t"))
  })
}

shinyApp(ui, server)