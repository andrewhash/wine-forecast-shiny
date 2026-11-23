library(shiny)
library(bslib)
suppressPackageStartupMessages(library(fpp3))
library(markdown)   # needed for includeMarkdown() on the server
library(urca)       # needed by ARIMA() on the server
library(readr)

# ---- Data: monthly tsibble keyed by Varietal ----
parse_month <- function(x) if (inherits(x, "Date")) x else lubridate::my(x)

wine <- readr::read_csv("data/AustralianWines.csv", na = c("", "NA", "*"), show_col_types = FALSE) |>
  select(where(~ !all(is.na(.x)))) |>
  rename(Month = 1) |>
  pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
  mutate(Varietal = tools::toTitleCase(trimws(Varietal)),   # headers have trailing spaces
         Month = yearmonth(parse_month(Month)),
         Sales = as.numeric(Sales)) |>
  filter(!is.na(Month)) |>
  group_by(Varietal) |>
  tidyr::fill(Sales, .direction = "downup") |>   # a few blank months in Rose
  ungroup() |>
  as_tsibble(index = Month, key = Varietal)

vars  <- sort(unique(wine$Varietal))
d_min <- as.Date(min(wine$Month))
d_max <- as.Date(max(wine$Month))
default_vars <- vars[grepl("sparkling|fortified", vars, ignore.case = TRUE)]

fit_models <- function(d, models) {
  specs <- list(TSLM  = TSLM(Sales ~ trend() + season()),
                ETS   = ETS(Sales),
                ARIMA = ARIMA(Sales))
  do.call(model, c(list(d), specs[models]))
}

# ---- UI ----
ui <- page_sidebar(
  title = "Australian Wine Sales: Forecast Model Comparison",
  sidebar = sidebar(
    width = 300,
    tooltip(selectInput("var", "Varietals", vars, default_vars, multiple = TRUE),
            "Pick one or more wines. Each one gets its own set of models."),
    dateRangeInput("dates", "Date range", d_min, d_max, d_min, d_max,
                   format = "M yyyy", startview = "year"),
    tooltip(sliderInput("train_end", "Training ends", d_min, d_max,
                        as.Date("1992-12-01"), timeFormat = "%b %Y"),
            "Months up to here train the models. Months after are the validation window."),
    numericInput("h", "Forecast horizon (months)", 12, min = 1, max = 36),
    checkboxGroupInput("models", "Models", c("TSLM", "ETS", "ARIMA"),
                       c("TSLM", "ETS", "ARIMA"), inline = TRUE),
    downloadButton("dl", "Download metrics (CSV)")
  ),
  navset_card_tab(
    nav_panel("1. Explore",
              plotOutput("ts_plot", height = 280),
              plotOutput("season_plot", height = 280)),
    nav_panel("2. Validation forecasts",
              card_header(textOutput("best")),
              plotOutput("valid_plot", height = 520)),
    nav_panel("3. Accuracy", tableOutput("acc_tbl")),
    nav_panel("4. Model specs",
              tableOutput("spec_tbl"),
              helpText("ETS(Error, Trend, Season): N = none, A = additive, Ad = damped, M = multiplicative.",
                       "ARIMA(p,d,q)(P,D,Q)[12]: lowercase is the regular part, uppercase is the seasonal part.",
                       "TSLM is always Sales ~ trend() + season().")),
    nav_panel("5. Future forecast", plotOutput("future_plot", height = 520)),
    nav_panel("About", includeMarkdown("about.md"))
  )
)

# ---- Server ----
server <- function(input, output, session) {

  filtered <- reactive({
    req(input$var, input$models, input$dates)
    wine |> filter(Varietal %in% input$var,
                   Month >= yearmonth(input$dates[1]),
                   Month <= yearmonth(input$dates[2]))
  })

  split <- reactive({
    te <- yearmonth(input$train_end)
    d  <- filtered()
    train <- d |> filter(Month <= te)
    valid <- d |> filter(Month > te)
    validate(
      need(nrow(train) / length(input$var) >= 36,
           "Need at least 3 years of training data. Move 'Training ends' later or widen the date range."),
      need(nrow(valid) > 0,
           "'Training ends' has to be before the end of the date range so there's a validation window.")
    )
    list(train = train, valid = valid)
  })

  fit      <- reactive(fit_models(split()$train, input$models))
  fc_valid <- reactive(forecast(fit(), new_data = split()$valid))
  fc_future <- reactive({
    req(input$h >= 1)
    fit_models(filtered(), input$models) |> forecast(h = input$h)
  })

  acc <- reactive({
    bind_rows(accuracy(fit()), accuracy(fc_valid(), filtered())) |>
      mutate(Window = if_else(.type == "Training", "Train", "Validation")) |>
      select(Varietal, Model = .model, Window, RMSE, MAE, MAPE) |>
      arrange(Varietal, desc(Window), RMSE)
  })

  recent <- function(d, yrs = 3) d |> filter(Month > max(Month) - 12 * yrs)

  output$ts_plot <- renderPlot({
    autoplot(filtered(), Sales) +
      geom_vline(xintercept = as.Date(input$train_end), linetype = "dashed") +
      labs(title = "Monthly sales (dashed line = end of training)", y = "Sales", x = NULL)
  })

  output$season_plot <- renderPlot({
    filtered() |> gg_season(Sales) + labs(title = "Seasonal plot: one line per year", y = "Sales", x = NULL)
  })

  output$valid_plot <- renderPlot({
    fc_valid() |>
      autoplot(recent(split()$train), level = 95) +
      geom_line(data = split()$valid, aes(Month, Sales), colour = "black",
                linetype = "dashed", linewidth = 0.4) +
      facet_grid(Varietal ~ .model, scales = "free_y") +
      scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
      theme(panel.spacing.x = unit(1.5, "lines")) +
      labs(title = "Forecasts over the validation window (dashed black = actual sales)",
           y = "Sales", x = NULL, colour = "Model", fill = "Model")
  })

  output$future_plot <- renderPlot({
    fc_future() |>
      autoplot(recent(filtered()), level = c(80, 95)) +
      facet_grid(Varietal ~ .model, scales = "free_y") +
      scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
      theme(panel.spacing.x = unit(1.5, "lines")) +
      labs(title = paste(input$h, "month forecast, refit on all selected data"),
           y = "Sales", x = NULL, colour = "Model", fill = "Model")
  })

  output$best <- renderText({
    acc() |> filter(Window == "Validation") |> group_by(Varietal) |>
      slice_min(RMSE, n = 1) |>
      mutate(t = sprintf("%s: %s (RMSE %.0f)", Varietal, Model, RMSE)) |>
      pull(t) |> paste(collapse = "   |   ") |>
      (\(x) paste("Best on validation RMSE ->", x))()
  })

  output$acc_tbl <- renderTable(acc(), digits = 1, striped = TRUE)

  output$spec_tbl <- renderTable({
    fit() |> as_tibble() |>
      mutate(across(-Varietal, ~ gsub("[<>]", "", format(.x))))
  }, striped = TRUE)

  output$dl <- downloadHandler(
    filename = function() "wine_model_metrics.csv",
    content  = function(file) readr::write_csv(acc(), file)
  )
}

shinyApp(ui, server)
