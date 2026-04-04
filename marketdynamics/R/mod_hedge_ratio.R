#' hedge_ratio UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_hedge_ratio_ui <- function(id) {
  ns <- NS(id)

  energy_markets <- c(
    "WTI Crude (CL)"     = "CL",
    "Brent Crude (BRN)"  = "BRN",
    "Natural Gas (NG)"   = "NG",
    "Heating Oil (HO)"   = "HO",
    "RBOB Gasoline (RB)" = "RB"
  )

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::tagList(bsicons::bs_icon("shield-check"), " Hedge Ratio Dynamics")
    ),
    bslib::card_body(
      # Row 1: data controls
      shiny::fluidRow(
        shiny::column(3,
          shiny::selectInput(ns("energy_market"), "Energy Market",
            choices = energy_markets, selected = "CL")
        ),
        shiny::column(4,
          shiny::dateRangeInput(ns("date_range"), "Date Range",
            start = "2007-01-02", end = Sys.Date(),
            min   = "2007-01-02", max = Sys.Date())
        ),
        shiny::column(3,
          shiny::sliderInput(ns("window"), "Rolling Window (days)",
            min = 21, max = 252, value = 63, step = 1)
        ),
        shiny::column(2,
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      # Row 2: analysis controls
      shiny::fluidRow(
        shiny::column(3,
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'rolling' || input['%s'] == 'scatter'",
              ns("view_type"), ns("view_type")),
            shiny::selectInput(ns("exposure"), "Exposure Series (Y)", choices = NULL)
          )
        ),
        shiny::column(3,
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'rolling' || input['%s'] == 'scatter'",
              ns("view_type"), ns("view_type")),
            shiny::selectInput(ns("hedge"), "Hedge Instrument (X)", choices = NULL)
          )
        ),
        shiny::column(3,
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'rolling' || input['%s'] == 'scatter'",
              ns("view_type"), ns("view_type")),
            shiny::radioButtons(ns("method"), "Method",
              choices  = c("OLS Beta" = "ols", "Min Variance" = "minvar"),
              inline   = TRUE,
              selected = "ols")
          )
        ),
        shiny::column(3,
          shiny::radioButtons(ns("view_type"), "View",
            choices  = c(
              "Rolling HR" = "rolling",
              "Scatter"    = "scatter"
            ),
            inline   = TRUE,
            selected = "rolling")
        )
      ),
      shiny::tags$hr(),
      plotly::plotlyOutput(ns("hr_plot"), height = "calc(100vh - 340px)")
    )
  )
}

#' hedge_ratio Server Function
#'
#' Loads energy + CMT data locally and renders hedge ratio and crack spread charts.
#' Does NOT read from or write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object (unused; kept for API consistency).
#' @return None.
#' @export
mod_hedge_ratio_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    combined_data <- shiny::reactiveVal(NULL)

    # Primary data load: selected market + CMT
    shiny::observeEvent(
      list(input$energy_market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(input$energy_market, input$date_range)

        output$load_status <- shiny::renderUI({
          shiny::tags$small(class = "text-muted", "Loading...")
        })

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        energy <- tryCatch(
          load_energy_data(input$energy_market, start, end),
          error = function(e) NULL
        )
        cmt <- tryCatch(
          load_cmt_data(start_date = start, end_date = end),
          error = function(e) NULL
        )

        combined <- if (!is.null(energy) && !is.null(cmt)) {
          combine_data(energy, cmt)
        } else if (!is.null(energy)) {
          dplyr::mutate(energy, source = "energy")
        } else {
          NULL
        }

        combined_data(combined)

        if (!is.null(combined)) {
          energy_front <- combined |>
            dplyr::filter(.data$source == "energy", .data$contract_num == 1) |>
            dplyr::pull(.data$series) |> unique() |> sort()
          cmt_series <- combined |>
            dplyr::filter(.data$source == "cmt") |>
            dplyr::pull(.data$series) |> unique() |> sort()
          all_series <- c(energy_front, cmt_series)

          shiny::updateSelectInput(session, "exposure",
            choices = all_series, selected = all_series[1])
          shiny::updateSelectInput(session, "hedge",
            choices  = all_series,
            selected = if (length(all_series) > 1) all_series[2] else all_series[1])

          output$load_status <- shiny::renderUI({ NULL })
        } else {
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-danger", "Error loading data")
          })
        }
      }
    )

    aligned_returns <- shiny::reactive({
      shiny::req(combined_data(), input$exposure, input$hedge)
      all_ret <- compute_log_returns(combined_data())

      y_df <- dplyr::filter(all_ret, .data$series == input$exposure) |>
        dplyr::select(date, log_return) |>
        dplyr::rename(y = log_return)
      x_df <- dplyr::filter(all_ret, .data$series == input$hedge) |>
        dplyr::select(date, log_return) |>
        dplyr::rename(x = log_return)

      dplyr::inner_join(y_df, x_df, by = "date") |>
        dplyr::arrange(date) |>
        dplyr::filter(!is.na(x), !is.na(y))
    })

    hedge_ratio <- shiny::reactive({
      shiny::req(aligned_returns(), input$method, input$window)
      ar <- aligned_returns()
      if (input$method == "ols") {
        compute_rolling_beta(ar$y, ar$x, window = input$window)
      } else {
        rho     <- compute_rolling_correlation(ar$y, ar$x, window = input$window)
        sigma_y <- compute_rolling_vol(ar$y, window = input$window)
        sigma_x <- compute_rolling_vol(ar$x, window = input$window)
        rho * (sigma_y / sigma_x)
      }
    })

    market_label <- shiny::reactive({
      labels <- c(CL = "WTI Crude", BRN = "Brent Crude", NG = "Natural Gas",
                  HO = "Heating Oil", RB = "RBOB Gasoline")
      unname(labels[input$energy_market])
    })

    output$hr_plot <- plotly::renderPlotly({
      shiny::req(input$view_type, aligned_returns(), hedge_ratio())

        ar <- aligned_returns()
        hr <- hedge_ratio()
        df <- dplyr::tibble(date = ar$date, hedge_ratio = hr) |>
          dplyr::filter(!is.na(.data$hedge_ratio))

        if (input$view_type == "rolling") {

          avg_line <- mean(tail(df$hedge_ratio, 252), na.rm = TRUE)

          plotly::plot_ly(df,
            x    = ~date,
            y    = ~hedge_ratio,
            type = "scatter",
            mode = "lines",
            line = list(color = "#2c7bb6", width = 1.5),
            name = "Hedge Ratio",
            hovertemplate = "Date: %{x}<br>HR: %{y:.3f}<extra></extra>"
          ) |>
            plotly::add_lines(y = avg_line,
              line       = list(color = "#e74c3c", dash = "dash"),
              name       = "1-yr Avg",
              showlegend = TRUE,
              hoverinfo  = "none"
            ) |>
            plotly::layout(
              title  = paste0(market_label(), " \u2014 Hedge Ratio: ", input$exposure,
                " vs ", input$hedge,
                " (", input$window, "-day ", toupper(input$method), ")"),
              xaxis  = list(title = "Date"),
              yaxis  = list(title = "Hedge Ratio"),
              legend = list(orientation = "h")
            )

        } else {

          scatter_df <- ar |>
            dplyr::mutate(year = as.character(lubridate::year(.data$date)))

          years     <- sort(unique(scatter_df$year))
          pal       <- grDevices::colorRampPalette(
            c("#440154", "#31688e", "#35b779", "#fde725")
          )(length(years))
          color_map  <- setNames(pal, years)
          current_hr <- if (length(na.omit(hr)) > 0) tail(na.omit(hr), 1) else 0
          x_range    <- range(scatter_df$x, na.rm = TRUE)
          line_df    <- dplyr::tibble(x = x_range, y = current_hr * x_range)

          plotly::plot_ly(scatter_df,
            x      = ~x,
            y      = ~y,
            color  = ~year,
            colors = color_map,
            type   = "scatter",
            mode   = "markers",
            marker = list(size = 4, opacity = 0.6),
            hovertemplate = paste0(
              input$hedge, ": %{x:.4f}<br>",
              input$exposure, ": %{y:.4f}<extra></extra>"
            )
          ) |>
            plotly::add_lines(data = line_df, x = ~x, y = ~y, inherit = FALSE,
              line       = list(color = "black", width = 2, dash = "dash"),
              name       = sprintf("HR = %.3f", current_hr),
              showlegend = TRUE
            ) |>
            plotly::layout(
              title  = paste0(market_label(), " \u2014 Return Scatter: ",
                input$exposure, " vs ", input$hedge),
              xaxis  = list(title = paste(input$hedge, "Log Return")),
              yaxis  = list(title = paste(input$exposure, "Log Return")),
              legend = list(orientation = "h")
            )
        }
    })
  })
}
