#' volatility UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_volatility_ui <- function(id) {
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
      shiny::tagList(bsicons::bs_icon("activity"), " Volatility Analysis")
    ),
    bslib::card_body(
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
          shiny::selectInput(ns("ticker"), "Series", choices = NULL)
        ),
        shiny::column(2,
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      shiny::fluidRow(
        shiny::column(4,
          shiny::sliderInput(ns("vol_window"), "Rolling Window (days)",
            min = 5, max = 63, value = 21, step = 1)
        ),
        shiny::column(8,
          shiny::radioButtons(ns("view_type"), "View",
            choices  = c(
              "Rolling Vol"     = "rolling",
              "Term Structure"  = "term",
              "Crack Spread Vol" = "crack"
            ),
            inline   = TRUE,
            selected = "rolling")
        )
      ),
      shiny::tags$hr(),
      plotly::plotlyOutput(ns("vol_plot"), height = "calc(100vh - 340px)")
    )
  )
}

#' volatility Server Function
#'
#' Loads energy data locally and renders volatility charts.
#' Does NOT read from or write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object (unused; kept for API consistency).
#' @return None.
#' @export
mod_volatility_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    energy_data  <- shiny::reactiveVal(NULL)
    crack_data   <- shiny::reactiveVal(NULL)   # CL + HO + RB front months

    # Primary market data load
    shiny::observeEvent(
      list(input$energy_market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(input$energy_market, input$date_range)

        output$load_status <- shiny::renderUI({
          shiny::tags$small(class = "text-muted", "Loading...")
        })

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        data <- tryCatch(
          load_energy_data(input$energy_market, start, end),
          error = function(e) {
            output$load_status <- shiny::renderUI({
              shiny::tags$small(class = "text-danger", "Error loading data")
            })
            NULL
          }
        )

        energy_data(data)
        crack_data(NULL)   # reset crack data on market/date change

        if (!is.null(data)) {
          choices <- sort(unique(data$series))
          shiny::updateSelectInput(session, "ticker",
            choices = choices, selected = choices[1])
          output$load_status <- shiny::renderUI({ NULL })
        }
      }
    )

    # Crack data load — triggered when crack view is selected or date/market changes
    shiny::observeEvent(
      list(input$view_type, input$date_range, input$energy_market),
      ignoreNULL = TRUE, ignoreInit = TRUE, {
        shiny::req(input$view_type, input$date_range, input$energy_market)
        if (input$view_type != "crack") return()
        if (!input$energy_market %in% c("CL", "HO", "RB")) return()

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        data <- tryCatch(
          load_energy_data(c("CL", "HO", "RB"), start, end) |>
            dplyr::filter(.data$contract_num == 1),
          error = function(e) NULL
        )
        crack_data(data)
      }
    )

    returns_data <- shiny::reactive({
      shiny::req(energy_data())
      compute_log_returns(energy_data())
    })

    market_label <- shiny::reactive({
      labels <- c(CL = "WTI Crude", BRN = "Brent Crude", NG = "Natural Gas",
                  HO = "Heating Oil", RB = "RBOB Gasoline")
      unname(labels[input$energy_market])
    })

    output$vol_plot <- plotly::renderPlotly({
      shiny::req(returns_data(), input$ticker, input$view_type, input$vol_window)

      if (input$view_type == "rolling") {

        ser <- dplyr::filter(returns_data(), .data$series == input$ticker) |>
          dplyr::arrange(.data$date) |>
          dplyr::mutate(
            rolling_vol = compute_rolling_vol(.data$log_return, window = input$vol_window) * 100
          ) |>
          dplyr::filter(!is.na(.data$rolling_vol))

        q25   <- stats::quantile(ser$rolling_vol, 0.25, na.rm = TRUE)
        q75   <- stats::quantile(ser$rolling_vol, 0.75, na.rm = TRUE)
        y_max <- max(ser$rolling_vol, na.rm = TRUE) * 1.05

        bg_shapes <- list(
          list(
            type      = "rect",
            xref      = "paper", yref = "y",
            x0 = 0, x1 = 1,
            y0 = q75, y1 = y_max,
            fillcolor = "rgba(230,126,34,0.10)",
            line      = list(width = 0)
          )
        )

        plotly::plot_ly(ser,
          x    = ~date,
          y    = ~rolling_vol,
          type = "scatter",
          mode = "lines",
          name = paste0(input$vol_window, "-day rolling vol"),
          line = list(color = "#2c3e50", width = 1.5),
          hovertemplate = "Date: %{x}<br>Vol: %{y:.1f}%<extra></extra>"
        ) |>
          plotly::add_lines(
            y          = q25,
            line       = list(color = "#27ae60", dash = "dot", width = 1.2),
            name       = sprintf("25th pct (%.1f%%)", q25),
            showlegend = TRUE,
            hoverinfo  = "none"
          ) |>
          plotly::add_lines(
            y          = q75,
            line       = list(color = "#e67e22", dash = "dot", width = 1.2),
            name       = sprintf("75th pct (%.1f%%)", q75),
            showlegend = TRUE,
            hoverinfo  = "none"
          ) |>
          plotly::layout(
            title  = paste0(market_label(), " \u2014 Rolling Realized Volatility (",
              input$ticker, ", ", input$vol_window, "-day window)"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Annualized Volatility (%)"),
            shapes = bg_shapes,
            legend = list(orientation = "h", x = 0, y = 1.08),
            annotations = list(list(
              x = 0.01, y = 0.98, xref = "paper", yref = "paper",
              text      = "Orange band = historically high vol regime (above 75th percentile)",
              showarrow = FALSE,
              font      = list(size = 10, color = "grey50"),
              align     = "left"
            ))
          )

      } else if (input$view_type == "term") {

        term <- compute_vol_surface(
          returns_data(),
          unique(returns_data()$series),
          window = input$vol_window
        ) |>
          dplyr::mutate(
            contract_num = as.integer(sub("^[A-Za-z]+", "", .data$series))
          ) |>
          dplyr::filter(!is.na(.data$contract_num)) |>
          dplyr::group_by(.data$contract_num) |>
          dplyr::summarise(
            mean_vol = mean(.data$rolling_vol, na.rm = TRUE) * 100,
            .groups  = "drop"
          ) |>
          dplyr::arrange(.data$contract_num)

        plotly::plot_ly(term,
          x      = ~contract_num,
          y      = ~mean_vol,
          type   = "bar",
          marker = list(color = "#2c7bb6"),
          hovertemplate = "Contract: %{x}<br>Avg Vol: %{y:.1f}%<extra></extra>"
        ) |> plotly::layout(
          title       = paste0(market_label(),
            " \u2014 Term Structure of Volatility (Samuelson Effect)"),
          xaxis       = list(title = "Contract Month"),
          yaxis       = list(title = "Average Annualized Volatility (%)"),
          annotations = list(list(
            x = 0.5, y = 1.05, xref = "paper", yref = "paper",
            text      = "Near-term contracts are typically more volatile (Samuelson Effect)",
            showarrow = FALSE,
            font      = list(size = 11, color = "grey50")
          ))
        )

      } else {
        # Crack Spread Vol — petroleum markets only
        if (!input$energy_market %in% c("CL", "HO", "RB")) {
          return(
            plotly::plot_ly() |>
              plotly::layout(
                title = "Crack Spread Vol is only available for CL, HO, and RB markets",
                xaxis = list(visible = FALSE),
                yaxis = list(visible = FALSE)
              )
          )
        }

        shiny::req(crack_data())
        cd <- crack_data()

        # Build front-month price series for CL, HO, RB
        cl_px <- dplyr::filter(cd, .data$market == "CL") |>
          dplyr::arrange(.data$date) |>
          dplyr::select(date, value) |>
          dplyr::rename(cl = value)
        ho_px <- dplyr::filter(cd, .data$market == "HO") |>
          dplyr::arrange(.data$date) |>
          dplyr::select(date, value) |>
          dplyr::rename(ho = value)
        rb_px <- dplyr::filter(cd, .data$market == "RB") |>
          dplyr::arrange(.data$date) |>
          dplyr::select(date, value) |>
          dplyr::rename(rb = value)

        # Align all three by date
        px_wide <- dplyr::inner_join(cl_px, ho_px, by = "date") |>
          dplyr::inner_join(rb_px, by = "date") |>
          dplyr::arrange(date) |>
          dplyr::mutate(
            # HO and RB are in $/gallon; CL is in $/bbl — convert to $/bbl for spreads
            ho_bbl    = ho * 42,
            rb_bbl    = rb * 42,
            ho_cl_spd = ho_bbl - cl,
            rb_cl_spd = rb_bbl - cl
          )

        # Outright front-month vol for selected market
        market_front_series <- paste0(input$energy_market, "01")
        outright_ser <- dplyr::filter(returns_data(),
          .data$series == market_front_series) |>
          dplyr::arrange(.data$date)

        outright_vol_df <- outright_ser |>
          dplyr::mutate(
            vol = compute_rolling_vol(.data$log_return, window = input$vol_window) * 100
          ) |>
          dplyr::filter(!is.na(.data$vol)) |>
          dplyr::select(date, vol)

        # Crack spread vol — use simple % return since spread can go negative
        crack_col <- if (input$energy_market %in% c("CL", "HO")) "ho_cl_spd" else "rb_cl_spd"
        crack_label <- if (input$energy_market %in% c("CL", "HO")) "HO-CL" else "RB-CL"

        crack_vol_df <- px_wide |>
          dplyr::mutate(
            spd_return  = (.data[[crack_col]] - dplyr::lag(.data[[crack_col]])) /
                          abs(dplyr::lag(.data[[crack_col]])),
            crack_vol   = zoo::rollapply(
              .data$spd_return, width = input$vol_window,
              FUN = function(x) stats::sd(x, na.rm = TRUE) * sqrt(252) * 100,
              fill = NA, align = "right"
            )
          ) |>
          dplyr::filter(!is.na(.data$crack_vol)) |>
          dplyr::select(date, crack_vol)

        joined_vol <- dplyr::inner_join(outright_vol_df, crack_vol_df, by = "date")

        shiny::req(nrow(joined_vol) > 0)

        plotly::plot_ly(joined_vol,
          x    = ~date,
          y    = ~vol,
          type = "scatter",
          mode = "lines",
          name = paste0(input$energy_market, " Outright Vol"),
          line = list(color = "#2c3e50", width = 1.5),
          hovertemplate = "Date: %{x}<br>Outright Vol: %{y:.1f}%<extra></extra>"
        ) |>
          plotly::add_lines(
            data          = joined_vol,
            x             = ~date,
            y             = ~crack_vol,
            name          = paste0(crack_label, " Crack Vol"),
            line          = list(color = "#e67e22", width = 1.5),
            hovertemplate = paste0("Date: %{x}<br>", crack_label, " Vol: %{y:.1f}%<extra></extra>")
          ) |>
          plotly::layout(
            title  = paste0(market_label(), " \u2014 Outright vs. ",
              crack_label, " Crack Spread Volatility"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Annualized Volatility (%)"),
            legend = list(orientation = "h", x = 0, y = 1.08),
            annotations = list(list(
              x = 0.01, y = 0.98, xref = "paper", yref = "paper",
              text = "Refiners hedge crack spreads, not outright prices \u2014 spread vol spikes on supply disruptions",
              showarrow = FALSE,
              font = list(size = 10, color = "grey50"),
              align = "left"
            ))
          )
      }
    })
  })
}
