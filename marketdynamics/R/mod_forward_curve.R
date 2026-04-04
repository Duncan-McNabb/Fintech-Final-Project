#' forward_curve UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_forward_curve_ui <- function(id) {
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
      shiny::tagList(bsicons::bs_icon("layers"), " Market Structure")
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
          shiny::radioButtons(ns("view_type"), "View",
            choices  = c(
              "Curve Lines"     = "curves",
              "Calendar Spread" = "spread"
            ),
            inline   = TRUE,
            selected = "curves")
        ),
        shiny::column(2,
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::value_box(
          title    = "Market Structure",
          value    = shiny::uiOutput(ns("vb_structure")),
          showcase = bsicons::bs_icon("graph-up-arrow"),
          theme    = "primary"
        ),
        bslib::value_box(
          title    = "M1\u2212M2 Spread",
          value    = shiny::uiOutput(ns("vb_spread")),
          showcase = bsicons::bs_icon("arrows-expand")
        ),
        bslib::value_box(
          title    = "Historical Percentile",
          value    = shiny::uiOutput(ns("vb_pct")),
          showcase = bsicons::bs_icon("percent")
        )
      ),
      shiny::tags$hr(),
      plotly::plotlyOutput(ns("curve_plot"), height = "380px"),
      shiny::tags$hr(),
      shiny::uiOutput(ns("market_context"))
    )
  )
}

#' forward_curve Server Function
#'
#' Loads energy data locally and renders forward curve charts.
#' Reads r$market to sync the market selector.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object.
#' @return None.
#' @export
mod_forward_curve_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    energy_data   <- shiny::reactiveVal(NULL)
    cl_brn_data   <- shiny::reactiveVal(NULL)
    cl_stocks_data <- shiny::reactiveVal(NULL)

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
              shiny::tags$small(class = "text-danger",
                paste("Error:", conditionMessage(e)))
            })
            NULL
          }
        )

        energy_data(data)

        # Load EIA crude stocks when WTI is selected
        if (input$energy_market == "CL") {
          stocks <- tryCatch(
            load_eia_data(roles = "crude_stocks", start_date = start, end_date = end),
            error = function(e) NULL
          )
          cl_stocks_data(stocks)
        } else {
          cl_stocks_data(NULL)
        }

        if (!is.null(data)) {
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-success",
              format(nrow(data), big.mark = ","), " rows loaded")
          })
        }
      }
    )

    shiny::observeEvent(
      list(input$energy_market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = TRUE, {
        shiny::req(input$energy_market, input$date_range)
        if (input$energy_market != "BRN") {
          cl_brn_data(NULL)
          return()
        }
        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])
        cbd <- tryCatch(
          load_energy_data(c("CL", "BRN"), start, end) |>
            dplyr::filter(.data$contract_num == 1),
          error = function(e) NULL
        )
        cl_brn_data(cbd)
      }
    )

    shiny::observeEvent(r$market, {
      shiny::req(r$market)
      shiny::updateSelectInput(session, "energy_market", selected = r$market)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    curve_data <- shiny::reactive({
      shiny::req(energy_data(), input$energy_market)
      pivot_wide(energy_data(), input$energy_market)
    })

    market_label <- shiny::reactive({
      labels <- c(CL = "WTI Crude", BRN = "Brent Crude", NG = "Natural Gas",
                  HO = "Heating Oil", RB = "RBOB Gasoline")
      unname(labels[input$energy_market])
    })

    spread_stats <- shiny::reactive({
      cd <- curve_data()
      shiny::req("c1" %in% names(cd), "c2" %in% names(cd))
      spread_series <- cd$c1 - cd$c2
      spread_series <- spread_series[!is.na(spread_series)]
      shiny::req(length(spread_series) > 0)
      current <- tail(spread_series, 1)
      pct_rank <- round(mean(spread_series <= current) * 100)
      list(current = current, pct_rank = pct_rank,
           is_backwardation = current > 0)
    })

    output$vb_structure <- shiny::renderUI({
      s <- tryCatch(spread_stats(), error = function(e) NULL)
      if (is.null(s)) return(shiny::tags$span("\u2014"))
      if (s$is_backwardation) {
        shiny::tags$span(style = "color:#27ae60; font-weight:700;", "Backwardation")
      } else {
        shiny::tags$span(style = "color:#e74c3c; font-weight:700;", "Contango")
      }
    })

    output$vb_spread <- shiny::renderUI({
      s <- tryCatch(spread_stats(), error = function(e) NULL)
      if (is.null(s)) return(shiny::tags$span("\u2014"))
      color <- if (s$is_backwardation) "#27ae60" else "#e74c3c"
      shiny::tags$span(
        style = paste0("color:", color, "; font-weight:700;"),
        sprintf("$%+.3f", s$current)
      )
    })

    output$vb_pct <- shiny::renderUI({
      s <- tryCatch(spread_stats(), error = function(e) NULL)
      if (is.null(s)) return(shiny::tags$span("\u2014"))
      shiny::tags$span(
        style = "font-weight:700;",
        paste0(s$pct_rank, "th")
      )
    })

    output$curve_plot <- plotly::renderPlotly({
      shiny::req(curve_data(), input$view_type)
      cd <- curve_data()

      if (input$view_type == "spread") {
        # Calendar spread: M1 - M2
        shiny::req("c1" %in% names(cd), "c2" %in% names(cd))

        spread_df <- dplyr::tibble(
          date   = cd$date,
          spread = cd$c1 - cd$c2
        ) |>
          dplyr::filter(!is.na(spread))

        y_max  <- max(spread_df$spread, na.rm = TRUE)
        y_min  <- min(spread_df$spread, na.rm = TRUE)
        y_pad  <- (y_max - y_min) * 0.05
        median_spread <- stats::median(spread_df$spread, na.rm = TRUE)

        # Background shading shapes: green above 0 (backwardation), red below 0 (contango)
        bg_shapes <- list(
          list(
            type    = "rect",
            xref    = "paper", yref = "y",
            x0 = 0, x1 = 1, y0 = 0, y1 = y_max + y_pad,
            fillcolor = "rgba(39,174,96,0.07)",
            line      = list(width = 0)
          ),
          list(
            type    = "rect",
            xref    = "paper", yref = "y",
            x0 = 0, x1 = 1, y0 = y_min - y_pad, y1 = 0,
            fillcolor = "rgba(231,76,60,0.07)",
            line      = list(width = 0)
          )
        )

        plotly::plot_ly(spread_df,
          x    = ~date,
          y    = ~spread,
          type = "scatter",
          mode = "lines",
          name = "M1-M2 Spread",
          line = list(color = "#2c3e50", width = 1.5),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>Spread: $%{y:.3f}<extra></extra>"
        ) |>
          plotly::add_lines(
            y          = 0,
            line       = list(color = "grey40", dash = "dash", width = 1),
            showlegend = FALSE,
            hoverinfo  = "none"
          ) |>
          plotly::add_lines(
            y          = median_spread,
            line       = list(color = "#e67e22", dash = "dot", width = 1.2),
            name       = sprintf("Median ($%.3f)", median_spread),
            showlegend = TRUE,
            hoverinfo  = "none"
          ) |>
          plotly::layout(
            title  = paste0(market_label(),
              " \u2014 M1-M2 Calendar Spread (Backwardation = green / Contango = red)"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "M1 \u2212 M2 Spread ($/unit)"),
            shapes = bg_shapes,
            legend = list(orientation = "h", x = 0, y = 1.08),
            annotations = list(list(
              x = 0.01, y = 0.98, xref = "paper", yref = "paper",
              text      = "Above 0 = Backwardation (supply tight)   Below 0 = Contango (supply ample)",
              showarrow = FALSE,
              font      = list(size = 10, color = "grey50"),
              align     = "left"
            ))
          )

      } else {
        # Default: multi-line forward curve
        contract_cols <- setdiff(names(cd), "date")
        contract_nums <- as.integer(sub("^c", "", contract_cols))

        n_contracts <- length(contract_cols)
        pal <- grDevices::colorRampPalette(
          c("#440154", "#31688e", "#35b779", "#fde725")
        )(max(n_contracts - 1, 1))

        p <- plotly::plot_ly()

        # Pass 1: all visual lines, no tooltip contribution
        for (i in seq_along(contract_cols)) {
          col      <- contract_cols[i]
          is_front <- col == "c1"
          p <- plotly::add_lines(p,
            x          = cd$date,
            y          = cd[[col]],
            name       = if (is_front) "Front Month" else paste("Contract", contract_nums[i]),
            line       = list(
              color = if (is_front) "black" else pal[max(i - 1, 1)],
              width = if (is_front) 2.5 else 0.8
            ),
            showlegend = is_front,
            hoverinfo  = "none"
          )
        }

        # Pass 2: invisible ghost traces for 6 benchmark contracts — supply tooltip
        benchmark_nums <- c(1, 6, 12, 18, 24, 36)
        benchmark_cols <- paste0("c", benchmark_nums[benchmark_nums %in% contract_nums])
        for (col in benchmark_cols) {
          cnum <- as.integer(sub("^c", "", col))
          p <- plotly::add_lines(p,
            x             = cd$date,
            y             = cd[[col]],
            name          = if (cnum == 1) "Front (c1)" else paste0("c", cnum),
            line          = list(width = 0, color = "rgba(0,0,0,0)"),
            showlegend    = FALSE,
            hovertemplate = paste0("c", cnum, " \u2014 %{x|%Y-%m-%d}: $%{y:.2f}<extra></extra>")
          )
        }

        p |> plotly::layout(
          title     = paste(market_label(), "\u2014 Forward Curve Dynamics"),
          xaxis     = list(title = "Date"),
          yaxis     = list(title = "Price ($/unit)"),
          hovermode = "x unified",
          legend    = list(orientation = "h", x = 0, y = 1.08)
        )
      }
    })

    output$market_context <- shiny::renderUI({
      shiny::req(input$energy_market)
      mkt <- input$energy_market

      if (mkt == "CL") {
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("info-circle"), " Market Context: WTI Crude \u2014 Price vs. US Crude Stocks")
          ),
          bslib::card_body(
            plotly::plotlyOutput(session$ns("cl_stocks_plot"), height = "320px"),
            shiny::tags$p(
              class = "text-muted mt-2", style = "font-size:0.9rem;",
              "Cushing, Oklahoma inventory is the primary driver of WTI curve shape. High stocks \u2192 contango (storage incentive); low stocks \u2192 backwardation (supply squeeze). Stocks axis inverted to illustrate the inverse relationship."
            )
          )
        )

      } else if (mkt == "BRN") {
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("info-circle"), " Market Context: Brent Crude \u2014 WTI vs. Brent Price & Location Spread")
          ),
          bslib::card_body(
            plotly::plotlyOutput(session$ns("wti_brn_plot"), height = "320px")
          )
        )

      } else if (mkt == "NG") {
        info_text <- "Henry Hub is the benchmark delivery point for NYMEX natural gas. The forward curve reflects the storage cycle: injection season (Apr\u2013Oct) tends to depress near-term prices, building contango; withdrawal season (Nov\u2013Mar) draws inventory and drives backwardation. LNG export growth (12 Bcf/d in 2024) is adding a structural floor to Henry Hub prices."
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("info-circle"), " Market Context: Natural Gas")
          ),
          bslib::card_body(shiny::tags$p(class = "text-muted", style = "font-size:0.9rem;", info_text))
        )

      } else if (mkt == "HO") {
        info_text <- "Heating Oil (HO) on NYMEX is the closest liquid proxy for ULSD (ultra-low sulfur diesel) traded in the US. The curve reflects seasonal heating demand in the US Northeast: winter draws inventory and drives backwardation (Nov\u2013Mar); spring/summer rebuilds stocks and tips the curve toward contango."
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("info-circle"), " Market Context: Heating Oil")
          ),
          bslib::card_body(shiny::tags$p(class = "text-muted", style = "font-size:0.9rem;", info_text))
        )

      } else if (mkt == "RB") {
        info_text <- "RBOB Gasoline prices reflect refinery run rates, RVP (Reid Vapor Pressure) spec transitions, and the US driving season. The Apr\u2013May RVP spec change (summer blends cost more to produce) typically tightens the front of the curve. The Dec\u2013Jan shoulder is usually the weakest seasonal period."
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("info-circle"), " Market Context: RBOB Gasoline")
          ),
          bslib::card_body(shiny::tags$p(class = "text-muted", style = "font-size:0.9rem;", info_text))
        )

      } else {
        NULL
      }
    })

    output$cl_stocks_plot <- plotly::renderPlotly({
      shiny::req(cl_stocks_data(), energy_data())

      cl_front <- dplyr::filter(energy_data(), .data$series == "CL01") |>
        dplyr::arrange(date) |>
        dplyr::filter(!is.na(.data$value))

      stocks <- dplyr::filter(cl_stocks_data(), !is.na(.data$value)) |>
        dplyr::arrange(date)

      shiny::req(nrow(cl_front) > 0, nrow(stocks) > 0)

      plotly::plot_ly(cl_front, x = ~date) |>
        plotly::add_lines(
          y             = ~value,
          name          = "WTI Front Month",
          line          = list(color = "#2c3e50", width = 1.5),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>$%{y:.2f}/bbl<extra></extra>"
        ) |>
        plotly::add_lines(
          data          = stocks,
          x             = ~date,
          y             = ~value,
          name          = "US Crude Stocks (kbbl)",
          yaxis         = "y2",
          line          = list(color = "#e67e22", width = 1.2, dash = "dot"),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>Stocks: %{y:,.0f} kbbl<extra></extra>"
        ) |>
        plotly::layout(
          title  = "WTI Crude \u2014 Front-Month Price vs. US Crude Stocks",
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Price ($/bbl)"),
          yaxis2 = list(
            title      = "Crude Stocks (kbbl)",
            overlaying = "y", side = "right",
            showgrid   = FALSE,
            autorange  = "reversed"
          ),
          legend = list(orientation = "h", x = 0, y = 1.08),
          annotations = list(list(
            x         = 0.01, y = 0.98, xref = "paper", yref = "paper",
            text      = "Stocks axis inverted \u2014 rising inventory correlates with falling price (contango pressure)",
            showarrow = FALSE, font = list(size = 10, color = "grey50"), align = "left"
          ))
        )
    })

    output$wti_brn_plot <- plotly::renderPlotly({
      shiny::req(cl_brn_data())
      cbd <- cl_brn_data()

      cl  <- dplyr::filter(cbd, .data$market == "CL") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(cl = value)
      brn <- dplyr::filter(cbd, .data$market == "BRN") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(brn = value)

      shiny::req(nrow(cl) > 0, nrow(brn) > 0)

      px <- dplyr::inner_join(cl, brn, by = "date") |>
        dplyr::arrange(date) |>
        dplyr::mutate(spread = cl - brn)

      plotly::plot_ly(px, x = ~date) |>
        plotly::add_lines(
          y    = ~cl, name = "WTI (CL)",
          line = list(color = "#2c3e50", width = 1.5),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>WTI: $%{y:.2f}/bbl<extra></extra>"
        ) |>
        plotly::add_lines(
          y    = ~brn, name = "Brent (BRN)",
          line = list(color = "#e74c3c", width = 1.5),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>Brent: $%{y:.2f}/bbl<extra></extra>"
        ) |>
        plotly::add_lines(
          y     = ~spread,
          name  = "WTI\u2212Brent Spread",
          line  = list(color = "#27ae60", width = 1.5, dash = "dash"),
          yaxis = "y2",
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>Spread: $%{y:.2f}/bbl<extra></extra>"
        ) |>
        plotly::layout(
          title  = "WTI vs. Brent Crude \u2014 Price & Location Spread",
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Price ($/bbl)"),
          yaxis2 = list(
            title      = "WTI\u2212Brent Spread ($/bbl)",
            overlaying = "y", side = "right",
            showgrid   = FALSE,
            zeroline   = TRUE, zerolinecolor = "grey80"
          ),
          legend = list(orientation = "h", x = 0, y = 1.08),
          annotations = list(list(
            x = 0.01, y = 0.98, xref = "paper", yref = "paper",
            text = "Spread went negative 2011\u20132015 as Cushing storage filled (US shale boom) \u2014 has since converged",
            showarrow = FALSE, font = list(size = 10, color = "grey50"), align = "left"
          ))
        )
    })
  })
}
