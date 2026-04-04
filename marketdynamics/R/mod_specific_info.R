#' specific_info UI Function
#'
#' Market-specific charts: crack spreads and 3-2-1 (CL/HO/RB),
#' storage cycle (NG), WTI-Brent spread (BRN).
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_specific_info_ui <- function(id) {
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
      shiny::tagList(bsicons::bs_icon("pin-map"), " Market-Specific Analysis")
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
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      shiny::tags$hr(),
      shiny::uiOutput(ns("specific_layout"))
    )
  )
}

#' specific_info Server Function
#'
#' Self-contained module. Layout and charts adapt to the selected market:
#'   CL/HO/RB  ->  crack spreads + 3-2-1 crack + crack vol
#'   NG        ->  front-month price with injection/withdrawal shading
#'   BRN       ->  WTI vs Brent price overlay + spread
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object (unused; kept for API consistency).
#' @return None.
#' @export
mod_specific_info_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    energy_data <- shiny::reactiveVal(NULL)  # primary market all contracts
    petro_data  <- shiny::reactiveVal(NULL)  # CL + HO + RB front months
    cl_brn_data <- shiny::reactiveVal(NULL)  # CL + BRN front months

    shiny::observeEvent(
      list(input$energy_market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(input$energy_market, input$date_range)

        output$load_status <- shiny::renderUI(
          shiny::tags$small(class = "text-muted", "Loading...")
        )

        mkt   <- input$energy_market
        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        # Primary market (all contracts)
        ed <- tryCatch(
          load_energy_data(mkt, start, end),
          error = function(e) NULL
        )
        energy_data(ed)

        # Partner data based on market type
        if (mkt %in% c("CL", "HO", "RB")) {
          pd <- tryCatch(
            load_energy_data(c("CL", "HO", "RB"), start, end) |>
              dplyr::filter(.data$contract_num == 1),
            error = function(e) NULL
          )
          petro_data(pd)
          cl_brn_data(NULL)
        } else if (mkt == "BRN") {
          cbd <- tryCatch(
            load_energy_data(c("CL", "BRN"), start, end) |>
              dplyr::filter(.data$contract_num == 1),
            error = function(e) NULL
          )
          cl_brn_data(cbd)
          petro_data(NULL)
        } else {
          petro_data(NULL)
          cl_brn_data(NULL)
        }

        if (!is.null(ed)) {
          output$load_status <- shiny::renderUI(
            shiny::tags$small(class = "text-success",
              format(nrow(ed), big.mark = ","), " rows loaded")
          )
        } else {
          output$load_status <- shiny::renderUI(
            shiny::tags$small(class = "text-danger", "Error loading data")
          )
        }
      }
    )

    # ── Dynamic layout ───────────────────────────────────────────────────────
    output$specific_layout <- shiny::renderUI({
      shiny::req(input$energy_market)
      mkt <- input$energy_market

      if (mkt %in% c("CL", "HO", "RB")) {
        shiny::tagList(
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header(
                shiny::tagList(bsicons::bs_icon("bar-chart-line"),
                  " HO-CL & RB-CL Crack Spreads")
              ),
              bslib::card_body(
                plotly::plotlyOutput(ns("crack_spread_plot"), height = "360px")
              )
            ),
            bslib::card(
              bslib::card_header(
                shiny::tagList(bsicons::bs_icon("calculator"),
                  " 3-2-1 Crack Spread \u2014 Refinery Margin")
              ),
              bslib::card_body(
                plotly::plotlyOutput(ns("crack_321_plot"), height = "360px")
              )
            )
          ),
          shiny::tags$div(
            style = "margin-top:1rem;",
            bslib::card(
              bslib::card_header(
                shiny::tagList(bsicons::bs_icon("activity"),
                  " Outright vs. Crack Spread Volatility")
              ),
              bslib::card_body(
                plotly::plotlyOutput(ns("crack_vol_plot"), height = "320px")
              )
            )
          )
        )

      } else if (mkt == "NG") {
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("thermometer-half"),
              " Natural Gas \u2014 Price with Injection & Withdrawal Cycle")
          ),
          bslib::card_body(
            plotly::plotlyOutput(ns("ng_storage_plot"),
              height = "calc(100vh - 300px)")
          )
        )

      } else {   # BRN
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("arrow-left-right"),
              " WTI vs. Brent Crude \u2014 Price & Location Spread")
          ),
          bslib::card_body(
            plotly::plotlyOutput(ns("wti_brn_plot"),
              height = "calc(100vh - 300px)")
          )
        )
      }
    })

    # ── Crack Spread Plot ────────────────────────────────────────────────────
    output$crack_spread_plot <- plotly::renderPlotly({
      shiny::req(petro_data())
      cd <- petro_data()

      cl <- dplyr::filter(cd, .data$market == "CL") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(cl = value)
      ho <- dplyr::filter(cd, .data$market == "HO") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(ho = value)
      rb <- dplyr::filter(cd, .data$market == "RB") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(rb = value)

      px <- dplyr::inner_join(cl, ho, by = "date") |>
        dplyr::inner_join(rb, by = "date") |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          ho_cl = ho * 42 - cl,
          rb_cl = rb * 42 - cl
        )

      med_ho <- stats::median(px$ho_cl, na.rm = TRUE)
      med_rb <- stats::median(px$rb_cl, na.rm = TRUE)

      plotly::plot_ly(px,
        x    = ~date, y = ~ho_cl,
        type = "scatter", mode = "lines",
        name = "HO-CL",
        line = list(color = "#e74c3c", width = 1.5),
        hovertemplate = "Date: %{x|%Y-%m-%d}<br>HO-CL: $%{y:.2f}/bbl<extra></extra>"
      ) |>
        plotly::add_lines(
          data = px, x = ~date, y = ~rb_cl, name = "RB-CL",
          line = list(color = "#2980b9", width = 1.5),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>RB-CL: $%{y:.2f}/bbl<extra></extra>"
        ) |>
        plotly::add_lines(
          y = med_ho,
          line = list(color = "#e74c3c", dash = "dot", width = 1),
          name = sprintf("HO-CL med ($%.2f)", med_ho),
          showlegend = TRUE, hoverinfo = "none"
        ) |>
        plotly::add_lines(
          y = med_rb,
          line = list(color = "#2980b9", dash = "dot", width = 1),
          name = sprintf("RB-CL med ($%.2f)", med_rb),
          showlegend = TRUE, hoverinfo = "none"
        ) |>
        plotly::layout(
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Crack Spread ($/bbl)"),
          legend = list(orientation = "h", x = 0, y = 1.08),
          annotations = list(list(
            x = 0.01, y = 0.98, xref = "paper", yref = "paper",
            text = "Refinery margin = refined product price \u2212 crude cost (HO & RB converted to $/bbl)",
            showarrow = FALSE, font = list(size = 10, color = "grey50"), align = "left"
          ))
        )
    })

    # ── 3-2-1 Crack Plot ─────────────────────────────────────────────────────
    output$crack_321_plot <- plotly::renderPlotly({
      shiny::req(petro_data())
      cd <- petro_data()

      cl <- dplyr::filter(cd, .data$market == "CL") |>
        dplyr::select(date, value) |> dplyr::rename(cl = value)
      ho <- dplyr::filter(cd, .data$market == "HO") |>
        dplyr::select(date, value) |> dplyr::rename(ho = value)
      rb <- dplyr::filter(cd, .data$market == "RB") |>
        dplyr::select(date, value) |> dplyr::rename(rb = value)

      px <- dplyr::inner_join(cl, ho, by = "date") |>
        dplyr::inner_join(rb, by = "date") |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          crack_321 = (2 * rb * 42 + 1 * ho * 42 - 3 * cl) / 3
        )

      med_321 <- stats::median(px$crack_321, na.rm = TRUE)
      p75_321 <- stats::quantile(px$crack_321, 0.75, na.rm = TRUE)

      plotly::plot_ly(px,
        x    = ~date, y = ~crack_321,
        type = "scatter", mode = "lines",
        name = "3-2-1 Crack",
        line = list(color = "#8e44ad", width = 1.8),
        hovertemplate = "Date: %{x|%Y-%m-%d}<br>3-2-1: $%{y:.2f}/bbl<extra></extra>"
      ) |>
        plotly::add_lines(
          y = med_321,
          line = list(color = "#7f8c8d", dash = "dot", width = 1.2),
          name = sprintf("Median ($%.2f)", med_321),
          showlegend = TRUE, hoverinfo = "none"
        ) |>
        plotly::add_lines(
          y = p75_321,
          line = list(color = "#e67e22", dash = "dot", width = 1.2),
          name = sprintf("75th pct ($%.2f)", p75_321),
          showlegend = TRUE, hoverinfo = "none"
        ) |>
        plotly::layout(
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "3-2-1 Crack ($/bbl)"),
          legend = list(orientation = "h", x = 0, y = 1.08),
          annotations = list(list(
            x = 0.01, y = 0.98, xref = "paper", yref = "paper",
            text = "(2 \u00d7 RBOB + 1 \u00d7 HO \u2212 3 \u00d7 CL) / 3",
            showarrow = FALSE, font = list(size = 10, color = "grey50"), align = "left"
          ))
        )
    })

    # ── Crack Spread Vol Plot ────────────────────────────────────────────────
    output$crack_vol_plot <- plotly::renderPlotly({
      shiny::req(petro_data(), input$energy_market)
      cd  <- petro_data()
      mkt <- input$energy_market

      cl_px <- dplyr::filter(cd, .data$market == "CL") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(cl = value)
      ho_px <- dplyr::filter(cd, .data$market == "HO") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(ho = value)
      rb_px <- dplyr::filter(cd, .data$market == "RB") |>
        dplyr::arrange(date) |> dplyr::select(date, value) |> dplyr::rename(rb = value)

      px_wide <- dplyr::inner_join(cl_px, ho_px, by = "date") |>
        dplyr::inner_join(rb_px, by = "date") |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          ho_bbl    = ho * 42,
          rb_bbl    = rb * 42,
          ho_cl_spd = ho_bbl - cl,
          rb_cl_spd = rb_bbl - cl
        )

      # Outright vol from petro_data (already front-month)
      outright_ret <- dplyr::filter(cd, .data$market == mkt) |>
        dplyr::arrange(date) |>
        compute_log_returns() |>
        dplyr::mutate(
          vol = compute_rolling_vol(.data$log_return, window = 21) * 100
        ) |>
        dplyr::filter(!is.na(.data$vol)) |>
        dplyr::select(date, vol)

      crack_col   <- if (mkt %in% c("CL", "HO")) "ho_cl_spd" else "rb_cl_spd"
      crack_label <- if (mkt %in% c("CL", "HO")) "HO-CL" else "RB-CL"

      crack_vol_df <- px_wide |>
        dplyr::mutate(
          spd_ret   = (.data[[crack_col]] - dplyr::lag(.data[[crack_col]])) /
                      abs(dplyr::lag(.data[[crack_col]])),
          crack_vol = zoo::rollapply(
            .data$spd_ret, width = 21,
            FUN = function(x) stats::sd(x, na.rm = TRUE) * sqrt(252) * 100,
            fill = NA, align = "right"
          )
        ) |>
        dplyr::filter(!is.na(.data$crack_vol)) |>
        dplyr::select(date, crack_vol)

      joined <- dplyr::inner_join(outright_ret, crack_vol_df, by = "date")
      shiny::req(nrow(joined) > 0)

      plotly::plot_ly(joined,
        x    = ~date, y = ~vol,
        type = "scatter", mode = "lines",
        name = paste0(mkt, " Outright Vol"),
        line = list(color = "#2c3e50", width = 1.5),
        hovertemplate = "Date: %{x}<br>Outright: %{y:.1f}%<extra></extra>"
      ) |>
        plotly::add_lines(
          data = joined, x = ~date, y = ~crack_vol,
          name = paste0(crack_label, " Crack Vol"),
          line = list(color = "#e67e22", width = 1.5),
          hovertemplate = paste0("Date: %{x}<br>", crack_label,
            " Vol: %{y:.1f}%<extra></extra>")
        ) |>
        plotly::layout(
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Annualized Volatility (%)"),
          legend = list(orientation = "h", x = 0, y = 1.08),
          annotations = list(list(
            x = 0.01, y = 0.98, xref = "paper", yref = "paper",
            text = "Refiners hedge crack spreads, not outright prices \u2014 spread vol spikes on supply disruptions",
            showarrow = FALSE, font = list(size = 10, color = "grey50"), align = "left"
          ))
        )
    })

    # ── NG Storage Cycle Plot ────────────────────────────────────────────────
    output$ng_storage_plot <- plotly::renderPlotly({
      shiny::req(energy_data(), input$energy_market == "NG")

      ng_front <- dplyr::filter(energy_data(), .data$series == "NG01") |>
        dplyr::arrange(date) |>
        dplyr::filter(!is.na(.data$value))

      shiny::req(nrow(ng_front) > 0)

      start_yr <- as.integer(format(min(ng_front$date), "%Y"))
      end_yr   <- as.integer(format(max(ng_front$date), "%Y"))

      # Generate per-year injection/withdrawal background shapes
      shapes <- list()
      for (yr in start_yr:end_yr) {
        # Injection season: Apr 1 – Oct 31 (blue tint)
        shapes <- c(shapes, list(list(
          type      = "rect",
          xref      = "x", yref = "paper",
          x0        = as.character(as.Date(paste0(yr, "-04-01"))),
          x1        = as.character(as.Date(paste0(yr, "-10-31"))),
          y0 = 0, y1 = 1,
          fillcolor = "rgba(52,152,219,0.07)",
          line      = list(width = 0)
        )))
        # Withdrawal season: Nov 1 – Mar 31 next year (red tint)
        shapes <- c(shapes, list(list(
          type      = "rect",
          xref      = "x", yref = "paper",
          x0        = as.character(as.Date(paste0(yr, "-11-01"))),
          x1        = as.character(as.Date(paste0(yr + 1, "-03-31"))),
          y0 = 0, y1 = 1,
          fillcolor = "rgba(231,76,60,0.07)",
          line      = list(width = 0)
        )))
      }

      plotly::plot_ly(ng_front,
        x    = ~date, y = ~value,
        type = "scatter", mode = "lines",
        name = "NG Front Month",
        line = list(color = "#2c3e50", width = 1.5),
        hovertemplate = "Date: %{x|%Y-%m-%d}<br>$%{y:.3f}/MMBtu<extra></extra>"
      ) |>
        plotly::layout(
          title  = "Natural Gas \u2014 Front-Month Price with Storage Cycle",
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Price ($/MMBtu)"),
          shapes = shapes,
          legend = list(orientation = "h"),
          annotations = list(list(
            x = 0.01, y = 0.98, xref = "paper", yref = "paper",
            text = "Blue = Injection Season (Apr\u2013Oct, prices tend soft)  |  Red = Withdrawal Season (Nov\u2013Mar, prices firm)",
            showarrow = FALSE, font = list(size = 10, color = "grey50"), align = "left"
          ))
        )
    })

    # ── WTI-Brent Plot ───────────────────────────────────────────────────────
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
