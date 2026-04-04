#' treasury_curve UI Function
#'
#' Displays US Treasury yield curve dynamics with a full date range control.
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_treasury_curve_ui <- function(id) {
  ns <- NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::tagList(bsicons::bs_icon("bank"), " US Treasury Yield Curve")
    ),
    bslib::card_body(
      shiny::fluidRow(
        shiny::column(5,
          shiny::dateRangeInput(ns("date_range"), "Date Range",
            start = "1990-01-01", end = Sys.Date(),
            min   = "1990-01-01", max = Sys.Date())
        ),
        shiny::column(4,
          shiny::radioButtons(ns("view_type"), "View",
            choices  = c(
              "Yield Curves"  = "curves",
              "Yield Spreads" = "spreads"
            ),
            inline   = TRUE,
            selected = "curves")
        ),
        shiny::column(3,
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      shiny::tags$hr(),
      plotly::plotlyOutput(ns("curve_plot"), height = "calc(100vh - 260px)")
    )
  )
}

#' treasury_curve Server Function
#'
#' Loads CMT data locally and renders yield curve charts.
#' Does NOT read from or write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object (unused; kept for API consistency).
#' @return None.
#' @export
mod_treasury_curve_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    # Tenor label lookup: pivot_wide column name -> human-readable label
    tenor_labels <- c(
      c1   = "1-Month",  c3   = "3-Month",  c6   = "6-Month",
      c12  = "1-Year",   c24  = "2-Year",   c36  = "3-Year",
      c60  = "5-Year",   c84  = "7-Year",   c120 = "10-Year",
      c240 = "20-Year",  c360 = "30-Year"
    )

    cmt_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      input$date_range,
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(input$date_range)

        output$load_status <- shiny::renderUI({
          shiny::tags$small(class = "text-muted", "Loading...")
        })

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        data <- tryCatch(
          load_cmt_data(start_date = start, end_date = end),
          error = function(e) {
            output$load_status <- shiny::renderUI({
              shiny::tags$small(class = "text-danger", "Error loading CMT data")
            })
            NULL
          }
        )

        cmt_data(data)

        if (!is.null(data)) {
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-success",
              format(nrow(data), big.mark = ","), " rows loaded")
          })
        }
      }
    )

    curve_data <- shiny::reactive({
      shiny::req(cmt_data())
      pivot_wide(cmt_data(), "CMT")
    })

    output$curve_plot <- plotly::renderPlotly({
      shiny::req(curve_data(), input$view_type)
      cd <- curve_data()

      if (input$view_type == "spreads") {
        # Yield Spreads: 2s10s and 3m10y
        shiny::req("c120" %in% names(cd))

        spread_df <- dplyr::tibble(date = cd$date)

        if ("c24" %in% names(cd)) {
          spread_df$s_2s10s <- (cd$c120 - cd$c24) * 100   # basis points
        } else {
          spread_df$s_2s10s <- NA_real_
        }

        if ("c1" %in% names(cd)) {
          spread_df$s_3m10y <- (cd$c120 - cd$c1) * 100    # basis points
        } else {
          spread_df$s_3m10y <- NA_real_
        }

        spread_df <- dplyr::filter(spread_df,
          !is.na(.data$s_2s10s) | !is.na(.data$s_3m10y))

        # Build red inversion shading: identify periods where 2s10s < 0
        y_min  <- min(c(spread_df$s_2s10s, spread_df$s_3m10y), na.rm = TRUE)
        y_max  <- max(c(spread_df$s_2s10s, spread_df$s_3m10y), na.rm = TRUE)
        y_pad  <- (y_max - y_min) * 0.05

        bg_shapes <- list(
          list(
            type      = "rect",
            xref      = "paper", yref = "y",
            x0 = 0, x1 = 1,
            y0 = y_min - y_pad, y1 = 0,
            fillcolor = "rgba(231,76,60,0.08)",
            line      = list(width = 0)
          )
        )

        p <- plotly::plot_ly()

        if (!all(is.na(spread_df$s_2s10s))) {
          p <- plotly::add_lines(p,
            data          = spread_df,
            x             = ~date,
            y             = ~s_2s10s,
            name          = "2s10s (10yr \u2212 2yr)",
            line          = list(color = "#2c7bb6", width = 1.8),
            hovertemplate = "Date: %{x|%Y-%m-%d}<br>2s10s: %{y:.1f} bps<extra></extra>"
          )
        }

        if (!all(is.na(spread_df$s_3m10y))) {
          p <- plotly::add_lines(p,
            data          = spread_df,
            x             = ~date,
            y             = ~s_3m10y,
            name          = "3m10y (10yr \u2212 3m)",
            line          = list(color = "#d7191c", width = 1.8),
            hovertemplate = "Date: %{x|%Y-%m-%d}<br>3m10y: %{y:.1f} bps<extra></extra>"
          )
        }

        p |>
          plotly::add_lines(
            x          = spread_df$date,
            y          = rep(0, nrow(spread_df)),
            line       = list(color = "grey30", dash = "dash", width = 1),
            showlegend = FALSE,
            hoverinfo  = "none"
          ) |>
          plotly::layout(
            title  = "Yield Curve Spreads \u2014 Recession Indicator",
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Spread (basis points)"),
            shapes = bg_shapes,
            legend = list(orientation = "h", x = 0, y = 1.08),
            annotations = list(list(
              x         = 0.01, y = 0.02,
              xref      = "paper", yref = "paper",
              text      = "Red zone = inverted curve (negative spread \u2014 historical recession precursor)",
              showarrow = FALSE,
              font      = list(size = 10, color = "#c0392b"),
              align     = "left"
            ))
          )

      } else {
        # Default: all tenor time-series lines
        contract_cols <- intersect(names(tenor_labels), names(cd))

        n   <- length(contract_cols)
        pal <- grDevices::colorRampPalette(
          c("#440154", "#31688e", "#35b779", "#fde725")
        )(max(n, 1))

        p <- plotly::plot_ly()

        # Pass 1: visual lines, no hover contribution
        for (i in seq_along(contract_cols)) {
          col    <- contract_cols[i]
          is_10y <- col == "c120"
          p <- plotly::add_lines(p,
            x          = cd$date,
            y          = cd[[col]],
            name       = tenor_labels[[col]],
            line       = list(
              color = if (is_10y) "black" else pal[i],
              width = if (is_10y) 2.5 else 0.8
            ),
            showlegend = is_10y,
            hoverinfo  = "none"
          )
        }

        # Pass 2: invisible ghost traces for 6 benchmark tenors — supply tooltip
        benchmark_cols <- intersect(c("c1", "c6", "c24", "c60", "c120", "c360"),
          contract_cols)
        for (col in benchmark_cols) {
          p <- plotly::add_lines(p,
            x             = cd$date,
            y             = cd[[col]],
            name          = tenor_labels[[col]],
            line          = list(width = 0, color = "rgba(0,0,0,0)"),
            showlegend    = FALSE,
            hovertemplate = paste0(
              tenor_labels[[col]], " \u2014 %{x|%Y-%m-%d}: %{y:.2f}%<extra></extra>"
            )
          )
        }

        p |> plotly::layout(
          title     = "US Treasury Yield Curve Dynamics",
          xaxis     = list(title = "Date"),
          yaxis     = list(title = "Yield (%)"),
          hovermode = "x unified",
          legend    = list(orientation = "h", x = 0, y = 1.08)
        )
      }
    })
  })
}
