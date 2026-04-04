#' seasonality UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_seasonality_ui <- function(id) {
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
      shiny::tagList(bsicons::bs_icon("calendar3"), " Seasonality Patterns")
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
              "Monthly Returns"      = "monthly",
              "Year Overlay"         = "overlay",
              "Seasonal Vol"         = "vol",
              "Calendar Spread"      = "cal_spread"
            ),
            inline   = TRUE,
            selected = "monthly")
        ),
        shiny::column(2,
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      shiny::tags$hr(),
      plotly::plotlyOutput(ns("seas_plot"), height = "calc(100vh - 260px)")
    )
  )
}

#' seasonality Server Function
#'
#' Loads energy data locally and renders seasonality charts.
#' Does NOT read from or write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object (unused; kept for API consistency).
#' @return None.
#' @export
mod_seasonality_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    energy_data <- shiny::reactiveVal(NULL)

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

        if (!is.null(data)) {
          output$load_status <- shiny::renderUI({ NULL })
        }
      }
    )

    # Front-month series derived from selected market (e.g. "CL" -> "CL01")
    series_id <- shiny::reactive({
      shiny::req(input$energy_market)
      paste0(input$energy_market, "01")
    })

    returns <- shiny::reactive({
      shiny::req(energy_data(), series_id())
      energy_data() |>
        dplyr::filter(.data$series == series_id()) |>
        compute_log_returns()
    })

    market_label <- shiny::reactive({
      labels <- c(CL = "WTI Crude", BRN = "Brent Crude", NG = "Natural Gas",
                  HO = "Heating Oil", RB = "RBOB Gasoline")
      unname(labels[input$energy_market])
    })

    output$seas_plot <- plotly::renderPlotly({
      shiny::req(returns(), input$view_type)

      if (input$view_type == "monthly") {

        idx <- compute_seasonal_index(returns(), series_id(), freq = "month")
        idx$month_name <- month.abb[idx$period]
        idx$month_name <- factor(idx$month_name, levels = month.abb)
        idx$color      <- ifelse(idx$mean_return >= 0, "#27ae60", "#e74c3c")
        idx$pct        <- idx$mean_return * 100
        idx$se_pct     <- idx$se_return  * 100

        p <- plotly::plot_ly(idx,
          x       = ~month_name,
          y       = ~pct,
          type    = "bar",
          marker  = list(color = ~color),
          error_y = list(type = "data", array = ~se_pct, visible = TRUE,
            color = "grey40"),
          hovertemplate = "%{x}: %{y:.2f}%<extra></extra>"
        )

        # NG storage cycle annotations
        if (input$energy_market == "NG") {
          y_top <- max(abs(idx$pct), na.rm = TRUE) * 1.5

          p <- p |> plotly::layout(
            annotations = list(
              list(
                x = "May", y = y_top,
                xref = "x", yref = "y",
                text = "\u2190 Injection Season (Apr\u2013Oct) \u2192",
                showarrow = FALSE,
                font = list(size = 10, color = "#2980b9")
              ),
              list(
                x = "Jan", y = y_top,
                xref = "x", yref = "y",
                text = "Withdrawal",
                showarrow = FALSE,
                font = list(size = 10, color = "#c0392b")
              ),
              list(
                x = "Dec", y = y_top,
                xref = "x", yref = "y",
                text = "Withdrawal",
                showarrow = FALSE,
                font = list(size = 10, color = "#c0392b")
              )
            )
          )
        }

        # RB summer driving + RVP annotation
        if (input$energy_market == "RB") {
          y_top <- max(abs(idx$pct), na.rm = TRUE) * 1.5
          p <- p |> plotly::layout(
            annotations = list(list(
              x = "Jun", y = y_top,
              xref = "x", yref = "y",
              text = "\u2190 Summer Driving Season \u2192",
              showarrow = FALSE,
              font = list(size = 10, color = "#8e44ad")
            ), list(
              x = "Apr", y = y_top * 0.8,
              xref = "x", yref = "y",
              text = "RVP\ntransition",
              showarrow = FALSE,
              font = list(size = 9, color = "#e67e22")
            ))
          )
        }

        p |> plotly::layout(
          title  = paste0(market_label(), " \u2014 Average Monthly Return (", series_id(), ")"),
          xaxis  = list(title = "Month", categoryorder = "array",
            categoryarray = month.abb),
          yaxis  = list(title = "Mean Log Return (%)")
        )

      } else if (input$view_type == "overlay") {

        df <- returns() |>
          dplyr::filter(!is.na(.data$log_return)) |>
          dplyr::mutate(
            year  = lubridate::year(.data$date),
            month = lubridate::month(.data$date)
          )

        df_cum <- df |>
          dplyr::group_by(.data$year, .data$month) |>
          dplyr::summarise(monthly_ret = sum(.data$log_return, na.rm = TRUE),
            .groups = "drop") |>
          dplyr::group_by(.data$year) |>
          dplyr::mutate(cum_ret = cumsum(.data$monthly_ret) * 100) |>
          dplyr::ungroup()

        years    <- sort(unique(df_cum$year))
        pal      <- grDevices::colorRampPalette(
          c("#440154", "#31688e", "#35b779", "#fde725")
        )(length(years))
        avg_path <- df_cum |>
          dplyr::group_by(.data$month) |>
          dplyr::summarise(avg_cum = mean(.data$cum_ret, na.rm = TRUE), .groups = "drop")

        p <- plotly::plot_ly()
        for (i in seq_along(years)) {
          yr_data <- dplyr::filter(df_cum, .data$year == years[i])
          p <- plotly::add_lines(p,
            data       = yr_data,
            x          = ~month,
            y          = ~cum_ret,
            name       = as.character(years[i]),
            line       = list(color = pal[i], width = 1),
            showlegend = FALSE,
            hovertemplate = paste0(years[i], " \u2014 Month %{x}: %{y:.1f}%<extra></extra>")
          )
        }
        p |>
          plotly::add_lines(data = avg_path, x = ~month, y = ~avg_cum,
            name = "Average",
            line = list(color = "black", width = 3, dash = "dash"),
            hovertemplate = "Avg \u2014 Month %{x}: %{y:.1f}%<extra></extra>"
          ) |>
          plotly::layout(
            title  = paste0(market_label(),
              " \u2014 Year Overlay Cumulative Returns (", series_id(), ")"),
            xaxis  = list(title = "Month", tickvals = 1:12, ticktext = month.abb),
            yaxis  = list(title = "Cumulative Log Return (%)")
          )

      } else if (input$view_type == "vol") {

        vol_df <- returns() |>
          dplyr::arrange(.data$date) |>
          dplyr::mutate(
            rolling_vol = compute_rolling_vol(.data$log_return, window = 21) * 100,
            month       = lubridate::month(.data$date)
          ) |>
          dplyr::filter(!is.na(.data$rolling_vol)) |>
          dplyr::group_by(.data$month) |>
          dplyr::summarise(mean_vol = mean(.data$rolling_vol, na.rm = TRUE),
            .groups = "drop")

        vol_df$month_name <- factor(month.abb[vol_df$month], levels = month.abb)

        plotly::plot_ly(vol_df,
          x    = ~month_name,
          y    = ~mean_vol,
          type = "bar",
          marker = list(
            color      = ~mean_vol,
            colorscale = list(
              list(0, "#1a9850"), list(0.5, "#ffffbf"), list(1, "#d73027")
            ),
            showscale = TRUE,
            colorbar  = list(title = "Vol %")
          ),
          hovertemplate = "%{x}: %{y:.1f}%<extra></extra>"
        ) |> plotly::layout(
          title  = paste0(market_label(),
            " \u2014 Seasonal Volatility by Month (", series_id(), ")"),
          xaxis  = list(title = "Month", categoryorder = "array",
            categoryarray = month.abb),
          yaxis  = list(title = "Mean 21-day Rolling Vol (% ann.)")
        )

      } else {
        # Calendar Spread seasonality: avg M1-M2 by calendar month
        shiny::req(energy_data())

        wide <- tryCatch(
          pivot_wide(energy_data(), input$energy_market),
          error = function(e) NULL
        )
        shiny::req(wide, "c1" %in% names(wide), "c2" %in% names(wide))

        cal_df <- dplyr::tibble(
          date   = wide$date,
          spread = wide$c1 - wide$c2
        ) |>
          dplyr::filter(!is.na(.data$spread)) |>
          dplyr::mutate(month = lubridate::month(.data$date)) |>
          dplyr::group_by(.data$month) |>
          dplyr::summarise(
            mean_spread = mean(.data$spread, na.rm = TRUE),
            sd_spread   = stats::sd(.data$spread, na.rm = TRUE),
            .groups     = "drop"
          ) |>
          dplyr::mutate(
            month_name = factor(month.abb[.data$month], levels = month.abb),
            color      = ifelse(.data$mean_spread >= 0, "#27ae60", "#e74c3c")
          )

        plotly::plot_ly(cal_df,
          x       = ~month_name,
          y       = ~mean_spread,
          type    = "bar",
          marker  = list(color = ~color),
          error_y = list(type = "data", array = ~sd_spread, visible = TRUE,
            color = "grey40"),
          hovertemplate = "%{x}: $%{y:.3f}<extra></extra>"
        ) |>
          plotly::layout(
            title  = paste0(market_label(),
              " \u2014 Average M1\u2212M2 Calendar Spread by Month"),
            xaxis  = list(title = "Month", categoryorder = "array",
              categoryarray = month.abb),
            yaxis  = list(title = "Avg M1 \u2212 M2 Spread ($/unit)"),
            annotations = list(list(
              x         = 0.5, y = 1.06,
              xref      = "paper", yref = "paper",
              text      = "Green = typically backwardated | Red = typically in contango",
              showarrow = FALSE,
              font      = list(size = 10, color = "grey50")
            ))
          )
      }
    })
  })
}
