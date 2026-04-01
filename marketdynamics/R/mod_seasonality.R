#' seasonality UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_seasonality_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(ns("series"), "Series (front month)", choices = NULL)
      ),
      shiny::column(8,
        shiny::radioButtons(ns("view_type"), "View",
          choices = c(
            "Monthly Returns"  = "monthly",
            "Year Overlay"     = "overlay",
            "Seasonal Vol"     = "vol"
          ),
          inline   = TRUE,
          selected = "monthly"
        )
      )
    ),
    plotly::plotlyOutput(ns("seas_plot"), height = "540px")
  )
}

#' seasonality Server Function
#'
#' Reads from `r$energy_long`. Does NOT write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object created in `app_server`.
#' @return None.
#' @export
mod_seasonality_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    # Populate series choices from front-month energy series
    shiny::observe({
      shiny::req(r$energy_long)
      front_series <- r$energy_long |>
        dplyr::filter(.data$contract_num == 1) |>
        dplyr::pull(.data$series) |>
        unique() |>
        sort()
      shiny::updateSelectInput(session, "series", choices = front_series,
        selected = front_series[1])
    })

    # Log returns for the selected series
    returns <- shiny::reactive({
      shiny::req(r$energy_long, input$series)
      r$energy_long |>
        dplyr::filter(.data$series == input$series) |>
        compute_log_returns()
    })

    output$seas_plot <- plotly::renderPlotly({
      shiny::req(returns(), input$view_type)

      if (input$view_type == "monthly") {
        idx <- compute_seasonal_index(returns(), input$series, freq = "month")
        idx$month_name <- month.abb[idx$period]
        idx$month_name <- factor(idx$month_name, levels = month.abb)
        idx$color      <- ifelse(idx$mean_return >= 0, "#27ae60", "#e74c3c")
        idx$pct        <- idx$mean_return * 100
        idx$se_pct     <- idx$se_return  * 100

        plotly::plot_ly(idx,
          x            = ~month_name,
          y            = ~pct,
          type         = "bar",
          marker       = list(color = ~color),
          error_y      = list(type = "data", array = ~se_pct, visible = TRUE,
            color = "grey40"),
          hovertemplate = "%{x}: %{y:.2f}%<extra></extra>"
        ) |>
          plotly::layout(
            title  = paste0("Average Monthly Return — ", input$series),
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
          ) |>
          dplyr::arrange(.data$year, .data$month, .data$date)

        # Cumulative returns within each year, indexed to month
        df_cum <- df |>
          dplyr::group_by(.data$year, .data$month) |>
          dplyr::summarise(monthly_ret = sum(.data$log_return, na.rm = TRUE),
            .groups = "drop") |>
          dplyr::group_by(.data$year) |>
          dplyr::mutate(cum_ret = cumsum(.data$monthly_ret) * 100) |>
          dplyr::ungroup()

        years  <- sort(unique(df_cum$year))
        n_yr   <- length(years)
        pal    <- grDevices::colorRampPalette(c("#440154", "#31688e", "#35b779", "#fde725"))(n_yr)

        # Average path
        avg_path <- df_cum |>
          dplyr::group_by(.data$month) |>
          dplyr::summarise(avg_cum = mean(.data$cum_ret, na.rm = TRUE), .groups = "drop")

        p <- plotly::plot_ly()
        for (i in seq_along(years)) {
          yr_data <- dplyr::filter(df_cum, year == years[i])
          p <- plotly::add_lines(p,
            data       = yr_data,
            x          = ~month,
            y          = ~cum_ret,
            name       = as.character(years[i]),
            line       = list(color = pal[i], width = 1),
            showlegend = FALSE,
            hovertemplate = paste0(years[i], " — Month %{x}: %{y:.1f}%<extra></extra>")
          )
        }
        p |>
          plotly::add_lines(data = avg_path, x = ~month, y = ~avg_cum,
            name = "Average", line = list(color = "black", width = 3, dash = "dash"),
            hovertemplate = "Avg — Month %{x}: %{y:.1f}%<extra></extra>"
          ) |>
          plotly::layout(
            title  = paste0("Year Overlay — ", input$series, " Cumulative Returns"),
            xaxis  = list(title = "Month", tickvals = 1:12, ticktext = month.abb),
            yaxis  = list(title = "Cumulative Log Return (%)")
          )

      } else {
        # Seasonal Vol
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
            color    = ~mean_vol,
            colorscale = list(
              list(0, "#1a9850"), list(0.5, "#ffffbf"), list(1, "#d73027")
            ),
            showscale = TRUE,
            colorbar  = list(title = "Vol %")
          ),
          hovertemplate = "%{x}: %{y:.1f}%<extra></extra>"
        ) |>
          plotly::layout(
            title  = paste0("Average Realized Volatility by Month — ", input$series),
            xaxis  = list(title = "Month", categoryorder = "array",
              categoryarray = month.abb),
            yaxis  = list(title = "Mean 21-day Rolling Vol (% ann.)")
          )
      }
    })
  })
}
