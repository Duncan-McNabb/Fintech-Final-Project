#' volatility UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_volatility_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::column(3,
        shiny::radioButtons(ns("market_type"), "Market",
          choices  = c("Energy" = "energy", "Treasuries" = "cmt"),
          inline   = TRUE,
          selected = "energy"
        )
      ),
      shiny::column(3,
        shiny::selectInput(ns("ticker"), "Series", choices = NULL)
      ),
      shiny::column(3,
        shiny::sliderInput(ns("vol_window"), "Rolling Window (days)",
          min = 5, max = 63, value = 21, step = 1)
      ),
      shiny::column(3,
        shiny::radioButtons(ns("view_type"), "View",
          choices = c(
            "Rolling Vol" = "rolling",
            "Vol Surface" = "surface",
            "Term Structure" = "term"
          ),
          selected = "rolling"
        )
      )
    ),
    plotly::plotlyOutput(ns("vol_plot"), height = "550px")
  )
}

#' volatility Server Function
#'
#' Reads from `r$energy_long` and `r$cmt_long`. Does NOT write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object created in `app_server`.
#' @return None.
#' @export
mod_volatility_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    # Update ticker choices when market type or data changes
    shiny::observe({
      if (input$market_type == "energy") {
        shiny::req(r$energy_long)
        choices <- sort(unique(r$energy_long$series))
      } else {
        shiny::req(r$cmt_long)
        choices <- sort(unique(r$cmt_long$series))
      }
      shiny::updateSelectInput(session, "ticker", choices = choices,
        selected = choices[1])
    })

    # Log returns for the active dataset
    returns_data <- shiny::reactive({
      if (input$market_type == "energy") {
        shiny::req(r$energy_long)
        compute_log_returns(r$energy_long)
      } else {
        shiny::req(r$cmt_long)
        compute_log_returns(r$cmt_long)
      }
    })

    output$vol_plot <- plotly::renderPlotly({
      shiny::req(returns_data(), input$ticker, input$view_type, input$vol_window)

      if (input$view_type == "rolling") {
        # Rolling vol for the selected series
        ser <- dplyr::filter(returns_data(), .data$series == input$ticker) |>
          dplyr::arrange(.data$date) |>
          dplyr::mutate(
            rolling_vol = compute_rolling_vol(.data$log_return, window = input$vol_window)
          ) |>
          dplyr::filter(!is.na(.data$rolling_vol))

        plotly::plot_ly(ser, x = ~date, y = ~rolling_vol * 100, type = "scatter",
          mode = "lines", line = list(color = "#2c3e50", width = 1.5),
          hovertemplate = "Date: %{x}<br>Vol: %{y:.1f}%<extra></extra>"
        ) |>
          plotly::layout(
            title  = paste0("Rolling Realized Volatility — ", input$ticker,
              " (", input$vol_window, "-day window)"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Annualized Volatility (%)")
          )

      } else if (input$view_type == "surface") {
        # Vol surface heatmap across all series in the active dataset
        all_series <- unique(returns_data()$series)
        surf <- compute_vol_surface(returns_data(), all_series, window = input$vol_window)

        # Pivot to matrix for heatmap
        wide <- tidyr::pivot_wider(surf, names_from = series, values_from = rolling_vol)
        z_mat <- t(as.matrix(wide[, -1])) * 100  # annualized %

        plotly::plot_ly(
          x          = wide$date,
          y          = colnames(as.matrix(wide[, -1])),
          z          = z_mat,
          type       = "heatmap",
          colorscale = list(
            list(0, "#1a9850"), list(0.5, "#ffffbf"), list(1, "#d73027")
          ),
          hovertemplate = "Date: %{x}<br>Series: %{y}<br>Vol: %{z:.1f}%<extra></extra>"
        ) |>
          plotly::layout(
            title = paste0("Volatility Surface (", input$vol_window, "-day rolling)"),
            xaxis = list(title = "Date"),
            yaxis = list(title = "Series")
          )

      } else {
        # Term structure: average vol by contract number (Samuelson Effect)
        shiny::req(input$market_type == "energy", r$energy_long)

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
          dplyr::summarise(mean_vol = mean(.data$rolling_vol, na.rm = TRUE) * 100,
            .groups = "drop") |>
          dplyr::arrange(.data$contract_num)

        plotly::plot_ly(term, x = ~contract_num, y = ~mean_vol, type = "bar",
          marker = list(color = "#2c7bb6"),
          hovertemplate = "Contract: %{x}<br>Avg Vol: %{y:.1f}%<extra></extra>"
        ) |>
          plotly::layout(
            title       = "Term Structure of Volatility (Samuelson Effect)",
            xaxis       = list(title = "Contract Month"),
            yaxis       = list(title = "Average Annualized Volatility (%)"),
            annotations = list(list(
              x = 0.5, y = 1.05, xref = "paper", yref = "paper",
              text = "Near-term contracts are typically more volatile (Samuelson Effect)",
              showarrow = FALSE, font = list(size = 11, color = "grey50")
            ))
          )
      }
    })
  })
}
