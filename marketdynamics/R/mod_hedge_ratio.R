#' hedge_ratio UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_hedge_ratio_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::column(3,
        shiny::selectInput(ns("exposure"), "Exposure (Y — what you own)", choices = NULL)
      ),
      shiny::column(3,
        shiny::selectInput(ns("hedge"), "Hedge Instrument (X)", choices = NULL)
      ),
      shiny::column(3,
        shiny::sliderInput(ns("window"), "Rolling Window (days)",
          min = 21, max = 252, value = 63, step = 1)
      ),
      shiny::column(3,
        shiny::radioButtons(ns("method"), "Method",
          choices  = c("OLS Beta" = "ols", "Min Variance" = "minvar"),
          inline   = TRUE,
          selected = "ols"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(4,
        shiny::radioButtons(ns("view_type"), "View",
          choices  = c("Rolling Hedge Ratio" = "rolling", "Return Scatter" = "scatter"),
          inline   = TRUE,
          selected = "rolling"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(4, bslib::value_box(
        title    = "Current Hedge Ratio",
        value    = shiny::uiOutput(ns("vb_current")),
        showcase = bsicons::bs_icon("shield-check"),
        theme    = "primary"
      )),
      shiny::column(4, bslib::value_box(
        title    = "1-Year Average HR",
        value    = shiny::uiOutput(ns("vb_avg")),
        showcase = bsicons::bs_icon("bar-chart"),
        theme    = "secondary"
      )),
      shiny::column(4, bslib::value_box(
        title    = "Deviation from Avg",
        value    = shiny::uiOutput(ns("vb_dev")),
        showcase = bsicons::bs_icon("arrow-up-down"),
        theme    = "info"
      ))
    ),
    plotly::plotlyOutput(ns("hr_plot"), height = "430px")
  )
}

#' hedge_ratio Server Function
#'
#' Reads from `r$combined_long`. Does NOT write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object created in `app_server`.
#' @return None.
#' @export
mod_hedge_ratio_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    # Available series: front-month energy + all CMT
    available_series <- shiny::reactive({
      shiny::req(r$combined_long)
      energy_front <- r$combined_long |>
        dplyr::filter(.data$source == "energy", .data$contract_num == 1) |>
        dplyr::pull(.data$series) |> unique() |> sort()
      cmt <- r$combined_long |>
        dplyr::filter(.data$source == "cmt") |>
        dplyr::pull(.data$series) |> unique() |> sort()
      c(energy_front, cmt)
    })

    shiny::observe({
      shiny::req(available_series())
      s <- available_series()
      shiny::updateSelectInput(session, "exposure", choices = s, selected = s[1])
      shiny::updateSelectInput(session, "hedge",    choices = s,
        selected = if (length(s) > 1) s[2] else s[1])
    })

    # Aligned log returns for the two selected series
    aligned_returns <- shiny::reactive({
      shiny::req(r$combined_long, input$exposure, input$hedge)
      all_ret <- compute_log_returns(r$combined_long)

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

    # Rolling hedge ratio
    hedge_ratio <- shiny::reactive({
      shiny::req(aligned_returns(), input$method, input$window)
      ar <- aligned_returns()
      if (input$method == "ols") {
        compute_rolling_beta(ar$y, ar$x, window = input$window)
      } else {
        # Min-variance: beta = rho * (sigma_y / sigma_x)
        rho     <- compute_rolling_correlation(ar$y, ar$x, window = input$window)
        sigma_y <- compute_rolling_vol(ar$y, window = input$window)
        sigma_x <- compute_rolling_vol(ar$x, window = input$window)
        rho * (sigma_y / sigma_x)
      }
    })

    # Value box helpers
    current_hr <- shiny::reactive({
      hr <- na.omit(hedge_ratio())
      if (length(hr) == 0) return(NA_real_)
      tail(hr, 1)
    })

    avg_1yr_hr <- shiny::reactive({
      hr <- na.omit(hedge_ratio())
      if (length(hr) == 0) return(NA_real_)
      mean(tail(hr, 252), na.rm = TRUE)
    })

    output$vb_current <- shiny::renderUI({
      shiny::req(!is.na(current_hr()))
      shiny::tags$span(sprintf("%.3f", current_hr()))
    })

    output$vb_avg <- shiny::renderUI({
      shiny::req(!is.na(avg_1yr_hr()))
      shiny::tags$span(sprintf("%.3f", avg_1yr_hr()))
    })

    output$vb_dev <- shiny::renderUI({
      shiny::req(!is.na(current_hr()), !is.na(avg_1yr_hr()))
      dev <- current_hr() - avg_1yr_hr()
      shiny::tags$span(sprintf("%+.3f", dev))
    })

    output$hr_plot <- plotly::renderPlotly({
      shiny::req(aligned_returns(), hedge_ratio(), input$view_type)

      ar <- aligned_returns()
      hr <- hedge_ratio()
      df <- dplyr::tibble(date = ar$date, hedge_ratio = hr) |>
        dplyr::filter(!is.na(hedge_ratio))

      if (input$view_type == "rolling") {
        avg_line <- mean(tail(df$hedge_ratio, 252), na.rm = TRUE)

        plotly::plot_ly(df, x = ~date, y = ~hedge_ratio,
          type = "scatter", mode = "lines",
          line = list(color = "#2c7bb6", width = 1.5),
          name = "Hedge Ratio",
          hovertemplate = "Date: %{x}<br>HR: %{y:.3f}<extra></extra>"
        ) |>
          plotly::add_lines(y = avg_line, line = list(color = "#e74c3c", dash = "dash"),
            name = "1-yr Avg", showlegend = TRUE,
            hoverinfo = "none") |>
          plotly::layout(
            title  = paste0("Rolling Hedge Ratio — ", input$exposure, " vs ", input$hedge,
              "\n(", input$window, "-day ", toupper(input$method), ")"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Hedge Ratio"),
            legend = list(orientation = "h")
          )

      } else {
        # Return scatter
        scatter_df <- ar |>
          dplyr::mutate(year = as.character(lubridate::year(.data$date)))

        years  <- sort(unique(scatter_df$year))
        n_yr   <- length(years)
        pal    <- grDevices::colorRampPalette(c("#440154", "#31688e", "#35b779", "#fde725"))(n_yr)
        color_map <- setNames(pal, years)

        current <- if (!is.na(current_hr())) current_hr() else 0
        x_range <- range(scatter_df$x, na.rm = TRUE)
        line_df <- dplyr::tibble(
          x = x_range,
          y = current * x_range
        )

        plotly::plot_ly(scatter_df,
          x     = ~x,
          y     = ~y,
          color = ~year,
          colors = color_map,
          type  = "scatter",
          mode  = "markers",
          marker = list(size = 4, opacity = 0.6),
          hovertemplate = paste0(
            input$hedge, ": %{x:.4f}<br>",
            input$exposure, ": %{y:.4f}<extra></extra>"
          )
        ) |>
          plotly::add_lines(data = line_df, x = ~x, y = ~y, inherit = FALSE,
            line = list(color = "black", width = 2, dash = "dash"),
            name = sprintf("HR = %.3f", current), showlegend = TRUE
          ) |>
          plotly::layout(
            title  = paste0("Return Scatter: ", input$exposure, " vs ", input$hedge),
            xaxis  = list(title = paste(input$hedge, "Log Return")),
            yaxis  = list(title = paste(input$exposure, "Log Return")),
            legend = list(orientation = "h")
          )
      }
    })
  })
}
