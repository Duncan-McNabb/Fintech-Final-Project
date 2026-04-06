#' simulation UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_simulation_ui <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header(
        shiny::tagList(bsicons::bs_icon("bezier2"), " Price Simulation")
      ),
      bslib::card_body(
        shiny::fluidRow(
          shiny::column(6,
            shiny::dateRangeInput(ns("date_range"), "Estimation Window",
              start = "2015-01-01", end = Sys.Date(),
              min   = "2007-01-02", max = Sys.Date())
          ),
          shiny::column(4,
            shiny::radioButtons(ns("model"), "Model",
              choices  = c(
                "Geometric Brownian Motion" = "gbm",
                "Mean Reversion (OU)"       = "ou"
              ),
              inline   = FALSE,
              selected = "gbm")
          ),
          shiny::column(2,
            shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
          )
        ),
        shiny::fluidRow(
          shiny::column(3,
            shiny::sliderInput(ns("horizon_weeks"), "Horizon (weeks)",
              min = 4, max = 104, value = 52, step = 4)
          ),
          shiny::column(3,
            shiny::sliderInput(ns("n_paths"), "Simulated Paths",
              min = 50, max = 500, value = 200, step = 50)
          ),
          shiny::column(3,
            shiny::numericInput(ns("seed"), "Random Seed",
              value = 42, min = 1, max = 9999, step = 1)
          ),
          shiny::column(3,
            shiny::tags$div(class = "mt-4",
              shiny::actionButton(ns("run_sim"), "Run Simulation",
                class = "btn-primary btn-sm")
            )
          )
        )
      )
    ),
    shiny::uiOutput(ns("plot_context")),
    bslib::card(
      bslib::card_body(
        plotly::plotlyOutput(ns("sim_plot"), height = "50vh")
      )
    ),
    bslib::card(
      bslib::card_header(
        shiny::tagList(bsicons::bs_icon("table"), " Estimated Parameters")
      ),
      bslib::card_body(
        shiny::uiOutput(ns("param_table"))
      )
    )
  )
}

#' simulation Server Function
#'
#' Fits GBM or Ornstein-Uhlenbeck model to front-month energy returns,
#' simulates forward price paths, and renders percentile fan chart.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object.
#' @return None.
#' @export
mod_simulation_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    energy_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      list(r$market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(r$market, input$date_range)

        output$load_status <- shiny::renderUI({
          shiny::tags$small(class = "text-muted", "Loading...")
        })

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        data <- tryCatch(
          load_energy_data(r$market, start, end),
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

    market_label <- shiny::reactive({
      unname(market_labels[r$market])
    })

    # Front-month log returns
    returns <- shiny::reactive({
      shiny::req(energy_data())
      series_id <- paste0(r$market, "01")
      energy_data() |>
        dplyr::filter(.data$series == series_id, !is.na(.data$value)) |>
        dplyr::arrange(.data$date) |>
        compute_log_returns() |>
        dplyr::filter(!is.na(.data$log_return))
    })

    # Fit model parameters
    params <- shiny::reactive({
      shiny::req(returns())
      r_vec <- returns()$log_return
      p_vec <- returns()$value

      if (input$model == "gbm") {
        mu_daily    <- mean(r_vec)
        sigma_daily <- stats::sd(r_vec)
        list(
          mu          = mu_daily * 252,
          sigma       = sigma_daily * sqrt(252),
          mu_daily    = mu_daily,
          sigma_daily = sigma_daily,
          model       = "gbm"
        )
      } else {
        # OU on log prices: fit AR(1)  X(t+1) = a + b*X(t) + eps
        log_p  <- log(p_vec)
        n      <- length(log_p)
        x_lag  <- log_p[-n]
        x_now  <- log_p[-1]
        fit    <- stats::lm(x_now ~ x_lag)
        a      <- stats::coef(fit)[1]
        b      <- stats::coef(fit)[2]
        resid_sd <- stats::sd(stats::residuals(fit))

        # Clamp b to (0, 1) for a valid stationary OU process
        # b >= 1 → non-stationary (unit root); b <= 0 → explosive/invalid
        b_clamped <- min(max(b, 0.001), 0.999)

        # Daily AR(1) coefficients are the simulation primitives
        # Annualised versions are for display only
        theta_ann <- -log(b_clamped) * 252
        mu_eq     <- a / (1 - b_clamped)    # long-run equilibrium log price
        sigma_ann <- resid_sd * sqrt(252)   # annualized diffusion
        half_life <- -log(2) / log(b_clamped)

        list(
          theta       = theta_ann,
          mu_eq       = mu_eq,
          mu_eq_price = exp(mu_eq),
          sigma       = sigma_ann,
          b           = b_clamped,
          b_raw       = b,
          a           = a,
          resid_sd    = resid_sd,
          half_life   = half_life,
          model       = "ou"
        )
      }
    })

    # Simulation — triggered by button or parameter changes
    sim_result <- shiny::eventReactive(
      list(input$run_sim, returns(), input$model,
           input$horizon_weeks, input$n_paths, input$seed),
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(returns(), params())

        set.seed(input$seed)
        p      <- params()
        prices <- returns()$value
        s0     <- tail(prices, 1)        # last observed price
        n_days <- round(input$horizon_weeks * 5)  # trading days
        dt     <- 1 / 252
        n_sim  <- input$n_paths

        # Simulate paths matrix: rows = time steps, cols = paths
        sim_mat <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sim)
        sim_mat[1, ] <- s0

        if (p$model == "gbm") {
          drift <- (p$mu_daily - 0.5 * p$sigma_daily^2)
          for (t in seq_len(n_days)) {
            z <- stats::rnorm(n_sim)
            sim_mat[t + 1, ] <- sim_mat[t, ] *
              exp(drift + p$sigma_daily * z)
          }
        } else {
          # Exact discrete AR(1) simulation on log prices:
          # X(t+1) = a + b * X(t) + resid_sd * Z
          # This is the exact solution — no Euler approximation needed.
          log_s0 <- log(s0)
          log_mat <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sim)
          log_mat[1, ] <- log_s0
          for (t in seq_len(n_days)) {
            z <- stats::rnorm(n_sim)
            log_mat[t + 1, ] <- p$a + p$b * log_mat[t, ] + p$resid_sd * z
          }
          sim_mat <- exp(log_mat)
        }

        # Build dates: business days from last observation
        last_date   <- tail(returns()$date, 1)
        future_dates <- seq(last_date, by = "day", length.out = n_days * 2)
        future_dates <- future_dates[!weekdays(future_dates) %in% c("Saturday", "Sunday")]
        future_dates <- future_dates[seq_len(n_days + 1)]

        list(mat = sim_mat, dates = future_dates, s0 = s0)
      }
    )

    output$plot_context <- shiny::renderUI({
      shiny::req(input$model)
      txt <- switch(input$model,
        "gbm" = "Geometric Brownian Motion assumes log returns are i.i.d. normal with constant drift and volatility. Parameters are estimated from the historical window. The fan chart shows the 10th\u201390th and 25th\u201375th percentile range of simulated paths. GBM has no memory \u2014 price can drift arbitrarily far from the current level.",
        "ou"  = "The Ornstein-Uhlenbeck (mean-reversion) model pulls the log price back toward a long-run equilibrium. It fits an AR(1) to log prices to estimate the reversion speed (\u03b8), equilibrium price, and diffusion (\u03c3). Useful for commodities with fundamental anchors (storage costs, marginal cost of production). The fan is narrower than GBM when reversion is strong."
      )
      shiny::tags$p(class = "text-muted px-2", style = "font-size:0.85rem;", shiny::HTML(txt))
    })

    output$sim_plot <- plotly::renderPlotly({
      shiny::req(sim_result())
      res   <- sim_result()
      mat   <- res$mat
      dates <- res$dates

      # Compute percentiles across paths at each time step
      pct_df <- data.frame(
        date = dates,
        p10  = apply(mat, 1, stats::quantile, 0.10, na.rm = TRUE),
        p25  = apply(mat, 1, stats::quantile, 0.25, na.rm = TRUE),
        p50  = apply(mat, 1, stats::quantile, 0.50, na.rm = TRUE),
        p75  = apply(mat, 1, stats::quantile, 0.75, na.rm = TRUE),
        p90  = apply(mat, 1, stats::quantile, 0.90, na.rm = TRUE)
      )

      # Historical tail (last 90 trading days) for context
      hist_df <- returns() |>
        dplyr::arrange(.data$date) |>
        tail(90) |>
        dplyr::select(date, value)

      model_name <- if (input$model == "gbm") "GBM" else "Mean Reversion (OU)"

      plotly::plot_ly() |>
        # Outer ribbon: 10-90
        plotly::add_ribbons(
          data      = pct_df, x = ~date, ymin = ~p10, ymax = ~p90,
          fillcolor = "rgba(44,114,184,0.10)",
          line      = list(color = "transparent"),
          name      = "10\u201390th pct",
          showlegend = TRUE, hoverinfo = "none"
        ) |>
        # Inner ribbon: 25-75
        plotly::add_ribbons(
          data      = pct_df, x = ~date, ymin = ~p25, ymax = ~p75,
          fillcolor = "rgba(44,114,184,0.20)",
          line      = list(color = "transparent"),
          name      = "25\u201375th pct",
          showlegend = TRUE, hoverinfo = "none"
        ) |>
        # Median path
        plotly::add_lines(
          data          = pct_df, x = ~date, y = ~p50,
          name          = "Median path",
          line          = list(color = "#2c72b8", width = 2),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>Median: $%{y:.2f}<extra></extra>"
        ) |>
        # Historical price
        plotly::add_lines(
          data          = hist_df, x = ~date, y = ~value,
          name          = "Historical",
          line          = list(color = "#2c3e50", width = 1.8),
          hovertemplate = "Date: %{x|%Y-%m-%d}<br>Price: $%{y:.2f}<extra></extra>"
        ) |>
        # Starting point marker
        plotly::add_markers(
          x             = res$dates[1],
          y             = res$s0,
          name          = "Start",
          marker        = list(color = "#e74c3c", size = 8, symbol = "circle"),
          hovertemplate = paste0("Start: $", round(res$s0, 2), "<extra></extra>"),
          showlegend    = TRUE
        ) |>
        plotly::layout(
          title  = paste0(market_label(), " \u2014 ", model_name,
            " Price Simulation (", input$n_paths, " paths, ",
            input$horizon_weeks, " weeks)"),
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Price ($/unit)"),
          legend = list(orientation = "h", x = 0, y = 1.08)
        )
    })

    output$param_table <- shiny::renderUI({
      shiny::req(params(), returns())
      p      <- params()
      s0     <- tail(returns()$value, 1)
      n_obs  <- nrow(returns())
      start  <- format(min(returns()$date), "%Y-%m-%d")
      end    <- format(max(returns()$date), "%Y-%m-%d")

      if (p$model == "gbm") {
        rows <- list(
          c("Estimation window",       paste0(start, " \u2013 ", end, " (", n_obs, " obs)")),
          c("Last observed price",     sprintf("$%.3f", s0)),
          c("Annual drift (\u03bc)",   sprintf("%.1f%%", p$mu * 100)),
          c("Annual volatility (\u03c3)", sprintf("%.1f%%", p$sigma * 100)),
          c("1-year 90th pct (est.)",  sprintf("$%.2f", s0 * exp(p$mu + 1.645 * p$sigma))),
          c("1-year 10th pct (est.)",  sprintf("$%.2f", s0 * exp(p$mu - 1.645 * p$sigma)))
        )
      } else {
        hl_txt <- if (!is.na(p$half_life)) sprintf("%.0f trading days", p$half_life) else "Non-stationary"
        rows <- list(
          c("Estimation window",           paste0(start, " \u2013 ", end, " (", n_obs, " obs)")),
          c("Last observed price",         sprintf("$%.3f", s0)),
          c("Equilibrium price (\u03bc\u2091\u2091)", sprintf("$%.2f", p$mu_eq_price)),
          c("Mean reversion speed (\u03b8)",  sprintf("%.2f (annualized)", p$theta)),
          c("Half-life",                    hl_txt),
          c("Annual diffusion (\u03c3)",      sprintf("%.1f%%", p$sigma * 100)),
          c("AR(1) coefficient (b)",        paste0(sprintf("%.4f", p$b_raw),
            if (p$b_raw != p$b) sprintf(" (clamped to %.3f)", p$b) else ""))
        )
      }

      tbl_rows <- lapply(rows, function(r) {
        shiny::tags$tr(
          shiny::tags$td(style = "color: #666; font-size:0.85rem; padding: 4px 12px;", r[1]),
          shiny::tags$td(style = "font-weight:600; font-size:0.85rem; padding: 4px 12px;", r[2])
        )
      })

      shiny::tags$table(
        class = "table table-sm table-borderless",
        style = "max-width:500px;",
        shiny::tags$tbody(tbl_rows)
      )
    })
  })
}
