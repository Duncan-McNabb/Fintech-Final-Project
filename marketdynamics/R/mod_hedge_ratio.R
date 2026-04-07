#' hedge_ratio UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_hedge_ratio_ui <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header(
        shiny::tagList(bsicons::bs_icon("shield-check"), " Hedge Ratio Dynamics")
      ),
      bslib::card_body(
        # Row 1: data controls
        shiny::fluidRow(
          shiny::column(5,
            shiny::dateRangeInput(ns("date_range"), "Date Range",
              start = "2007-01-02", end = Sys.Date(),
              min   = "2007-01-02", max = Sys.Date())
          ),
          shiny::column(5,
            shiny::sliderInput(ns("window"), "Rolling Window (days)",
              min = 21, max = 252, value = 63, step = 1)
          ),
          shiny::column(2,
            shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
          )
        ),
        # Row 2: analysis controls
        shiny::fluidRow(
          shiny::column(4,
            shiny::conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'rolling' || input['%s'] == 'scatter'",
                ns("view_type"), ns("view_type")),
              shiny::selectInput(ns("hedge"), "Hedge Instrument (X)", choices = NULL)
            )
          ),
          shiny::column(4,
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
          shiny::column(4,
            shiny::radioButtons(ns("view_type"), "View",
              choices  = c(
                "Rolling HR" = "rolling",
                "Scatter"    = "scatter"
              ),
              inline   = TRUE,
              selected = "rolling")
          )
        )
      )
    ),
    shiny::uiOutput(ns("plot_context")),
    bslib::card(
      bslib::card_body(
        plotly::plotlyOutput(ns("hr_plot"), height = "50vh")
      )
    ),
    shiny::uiOutput(ns("market_context"))
  )
}

#' hedge_ratio Server Function
#'
#' Loads energy + CMT data locally and renders hedge ratio and crack spread charts.
#' Reads r$market to sync the market selector.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object.
#' @return None.
#' @export
mod_hedge_ratio_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    combined_data <- shiny::reactiveVal(NULL)
    petro_data    <- shiny::reactiveVal(NULL)

    # Best historical hedge instrument for each commodity
    # CL-BRN: tightest energy pair (same barrel, different delivery)
    # HO/RB: crack against crude (refinery margin hedge)
    # NG: most independent; CL is the most liquid cross-hedge
    default_hedge <- c(
      CL  = "BRN01",
      BRN = "CL01",
      HO  = "CL01",
      RB  = "CL01",
      NG  = "CL01"
    )

    # Primary data load: selected market + CMT
    shiny::observeEvent(
      list(r$market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(r$market, input$date_range)

        output$load_status <- shiny::renderUI({
          shiny::tags$small(class = "text-muted", "Loading...")
        })

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        energy <- tryCatch(
          load_energy_data(r$market, start, end),
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

          best <- default_hedge[[r$market]]
          hedge_default <- if (!is.null(best) && best %in% all_series) {
            best
          } else if (length(all_series) > 1) {
            all_series[all_series != paste0(r$market, "01")][1]
          } else {
            all_series[1]
          }
          shiny::updateSelectInput(session, "hedge",
            choices  = all_series,
            selected = hedge_default)

          output$load_status <- shiny::renderUI({ NULL })
        } else {
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-danger", "Error loading data")
          })
        }
      }
    )

    shiny::observeEvent(
      list(r$market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = TRUE, {
        shiny::req(r$market, input$date_range)
        if (!r$market %in% petro_markets) {
          petro_data(NULL)
          return()
        }
        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])
        pd <- tryCatch(
          load_energy_data(petro_markets, start, end) |>
            dplyr::filter(.data$contract_num == 1),
          error = function(e) NULL
        )
        petro_data(pd)
      }
    )

    # Exposure defaults to front-month contract of selected commodity
    exposure_series <- shiny::reactive({
      shiny::req(combined_data(), r$market)
      paste0(r$market, "01")
    })

    aligned_returns <- shiny::reactive({
      shiny::req(combined_data(), exposure_series(), input$hedge)
      all_ret <- compute_log_returns(combined_data())

      y_df <- dplyr::filter(all_ret, .data$series == exposure_series()) |>
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
        dplyr::if_else(sigma_x > 1e-10, rho * (sigma_y / sigma_x), NA_real_)
      }
    })

    market_label <- shiny::reactive({
      unname(market_labels[r$market])
    })

    output$plot_context <- shiny::renderUI({
      shiny::req(input$view_type)
      txt <- switch(input$view_type,
        "rolling" = "The hedge ratio over time, computed using the selected **Method** over a **Rolling Window**. **OLS Beta** is the regression slope of Exposure returns on Hedge returns; **Min Variance** minimizes portfolio variance using rolling correlation and vol. The red dashed line is the trailing 1-year average. A ratio of 1.0 means a 1:1 hedge; values above 1 mean you need more hedge contracts per unit of exposure.",
        "scatter" = "Daily log returns of the Hedge Instrument (x-axis) vs. the Exposure Series (y-axis), colored by year. The dashed line is the current hedge ratio \u2014 its slope shows how many units of hedge are needed per unit of exposure. Tighter scatter around the line = more effective hedge."
      )
      shiny::tags$p(class = "text-muted px-2", style = "font-size:0.85rem;", shiny::HTML(txt))
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
              title  = paste0(market_label(), " \u2014 Hedge Ratio: ", exposure_series(),
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
              exposure_series(), ": %{y:.4f}<extra></extra>"
            )
          ) |>
            plotly::add_lines(data = line_df, x = ~x, y = ~y, inherit = FALSE,
              line       = list(color = "black", width = 2, dash = "dash"),
              name       = sprintf("HR = %.3f", current_hr),
              showlegend = TRUE
            ) |>
            plotly::layout(
              title  = paste0(market_label(), " \u2014 Return Scatter: ",
                exposure_series(), " vs ", input$hedge),
              xaxis  = list(title = paste(input$hedge, "Log Return")),
              yaxis  = list(title = paste(exposure_series(), "Log Return")),
              legend = list(orientation = "h")
            )
        }
    })

    output$market_context <- shiny::renderUI({
      shiny::req(r$market)
      mkt <- r$market

      if (mkt %in% petro_markets) {
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("bar-chart-line"), " Refinery Margin Context")
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(6, 6),
              plotly::plotlyOutput(session$ns("crack_spread_plot"), height = "50vh"),
              plotly::plotlyOutput(session$ns("crack_321_plot"),    height = "50vh")
            ),
            shiny::tags$p(class = "text-muted", style = "font-size:0.9rem;",
              "The 3-2-1 crack spread is the industry benchmark for refinery margin: for every 3 barrels of crude input, a typical US refinery produces approximately 2 barrels of gasoline (RB) and 1 barrel of distillate/diesel (HO). Refiners monitor this spread to decide when to lock in forward margins or adjust crude purchasing. The individual crack spreads (HO crack and RB crack) measure each product\u2019s margin independently and can diverge significantly during seasonal demand shifts. Wide crack spreads attract increased refinery throughput; narrow or negative spreads trigger run cuts and maintenance pull-forwards."
            )
          )
        )

      } else if (mkt == "NG") {
        info_text <- "Natural gas hedges for utilities and industrial consumers often reference the 'spark spread' \u2014 the margin between power output price and natural gas input cost. For a gas-fired power plant: spark_spread = electricity_price / heat_rate \u2212 gas_price. When spark spreads are wide, utilities lock in gas forward prices; when narrow, they may reduce gas hedging. RTL data does not include power prices, so the spark spread is not directly computable here."
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("info-circle"), " Market Context: Natural Gas")
          ),
          bslib::card_body(shiny::tags$p(class = "text-muted", style = "font-size:0.9rem;", info_text))
        )

      } else if (mkt == "BRN") {
        info_text <- "WTI-Brent basis risk is a key consideration when hedging Brent-priced crude exposure with NYMEX WTI futures. The spread has historically ranged from -$5 to +$8/bbl. Post-2018, US crude export infrastructure expansion has tightened the spread. A company with Brent-priced crude revenues hedging with CL futures carries residual basis risk equal to the WTI-Brent spread movement."
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("info-circle"), " Market Context: Brent Crude")
          ),
          bslib::card_body(shiny::tags$p(class = "text-muted", style = "font-size:0.9rem;", info_text))
        )

      } else {
        NULL
      }
    })

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
  })
}
