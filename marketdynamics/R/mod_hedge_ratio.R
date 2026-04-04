#' hedge_ratio UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_hedge_ratio_ui <- function(id) {
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
      shiny::tagList(bsicons::bs_icon("shield-check"), " Hedge Ratio Dynamics")
    ),
    bslib::card_body(
      # Row 1: data controls
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
          shiny::sliderInput(ns("window"), "Rolling Window (days)",
            min = 21, max = 252, value = 63, step = 1)
        ),
        shiny::column(2,
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      # Row 2: analysis controls
      shiny::fluidRow(
        shiny::column(3,
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'rolling' || input['%s'] == 'scatter'",
              ns("view_type"), ns("view_type")),
            shiny::selectInput(ns("exposure"), "Exposure Series (Y)", choices = NULL)
          )
        ),
        shiny::column(3,
          shiny::conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'rolling' || input['%s'] == 'scatter'",
              ns("view_type"), ns("view_type")),
            shiny::selectInput(ns("hedge"), "Hedge Instrument (X)", choices = NULL)
          )
        ),
        shiny::column(3,
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
        shiny::column(3,
          shiny::radioButtons(ns("view_type"), "View",
            choices  = c(
              "Rolling HR" = "rolling",
              "Scatter"    = "scatter"
            ),
            inline   = TRUE,
            selected = "rolling")
        )
      ),
      shiny::tags$hr(),
      plotly::plotlyOutput(ns("hr_plot"), height = "calc(100vh - 340px)"),
      shiny::tags$hr(),
      shiny::uiOutput(ns("market_context"))
    )
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

    # Primary data load: selected market + CMT
    shiny::observeEvent(
      list(input$energy_market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(input$energy_market, input$date_range)

        output$load_status <- shiny::renderUI({
          shiny::tags$small(class = "text-muted", "Loading...")
        })

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        energy <- tryCatch(
          load_energy_data(input$energy_market, start, end),
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

          shiny::updateSelectInput(session, "exposure",
            choices = all_series, selected = all_series[1])
          shiny::updateSelectInput(session, "hedge",
            choices  = all_series,
            selected = if (length(all_series) > 1) all_series[2] else all_series[1])

          output$load_status <- shiny::renderUI({ NULL })
        } else {
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-danger", "Error loading data")
          })
        }
      }
    )

    shiny::observeEvent(
      list(input$energy_market, input$date_range),
      ignoreNULL = TRUE, ignoreInit = TRUE, {
        shiny::req(input$energy_market, input$date_range)
        if (!input$energy_market %in% c("CL", "HO", "RB")) {
          petro_data(NULL)
          return()
        }
        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])
        pd <- tryCatch(
          load_energy_data(c("CL", "HO", "RB"), start, end) |>
            dplyr::filter(.data$contract_num == 1),
          error = function(e) NULL
        )
        petro_data(pd)
      }
    )

    shiny::observeEvent(r$market, {
      shiny::req(r$market)
      shiny::updateSelectInput(session, "energy_market", selected = r$market)
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    aligned_returns <- shiny::reactive({
      shiny::req(combined_data(), input$exposure, input$hedge)
      all_ret <- compute_log_returns(combined_data())

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

    hedge_ratio <- shiny::reactive({
      shiny::req(aligned_returns(), input$method, input$window)
      ar <- aligned_returns()
      if (input$method == "ols") {
        compute_rolling_beta(ar$y, ar$x, window = input$window)
      } else {
        rho     <- compute_rolling_correlation(ar$y, ar$x, window = input$window)
        sigma_y <- compute_rolling_vol(ar$y, window = input$window)
        sigma_x <- compute_rolling_vol(ar$x, window = input$window)
        rho * (sigma_y / sigma_x)
      }
    })

    market_label <- shiny::reactive({
      labels <- c(CL = "WTI Crude", BRN = "Brent Crude", NG = "Natural Gas",
                  HO = "Heating Oil", RB = "RBOB Gasoline")
      unname(labels[input$energy_market])
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
              title  = paste0(market_label(), " \u2014 Hedge Ratio: ", input$exposure,
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
              input$exposure, ": %{y:.4f}<extra></extra>"
            )
          ) |>
            plotly::add_lines(data = line_df, x = ~x, y = ~y, inherit = FALSE,
              line       = list(color = "black", width = 2, dash = "dash"),
              name       = sprintf("HR = %.3f", current_hr),
              showlegend = TRUE
            ) |>
            plotly::layout(
              title  = paste0(market_label(), " \u2014 Return Scatter: ",
                input$exposure, " vs ", input$hedge),
              xaxis  = list(title = paste(input$hedge, "Log Return")),
              yaxis  = list(title = paste(input$exposure, "Log Return")),
              legend = list(orientation = "h")
            )
        }
    })

    output$market_context <- shiny::renderUI({
      shiny::req(input$energy_market)
      mkt <- input$energy_market

      if (mkt %in% c("CL", "HO", "RB")) {
        bslib::card(
          bslib::card_header(
            shiny::tagList(bsicons::bs_icon("bar-chart-line"), " Refinery Margin Context")
          ),
          bslib::card_body(
            bslib::layout_columns(
              col_widths = c(6, 6),
              plotly::plotlyOutput(session$ns("crack_spread_plot"), height = "300px"),
              plotly::plotlyOutput(session$ns("crack_321_plot"),    height = "300px")
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
