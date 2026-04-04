#' codynamics UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_codynamics_ui <- function(id) {
  ns <- NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::tagList(bsicons::bs_icon("arrows-angle-expand"), " Co-Dynamics")
    ),
    bslib::card_body(
      shiny::fluidRow(
        shiny::column(5,
          shiny::dateRangeInput(ns("date_range"), "Date Range",
            start = "2010-01-01", end = Sys.Date(),
            min   = "2007-01-02", max = Sys.Date())
        ),
        shiny::column(4,
          shiny::radioButtons(ns("view_type"), "View",
            choices  = c(
              "Rolling Correlation" = "rolling",
              "Return Scatter"      = "scatter",
              "PCA"                 = "pca"
            ),
            inline   = TRUE,
            selected = "rolling")
        ),
        shiny::column(3,
          shiny::tags$div(class = "mt-4", shiny::uiOutput(ns("load_status")))
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] != 'pca'", ns("view_type")),
        shiny::fluidRow(
          shiny::column(4,
            shiny::selectInput(ns("series_x"), "Series A", choices = NULL)
          ),
          shiny::column(4,
            shiny::selectInput(ns("series_y"), "Series B", choices = NULL)
          ),
          shiny::column(4,
            shiny::sliderInput(ns("cor_window"), "Rolling Window (days)",
              min = 21, max = 252, value = 63, step = 1)
          )
        )
      ),
      shiny::tags$hr(),
      plotly::plotlyOutput(ns("cody_plot"), height = "calc(100vh - 300px)")
    )
  )
}

#' codynamics Server Function
#'
#' Self-contained module: loads all energy front months + CMT.
#' Renders rolling correlation, return scatter, and PCA views.
#' Does NOT read from or write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object (unused; kept for API consistency).
#' @return None.
#' @export
mod_codynamics_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    combined_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(
      input$date_range,
      ignoreNULL = TRUE, ignoreInit = FALSE, {
        shiny::req(input$date_range)

        output$load_status <- shiny::renderUI({
          shiny::tags$small(class = "text-muted", "Loading all markets...")
        })

        start <- as.Date(input$date_range[1])
        end   <- as.Date(input$date_range[2])

        energy <- tryCatch(
          load_energy_data(c("CL", "BRN", "NG", "HO", "RB"), start, end) |>
            dplyr::filter(.data$contract_num == 1),
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
          all_s <- sort(unique(combined$series))
          shiny::updateSelectInput(session, "series_x",
            choices  = all_s,
            selected = if ("CL01" %in% all_s) "CL01" else all_s[1])
          shiny::updateSelectInput(session, "series_y",
            choices  = all_s,
            selected = if ("HO01" %in% all_s) "HO01"
                       else if (length(all_s) > 1) all_s[2]
                       else all_s[1])
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-success",
              format(nrow(combined), big.mark = ","), " rows loaded")
          })
        } else {
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-danger", "Error loading data")
          })
        }
      }
    )

    all_returns <- shiny::reactive({
      shiny::req(combined_data())
      compute_log_returns(combined_data())
    })

    output$cody_plot <- plotly::renderPlotly({
      shiny::req(input$view_type, all_returns())

      if (input$view_type == "rolling") {
        shiny::req(input$series_x, input$series_y, input$cor_window)

        x_ret <- dplyr::filter(all_returns(), .data$series == input$series_x) |>
          dplyr::select(date, log_return) |>
          dplyr::rename(x = log_return)
        y_ret <- dplyr::filter(all_returns(), .data$series == input$series_y) |>
          dplyr::select(date, log_return) |>
          dplyr::rename(y = log_return)

        joined <- dplyr::inner_join(x_ret, y_ret, by = "date") |>
          dplyr::arrange(date) |>
          dplyr::filter(!is.na(x), !is.na(y))

        shiny::req(nrow(joined) > input$cor_window)

        roll_cor <- compute_rolling_correlation(joined$x, joined$y,
          window = input$cor_window)
        df_cor <- dplyr::tibble(date = joined$date, correlation = roll_cor) |>
          dplyr::filter(!is.na(correlation))

        full_avg <- mean(df_cor$correlation, na.rm = TRUE)

        plotly::plot_ly(df_cor,
          x    = ~date,
          y    = ~correlation,
          type = "scatter",
          mode = "lines",
          name = paste0(input$cor_window, "-day rolling"),
          line = list(color = "#2c7bb6", width = 1.5),
          hovertemplate = "Date: %{x}<br>Corr: %{y:.3f}<extra></extra>"
        ) |>
          plotly::add_lines(
            y = 0, line = list(color = "grey50", dash = "dash", width = 1),
            showlegend = FALSE, hoverinfo = "none"
          ) |>
          plotly::add_lines(
            y = full_avg,
            line       = list(color = "#e74c3c", dash = "dot", width = 1.5),
            name       = sprintf("Full-period avg (%.2f)", full_avg),
            showlegend = TRUE,
            hoverinfo  = "none"
          ) |>
          plotly::layout(
            title  = paste0("Rolling Correlation: ", input$series_x,
              " vs ", input$series_y, " (", input$cor_window, "-day)"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Pearson Correlation", range = c(-1, 1)),
            legend = list(orientation = "h", x = 0, y = 1.08)
          )

      } else if (input$view_type == "scatter") {
        shiny::req(input$series_x, input$series_y)

        x_ret <- dplyr::filter(all_returns(), .data$series == input$series_x) |>
          dplyr::select(date, log_return) |>
          dplyr::rename(x = log_return)
        y_ret <- dplyr::filter(all_returns(), .data$series == input$series_y) |>
          dplyr::select(date, log_return) |>
          dplyr::rename(y = log_return)

        scatter_df <- dplyr::inner_join(x_ret, y_ret, by = "date") |>
          dplyr::filter(!is.na(x), !is.na(y)) |>
          dplyr::mutate(year = as.character(lubridate::year(.data$date)))

        shiny::req(nrow(scatter_df) > 2)

        years     <- sort(unique(scatter_df$year))
        pal       <- grDevices::colorRampPalette(
          c("#440154", "#31688e", "#35b779", "#fde725")
        )(length(years))
        color_map <- setNames(pal, years)

        fit     <- stats::lm(y ~ x, data = scatter_df)
        x_rng   <- range(scatter_df$x, na.rm = TRUE)
        line_df <- dplyr::tibble(
          x = x_rng,
          y = stats::coef(fit)[1] + stats::coef(fit)[2] * x_rng
        )

        plotly::plot_ly(scatter_df,
          x      = ~x,
          y      = ~y,
          color  = ~year,
          colors = color_map,
          type   = "scatter",
          mode   = "markers",
          marker = list(size = 4, opacity = 0.5),
          hovertemplate = paste0(
            input$series_x, ": %{x:.4f}<br>",
            input$series_y, ": %{y:.4f}<extra></extra>")
        ) |>
          plotly::add_lines(data = line_df, x = ~x, y = ~y, inherit = FALSE,
            line       = list(color = "black", width = 2, dash = "dash"),
            name       = sprintf("Beta = %.3f", stats::coef(fit)[2]),
            showlegend = TRUE
          ) |>
          plotly::layout(
            title  = paste0("Return Scatter: ", input$series_x, " vs ", input$series_y),
            xaxis  = list(title = paste(input$series_x, "Log Return")),
            yaxis  = list(title = paste(input$series_y, "Log Return")),
            legend = list(orientation = "h", x = 0, y = 1.08)
          )

      } else {
        # PCA biplot
        all_series_ids <- unique(all_returns()$series)
        ret_mat <- compute_return_matrix(all_returns(), all_series_ids)
        shiny::req(nrow(ret_mat) > 5, ncol(ret_mat) > 1)

        pca      <- stats::prcomp(ret_mat, scale. = TRUE)
        pct_var  <- round(summary(pca)$importance[2, 1:2] * 100, 1)
        loadings <- as.data.frame(pca$rotation[, 1:2])
        loadings$series <- rownames(loadings)
        loadings$source <- dplyr::case_when(
          grepl("^DGS", loadings$series) ~ "Treasury",
          TRUE                            ~ "Energy"
        )

        colors <- c("Energy" = "#e74c3c", "Treasury" = "#2980b9")

        max_pc1 <- max(abs(loadings$PC1))
        max_pc2 <- max(abs(loadings$PC2))

        plotly::plot_ly(loadings,
          x            = ~PC1,
          y            = ~PC2,
          text         = ~series,
          color        = ~source,
          colors       = colors,
          type         = "scatter",
          mode         = "markers+text",
          textposition = "top center",
          marker       = list(size = 10),
          hovertemplate = "%{text}<br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>"
        ) |>
          plotly::add_lines(
            x = c(-max_pc1, max_pc1), y = c(0, 0),
            line = list(color = "lightgrey", dash = "dot"),
            showlegend = FALSE, hoverinfo = "none"
          ) |>
          plotly::add_lines(
            x = c(0, 0), y = c(-max_pc2, max_pc2),
            line = list(color = "lightgrey", dash = "dot"),
            showlegend = FALSE, hoverinfo = "none"
          ) |>
          plotly::layout(
            title  = "PCA Biplot — Market Factor Loadings",
            xaxis  = list(title = paste0("PC1 (", pct_var[1], "% variance explained)")),
            yaxis  = list(title = paste0("PC2 (", pct_var[2], "% variance explained)")),
            legend = list(orientation = "h", x = 0, y = 1.08),
            annotations = list(list(
              x         = 0.5, y = -0.12,
              xref      = "paper", yref = "paper",
              text      = "PC1 = parallel shift across markets; PC2 = slope / divergence",
              showarrow = FALSE,
              font      = list(size = 10, color = "grey50")
            ))
          )
      }
    })
  })
}
