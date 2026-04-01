#' codynamics UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_codynamics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::radioButtons(ns("view_type"), "View",
          choices = c(
            "Correlation Matrix" = "cormat",
            "Rolling Correlation" = "rolling",
            "PCA"                = "pca"
          ),
          inline   = TRUE,
          selected = "cormat"
        )
      ),
      shiny::column(4,
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'rolling'", ns("view_type")),
          shiny::selectInput(ns("series_x"), "Series X", choices = NULL),
          shiny::selectInput(ns("series_y"), "Series Y", choices = NULL)
        )
      ),
      shiny::column(4,
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'rolling'", ns("view_type")),
          shiny::sliderInput(ns("cor_window"), "Rolling Window (days)",
            min = 21, max = 252, value = 63, step = 1)
        )
      )
    ),
    plotly::plotlyOutput(ns("cody_plot"), height = "560px")
  )
}

#' codynamics Server Function
#'
#' Reads from `r$combined_long`. Does NOT write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object created in `app_server`.
#' @return None.
#' @export
mod_codynamics_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    # All series available: front-month energy + CMT
    all_series <- shiny::reactive({
      shiny::req(r$combined_long)
      energy_front <- r$combined_long |>
        dplyr::filter(.data$source == "energy", .data$contract_num == 1) |>
        dplyr::pull(.data$series) |>
        unique() |>
        sort()
      cmt_series <- r$combined_long |>
        dplyr::filter(.data$source == "cmt") |>
        dplyr::pull(.data$series) |>
        unique() |>
        sort()
      c(energy_front, cmt_series)
    })

    # Update pair selectors
    shiny::observe({
      shiny::req(all_series())
      s <- all_series()
      shiny::updateSelectInput(session, "series_x", choices = s, selected = s[1])
      shiny::updateSelectInput(session, "series_y", choices = s,
        selected = if (length(s) > 1) s[2] else s[1])
    })

    # Return matrix (dates x series, NA rows dropped)
    ret_matrix <- shiny::reactive({
      shiny::req(r$combined_long, all_series())
      combined_ret <- compute_log_returns(r$combined_long)
      compute_return_matrix(combined_ret, all_series())
    })

    # All log returns (long form) for rolling correlation
    all_returns <- shiny::reactive({
      shiny::req(r$combined_long)
      compute_log_returns(r$combined_long)
    })

    output$cody_plot <- plotly::renderPlotly({
      shiny::req(input$view_type)

      if (input$view_type == "cormat") {
        shiny::req(ret_matrix())
        mat  <- ret_matrix()
        cmat <- stats::cor(mat, use = "complete.obs")
        sn   <- colnames(cmat)
        n    <- length(sn)

        # Build annotation text matrix
        annots <- lapply(seq_len(n), function(i) {
          lapply(seq_len(n), function(j) {
            list(x = sn[j], y = sn[i], text = sprintf("%.2f", cmat[i, j]),
              xref = "x", yref = "y", showarrow = FALSE,
              font = list(size = 9, color = if (abs(cmat[i, j]) > 0.5) "white" else "black"))
          })
        })
        annots_flat <- unlist(annots, recursive = FALSE)

        plotly::plot_ly(
          x          = sn,
          y          = sn,
          z          = cmat,
          type       = "heatmap",
          colorscale = "RdBu",
          zmin       = -1,
          zmax       = 1,
          hovertemplate = "%{x} vs %{y}: %{z:.3f}<extra></extra>"
        ) |>
          plotly::layout(
            title       = "Return Correlation Matrix",
            xaxis       = list(title = "", tickangle = -45),
            yaxis       = list(title = "", autorange = "reversed"),
            annotations = annots_flat
          )

      } else if (input$view_type == "rolling") {
        shiny::req(all_returns(), input$series_x, input$series_y, input$cor_window)

        x_ret <- dplyr::filter(all_returns(), .data$series == input$series_x) |>
          dplyr::select(date, log_return) |>
          dplyr::rename(x = log_return)
        y_ret <- dplyr::filter(all_returns(), .data$series == input$series_y) |>
          dplyr::select(date, log_return) |>
          dplyr::rename(y = log_return)

        joined <- dplyr::inner_join(x_ret, y_ret, by = "date") |>
          dplyr::arrange(date) |>
          dplyr::filter(!is.na(x), !is.na(y))

        rolling_cor <- compute_rolling_correlation(joined$x, joined$y, window = input$cor_window)
        df_cor <- dplyr::tibble(date = joined$date, correlation = rolling_cor) |>
          dplyr::filter(!is.na(correlation))

        plotly::plot_ly(df_cor, x = ~date, y = ~correlation, type = "scatter",
          mode = "lines", line = list(color = "#2c7bb6", width = 1.5),
          hovertemplate = "Date: %{x}<br>Correlation: %{y:.3f}<extra></extra>"
        ) |>
          plotly::add_lines(y = 0, line = list(color = "grey", dash = "dash"),
            showlegend = FALSE, hoverinfo = "none") |>
          plotly::layout(
            title  = paste0("Rolling Correlation: ", input$series_x, " vs ", input$series_y,
              " (", input$cor_window, "-day)"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Correlation", range = c(-1, 1))
          )

      } else {
        # PCA biplot
        shiny::req(ret_matrix())
        mat <- ret_matrix()
        pca <- stats::prcomp(mat, scale. = TRUE)

        pct_var <- round(summary(pca)$importance[2, 1:2] * 100, 1)
        loadings <- as.data.frame(pca$rotation[, 1:2])
        loadings$series <- rownames(loadings)
        loadings$source <- ifelse(grepl("^DGS", loadings$series), "Treasury", "Energy")

        colors <- c("Energy" = "#e74c3c", "Treasury" = "#2980b9")

        plotly::plot_ly(loadings,
          x    = ~PC1,
          y    = ~PC2,
          text = ~series,
          color = ~source,
          colors = colors,
          type = "scatter",
          mode = "markers+text",
          textposition = "top center",
          marker = list(size = 10),
          hovertemplate = "%{text}<br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>"
        ) |>
          plotly::add_lines(x = c(-max(abs(loadings$PC1)), max(abs(loadings$PC1))),
            y = c(0, 0), line = list(color = "lightgrey", dash = "dot"),
            showlegend = FALSE, hoverinfo = "none") |>
          plotly::add_lines(x = c(0, 0),
            y = c(-max(abs(loadings$PC2)), max(abs(loadings$PC2))),
            line = list(color = "lightgrey", dash = "dot"),
            showlegend = FALSE, hoverinfo = "none") |>
          plotly::layout(
            title  = "PCA Biplot — Market Factor Loadings",
            xaxis  = list(title = paste0("PC1 (", pct_var[1], "% variance)")),
            yaxis  = list(title = paste0("PC2 (", pct_var[2], "% variance)"))
          )
      }
    })
  })
}
