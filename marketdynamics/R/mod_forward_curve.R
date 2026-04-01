#' forward_curve UI Function
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_forward_curve_ui <- function(id) {
  ns <- NS(id)

  tagList(
    shiny::fluidRow(
      shiny::column(4,
        shiny::selectInput(ns("market"), "Market", choices = NULL)
      ),
      shiny::column(4,
        shiny::sliderInput(ns("n_curves"), "Curve Snapshots",
          min = 10, max = 104, value = 52, step = 1)
      ),
      shiny::column(4,
        shiny::radioButtons(ns("view_type"), "View",
          choices  = c("Curve Snapshots" = "snapshots", "Price Heatmap" = "heatmap"),
          inline   = TRUE,
          selected = "snapshots"
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(4, bslib::value_box(
        title    = "Front Month Price",
        value    = shiny::uiOutput(ns("vb_front_price")),
        showcase = bsicons::bs_icon("currency-dollar"),
        theme    = "primary"
      )),
      shiny::column(4, bslib::value_box(
        title    = "M2 - M1 Roll Yield",
        value    = shiny::uiOutput(ns("vb_roll_yield")),
        showcase = bsicons::bs_icon("arrow-left-right"),
        theme    = "secondary"
      )),
      shiny::column(4, bslib::value_box(
        title    = "Curve Shape",
        value    = shiny::uiOutput(ns("vb_shape")),
        showcase = bsicons::bs_icon("graph-up"),
        theme    = "info"
      ))
    ),
    plotly::plotlyOutput(ns("curve_plot"), height = "500px")
  )
}

#' forward_curve Server Function
#'
#' Reads from `r$energy_long` and `r$selected_energy`. Does NOT write to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object created in `app_server`.
#' @return None.
#' @export
mod_forward_curve_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    # Keep market selector in sync with sidebar selections
    shiny::observe({
      shiny::req(r$selected_energy)
      shiny::updateSelectInput(session, "market", choices = r$selected_energy,
        selected = r$selected_energy[1])
    })

    # Wide format: date + one column per contract number (c1, c2, ...)
    curve_data <- shiny::reactive({
      shiny::req(r$energy_long, input$market)
      pivot_wide(r$energy_long, input$market)
    })

    # Latest row for value box computations
    latest <- shiny::reactive({
      shiny::req(curve_data())
      cd <- curve_data()
      cd[which.max(cd$date), ]
    })

    output$vb_front_price <- shiny::renderUI({
      shiny::req(latest())
      row <- latest()
      val <- if ("c1" %in% names(row)) round(row[["c1"]], 2) else NA
      shiny::tags$span(format(val, big.mark = ","))
    })

    output$vb_roll_yield <- shiny::renderUI({
      shiny::req(latest())
      row <- latest()
      if (all(c("c1", "c2") %in% names(row)) && !is.na(row[["c1"]]) && row[["c1"]] != 0) {
        ry <- (row[["c2"]] - row[["c1"]]) / row[["c1"]] * 100
        shiny::tags$span(sprintf("%.2f%%", ry))
      } else {
        shiny::tags$span("N/A")
      }
    })

    output$vb_shape <- shiny::renderUI({
      shiny::req(latest())
      row <- latest()
      if (all(c("c1", "c2") %in% names(row)) && !is.na(row[["c1"]]) && row[["c1"]] != 0) {
        ry <- (row[["c2"]] - row[["c1"]]) / row[["c1"]] * 100
        label <- if (ry > 0) "Contango" else "Backwardation"
        shiny::tags$span(label)
      } else {
        shiny::tags$span("N/A")
      }
    })

    output$curve_plot <- plotly::renderPlotly({
      shiny::req(curve_data(), input$view_type)
      cd <- curve_data()

      # Drop date column to get numeric contract columns
      contract_cols <- setdiff(names(cd), "date")
      contract_nums <- as.integer(sub("^c", "", contract_cols))

      if (input$view_type == "snapshots") {
        # Sample n_curves evenly-spaced rows
        n      <- min(input$n_curves, nrow(cd))
        idx    <- round(seq(1, nrow(cd), length.out = n))
        sample <- cd[idx, ]

        # Color palette mapped to date index
        pal  <- grDevices::colorRampPalette(c("#440154", "#31688e", "#35b779", "#fde725"))(n)

        p <- plotly::plot_ly()
        for (i in seq_len(nrow(sample))) {
          prices <- as.numeric(sample[i, contract_cols])
          p <- plotly::add_lines(p,
            x    = contract_nums,
            y    = prices,
            name = as.character(sample$date[i]),
            line = list(color = pal[i], width = 1),
            showlegend = FALSE,
            hovertemplate = paste0(
              "Date: ", sample$date[i],
              "<br>Contract: %{x}<br>Price: $%{y:.2f}<extra></extra>"
            )
          )
        }
        p |>
          plotly::layout(
            title  = paste(input$market, "Forward Curve Snapshots"),
            xaxis  = list(title = "Contract Month"),
            yaxis  = list(title = "Price"),
            hovermode = "closest"
          )

      } else {
        # Heatmap: x=date, y=contract number, z=price
        z_mat <- t(as.matrix(cd[, contract_cols]))
        plotly::plot_ly(
          x          = cd$date,
          y          = contract_cols,
          z          = z_mat,
          type       = "heatmap",
          colorscale = "Viridis",
          hovertemplate = "Date: %{x}<br>Contract: %{y}<br>Price: $%{z:.2f}<extra></extra>"
        ) |>
          plotly::layout(
            title  = paste(input$market, "Price Heatmap"),
            xaxis  = list(title = "Date"),
            yaxis  = list(title = "Contract Month", autorange = "reversed")
          )
      }
    })
  })
}
