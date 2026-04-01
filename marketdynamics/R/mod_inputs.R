#' inputs UI Function
#'
#' Global market and date selector. Sits in the persistent sidebar.
#' This is the only module that writes to the shared `r` reactive object.
#'
#' @param id Module namespace id.
#' @return A Shiny UI element.
#' @export
mod_inputs_ui <- function(id) {
  ns <- NS(id)

  # All available markets from RTL::dflong
  energy_markets <- c(
    "WTI Crude (CL)"       = "CL",
    "Brent Crude (BRN)"    = "BRN",
    "Natural Gas (NG)"     = "NG",
    "Heating Oil (HO)"     = "HO",
    "RBOB Gasoline (RB)"   = "RB",
    "Aluminum (ALI)"       = "ALI",
    "Gold (AUP)"           = "AUP",
    "Euro Power (EDP)"     = "EDP",
    "Heat Rate (HTT)"      = "HTT",
    "Jet Fuel (MJP)"       = "MJP"
  )

  cmt_series <- c(
    "1-Month"  = "DGS1MO",
    "3-Month"  = "DGS3MO",
    "6-Month"  = "DGS6MO",
    "1-Year"   = "DGS1",
    "2-Year"   = "DGS2",
    "3-Year"   = "DGS3",
    "5-Year"   = "DGS5",
    "7-Year"   = "DGS7",
    "10-Year"  = "DGS10",
    "20-Year"  = "DGS20",
    "30-Year"  = "DGS30"
  )

  tagList(
    shiny::tags$h6("Energy Markets", class = "text-muted mt-2 mb-1"),
    shiny::checkboxGroupInput(
      ns("energy_markets"),
      label   = NULL,
      choices = energy_markets,
      selected = c("CL", "NG", "HO", "RB")
    ),
    shiny::tags$h6("US Treasuries (CMT)", class = "text-muted mt-3 mb-1"),
    shiny::checkboxGroupInput(
      ns("cmt_series"),
      label    = NULL,
      choices  = cmt_series,
      selected = c("DGS1MO", "DGS2", "DGS5", "DGS10", "DGS30")
    ),
    shiny::tags$h6("Date Range", class = "text-muted mt-3 mb-1"),
    shiny::dateRangeInput(
      ns("date_range"),
      label = NULL,
      start = "2007-01-02",
      end   = Sys.Date(),
      min   = "2007-01-02",
      max   = Sys.Date()
    ),
    shiny::tags$div(
      class = "mt-3",
      shiny::actionButton(
        ns("update"),
        label = "Load Data",
        icon  = shiny::icon("refresh"),
        class = "btn-primary w-100"
      )
    ),
    shiny::tags$div(
      class = "mt-2",
      shiny::uiOutput(ns("load_status"))
    )
  )
}

#' inputs Server Function
#'
#' Loads energy and CMT data on button click and writes results to the shared
#' `r` reactiveValues object. This is the ONLY module that writes to `r`.
#'
#' @param id Module namespace id.
#' @param r A `shiny::reactiveValues` object created in `app_server`.
#' @return None. Side effects: populates `r$energy_long`, `r$cmt_long`,
#'   `r$combined_long`, `r$date_range`, `r$selected_energy`, `r$selected_cmt`.
#' @export
mod_inputs_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {

    output$load_status <- shiny::renderUI({ NULL })

    shiny::observeEvent(input$update, {
      shiny::req(input$energy_markets, input$date_range)

      # Show loading indicator
      output$load_status <- shiny::renderUI({
        shiny::tags$small(class = "text-muted", "Loading data...")
      })

      start <- as.Date(input$date_range[1])
      end   <- as.Date(input$date_range[2])

      # Load energy data
      energy <- tryCatch(
        load_energy_data(input$energy_markets, start, end),
        error = function(e) {
          output$load_status <- shiny::renderUI({
            shiny::tags$small(class = "text-danger",
              paste("Energy data error:", conditionMessage(e)))
          })
          NULL
        }
      )

      # Load CMT data (only if FRED key is set)
      cmt <- NULL
      if (length(input$cmt_series) > 0 && nchar(Sys.getenv("FRED_API_KEY")) > 0) {
        cmt <- tryCatch(
          load_cmt_data(input$cmt_series, start, end),
          error = function(e) {
            output$load_status <- shiny::renderUI({
              shiny::tags$small(class = "text-warning",
                paste("CMT data error:", conditionMessage(e)))
            })
            NULL
          }
        )
      }

      # Write to r
      r$energy_long     <- energy
      r$cmt_long        <- cmt
      r$combined_long   <- if (!is.null(energy) && !is.null(cmt)) {
        combine_data(energy, cmt)
      } else {
        energy
      }
      r$date_range      <- c(start, end)
      r$selected_energy <- input$energy_markets
      r$selected_cmt    <- input$cmt_series

      n_rows <- if (!is.null(energy)) nrow(energy) else 0
      output$load_status <- shiny::renderUI({
        shiny::tags$small(
          class = "text-success",
          paste0("Loaded ", format(n_rows, big.mark = ","), " energy rows")
        )
      })
    })
  })
}
