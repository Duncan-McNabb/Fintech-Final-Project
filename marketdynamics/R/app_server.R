#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # r is a shared reactive values object for cross-module communication.
  r <- shiny::reactiveValues()

  shiny::observeEvent(input$global_market, {
    r$market <- input$global_market
  }, ignoreNULL = TRUE)

  mod_forward_curve_server("forward_curve_1",   r = r)
  mod_volatility_server("volatility_1",         r = r)
  mod_codynamics_server("codynamics_1",         r = r)
  mod_seasonality_server("seasonality_1",       r = r)
  mod_hedge_ratio_server("hedge_ratio_1",       r = r)
  mod_simulation_server("simulation_1",         r = r)
}
