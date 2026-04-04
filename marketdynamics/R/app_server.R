#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # r is kept as an empty shared object for future cross-module use.
  # Each module now manages its own data loading independently.
  r <- shiny::reactiveValues()

  mod_forward_curve_server("forward_curve_1",   r = r)
  mod_treasury_curve_server("treasury_curve_1", r = r)
  mod_volatility_server("volatility_1",         r = r)
  mod_codynamics_server("codynamics_1",         r = r)
  mod_seasonality_server("seasonality_1",       r = r)
  mod_hedge_ratio_server("hedge_ratio_1",       r = r)
}
