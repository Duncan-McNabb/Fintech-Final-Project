#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Small r Strategy: single reactiveValues object passed to all modules.
  # mod_inputs_server is the ONLY module that writes to r.
  r <- shiny::reactiveValues(
    energy_long     = NULL,
    cmt_long        = NULL,
    combined_long   = NULL,
    date_range      = NULL,
    selected_energy = character(0),
    selected_cmt    = character(0)
  )

  mod_inputs_server("inputs_1",           r = r)
  mod_forward_curve_server("forward_curve_1", r = r)
  mod_volatility_server("volatility_1",       r = r)
  mod_codynamics_server("codynamics_1",       r = r)
  mod_seasonality_server("seasonality_1",     r = r)
  mod_hedge_ratio_server("hedge_ratio_1",     r = r)
}
