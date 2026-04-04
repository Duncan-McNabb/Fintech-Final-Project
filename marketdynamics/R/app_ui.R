#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = shiny::tags$span(
        bsicons::bs_icon("graph-up-arrow"), " Market Dynamics"
      ),
      theme = bslib::bs_theme(
        version    = 5,
        bootswatch = "flatly",
        primary    = "#2c3e50"
      ),
      id = "main_tabs",
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("layers"), " Forward Curve"),
        mod_forward_curve_ui("forward_curve_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("bank"), " Treasury Curve"),
        mod_treasury_curve_ui("treasury_curve_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("activity"), " Volatility"),
        mod_volatility_ui("volatility_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("arrows-angle-expand"), " Co-Dynamics"),
        mod_codynamics_ui("codynamics_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("calendar3"), " Seasonality"),
        mod_seasonality_ui("seasonality_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("shield-check"), " Hedge Ratios"),
        mod_hedge_ratio_ui("hedge_ratio_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www", app_sys("app/www"))
  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "marketdynamics")
  )
}
