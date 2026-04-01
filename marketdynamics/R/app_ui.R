#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_sidebar(
      title = shiny::tags$span(
        bsicons::bs_icon("graph-up-arrow"), " Market Dynamics"
      ),
      theme = bslib::bs_theme(
        version    = 5,
        bootswatch = "flatly",
        primary    = "#2c3e50"
      ),
      sidebar = bslib::sidebar(
        width = 290,
        title = "Market Selection",
        mod_inputs_ui("inputs_1")
      ),
      bslib::navset_card_tab(
        id = "main_tabs",
        bslib::nav_panel(
          title = shiny::tagList(bsicons::bs_icon("layers"), " Forward Curve"),
          mod_forward_curve_ui("forward_curve_1")
        ),
        bslib::nav_panel(
          title = shiny::tagList(bsicons::bs_icon("activity"), " Volatility"),
          mod_volatility_ui("volatility_1")
        ),
        bslib::nav_panel(
          title = shiny::tagList(bsicons::bs_icon("diagram-3"), " Co-Dynamics"),
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
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "marketdynamics"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
