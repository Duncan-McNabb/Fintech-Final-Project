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

      # ── Cover Page ──────────────────────────────────────────────────────────
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("house"), " Overview"),

        # Full-viewport hero with background image + dark overlay
        shiny::tags$div(
          style = paste0(
            "position:relative;",
            "height:calc(100vh - 56px);",   # subtract navbar height
            "display:flex;",
            "align-items:center;",
            "justify-content:center;",
            "overflow:hidden;"
          ),

          # Background image layer
          shiny::tags$div(
            style = paste0(
              "position:absolute; inset:0;",
              "background-image:url('https://images.unsplash.com/photo-1611974789855-9c2a0a7236a3",
              "?auto=format&fit=crop&w=1920&q=80');",
              "background-size:cover;",
              "background-position:center;"
            )
          ),

          # Dark gradient overlay for readability
          shiny::tags$div(
            style = paste0(
              "position:absolute; inset:0;",
              "background:linear-gradient(",
              "135deg,",
              "rgba(17,24,39,0.82) 0%,",
              "rgba(44,62,80,0.75) 100%",
              ");"
            )
          ),

          # Text content — sits above both layers
          shiny::tags$div(
            style = paste0(
              "position:relative; z-index:1;",
              "text-align:center;",
              "color:#ffffff;",
              "padding:0 1.5rem;",
              "max-width:720px;"
            ),
            shiny::tags$h1(
              style = paste0(
                "font-size:3rem;",
                "font-weight:700;",
                "letter-spacing:-0.5px;",
                "margin-bottom:1rem;",
                "text-shadow:0 2px 12px rgba(0,0,0,0.5);"
              ),
              "Market Dynamics"
            ),
            shiny::tags$p(
              style = paste0(
                "font-size:1.2rem;",
                "line-height:1.7;",
                "color:rgba(255,255,255,0.85);",
                "font-weight:300;",
                "text-shadow:0 1px 6px rgba(0,0,0,0.4);"
              ),
              "An interactive analytics dashboard for energy futures and US Treasury markets.",
              "Built to help trading and risk management professionals understand",
              "market structure, seasonal patterns, volatility regimes,",
              "and cross-market relationships."
            )
          )
        )
      ),

      # ── Analysis Tabs ────────────────────────────────────────────────────────
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("layers"), " Market Structure"),
        mod_forward_curve_ui("forward_curve_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("calendar3"), " Seasonality"),
        mod_seasonality_ui("seasonality_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("activity"), " Volatility"),
        mod_volatility_ui("volatility_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("arrows-angle-expand"), " Cross-Market"),
        mod_codynamics_ui("codynamics_1")
      ),
      bslib::nav_panel(
        title = shiny::tagList(bsicons::bs_icon("shield-check"), " Hedge Ratios"),
        mod_hedge_ratio_ui("hedge_ratio_1")
      ),

      bslib::nav_spacer(),
      bslib::nav_item(
        shiny::selectInput(
          "global_market",
          label   = NULL,
          choices = c(
            "WTI Crude (CL)"     = "CL",
            "Brent Crude (BRN)"  = "BRN",
            "Nat Gas (NG)"       = "NG",
            "Heating Oil (HO)"   = "HO",
            "RBOB Gasoline (RB)" = "RB"
          ),
          selected = "CL",
          width    = "155px"
        )
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
