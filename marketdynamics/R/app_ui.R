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
        shiny::tags$div(
          style = "max-width:960px; margin:0 auto; padding:2rem 1rem;",

          # Hero block
          shiny::tags$div(
            class = "text-center mb-5",
            shiny::tags$h1(
              style = "font-size:2.4rem; font-weight:700; color:#2c3e50;",
              bsicons::bs_icon("graph-up-arrow"), " Market Dynamics"
            ),
            shiny::tags$p(
              class = "lead text-muted",
              style = "font-size:1.15rem; max-width:680px; margin:0.6rem auto 0;",
              "An interactive analytics dashboard for energy futures and US Treasury markets.",
              "Built to help trading and risk management professionals understand",
              "market structure, seasonal patterns, volatility regimes, and cross-market relationships."
            )
          ),

          # Module cards grid
          bslib::layout_columns(
            col_widths = c(4, 4, 4),

            bslib::card(
              class = "h-100",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::tagList(bsicons::bs_icon("layers"), " Forward Curve")
              ),
              bslib::card_body(
                shiny::tags$p(
                  "Visualize the full futures forward curve across all contract months.",
                  "Switch to the", shiny::tags$strong("Calendar Spread"),
                  "view to see the M1\u2212M2 spread over time \u2014 the primary signal",
                  "for whether a market is in backwardation (supply tight) or contango (supply ample)."
                )
              )
            ),

            bslib::card(
              class = "h-100",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::tagList(bsicons::bs_icon("bank"), " Treasury Curve")
              ),
              bslib::card_body(
                shiny::tags$p(
                  "Track all 11 US Treasury CMT tenors from 1990 to today.",
                  "The", shiny::tags$strong("Yield Spreads"),
                  "view plots the 2s10s and 3m10y spreads in basis points,",
                  "with red shading during inversion periods \u2014 a classic recession indicator."
                )
              )
            ),

            bslib::card(
              class = "h-100",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::tagList(bsicons::bs_icon("activity"), " Volatility")
              ),
              bslib::card_body(
                shiny::tags$p(
                  "Rolling realized volatility with regime context (25th/75th percentile bands).",
                  "Term structure view illustrates the", shiny::tags$strong("Samuelson Effect"),
                  "\u2014 near-term contracts are more volatile than deferred ones.",
                  "Crack Spread Vol compares outright price vol to refinery margin vol."
                )
              )
            ),

            bslib::card(
              class = "h-100",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::tagList(bsicons::bs_icon("arrows-angle-expand"), " Co-Dynamics")
              ),
              bslib::card_body(
                shiny::tags$p(
                  "Cross-market analysis across all energy front months and Treasury tenors.",
                  "Rolling pairwise correlation shows how relationships shift over time.",
                  "Return scatter reveals year-by-year clustering.",
                  "PCA biplot separates energy vs. Treasury factor exposures."
                )
              )
            ),

            bslib::card(
              class = "h-100",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::tagList(bsicons::bs_icon("calendar3"), " Seasonality")
              ),
              bslib::card_body(
                shiny::tags$p(
                  "Average monthly returns, year-overlay cumulative paths, and seasonal volatility.",
                  "The", shiny::tags$strong("Calendar Spread"),
                  "view shows which months are structurally backwardated vs. in contango.",
                  "Natural gas includes injection/withdrawal season annotations."
                )
              )
            ),

            bslib::card(
              class = "h-100",
              bslib::card_header(
                class = "bg-primary text-white",
                shiny::tagList(bsicons::bs_icon("shield-check"), " Hedge Ratios")
              ),
              bslib::card_body(
                shiny::tags$p(
                  "Rolling OLS and minimum-variance hedge ratios between any two series.",
                  shiny::tags$strong("Crack Spreads"), "view tracks HO\u2212CL and RB\u2212CL margins.",
                  "The", shiny::tags$strong("3-2-1 Crack"), "view shows the industry-standard",
                  "refinery margin: (2\u00d7RBOB + 1\u00d7HO \u2212 3\u00d7CL) / 3."
                )
              )
            )
          ),

          shiny::tags$hr(class = "my-4"),

          # Markets + Data sources row
          bslib::layout_columns(
            col_widths = c(6, 6),

            bslib::card(
              bslib::card_header(
                shiny::tagList(bsicons::bs_icon("fuel-pump"), " Energy Markets")
              ),
              bslib::card_body(
                shiny::tags$table(
                  class = "table table-sm table-borderless mb-0",
                  shiny::tags$tbody(
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("CL")),
                      shiny::tags$td("WTI Crude Oil \u2014 NYMEX, $/barrel")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("BRN")),
                      shiny::tags$td("Brent Crude \u2014 ICE, $/barrel")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("NG")),
                      shiny::tags$td("Natural Gas \u2014 Henry Hub, $/MMBtu")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("HO")),
                      shiny::tags$td("Heating Oil / ULSD \u2014 NYMEX, $/gallon")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("RB")),
                      shiny::tags$td("RBOB Gasoline \u2014 NYMEX, $/gallon")
                    )
                  )
                )
              )
            ),

            bslib::card(
              bslib::card_header(
                shiny::tagList(bsicons::bs_icon("database"), " Data Sources")
              ),
              bslib::card_body(
                shiny::tags$table(
                  class = "table table-sm table-borderless mb-0",
                  shiny::tags$tbody(
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("RTL")),
                      shiny::tags$td("Continuous futures contracts \u2014 patzlaw/RTL (GitHub)")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("FRED")),
                      shiny::tags$td("US Treasury CMT rates via tidyquant, 1990\u2013present")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Coverage")),
                      shiny::tags$td("Energy: 2007\u2013present | Treasuries: 1990\u2013present")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Frequency")),
                      shiny::tags$td("Daily business day observations")
                    ),
                    shiny::tags$tr(
                      shiny::tags$td(shiny::tags$strong("Built with")),
                      shiny::tags$td("R \u00b7 golem \u00b7 bslib \u00b7 plotly \u00b7 Docker")
                    )
                  )
                )
              )
            )
          ),

          # Footer note
          shiny::tags$p(
            class = "text-center text-muted mt-4",
            style = "font-size:0.85rem;",
            "University of Alberta \u00b7 Fintech3 Capstone \u00b7 ",
            format(Sys.Date(), "%Y")
          )
        )
      ),

      # ── Analysis Tabs ────────────────────────────────────────────────────────
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
