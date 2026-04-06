# Project Context: marketdynamics

## Project Overview

**Course:** University of Alberta — Fintech3 (Posit Connect / Golem Apps module)
**Type:** Capstone individual project — graded on absolute basis
**Due:** 2026-04-06 at 10pm MST
**Deliverables:** Working golem R package, public GitHub repo, Docker image on DockerHub via GitHub Actions, 8-minute presentation

**Use case:** A new senior leader in Risk/Trading Management with limited market experience needs to understand and communicate market dynamics — both individual market behavior and cross-market interactions.

---

## Tech Stack

| Tool | Purpose |
|---|---|
| `golem` | Package framework for Shiny apps |
| `bslib` | UI — Bootstrap 5, flatly theme, `page_navbar()` layout |
| `bsicons` | Icons for nav tabs and value boxes |
| `plotly` | All interactive charts |
| `dplyr`, `tidyr` | Data manipulation |
| `lubridate` | Date handling |
| `zoo` | Rolling window operations (`rollapply`) |
| `arrow` | Reading bundled feather data files |
| `config` | golem environment config |
| `testthat` | Unit testing |

---

## Package

**Name:** `marketdynamics`
**GitHub:** `https://github.com/Duncan-McNabb/Market-Dynamics-Golem-App`
**DockerHub:** `duncanmcnabb/marketdynamics:latest`

---

## Architecture: Small r Strategy

The app uses the Small r Strategy. `r` is created once in `app_server.R` and passed to every module server. The global market selector in the navbar writes `r$market`; all analysis modules read from it.

```r
# app_server.R
app_server <- function(input, output, session) {
  r <- shiny::reactiveValues()

  shiny::observeEvent(input$global_market, {
    r$market <- input$global_market
  }, ignoreNULL = TRUE)

  mod_forward_curve_server("forward_curve_1", r = r)
  mod_volatility_server("volatility_1",       r = r)
  mod_codynamics_server("codynamics_1",       r = r)
  mod_seasonality_server("seasonality_1",     r = r)
  mod_hedge_ratio_server("hedge_ratio_1",     r = r)
  mod_simulation_server("simulation_1",       r = r)
}
```

**Rule:** `app_server.R` is the only place that writes to `r`. All module servers read `r$market` only. Modules do their own data loading from the bundled feather files using the selected market.

---

## UI Layout

```r
# app_ui.R
bslib::page_navbar(
  title  = "Market Dynamics",
  theme  = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  id     = "main_tabs",
  fillable = FALSE,

  bslib::nav_panel("Overview",        ...),   # hero landing page
  bslib::nav_panel("Market Structure", mod_forward_curve_ui("forward_curve_1")),
  bslib::nav_panel("Seasonality",      mod_seasonality_ui("seasonality_1")),
  bslib::nav_panel("Volatility",       mod_volatility_ui("volatility_1")),
  bslib::nav_panel("Cross-Market",     mod_codynamics_ui("codynamics_1")),
  bslib::nav_panel("Hedge Ratios",     mod_hedge_ratio_ui("hedge_ratio_1")),
  bslib::nav_panel("Simulation",       mod_simulation_ui("simulation_1")),

  bslib::nav_spacer(),
  bslib::nav_item(
    shiny::selectInput("global_market", label = NULL,
      choices = energy_market_choices, selected = "CL", width = "175px")
  )
)
```

The global market selector lives in the navbar. There is no per-tab market dropdown — all modules read `r$market` directly.

---

## Data Sources

All data is pre-fetched and bundled as Apache Arrow feather files in `inst/extdata/`. There are no runtime API calls.

| File | Contents |
|---|---|
| `energy_data.feather` | RTL energy futures (CL, BRN, NG, HO, RB) — front through deferred contracts |
| `eia_data.feather` | 14 EIA weekly series (crude stocks, refinery runs, NG storage, etc.) |
| `cmt_data.feather` | FRED CMT yields — 11 tenors, 1M through 30Y |

A GitHub Actions workflow (`refresh-data.yml`) refreshes EIA and CMT data every weekday at 12:00 UTC and rebuilds the Docker image if data changed.

---

## File Responsibilities

| File | Owns |
|---|---|
| `R/fct_data.R` | All data loading — pure functions, no reactivity |
| `R/fct_helpers.R` | All analytical computations (rolling vol, beta, correlation, etc.) — pure functions |
| `R/app_config.R` | Shared constants: `energy_market_choices`, `market_labels`, `petro_markets` |
| `R/mod_forward_curve.R` | Forward curve dynamics, calendar spread, inventory overlay |
| `R/mod_seasonality.R` | Monthly returns, year overlay, seasonal vol, calendar spread seasonality |
| `R/mod_volatility.R` | Rolling vol with regime bands, Samuelson term structure, crack spread vol |
| `R/mod_codynamics.R` | Rolling correlation, scatter, PCA, Treasury yield curve, yield spreads |
| `R/mod_hedge_ratio.R` | Rolling OLS and min-variance hedge ratios, crack spreads, 3-2-1 margin |
| `R/mod_simulation.R` | GBM and OU Monte Carlo price simulation with percentile fan charts |
| `R/app_ui.R` | Top-level UI — layout only, no business logic |
| `R/app_server.R` | Creates `r`, writes `r$market` from global selector, calls all module servers |
| `R/run_app.R` | `run_app()` entry point |

---

## Shared Constants (app_config.R)

```r
energy_market_choices <- c(
  "WTI Crude (CL)"     = "CL",
  "Brent Crude (BRN)"  = "BRN",
  "Natural Gas (NG)"   = "NG",
  "Heating Oil (HO)"   = "HO",
  "RBOB Gasoline (RB)" = "RB"
)

market_labels <- c(CL = "WTI Crude", BRN = "Brent Crude",
                   NG = "Natural Gas", HO = "Heating Oil", RB = "RBOB Gasoline")

petro_markets <- c("CL", "HO", "RB")  # markets that participate in crack spread analysis
```

Never redefine these locally in a module. Always use the constants from `app_config.R`.

---

## Key fct_data.R Functions

| Function | Returns |
|---|---|
| `load_energy_data(markets, start_date, end_date)` | tibble: `{date, series, market, contract_num, value}` |
| `load_cmt_data(series, start_date, end_date)` | tibble: same schema, market = "CMT" |
| `load_eia_data(roles, start_date, end_date)` | tibble: `{role, label, date, value}` |
| `combine_data(energy_long, cmt_long)` | tibble: row-bound with `source` column added |
| `pivot_wide(long_data, market_prefix)` | wide tibble: contract_num as columns prefixed "c" |
| `compute_log_returns(long_data)` | input tibble with `log_return` column added |

## Key fct_helpers.R Functions

| Function | Used By |
|---|---|
| `compute_rolling_vol(returns, window = 21)` | mod_volatility, mod_simulation |
| `compute_return_matrix(long_returns, series)` | mod_codynamics |
| `compute_rolling_correlation(x, y, window = 63)` | mod_codynamics, mod_hedge_ratio |
| `compute_rolling_beta(y, x, window = 63)` | mod_hedge_ratio |
| `compute_seasonal_index(long_returns, series_id, freq)` | mod_seasonality |
| `compute_vol_surface(long_returns, series_ids, window)` | mod_volatility |

All rolling functions use `zoo::rollapply`.

---

## Simulation Module (mod_simulation.R)

Two stochastic models fitted to front-month log returns:

**GBM:** `S(t+1) = S(t) * exp((μ - σ²/2)dt + σZ)` — Itô-corrected drift.

**OU (Ornstein-Uhlenbeck):** Exact discrete AR(1) on log prices:
`X(t+1) = a + b*X(t) + resid_sd*Z`
- `b` clamped to `(0.001, 0.999)` to ensure stationarity
- Half-life = `-log(2) / log(b)` trading days
- No Euler approximation — exact discrete step

---

## Shiny Patterns

- Use `reactive()` for data transformations — not `observe()`.
- Use `observeEvent()` only for side effects (writing to `r`).
- Use `req()` before every `r$market` access.
- Do not use `reactive()` inside `observeEvent()`.
- Do not write to `r` from inside analysis modules.

---

## Deployment

**Dockerfile base:** `rocker/shiny:4.3.3`
**Packages:** Installed via RSPM binary (no source compilation)
**Docker Hub:** `duncanmcnabb/marketdynamics:latest`
**GitHub Actions:**
- `docker-build-push.yml` — builds and pushes on every push to `main`
- `refresh-data.yml` — refreshes EIA + CMT data weekdays 12:00 UTC, commits if changed, triggers rebuild

---

## Testing

62 unit tests across three files:
- `tests/testthat/test-fct_data.R` — 15 tests for all 6 data functions
- `tests/testthat/test-fct_helpers.R` — 14 tests for all 6 helper functions
- `tests/testthat/test-simulation.R` — 10 tests for GBM/OU math and edge cases

Run with: `devtools::test()`
