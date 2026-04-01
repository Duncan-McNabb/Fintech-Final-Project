# Project Context: marketdynamics

## Project Overview

**Course:** University of Alberta — Fintech3 (Posit Connect / Golem Apps module)
**Type:** Capstone individual project — graded on absolute basis
**Due:** 2026-04-06 at 10pm MST
**Deliverables:** Working golem R package, GitHub repo (with instructor collaborator access), Docker image on DockerHub via GitHub Actions, 8-minute presentation

**Use case:** A new senior leader in Risk/Trading Management with limited market experience needs to understand and communicate market dynamics — both individual market behavior and cross-market interactions.

---

## Tech Stack

| Tool | Purpose |
|---|---|
| `golem` | Package framework for Shiny apps |
| `bslib` | UI — Bootstrap 5, flatly theme, `page_sidebar()` layout |
| `bsicons` | Icons for bslib value boxes and nav tabs |
| `plotly` | All interactive charts |
| `dplyr`, `tidyr` | Data manipulation |
| `lubridate` | Date handling |
| `zoo` | Rolling window operations (`rollapply`) |
| `fredr` | FRED API client for CMT data |
| `RTL` | Energy futures data (GitHub only: `patzlaw/RTL`) |
| `config` | golem environment config |
| `testthat` | Unit testing |
| `shinytest2` | End-to-end app testing |

---

## Package

**Name:** `marketdynamics`
**Location:** `/Users/duncanmcnabb/Fintech-Final-Project/marketdynamics/`
**Created via:** `golem::create_golem("marketdynamics")`

---

## Architecture: Small r Strategy

The app uses the Small r Strategy for reactive state management.

```r
# app_server.R — the only place r is created
app_server <- function(input, output, session) {
  r <- shiny::reactiveValues(
    energy_long     = NULL,   # filtered RTL::dflong data
    cmt_long        = NULL,   # filtered FRED CMT data
    combined_long   = NULL,   # row-bound union with source column
    date_range      = NULL,   # selected date range
    selected_energy = character(0),
    selected_cmt    = character(0)
  )

  mod_inputs_server("inputs_1", r = r)             # ONLY writer to r
  mod_forward_curve_server("forward_curve_1", r = r)
  mod_volatility_server("volatility_1",       r = r)
  mod_codynamics_server("codynamics_1",       r = r)
  mod_seasonality_server("seasonality_1",     r = r)
  mod_hedge_ratio_server("hedge_ratio_1",     r = r)
}
```

**Rule:** `mod_inputs` is the only module that writes to `r`. All other modules read from `r` only. This enforces unidirectional data flow and prevents circular reactivity.

---

## UI Layout

```r
# app_ui.R
bslib::page_sidebar(
  title = "Market Dynamics",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = bslib::sidebar(mod_inputs_ui("inputs_1")),
  bslib::navset_card_tab(
    bslib::nav_panel("Forward Curve",  mod_forward_curve_ui("forward_curve_1")),
    bslib::nav_panel("Volatility",     mod_volatility_ui("volatility_1")),
    bslib::nav_panel("Co-Dynamics",    mod_codynamics_ui("codynamics_1")),
    bslib::nav_panel("Seasonality",    mod_seasonality_ui("seasonality_1")),
    bslib::nav_panel("Hedge Ratios",   mod_hedge_ratio_ui("hedge_ratio_1"))
  )
)
```

---

## Data Sources

### Energy Futures — `RTL::dflong`

- Install: `remotes::install_github("patzlaw/RTL")`
- Long-format dataset of historical continuous futures contracts
- **Inspect columns before writing data code:** `dplyr::glimpse(RTL::dflong)`
- Contract number is embedded in the ticker name (e.g., `CL01` = crude oil contract 1)
- Extract contract number: `as.integer(stringr::str_extract(ticker, "[0-9]+$"))`
- Markets include: CL (crude oil), NG (natural gas), HO (heating oil), RB (RBOB gasoline)

### US Treasury CMT — `fredr`

- FRED series: `DGS1MO`, `DGS3MO`, `DGS6MO`, `DGS1`, `DGS2`, `DGS3`, `DGS5`, `DGS7`, `DGS10`, `DGS20`, `DGS30`
- Start date: `1990-01-01` (pre-2007 as required)
- Values are in percent (e.g., 4.5 = 4.5%) — divide by 100 before log-return calculations
- Tenor months map: 1MO=1, 3MO=3, 6MO=6, 1=12, 2=24, 3=36, 5=60, 7=84, 10=120, 20=240, 30=360

### API Key

```r
# In fct_data.R — loadCMTData()
key <- Sys.getenv("FRED_API_KEY")
if (nchar(key) == 0) stop("FRED_API_KEY environment variable is not set.")
fredr::fredr_set_key(key)
```

- Set locally in `.Renviron` (never commit this file)
- Passed at Docker runtime: `docker run -e FRED_API_KEY=xxx -p 3838:3838 image`

---

## File Responsibilities

| File | Owns |
|---|---|
| `R/fct_data.R` | All data loading (RTL + FRED) — pure functions, no reactivity |
| `R/fct_helpers.R` | All analytical computations (rolling vol, beta, correlation, etc.) — pure functions |
| `R/mod_inputs.R` | Global market/date selector UI + the only server that writes to `r` |
| `R/mod_forward_curve.R` | Forward curve snapshots and heatmap |
| `R/mod_volatility.R` | Rolling vol and vol surface |
| `R/mod_codynamics.R` | Correlation matrix, rolling correlation, PCA |
| `R/mod_seasonality.R` | Monthly seasonal patterns and year-overlay |
| `R/mod_hedge_ratio.R` | Rolling OLS hedge ratios between markets |
| `R/app_ui.R` | Top-level UI — layout only, no logic |
| `R/app_server.R` | Top-level server — creates `r`, calls all module servers |
| `R/run_app.R` | `run_app()` entry point |

---

## Required Analysis Modules (Grading Minimums)

1. **Historical forward curve behavior** — `mod_forward_curve`
2. **Volatility across time to maturity and over time** — `mod_volatility`
3. **Co-dynamics across markets** — `mod_codynamics`
4. **Seasonality impact on market dynamics** — `mod_seasonality`
5. **Hedge ratio dynamics** — `mod_hedge_ratio`

---

## Key `fct_data.R` Functions

| Function | Returns |
|---|---|
| `load_energy_data(tickers, start_date, end_date)` | tibble: `{date, ticker, contract_num, price}` |
| `load_cmt_data(tickers, start_date, end_date)` | tibble: same schema, rate instead of price |
| `combine_data(energy_long, cmt_long)` | tibble: row-bound with `source` column added |
| `pivot_wide(long_data, ticker)` | wide tibble: contract_num as columns |
| `compute_log_returns(long_data)` | long tibble with `log_return` column added |

## Key `fct_helpers.R` Functions

| Function | Used By |
|---|---|
| `compute_rolling_vol(returns, window = 21)` | mod_volatility |
| `compute_return_matrix(long_returns, tickers)` | mod_codynamics |
| `compute_rolling_correlation(x, y, window = 63)` | mod_codynamics |
| `compute_rolling_beta(y, x, window = 63)` | mod_hedge_ratio |
| `compute_seasonal_index(long_data, ticker, freq = "month")` | mod_seasonality |
| `compute_vol_surface(long_returns, tickers, window = 21)` | mod_volatility |

All rolling functions use `zoo::rollapply`.

---

## Shiny Patterns

- Use `reactive()` for data transformations — not `observe()` or `observeEvent()`.
- Use `observeEvent()` only for side effects (writing to `r`, showing notifications).
- Gate expensive data loads behind an `actionButton` in `mod_inputs` to prevent reactive churn.
- Use `req()` to guard against NULL inputs before computing. Example: `req(r$energy_long)`.
- Do not use `reactive()` inside `observeEvent()` — it does not work as expected.

---

## Deployment

**Dockerfile base:** `rocker/shiny:4.3.3`
**Docker layer order (for cache efficiency):**
1. System libs
2. CRAN packages
3. GitHub packages (`RTL`)
4. Local package install

**GitHub Actions:** Push to `main` triggers build + push to DockerHub.
**Required GitHub secrets:** `DOCKERHUB_USERNAME`, `DOCKERHUB_TOKEN`

---

## Build Sequence

1. Run `dev/01_start.R` — scaffold with `golem::create_golem()`
2. Inspect `RTL::dflong` columns — confirm schema before writing `fct_data.R`
3. Write and unit-test `fct_data.R`
4. Write `mod_inputs.R` — confirm `r` is populated on button click
5. Write `app_ui.R` + `app_server.R` with stub modules — confirm app launches
6. Build `mod_forward_curve.R`
7. Build `mod_volatility.R`
8. Build `mod_codynamics.R`
9. Build `mod_seasonality.R`
10. Build `mod_hedge_ratio.R`
11. Write tests (`tests/testthat/`)
12. Dockerfile — test local build
13. Push to GitHub, set secrets, verify Actions workflow
