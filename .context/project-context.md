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
| `bslib` | UI — Bootstrap 5, flatly theme, `page_navbar()` layout |
| `bsicons` | Icons for bslib nav tabs |
| `plotly` | All interactive charts |
| `dplyr`, `tidyr` | Data manipulation |
| `lubridate` | Date handling |
| `zoo` | Rolling window operations (`rollapply`) |
| `tidyquant` | FRED CMT data via `tq_get(get="economic.data")` — no API key required |
| `RTL` | Energy futures data (GitHub only: `patzlaw/RTL`) |
| `config` | golem environment config |
| `testthat` | Unit testing |

---

## Package

**Name:** `marketdynamics`
**Location:** `/Users/duncanmcnabb/Fintech-Final-Project/marketdynamics/`
**Created via:** `golem::create_golem("marketdynamics")`

---

## Architecture

Each analysis module is self-contained: it owns its own `reactiveVal(NULL)` for local data state, loads data via `observeEvent` on market/date input changes, and does not read from or write to `r`. The shared `r` object exists in `app_server.R` but is currently unused (kept for API consistency).

```r
# app_server.R
app_server <- function(input, output, session) {
  r <- shiny::reactiveValues()   # kept for API consistency; currently unused
  mod_forward_curve_server("forward_curve_1", r = r)
  mod_volatility_server("volatility_1",       r = r)
  mod_seasonality_server("seasonality_1",     r = r)
  mod_hedge_ratio_server("hedge_ratio_1",     r = r)
  mod_treasury_curve_server("treasury_curve_1", r = r)
}
```

---

## UI Layout

```r
# app_ui.R — bslib::page_navbar(), 5 tabs, no sidebar
bslib::page_navbar(
  title = "Market Dynamics",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  bslib::nav_panel("Forward Curve",   mod_forward_curve_ui("forward_curve_1")),
  bslib::nav_panel("Treasury Curve",  mod_treasury_curve_ui("treasury_curve_1")),
  bslib::nav_panel("Volatility",      mod_volatility_ui("volatility_1")),
  bslib::nav_panel("Seasonality",     mod_seasonality_ui("seasonality_1")),
  bslib::nav_panel("Hedge Ratios",    mod_hedge_ratio_ui("hedge_ratio_1"))
)
```

Each tab is wrapped in a `bslib::card()` with a `card_header()` showing a bsicons icon and the tab name.

---

## Data Sources

### Energy Futures — `RTL::dflong`

- Install: `remotes::install_github("patzlaw/RTL")`
- Long-format dataset of historical continuous futures contracts
- **Inspect columns before writing data code:** `dplyr::glimpse(RTL::dflong)`
- Contract number is embedded in the ticker name (e.g., `CL01` = crude oil contract 1)
- Extract contract number: `as.integer(stringr::str_extract(series, "[0-9]+$"))`
- Markets: CL (WTI crude), NG (natural gas), HO (heating oil/ULSD), RB (RBOB gasoline), BRN (Brent crude)

### US Treasury CMT — `tidyquant`

- Fetched via `tidyquant::tq_get(symbol, get = "economic.data")`
- No API key required
- FRED series: `DGS1MO`, `DGS3MO`, `DGS6MO`, `DGS1`, `DGS2`, `DGS3`, `DGS5`, `DGS7`, `DGS10`, `DGS20`, `DGS30`
- Start date: `1990-01-01`
- Values are in percent (e.g., 4.5 = 4.5%) — divide by 100 before log-return calculations
- Tenor months map: 1MO=1, 3MO=3, 6MO=6, 1=12, 2=24, 3=36, 5=60, 7=84, 10=120, 20=240, 30=360

---

## Market Fundamentals

### WTI Crude Oil (CL)

**Primary price drivers:**
- Weekly EIA petroleum inventory report (Wednesday release, ~10:30am ET) — THE primary near-term catalyst
- OPEC+ production decisions — most important medium-term structural driver
- Cushing, Oklahoma storage levels — physical delivery point for NYMEX WTI; high storage = contango; low storage = backwardation

**Key spreads to watch:**
- **WTI-Brent location spread:** WTI historically traded at a premium to Brent (global benchmark). Post-2013 shale boom: WTI went to persistent discount as Cushing storage filled. Has since narrowed as US export infrastructure expanded.
- **Dec/Jan calendar spread (M1-M2):** Primary near-term market structure signal. Widening backwardation (positive spread, M1 > M2) signals supply tightening. Contango (negative spread) signals storage glut or demand weakness.

**Historical context:**
- Pre-2013: WTI dominated global crude pricing
- 2013-2016: Shale revolution caused US production surge → WTI discount to Brent
- Post-2018: Export infrastructure growth re-converged WTI and Brent

**Hedging patterns:** Producers sell forward curve; refiners buy calendar spreads for margin protection; airlines hedge flat price exposure.

---

### Natural Gas (NG)

**Primary price drivers:**
- Weekly EIA natural gas storage report (Thursday release) — single most important weekly signal
- Weather forecasts (heating degree days in winter, cooling degree days in summer)
- LNG export volumes (structural new demand since ~2016)

**Dual seasonality — unique vs. crude:**
- **Winter peak:** January-February — heating demand, ~$4.14/MMBtu historical average
- **Summer peak:** July-August — cooling demand (AC electricity generation)
- Two distinct seasonal demand surges per year, unlike crude's single summer driving season peak

**Storage cycle:**
- **Injection season:** April-October — operators fill underground storage; prices tend soft
- **Withdrawal season:** November-March — inventory drawn down for heating; prices firm
- The weekly EIA storage report is watched against 5-year average: surplus → price weakness, deficit → price strength

**Key spreads:**
- **Spark spread:** `Power Price ($/MWh) − (Gas Price ($/MMBtu) × Heat Rate (MMBtu/MWh))`
  - Heat rate ≈ 7,000-8,000 BTU/kWh for efficient gas turbines
  - Positive = profitable to run gas-fired power plant; drives gas demand from power sector
- **Location spreads:** Henry Hub (US benchmark) vs. AECO (Canadian), vs. JKM (Asian LNG benchmark)

**Structural shift:** LNG export capacity grew from near-zero in 2016 to ~14 Bcf/day by 2025. This permanently raised the floor on Henry Hub prices and linked US gas prices to global LNG markets.

**Seasonality cycles:** STL decomposition reveals a 9-year super-cycle + 3-year sub-cycle overlaying the annual seasonal pattern.

---

### Heating Oil / ULSD (HO)

**Primary price drivers:**
- EIA weekly distillate inventory (Wednesday release)
- Winter weather in PADD 1 (Northeast US) — primary heating demand region
- Refinery utilization rates, especially PADD 3 Gulf Coast

**PADD regional structure (critical for HO):**
- **PADD 1 (East Coast/Northeast):** Primary demand region — home heating oil; tight inventories in winter spike prices
- **PADD 2 (Midwest):** Secondary demand; harvest season creates Oct-Nov diesel peak (agricultural machinery)
- **PADD 3 (Gulf Coast):** Main refining supply hub; Colonial Pipeline (PADD 3 → PADD 1) is critical infrastructure

**Key spreads:**
- **HO-CL crack spread:** Heating oil price minus crude oil price — the refinery distillate margin
- **3-2-1 crack spread:** `(2 × RB + 1 × HO − 3 × CL) / 3` — industry-standard refinery hedging metric representing the average margin per barrel of crude processed

**Curve signal:** Backwardated in tight winter supply (spot premium); contango in summer as distillate builds. Watch the M1-M2 spread crossing zero at end of winter as a structural signal.

**Infrastructure risk:** Colonial Pipeline disruptions (hurricanes, accidents) cause sharp PADD 1 basis spikes — Northeast heating oil can trade at a premium of $0.20-0.50/gallon over PADD 3.

**PADD spread dynamics:** PADD 1 vs. PADD 3 basis widened significantly 2019-2025 reflecting regional supply/demand divergence.

---

### RBOB Gasoline (RB)

**Primary price drivers:**
- EIA weekly gasoline inventory report (Wednesday release); specifically PADD 1b (Central Atlantic) is the key regional barometer
- Summer driving season demand
- RVP (Reid Vapor Pressure) regulation transition in April/May

**Dual seasonality drivers — unique vs. other markets:**
1. **Demand seasonality:** Summer driving season (May-August peak); Labor Day marks the end
2. **Supply seasonality:** RVP regulations require a more expensive, lower-evaporative summer blend starting April-June. The transition itself drives prices higher regardless of crude.

**Key spread:**
- **RBOB-WTI crack spread:** Gasoline refinery margin; analogous to HO-CL for distillates
- Ethanol blending economics (10% blend mandate) affect the net margin slightly

**Seasonal risk event:** Hurricane season (August-October) overlaps peak driving demand. Gulf Coast refinery disruptions cause acute supply spikes — historically the sharpest short-term price moves in the gasoline market.

**Curve signal:** Backwardation = market incentive to draw down inventory (supply tight); contango = incentive to store (supply ample). Key watch: does the front-month flip from contango to backwardation heading into May driving season demand build?

---

## Required Analysis Modules (Grading Minimums)

1. **Historical forward curve behavior** — `mod_forward_curve`
2. **Volatility across time to maturity and over time** — `mod_volatility`
3. **Co-dynamics across markets** — `mod_codynamics` *(currently orphaned — must rebuild)*
4. **Seasonality impact on market dynamics** — `mod_seasonality`
5. **Hedge ratio dynamics** — `mod_hedge_ratio`

---

## File Responsibilities

| File | Owns |
|---|---|
| `R/fct_data.R` | All data loading (RTL + tidyquant/FRED) — pure functions, no reactivity |
| `R/fct_helpers.R` | All analytical computations (rolling vol, beta, correlation, etc.) — pure functions |
| `R/mod_forward_curve.R` | Forward curve time-series + calendar spread view |
| `R/mod_volatility.R` | Rolling vol, term structure (Samuelson Effect) |
| `R/mod_treasury_curve.R` | US Treasury yield curve dynamics |
| `R/mod_codynamics.R` | Rolling pairwise correlation, cross-market scatter |
| `R/mod_seasonality.R` | Monthly seasonal patterns and year-overlay |
| `R/mod_hedge_ratio.R` | Rolling OLS hedge ratios, crack spreads |
| `R/app_ui.R` | Top-level UI — layout only, no logic |
| `R/app_server.R` | Top-level server — creates `r`, calls all module servers |
| `R/run_app.R` | `run_app()` entry point |

---

## Key `fct_data.R` Functions

| Function | Returns |
|---|---|
| `load_energy_data(market, start_date, end_date)` | tibble: `{date, series, contract_num, value, source}` |
| `load_cmt_data(start_date, end_date)` | tibble: same schema, CMT rates |
| `combine_data(energy_long, cmt_long)` | tibble: row-bound with `source` column |
| `pivot_wide(long_data, market_prefix)` | wide tibble: `{date, c1, c2, ...cN}` |
| `compute_log_returns(long_data)` | long tibble with `log_return` column added |

## Key `fct_helpers.R` Functions

| Function | Used By |
|---|---|
| `compute_rolling_vol(returns, window = 21)` | mod_volatility |
| `compute_return_matrix(long_returns, tickers)` | mod_codynamics |
| `compute_rolling_correlation(x, y, window = 63)` | mod_codynamics, mod_hedge_ratio |
| `compute_rolling_beta(y, x, window = 63)` | mod_hedge_ratio |
| `compute_seasonal_index(long_data, ticker, freq = "month")` | mod_seasonality |
| `compute_vol_surface(long_returns, tickers, window = 21)` | mod_volatility |

All rolling functions use `zoo::rollapply`.

---

## Shiny Patterns

- Each module uses `shiny::reactiveVal(NULL)` for local data state — no shared `r` writes
- Data loads inside `observeEvent(list(input$energy_market, input$date_range), ignoreNULL=TRUE, ignoreInit=FALSE, ...)`
- Use `req()` to guard all outputs before data is loaded
- Never use `observe()` for data transformation — use `reactive()`
- Never use `<<-` for state — use `reactiveVal()` or module-local variables

---

## Deployment

**Dockerfile base:** `rocker/shiny:4.3.3`
**Docker layer order (for cache efficiency):**
1. System libs (including `pandoc` for vignette builds)
2. CRAN packages
3. GitHub packages (`RTL`) with `build_vignettes=FALSE, upgrade='never'`
4. Local package install

**GitHub Actions:** Push to `main` triggers build + push to DockerHub.
**Required GitHub secrets:** `DOCKERHUB_USERNAME`, `DOCKERHUB_TOKEN`
**Built-in secret used:** `GITHUB_TOKEN` (passed as `GITHUB_PAT` build-arg to avoid rate limiting on RTL install)
