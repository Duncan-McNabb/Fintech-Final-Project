# Project Progress — Market Dynamics App

**Due:** April 6, 2026 at 10pm MST
**Course:** University of Alberta — Fintech3 (Posit Connect / Golem Apps)
**Last Updated:** April 4, 2026

---

## What We've Accomplished

### Infrastructure & Deployment
- ✅ **golem R package** fully scaffolded (`marketdynamics/`)
- ✅ **Dockerfile** working — base `rocker/shiny:4.3.3`, pandoc installed, RTL installed with `build_vignettes=FALSE` to bypass vignette compilation failures
- ✅ **GitHub Actions CI/CD** — pushes to `main` trigger Docker build + push to DockerHub (`docker/build-push-action@v6`)
- ✅ **GitHub repo** live with instructor collaborator access

### Data Layer (`fct_data.R`)
- ✅ `load_energy_data(markets, start, end)` — filters RTL::dflong, adds `market` + `contract_num` columns, accepts vector of prefixes
- ✅ `load_cmt_data(series, start_date, end_date)` — fetches FRED CMT data via tidyquant (no API key)
- ✅ `combine_data(energy_long, cmt_long)` — row-binds with `source` column
- ✅ `pivot_wide(long_data, market_prefix)` — long → wide by contract_num
- ✅ `compute_log_returns(long_data)` — grouped by series, log(value/lag(value))

### Analytics Layer (`fct_helpers.R`)
- ✅ `compute_rolling_vol(returns, window)` — annualized rolling SD via zoo
- ✅ `compute_rolling_correlation(x, y, window)` — rolling Pearson via zoo
- ✅ `compute_rolling_beta(y, x, window)` — rolling OLS slope via zoo
- ✅ `compute_seasonal_index(long_returns, series_id, freq)` — monthly/weekly mean return
- ✅ `compute_vol_surface(long_returns, series_ids, window)` — long vol surface tibble
- ✅ `compute_return_matrix(long_returns, series)` — wide numeric matrix for PCA

### App Architecture
- ✅ **Self-contained modules** — each module owns its `reactiveVal(NULL)` + data loading; no inter-module dependencies
- ✅ **Small r global market selector** — `input$global_market` in navbar → writes `r$market` → all 5 energy modules sync via `updateSelectInput`
- ✅ **7-tab layout** following analytical narrative (see Current State below)

### Grading Requirements (all 5 satisfied)
- ✅ **#1 Historical forward curve behavior** — `mod_forward_curve.R`: multi-contract lines + calendar spread + contango/backwardation value boxes
- ✅ **#2 Volatility across time to maturity and over time** — `mod_volatility.R`: rolling vol with regime bands (25th/75th percentile), term structure (Samuelson Effect)
- ✅ **#3 Co-dynamics across markets** — `mod_codynamics.R`: rolling correlation, return scatter (Viridis by year + beta line), PCA biplot (Energy red / Treasury blue)
- ✅ **#4 Seasonality impact** — `mod_seasonality.R`: monthly returns, year overlay, seasonal vol, seasonal calendar spread
- ✅ **#5 Hedge ratio dynamics** — `mod_hedge_ratio.R`: rolling OLS/min-variance HR, return scatter colored by year

---

## Current State of the App

### Tab Structure (7 tabs)

| Tab | Module | Key Visuals |
|---|---|---|
| Overview | (inline) | Full-viewport hero cover page |
| Market Structure | mod_forward_curve | Forward curve lines OR calendar spread + 3 value boxes (Backwardation/Contango, M1-M2 spread, historical percentile) |
| Seasonality | mod_seasonality | Monthly returns, year overlay, seasonal vol, seasonal spread |
| Volatility | mod_volatility | Rolling vol + regime bands, term structure |
| Cross-Market | mod_codynamics | Rolling correlation, return scatter, PCA, **Treasury Curve**, **Yield Spreads** |
| Hedge Ratios | mod_hedge_ratio | Rolling OLS/MinVar HR, return scatter |

### Market-Specific Bottom Panels (context-aware, loaded on demand)

| Tab | Market | Content |
|---|---|---|
| Market Structure | BRN | WTI vs. Brent price + spread chart |
| Market Structure | CL/NG/HO/RB | Market-specific info cards (drivers, curve signals) |
| Seasonality | NG | Front-month price with injection/withdrawal shading |
| Seasonality | HO/RB | Info cards (winter heating peak / RVP transitions) |
| Volatility | CL/HO/RB | Outright vol vs. crack spread vol (two lines) |
| Volatility | NG/BRN | Info cards (weather/storage vol / geopolitical vol) |
| Hedge Ratios | CL/HO/RB | HO-CL & RB-CL crack spreads + 3-2-1 crack (side by side) |
| Hedge Ratios | NG/BRN | Info cards (spark spread / WTI-Brent basis risk) |

### Files in codebase
```
R/app_config.R          — golem config
R/app_server.R          — global market selector + module wiring
R/app_ui.R              — page_navbar layout
R/fct_data.R            — all data loading functions
R/fct_helpers.R         — all analytical computations
R/mod_forward_curve.R   — Market Structure tab (ACTIVE)
R/mod_volatility.R      — Volatility tab (ACTIVE)
R/mod_seasonality.R     — Seasonality tab (ACTIVE)
R/mod_codynamics.R      — Cross-Market tab (ACTIVE, includes Treasury views)
R/mod_hedge_ratio.R     — Hedge Ratios tab (ACTIVE)
R/mod_treasury_curve.R  — UNWIRED (logic moved into mod_codynamics.R)
R/mod_specific_info.R   — UNWIRED (logic moved into respective modules)
R/mod_inputs.R          — legacy, unused
R/run_app.R             — entry point
```

---

## Next Steps

### High Priority (before April 6 deadline)
- [ ] **Unit tests** — `tests/testthat/` directory missing entirely; DESCRIPTION lists `testthat` in Suggests. Add basic tests for `fct_data.R` and `fct_helpers.R` functions (e.g., `test_that("compute_log_returns returns NA for first obs", ...)`)
- [ ] **LICENSE file** — DESCRIPTION says `MIT + file LICENSE` but the file doesn't exist; add it
- [ ] **Verify Docker build** — confirm most recent push to DockerHub succeeded (check GitHub Actions run for commit `31ddfa9`)
- [ ] **Presentation prep** — 8-minute presentation; key talking points should walk through the analytical narrative of the app (market structure → seasonality → volatility → cross-market → hedge ratios)

### Medium Priority
- [ ] **Remove dead modules** from NAMESPACE / DESCRIPTION — `mod_treasury_curve.R` and `mod_specific_info.R` are still exported but unwired; either delete them or mark `@noRd` and remove `@export`
- [ ] **Plot height tuning** — now that bottom panels are present on each tab, the main plot heights may need adjustment so the bottom panel is visible without excessive scrolling
- [ ] **Test global market selector** end-to-end — verify that selecting "NG" in the navbar immediately syncs Market Structure, Seasonality, Volatility, and Hedge Ratios tabs

### Nice to Have
- [ ] Add `shiny::withSpinner()` to plotly outputs so loading state is visible during data fetch
- [ ] Global date range selector (similar to global market selector) — currently each tab keeps its own date range
- [ ] Add a `NEWS.md` or version tag for submission

---

## Deliverables Checklist

| Deliverable | Status |
|---|---|
| Working golem R package | ✅ |
| GitHub repo with instructor access | ✅ |
| Docker image on DockerHub via GitHub Actions | ✅ (verify latest build) |
| 5 grading requirements in the app | ✅ |
| 8-minute presentation | ⬜ Not started |
| Unit tests | ⬜ Missing |
| LICENSE file | ⬜ Missing |
