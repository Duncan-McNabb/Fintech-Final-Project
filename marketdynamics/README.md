# Market Dynamics

An interactive analytics dashboard for energy futures and US Treasury markets, built as a production-grade [golem](https://thinkr-open.github.io/golem/) R Shiny application.

**University of Alberta | Fintech 3 Capstone | April 2026**

## Overview

Market Dynamics provides six analytical modules covering the core dimensions of commodity and fixed-income market analysis:

| Tab | What it shows |
|-----|---------------|
| **Market Structure** | Forward curve dynamics, M1-M2 calendar spread, WTI vs Brent location spread, crude inventory overlay |
| **Seasonality** | Monthly return patterns, percentile-band seasonal overlay, seasonal volatility, calendar spread seasonality |
| **Volatility** | Rolling realized vol with regime highlighting, Samuelson Effect term structure, crack spread vol comparison |
| **Cross-Market** | Rolling correlation, return scatter with OLS beta, PCA biplot, Treasury yield curve, yield spread recession indicators |
| **Hedge Ratios** | Rolling OLS and min-variance hedge ratios, return scatter, crack spread and 3-2-1 refinery margins |
| **Simulation** | Geometric Brownian Motion and Ornstein-Uhlenbeck (mean-reversion) Monte Carlo price simulation with percentile fan charts |

**Markets covered:** WTI Crude (CL), Brent Crude (BRN), Natural Gas (NG), Heating Oil (HO), RBOB Gasoline (RB), US Treasury yields (1M-30Y).

## Quick Start

### Docker (recommended)

```bash
docker pull duncanmcnabb/marketdynamics:latest
docker run -p 3838:3838 duncanmcnabb/marketdynamics:latest
```

Open [http://localhost:3838](http://localhost:3838).

### From source

```r
# install.packages("remotes")
remotes::install_local(".")
marketdynamics::run_app()
```

## Architecture

```
marketdynamics/
├── R/
│   ├── app_config.R          # Shared constants (market lists, labels)
│   ├── app_ui.R              # bslib page_navbar layout
│   ├── app_server.R          # Cross-module reactive wiring
│   ├── fct_data.R            # Data loading (feather files)
│   ├── fct_helpers.R         # Rolling vol, correlation, beta, PCA helpers
│   ├── mod_forward_curve.R   # Market structure module
│   ├── mod_seasonality.R     # Seasonality module
│   ├── mod_volatility.R      # Volatility module
│   ├── mod_codynamics.R      # Cross-market module
│   ├── mod_hedge_ratio.R     # Hedge ratio module
│   └── mod_simulation.R      # Stochastic simulation module
├── inst/extdata/
│   ├── energy_data.feather   # RTL energy futures (CL, BRN, NG, HO, RB)
│   ├── eia_data.feather      # EIA weekly petroleum & gas indicators
│   └── cmt_data.feather      # FRED US Treasury CMT yields
├── tests/testthat/           # 62 unit tests
├── Dockerfile
└── .github/workflows/
    ├── docker-build-push.yml # Auto-build Docker image on push
    └── refresh-data.yml      # Daily EIA + FRED data refresh (weekdays)
```

## Data Pipeline

All market data is pre-fetched and bundled as Apache Arrow feather files to eliminate runtime API dependencies:

- **Energy futures** — sourced from the RTL package (`RTL::dflong`), covering front-month through deferred contracts
- **EIA indicators** — 14 weekly series including crude stocks, refinery runs, gasoline supply/demand, NG storage, and Baker Hughes rig count, fetched via the EIA v2 API
- **Treasury yields** — 11 constant-maturity tenors (1M through 30Y) from FRED via tidyquant

A GitHub Actions workflow refreshes EIA and Treasury data automatically every weekday and rebuilds the Docker image with updated data.

## CI/CD

- **Docker build** triggers on every push to `main`, pushing to [DockerHub](https://hub.docker.com/r/duncanmcnabb/marketdynamics)
- **Data refresh** runs on a weekday cron schedule (12:00 UTC), commits updated feather files if data changed, which in turn triggers a fresh Docker build

## Testing

```r
devtools::test()
# 62 tests covering data loading, helper functions, simulation math, and edge cases
```

## Tech Stack

R 4.3+ | Shiny | golem | bslib (Bootstrap 5) | plotly | dplyr | arrow | Docker | GitHub Actions
