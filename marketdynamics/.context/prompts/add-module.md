# Prompt: Add a New Golem Module

Use this prompt template when adding a new analysis tab to the `marketdynamics` app.

---

## How to Use

Fill in the variables below, then paste the full prompt into Claude Code.

---

## Prompt Template

```
Read `.context/ai-rules.md` and `.context/project-context.md` before starting.

Add a new golem module to the marketdynamics app with the following spec:

**Module name:** [e.g., mod_spreads]
**Tab title:** [e.g., "Spreads"]

**Reads from `r`:**
- [e.g., r$energy_long — filtered energy futures data]
- [e.g., r$date_range — selected date range]

**Module-level UI inputs:**
- [e.g., selectInput("market_x") — first market for spread calculation]
- [e.g., selectInput("market_y") — second market]
- [e.g., sliderInput("window") — rolling window in days]

**Visualizations:**
1. [e.g., Plotly line chart — spread over time (market_x price - market_y price)]
2. [e.g., Plotly histogram — distribution of spread values]
3. [e.g., bslib value boxes — current spread, 1-year average, z-score]

**Analytical computation needed (add to fct_helpers.R if new):**
- [e.g., compute_spread(long_data, ticker_x, ticker_y) — returns tibble with date and spread columns]

Do the following:
1. Create R/mod_[name].R with both mod_[name]_ui() and mod_[name]_server() functions
2. Register the module in R/app_ui.R (add nav_panel) and R/app_server.R (call module server)
3. Add any new fct_ functions to the appropriate fct_ file with roxygen2 docs
4. Add a unit test stub in tests/testthat/test-mod_[name].R using shiny::testServer()
5. Follow the Small r Strategy — this module only reads from r, never writes
6. Use req() before any computation on r$* values
7. Use NS(id) for all input/output IDs in the UI function
```

---

## Example (Filled In)

```
Read `.context/ai-rules.md` and `.context/project-context.md` before starting.

Add a new golem module to the marketdynamics app with the following spec:

**Module name:** mod_spreads
**Tab title:** "Spreads"

**Reads from `r`:**
- r$energy_long — filtered energy futures data
- r$date_range — selected date range

**Module-level UI inputs:**
- selectInput("market_x") — first market (e.g., CL)
- selectInput("market_y") — second market (e.g., HO)
- radioButtons("spread_type") — "Price Spread" or "Ratio"

**Visualizations:**
1. Plotly line chart — spread over time
2. Plotly histogram — spread distribution with mean/std lines
3. bslib value boxes — current spread, 1yr average, z-score

**Analytical computation needed:**
- compute_spread(long_data, ticker_x, ticker_y, type) — returns tibble {date, spread}

Do the following:
1. Create R/mod_spreads.R ...
[rest of instructions]
```
