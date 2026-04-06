# Glossary

Project-specific terminology. When these terms appear in a request, use these definitions.

---

## Financial Terms

**Forward Curve**
The set of prices for a commodity or financial instrument across successive delivery dates (contract months). Plotted as price vs. time to expiry. Shape reveals market expectations about future supply/demand.

**Contango**
Forward curve shape where prices for later delivery dates are *higher* than near-term dates. Typically occurs when spot supply is adequate or storage is profitable. Roll yield is negative for long holders.

**Backwardation**
Forward curve shape where prices for later delivery dates are *lower* than near-term dates. Typical in supply-constrained or high-demand conditions. Roll yield is positive for long holders.

**Roll Yield**
The gain or loss from rolling a futures position forward as it approaches expiry. Calculated as `(M2 price - M1 price) / M1 price`. Negative in contango, positive in backwardation.

**Continuous Futures Contract**
A synthetic time series created by splicing together successive front-month futures contracts. Removes expiry gaps, enabling long-term historical analysis. `RTL::dflong` contains these series.

**Contract Number (contract_num)**
The position of a futures contract in the forward curve, where 1 = front month (nearest expiry), 2 = second nearest, etc. Extracted from ticker suffix (e.g., `CL01` → contract_num = 1).

**Constant Maturity Treasury (CMT)**
US Treasury interest rates at fixed maturities (1 month through 30 years), published daily by FRED. Because actual Treasury bonds have varying remaining maturities, CMT rates are interpolated to fixed tenors.

**Yield Curve**
The plot of CMT interest rates across maturities at a point in time. Normal shape = upward sloping (longer maturities yield more). Inverted = shorter maturities yield more (recession signal).

**Term Structure of Volatility**
How volatility changes across contract maturities. For energy futures, near-term contracts are typically more volatile than deferred contracts — known as the Samuelson Effect.

**Samuelson Effect**
The empirical observation that futures price volatility decreases as time to expiry increases. Near-term contracts react more to supply/demand news than deferred contracts.

**Realized Volatility**
Volatility estimated from historical price changes. Calculated as the annualized standard deviation of log returns over a rolling window. Formula: `sd(log_returns) * sqrt(252)` for daily data.

**Vol Surface**
A 2D representation of realized volatility: one axis = date (time), other axis = contract number or CMT tenor, z-value = volatility. Reveals how the term structure of volatility evolves over time.

**Hedge Ratio**
The quantity of a hedge instrument required to offset one unit of exposure. Computed via OLS regression (beta) or as the ratio of volatilities times correlation (minimum variance hedge).

**Minimum Variance Hedge Ratio**
`HR = ρ(x,y) × σ(x) / σ(y)` where ρ = correlation, σ = standard deviation of returns. Minimizes variance of the hedged portfolio without requiring regression.

**OLS Beta (Rolling)**
The slope coefficient from regressing exposure returns on hedge instrument returns over a rolling window. Gives a time-varying estimate of the hedge ratio. Computed via `zoo::rollapply` with inline `lm`.

**Crack Spread**
The price difference between refined petroleum products (e.g., heating oil, gasoline) and crude oil. Represents refining margin. Common: 3-2-1 crack spread = (2 × gasoline + 1 × heating oil - 3 × crude) / 3.

**Heat Rate Spread**
In natural gas/power markets: the ratio of electricity price to natural gas price. Represents the efficiency of converting gas to electricity. Used for hedging gas-fired power plants.

**Co-dynamics**
How multiple markets move together over time. Analyzed through correlation matrices, rolling pairwise correlations, and PCA. Critical for understanding portfolio concentration risk.

**Principal Component Analysis (PCA)**
A dimensionality reduction technique. Applied to a matrix of market returns, the first PC typically explains parallel shifts (all markets move together), the second explains slope changes (short vs. long term moves differently).

---

## golem / Shiny Terms

**Small r Strategy**
The design pattern used in this app for sharing reactive state between modules. A single `r <- shiny::reactiveValues()` object is created in `app_server.R` and passed as an argument to every module server. Only `mod_inputs_server` writes to `r`; all analysis modules read from it.

**Module (`mod_`)**
A self-contained Shiny UI/server pair created with `golem::add_module()`. Each module owns one analysis tab. Files are prefixed `mod_` (e.g., `mod_volatility.R`). Requires `NS(id)` namespacing in the UI function.

**Helper Function (`fct_`)**
A pure R function (no reactivity) that performs data loading or computation. Lives in `fct_data.R` or `fct_helpers.R`. Pure functions are testable in isolation. Files are prefixed `fct_`.

**NS() / Namespace**
`NS(id)` creates a namespacing function inside a module UI. Applied to all input/output IDs to prevent collisions when the same module is instantiated multiple times. Example: `ns <- NS(id); textInput(ns("ticker"), ...)`.

**`reactiveValues`**
An R list-like object where every element is a reactive expression. Changes to any element automatically invalidate downstream reactives. Used in the Small r Strategy to store shared app state (`r <- shiny::reactiveValues(...)`).

**`reactive()`**
Creates a cached reactive expression. Re-evaluates only when its dependencies change. Use for data transformations. Returns a function — call with `()` to get the value.

**`observeEvent()`**
Runs code as a side effect when a specific event fires (e.g., button click). Does not return a value. Used in `mod_inputs_server` to gate data loading behind the Update button.

**`req()`**
Silently stops reactive execution if any argument is NULL, FALSE, or zero-length. Prevents errors during app initialization before data is loaded. Always use before computing on `r$*` values.

---

## Data Terms

**`RTL::dflong`**
The primary energy futures dataset from the RTL R package (GitHub: `patzlaw/RTL`). Long-format tibble of historical continuous futures contracts. Must inspect column names with `glimpse(RTL::dflong)` before writing data loading code. Contract number encoded in ticker suffix.

**FRED**
Federal Reserve Economic Data. Free public database of US and international economic time series. Accessed via the `fredr` R package using a free API key (`FRED_API_KEY` env var).

**DGS Series**
FRED series identifiers for Constant Maturity Treasury rates:
- `DGS1MO` = 1-month CMT
- `DGS3MO` = 3-month CMT
- `DGS6MO` = 6-month CMT
- `DGS1` = 1-year CMT
- `DGS2` = 2-year CMT
- `DGS3` = 3-year CMT
- `DGS5` = 5-year CMT
- `DGS7` = 7-year CMT
- `DGS10` = 10-year CMT
- `DGS20` = 20-year CMT
- `DGS30` = 30-year CMT

Values are in percent (e.g., 4.5 = 4.5%). Divide by 100 for log-return calculations.

**Log Return**
`log(P_t / P_{t-1})` — the continuously compounded return. Preferred over simple returns for volatility and correlation calculations because they are time-additive and more normally distributed.
