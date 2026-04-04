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

**Calendar Spread (Time Spread)**
The price difference between two contract months in the same market (e.g., M1 − M2). Positive = backwardation (nearer month at a premium). Negative = contango. The single most important market structure signal for physical traders. For WTI: the Dec/Jan time spread is the primary trade signal.

**Continuous Futures Contract**
A synthetic time series created by splicing together successive front-month futures contracts. Removes expiry gaps, enabling long-term historical analysis. `RTL::dflong` contains these series.

**Contract Number (contract_num)**
The position of a futures contract in the forward curve, where 1 = front month (nearest expiry), 2 = second nearest, etc. Extracted from ticker suffix (e.g., `CL01` → contract_num = 1).

**Constant Maturity Treasury (CMT)**
US Treasury interest rates at fixed maturities (1 month through 30 years), published daily by FRED. Because actual Treasury bonds have varying remaining maturities, CMT rates are interpolated to fixed tenors.

**Yield Curve**
The plot of CMT interest rates across maturities at a point in time. Normal shape = upward sloping (longer maturities yield more). Inverted = shorter maturities yield more (historically a recession signal). Key spreads: 2s10s (10yr minus 2yr), 3m10y (10yr minus 3-month).

**Yield Curve Inversion**
When short-term Treasury rates exceed long-term rates (negative 2s10s or 3m10y spread). Has preceded every US recession in the past 50 years. A primary macro signal for risk/trading managers presenting to senior leadership.

**Term Structure of Volatility**
How volatility changes across contract maturities. For energy futures, near-term contracts are typically more volatile than deferred contracts — known as the Samuelson Effect.

**Samuelson Effect**
The empirical observation that futures price volatility decreases as time to expiry increases. Near-term contracts react more to supply/demand news than deferred contracts.

**Realized Volatility**
Volatility estimated from historical price changes. Calculated as the annualized standard deviation of log returns over a rolling window. Formula: `sd(log_returns) * sqrt(252)` for daily data.

**Vol Surface**
A 2D representation of realized volatility: one axis = date (time), other axis = contract number or CMT tenor, z-value = volatility. Reveals how the term structure of volatility evolves over time. Note: heatmap representation is prohibited in this app — use line charts per contract or bar charts for the term structure snapshot.

**Hedge Ratio**
The quantity of a hedge instrument required to offset one unit of exposure. Computed via OLS regression (beta) or as the ratio of volatilities times correlation (minimum variance hedge).

**Minimum Variance Hedge Ratio**
`HR = ρ(x,y) × σ(y) / σ(x)` where ρ = correlation, σ = standard deviation of returns. Minimizes variance of the hedged portfolio without requiring regression.

**OLS Beta (Rolling)**
The slope coefficient from regressing exposure returns on hedge instrument returns over a rolling window. Gives a time-varying estimate of the hedge ratio. Computed via `zoo::rollapply` with inline `lm`.

**Crack Spread**
The price difference between refined petroleum products (e.g., heating oil, gasoline) and crude oil. Represents refining margin. HO-CL = distillate crack; RB-CL = gasoline crack.

**3-2-1 Crack Spread**
Industry-standard refinery margin metric: `(2 × RB_front + 1 × HO_front − 3 × CL_front) / 3`. Represents the profit from cracking 3 barrels of crude into 2 barrels of gasoline and 1 barrel of distillate. The number refiners hedge against.

**Heat Rate Spread**
In natural gas/power markets: the ratio of electricity price to natural gas price. Represents the efficiency of converting gas to electricity. Used for hedging gas-fired power plants.

**Spark Spread**
`Power Price ($/MWh) − (Gas Price ($/MMBtu) × Heat Rate (MMBtu/MWh))`. The implied profitability of running a gas-fired power plant. Positive = profitable to generate; key demand signal for natural gas. Typical heat rate: 7,000-8,000 BTU/kWh.

**PADD (Petroleum Administration for Defense District)**
EIA's regional framework for US petroleum data. PADD 1 = East Coast/Northeast (primary HO demand), PADD 2 = Midwest (agricultural diesel), PADD 3 = Gulf Coast (dominant refining hub), PADD 4 = Rocky Mountain, PADD 5 = West Coast. Regional supply/demand imbalances drive basis spreads between PADDs.

**RVP (Reid Vapor Pressure)**
A measure of gasoline volatility. EPA regulations require lower RVP (less evaporative emissions) in summer months, mandating a more expensive summer-blend gasoline. The spring RVP transition (April/May) drives annual gasoline price seasonality independent of crude oil moves.

**Henry Hub**
The primary US natural gas pricing benchmark, located in Erath, Louisiana. All NYMEX NG futures are priced at Henry Hub delivery. Linked to global LNG markets since US LNG export capacity grew post-2016.

**LNG (Liquefied Natural Gas)**
Natural gas cooled to −162°C for transport by ship. US LNG export capacity grew from near-zero in 2016 to ~14 Bcf/day by 2025, permanently adding ~2 Bcf/day of structural demand and linking Henry Hub prices to global gas markets (JKM in Asia, TTF in Europe).

**Storage Injection/Withdrawal Cycle (NG)**
The annual pattern of natural gas storage: April-October = injection season (operators fill underground storage, prices tend soft); November-March = withdrawal season (inventory drawn for heating, prices firm). The weekly EIA storage report (Thursday release) is compared to the 5-year seasonal average — surplus = price weakness, deficit = price strength.

**WTI-Brent Spread**
The price differential between WTI (West Texas Intermediate, US benchmark, Cushing OK delivery) and Brent (North Sea, global benchmark). Pre-2013: WTI at premium to Brent. 2011-2015: WTI at persistent discount as Cushing filled from shale production. Post-2018: converged toward parity as US export infrastructure (Seaway Pipeline reversal, new Gulf Coast terminals) cleared the Cushing overhang.

**EIA Weekly Inventory Report**
US Energy Information Administration releases weekly petroleum supply data. Wednesday 10:30am ET: crude oil and products (gasoline, distillates). Thursday 10:30am ET: natural gas storage. These two releases are the most important near-term price catalysts in US energy markets — price moves of 2-5% within minutes of the release are common.

**Co-dynamics**
How multiple markets move together over time. Analyzed through rolling pairwise correlations and cross-market return scatters. Critical for understanding portfolio concentration risk and identifying diversification opportunities.

**Principal Component Analysis (PCA)**
A dimensionality reduction technique. Applied to a matrix of market returns, the first PC typically explains parallel shifts (all markets move together), the second explains slope changes (short vs. long term moves differently).

---

## golem / Shiny Terms

**Module (`mod_`)**
A self-contained Shiny UI/server pair created with `golem::add_module()`. Each module owns one analysis tab. Files are prefixed `mod_` (e.g., `mod_volatility.R`). Requires `NS(id)` namespacing in the UI function.

**Helper Function (`fct_`)**
A pure R function (no reactivity) that performs data loading or computation. Lives in `fct_data.R` or `fct_helpers.R`. Pure functions are testable in isolation. Files are prefixed `fct_`.

**NS() / Namespace**
`NS(id)` creates a namespacing function inside a module UI. Applied to all input/output IDs to prevent collisions when the same module is instantiated multiple times. Example: `ns <- NS(id); textInput(ns("ticker"), ...)`.

**`reactiveValues`**
An R list-like object where every element is a reactive expression. Changes to any element automatically invalidate downstream reactives. In this app, `r <- shiny::reactiveValues()` is created in `app_server.R` but modules use their own `reactiveVal(NULL)` for local state.

**`reactiveVal`**
A single reactive value (as opposed to `reactiveValues` which is a list). Each module uses `energy_data <- shiny::reactiveVal(NULL)` as its local data container. Set by calling `energy_data(new_value)`, read by calling `energy_data()`.

**`reactive()`**
Creates a cached reactive expression. Re-evaluates only when its dependencies change. Use for data transformations. Returns a function — call with `()` to get the value.

**`observeEvent()`**
Runs code as a side effect when a specific event fires (e.g., input change). Does not return a value. Used in each module to trigger data loading when market or date inputs change.

**`req()`**
Silently stops reactive execution if any argument is NULL, FALSE, or zero-length. Prevents errors during app initialization before data is loaded. Always use before computing on reactive data values.

---

## Data Terms

**`RTL::dflong`**
The primary energy futures dataset from the RTL R package (GitHub: `patzlaw/RTL`). Long-format tibble of historical continuous futures contracts. Must inspect column names with `glimpse(RTL::dflong)` before writing data loading code. Contract number encoded in series suffix (e.g., `CL01` → contract 1).

**FRED**
Federal Reserve Economic Data. Free public database of US and international economic time series. Accessed via `tidyquant::tq_get(get="economic.data")` — no API key required.

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
