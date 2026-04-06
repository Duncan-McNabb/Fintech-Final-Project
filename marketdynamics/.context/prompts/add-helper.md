# Prompt: Add a New Helper Function (fct_)

Use this prompt template when adding a new pure analytical or data loading function.

---

## How to Use

Fill in the variables below, then paste the full prompt into Claude Code.

---

## Prompt Template

```
Read `.context/ai-rules.md` and `.context/project-context.md` before starting.

Add a new helper function to the marketdynamics package with the following spec:

**Function name:** [e.g., compute_crack_spread]
**File:** [e.g., R/fct_helpers.R — or R/fct_data.R if it's a data loading function]

**Purpose:** [one sentence — what does it compute or return?]
[e.g., Computes the 3-2-1 crack spread between crude oil, gasoline, and heating oil.]

**Arguments:**
- `[arg_name]` ([type]): [description]
  [e.g., `long_data` (tibble): filtered energy futures data from r$energy_long]
  [e.g., `window` (integer): rolling window in trading days, default 21]

**Returns:** [describe the output]
[e.g., A tibble with columns {date, spread} where spread is the 3-2-1 crack spread in USD/bbl.]

**Implementation notes:**
- [any specific logic, formula, or edge cases to handle]
  [e.g., Formula: (2 × RB + 1 × HO - 3 × CL) / 3, all prices in USD/bbl (multiply RB/HO by 42 to convert from gal to bbl)]
  [e.g., Return NA for dates where any of the three tickers is missing]

Do the following:
1. Add the function to [file] with a full roxygen2 header (@param, @return, @export if public, @examples)
2. Add a unit test in tests/testthat/test-fct_[file_suffix].R with at least:
   - One test on a minimal known input verifying the output is correct
   - One test verifying NA handling for missing data
3. The function must be a pure function — no reactivity, no Shiny dependencies, no side effects
4. Use :: notation for all package calls (e.g., dplyr::filter(), not filter())
```

---

## Example (Filled In)

```
Read `.context/ai-rules.md` and `.context/project-context.md` before starting.

Add a new helper function to the marketdynamics package with the following spec:

**Function name:** compute_crack_spread
**File:** R/fct_helpers.R

**Purpose:** Computes the 3-2-1 crack spread from energy futures data.

**Arguments:**
- `long_data` (tibble): filtered energy futures data (date, ticker, contract_num, price)
- `contract_num` (integer): which contract month to use, default 1 (front month)

**Returns:** A tibble with columns {date, spread} where spread is the 3-2-1 crack spread in USD/bbl.

**Implementation notes:**
- Formula: (2 × RB_price × 42 + 1 × HO_price × 42 - 3 × CL_price) / 3
- Multiply RB and HO by 42 to convert from USD/gallon to USD/barrel
- Return NA rows where any ticker is missing for that date

Do the following:
1. Add the function to R/fct_helpers.R with full roxygen2 header
2. Add unit tests in tests/testthat/test-fct_helpers.R
3. Pure function — no reactivity
4. Use :: notation throughout
```
