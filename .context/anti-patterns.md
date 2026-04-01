# Anti-Patterns

Patterns to actively avoid in this codebase. These override any conflicting defaults.

---

## Shiny Reactivity Anti-Patterns

**Using `observe()` for data transformation**
```r
# WRONG
observe({
  filtered <- dplyr::filter(r$energy_long, ticker == input$ticker)
  output$plot <- renderPlotly({ plot_ly(filtered) })
})

# RIGHT
filtered <- reactive({
  req(r$energy_long, input$ticker)
  dplyr::filter(r$energy_long, ticker == input$ticker)
})
output$plot <- renderPlotly({ plot_ly(filtered()) })
```

**Calling `reactive()` inside `observeEvent()`**
```r
# WRONG — reactive() inside observeEvent() does not behave as expected
observeEvent(input$update, {
  data <- reactive({ load_energy_data() })  # this does nothing useful
})

# RIGHT — compute directly inside observeEvent() for side effects
observeEvent(input$update, {
  r$energy_long <- load_energy_data(input$tickers, input$start, input$end)
})
```

**Writing to `r` from analysis modules**
```r
# WRONG — only mod_inputs should write to r
mod_volatility_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    r$vol_data <- computed_vol  # never do this
  })
}

# RIGHT — analysis modules only read from r
mod_volatility_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    vol_data <- reactive({
      req(r$energy_long)
      compute_rolling_vol(r$energy_long, input$window)
    })
  })
}
```

**Not guarding against NULL with `req()`**
```r
# WRONG — will error on startup before data loads
output$plot <- renderPlotly({
  dplyr::filter(r$energy_long, ticker == "CL")  # r$energy_long is NULL initially
})

# RIGHT
output$plot <- renderPlotly({
  req(r$energy_long)
  dplyr::filter(r$energy_long, ticker == "CL")
})
```

**Triggering data loads on every reactive change**
```r
# WRONG — loads data every time any input changes
observe({
  r$energy_long <- load_energy_data(input$tickers, input$start, input$end)
})

# RIGHT — gate behind actionButton in mod_inputs
observeEvent(input$update, {
  r$energy_long <- load_energy_data(input$tickers, input$start, input$end)
})
```

**Using `<<-` for shared state**
```r
# WRONG — global mutable state is unpredictable in Shiny
energy_cache <<- load_energy_data()

# RIGHT — use r$* for shared reactive state
r$energy_long <- load_energy_data()
```

---

## R Package Anti-Patterns

**`library()` or `require()` inside package R files**
```r
# WRONG
library(dplyr)
filter(data, date > "2007-01-01")

# RIGHT
dplyr::filter(data, date > "2007-01-01")
```

**Modifying `NAMESPACE` manually**
```
# WRONG — never edit NAMESPACE by hand
# RIGHT — run devtools::document() to regenerate from roxygen2 tags
```

**Hard-coding file paths**
```r
# WRONG
readRDS("/Users/duncanmcnabb/data/cache.rds")

# RIGHT — use system.file() for package assets
system.file("app", "www", "custom.css", package = "marketdynamics")
```

**Storing API keys in code**
```r
# WRONG
fredr::fredr_set_key("abc123secretkey")

# RIGHT
fredr::fredr_set_key(Sys.getenv("FRED_API_KEY"))
```

**Importing entire packages when only one function is needed**
```r
# WRONG in DESCRIPTION
Imports: tidyverse

# RIGHT — import only what you use
Imports: dplyr, tidyr, lubridate
```

---

## golem Anti-Patterns

**Business logic in `app_ui.R` or `app_server.R`**
```r
# WRONG — data loading in app_server.R
app_server <- function(input, output, session) {
  data <- RTL::dflong |> dplyr::filter(...)  # belongs in fct_data.R
}

# RIGHT — app_server.R only initializes r and calls module servers
app_server <- function(input, output, session) {
  r <- shiny::reactiveValues(...)
  mod_inputs_server("inputs_1", r = r)
}
```

**Creating modules without `NS()` namespacing**
```r
# WRONG — input IDs will collide across modules
mod_volatility_ui <- function(id) {
  sliderInput("window", "Window", 5, 252, 21)  # no namespace
}

# RIGHT
mod_volatility_ui <- function(id) {
  ns <- NS(id)
  sliderInput(ns("window"), "Window", 5, 252, 21)
}
```

**Not using golem file prefixes**
```
# WRONG
R/volatility_helpers.R
R/inputs.R

# RIGHT
R/fct_helpers.R
R/mod_inputs.R
```

---

## Performance Anti-Patterns

**Loading all of `RTL::dflong` on every reactive trigger**
```r
# WRONG — dflong is large; loading on every trigger is slow
output$plot <- renderPlotly({
  data <- RTL::dflong |> dplyr::filter(ticker == input$ticker)
})

# RIGHT — load once in mod_inputs on button click, cache in r$energy_long
```

**Re-fetching FRED data on every session**
```r
# WRONG — API call on every session start
app_server <- function(input, output, session) {
  cmt <- fredr::fredr("DGS10")  # called for every user
}

# RIGHT — fetch in mod_inputs on button click only; consider bindCache()
```

**Using `Sys.sleep()` in Shiny**
```r
# WRONG — blocks the entire R session
observeEvent(input$update, {
  Sys.sleep(2)
  r$data <- load_data()
})

# RIGHT — if showing progress is needed, use shiny::withProgress()
```

**Leaving `browser()` or `debugonce()` in committed code**
```r
# WRONG
compute_rolling_vol <- function(returns, window) {
  browser()  # never commit this
  zoo::rollapply(returns, window, sd)
}
```
