#' Load energy futures data from RTL::dflong
#'
#' Filters the RTL long-format futures dataset by market prefix and date range.
#' Adds `market` (ticker prefix) and `contract_num` (integer suffix) columns.
#'
#' @param markets Character vector of market prefixes, e.g. `c("CL", "NG")`.
#'   Available: ALI, AUP, BRN, CL, EDP, HO, HTT, MJP, NG, RB.
#' @param start_date Date. Start of date range (inclusive).
#' @param end_date Date. End of date range (inclusive).
#' @return A tibble with columns: date, series, market, contract_num, value.
#' @export
load_energy_data <- function(markets, start_date, end_date) {
  df <- RTL::dflong
  df$market       <- sub("[0-9]+$", "", df$series)
  df$contract_num <- as.integer(
    regmatches(df$series, regexpr("[0-9]+$", df$series))
  )
  dplyr::filter(
    df,
    .data$market %in% markets,
    .data$date >= start_date,
    .data$date <= end_date
  )
}

#' Load FRED Constant Maturity Treasury data
#'
#' Fetches CMT rate series from FRED via tidyquant. No API key required.
#'
#' @param series Character vector of FRED series IDs.
#'   Defaults to all CMT tenors: DGS1MO, DGS3MO, DGS6MO, DGS1, DGS2, DGS3,
#'   DGS5, DGS7, DGS10, DGS20, DGS30.
#' @param start_date Date. Start of date range. Defaults to 1990-01-01.
#' @param end_date Date. End of date range. Defaults to today.
#' @return A tibble with columns: date, series, market, contract_num, value.
#'   `value` is the rate in percent (e.g. 4.5 = 4.5%). `market` is "CMT".
#'   `contract_num` is tenor in months (1, 3, 6, 12, 24, 36, 60, 84, 120, 240, 360).
#' @export
load_cmt_data <- function(
    series     = c("DGS1MO","DGS3MO","DGS6MO","DGS1","DGS2","DGS3",
                   "DGS5","DGS7","DGS10","DGS20","DGS30"),
    start_date = as.Date("1990-01-01"),
    end_date   = Sys.Date()
) {
  # Map series ID to tenor in months
  tenor_map <- c(
    DGS1MO = 1,  DGS3MO = 3,   DGS6MO = 6,
    DGS1   = 12, DGS2   = 24,  DGS3   = 36,
    DGS5   = 60, DGS7   = 84,  DGS10  = 120,
    DGS20  = 240, DGS30 = 360
  )

  raw <- tidyquant::tq_get(
    series,
    get  = "economic.data",
    from = start_date,
    to   = end_date
  )

  # tq_get returns: symbol, date, price
  raw |>
    dplyr::rename(series = symbol, value = price) |>
    dplyr::mutate(
      market       = "CMT",
      contract_num = unname(tenor_map[.data$series])
    ) |>
    dplyr::filter(!is.na(.data$value)) |>
    dplyr::select(date, series, market, contract_num, value)
}

#' Combine energy and CMT data into a single tibble
#'
#' Row-binds the two datasets and adds a `source` column ("energy" or "cmt").
#'
#' @param energy_long Tibble from `load_energy_data()`.
#' @param cmt_long Tibble from `load_cmt_data()`.
#' @return A tibble with all columns from both inputs plus a `source` column.
#' @export
combine_data <- function(energy_long, cmt_long) {
  energy_long$source <- "energy"
  cmt_long$source    <- "cmt"
  dplyr::bind_rows(energy_long, cmt_long)
}

#' Pivot energy data from long to wide format
#'
#' Filters to a single market prefix and pivots contract_num to columns,
#' so each row is a date and each column is a contract month.
#'
#' @param long_data Tibble from `load_energy_data()`.
#' @param market_prefix Single market prefix string, e.g. `"CL"`.
#' @return A wide tibble: date + one column per contract_num.
#' @export
pivot_wide <- function(long_data, market_prefix) {
  filtered <- dplyr::filter(long_data, .data$market == market_prefix)
  tidyr::pivot_wider(
    filtered,
    id_cols     = date,
    names_from  = contract_num,
    values_from = value,
    names_prefix = "c"
  )
}

#' Load pre-fetched EIA weekly data from bundled feather file
#'
#' Reads the EIA dataset from `inst/extdata/eia_data.feather`.
#' Generate this file by running `fetch_data.qmd` at the project root.
#'
#' @param roles Character vector of roles to include. Available:
#'   `gasoline_supply`, `gasoline_demand`, `crude_stocks`,
#'   `padd1_crude_stocks` through `padd5_crude_stocks`, `refinery_runs`,
#'   `cl_demand`, `ho_demand`, `rb_exports`, `ng_storage`, `rig_count`.
#'   `NULL` returns all roles.
#' @param start_date Date. Start of date range (inclusive). Defaults to 2007-01-02.
#' @param end_date Date. End of date range (inclusive). Defaults to today.
#' @return A tibble with columns: role, label, date, value.
#' @export
load_eia_data <- function(
    roles      = NULL,
    start_date = as.Date("2007-01-02"),
    end_date   = Sys.Date()
) {
  path <- system.file("extdata", "eia_data.feather", package = "marketdynamics")
  if (nchar(path) == 0) {
    stop("EIA data file not found. Run fetch_data.qmd to generate inst/extdata/eia_data.feather.")
  }
  df <- arrow::read_feather(path)
  df <- dplyr::filter(df, .data$date >= start_date, .data$date <= end_date)
  if (!is.null(roles)) {
    df <- dplyr::filter(df, .data$role %in% roles)
  }
  df
}

#' Compute log returns for long-format price data
#'
#' Groups by `series` and computes `log(value / lag(value))`.
#' First observation per series is NA.
#'
#' @param long_data Tibble with columns: date, series, value (at minimum).
#' @return The input tibble with a `log_return` column added.
#' @export
compute_log_returns <- function(long_data) {
  long_data |>
    dplyr::arrange(.data$series, .data$date) |>
    dplyr::group_by(.data$series) |>
    dplyr::mutate(log_return = log(.data$value / dplyr::lag(.data$value))) |>
    dplyr::ungroup()
}
