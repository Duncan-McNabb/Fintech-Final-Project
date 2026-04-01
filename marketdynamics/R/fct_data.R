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
#' Fetches CMT rate series from FRED via the fredr package.
#' Requires the `FRED_API_KEY` environment variable to be set.
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
    series    = c("DGS1MO","DGS3MO","DGS6MO","DGS1","DGS2","DGS3",
                  "DGS5","DGS7","DGS10","DGS20","DGS30"),
    start_date = as.Date("1990-01-01"),
    end_date   = Sys.Date()
) {
  key <- Sys.getenv("FRED_API_KEY")
  if (nchar(key) == 0) stop("FRED_API_KEY environment variable is not set.")
  fredr::fredr_set_key(key)

  # Map series ID to tenor in months
  tenor_map <- c(
    DGS1MO = 1, DGS3MO = 3, DGS6MO = 6,
    DGS1 = 12, DGS2 = 24, DGS3 = 36,
    DGS5 = 60, DGS7 = 84, DGS10 = 120,
    DGS20 = 240, DGS30 = 360
  )

  rows <- lapply(series, function(s) {
    raw <- fredr::fredr(
      series_id         = s,
      observation_start = start_date,
      observation_end   = end_date
    )
    raw$series       <- s
    raw$market       <- "CMT"
    raw$contract_num <- unname(tenor_map[s])
    raw$value        <- raw$value  # already numeric; NA for missing dates
    dplyr::select(raw, date, series, market, contract_num, value)
  })

  dplyr::bind_rows(rows)
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
