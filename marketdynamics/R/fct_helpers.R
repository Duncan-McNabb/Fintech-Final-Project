#' Compute annualized rolling realized volatility
#'
#' @param returns Numeric vector of log returns.
#' @param window Integer. Rolling window in trading days. Default 21.
#' @return Numeric vector of annualized rolling standard deviations (same length as input).
#'   Values within the first `window - 1` observations are NA.
#' @export
compute_rolling_vol <- function(returns, window = 21) {
  zoo::rollapply(returns, width = window, FUN = sd, fill = NA, align = "right") * sqrt(252)
}

#' Build a wide return matrix from long-format return data
#'
#' Pivots log returns to a matrix where rows are dates and columns are series.
#' Drops rows with any NA (inner join on dates).
#'
#' @param long_returns Tibble from `compute_log_returns()` with columns: date, series, log_return.
#' @param series Character vector of series to include.
#' @return A numeric matrix with rownames = dates and colnames = series.
#' @export
compute_return_matrix <- function(long_returns, series) {
  wide <- long_returns |>
    dplyr::filter(.data$series %in% .env$series) |>
    dplyr::select(date, series, log_return) |>
    tidyr::pivot_wider(names_from = series, values_from = log_return) |>
    dplyr::arrange(date)

  mat <- as.matrix(wide[, -1])
  rownames(mat) <- as.character(wide$date)
  mat[stats::complete.cases(mat), , drop = FALSE]
}

#' Compute rolling pairwise correlation between two return series
#'
#' @param x Numeric vector of log returns (series 1).
#' @param y Numeric vector of log returns (series 2). Must be same length as x.
#' @param window Integer. Rolling window in trading days. Default 63.
#' @return Numeric vector of rolling correlations (same length as input). NAs for initial window.
#' @export
compute_rolling_correlation <- function(x, y, window = 63) {
  xy <- cbind(x, y)
  zoo::rollapply(
    xy,
    width   = window,
    FUN     = function(m) stats::cor(m[, 1], m[, 2], use = "complete.obs"),
    fill    = NA,
    align   = "right",
    by.column = FALSE
  )
}

#' Compute rolling OLS beta (hedge ratio)
#'
#' Regresses y on x over a rolling window and returns the slope coefficient.
#'
#' @param y Numeric vector. The exposure (dependent variable).
#' @param x Numeric vector. The hedge instrument (independent variable). Same length as y.
#' @param window Integer. Rolling window in trading days. Default 63.
#' @return Numeric vector of rolling betas (same length as input). NAs for initial window.
#' @export
compute_rolling_beta <- function(y, x, window = 63) {
  xy <- cbind(y, x)
  zoo::rollapply(
    xy,
    width   = window,
    FUN     = function(m) {
      fit <- stats::lm(m[, 1] ~ m[, 2])
      stats::coef(fit)[2]
    },
    fill      = NA,
    align     = "right",
    by.column = FALSE
  )
}

#' Compute seasonal index for a single series
#'
#' Groups log returns by calendar period (month or week) and computes the
#' mean and standard error across all years in the sample.
#'
#' @param long_returns Tibble from `compute_log_returns()` with columns: date, series, log_return.
#' @param series_id Single series string, e.g. `"CL01"`.
#' @param freq Character. Either `"month"` (default) or `"week"`.
#' @return A tibble with columns: period (1–12 or 1–52), mean_return, se_return, n_years.
#' @export
compute_seasonal_index <- function(long_returns, series_id, freq = "month") {
  filtered <- dplyr::filter(long_returns, .data$series == series_id)

  filtered <- dplyr::mutate(filtered,
    period = if (freq == "month") {
      lubridate::month(.data$date)
    } else {
      lubridate::isoweek(.data$date)
    }
  )

  filtered |>
    dplyr::filter(!is.na(.data$log_return)) |>
    dplyr::group_by(.data$period) |>
    dplyr::summarise(
      mean_return = mean(.data$log_return, na.rm = TRUE),
      se_return   = stats::sd(.data$log_return, na.rm = TRUE) /
                      sqrt(dplyr::n()),
      n_obs       = dplyr::n(),
      .groups     = "drop"
    )
}

#' Compute volatility surface matrix
#'
#' For each series in `series_ids`, computes rolling annualized vol and
#' returns a long tibble suitable for a heatmap (date × series × vol).
#'
#' @param long_returns Tibble from `compute_log_returns()`.
#' @param series_ids Character vector of series to include.
#' @param window Integer. Rolling window in trading days. Default 21.
#' @return A tibble with columns: date, series, rolling_vol.
#' @export
compute_vol_surface <- function(long_returns, series_ids, window = 21) {
  long_returns |>
    dplyr::filter(.data$series %in% series_ids) |>
    dplyr::arrange(.data$series, .data$date) |>
    dplyr::group_by(.data$series) |>
    dplyr::mutate(
      rolling_vol = compute_rolling_vol(.data$log_return, window = window)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(.data$rolling_vol)) |>
    dplyr::select(date, series, rolling_vol)
}
