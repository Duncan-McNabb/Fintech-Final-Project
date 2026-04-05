test_that("compute_rolling_vol returns same length as input", {
  ret <- rnorm(50, 0, 0.02)
  vol <- compute_rolling_vol(ret, window = 10)
  expect_equal(length(vol), length(ret))
})

test_that("compute_rolling_vol first window-1 values are NA", {
  ret <- rnorm(50, 0, 0.02)
  vol <- compute_rolling_vol(ret, window = 10)
  expect_true(all(is.na(vol[1:9])))
  expect_false(is.na(vol[10]))
})

test_that("compute_rolling_vol non-NA values are non-negative", {
  ret <- rnorm(50, 0, 0.02)
  vol <- compute_rolling_vol(ret, window = 10)
  expect_true(all(vol[!is.na(vol)] >= 0))
})

test_that("compute_return_matrix returns a matrix", {
  lr <- tibble::tibble(
    date       = rep(as.Date("2020-01-01") + 1:30, times = 2),
    series     = rep(c("CL01", "NG01"), each = 30),
    log_return = rnorm(60, 0, 0.02)
  )
  mat <- compute_return_matrix(lr, c("CL01", "NG01"))
  expect_true(is.matrix(mat))
})

test_that("compute_return_matrix column names match requested series", {
  lr <- tibble::tibble(
    date       = rep(as.Date("2020-01-01") + 1:30, times = 2),
    series     = rep(c("CL01", "NG01"), each = 30),
    log_return = rnorm(60, 0, 0.02)
  )
  mat <- compute_return_matrix(lr, c("CL01", "NG01"))
  expect_setequal(colnames(mat), c("CL01", "NG01"))
})

test_that("compute_return_matrix has no NA rows", {
  lr <- tibble::tibble(
    date       = rep(as.Date("2020-01-01") + 1:30, times = 2),
    series     = rep(c("CL01", "NG01"), each = 30),
    log_return = rnorm(60, 0, 0.02)
  )
  mat <- compute_return_matrix(lr, c("CL01", "NG01"))
  expect_equal(sum(!stats::complete.cases(mat)), 0L)
})

test_that("compute_rolling_correlation returns same length as input", {
  x <- rnorm(80, 0, 0.02)
  y <- rnorm(80, 0, 0.02)
  corr <- compute_rolling_correlation(x, y, window = 20)
  expect_equal(length(corr), length(x))
})

test_that("compute_rolling_correlation first window-1 values are NA", {
  x <- rnorm(80, 0, 0.02)
  y <- rnorm(80, 0, 0.02)
  corr <- compute_rolling_correlation(x, y, window = 20)
  expect_true(all(is.na(corr[1:19])))
  expect_false(is.na(corr[20]))
})

test_that("compute_rolling_correlation of identical series equals 1", {
  x    <- rnorm(80, 0, 0.02)
  corr <- compute_rolling_correlation(x, x, window = 20)
  expect_true(all(abs(corr[!is.na(corr)] - 1) < 1e-10))
})

test_that("compute_rolling_beta returns same length as input", {
  x <- rnorm(80, 0, 0.02)
  y <- rnorm(80, 0, 0.02)
  beta <- compute_rolling_beta(y, x, window = 20)
  expect_equal(length(beta), length(y))
})

test_that("compute_rolling_beta first window-1 values are NA", {
  x <- rnorm(80, 0, 0.02)
  y <- rnorm(80, 0, 0.02)
  beta <- compute_rolling_beta(y, x, window = 20)
  expect_true(all(is.na(beta[1:19])))
  expect_false(is.na(beta[20]))
})

test_that("compute_rolling_beta of identical series is approximately 1", {
  x    <- rnorm(80, 0, 0.02)
  beta <- compute_rolling_beta(x, x, window = 20)
  expect_true(all(abs(beta[!is.na(beta)] - 1) < 1e-6))
})

test_that("compute_seasonal_index returns periods 1-12 for month freq", {
  # Two full years of daily data to ensure all months represented
  dates <- seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by = "day")
  lr <- tibble::tibble(
    date       = dates,
    series     = "CL01",
    log_return = rnorm(length(dates), 0, 0.02)
  )
  result <- compute_seasonal_index(lr, "CL01", freq = "month")
  expect_setequal(result$period, 1:12)
})

test_that("compute_seasonal_index has required columns", {
  dates <- seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by = "day")
  lr <- tibble::tibble(
    date       = dates,
    series     = "CL01",
    log_return = rnorm(length(dates), 0, 0.02)
  )
  result <- compute_seasonal_index(lr, "CL01", freq = "month")
  expect_named(result, c("period", "mean_return", "se_return", "n_obs"),
               ignore.order = TRUE)
})

test_that("compute_vol_surface returns correct columns", {
  dates <- as.Date("2020-01-01") + 1:50
  lr <- tibble::tibble(
    date       = rep(dates, times = 2),
    series     = rep(c("CL01", "CL02"), each = 50),
    log_return = rnorm(100, 0, 0.02)
  )
  result <- compute_vol_surface(lr, c("CL01", "CL02"), window = 10)
  expect_named(result, c("date", "series", "rolling_vol"), ignore.order = TRUE)
})

test_that("compute_vol_surface has no NA in rolling_vol", {
  dates <- as.Date("2020-01-01") + 1:50
  lr <- tibble::tibble(
    date       = rep(dates, times = 2),
    series     = rep(c("CL01", "CL02"), each = 50),
    log_return = rnorm(100, 0, 0.02)
  )
  result <- compute_vol_surface(lr, c("CL01", "CL02"), window = 10)
  expect_false(any(is.na(result$rolling_vol)))
})
