# Tests for simulation module math (GBM + OU)
# These test the pure computation logic, not the Shiny reactive layer.

# ── GBM simulation ───────────────────────────────────────────────────────────

test_that("GBM produces correct matrix dimensions", {
  set.seed(1)
  s0     <- 75
  n_days <- 126
  n_sim  <- 50
  mu_daily    <- 0.0002
  sigma_daily <- 0.015
  drift <- mu_daily - 0.5 * sigma_daily^2

  sim_mat <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sim)
  sim_mat[1, ] <- s0
  for (t in seq_len(n_days)) {
    z <- stats::rnorm(n_sim)
    sim_mat[t + 1, ] <- sim_mat[t, ] * exp(drift + sigma_daily * z)
  }

  expect_equal(nrow(sim_mat), n_days + 1)
  expect_equal(ncol(sim_mat), n_sim)
  expect_true(all(sim_mat[1, ] == s0))
})

test_that("GBM paths are always positive", {
  set.seed(42)
  s0 <- 50
  n_days <- 252
  n_sim  <- 200
  mu_daily    <- -0.001
  sigma_daily <- 0.03
  drift <- mu_daily - 0.5 * sigma_daily^2

  sim_mat <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sim)
  sim_mat[1, ] <- s0
  for (t in seq_len(n_days)) {
    z <- stats::rnorm(n_sim)
    sim_mat[t + 1, ] <- sim_mat[t, ] * exp(drift + sigma_daily * z)
  }

  expect_true(all(sim_mat > 0))
})

test_that("GBM mean path approximates expected drift", {
  set.seed(123)
  s0     <- 100
  n_days <- 252
  n_sim  <- 5000
  mu_daily    <- 0.0003
  sigma_daily <- 0.01
  drift <- mu_daily - 0.5 * sigma_daily^2

  sim_mat <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sim)
  sim_mat[1, ] <- s0
  for (t in seq_len(n_days)) {
    z <- stats::rnorm(n_sim)
    sim_mat[t + 1, ] <- sim_mat[t, ] * exp(drift + sigma_daily * z)
  }

  # Expected price = S0 * exp(mu * T)
  expected <- s0 * exp(mu_daily * n_days)
  actual   <- mean(sim_mat[n_days + 1, ])
  # Within 5% of expected (Monte Carlo variance)
  expect_true(abs(actual - expected) / expected < 0.05)
})

# ── OU simulation ────────────────────────────────────────────────────────────

test_that("OU AR(1) simulation has correct dimensions", {
  set.seed(1)
  s0     <- 75
  n_days <- 126
  n_sim  <- 50
  a      <- 0.005
  b      <- 0.998
  resid_sd <- 0.015

  log_mat <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sim)
  log_mat[1, ] <- log(s0)
  for (t in seq_len(n_days)) {
    z <- stats::rnorm(n_sim)
    log_mat[t + 1, ] <- a + b * log_mat[t, ] + resid_sd * z
  }
  sim_mat <- exp(log_mat)

  expect_equal(nrow(sim_mat), n_days + 1)
  expect_equal(ncol(sim_mat), n_sim)
  expect_true(all(sim_mat > 0))
})

test_that("OU mean reverts toward equilibrium", {
  set.seed(42)
  # Strong mean reversion: b = 0.95 (half-life ~14 days)
  a      <- 0.2     # implies mu_eq = a/(1-b) = 4.0 → exp(4) ≈ $54.6
  b      <- 0.95
  resid_sd <- 0.005
  mu_eq  <- a / (1 - b)

  # Start far above equilibrium
  s0     <- 200     # log(200) ≈ 5.3 >> 4.0
  n_days <- 500
  n_sim  <- 1000

  log_mat <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sim)
  log_mat[1, ] <- log(s0)
  for (t in seq_len(n_days)) {
    z <- stats::rnorm(n_sim)
    log_mat[t + 1, ] <- a + b * log_mat[t, ] + resid_sd * z
  }

  mean_final_log <- mean(log_mat[n_days + 1, ])
  # After 500 days with b=0.95, should be close to equilibrium (mu_eq=4.0)
  expect_true(abs(mean_final_log - mu_eq) < 0.1)
})

test_that("OU b clamping keeps b in (0, 1)", {
  # Simulate what the module does when b is out of range
  for (b_raw in c(-0.5, 0.0, 1.0, 1.5)) {
    b_clamped <- min(max(b_raw, 0.001), 0.999)
    expect_true(b_clamped > 0 && b_clamped < 1)
  }
})

# ── Edge cases ───────────────────────────────────────────────────────────────

test_that("min-variance hedge ratio handles zero vol", {
  # sigma_x = 0 should produce NA, not Inf
  rho     <- rep(0.8, 10)
  sigma_y <- rep(0.02, 10)
  sigma_x <- rep(0.0, 10)  # zero vol
  result  <- dplyr::if_else(sigma_x > 1e-10, rho * (sigma_y / sigma_x), NA_real_)
  expect_true(all(is.na(result)))
})

test_that("crack spread return handles zero denominator", {
  spread <- c(0.5, 0.0, -0.3, 0.2)
  lag_spread <- c(NA, 0.5, 0.0, -0.3)
  result <- dplyr::if_else(
    abs(lag_spread) > 1e-6,
    (spread - lag_spread) / abs(lag_spread),
    NA_real_
  )
  expect_true(is.na(result[1]))   # first is NA (no lag)
  expect_true(is.na(result[3]))   # lag was 0.0 → guarded
  expect_false(is.na(result[2]))  # normal case
  expect_false(is.na(result[4]))  # normal case
})

test_that("business day generation skips weekends", {
  start <- as.Date("2025-01-06")  # Monday
  n_days <- 10
  future_dates <- seq(start, by = "day", length.out = n_days * 2)
  future_dates <- future_dates[!weekdays(future_dates) %in% c("Saturday", "Sunday")]
  future_dates <- future_dates[seq_len(n_days + 1)]

  days <- weekdays(future_dates)
  expect_false("Saturday" %in% days)
  expect_false("Sunday" %in% days)
  expect_equal(length(future_dates), n_days + 1)
})
