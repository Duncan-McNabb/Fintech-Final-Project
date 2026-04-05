test_that("load_energy_data returns correct columns", {
  df <- load_energy_data("CL", as.Date("2020-01-01"), as.Date("2020-01-31"))
  expect_named(df, c("date", "series", "market", "contract_num", "value"),
               ignore.order = TRUE)
})

test_that("load_energy_data filters by market", {
  df <- load_energy_data("NG", as.Date("2020-01-01"), as.Date("2020-06-30"))
  expect_true(all(df$market == "NG"))
  expect_true(nrow(df) > 0)
})

test_that("load_energy_data filters by date range", {
  start <- as.Date("2020-03-01")
  end   <- as.Date("2020-03-31")
  df <- load_energy_data("CL", start, end)
  expect_true(all(df$date >= start))
  expect_true(all(df$date <= end))
})

test_that("load_cmt_data returns correct columns", {
  df <- load_cmt_data(start_date = as.Date("2020-01-01"),
                      end_date   = as.Date("2020-03-31"))
  expect_named(df, c("date", "series", "market", "contract_num", "value"),
               ignore.order = TRUE)
})

test_that("load_cmt_data market column is always CMT", {
  df <- load_cmt_data(start_date = as.Date("2020-01-01"),
                      end_date   = as.Date("2020-03-31"))
  expect_true(all(df$market == "CMT"))
})

test_that("load_cmt_data filters by date range", {
  start <- as.Date("2015-01-01")
  end   <- as.Date("2015-12-31")
  df <- load_cmt_data(start_date = start, end_date = end)
  expect_true(all(df$date >= start))
  expect_true(all(df$date <= end))
})

test_that("load_eia_data returns correct columns", {
  df <- load_eia_data(roles = "ng_storage")
  expect_named(df, c("role", "label", "date", "value"), ignore.order = TRUE)
})

test_that("load_eia_data filters by role", {
  df <- load_eia_data(roles = "ng_storage")
  expect_true(all(df$role == "ng_storage"))
})

test_that("load_eia_data NULL roles returns multiple roles", {
  df <- load_eia_data(roles = NULL)
  expect_true(length(unique(df$role)) > 1)
})

test_that("combine_data adds source column", {
  energy <- tibble::tibble(
    date = as.Date("2020-01-02"), series = "CL01",
    market = "CL", contract_num = 1L, value = 50.0
  )
  cmt <- tibble::tibble(
    date = as.Date("2020-01-02"), series = "DGS10",
    market = "CMT", contract_num = 120L, value = 1.5
  )
  result <- combine_data(energy, cmt)
  expect_true("source" %in% names(result))
  expect_equal(result$source[result$market == "CL"],  "energy")
  expect_equal(result$source[result$market == "CMT"], "cmt")
})

test_that("combine_data row count equals sum of inputs", {
  energy <- tibble::tibble(
    date = as.Date("2020-01-02"), series = "CL01",
    market = "CL", contract_num = 1L, value = 50.0
  )
  cmt <- tibble::tibble(
    date = as.Date("2020-01-02"), series = "DGS10",
    market = "CMT", contract_num = 120L, value = 1.5
  )
  result <- combine_data(energy, cmt)
  expect_equal(nrow(result), nrow(energy) + nrow(cmt))
})

test_that("pivot_wide produces date column and c-prefixed contract columns", {
  df <- tibble::tibble(
    date         = rep(as.Date(c("2020-01-02", "2020-01-03")), each = 2),
    series       = c("CL01", "CL02", "CL01", "CL02"),
    market       = "CL",
    contract_num = c(1L, 2L, 1L, 2L),
    value        = c(50.0, 49.5, 51.0, 50.5)
  )
  wide <- pivot_wide(df, "CL")
  expect_true("date" %in% names(wide))
  expect_true(all(grepl("^c", setdiff(names(wide), "date"))))
  expect_equal(nrow(wide), 2L)
})

test_that("compute_log_returns adds log_return column", {
  df <- tibble::tibble(
    date   = as.Date("2020-01-01") + 0:2,
    series = "CL01",
    value  = c(50, 55, 52.25)
  )
  result <- compute_log_returns(df)
  expect_true("log_return" %in% names(result))
})

test_that("compute_log_returns first obs per series is NA", {
  df <- tibble::tibble(
    date   = as.Date("2020-01-01") + 0:2,
    series = "CL01",
    value  = c(50, 55, 52.25)
  )
  result <- compute_log_returns(df)
  expect_true(is.na(result$log_return[1]))
})

test_that("compute_log_returns value is correct", {
  df <- tibble::tibble(
    date   = as.Date("2020-01-01") + 0:1,
    series = "CL01",
    value  = c(50, 55)
  )
  result <- compute_log_returns(df)
  expect_equal(result$log_return[2], log(55 / 50), tolerance = 1e-10)
})
