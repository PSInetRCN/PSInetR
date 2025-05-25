test_that("get_data_dir returns a valid path", {
  data_dir <- get_data_dir()
  expect_type(data_dir, "character")
  expect_true(dir.exists(data_dir))
  expect_equal(data_dir, getwd())
})

test_that("check_psi_data validates inputs", {
  expect_error(check_psi_data(format = "invalid"), "should be one of")
  expect_error(check_psi_data(format = c("duckdb", "csv")), NA)
})

test_that("get_psi_data validates inputs", {
  expect_error(get_psi_data(source = "invalid"), "should be one of")
  expect_error(get_psi_data(format = "invalid"), "should be one of")
})

test_that("utils functions handle edge cases", {
  # Test get_data_dir returns current working directory
  orig_wd <- getwd()
  temp_dir <- tempdir()
  setwd(temp_dir)
  expect_equal(normalizePath(get_data_dir()), normalizePath(temp_dir))
  setwd(orig_wd)
})

test_that("coverage functions validate inputs", {
  expect_error(generate_coverage_report(type = "invalid"), "should be one of")
})