test_that("get_data_dir returns a valid path", {
  data_dir <- get_data_dir()
  expect_type(data_dir, "character")
  expect_true(grepl("\\.PSINetR$", data_dir))
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
  # Test get_data_dir with different HOME values
  orig_home <- Sys.getenv("HOME")
  Sys.setenv(HOME = "/tmp")
  expect_equal(get_data_dir(), "/tmp/.PSINetR")
  Sys.setenv(HOME = orig_home)
})

test_that("coverage functions validate inputs", {
  expect_error(generate_coverage_report(type = "invalid"), "should be one of")
})