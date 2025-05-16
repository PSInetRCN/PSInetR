test_that("get_data_dir returns a valid path", {
  data_dir <- get_data_dir()
  expect_type(data_dir, "character")
  expect_true(grepl("\\.PSINetR$", data_dir))
})

test_that("check_psi_data returns logical", {
  # Mock the file check to avoid actual file system dependencies
  with_mock(
    `file.exists` = function(...) FALSE,
    `dir.exists` = function(...) FALSE,
    {
      result <- check_psi_data()
      expect_type(result, "logical")
      expect_false(result)
    }
  )
})

test_that("get_psi_data validates inputs", {
  expect_error(get_psi_data(source = "invalid"), "should be one of")
  expect_error(get_psi_data(format = "invalid"), "should be one of")
})