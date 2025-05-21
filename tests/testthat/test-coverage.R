test_that("coverage functions have expected signatures", {
  # Test that the functions have the expected arguments
  expect_true(all(c("type", "output_dir") %in% names(formals(generate_coverage_report))))
  expect_equal(length(names(formals(get_coverage_percentage))), 0)
})