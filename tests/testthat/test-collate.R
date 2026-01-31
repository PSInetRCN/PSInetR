# Test input validation for all collation functions

test_that("collate_met validates connection parameters", {
  expect_error(
    collate_met(con = NULL, db_path = NULL),
    "Either.*con.*or.*db_path.*must be provided"
  )
})

test_that("collate_chamber_wp validates connection parameters", {
  expect_error(
    collate_chamber_wp(con = NULL, db_path = NULL),
    "Either.*con.*or.*db_path.*must be provided"
  )
})

test_that("collate_auto_wp validates connection parameters", {
  expect_error(
    collate_auto_wp(con = NULL, db_path = NULL),
    "Either.*con.*or.*db_path.*must be provided"
  )
})

test_that("collate_soil validates connection parameters", {
  expect_error(
    collate_soil(con = NULL, db_path = NULL),
    "Either.*con.*or.*db_path.*must be provided"
  )
})

test_that("collate functions error with non-existent database path", {
  fake_path <- file.path(tempdir(), "nonexistent.duckdb")

  expect_error(
    collate_met(db_path = fake_path),
    "Database file not found"
  )

  expect_error(
    collate_chamber_wp(db_path = fake_path),
    "Database file not found"
  )

  expect_error(
    collate_auto_wp(db_path = fake_path),
    "Database file not found"
  )

  expect_error(
    collate_soil(db_path = fake_path),
    "Database file not found"
  )
})

# Test connection management
test_that("collate functions close connections they open", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  # Record open connections before
  before_count <- length(DBI::dbListConnections(duckdb::duckdb()))

  # Run function with db_path (it should open and close)
  result <- collate_met(db_path = get_db_path())

  # Check connection count returned to baseline
  after_count <- length(DBI::dbListConnections(duckdb::duckdb()))
  expect_equal(before_count, after_count)
})

test_that("collate functions don't close provided connections", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  # Create a connection
  con <- DBI::dbConnect(duckdb::duckdb(), get_db_path())

  # Use the connection
  result <- collate_met(con = con)

  # Connection should still be valid
  expect_true(DBI::dbIsValid(con))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test return structures
test_that("collate_met returns a data frame", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  result <- collate_met(db_path = get_db_path())

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("dataset_name" %in% colnames(result))
  expect_true("timezone" %in% colnames(result))
})

test_that("collate_chamber_wp returns a data frame with SFN column", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  result <- collate_chamber_wp(db_path = get_db_path())

  expect_s3_class(result, "data.frame")
  expect_true("dataset_name" %in% colnames(result))
  expect_true("SFN" %in% colnames(result))
  expect_true("timezone" %in% colnames(result))
  expect_type(result$SFN, "logical")
})

test_that("collate_auto_wp returns a data frame with SFN column", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  result <- collate_auto_wp(db_path = get_db_path())

  expect_s3_class(result, "data.frame")
  expect_true("dataset_name" %in% colnames(result))
  expect_true("SFN" %in% colnames(result))
  expect_true("sensor_id" %in% colnames(result))
  expect_true("timezone" %in% colnames(result))
  expect_type(result$SFN, "logical")
})

test_that("collate_soil returns a named list with three data frames", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  result <- collate_soil(db_path = get_db_path())

  expect_type(result, "list")
  expect_named(result, c("individual", "plot", "study"))

  expect_s3_class(result$individual, "data.frame")
  expect_s3_class(result$plot, "data.frame")
  expect_s3_class(result$study, "data.frame")

  # All should have dataset_name and timezone columns
  expect_true("dataset_name" %in% colnames(result$individual))
  expect_true("dataset_name" %in% colnames(result$plot))
  expect_true("dataset_name" %in% colnames(result$study))

  expect_true("timezone" %in% colnames(result$individual))
  expect_true("timezone" %in% colnames(result$plot))
  expect_true("timezone" %in% colnames(result$study))
})

# Test dataset filtering
test_that("dataset_name parameter filters collate_met results", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  # Get full results
  full_result <- collate_met(db_path = get_db_path())

  # Get filtered results (pick first dataset)
  if (nrow(full_result) > 0) {
    test_dataset <- full_result$dataset_name[1]
    filtered_result <- collate_met(
      db_path = get_db_path(),
      dataset_name = test_dataset
    )

    # Filtered result should be subset
    expect_true(nrow(filtered_result) <= nrow(full_result))
    expect_true(all(filtered_result$dataset_name == test_dataset))
  }
})

test_that("dataset_name parameter filters collate_chamber_wp results", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  # Get full results
  full_result <- collate_chamber_wp(db_path = get_db_path())

  # Get filtered results (pick first dataset if available)
  if (nrow(full_result) > 0) {
    test_dataset <- full_result$dataset_name[1]
    filtered_result <- collate_chamber_wp(
      db_path = get_db_path(),
      dataset_name = test_dataset
    )

    # Filtered result should be subset
    expect_true(nrow(filtered_result) <= nrow(full_result))
    expect_true(all(filtered_result$dataset_name == test_dataset))
  }
})

test_that("dataset_name parameter filters collate_auto_wp results", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  # Get full results
  full_result <- collate_auto_wp(db_path = get_db_path())

  # Get filtered results (pick first dataset if available)
  if (nrow(full_result) > 0) {
    test_dataset <- full_result$dataset_name[1]
    filtered_result <- collate_auto_wp(
      db_path = get_db_path(),
      dataset_name = test_dataset
    )

    # Filtered result should be subset
    expect_true(nrow(filtered_result) <= nrow(full_result))
    expect_true(all(filtered_result$dataset_name == test_dataset))
  }
})

test_that("dataset_name parameter filters collate_soil results", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  # Get full results
  full_result <- collate_soil(db_path = get_db_path())

  # Test filtering on individual level if data exists
  if (nrow(full_result$individual) > 0) {
    test_dataset <- full_result$individual$dataset_name[1]
    filtered_result <- collate_soil(
      db_path = get_db_path(),
      dataset_name = test_dataset
    )

    # All levels should only contain the filtered dataset
    if (nrow(filtered_result$individual) > 0) {
      expect_true(all(filtered_result$individual$dataset_name == test_dataset))
    }
    if (nrow(filtered_result$plot) > 0) {
      expect_true(all(filtered_result$plot$dataset_name == test_dataset))
    }
    if (nrow(filtered_result$study) > 0) {
      expect_true(all(filtered_result$study$dataset_name == test_dataset))
    }
  }
})

# Test that auto_wp filters NA values
test_that("collate_auto_wp filters NA water potential values", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  result <- collate_auto_wp(db_path = get_db_path())

  if (nrow(result) > 0 && "water_potential_mean" %in% colnames(result)) {
    # All water_potential_mean values should be non-NA
    expect_false(any(is.na(result$water_potential_mean)))
  }
})

# Test that auto_wp filters plants without sensor_id
test_that("collate_auto_wp only includes plants with sensor_id", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  result <- collate_auto_wp(db_path = get_db_path())

  if (nrow(result) > 0 && "sensor_id" %in% colnames(result)) {
    # All sensor_id values should be non-NA
    expect_false(any(is.na(result$sensor_id)))
  }
})

# Test that soil filters rows without any soil values
test_that("collate_soil filters rows without soil measurements", {
  skip_if_not(file.exists(get_db_path()), message = "Database not found")

  result <- collate_soil(db_path = get_db_path())

  soil_cols <- c("swc_mean_shallow", "swc_mean_deep", "swp_mean_shallow", "swp_mean_deep")

  # For each level, at least one soil column should exist and have non-NA values
  for (level in c("individual", "plot", "study")) {
    if (nrow(result[[level]]) > 0) {
      available_cols <- intersect(soil_cols, colnames(result[[level]]))
      if (length(available_cols) > 0) {
        # At least one row should have at least one non-NA soil value
        has_value <- apply(result[[level]][, available_cols, drop = FALSE], 1, function(row) {
          any(!is.na(row))
        })
        expect_true(all(has_value))
      }
    }
  }
})
