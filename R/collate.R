#' Collate Meteorological Data with Metadata
#'
#' Joins meteorological data with study site metadata including timezones.
#' Meteorological data is only reported at the study site level.
#'
#' @param con A DBI database connection object. If NULL, will create a connection using db_path.
#' @param db_path Character string specifying the path to the DuckDB database file.
#'   Only used if con is NULL. If both are NULL, an error is raised.
#' @param dataset_name Optional character vector of dataset names to filter results.
#'   If NULL (default), returns data for all available datasets.
#'
#' @return A data frame with meteorological measurements joined with study site metadata.
#' @export
#'
#' @examples
#' \dontrun{
#' # Using database path
#' met_data <- collate_met(db_path = "psinet.duckdb")
#'
#' # Using existing connection
#' con <- DBI::dbConnect(duckdb::duckdb(), "psinet.duckdb")
#' met_data <- collate_met(con = con)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#'
#' # Filter to specific datasets
#' met_data <- collate_met(
#'   db_path = "psinet.duckdb",
#'   dataset_name = c("Smith_1", "Jones_2")
#' )
#' }
collate_met <- function(con = NULL, db_path = NULL, dataset_name = NULL) {
  # Validate input parameters
  if (is.null(con) && is.null(db_path)) {
    cli::cli_abort(
      "Either {.arg con} or {.arg db_path} must be provided."
    )
  }

  # Manage database connection
  owns_connection <- FALSE
  if (is.null(con)) {
    if (!file.exists(db_path)) {
      cli::cli_abort(
        "Database file not found at {.file {db_path}}."
      )
    }
    con <- DBI::dbConnect(duckdb::duckdb(), db_path)
    owns_connection <- TRUE
  }

  # Ensure cleanup on error
  on.exit({
    if (owns_connection && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  })

  # Compute timezone from the small study_site table in R, then copy back
  # as a temporary table so the join can happen inside DuckDB before collect()
  site_tz_local <- dplyr::tbl(con, "study_site") |>
    dplyr::select("dataset_name", "latitude_wgs84", "longitude_wgs84") |>
    dplyr::collect() |>
    dplyr::mutate(timezone = suppressWarnings(lutz::tz_lookup_coords(
      lat = .data$latitude_wgs84,
      lon = .data$longitude_wgs84
    ))) |>
    dplyr::select("dataset_name", "timezone")

  # Register the small timezone lookup as a temporary DuckDB table
  DBI::dbWriteTable(con, "site_tz_tmp", site_tz_local, overwrite = TRUE, temporary = TRUE)

  # Build lazy references
  site    <- dplyr::tbl(con, "study_site")
  met_var <- dplyr::tbl(con, "met_var")
  site_tz <- dplyr::tbl(con, "site_tz_tmp")

  # Columns in study_site that overlap with meta (excluding join key)
  site_cols <- dplyr::tbl(con, "study_site") |> head(0) |> dplyr::collect() |> colnames()
  meta_cols <- dplyr::tbl(con, "meta")      |> head(0) |> dplyr::collect() |> colnames()
  meta_drop <- setdiff(intersect(site_cols, meta_cols), "dataset_name")

  meta <- dplyr::tbl(con, "meta") |>
    dplyr::select(!dplyr::any_of(meta_drop))

  # Apply dataset filter inside the DB if provided
  if (!is.null(dataset_name)) {
    site    <- site    |> dplyr::filter(.data$dataset_name %in% .env$dataset_name)
    met_var <- met_var |> dplyr::filter(.data$dataset_name %in% .env$dataset_name)
    meta    <- meta    |> dplyr::filter(.data$dataset_name %in% .env$dataset_name)
    site_tz <- site_tz |> dplyr::filter(.data$dataset_name %in% .env$dataset_name)
  }

  # Perform all joins inside DuckDB (including timezone), then collect once
  all_met <- site |>
    dplyr::inner_join(met_var, by = dplyr::join_by(dataset_name)) |>
    dplyr::left_join(meta,    by = dplyr::join_by(dataset_name)) |>
    dplyr::left_join(site_tz, by = dplyr::join_by(dataset_name)) |>
    dplyr::distinct() |>
    dplyr::collect()

  return(all_met)
}


#' Collate Pressure Chamber Water Potential Data with Metadata
#'
#' Joins pressure chamber water potential measurements with study site, treatment,
#' plot, and plant metadata. Includes treatment hierarchy logic to properly assign
#' treatments at different organizational levels. Optionally flags datasets that
#' are part of the SAPFLUXNET network.
#'
#' @param con A DBI database connection object. If NULL, will create a connection using db_path.
#' @param db_path Character string specifying the path to the DuckDB database file.
#'   Only used if con is NULL. If both are NULL, an error is raised.
#' @param dataset_name Optional character vector of dataset names to filter results.
#'   If NULL (default), returns data for all available datasets with pressure chamber data.
#'
#' @return A data frame with pressure chamber measurements joined with complete metadata.
#'   Includes a logical SFN column indicating SAPFLUXNET membership.
#' @export
#'
#' @examples
#' \dontrun{
#' # Using database path
#' chamber_data <- collate_chamber_wp(db_path = "psinet.duckdb")
#'
#' # Using existing connection
#' con <- DBI::dbConnect(duckdb::duckdb(), "psinet.duckdb")
#' chamber_data <- collate_chamber_wp(con = con)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#'
#' # Filter to specific datasets
#' chamber_data <- collate_chamber_wp(
#'   db_path = "psinet.duckdb",
#'   dataset_name = c("Smith_1", "Jones_2")
#' )
#' }
collate_chamber_wp <- function(con = NULL, db_path = NULL, dataset_name = NULL) {
  # Validate input parameters
  if (is.null(con) && is.null(db_path)) {
    cli::cli_abort(
      "Either {.arg con} or {.arg db_path} must be provided."
    )
  }

  # Manage database connection
  owns_connection <- FALSE
  if (is.null(con)) {
    if (!file.exists(db_path)) {
      cli::cli_abort(
        "Database file not found at {.file {db_path}}."
      )
    }
    con <- DBI::dbConnect(duckdb::duckdb(), db_path)
    owns_connection <- TRUE
  }

  # Ensure cleanup on error
  on.exit({
    if (owns_connection && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  })

  # Identify chamber datasets from data_description table
  chamb_datasets <- dplyr::tbl(con, "data_description") |>
    dplyr::filter(
      .data$data_variable == "Plant water potential - pressure chamber" &
        .data$is_it_available == TRUE
    ) |>
    dplyr::collect() |>
    dplyr::pull(.data$dataset_name)

  # Apply user filter if provided
  if (!is.null(dataset_name)) {
    chamb_datasets <- intersect(chamb_datasets, dataset_name)
    if (length(chamb_datasets) == 0) {
      cli::cli_warn(
        "No pressure chamber datasets found matching the provided dataset_name filter."
      )
    }
  }

  # Load tables filtered to chamber datasets
  site <- dplyr::tbl(con, "study_site") |>
    dplyr::filter(.data$dataset_name %in% chamb_datasets) |>
    dplyr::collect() |>
    dplyr::mutate(timezone = suppressWarnings(lutz::tz_lookup_coords(
      lat = .data$latitude_wgs84,
      lon = .data$longitude_wgs84
    )))

  trt <- dplyr::tbl(con, "treatment") |>
    dplyr::filter(.data$dataset_name %in% chamb_datasets) |>
    dplyr::collect()

  plt <- dplyr::tbl(con, "plot") |>
    dplyr::filter(.data$dataset_name %in% chamb_datasets) |>
    dplyr::collect()

  plant <- dplyr::tbl(con, "plant") |>
    dplyr::filter(.data$dataset_name %in% chamb_datasets) |>
    dplyr::collect()

  chamber_wp <- dplyr::tbl(con, "chamber_wp") |>
    dplyr::filter(.data$dataset_name %in% chamb_datasets) |>
    dplyr::collect()

  sfn <- dplyr::tbl(con, "sapfluxnet") |>
    dplyr::filter(.data$dataset_name %in% chamb_datasets) |>
    dplyr::collect()

  # Drop columns from meta that already exist in study_site
  site_cols <- colnames(site)
  meta <- dplyr::tbl(con, "meta") |>
    dplyr::filter(.data$dataset_name %in% chamb_datasets) |>
    dplyr::collect() |>
    dplyr::select(!dplyr::any_of(setdiff(site_cols, "dataset_name")))

  # Determine SAPFLUXNET datasets
  sfn_datasets <- sfn |>
    dplyr::filter(!is.na(.data$pl_name)) |>
    dplyr::count(.data$dataset_name) |>
    dplyr::pull(.data$dataset_name)

  # Apply treatment level logic and join all metadata
  all_chamber <- trt |>
    dplyr::mutate(
      plot_treatment_id = dplyr::case_when(
        .data$level_of_treatment == "Individual" ~ "No treatment",
        .data$level_of_treatment == "Stand/plot/transect" ~ .data$treatment_id,
        .data$level_of_treatment == "Whole study" ~ .data$treatment_id
      ),
      individual_treatment_id = dplyr::case_when(
        .data$level_of_treatment == "Individual" ~ .data$treatment_id,
        .data$level_of_treatment == "Stand/plot/transect" ~ "No treatment",
        .data$level_of_treatment == "Whole study" ~ "No treatment"
      )
    ) |>
    dplyr::full_join(
      plt,
      by = dplyr::join_by(dataset_name, plot_treatment_id),
      relationship = "many-to-many"
    ) |>
    dplyr::full_join(
      plant,
      by = dplyr::join_by(
        dataset_name,
        plot_treatment_id,
        individual_treatment_id,
        plot_id
      )
    ) |>
    dplyr::inner_join(
      chamber_wp,
      by = dplyr::join_by(dataset_name, plot_id, individual_id)
    )

  # Add site characteristics and SFN flag
  all_chamber_joined <- site |>
    dplyr::mutate(
      SFN = dplyr::case_when(
        .data$dataset_name %in% sfn_datasets ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::inner_join(all_chamber, by = dplyr::join_by(dataset_name)) |>
    dplyr::left_join(meta, by = dplyr::join_by(dataset_name))

  return(all_chamber_joined)
}


#' Collate Automated Water Potential Data with Metadata
#'
#' Joins automated water potential measurements with study site, treatment, plot,
#' plant, and sensor metadata. This is the most complex collation function as it
#' handles sensor deployment periods and filters measurements to valid date ranges.
#' Includes treatment hierarchy logic and SAPFLUXNET flagging.
#'
#' Plants without sensor_id assignments are excluded. Measurements with NA water
#' potential values are filtered out.
#'
#' @param con A DBI database connection object. If NULL, will create a connection using db_path.
#' @param db_path Character string specifying the path to the DuckDB database file.
#'   Only used if con is NULL. If both are NULL, an error is raised.
#' @param dataset_name Optional character vector of dataset names to filter results.
#'   If NULL (default), returns data for all available datasets with automated water potential data.
#'
#' @return A data frame with automated water potential measurements joined with complete
#'   metadata. Measurements are filtered to sensor deployment periods (between start_date
#'   and end_date). Includes a logical SFN column indicating SAPFLUXNET membership.
#' @export
#'
#' @examples
#' \dontrun{
#' # Using database path
#' auto_data <- collate_auto_wp(db_path = "psinet.duckdb")
#'
#' # Using existing connection
#' con <- DBI::dbConnect(duckdb::duckdb(), "psinet.duckdb")
#' auto_data <- collate_auto_wp(con = con)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#'
#' # Filter to specific datasets
#' auto_data <- collate_auto_wp(
#'   db_path = "psinet.duckdb",
#'   dataset_name = c("Smith_1", "Jones_2")
#' )
#' }
collate_auto_wp <- function(con = NULL, db_path = NULL, dataset_name = NULL) {
  # Validate input parameters
  if (is.null(con) && is.null(db_path)) {
    cli::cli_abort(
      "Either {.arg con} or {.arg db_path} must be provided."
    )
  }

  # Manage database connection
  owns_connection <- FALSE
  if (is.null(con)) {
    if (!file.exists(db_path)) {
      cli::cli_abort(
        "Database file not found at {.file {db_path}}."
      )
    }
    con <- DBI::dbConnect(duckdb::duckdb(), db_path)
    owns_connection <- TRUE
  }

  # Ensure cleanup on error
  on.exit({
    if (owns_connection && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  })

  # Identify automated datasets from data_description table
  auto_datasets <- dplyr::tbl(con, "data_description") |>
    dplyr::filter(
      .data$data_variable == "Plant water potential - automated" &
        .data$is_it_available == TRUE
    ) |>
    dplyr::collect() |>
    dplyr::pull(.data$dataset_name)

  # Apply user filter if provided
  if (!is.null(dataset_name)) {
    auto_datasets <- intersect(auto_datasets, dataset_name)
    if (length(auto_datasets) == 0) {
      cli::cli_warn(
        "No automated water potential datasets found matching the provided dataset_name filter."
      )
    }
  }

  # Load tables filtered to automated datasets
  site <- dplyr::tbl(con, "study_site") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect() |>
    dplyr::mutate(timezone = suppressWarnings(lutz::tz_lookup_coords(
      lat = .data$latitude_wgs84,
      lon = .data$longitude_wgs84
    )))

  trt <- dplyr::tbl(con, "treatment") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect()

  plt <- dplyr::tbl(con, "plot") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect()

  plant <- dplyr::tbl(con, "plant") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect()

  auto_sens <- dplyr::tbl(con, "auto_wp_sensor") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect()

  auto_wp <- dplyr::tbl(con, "auto_wp") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect()

  sfn <- dplyr::tbl(con, "sapfluxnet") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect()

  # Drop columns from meta that already exist in study_site
  site_cols <- colnames(site)
  meta <- dplyr::tbl(con, "meta") |>
    dplyr::filter(.data$dataset_name %in% auto_datasets) |>
    dplyr::collect() |>
    dplyr::select(!dplyr::any_of(setdiff(site_cols, "dataset_name")))

  # Determine SAPFLUXNET datasets
  sfn_datasets <- sfn |>
    dplyr::filter(!is.na(.data$pl_name) | !is.na(.data$pl_code)) |>
    dplyr::count(.data$dataset_name) |>
    dplyr::pull(.data$dataset_name)

  # Apply treatment level logic and join metadata with sensors
  all_meta <- trt |>
    dplyr::mutate(
      plot_treatment_id = dplyr::case_when(
        .data$level_of_treatment == "Individual" ~ "No treatment",
        .data$level_of_treatment == "Stand/plot/transect" ~ .data$treatment_id,
        .data$level_of_treatment == "Whole study" ~ .data$treatment_id
      ),
      individual_treatment_id = dplyr::case_when(
        .data$level_of_treatment == "Individual" ~ .data$treatment_id,
        .data$level_of_treatment == "Stand/plot/transect" ~ "No treatment",
        .data$level_of_treatment == "Whole study" ~ "No treatment"
      )
    ) |>
    dplyr::full_join(
      plt,
      by = dplyr::join_by(dataset_name, plot_treatment_id),
      relationship = "many-to-many"
    ) |>
    dplyr::full_join(
      plant,
      by = dplyr::join_by(
        dataset_name,
        plot_treatment_id,
        individual_treatment_id,
        plot_id
      )
    ) |>
    dplyr::full_join(
      auto_sens,
      by = dplyr::join_by(dataset_name, individual_id)
    ) |>
    dplyr::filter(!is.na(.data$sensor_id)) |>
    dplyr::full_join(
      site |> dplyr::select("dataset_name", "timezone"),
      by = dplyr::join_by(dataset_name)
    ) |>
    dplyr::mutate(
      start_date = as.Date(.data$start_date, tz = .data$timezone),
      end_date = as.Date(.data$end_date, tz = .data$timezone)
    ) |>
    dplyr::select(!dplyr::all_of("timezone"))

  # Create intermediate table with auto_wp measurements and site metadata
  int_table <- site |>
    dplyr::mutate(
      SFN = dplyr::case_when(
        .data$dataset_name %in% sfn_datasets ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::inner_join(auto_wp, by = dplyr::join_by(dataset_name)) |>
    dplyr::mutate(date = as.Date(.data$date, tz = .data$timezone))

  # Join with metadata using sensor deployment dates
  # Using bounds = "[)" is conservative for dates
  all_auto <- int_table |>
    dplyr::inner_join(
      all_meta,
      by = dplyr::join_by(
        dataset_name,
        individual_id,
        plot_id,
        sensor_id,
        dplyr::between(date, start_date, end_date, bounds = "[)")
      )
    ) |>
    dplyr::filter(!is.na(.data$water_potential_mean)) |>
    dplyr::left_join(meta, by = dplyr::join_by(dataset_name))

  return(all_auto)
}


#' Collate Soil Data with Metadata at Multiple Organizational Levels
#'
#' Joins soil moisture and soil water potential data with appropriate metadata at
#' three organizational levels: individual plant, plot, and whole study. Returns
#' a named list with separate data frames for each level since soil data can be
#' reported at different hierarchical levels with different metadata structures.
#'
#' Rows with all NA values for soil variables (swc_mean_shallow, swc_mean_deep,
#' swp_mean_shallow, swp_mean_deep) are filtered out.
#'
#' @param con A DBI database connection object. If NULL, will create a connection using db_path.
#' @param db_path Character string specifying the path to the DuckDB database file.
#'   Only used if con is NULL. If both are NULL, an error is raised.
#' @param dataset_name Optional character vector of dataset names to filter results.
#'   If NULL (default), returns data for all available datasets with soil data.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{individual}{Data frame with individual-level soil data joined with
#'       site, treatment, plot, and plant metadata}
#'     \item{plot}{Data frame with plot-level soil data joined with site,
#'       treatment, and plot metadata}
#'     \item{study}{Data frame with study-level soil data joined with site metadata}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Using database path
#' soil_data <- collate_soil(db_path = "psinet.duckdb")
#'
#' # Access different levels
#' individual_soil <- soil_data$individual
#' plot_soil <- soil_data$plot
#' study_soil <- soil_data$study
#'
#' # Using existing connection
#' con <- DBI::dbConnect(duckdb::duckdb(), "psinet.duckdb")
#' soil_data <- collate_soil(con = con)
#' DBI::dbDisconnect(con, shutdown = TRUE)
#'
#' # Filter to specific datasets
#' soil_data <- collate_soil(
#'   db_path = "psinet.duckdb",
#'   dataset_name = c("Smith_1", "Jones_2")
#' )
#' }
collate_soil <- function(con = NULL, db_path = NULL, dataset_name = NULL) {
  # Validate input parameters
  if (is.null(con) && is.null(db_path)) {
    cli::cli_abort(
      "Either {.arg con} or {.arg db_path} must be provided."
    )
  }

  # Manage database connection
  owns_connection <- FALSE
  if (is.null(con)) {
    if (!file.exists(db_path)) {
      cli::cli_abort(
        "Database file not found at {.file {db_path}}."
      )
    }
    con <- DBI::dbConnect(duckdb::duckdb(), db_path)
    owns_connection <- TRUE
  }

  # Ensure cleanup on error
  on.exit({
    if (owns_connection && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  })

  # Load all necessary tables
  site <- dplyr::tbl(con, "study_site") |>
    dplyr::collect() |>
    dplyr::mutate(timezone = suppressWarnings(lutz::tz_lookup_coords(
      lat = .data$latitude_wgs84,
      lon = .data$longitude_wgs84
    )))

  # Apply dataset filter to site first if provided
  if (!is.null(dataset_name)) {
    site <- site |> dplyr::filter(.data$dataset_name %in% .env$dataset_name)
  }

  # Filter all remaining tables to only datasets present in site
  site_datasets <- unique(site$dataset_name)

  trt <- dplyr::tbl(con, "treatment") |>
    dplyr::filter(.data$dataset_name %in% site_datasets) |>
    dplyr::collect()

  plt <- dplyr::tbl(con, "plot") |>
    dplyr::filter(.data$dataset_name %in% site_datasets) |>
    dplyr::collect()

  plant <- dplyr::tbl(con, "plant") |>
    dplyr::filter(.data$dataset_name %in% site_datasets) |>
    dplyr::collect()

  soil_var <- dplyr::tbl(con, "soil_var") |>
    dplyr::filter(.data$dataset_name %in% site_datasets) |>
    dplyr::collect()

  # Drop columns from meta that already exist in study_site
  site_cols <- colnames(site)
  meta <- dplyr::tbl(con, "meta") |>
    dplyr::filter(.data$dataset_name %in% site_datasets) |>
    dplyr::select(!dplyr::any_of(setdiff(site_cols, "dataset_name"))) |>
    dplyr::collect()

  # Load data_description to get sensor_location for soil variables
  data_desc <- dplyr::tbl(con, "data_description") |>
    dplyr::filter(
      .data$dataset_name %in% site_datasets &
        .data$data_variable %in% c("Soil water content", "Soil water potential")
    ) |>
    dplyr::select("dataset_name", "sensor_location") |>
    dplyr::distinct() |>
    dplyr::collect()

  # Join sensor_location onto soil_var to categorize datasets
  soil_var <- soil_var |>
    dplyr::left_join(data_desc, by = dplyr::join_by(dataset_name)) |>
    dplyr::filter(dplyr::if_any(
      dplyr::any_of(c("swc_mean_shallow", "swc_mean_deep", "swp_mean_shallow", "swp_mean_deep")),
      ~ !is.na(.x)
    ))

  # --- Individual Level ---
  # Metadata for individual level (site/trt/plt/plant)
  meta_4_6 <- trt |>
    dplyr::full_join(
      plt,
      by = dplyr::join_by(dataset_name, plot_treatment_id),
      relationship = "many-to-many"
    ) |>
    dplyr::full_join(
      plant,
      by = dplyr::join_by(
        dataset_name,
        plot_treatment_id,
        individual_treatment_id,
        plot_id
      )
    ) |>
    dplyr::full_join(site, by = dplyr::join_by(dataset_name))

  # Soil data for individuals (sensor_location == "Individual")
  soil_ind <- soil_var |>
    dplyr::filter(.data$sensor_location == "Individual")

  all_soil_ind <- meta_4_6 |>
    dplyr::inner_join(
      soil_ind,
      by = dplyr::join_by(dataset_name, plot_id, individual_id)
    ) |>
    dplyr::left_join(meta, by = dplyr::join_by(dataset_name))

  # --- Plot Level ---
  # Metadata for plot level (site/trt/plt)
  meta_4_5 <- trt |>
    dplyr::full_join(
      plt,
      by = dplyr::join_by(dataset_name, plot_treatment_id),
      relationship = "many-to-many"
    ) |>
    dplyr::full_join(site, by = dplyr::join_by(dataset_name))

  # Soil data for plots (sensor_location == "Plot")
  soil_plt <- soil_var |>
    dplyr::filter(.data$sensor_location == "Plot")

  all_soil_plt <- meta_4_5 |>
    dplyr::inner_join(soil_plt, by = dplyr::join_by(dataset_name, plot_id)) |>
    dplyr::left_join(meta, by = dplyr::join_by(dataset_name))

  # --- Whole Study Level ---
  # Soil data for whole study (sensor_location == "Whole study")
  soil_study <- soil_var |>
    dplyr::filter(.data$sensor_location == "Whole study")

  all_soil_study <- site |>
    dplyr::inner_join(soil_study, by = dplyr::join_by(dataset_name)) |>
    dplyr::left_join(meta, by = dplyr::join_by(dataset_name))

  # Return named list
  return(list(
    individual = all_soil_ind,
    plot = all_soil_plt,
    study = all_soil_study
  ))
}
