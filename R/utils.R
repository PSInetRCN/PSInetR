#' Get Default Data Directory
#'
#' Returns the default directory for storing downloaded data.
#' This directory is located within the user's home directory in a folder called '.PSINetR'.
#'
#' @return Character string with the path to the default data directory.
#' @keywords internal
get_data_dir <- function() {
  data_dir <- file.path(Sys.getenv("HOME"), ".PSINetR")
  return(data_dir)
}

#' Download Data from Repository
#'
#' Downloads plant water potential data from a repository.
#'
#' @param format Character string specifying the data format to download, either "duckdb" or "csv".
#' @param dest_dir Character string specifying the destination directory for downloaded files.
#' @param overwrite Logical indicating whether to overwrite existing files.
#' @param repo_url Character string specifying the repository URL.
#' @param github_token Character string containing a GitHub Personal Access Token (PAT).
#'
#' @return Character string with the path to the downloaded data.
#' @keywords internal
download_from_repo <- function(format, dest_dir, overwrite, repo_url, github_token) {
  # Set default repo URL if not provided
  if (is.null(repo_url)) {
    repo_url <- getOption("PSINetR.repo_url", "https://github.com/username/repo_name")
  }
  
  # Construct file paths based on format
  if (format == "duckdb") {
    file_path <- file.path(dest_dir, "psi_data.duckdb")
    download_url <- file.path(repo_url, "raw/main/data/psi_data.duckdb")
  } else {
    # For CSV format, we'll download a zip file and extract it
    file_path <- file.path(dest_dir, "psi_data_csv")
    zip_path <- file.path(dest_dir, "psi_data_csv.zip")
    download_url <- file.path(repo_url, "raw/main/data/psi_data_csv.zip")
  }
  
  # Check if files already exist
  if (file.exists(file_path) && !overwrite) {
    cli::cli_alert_info("Data files already exist. Use {.code overwrite = TRUE} to replace them")
    return(file_path)
  }
  
  # Download file
  cli::cli_alert_info("Downloading data from {.url {download_url}}")
  cli::cli_progress_step("Downloading", spinner = TRUE)
  
  # Configure HTTP request with authentication if needed
  headers <- list()
  if (!is.null(github_token) && github_token != "") {
    headers <- c(headers, Authorization = paste("token", github_token))
  }
  
  # Use httr to download the file
  response <- httr::GET(
    download_url,
    httr::write_disk(ifelse(format == "duckdb", file_path, zip_path), overwrite = TRUE),
    httr::add_headers(.headers = headers)
  )
  
  # Check for errors
  if (httr::http_error(response)) {
    cli::cli_progress_done(result = "failed")
    cli::cli_abort("Failed to download data: {httr::http_status(response)$message}")
  }
  
  # Extract if it's a zip file
  if (format == "csv") {
    cli::cli_progress_step("Extracting CSV files", spinner = TRUE)
    if (!dir.exists(file_path)) {
      dir.create(file_path, recursive = TRUE)
    }
    utils::unzip(zip_path, exdir = file_path)
    file.remove(zip_path)
  }
  
  cli::cli_progress_done(result = "done")
  cli::cli_alert_success("Data successfully downloaded to {.file {file_path}}")
  return(file_path)
}

#' Download Data from Zenodo
#'
#' Downloads plant water potential data from Zenodo.
#'
#' @param format Character string specifying the data format to download, either "duckdb" or "csv".
#' @param dest_dir Character string specifying the destination directory for downloaded files.
#' @param overwrite Logical indicating whether to overwrite existing files.
#' @param zenodo_doi Character string specifying the Zenodo DOI.
#'
#' @return Character string with the path to the downloaded data.
#' @keywords internal
download_from_zenodo <- function(format, dest_dir, overwrite, zenodo_doi) {
  # Set default Zenodo DOI if not provided
  if (is.null(zenodo_doi)) {
    zenodo_doi <- getOption("PSINetR.zenodo_doi", "10.5281/zenodo.XXXXXXX")
  }
  
  # Construct file paths based on format
  if (format == "duckdb") {
    file_path <- file.path(dest_dir, "psi_data.duckdb")
    # Convert DOI to download URL
    download_url <- paste0("https://zenodo.org/record/", zenodo_doi, "/files/psi_data.duckdb")
  } else {
    # For CSV format, we'll download a zip file and extract it
    file_path <- file.path(dest_dir, "psi_data_csv")
    zip_path <- file.path(dest_dir, "psi_data_csv.zip")
    # Convert DOI to download URL
    download_url <- paste0("https://zenodo.org/record/", zenodo_doi, "/files/psi_data_csv.zip")
  }
  
  # Check if files already exist
  if (file.exists(file_path) && !overwrite) {
    cli::cli_alert_info("Data files already exist. Use {.code overwrite = TRUE} to replace them")
    return(file_path)
  }
  
  # Download file
  cli::cli_alert_info("Downloading data from Zenodo: {.val {zenodo_doi}}")
  cli::cli_progress_step("Downloading", spinner = TRUE)
  
  # Use httr to download the file
  response <- httr::GET(
    download_url,
    httr::write_disk(ifelse(format == "duckdb", file_path, zip_path), overwrite = TRUE)
  )
  
  # Check for errors
  if (httr::http_error(response)) {
    cli::cli_progress_done(result = "failed")
    cli::cli_abort("Failed to download data: {httr::http_status(response)$message}")
  }
  
  # Extract if it's a zip file
  if (format == "csv") {
    cli::cli_progress_step("Extracting CSV files", spinner = TRUE)
    if (!dir.exists(file_path)) {
      dir.create(file_path, recursive = TRUE)
    }
    utils::unzip(zip_path, exdir = file_path)
    file.remove(zip_path)
  }
  
  cli::cli_progress_done(result = "done")
  cli::cli_alert_success("Data successfully downloaded to {.file {file_path}}")
  return(file_path)
}