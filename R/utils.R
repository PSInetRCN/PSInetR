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

#' Get Latest GitHub Release Tag
#'
#' Fetches the latest release tag from a GitHub repository using the GitHub API.
#'
#' @param repo_url Character string specifying the repository URL or name (e.g., "PSInetRCN/PSInetDB").
#' @param github_token Character string containing a GitHub Personal Access Token (PAT).
#'   Can be NULL if the repository is public.
#'
#' @return Character string with the latest release tag (e.g., "v0.1.0").
#' @keywords internal
get_latest_release_tag <- function(repo_url, github_token = NULL) {
  # Extract owner/repo from URL if needed
  if (grepl("^https://github.com/", repo_url)) {
    repo_path <- sub("^https://github.com/", "", repo_url)
    repo_path <- sub("\.git$", "", repo_path)
  } else {
    repo_path <- repo_url
  }
  
  # Construct API URL
  api_url <- paste0("https://api.github.com/repos/", repo_path, "/releases/latest")
  
  # Configure HTTP request with authentication if needed
  headers <- list(
    Accept = "application/vnd.github.v3+json",
    "X-GitHub-Api-Version" = "2022-11-28"
  )
  
  if (!is.null(github_token) && github_token != "") {
    headers <- c(headers, Authorization = paste("token", github_token))
  }
  
  # Use httr to fetch the latest release info
  response <- httr::GET(
    api_url,
    httr::add_headers(.headers = headers)
  )
  
  # Check for errors
  if (httr::http_error(response)) {
    cli::cli_abort("Failed to fetch latest release: {httr::http_status(response)$message}")
  }
  
  # Parse response content
  release_data <- httr::content(response)
  
  # Extract tag name
  if (is.null(release_data$tag_name)) {
    cli::cli_abort("No release tag found for repository: {repo_path}")
  }
  
  return(release_data$tag_name)
}

#' Download Data from Repository
#'
#' Downloads plant water potential data from a repository's GitHub releases.
#' By default, it will fetch the data from the latest release.
#'
#' @param format Character string specifying the data format to download, either "duckdb" or "csv".
#' @param dest_dir Character string specifying the destination directory for downloaded files.
#' @param overwrite Logical indicating whether to overwrite existing files.
#' @param repo_url Character string specifying the repository URL or name (e.g., "PSInetRCN/PSInetDB").
#' @param github_token Character string containing a GitHub Personal Access Token (PAT).
#' @param release_tag Character string specifying a specific release tag to use (e.g., "v0.1.0").
#'   If NULL (default), the latest release will be used.
#'
#' @return Character string with the path to the downloaded data.
#' @keywords internal
download_from_repo <- function(format, dest_dir, overwrite, repo_url, github_token, release_tag = NULL) {
  # Set default repo URL if not provided
  if (is.null(repo_url)) {
    repo_url <- getOption("PSINetR.repo_url", "PSInetRCN/PSInetDB")
  }
  
  # Get the latest release tag if not specified
  if (is.null(release_tag)) {
    cli::cli_progress_step("Fetching latest release information", spinner = TRUE)
    release_tag <- get_latest_release_tag(repo_url, github_token)
    cli::cli_alert_info("Using latest release: {.val {release_tag}}")
  }
  
  # Extract owner/repo from URL if needed
  if (grepl("^https://github.com/", repo_url)) {
    repo_path <- sub("^https://github.com/", "", repo_url)
    repo_path <- sub("\.git$", "", repo_path)
  } else {
    repo_path <- repo_url
  }
  
  # Construct file paths and download URLs based on format
  if (format == "duckdb") {
    file_name <- "psinet.duckdb"
    file_path <- file.path(dest_dir, file_name)
    download_url <- sprintf("https://github.com/%s/releases/download/%s/%s", 
                           repo_path, release_tag, file_name)
  } else {
    # For CSV format, we'll download a zip file and extract it
    file_name <- "psinet_csv.zip"
    file_path <- file.path(dest_dir, "psinet_csv")
    zip_path <- file.path(dest_dir, file_name)
    download_url <- sprintf("https://github.com/%s/releases/download/%s/%s", 
                           repo_path, release_tag, file_name)
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