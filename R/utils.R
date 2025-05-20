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
#' @param verbose Logical indicating whether to print progress information.
#'
#' @return Character string with the latest release tag (e.g., "v0.1.0").
#' @keywords internal
get_latest_release_tag <- function(repo_url, github_token = NULL, verbose = TRUE) {
  # Extract owner/repo from URL if needed
  if (grepl("^https://github.com/", repo_url)) {
    repo_path <- sub("^https://github.com/", "", repo_url)
    repo_path <- sub("\\.git$", "", repo_path)
  } else {
    repo_path <- repo_url
  }
  
  # Construct API URL
  api_url <- paste0("https://api.github.com/repos/", repo_path, "/releases/latest")
  
  # Configure HTTP request with authentication if needed
  headers <- c(
    "Accept" = "application/vnd.github.v3+json",
    "X-GitHub-Api-Version" = "2022-11-28",
    "User-Agent" = "PSINetR/0.1.0"
  )
  
  if (!is.null(github_token) && github_token != "") {
    headers <- c(headers, "Authorization" = paste("token", github_token))
  }
  
  # Use httr to fetch the latest release info
  response <- httr::GET(
    api_url,
    httr::add_headers(.headers = headers)
  )
  
  # Check for errors
  if (httr::http_error(response)) {
    status_code <- httr::status_code(response)
    if (status_code == 404) {
      cli::cli_abort(paste(
        "Repository or release not found.",
        "Please check the repository name and that it has at least one release."
      ))
    } else if (status_code == 401 || status_code == 403) {
      cli::cli_abort(paste(
        "Authentication failed.",
        "Please check your GitHub token and ensure it has the 'repo' scope for private repositories."
      ))
    } else {
      cli::cli_abort("Failed to fetch latest release: {httr::http_status(response)$message}")
    }
  }
  
  # Parse response content
  release_data <- httr::content(response)
  
  # Extract tag name
  if (is.null(release_data$tag_name)) {
    cli::cli_abort("No release tag found for repository: {repo_path}")
  }
  
  # Show available assets in verbose mode
  if (verbose && !is.null(release_data$assets)) {
    cli::cli_h3("Available files in release {release_data$tag_name}:")
    for (asset in release_data$assets) {
      if (asset$size < 1048576) { # Less than 1MB
        size_str <- paste0(round(asset$size / 1024, 1), " KB")
      } else { # 1MB or larger
        size_str <- paste0(round(asset$size / 1048576, 1), " MB")
      }
      cli::cli_li("{asset$name} ({size_str})")
    }
    cli::cli_end()
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
#' @param direct_url Character string specifying a direct URL to the file to download. If provided, this overrides
#'   the repository and release tag settings and downloads directly from the specified URL.
#' @param file_name Character string specifying the file name to download from the release. By default, this is
#'   "psinet.duckdb" for DuckDB format and "psinet_csv.zip" for CSV format.
#' @param verbose Logical indicating whether to print progress information. Default is TRUE.
#'
#' @importFrom utils unzip
#' @return Character string with the path to the downloaded data.
#' @keywords internal
download_from_repo <- function(format, dest_dir, overwrite, repo_url, github_token, release_tag = NULL, direct_url = NULL, file_name = NULL, verbose = TRUE) {
  # Handle direct URL if provided
  if (!is.null(direct_url)) {
    if (verbose) {
      cli::cli_alert_info("Using direct URL: {.url {direct_url}}")
    }
    
    # Construct file paths based on format
    if (format == "duckdb") {
      file_name <- basename(direct_url)
      file_path <- file.path(dest_dir, file_name)
      download_url <- direct_url
    } else {
      # For CSV format, we'll download a zip file and extract it
      file_name <- basename(direct_url)
      file_path <- file.path(dest_dir, gsub("\\.zip$", "", file_name))
      zip_path <- file.path(dest_dir, file_name)
      download_url <- direct_url
    }
  } else {
    # Set default repo URL if not provided
    if (is.null(repo_url)) {
      repo_url <- getOption("PSINetR.repo_url", "PSInetRCN/PSInetDB")
    }
    
    # Get the latest release tag if not specified
    if (is.null(release_tag)) {
      cli::cli_progress_step("Fetching latest release information", spinner = TRUE)
      tryCatch({
        release_tag <- get_latest_release_tag(repo_url, github_token, verbose = verbose)
        cli::cli_alert_info("Using latest release: {.val {release_tag}}")
      }, error = function(e) {
        cli::cli_progress_done(result = "failed")
        cli::cli_alert_danger("Could not access repository releases.")
        cli::cli_alert_info("If the repository is private, make sure your token has the 'repo' scope.")
        cli::cli_alert_info("Alternatively, specify a release_tag directly if you know it exists.")
        cli::cli_abort(paste("Error fetching latest release:", e$message))
      })
    }
    
    # Extract owner/repo from URL if needed
    if (grepl("^https://github.com/", repo_url)) {
      repo_path <- sub("^https://github.com/", "", repo_url)
      repo_path <- sub("\\.git$", "", repo_path)
    } else {
      repo_path <- repo_url
    }
    
    # Construct file paths and download URLs based on format
    if (format == "duckdb") {
      # Use provided file name or default
      duckdb_file <- if(!is.null(file_name)) file_name else "psinet.duckdb"
      file_path <- file.path(dest_dir, duckdb_file)
      
      # For direct download we'll use the standard URL but will use the API for actual download
      download_url <- sprintf("https://github.com/%s/releases/download/%s/%s", 
                              repo_path, release_tag, duckdb_file)
    } else {
      # For CSV format, we'll download a zip file and extract it
      csv_file <- if(!is.null(file_name)) file_name else "psinet_csv.zip"
      file_path <- file.path(dest_dir, gsub("\\.zip$", "", csv_file))
      zip_path <- file.path(dest_dir, csv_file)
      
      # For direct download we'll use the standard URL but will use the API for actual download
      download_url <- sprintf("https://github.com/%s/releases/download/%s/%s", 
                              repo_path, release_tag, csv_file)
    }
  }
  
  # Check if files already exist
  if (file.exists(file_path) && !overwrite) {
    cli::cli_alert_info("Data files already exist. Use {.code overwrite = TRUE} to replace them")
    return(file_path)
  }
  
  # Download file using GitHub API - this is the more reliable method
  cli::cli_alert_info("Downloading data from {.url {download_url}}")
  cli::cli_progress_step("Downloading", spinner = TRUE)
  
  # Configure HTTP request with authentication if needed
  headers <- c(
    "User-Agent" = "PSINetR/0.1.0"
  )
  
  if (!is.null(github_token) && github_token != "") {
    headers <- c(headers, "Authorization" = paste("token", github_token))
  }
  
  # If we're downloading from GitHub releases, use the API approach which is more reliable
  if (grepl("github.com/.*/releases/download", download_url)) {
    # Get the repo path, tag and filename
    repo_parts <- strsplit(download_url, "/releases/download/")[[1]]
    repo_part <- gsub("https://github.com/", "", repo_parts[1])
    tag_file_part <- repo_parts[2]
    tag_file_parts <- strsplit(tag_file_part, "/")[[1]]
    tag <- tag_file_parts[1]
    file_name_part <- tag_file_parts[2]
    
    # Set up download path
    output_path <- ifelse(format == "duckdb", file_path, zip_path)
    
    # Use the GitHub API to get release information
    api_url <- paste0("https://api.github.com/repos/", repo_part, "/releases/tags/", tag)
    
    if (verbose) {
      cli::cli_alert_info("Accessing GitHub release API to download file")
    }
    
    api_response <- httr::GET(
      api_url,
      httr::add_headers(.headers = headers)
    )
    
    if (!httr::http_error(api_response)) {
      release_info <- httr::content(api_response)
      
      # Find the asset that matches our filename
      asset_id <- NULL
      asset_url <- NULL
      
      for (asset in release_info$assets) {
        if (asset$name == file_name_part) {
          asset_id <- asset$id
          asset_url <- asset$url
          asset_size <- asset$size
          
          if (verbose) {
            if (asset_size < 1048576) { # Less than 1MB
              size_str <- paste0(round(asset_size / 1024, 1), " KB")
            } else { # 1MB or larger
              size_str <- paste0(round(asset_size / 1048576, 1), " MB")
            }
            cli::cli_alert_info("Found matching file: {.val {asset$name}} ({size_str})")
          }
          break
        }
      }
      
      if (!is.null(asset_url)) {
        # Add the Accept header for getting raw content from the API
        api_headers <- c(headers, "Accept" = "application/octet-stream")
        
        # Download via the GitHub API
        response <- httr::GET(
          asset_url,
          httr::write_disk(output_path, overwrite = TRUE),
          httr::add_headers(.headers = api_headers)
        )
      } else {
        cli::cli_progress_done(result = "failed")
        cli::cli_abort(paste(
          "Could not find file:", file_name_part, "in release", tag,
          "\nMake sure the file name matches exactly what's in the release."
        ))
      }
    } else {
      cli::cli_progress_done(result = "failed")
      cli::cli_abort(paste(
        "Failed to access release:", tag,
        "\nCheck if the release exists and your token has sufficient permissions."
      ))
    }
  } else {
    # Not a GitHub release URL, just do a normal download
    response <- httr::GET(
      download_url,
      httr::write_disk(ifelse(format == "duckdb", file_path, zip_path), overwrite = TRUE),
      httr::add_headers(.headers = headers)
    )
  }
  
  # Check for errors
  if (httr::http_error(response)) {
    cli::cli_progress_done(result = "failed")
    status_code <- httr::status_code(response)
    
    if (status_code == 404) {
      cli::cli_abort(paste(
        "File not found in release.",
        "Make sure the file exists in the release and that the filename is correct."
      ))
    } else if (status_code == 401 || status_code == 403) {
      cli::cli_abort(paste(
        "Authentication failed when downloading.",
        "Make sure your GitHub token has the 'repo' scope for private repositories."
      ))
    } else {
      cli::cli_abort("Failed to download data: {httr::http_status(response)$message}")
    }
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