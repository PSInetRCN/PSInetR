#' Test Download Function
#' 
#' A simplified version of get_psi_data that uses fixed parameters for testing.
#' This function attempts to download the duckdb file from the latest release.
#'
#' @param debug Logical indicating whether to print debugging information
#'
#' @return Invisibly returns the path to the downloaded data
#' @export
#'
#' @examples
#' \dontrun{
#' test_download()
#' test_download(debug = TRUE)
#' }
test_download <- function(debug = TRUE) {
  # Use temp directory for testing
  dest_dir <- tempdir()
  
  # Get GitHub token from environment
  github_token <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
  
  # Basic parameters
  repo_url <- "PSInetRCN/PSInetDB"
  format <- "duckdb"
  
  # Print debug info
  if (debug) {
    message("=== Test Download Settings ===")
    message("Repository: ", repo_url)
    message("Format: ", format)
    message("Destination: ", dest_dir)
    message("Token provided: ", github_token != "")
    if (github_token != "") {
      message("Token starts with: ", substr(github_token, 1, 4), "...")
    }
  }
  
  # Get the latest release tag
  if (debug) message("\n=== Fetching Latest Release Tag ===")
  release_tag <- tryCatch({
    tag <- get_latest_release_tag(repo_url, github_token, debug = debug)
    if (debug) message("Latest release tag: ", tag)
    tag
  }, error = function(e) {
    message("Error fetching latest release tag: ", e$message)
    # Return a default tag for testing
    if (debug) message("Using fallback tag: v0.1.0")
    return("v0.1.0")
  })
  
  # Try a few likely file names
  possible_file_names <- c("psinet.duckdb", "psi_data.duckdb", "PSInet.duckdb", "PSInetDB.duckdb")
  
  if (debug) {
    message("\n=== Trying multiple file names ===")
    message("Attempting to find the correct file name in the release")
  }
  
  # Start with the first file name as a default
  file_name <- possible_file_names[1]
  file_path <- file.path(dest_dir, file_name)
  
  # Try each file name
  success <- FALSE
  for (fname in possible_file_names) {
    test_url <- sprintf("https://github.com/%s/releases/download/%s/%s", 
                       repo_url, release_tag, fname)
    if (debug) message("Trying: ", test_url)
    
    # Configure headers for authentication
    test_headers <- c()
    if (github_token != "") {
      # Ensure token is properly formatted - GitHub expects exactly 'token XXXXX'
      # Using 'Bearer' prefix might not work correctly
      token_value <- paste("token", github_token)
      if (debug) message("Token format: 'token ' + [your token]")
      test_headers <- c(test_headers, "Authorization" = token_value)
    }
    
    # Make a HEAD request to check if the file exists without downloading it
    test_response <- httr::HEAD(test_url, httr::add_headers(.headers = test_headers))
    
    if (debug) {
      message("Status: ", httr::status_code(test_response))
      if (httr::http_error(test_response)) {
        message("Error: ", httr::http_status(test_response)$message)
      }
    }
    
    if (!httr::http_error(test_response)) {
      file_name <- fname
      file_path <- file.path(dest_dir, file_name)
      download_url <- test_url
      success <- TRUE
      if (debug) message("Found valid file: ", file_name)
      break
    }
  }
  
  if (!success) {
    if (debug) {
      message("Could not find a valid file. Using first name as fallback.")
    }
    file_name <- possible_file_names[1]
    file_path <- file.path(dest_dir, file_name)
    download_url <- sprintf("https://github.com/%s/releases/download/%s/%s", 
                           repo_url, release_tag, file_name)
  }
  
  if (debug) {
    message("\n=== Download Information ===")
    message("File name: ", file_name)
    message("Download URL: ", download_url)
    message("Destination: ", file_path)
  }
  
  # Configure HTTP headers
  headers <- c()
  if (github_token != "") {
    # GitHub API-style token format
    token_value <- paste("token", github_token)
    
    # Add proper authorization header
    headers <- c(headers, "Authorization" = token_value)
    
    # Add user agent as sometimes GitHub requires this
    headers <- c(headers, "User-Agent" = "PSInetR/0.1.0")
    
    if (debug) {
      message("Using authentication headers:")
      message("- Authorization: token [redacted]")
      message("- User-Agent: PSInetR/0.1.0")
    }
  }
  
  # Attempt download
  if (debug) message("\n=== Attempting Download ===")
  
  # First make a test request to check if we've identified the correct file
  if (debug) {
    message("First checking access to the file...")
    test_response <- httr::HEAD(
      download_url,
      httr::add_headers(.headers = headers)
    )
    
    message("HEAD request status: ", httr::status_code(test_response))
    if (httr::http_error(test_response)) {
      message("HEAD error: ", httr::http_status(test_response)$message)
      message("Check if your GitHub token has sufficient permissions")
      message("Token should have 'repo' scope for private repositories")
    } else {
      message("File confirmed accessible. Proceeding with download...")
    }
  }
  
  tryCatch({
    # Try direct download link format
    direct_download_url <- download_url
    
    # For GitHub releases, you can also try the raw assets API format
    if (grepl("github.com/.*/releases/download", download_url)) {
      # Extract repo path and release tag from the URL
      repo_parts <- strsplit(download_url, "/releases/download/")[[1]]
      repo_part <- gsub("https://github.com/", "", repo_parts[1])
      release_file_part <- repo_parts[2]
      
      # Split the release part into tag and filename
      release_file_parts <- strsplit(release_file_part, "/")[[1]]
      tag_part <- release_file_parts[1]
      file_part <- release_file_parts[2]
      
      # Construct a raw assets API URL that might work better
      api_url <- sprintf("https://api.github.com/repos/%s/releases/tags/%s", 
                     repo_part, tag_part)
      
      if (debug) message("Trying to get asset ID from API: ", api_url)
      
      # Get the release info to find the asset ID
      api_response <- httr::GET(
        api_url,
        httr::add_headers(.headers = headers)
      )
      
      if (!httr::http_error(api_response)) {
        release_info <- httr::content(api_response)
        
        # Find the asset that matches our filename
        asset_id <- NULL
        for (asset in release_info$assets) {
          if (asset$name == file_part) {
            asset_id <- asset$id
            if (debug) message("Found asset ID: ", asset_id)
            break
          }
        }
        
        if (!is.null(asset_id)) {
          # Use the GitHub API to download the asset directly
          direct_download_url <- sprintf("https://api.github.com/repos/%s/releases/assets/%s", 
                                     repo_part, asset_id)
          if (debug) message("Using API download URL: ", direct_download_url)
          
          # For the GitHub API, we need to set the Accept header to get the raw content
          headers <- c(headers, "Accept" = "application/octet-stream")
        }
      }
    }
    
    if (debug) message("Downloading from: ", direct_download_url)
    
    response <- httr::GET(
      direct_download_url,
      httr::write_disk(file_path, overwrite = TRUE),
      httr::add_headers(.headers = headers)
    )
    
    if (debug) {
      message("Response status: ", httr::status_code(response))
      if (httr::http_error(response)) {
        message("Error: ", httr::http_status(response)$message)
      } else {
        message("SUCCESS: File downloaded to ", file_path)
        message("File size: ", file.size(file_path), " bytes")
      }
    }
    
    # Return the path if successful
    if (!httr::http_error(response)) {
      return(invisible(file_path))
    } else {
      return(invisible(NULL))
    }
  }, error = function(e) {
    message("Download error: ", e$message)
    return(invisible(NULL))
  })
  
  # If we're still failing, provide a curl example
  if (debug && ((exists("response") && httr::http_error(response)) || !exists("response"))) {
    message("\n=== Curl Example (for manual download) ===")
    if (github_token != "") {
      # Create a safe version of the token for display (first few chars only)
      token_display <- paste0(substr(github_token, 1, 4), "...[REDACTED]")
      
      message("Since download via R failed, you can try a direct download using curl:")
      message("1. In your terminal, set your GitHub token:")
      message("   export GITHUB_TOKEN=your_github_token")
      message("2. Run this curl command to download:")
      message("   curl -H \"Authorization: token $GITHUB_TOKEN\" \\ ")
      message("        -L -o ", basename(file_path), " \\ ")
      message("        ", download_url)
      
      message("\nThis might work better for large files or with certain GitHub configurations.")
    }
  }
}