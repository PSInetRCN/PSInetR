#' Download Plant Water Potential Data
#'
#' This function downloads plant water potential data from a specified source.
#' Users can choose between fetching data from a GitHub repository release or from Zenodo.
#' Downloaded data can be saved as either a DuckDB file or CSV files.
#' For accessing data in a private GitHub repository, authentication is required.
#'
#' When downloading from a GitHub repository, the function will automatically fetch
#' the data from the latest release unless a specific release tag is provided.
#'
#' @param source Character string specifying the data source, either "repo" (default) or "zenodo".
#' @param format Character string specifying the data format to download, either "duckdb" (default) or "csv".
#' @param dest_dir Character string specifying the destination directory for downloaded files.
#'   If NULL (default), files will be downloaded to a default location determined by \code{get_data_dir()}.
#' @param overwrite Logical indicating whether to overwrite existing files. Default is FALSE.
#' @param repo_url Character string specifying the repository URL or name (e.g., "PSInetRCN/PSInetDB"). Only used when source = "repo".
#'   If NULL (default), a pre-configured URL will be used.
#' @param zenodo_doi Character string specifying the Zenodo DOI. Only used when source = "zenodo".
#'   If NULL (default), a pre-configured DOI will be used.
#' @param github_token Character string containing a GitHub Personal Access Token (PAT) for accessing private repositories.
#'   If NULL (default), the function will look for a token in the GITHUB_PAT or GITHUB_TOKEN environment variable.
#'   Set this with \code{Sys.setenv(GITHUB_PAT = "your_token_here")} or in your .Renviron file.
#' @param release_tag Character string specifying a specific GitHub release tag to use (e.g., "v0.1.0").
#'   If NULL (default), the latest release will be used. Only used when source = "repo".
#'
#' @return Invisibly returns the path to the downloaded data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download data from repository, as DuckDB (automatically uses latest release)
#' get_psi_data()
#'
#' # Download data from Zenodo, as CSV files
#' get_psi_data(source = "zenodo", format = "csv")
#'
#' # Specify a custom destination directory
#' get_psi_data(dest_dir = "~/my_data")
#'
#' # Download a specific release version
#' get_psi_data(release_tag = "v0.1.0")
#'
#' # Access data from a private GitHub repository using a token
#' get_psi_data(github_token = "your_github_token")
#'
#' # Alternative: set token in environment and use it
#' Sys.setenv(GITHUB_PAT = "your_github_token")
#' get_psi_data()
#' }
get_psi_data <- function(source = c("repo", "zenodo"),
                          format = c("duckdb", "csv"),
                          dest_dir = NULL,
                          overwrite = FALSE,
                          repo_url = NULL,
                          zenodo_doi = NULL,
                          github_token = NULL,
                          release_tag = NULL) {
  source <- match.arg(source)
  format <- match.arg(format)
  
  # Set default destination directory if not provided
  if (is.null(dest_dir)) {
    dest_dir <- get_data_dir()
  }
  
  # Create destination directory if it doesn't exist
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
    cli::cli_alert_success("Created data directory: {.file {dest_dir}}")
  }
  
  # Handle GitHub authentication
  if (source == "repo" && is.null(github_token)) {
    github_token <- Sys.getenv("GITHUB_PAT", Sys.getenv("GITHUB_TOKEN", ""))
    if (github_token == "") {
      cli::cli_alert_warning("No GitHub token provided. If accessing a private repository, this may fail.")
      cli::cli_alert_info("Set {.envvar GITHUB_PAT} or provide {.arg github_token} parameter for private repos.")
    }
  }
  
  # Download based on source
  cli::cli_h2("Downloading Plant Water Potential Data")
  
  if (source == "repo") {
    cli::cli_alert_info("Source: GitHub Repository")
    data_path <- download_from_repo(
      format = format, 
      dest_dir = dest_dir, 
      overwrite = overwrite, 
      repo_url = repo_url,
      github_token = github_token,
      release_tag = release_tag
    )
  } else {
    cli::cli_alert_info("Source: Zenodo")
    data_path <- download_from_zenodo(
      format = format, 
      dest_dir = dest_dir, 
      overwrite = overwrite, 
      zenodo_doi = zenodo_doi
    )
  }
  
  # Return path to downloaded data
  invisible(data_path)
}