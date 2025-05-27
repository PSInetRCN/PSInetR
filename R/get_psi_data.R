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
#'   If NULL (default), files will be downloaded to the current working directory.
#' @param overwrite Logical indicating whether to overwrite existing files. Default is FALSE.
#' @param repo_url Character string specifying the repository URL or name (e.g., "PSInetRCN/PSInetDB"). Only used when source = "repo".
#'   If NULL (default), a pre-configured URL will be used.
#' @param zenodo_doi Character string specifying the Zenodo DOI. Only used when source = "zenodo".
#'   If NULL (default), a pre-configured DOI will be used.
#' @param github_token Character string containing a GitHub Personal Access Token (PAT) for accessing private repositories.
#'   If NULL (default), the function will look for a token in the GITHUB_PAT or GITHUB_TOKEN environment variable.
#'   Set this with \code{Sys.setenv(GITHUB_PAT = "your_token_here")} or in your .Renviron file.
#'   For private repositories, make sure your token has the 'repo' scope.
#' @param release_tag Character string specifying a specific GitHub release tag to use (e.g., "v0.1.0").
#'   If NULL (default), the latest release will be used. Only used when source = "repo".
#'   If the latest release cannot be retrieved (due to permissions or no releases), the function will fail
#'   and you'll need to provide a specific release_tag.
#' @param direct_url Character string specifying a direct URL to the file to download. If provided, this overrides
#'   the repository and release tag settings and downloads directly from the specified URL.
#' @param file_name Character string specifying the file name to download from the release. By default, this is
#'   "psinet.duckdb" for DuckDB format. For CSV format, individual CSV files will be downloaded automatically.
#' @param verbose Logical indicating whether to print extra progress information. Default is TRUE.
#'
#' @return Invisibly returns the path to the downloaded data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download data from repository, as DuckDB (automatically uses latest release)
#' get_psi_data()
#'
#' # Download individual CSV files from Zenodo
#' get_psi_data(source = "zenodo", format = "csv")
#'
#' # Specify a custom destination directory
#' get_psi_data(dest_dir = "./my_data")
#'
#' # Download a specific release version
#' get_psi_data(release_tag = "v0.1.0")
#'
#' # Access data from a private GitHub repository using a token
#' get_psi_data(github_token = "your_github_token")
#'
#' # Download directly from a specific URL
#' get_psi_data(direct_url = "https://example.com/path/to/psinet.duckdb")
#'
#' # Specify a different file name from the release
#' get_psi_data(release_tag = "v0.1.0", file_name = "psi_data.duckdb")
#'
#' # Example for PSInetRCN/PSInetDB with specific file and tag
#' get_psi_data(repo_url = "PSInetRCN/PSInetDB", release_tag = "v0.1.0", file_name = "psinet.duckdb")
#'
#' # Turn off verbose output for silent operation
#' get_psi_data(verbose = FALSE)
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
                          release_tag = NULL,
                          direct_url = NULL,
                          file_name = NULL,
                          verbose = TRUE) {
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

    # Progress information if verbose mode is on
    if (verbose) {
      if (!is.null(direct_url)) {
        cli::cli_alert_info("Using direct URL: {.url {direct_url}}")
      } else {
        cli::cli_alert_info("Repository: {.val {if(is.null(repo_url)) 'PSInetRCN/PSInetDB' else repo_url}}")
        cli::cli_alert_info("Format: {.val {format}}")
        if (!is.null(release_tag)) {
          cli::cli_alert_info("Using release tag: {.val {release_tag}}")
        } else {
          cli::cli_alert_info("Fetching latest release tag...")
        }
        if (!is.null(file_name)) {
          cli::cli_alert_info("Looking for file: {.val {file_name}}")
        }
      }
    }

    data_path <- download_from_repo(
      format = format,
      dest_dir = dest_dir,
      overwrite = overwrite,
      repo_url = repo_url,
      github_token = github_token,
      release_tag = release_tag,
      direct_url = direct_url,
      file_name = file_name,
      verbose = verbose
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
