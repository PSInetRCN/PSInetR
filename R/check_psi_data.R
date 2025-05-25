#' Check for Local Plant Water Potential Data
#'
#' This function checks if the plant water potential data is available locally.
#' It can check for both DuckDB and CSV formats of the data.
#'
#' @param format Character string specifying the data format to check for, either "duckdb" (default) or "csv".
#' @param dir Character string specifying the directory to check for data.
#'   If NULL (default), the function will check the default location determined by \code{get_data_dir()}.
#' @param validate Logical indicating whether to perform basic validation on the data. Default is FALSE.
#'
#' @return Logical indicating whether the data is available locally.
#' @export
#'
#' @examples
#' \dontrun{
#' # Check if DuckDB data is available locally
#' is_available <- check_psi_data()
#'
#' # Check if CSV data is available locally
#' is_available <- check_psi_data(format = "csv")
#'
#' # Check in a specific directory and validate the data
#' is_available <- check_psi_data(dir = "~/my_data", validate = TRUE)
#' }
check_psi_data <- function(format = c("duckdb", "csv"),
                           dir = NULL,
                           validate = FALSE) {
  format <- match.arg(format)
  
  # Set default directory if not provided
  if (is.null(dir)) {
    dir <- get_data_dir()
  }
  
  # Construct file path based on format
  if (format == "duckdb") {
    file_path <- file.path(dir, "psinet.duckdb")
    is_available <- file.exists(file_path)
    
    # Validate DuckDB file if requested
    if (is_available && validate) {
      tryCatch({
        con <- DBI::dbConnect(duckdb::duckdb(), file_path)
        tables <- DBI::dbListTables(con)
        DBI::dbDisconnect(con)
        is_available <- length(tables) > 0
      }, error = function(e) {
        cli::cli_alert_warning("DuckDB file exists but appears to be invalid: {e$message}")
        return(FALSE)
      })
    }
  } else {
    # For CSV format, check directory and files within it
    csv_dir <- file.path(dir, "psinet_csv")
    is_available <- dir.exists(csv_dir)
    
    # Validate CSV files if requested
    if (is_available && validate) {
      csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
      is_available <- length(csv_files) > 0
      
      if (is_available) {
        # Sample check of first CSV file
        tryCatch({
          data <- utils::read.csv(csv_files[1], nrows = 5)
          is_available <- nrow(data) > 0 && ncol(data) > 0
        }, error = function(e) {
          cli::cli_alert_warning("CSV files exist but at least one appears to be invalid: {e$message}")
          return(FALSE)
        })
      }
    }
  }
  
  if (is_available) {
    cli::cli_alert_success("Plant water potential data in {.val {format}} format is available locally")
    cli::cli_alert_info("Location: {.file {ifelse(format == 'duckdb', file_path, csv_dir)}}")
  } else {
    cli::cli_alert_warning("Plant water potential data in {.val {format}} format is not available locally")
    cli::cli_alert_info("Use {.code get_psi_data()} to download it")
  }
  
  return(is_available)
}