#' Generate Test Coverage Report
#'
#' This function generates a test coverage report for the package.
#' It is useful for local development and CI/CD pipelines to ensure
#' that code is adequately tested.
#'
#' @param type Character string specifying the type of report to generate.
#'   Options are "report" for an HTML report, "summary" for a console summary,
#'   or "both" for both types of output. Default is "both".
#' @param output_dir Character string specifying the directory where the HTML
#'   report should be saved. Default is "coverage".
#'
#' @return A covr package coverage object (invisibly).
#'
#' @examples
#' \dontrun{
#' # Generate both console summary and HTML report
#' generate_coverage_report()
#'
#' # Generate only a console summary
#' generate_coverage_report(type = "summary")
#' }
#'
#' @keywords internal
generate_coverage_report <- function(type = c("both", "report", "summary"),
                                    output_dir = "coverage") {
  type <- match.arg(type)
  
  # Check if covr is installed
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Package 'covr' is needed for this function. Please install it with: install.packages('covr')")
  }
  
  # Calculate coverage
  coverage <- covr::package_coverage()
  
  # Generate summary if requested
  if (type %in% c("both", "summary")) {
    summary_data <- covr::coverage_to_list(coverage)
    
    # Calculate total coverage
    total_pct <- summary_data$totalcoverage
    
    # Print summary to console
    cat("\n=== PSINetR Test Coverage Summary ===\n")
    cat(sprintf("Total coverage: %.2f%%\n", total_pct))
    cat("\nCoverage by file:\n")
    for (file_data in summary_data$filecoverage) {
      cat(sprintf("- %s: %.2f%%\n", file_data$filename, file_data$coverage))
    }
    cat("\n")
  }
  
  # Generate HTML report if requested
  if (type %in% c("both", "report")) {
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Generate HTML report
    report_path <- file.path(output_dir, "coverage.html")
    covr::report(coverage, file = report_path)
    
    cat(sprintf("HTML coverage report generated at: %s\n", report_path))
  }
  
  # Return coverage object invisibly
  invisible(coverage)
}

#' Get Test Coverage Percentage 
#'
#' This function calculates the total test coverage percentage for the package.
#' It's useful for generating badges or status reports.
#'
#' @return A numeric value representing the percentage of code covered by tests.
#'
#' @examples
#' \dontrun{
#' coverage_pct <- get_coverage_percentage()
#' }
#'
#' @keywords internal
get_coverage_percentage <- function() {
  # Check if covr is installed
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("Package 'covr' is needed for this function. Please install it with: install.packages('covr')")
  }
  
  # Calculate coverage
  coverage <- covr::package_coverage()
  
  # Convert to percentage
  coverage_data <- covr::coverage_to_list(coverage)
  coverage_pct <- coverage_data$totalcoverage
  
  return(coverage_pct)
}