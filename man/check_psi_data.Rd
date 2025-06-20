% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_psi_data.R
\name{check_psi_data}
\alias{check_psi_data}
\title{Check for Local Plant Water Potential Data}
\usage{
check_psi_data(format = c("duckdb", "csv"), dir = NULL, validate = FALSE)
}
\arguments{
\item{format}{Character string specifying the data format to check for, either "duckdb" (default) or "csv".}

\item{dir}{Character string specifying the directory to check for data.
If NULL (default), the function will check the default location determined by \code{get_data_dir()}.}

\item{validate}{Logical indicating whether to perform basic validation on the data. Default is FALSE.}
}
\value{
Logical indicating whether the data is available locally.
}
\description{
This function checks if the plant water potential data is available locally.
It can check for both DuckDB and CSV formats of the data.
}
\examples{
\dontrun{
# Check if DuckDB data is available locally
is_available <- check_psi_data()

# Check if CSV data is available locally
is_available <- check_psi_data(format = "csv")

# Check in a specific directory and validate the data
is_available <- check_psi_data(dir = "~/my_data", validate = TRUE)
}
}
