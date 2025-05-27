# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

PSINetR is an R package that provides access to plant water potential data from a collaborative network of researchers. The package allows users to download, validate, and work with plant water potential measurements across various species, ecosystems, and time periods.

Key capabilities:
- Download plant water potential data from GitHub repositories or Zenodo
- Store data in DuckDB database or CSV file formats
- Handle authentication for private GitHub repositories
- Validate and check local data files
- Tools for analyzing plant water potential data

## Development Commands

### Installing Dependencies

```r
# Install package dependencies
remotes::install_deps(dependencies = TRUE)

# Using renv (package dependency manager)
renv::restore()
```

### Running Tests

```r
# Run all tests
devtools::test()

# Run a specific test file
devtools::test_file("tests/testthat/test-psi_data.R")

# Check test coverage
covr::package_coverage()
```

### Documentation

```r
# Generate documentation
devtools::document()

# Build vignettes
devtools::build_vignettes()
```

### Package Checks

```r
# Run R CMD check (complete package check)
devtools::check()

# Perform lint checks (if lintr is used)
lintr::lint_package()
```

### Package Installation

```r
# Install the package locally
devtools::install()

# Build the package
devtools::build()
```

## Architecture

The package has a straightforward structure:

1. **Data Acquisition Layer**:
   - `get_psi_data()`: Main function for downloading data from repositories or Zenodo
   - `download_from_repo()`: Helper function for GitHub downloads
   - `download_from_zenodo()`: Helper function for Zenodo downloads

2. **Validation Layer**:
   - `check_psi_data()`: Verifies if data is available locally and can validate its integrity

3. **Utility Functions**:
   - `get_data_dir()`: Provides the default data directory location

## Data Flow

1. User calls `get_psi_data()` with parameters for source, format, etc.
2. Based on source parameter, either `download_from_repo()` or `download_from_zenodo()` is called
3. Data is downloaded to the specified directory (default: ~/.PSINetR/)
4. Data can be accessed directly from the downloaded location

## Development Notes

1. **TODO Items**: Check the TODO.md file for pending tasks, which include:
   - Adding specific repository URL information in download functions
   - Adding Zenodo DOI information
   - Expanding test coverage
   - Building out vignettes with real data examples
   - Adding data exploration helper functions

2. **Testing Strategy**:
   - Unit tests are in tests/testthat/
   - Mock functions are used to avoid file system dependencies in tests

3. **Documentation**:
   - Function documentation is written using roxygen2
   - Package has vignettes for getting started and working with DuckDB

4. **Data Formats**:
   - DuckDB: High-performance analytical database
   - CSV: Standard format for maximum compatibility