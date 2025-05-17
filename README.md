# PSINetR

<!-- badges: start -->
[![R-CMD-check](https://github.com/PSInetRCN//PSINetR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PSInetRCN//PSINetR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/PSInetRCN//PSINetR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/PSInetRCN//PSINetR?branch=main)
<!-- badges: end -->

## Overview

PSINetR provides easy access to plant water potential data from a collaborative network of researchers. The package allows users to download, validate, and work with plant water potential measurements across various species, ecosystems, and time periods.

## Features

- Download plant water potential data from repositories or Zenodo
- Choose between DuckDB database or CSV file formats
- Authenticate with GitHub to access private data repositories
- Check and validate local data files
- Tools for analyzing and visualizing plant water potential data

## Installation

You can install the development version of PSINetR from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("username/PSINetR")
```

## Usage

```r
library(PSINetR)

# Download data (default: from repository as DuckDB)
get_psi_data()

# Download from Zenodo as CSV files
get_psi_data(source = "zenodo", format = "csv")

# Check if data is available locally
check_psi_data()

# For private repositories, provide GitHub token
get_psi_data(github_token = "your_token")
# Or set in .Renviron: GITHUB_PAT=your_token
```

## Data Sources

PSINetR accesses data from:

1. **GitHub Repository**: Contains the most up-to-date data
2. **Zenodo**: Contains stable, versioned data releases with DOIs

The data is available in two formats:

1. **DuckDB**: A fast, efficient database format ideal for analysis
2. **CSV files**: Standard format for maximum compatibility

## Documentation

For more detailed information, check out the vignettes:

- `vignette("getting-started", package = "PSINetR")`
- `vignette("working-with-duckdb", package = "PSINetR")`

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
