# PSInetR

<!-- badges: start -->
[![R-CMD-check](https://github.com/PSInetRCN//PSInetR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PSInetRCN//PSInetR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

PSInetR provides easy access to plant water potential (Ψ) data from the PSInet Research Coordination Network. The package allows users to download, validate, and work with plant water potential measurements across various species, ecosystems, and time periods.

### About PSInet

PSInet is a network focused on collecting and aggregating plant water potential (Ψ) data to improve understanding of ecosystem responses to drought and heat stress. Water potential can be thought of as the "blood pressure of the natural world" that directly controls plant functioning during drought.

The network aims to:
- Create a global, open database of plant and soil water potential time series
- Promote consistent data collection through shared protocols
- Support synthetic research on plant drought responses
- Enhance understanding of ecosystem water dynamics

PSInet fills a critical missing link between information about environmental drivers and physiological responses by providing a centralized, comprehensive water potential database.

### Lead Investigators
- Kim Novick (Indiana University)
- Jessica Guo (Harvey Mudd College)
- Dan Johnson (University of Georgia)
- Kate McCulloh (University of Wisconsin - Madison)

## Features

- Download plant water potential data from repositories or Zenodo
- Choose between DuckDB database or CSV file formats
- Authenticate with GitHub to access private data repositories
- Check and validate local data files
- Tools for analyzing and visualizing plant water potential data

### Data Characteristics

The PSInet database includes:

- Time series of plant and soil water potential measurements
- Data harmonized with other environmental observation networks
- Measurements across diverse ecosystems and plant species
- Information relevant for studying:
  - Plant responses to drought stress
  - Ecosystem water flows
  - Vegetation functioning under climate change
  - Connections between environmental drivers and physiological responses

## Installation

You can install the development version of PSInetR from GitHub with:

```r
# install.packages("remotes")
# To install with vignettes (recommended):
remotes::install_github("PSInetRCN/PSInetR", build_vignettes = TRUE)

# Or to install without vignettes (faster):
remotes::install_github("PSInetRCN/PSInetR")
```

**Note**: Installing with `build_vignettes = TRUE` is recommended to access the package documentation and examples, but requires that you have Pandoc installed on your system.

## Usage

```r
library(PSInetR)

# Download data (default: from repository as DuckDB)
get_psi_data()

# Download from Zenodo as CSV files
get_psi_data(source = "zenodo", format = "csv")

# Check if data is available locally
check_psi_data()

# Get path to database file for analysis
db_path <- get_db_path()

# For private repository access (e.g. early access), provide GitHub token
get_psi_data(github_token = "your_token")
# Or set in .Renviron: GITHUB_PAT=your_token
```

## Data Sources

PSInetR accesses data from:

1. **GitHub Repository**: Contains the most up-to-date data
2. **Zenodo**: Contains stable, versioned data releases with DOIs (forthcoming)

The data is available in two formats:

1. **DuckDB**: A fast, efficient database format ideal for analysis (single file)
2. **CSV files**: Individual CSV files for each database table, providing maximum compatibility (forthcoming)

## Documentation

For more detailed information, check out the vignettes:

- `vignette("getting-started", package = "PSInetR")` - Package overview and basic setup
- `vignette("working-with-duckdb", package = "PSInetR")` - Accessing database with DuckDB
- `vignette("data-analysis-examples", package = "PSInetR")` - Practical analysis examples

If you installed without vignettes, you can view them online:
- [Getting Started](https://github.com/PSInetRCN/PSInetR/blob/main/vignettes/getting-started.Rmd)
- [Working with DuckDB](https://github.com/PSInetRCN/PSInetR/blob/main/vignettes/working-with-duckdb.Rmd)
- [Data Analysis Examples](https://github.com/PSInetRCN/PSInetR/blob/main/vignettes/data-analysis-examples.Rmd)

## Development

### Test Coverage

To generate a local test coverage report:

```r
# Install the covr package if not already installed
if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr")

# Generate a test coverage report
PSInetR:::generate_coverage_report()
```

This will create an HTML report in the `coverage` directory and display a summary in the console.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

PSInetR is part of the PSInet Research Coordination Network initiative, which aims to build a global database of plant water potential measurements. For more information, visit [the PSInet website](https://psinetrcn.github.io/).

The PSInet initiative brings together researchers from multiple institutions to address fundamental questions about plant water use, drought responses, and ecosystem functioning under changing climate conditions.
