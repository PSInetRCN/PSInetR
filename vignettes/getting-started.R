## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(PSInetR)


## ----downloading, eval=FALSE--------------------------------------------------
# # Download data from repository as DuckDB (default)
# get_psi_data()
# 
# # Download individual CSV files from Zenodo (forthcoming)
# # get_psi_data(source = "zenodo", format = "csv")
# 
# # If the repository is private, provide a GitHub token
# get_psi_data(github_token = "your_github_token")
# 
# # Alternatively, store your GitHub token in .Renviron file:
# # GITHUB_PAT=your_github_token_here
# # Then simply call:
# # get_psi_data()


## ----basic-connection, eval=FALSE---------------------------------------------
# library(DBI)
# library(duckdb)
# 
# # Get the path to the database and connect
# db_path <- get_db_path()
# con <- dbConnect(duckdb::duckdb(), db_path)
# 
# # List all available tables
# tables <- dbListTables(con)
# print(tables)
# 
# # Always disconnect when finished
# dbDisconnect(con, shutdown = TRUE)


## ----next-steps, eval=FALSE---------------------------------------------------
# # View the examples vignette
# vignette("data-analysis-examples", package = "PSInetR")


## ----advanced, eval=FALSE-----------------------------------------------------
# # View the DuckDB vignette
# vignette("working-with-duckdb", package = "PSInetR")

