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
# # Download data from Zenodo as CSV files
# get_psi_data(source = "zenodo", format = "csv")
# 
# # If the repository is private, provide a GitHub token
# get_psi_data(github_token = "your_github_token")


## ----checking, eval=FALSE-----------------------------------------------------
# # Check if DuckDB data is available
# check_psi_data()
# 
# # Check if CSV data is available
# check_psi_data(format = "csv")
# 
# # Check and validate the data
# check_psi_data(validate = TRUE, dir = "/Users/keatonwilson/Documents/Projects/PSInetR/vignettes/")


## ----basic-exploration, eval=FALSE--------------------------------------------
# library(DBI)
# library(duckdb)
# library(dplyr)
# library(dbplyr)
# 
# # Connect to the DuckDB database
# db_path <- get_db_path()
# con <- dbConnect(duckdb::duckdb(), db_path)
# 
# # List available tables
# dbListTables(con)
# 
# # Preview the study sites
# study_sites <- tbl(con, "study_site") |>
#   collect()
# head(study_sites)
# 
# # Count plants by species
# species_counts <- tbl(con, "plants") |>
#   group_by(genus, specific_epithet) |>
#   summarize(plant_count = n()) |>
#   arrange(desc(plant_count)) |>
#   collect()
# head(species_counts)
# 
# # Don't forget to disconnect
# dbDisconnect(con, shutdown = TRUE)


## ----water-potential, eval=FALSE----------------------------------------------
# library(DBI)
# library(duckdb)
# library(dplyr)
# library(dbplyr)
# library(ggplot2)
# 
# # Connect to the database
# db_path <- get_db_path()
# con <- dbConnect(duckdb::duckdb(), db_path)
# 
# # Get water potential data for a specific plant - Sorghum bicolor
# sorghum_ids <- tbl(con, "plant_types") |>
#   filter(genus == "Sorghum" & specific_epithet == "bicolor")
# 
# # right join by filtered dataset above
# wp_sorg <- tbl(con, "press_chamb_wb") |>
#   right_join(sorghum_ids, by = c("dataset_name", "individual_id"))
# 
# # collect just to check and see dimensions
# wp_sorg |> collect()
# 
# # Plot water potential vs leaf area
# wp_sorg |>
#   ggplot(aes(x = leaf_area_index_m2_m2, y = water_potential_mean)) +
#   geom_point() +
#   labs(title = "Sorghum Water Potential by Leaf Area",
#        x = "Leaf Area (m2)",
#        y = "Water Potential (MPa)") +
#   theme_minimal()
# 
# # Disconnect
# dbDisconnect(con, shutdown = TRUE)


## ----combined-analysis, eval=FALSE--------------------------------------------
# library(DBI)
# library(duckdb)
# library(dplyr)
# library(ggplot2)
# 
# # Connect to the database
# db_path <- get_db_path()
# con <- dbConnect(duckdb::duckdb(), db_path)
# 
# # Get plants, water potential, and environmental data for a study site
# study <- "Ben_1"
# 
# # Get water potential data
# wp_data <- tbl(con, "press_chamb_wb") |>
#   filter(dataset_name == study) |>
#   collect()
# 
# # Get environmental data
# env_data <- tbl(con, "env_vars") |>
#   filter(dataset_name == study) |>
#   collect()
# 
# # Join water potential and VPD data by date and time
# combined_data <- wp_data |>
#   inner_join(env_data, by = c("dataset_name", "date", "time")) |>
#   select(date, water_potential_mean, vapor_pressure_deficit_k_pa,
#          air_temperature_c, precipitation_mm)
# 
# # Plot water potential vs. VPD
# combined_data |>
#   ggplot(aes(x = vapor_pressure_deficit_k_pa, y = water_potential_mean)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   labs(title = "Plant Water Potential vs. Vapor Pressure Deficit",
#        x = "VPD (kPa)",
#        y = "Water Potential (MPa)") +
#   theme_minimal()
# 
# # Disconnect
# dbDisconnect(con, shutdown = TRUE)


## ----integration, eval=FALSE--------------------------------------------------
# # Example: Use tidyverse for data wrangling and visualization
# library(dplyr)
# library(ggplot2)
# library(DBI)
# library(duckdb)
# 
# # Connect to the database
# db_path <- get_db_path()
# con <- dbConnect(duckdb::duckdb(), db_path)
# 
# # Get predawn and midday water potential measurements
# wp_data <- tbl(con, "press_chamb_wb") |>
#   filter(
#     organ == "stem" | organ == "leaf",
#     !is.na(water_potential_mean)
#   ) |>
#   left_join(
#     tbl(con, "plants"),
#     by = c("dataset_name", "individual_id")
#   ) |>
#   collect()
# 
# # Create time of day category based on time values
# # The time column appears to contain seconds since midnight
# # Convert to hours for easier categorization
# wp_data_filtered <- wp_data |>
#   mutate(
#     # Convert character time to numeric seconds, then to hours
#     time_numeric = as.numeric(time),
#     hour_decimal = time_numeric / 3600,  # Convert seconds to hours
#     hour = floor(hour_decimal),          # Extract integer hour
#     minute = round((hour_decimal - hour) * 60), # Extract minutes
# 
#     # Categorize based on typical measurement times
#     # NOTE: Adjust these ranges based on your specific measurement protocols
#     time_of_day = case_when(
#       hour >= 4 & hour <= 7 ~ "Predawn",    # 4:00 AM to 7:59 AM
#       hour >= 11 & hour <= 14 ~ "Midday",   # 11:00 AM to 2:59 PM
#       TRUE ~ "Other"
#     )
#   ) |>
#   filter(time_of_day %in% c("Predawn", "Midday"))
# 
# # Alternative approach if you want to see what times are actually present:
# # wp_data |>
# #   mutate(
# #     time_numeric = as.numeric(time),
# #     hour_decimal = time_numeric / 3600,
# #     hour = floor(hour_decimal),
# #     minute = round((hour_decimal - hour) * 60),
# #     clock_time = sprintf("%02d:%02d", hour, minute)
# #   ) |>
# #   count(clock_time, sort = TRUE) |>
# #   head(20)  # This will show you the most common measurement times
# 
# # Visualize water potential patterns by time of day and species
# wp_data_filtered %>%
#   ggplot(aes(x = genus, y = water_potential_mean, fill = time_of_day)) +
#   geom_boxplot() +
#   labs(
#     title = "Water Potential by Species and Time of Day",
#     x = "Genus",
#     y = "Water Potential (MPa)",
#     fill = "Time of Day"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # Disconnect
# dbDisconnect(con, shutdown = TRUE)

