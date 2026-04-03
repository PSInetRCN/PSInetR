## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE  # Set to FALSE since we don't have actual data during package build
)


## ----setup--------------------------------------------------------------------
library(PSInetR)
library(dplyr)
library(DBI)
library(duckdb)
library(lubridate)


## ----connect------------------------------------------------------------------
# Download the data (if not already available)
data_path <- get_psi_data(format = "duckdb")

# If you already have it, get the path to the db
path_to_db <- get_db_path()

# Connect to the DuckDB database
con <- dbConnect(duckdb::duckdb(), path_to_db)

# List available tables
tables <- dbListTables(con)
print(tables)


## ----explore_tables-----------------------------------------------------------
# List all tables in the database
dbListTables(con)

# Look at the database schema
db_schema <- DBI::dbGetQuery(con, "
  SELECT 
    table_name, 
    column_name, 
    data_type
  FROM information_schema.columns 
  WHERE table_schema = 'main'
  ORDER BY table_name, ordinal_position
")

# View the schema of a specific table
chamber_wp_fields <- dbListFields(con, "chamber_wp")
print(chamber_wp_fields)

# Check the dimensions of core tables
core_tables <- c("study_site", "plot", "plant", "treatment", "data_description", "addtl_data", "authorship")
for(table in core_tables) {
  count <- dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table))
  cat(table, ":", count$count, "records\n")
}

# Check the dimensions of measurement tables  
measurement_tables <- c("chamber_wp", "auto_wp", "soil_var", "met_var", "auto_wp_sensor")
for(table in measurement_tables) {
  count <- dbGetQuery(con, paste("SELECT COUNT(*) as count FROM", table))
  cat(table, ":", count$count, "records\n")
}


## ----basic_queries------------------------------------------------------------
# Get a list of all study sites
study_sites <- tbl(con, "study_site") |>
  select(dataset_name, latitude_wgs84, longitude_wgs84) |>
  head(10) |>
  collect()
print(study_sites)

# Get plant species information
plant_species <- tbl(con, "plant") |>
  select(genus, specific_epithet) |>
  distinct() |>
  head(10) |>
  collect()
print(plant_species)

# Get statistics on water potential measurements
wp_stats <- tbl(con, "chamber_wp") |>
  filter(!is.na(water_potential_mean)) |>
  summarize(
    count = n(),
    avg_potential = mean(water_potential_mean, na.rm = TRUE),
    min_potential = min(water_potential_mean, na.rm = TRUE),
    max_potential = max(water_potential_mean, na.rm = TRUE)
  ) |>
  collect()
print(wp_stats)


## ----dplyr_queries------------------------------------------------------------
# Create references to the tables
chamber_wp_tbl <- tbl(con, "chamber_wp")
plant_tbl <- tbl(con, "plant")
plot_tbl <- tbl(con, "plot")

# Query using dplyr syntax for water potential by species
wp_by_species <- chamber_wp_tbl |>
  inner_join(plant_tbl, by = c("dataset_name", "individual_id")) |>
  group_by(genus, specific_epithet) |>
  summarize(
    count = n(),
    avg_potential = mean(water_potential_mean, na.rm = TRUE),
    min_potential = min(water_potential_mean, na.rm = TRUE),
    max_potential = max(water_potential_mean, na.rm = TRUE)
  ) |>
  arrange(desc(count)) |>
  collect()  # This executes the query and brings results into R

print(wp_by_species)

# Find measurements by organ type
organ_summary <- chamber_wp_tbl |>
  group_by(organ, canopy_position) |>
  summarize(
    count = n(),
    avg_potential = mean(water_potential_mean, na.rm = TRUE),
    avg_n_samples = mean(water_potential_n, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(desc(count)) |>
  collect()

print(organ_summary)


## ----advanced_analyses--------------------------------------------------------
# Analyze seasonal patterns in water potential
seasonal_patterns <- chamber_wp_tbl |>
  # must collect first because substr can't be run by db
  collect() |>
  # Extract month from date (YYYYMMDD format)
  mutate(month = month(date)) |>
  group_by(month) |>
  summarize(
    avg_potential = mean(water_potential_mean, na.rm = TRUE),
    sd_potential = sd(water_potential_mean, na.rm = TRUE),
    sample_count = n()
  ) 

print(seasonal_patterns)

# Comparing water potential with soil variables
soil_wp_comparison <- chamber_wp_tbl |>
  # Join with soil data on dataset_name, plot_id, and date
  inner_join(
    tbl(con, "soil_var"),
    by = c("dataset_name", "plot_id", "date")
  ) |>
  # Group by soil moisture categories (using a window function)
  mutate(
    swc_shallow_category = case_when(
      swc_mean_shallow < 0.1 ~ "very low",
      swc_mean_shallow < 0.2 ~ "low",
      swc_mean_shallow < 0.3 ~ "medium",
      TRUE ~ "high"
    )
  ) |>
  group_by(swc_shallow_category) |>
  summarize(
    avg_water_potential = mean(water_potential_mean, na.rm = TRUE),
    sd_water_potential = sd(water_potential_mean, na.rm = TRUE),
    n_observations = n()
  ) |>
  collect()

print(soil_wp_comparison)

# Analyzing the relationship between meteorological variables and water potential
met_wp_comparison <- chamber_wp_tbl |>
  # Join with meteorological data on dataset_name and date
  inner_join(
    tbl(con, "met_var"),
    by = c("dataset_name", "date")
  ) |>
  # Select relevant columns
  select(
    dataset_name, date, water_potential_mean, water_potential_sd,
    vapor_pressure_deficit_k_pa, air_temperature_c, precipitation_mm
  ) |>
  # collect before running summarize to make sure cor works correctly
  collect() |>
  # Calculate correlations
  summarize(
    vpd_correlation = cor(water_potential_mean, vapor_pressure_deficit_k_pa, 
                           use = "complete.obs"),
    temp_correlation = cor(water_potential_mean, air_temperature_c, 
                            use = "complete.obs"),
    precip_correlation = cor(water_potential_mean, precipitation_mm, 
                              use = "complete.obs"),
    n_observations = n()
  ) 

print(met_wp_comparison)



## ----visualizations, message=FALSE, warning=FALSE, fig.height=8, fig.width=10----
library(ggplot2)
library(lubridate)

# Get water potential data for visualization
wp_time_data <- tbl(con, "chamber_wp") |>
  filter(!is.na(water_potential_mean)) |>
  collect() |>
  mutate(date_parsed = ymd(as.character(date)))  # Convert YYYYMMDD to Date

# Get water potential by species
wp_species <- tbl(con, "chamber_wp") |>
  inner_join(tbl(con, "plant"), by = c("dataset_name", "individual_id", "plot_id")) |>
  group_by(genus, specific_epithet) |>
  summarize(
    mean_wp = mean(water_potential_mean, na.rm = TRUE),
    sd_wp = sd(water_potential_mean, na.rm = TRUE),
    count = n()
  ) |>
  filter(count > 5) |>  # Only species with enough measurements
  collect()

# Create species name
wp_species$species <- paste(wp_species$genus, wp_species$specific_epithet)

# Show only the 20 species with most extreme (lowest) water potentials
top_species <- wp_species |>
  arrange(mean_wp) |>
  head(20)

top_species$species <- paste(top_species$genus, top_species$specific_epithet)

ggplot(top_species, aes(x = reorder(species, mean_wp), y = mean_wp)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_wp - sd_wp, ymax = mean_wp + sd_wp), width = 0.2) +
  labs(
    title = "Top 20 Species with Lowest Water Potential",
    x = "Species",
    y = "Water Potential (MPa)"
  ) +
  coord_flip() +
  theme_minimal()

# Don't forget to disconnect
dbDisconnect(con, shutdown = TRUE)


## ----combined_measurements----------------------------------------------------
# Connect to database
con <- dbConnect(duckdb::duckdb(), get_db_path())

# Combine automated and manual measurements using dplyr
# First get automated measurements
auto_data <- tbl(con, "auto_wp") |>
  inner_join(tbl(con, "plant"), by = c("dataset_name", "individual_id")) |>
  filter(!is.na(water_potential_mean)) |>
  select(dataset_name, individual_id, date, time, 
         water_potential_mean, water_potential_sd, genus, specific_epithet) |>
  mutate(measurement_type = "automated") |>
  head(500) |>  # Limit to avoid memory issues
  collect()

# Then get manual measurements  
manual_data <- tbl(con, "chamber_wp") |>
  inner_join(tbl(con, "plant"), by = c("dataset_name", "individual_id")) |>
  filter(!is.na(water_potential_mean)) |>
  select(dataset_name, individual_id, date, time,
         water_potential_mean, water_potential_sd, genus, specific_epithet) |>
  mutate(measurement_type = "manual") |>
  head(500) |>  # Limit to avoid memory issues
  collect()

# Combine the datasets
combined_data <- bind_rows(auto_data, manual_data)

# Compare automated vs manual measurements
measurement_summary <- combined_data |>
  group_by(measurement_type) |>
  summarize(
    count = n(),
    avg_potential = mean(water_potential_mean, na.rm = TRUE),
    sd_potential = sd(water_potential_mean, na.rm = TRUE),
    min_potential = min(water_potential_mean, na.rm = TRUE),
    max_potential = max(water_potential_mean, na.rm = TRUE)
  )

print(measurement_summary)

# Disconnect
dbDisconnect(con, shutdown = TRUE)


## ----performance_example------------------------------------------------------
# Example of good practice
con <- dbConnect(duckdb::duckdb(), get_db_path())

# Efficient query - filtering happens in database
efficient_query <- tbl(con, "chamber_wp") |>
  filter(water_potential_mean < -1.0) |>
  select(dataset_name, individual_id, date, water_potential_mean) |>
  collect()

# Disconnect
dbDisconnect(con, shutdown = TRUE)

