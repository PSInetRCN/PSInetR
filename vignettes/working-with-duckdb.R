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
press_chamb_fields <- dbListFields(con, "press_chamb_wp")
print(press_chamb_fields)


## ----basic_queries------------------------------------------------------------
# Get a list of all study sites
study_sites <- dbGetQuery(con, "SELECT dataset_name, latitude_wgs84, longitude_wgs84 FROM study_site LIMIT 10")
print(study_sites)

# Get plant species information
plant_species <- dbGetQuery(con, "SELECT DISTINCT genus, specific_epithet FROM plants LIMIT 10")
print(plant_species)

# Get statistics on water potential measurements
wp_stats <- dbGetQuery(con, "
  SELECT 
    COUNT(*) as count,
    AVG(water_potential_mean) as avg_potential,
    MIN(water_potential_mean) as min_potential,
    MAX(water_potential_mean) as max_potential
  FROM press_chamb_wp
  WHERE water_potential_mean IS NOT NULL
")
print(wp_stats)


## ----dplyr_queries------------------------------------------------------------
# Create references to the tables
press_chamb_wp_tbl <- tbl(con, "press_chamb_wp")
plants_tbl <- tbl(con, "plants")
plots_tbl <- tbl(con, "plots")

# Query using dplyr syntax for water potential by species
wp_by_species <- press_chamb_wp_tbl |>
  inner_join(plants_tbl, by = c("dataset_name", "individual_id")) |>
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
organ_summary <- press_chamb_wp_tbl |>
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
seasonal_patterns <- press_chamb_wp_tbl |>
  # must collect first because substr can't be run by db
  collect() |>
  # Extract month from date (YYYYMMDD format)
  mutate(month = substr(date, 5, 6)) |>
  group_by(month) %>%
  summarize(
    avg_potential = mean(water_potential_mean, na.rm = TRUE),
    sd_potential = sd(water_potential_mean, na.rm = TRUE),
    sample_count = n()
  ) 

print(seasonal_patterns)

# Comparing water potential with soil moisture
soil_wp_comparison <- press_chamb_wp_tbl |>
  # Join with soil moisture data on dataset_name, plot_id, and date
  inner_join(
    tbl(con, "soil_moisture"),
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
  ) %>%
  group_by(swc_shallow_category) %>%
  summarize(
    avg_water_potential = mean(water_potential_mean, na.rm = TRUE),
    sd_water_potential = sd(water_potential_mean, na.rm = TRUE),
    n_observations = n()
  ) %>%
  collect()

print(soil_wp_comparison)

# Analyzing the relationship between environmental variables and water potential
env_wp_comparison <- press_chamb_wp_tbl |>
  # Join with environmental data on dataset_name and date
  inner_join(
    tbl(con, "env_vars"),
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

print(env_wp_comparison)



## ----visualizations, message=FALSE, warning=FALSE, fig.height=8, fig.width=10----
library(ggplot2)
library(lubridate)

# Get water potential data for visualization
wp_time_data <- tbl(con, "press_chamb_wp") |>
  filter(!is.na(water_potential_mean)) |>
  collect() |>
  mutate(date_parsed = ymd(as.character(date)))  # Convert YYYYMMDD to Date

# Get water potential by species
wp_species <- tbl(con, "press_chamb_wp") |>
  inner_join(tbl(con, "plants"), by = c("dataset_name", "individual_id", "plot_id")) %>%
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

# Plot water potential by species
ggplot(wp_species, aes(x = reorder(species, mean_wp), y = mean_wp)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_wp - sd_wp, ymax = mean_wp + sd_wp), width = 0.2) +
  labs(
    title = "Average Water Potential by Species",
    x = "Species",
    y = "Water Potential (MPa)"
  ) +
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal()

# Don't forget to disconnect
dbDisconnect(con, shutdown = TRUE)


## ----combined_measurements----------------------------------------------------
# Connect to database
con <- dbConnect(duckdb::duckdb(), get_db_path())

# Query to combine manual and automated measurements
combined_query <- "
SELECT
  'manual' as measurement_type,
  pc.dataset_name,
  pc.individual_id,
  pc.date,
  pc.time,
  pc.water_potential_mean,
  pc.water_potential_sd,
  p.genus,
  p.specific_epithet
FROM press_chamb_wp pc
JOIN plants p ON pc.dataset_name = p.dataset_name AND pc.individual_id = p.individual_id
WHERE pc.water_potential_mean IS NOT NULL

UNION ALL

SELECT
  'automated' as measurement_type,
  a.dataset_name,
  a.individual_id,
  a.date,
  a.time,
  a.water_potential_mean,
  a.water_potential_sd,
  p.genus,
  p.specific_epithet
FROM auto_wp a
JOIN plants p ON a.dataset_name = p.dataset_name AND a.individual_id = p.individual_id
WHERE a.water_potential_mean IS NOT NULL
LIMIT 1000  -- Limit to avoid memory issues
"

combined_data <- dbGetQuery(con, combined_query)

# Compare manual vs automated measurements
measurement_summary <- combined_data %>%
  group_by(measurement_type) %>%
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
efficient_query <- tbl(con, "press_chamb_wp") %>%
  filter(water_potential_mean < -1.0) %>%
  select(dataset_name, individual_id, date, water_potential_mean) %>%
  collect()

# Disconnect
dbDisconnect(con, shutdown = TRUE)

