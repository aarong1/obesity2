# Example: How to Access and Analyze Past Populations Database
# This script demonstrates how to use the evaluation functions to analyze
# the population data stored in the DuckDB database

library(duckdb)
library(tidyverse)
source('./main/main_duckdb.R')

names(current_population)[!names(current_population)%in%dbListFields(con, tbl_name_current_simulation)]
dbListFields(con, tbl_name_current_simulation)[!dbListFields(con, tbl_name_current_simulation)%in%names(current_population)]

# 1. Connect to the database ----
# (read-only mode by default for safety)

# con <- connect_to_database(db_path = "past_populations_db/past_populations.duckdb")
con <- dbConnect(duckdb::duckdb(), dbdir = 'past_populations_db/past_populations.duckdb', read_only = F)


dbListTables(con)
dbListFields(con, "past_populations")
get_table_schema(con, "past_populations")
  
# for( i in dbListTables(con)) {
# dbExecute(con, paste0('DROP TABLE IF EXISTS "',i,'";'))  # Set cache size to 2MB
# }

dbDisconnect(con, shutdown = T)
duckdb::duckdb_shutdown(duckdb())


pp <- extract_population_data(con)

# 2. Get basic database summary ----
cat("\n=== Database Summary ===\n")
summary <- get_db_summary(con)
print(summary)

# 3. Get population counts by run, time, and year ----
cat("\n=== Population Counts ===\n")
pop_counts <- get_population_counts(con)
print(head(pop_counts, 20))

# 4. Analyze specific morbidity prevalence ----
cat("\n=== Stroke Prevalence Over Time ===\n")
stroke_prevalence <- get_morbidity_prevalence(con, "stroke")
print(stroke_prevalence)

# Plot prevalence
ggplot(stroke_prevalence, aes(x = year, y = prevalence_pct, color = factor(run))) +
  geom_line() +
  labs(title = "Stroke Prevalence Over Time by Run",
       x = "Year", y = "Prevalence (%)", color = "Run") +
  theme_minimal()

# 5. Get demographic summary ----
cat("\n=== Demographic Summary ===\n")
demographics <- get_demographic_summary(con)
print(head(demographics, 20))

# 6. Compare intervention vs non-intervention ----
cat("\n=== Intervention Comparison (Diabetes) ===\n")
intervention_comparison <- compare_interventions(con, "diabetes")
print(intervention_comparison)

# Plot intervention comparison
ggplot(intervention_comparison, aes(x = year, y = prevalence_pct, color = intervention)) +
  geom_line(size = 1) +
  labs(title = "Diabetes Prevalence: Intervention vs Non-Intervention",
       x = "Year", y = "Prevalence (%)", color = "Group") +
  theme_minimal()

# 7. Stratified analysis (e.g., by sex) ----
cat("\n=== Stroke Prevalence Stratified by Sex ===\n")
stroke_by_sex <- get_stratified_prevalence(con, "stroke", "sex")
print(head(stroke_by_sex, 20))

# 8. Get death statistics ----
cat("\n=== Death Statistics ===\n")
death_stats <- get_death_statistics(con)
print(death_stats)

# 9. Multimorbidity analysis ----
cat("\n=== Multimorbidity Analysis ===\n")
morbidities <- c("stroke", "diabetes", "chd", "hypertension", "chronic_kidney_disease")
multimorbidity <- get_multimorbidity_counts(con, morbidities)
print(head(multimorbidity, 30))

# Plot multimorbidity distribution
multimorbidity_avg <- multimorbidity %>%
  group_by(num_conditions, year) %>%
  summarize(avg_percentage = mean(percentage), .groups = "drop")

ggplot(multimorbidity_avg, aes(x = factor(num_conditions), y = avg_percentage, fill = factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Multimorbidity",
       x = "Number of Conditions", y = "Percentage of Population (%)", fill = "Year") +
  theme_minimal()

# 10. Extract specific data subset ----
cat("\n=== Extract Specific Runs and Years ===\n")
# Extract runs 1-3 for year 2023
subset_data <- extract_population_data(con, runs = c(1, 2, 3), years = c(2023))
cat(sprintf("Extracted %d rows\n", nrow(subset_data)))
print(head(subset_data))

# 11. Custom SQL query ----
cat("\n=== Custom Query: Average Age by Year ===\n")
custom_query <- "
  SELECT 
    year,
    ROUND(AVG(age), 1) as avg_age,
    COUNT(*) as population
  FROM past_populations
  GROUP BY year
  ORDER BY year
"
custom_result <- dbGetQuery(con, custom_query)
print(custom_result)

# 12. Disconnect from database ----
dbDisconnect(con, shutdown = TRUE)
cat("\nDatabase connection closed.\n")
