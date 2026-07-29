# =============================================================================
# Populate Site-Specific Cancer Prevalence using IPF
# =============================================================================
# Main script to populate synthetic population with multiple site-specific
# cancer prevalence using Iterative Proportional Fitting (IPF) to combine
# marginal distributions for age, sex, and HSCT
# 
# Similar approach to prostate cancer model
# =============================================================================

library(tidyverse)
library(mipfp)

# Load functions and data
source("cancer_ipf_functions.R")

message("================================================================================")
message("Populating Site-Specific Cancer Prevalence Using IPF")
message("================================================================================\n")

# -----------------------------------------------------------------------------
# Load Cleaned Data -----------------------------------------------------------
# -----------------------------------------------------------------------------

# Check if data exists, if not import it
if (!file.exists("data/cancer_prevalence_by_age_long.rds") || 
    !file.exists("data/cancer_prevalence_by_hsct_long.rds")) {
  message("Cleaned data not found. Importing cancer data...")
  source("import_all_cancer_data.R")
  message("")
}

age_data <- readRDS("data/cancer_prevalence_by_age_long.rds")
hsct_data <- readRDS("data/cancer_prevalence_by_hsct_long.rds")

message(sprintf("Loaded data for %d cancer sites (age) and %d sites (HSCT)\n",
                length(unique(age_data$cancer_site)),
                length(unique(hsct_data$cancer_site))))

# -----------------------------------------------------------------------------
# Populate Cancer Prevalence --------------------------------------------------
# -----------------------------------------------------------------------------

# Get list of cancer sites that have both age and HSCT data
sites_with_both <- intersect(unique(age_data$cancer_site), 
                             unique(hsct_data$cancer_site))

message(sprintf("Sites with both age and HSCT data: %d\n", length(sites_with_both)))

# Choose parameters for population
sex_choice <- "Male"         # "Male", "Female", or "All"
period_choice <- "10-year"   # "10-year" or "25-year"
use_integers <- TRUE         # Round to integers preserving margins

message(sprintf("Parameters: sex=%s, period=%s, integers=%s\n",
                sex_choice, period_choice, use_integers))

# Populate all cancer sites
cancer_population <- populate_all_cancers(
  age_data_long = age_data,
  hsct_data_long = hsct_data,
  sex = sex_choice,
  period = period_choice,
  integer_output = use_integers
)

message(sprintf("\nSuccessfully populated %d cancer sites", 
                length(unique(cancer_population$cancer_site))))

# -----------------------------------------------------------------------------
# Generate Populations for All Sex/Period Combinations -----------------------
# -----------------------------------------------------------------------------

message("\n================================================================================")
message("Generating populations for all sex/period combinations...")
message("================================================================================\n")

# Define combinations
combinations <- expand_grid(
  sex = c("Male", "Female", "All"),
  period = c("10-year", "25-year")
)

# Generate populations
all_populations <- combinations %>%
  pmap_dfr(function(sex, period) {
    message(sprintf("Processing: sex=%s, period=%s", sex, period))
    result <- tryCatch({
      populate_all_cancers(
        age_data_long = age_data,
        hsct_data_long = hsct_data,
        sex = sex,
        period = period,
        integer_output = TRUE
      )
    }, error = function(e) {
      message(sprintf("  Error: %s", e$message))
      NULL
    })
    return(result)
  })

message(sprintf("\nGenerated %d total population records", nrow(all_populations)))

# -----------------------------------------------------------------------------
# Save Results ----------------------------------------------------------------
# -----------------------------------------------------------------------------

# Save individual sex populations separately for easy access
male_10yr <- all_populations %>% filter(sex == "Male", period == "10-year")
female_10yr <- all_populations %>% filter(sex == "Female", period == "10-year")
all_10yr <- all_populations %>% filter(sex == "All", period == "10-year")

male_25yr <- all_populations %>% filter(sex == "Male", period == "25-year")
female_25yr <- all_populations %>% filter(sex == "Female", period == "25-year")
all_25yr <- all_populations %>% filter(sex == "All", period == "25-year")

# Save all
saveRDS(all_populations, "outputs/cancer_populations_all.rds")
write_csv(all_populations, "outputs/cancer_populations_all.csv")

# Save by sex/period
saveRDS(male_10yr, "outputs/cancer_population_male_10yr.rds")
saveRDS(female_10yr, "outputs/cancer_population_female_10yr.rds")
saveRDS(all_10yr, "outputs/cancer_population_all_10yr.rds")

saveRDS(male_25yr, "outputs/cancer_population_male_25yr.rds")
saveRDS(female_25yr, "outputs/cancer_population_female_25yr.rds")
saveRDS(all_25yr, "outputs/cancer_population_all_25yr.rds")

message("\n================================================================================")
message("Saved Outputs:")
message("================================================================================")
message("  - outputs/cancer_populations_all.rds/.csv (all combinations)")
message("  - outputs/cancer_population_male_10yr.rds")
message("  - outputs/cancer_population_female_10yr.rds")
message("  - outputs/cancer_population_all_10yr.rds")
message("  - outputs/cancer_population_male_25yr.rds")
message("  - outputs/cancer_population_female_25yr.rds")
message("  - outputs/cancer_population_all_25yr.rds")

# -----------------------------------------------------------------------------
# Summary Statistics ----------------------------------------------------------
# -----------------------------------------------------------------------------

message("\n================================================================================")
message("Summary Statistics")
message("================================================================================\n")

# Overall summary
summary_stats <- all_populations %>%
  group_by(cancer_site, sex, period) %>%
  summarise(
    total_prevalence = sum(prevalence),
    n_age_groups = n_distinct(age),
    n_hsct = n_distinct(HSCT),
    .groups = "drop"
  ) %>%
  arrange(desc(total_prevalence))

message("Top 10 cancer sites by prevalence (All persons, 10-year):")
top10 <- summary_stats %>%
  filter(sex == "All", period == "10-year") %>%
  head(10)

print(top10, n = 10)

# Age distribution example (Prostate cancer, Males, 10-year)
if ("Prostate Cancer" %in% unique(all_populations$cancer_site)) {
  message("\n\nExample: Prostate Cancer (Males, 10-year) by Age:")
  prostate_age <- all_populations %>%
    filter(cancer_site == "Prostate Cancer", sex == "Male", period == "10-year") %>%
    group_by(age) %>%
    summarise(prevalence = sum(prevalence), .groups = "drop") %>%
    mutate(percentage = round(100 * prevalence / sum(prevalence), 1))
  
  print(prostate_age, n = Inf)
}

# HSCT distribution example
message("\n\nExample: Lung Cancer (All, 10-year) by HSCT:")
lung_hsct <- all_populations %>%
  filter(cancer_site == "Lung Cancer", sex == "All", period == "10-year") %>%
  group_by(HSCT) %>%
  summarise(prevalence = sum(prevalence), .groups = "drop") %>%
  mutate(percentage = round(100 * prevalence / sum(prevalence), 1)) %>%
  arrange(desc(prevalence))

print(lung_hsct, n = Inf)

message("\n================================================================================")
message("Population generation complete!")
message("================================================================================")
