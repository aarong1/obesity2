# =============================================================================
# Generate 3D Cancer Prevalence Populations (Age × HSCT × Sex)
# =============================================================================
# This script generates synthetic populations with site-specific cancer
# prevalence using 3-dimensional IPF (age, HSCT, and sex)
# =============================================================================

library(tidyverse)
source("cancer_ipf_functions_3d.R")

# Load cleaned data (wide format with male/female columns)
age_data_wide <- readRDS("data/cancer_prevalence_by_age_wide.rds")
hsct_data_wide <- readRDS("data/cancer_prevalence_by_hsct_wide.rds")

# Preview data
cat("=== Age Data (Wide Format) ===\n")
print(head(age_data_wide))
cat("\nColumns:\n")
print(names(age_data_wide))

cat("\n\n=== HSCT Data (Wide Format) ===\n")
print(head(hsct_data_wide))
cat("\nColumns:\n")
print(names(hsct_data_wide))

# -----------------------------------------------------------------------------
# Generate 3D Populations (Age × HSCT × Sex) ---------------------------------
# -----------------------------------------------------------------------------

cat("\n\n=== Generating 3D Populations ===\n")

# 10-year prevalence (NO ROUNDING - keep fractional prevalence for accuracy)
cat("\n[1/2] Processing 10-year prevalence...\n")
pop_10yr <- populate_all_cancers_3d(
  age_data_wide = age_data_wide,
  hsct_data_wide = hsct_data_wide,
  period = "10-year",
  integer_output = FALSE  # Keep fractional prevalence - critical for small counts
)

# 25-year prevalence (NO ROUNDING - keep fractional prevalence for accuracy)
cat("\n[2/2] Processing 25-year prevalence...\n")
pop_25yr <- populate_all_cancers_3d(
  age_data_wide = age_data_wide,
  hsct_data_wide = hsct_data_wide,
  period = "25-year",
  integer_output = FALSE  # Keep fractional prevalence - critical for small counts
)

# Combine all results
all_populations <- bind_rows(pop_10yr, pop_25yr)

# -----------------------------------------------------------------------------
# Save Outputs ----------------------------------------------------------------
# -----------------------------------------------------------------------------

cat("\n\n=== Saving Results ===\n")

# Create outputs directory if needed
if (!dir.exists("outputs")) {
  dir.create("outputs")
}

# Save combined results
saveRDS(all_populations, "outputs/cancer_populations_3d_all.rds")
write_csv(all_populations, "outputs/cancer_populations_3d_all.csv")

# Save by period
saveRDS(pop_10yr, "outputs/cancer_population_3d_10yr.rds")
saveRDS(pop_25yr, "outputs/cancer_population_3d_25yr.rds")

write_csv(pop_10yr, "outputs/cancer_population_3d_10yr.csv")
write_csv(pop_25yr, "outputs/cancer_population_3d_25yr.csv")

# -----------------------------------------------------------------------------
# Summary Statistics ----------------------------------------------------------
# -----------------------------------------------------------------------------

cat("\n\n=== Summary Statistics ===\n")

summary_stats <- all_populations %>%
  group_by(cancer_site, period, sex) %>%
  summarise(
    total_prevalence = sum(prevalence),
    n_age_groups = n_distinct(age),
    n_hscts = n_distinct(HSCT),
    .groups = "drop"
  )

print(summary_stats)

cat("\n\n=== Dimension Check ===\n")
cat(sprintf("Total rows: %d\n", nrow(all_populations)))
cat(sprintf("Unique cancer sites: %d\n", n_distinct(all_populations$cancer_site)))
cat(sprintf("Unique periods: %d\n", n_distinct(all_populations$period)))
cat(sprintf("Unique age groups: %d\n", n_distinct(all_populations$age)))
cat(sprintf("Unique HSCTs: %d\n", n_distinct(all_populations$HSCT)))
cat(sprintf("Unique sexes: %d\n", n_distinct(all_populations$sex)))

# Expected: 21 cancer sites × 2 periods × 4 age groups × 5 HSCTs × 2 sexes = 1680 rows
expected_rows <- n_distinct(all_populations$cancer_site) * 
                 n_distinct(all_populations$period) * 
                 n_distinct(all_populations$age) *
                 n_distinct(all_populations$HSCT) *
                 n_distinct(all_populations$sex)

cat(sprintf("\nExpected rows: %d\n", expected_rows))
cat(sprintf("Match: %s\n", ifelse(nrow(all_populations) == expected_rows, "✓ YES", "✗ NO")))

# -----------------------------------------------------------------------------
# Example: Compare margins for one cancer site -------------------------------
# -----------------------------------------------------------------------------

cat("\n\n=== Example: All Cancers 10-year (Margin Validation) ===\n")

example <- all_populations %>%
  filter(cancer_site == "All Cancers", period == "10-year")

# Check age × sex margin
age_sex_check <- example %>%
  group_by(age, sex) %>%
  summarise(fitted = sum(prevalence), .groups = "drop") %>%
  pivot_wider(names_from = sex, values_from = fitted)

cat("\nFitted Age × Sex Margin:\n")
print(age_sex_check)

# Compare to original
original_age_sex <- age_data_wide %>%
  filter(cancer_site == "All Cancers") %>%
  select(age_group, Male = male_10yr, Female = female_10yr)

cat("\nOriginal Age × Sex Margin:\n")
print(original_age_sex)

# Check HSCT × sex margin
hsct_sex_check <- example %>%
  group_by(HSCT, sex) %>%
  summarise(fitted = sum(prevalence), .groups = "drop") %>%
  pivot_wider(names_from = sex, values_from = fitted)

cat("\nFitted HSCT × Sex Margin:\n")
print(hsct_sex_check)

# Compare to original
original_hsct_sex <- hsct_data_wide %>%
  filter(cancer_site == "All Cancers") %>%
  select(hsct, Male = male_10yr, Female = female_10yr)

cat("\nOriginal HSCT × Sex Margin:\n")
print(original_hsct_sex)

cat("\n\n✓ Script completed successfully!\n")
cat("Output files:\n")
cat("  - outputs/cancer_populations_3d_all.rds/.csv\n")
cat("  - outputs/cancer_population_3d_10yr.rds/.csv\n")
cat("  - outputs/cancer_population_3d_25yr.rds/.csv\n")
