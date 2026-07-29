#' =============================================================================
#' TEST SCRIPT FOR BMI INTERVENTION ENGINE
#' =============================================================================
#' 
#' Demonstrates usage of obesity intervention functions on synthetic population
#' 
#' @date 2026-01-04
#' =============================================================================

library(fst)
library(dplyr)
source("obesity_intervention/engine_bmi.R")

# Load synthetic population
cat("Loading synthetic population...\n")
pop <- read.fst("base_population_w_risk_factors.fst", from = 1, to = 10000)

cat("\nInitial population size:", nrow(pop), "\n")
cat("\nInitial BMI distribution:\n")
print(pop %>% count(bmi) %>% mutate(pct = 100 * n / sum(n)))

# Save original for comparison
pop_original <- pop

#' =============================================================================
#' TEST 1: reduce_to_percentile
#' =============================================================================

cat("\n\n=== TEST 1: Reduce individuals above 75th percentile ===\n")
pop_test1 <- reduce_to_percentile(
  pop_original,
  target_percentile = 75,
  reduction_amount = 0.10
)

cat("\nPost-intervention BMI distribution:\n")
print(pop_test1 %>% count(bmi) %>% mutate(pct = 100 * n / sum(n)))


#' =============================================================================
#' TEST 2: reduce_obese_to_overweight with targeting
#' =============================================================================

cat("\n\n=== TEST 2: Move 30% of obese individuals to overweight ===\n")
cat("Target: Older adults (age >= 50)\n\n")

pop_test2 <- reduce_obese_to_overweight(
  pop_original,
  proportion = 0.30,
  affected_group = "age >= 50",
  seed = 123
)

# Check by age group
cat("\nBMI distribution by age group:\n")
pop_test2 %>%
  mutate(age_group = cut(age, breaks = c(0, 50, 110), labels = c("<50", "50+"))) %>%
  count(age_group, bmi) %>%
  group_by(age_group) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  print()


#' =============================================================================
#' TEST 3: reduce_overweight_obese with differential rates
#' =============================================================================

cat("\n\n=== TEST 3: Differential reduction rates ===\n")
cat("15% overweight→normal, 25% obese→overweight\n\n")

pop_test3 <- reduce_overweight_obese(
  pop_original,
  overweight_reduction = 0.15,
  obese_reduction = 0.25,
  seed = 123
)


#' =============================================================================
#' TEST 4: Targeted intervention for deprived areas
#' =============================================================================

cat("\n\n=== TEST 4: Intensive intervention in most deprived areas ===\n\n")

pop_test4 <- reduce_overweight_obese(
  pop_original,
  overweight_reduction = 0.30,
  obese_reduction = 0.40,
  affected_group = "deprivation == 'Most deprived'",
  seed = 123
)

# Compare by deprivation
cat("\nBMI distribution by deprivation:\n")
comparison <- bind_rows(
  pop_original %>% 
    filter(deprivation == "Most deprived") %>%
    count(bmi) %>% 
    mutate(pct = 100 * n / sum(n), stage = "Before"),
  pop_test4 %>% 
    filter(deprivation == "Most deprived") %>%
    count(bmi) %>% 
    mutate(pct = 100 * n / sum(n), stage = "After")
)
print(comparison)


#' =============================================================================
#' TEST 5: Validation
#' =============================================================================

cat("\n\n=== TEST 5: Validate intervention results ===\n\n")

validation_results <- validate_intervention(pop_original, pop_test3)

cat("\nAll validation checks passed:", validation_results$all_valid, "\n")


#' =============================================================================
#' TEST 6: Cascading interventions
#' =============================================================================

cat("\n\n=== TEST 6: Apply multiple interventions sequentially ===\n\n")

# Start with original
pop_cascade <- pop_original

# Step 1: Target high percentiles
cat("Step 1: Reduce top 20% by 15 percentile points\n")
pop_cascade <- reduce_to_percentile(
  pop_cascade,
  target_percentile = 80,
  reduction_amount = 0.15
)

# Step 2: Move obese to overweight
cat("\n\nStep 2: Move 20% of remaining obese to overweight\n")
pop_cascade <- reduce_obese_to_overweight(
  pop_cascade,
  proportion = 0.20,
  seed = 123
)

# Final distribution
cat("\n\nFinal BMI distribution after cascade:\n")
print(pop_cascade %>% count(bmi) %>% mutate(pct = 100 * n / sum(n)))

# Validate final result
cat("\n\nValidating cascaded interventions:\n")
validate_intervention(pop_original, pop_cascade)


#' =============================================================================
#' SUMMARY
#' =============================================================================

cat("\n\n=== SUMMARY: Change in obesity prevalence across tests ===\n\n")

obesity_prev <- function(df, label) {
  df %>%
    summarise(
      test = label,
      n_obese = sum(bmi == "obese"),
      prev_obese = 100 * mean(bmi == "obese"),
      n_overweight = sum(bmi == "overweight"),
      prev_overweight = 100 * mean(bmi == "overweight")
    )
}

summary_table <- bind_rows(
  obesity_prev(pop_original, "Original"),
  obesity_prev(pop_test1, "Test 1: Percentile reduction"),
  obesity_prev(pop_test2, "Test 2: Obese→Overweight (age 50+)"),
  obesity_prev(pop_test3, "Test 3: Differential rates"),
  obesity_prev(pop_test4, "Test 4: Target deprived areas"),
  obesity_prev(pop_cascade, "Test 6: Cascaded interventions")
)

print(summary_table)

cat("\n\n=== All tests completed successfully! ===\n")
