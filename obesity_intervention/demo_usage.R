#' =============================================================================
#' DEMONSTRATION OF BMI INTERVENTION FUNCTIONS
#' =============================================================================
#' 
#' Quick examples showing how to use each intervention function
#' 
#' @date 2026-01-04
#' =============================================================================

library(fst)
library(dplyr)
source("obesity_intervention/engine_bmi.R")

# Load a sample of the synthetic population
pop <- read.fst("base_population_w_risk_factors.fst", from = 1, to = 5000)

cat("Starting population:\n")
cat("Total N =", nrow(pop), "\n")
pop %>% count(bmi) %>% mutate(pct = 100 * n / sum(n)) %>% print()


#' =============================================================================
#' FUNCTION 1: reduce_to_percentile()
#' =============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("FUNCTION 1: reduce_to_percentile()\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Example 1.1: Universal intervention - target everyone above 75th percentile
cat("Example 1.1: Universal intervention - reduce top 25% by 10 percentile points\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_1_1 <- reduce_to_percentile(
  current_population = pop,
  target_percentile = 75,
  reduction_amount = 0.10
)
cat("\n")

cbind(count(pop_1_1,bmi),filter(count(pop,bmi),!is.na(bmi))) 
sum(count(pop,bmi)$n)
sum(count(pop_1_1,bmi)$n)

# Example 1.2: Age-targeted intervention
cat("\nExample 1.2: Target older adults - reduce those 65+ above 70th percentile\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_1_2 <- reduce_to_percentile(
  current_population = pop,
  target_percentile = 70,
  reduction_amount = 0.15,
  affected_group = "age >= 65"
)
cat("\n")

# Example 1.3: Gender and age targeted
cat("\nExample 1.3: Target middle-aged men - reduce males 40-60 above 80th percentile\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_1_3 <- reduce_to_percentile(
  current_population = pop,
  target_percentile = 80,
  reduction_amount = 0.20,
  affected_group = "sex == 'Male' & age >= 40 & age <= 60"
)
cat("\n")

# Example 1.4: Target deprived areas with custom thresholds
cat("\nExample 1.4: Intensive intervention in most deprived areas\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_1_4 <- reduce_to_percentile(
  current_population = pop,
  target_percentile = 60,
  reduction_amount = 0.25,
  affected_group = "deprivation == 'Most deprived'",
  bmi_thresholds = c(normal = 0.45, overweight = 0.70, obese = 1.0)  # Custom cutoffs
)
cat("\n")


#' =============================================================================
#' FUNCTION 2: reduce_obese_to_overweight()
#' =============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("FUNCTION 2: reduce_obese_to_overweight()\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Example 2.1: Simple universal intervention
cat("Example 2.1: Universal - move 20% of all obese individuals to overweight\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_2_1 <- reduce_obese_to_overweight(
  current_population = pop,
  proportion = 0.20,
  seed = 123
)
cat("\n")

# Example 2.2: Target women aged 50+
cat("\nExample 2.2: Target postmenopausal women - move 30% of obese women 50+\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_2_2 <- reduce_obese_to_overweight(
  current_population = pop,
  proportion = 0.30,
  affected_group = "sex == 'Female' & age >= 50",
  seed = 456
)
cat("\n")

# Example 2.3: Urban areas with custom percentile range
cat("\nExample 2.3: Urban intervention - move 25% to lower overweight range\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_2_3 <- reduce_obese_to_overweight(
  current_population = pop,
  proportion = 0.25,
  affected_group = "geo == 'Urban'",
  new_percentile_range = c(0.51, 0.60),  # Lower end of overweight range
  seed = 789
)
cat("\n")

# Example 2.4: Multiple targeting criteria
cat("\nExample 2.4: Complex targeting - obese individuals in specific demographics\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_2_4 <- reduce_obese_to_overweight(
  current_population = pop,
  proportion = 0.35,
  affected_group = "age >= 30 & age <= 70 & deprivation %in% c('Most deprived', 'Quintile 2')",
  new_percentile_range = c(0.55, 0.70),
  seed = 101
)
cat("\n")


#' =============================================================================
#' FUNCTION 3: reduce_overweight_obese()
#' =============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("FUNCTION 3: reduce_overweight_obese()\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Example 3.1: Universal differential intervention
cat("Example 3.1: Universal - 15% overweight→normal, 25% obese→overweight\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_3_1 <- reduce_overweight_obese(
  current_population = pop,
  overweight_reduction = 0.15,
  obese_reduction = 0.25,
  seed = 111
)
cat("\n")

# Example 3.2: Age-targeted with gradual shift (default)
cat("\nExample 3.2: Target adults 40-65 with gradual percentile shifts\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_3_2 <- reduce_overweight_obese(
  current_population = pop,
  overweight_reduction = 0.20,
  obese_reduction = 0.30,
  affected_group = "age >= 40 & age <= 65",
  maintain_gradual = TRUE,  # Gradual percentile shift
  seed = 222
)
cat("\n")

# Example 3.3: Categorical jump approach
cat("\nExample 3.3: Intensive intervention with categorical jumps (no gradual shift)\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_3_3 <- reduce_overweight_obese(
  current_population = pop,
  overweight_reduction = 0.30,
  obese_reduction = 0.40,
  maintain_gradual = FALSE,  # Jump to middle of target category
  seed = 333
)
cat("\n")

# Example 3.4: Gender-specific intervention
cat("\nExample 3.4: Men only - different rates for each BMI category\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_3_4 <- reduce_overweight_obese(
  current_population = pop,
  overweight_reduction = 0.10,
  obese_reduction = 0.35,
  affected_group = "sex == 'Male'",
  seed = 444
)
cat("\n")

# Example 3.5: Deprivation-targeted intensive intervention
cat("\nExample 3.5: Most deprived areas - intensive dual-category intervention\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
pop_3_5 <- reduce_overweight_obese(
  current_population = pop,
  overweight_reduction = 0.40,
  obese_reduction = 0.50,
  affected_group = "deprivation == 'Most deprived'",
  maintain_gradual = TRUE,
  seed = 555
)
cat("\n")


#' =============================================================================
#' FUNCTION 4: validate_intervention() - Bonus examples
#' =============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("FUNCTION 4: validate_intervention()\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

# Example 4.1: Validate simple intervention
cat("Example 4.1: Validate a single intervention\n")
cat(paste(rep("-", 80), collapse = ""), "\n")
validation_1 <- validate_intervention(
  pre_intervention = pop,
  post_intervention = pop_1_1
)
cat("\nValidation passed:", validation_1$all_valid, "\n\n")

# Example 4.2: Validate cascaded interventions
cat("\nExample 4.2: Validate multiple cascaded interventions\n")
cat(paste(rep("-", 80), collapse = ""), "\n")

# Apply cascade
pop_cascade <- pop %>%
  reduce_to_percentile(target_percentile = 80, reduction_amount = 0.10) %>%
  reduce_obese_to_overweight(proportion = 0.15, seed = 999) %>%
  reduce_overweight_obese(overweight_reduction = 0.10, obese_reduction = 0.20, seed = 888)

validation_2 <- validate_intervention(
  pre_intervention = pop,
  post_intervention = pop_cascade
)
cat("\nCascaded validation passed:", validation_2$all_valid, "\n\n")


#' =============================================================================
#' COMPARATIVE SUMMARY
#' =============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("COMPARATIVE SUMMARY: Obesity Prevalence Changes\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

get_obesity_stats <- function(df, label) {
  df %>%
    summarise(
      Scenario = label,
      N = n(),
      `Obese %` = round(100 * mean(bmi == "obese"), 1),
      `Overweight %` = round(100 * mean(bmi == "overweight"), 1),
      `Normal %` = round(100 * mean(bmi == "normal"), 1)
    )
}

summary <- bind_rows(
  get_obesity_stats(pop, "Original Population"),
  get_obesity_stats(pop_1_1, "Ex 1.1: Top 25% reduced"),
  get_obesity_stats(pop_2_1, "Ex 2.1: 20% obese→overweight"),
  get_obesity_stats(pop_3_1, "Ex 3.1: Differential rates"),
  get_obesity_stats(pop_3_3, "Ex 3.3: Intensive categorical"),
  get_obesity_stats(pop_cascade, "Cascaded interventions")
)

print(summary)


#' =============================================================================
#' PRACTICAL USE CASE EXAMPLE
#' =============================================================================

cat("\n\n" , paste(rep("=", 80), collapse = ""), "\n")
cat("PRACTICAL USE CASE: Simulate a 5-year obesity reduction program\n")
cat(paste(rep("=", 80), collapse = ""), "\n\n")

cat("Program design:\n")
cat("  - Year 1: Screen high-risk (>80th percentile), reduce by 10 points\n")
cat("  - Year 2: Intensive intervention for obese (30% move to overweight)\n")
cat("  - Year 3: Community-wide differential intervention\n")
cat("  - Year 4: Maintenance with smaller reduction\n")
cat("  - Year 5: Final push with targeted approach\n\n")

# Year 1
cat("Year 1: Screening and initial intervention\n")
pop_year1 <- reduce_to_percentile(pop, target_percentile = 80, reduction_amount = 0.10)

# Year 2
cat("\nYear 2: Clinical intervention for obesity\n")
pop_year2 <- reduce_obese_to_overweight(pop_year1, proportion = 0.30, seed = 2024)

# Year 3
cat("\nYear 3: Community-wide program\n")
pop_year3 <- reduce_overweight_obese(pop_year2, 
                                     overweight_reduction = 0.20, 
                                     obese_reduction = 0.25,
                                     seed = 2025)

# Year 4
cat("\nYear 4: Maintenance phase\n")
pop_year4 <- reduce_to_percentile(pop_year3, target_percentile = 75, reduction_amount = 0.05)

# Year 5
cat("\nYear 5: Final targeted intervention\n")
pop_year5 <- reduce_overweight_obese(pop_year4,
                                     overweight_reduction = 0.15,
                                     obese_reduction = 0.20,
                                     affected_group = "age >= 40",
                                     seed = 2026)

cat("\n\nFinal validation:\n")
validate_intervention(pop, pop_year5)

cat("\n\n5-Year Program Results:\n")
program_results <- bind_rows(
  get_obesity_stats(pop, "Baseline"),
  get_obesity_stats(pop_year1, "Year 1"),
  get_obesity_stats(pop_year2, "Year 2"),
  get_obesity_stats(pop_year3, "Year 3"),
  get_obesity_stats(pop_year4, "Year 4"),
  get_obesity_stats(pop_year5, "Year 5")
)
print(program_results)

# Calculate total impact
obesity_reduction <- round(
  100 * mean(pop$bmi == "obese") - 100 * mean(pop_year5$bmi == "obese"), 
  1
)
cat("\n\nTotal obesity prevalence reduction: ", obesity_reduction, " percentage points\n", sep = "")
cat("Relative reduction: ", 
    round(100 * (1 - mean(pop_year5$bmi == "obese") / mean(pop$bmi == "obese")), 1),
    "%\n\n", sep = "")

cat(paste(rep("=", 80), collapse = ""), "\n")
cat("DEMONSTRATION COMPLETE\n")
cat(paste(rep("=", 80), collapse = ""), "\n")
