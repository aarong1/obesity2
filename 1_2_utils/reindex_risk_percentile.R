# names(current_population )
# 
# names(current_population )[endsWith(suffix = '_percentile', x = names(current_population))]
# 
# [1] "bmi_percentile"                 
# [2] "smoking_percentile"             
# [3] "depression_percentile"          
# [4] "alcohol_percentile"             
# [5] "diet_percentile"                
# [6] "sleep_percentile"               
# [7] "hypertension_percentile"        
# [8] "diabetes_percentile"            
# [9] "cholesterol_percentile"         
# [10] "physical_activity_percentile"   
# [11] "ckd_percentile"                 
# [12] "pad_percentile"                 
# [13] "vte_percentile"                 
# [14] "af_percentile"                  
# [15] "prostate_cancer_percentile"     
# [16] "female_breast_cancer_percentile"
# [17] "uterine_cancer_percentile"      
# [18] "ovarian_cancer_percentile"      
# [19] "osteoarthritis_percentile"      
# [20] "epilepsy_percentile"            
# [21] "morbidity_percentile" 
# 
# frank(ties.method = 'random',osteoarthritis_year_risk)/max(frank(ties.method = 'random',osteoarthritis_year_risk))

#' Reindex Risk Percentiles to Maintain Uniform Distribution

reindex_risk_percentile <- function(dt,avoid=F) {
  
  # Validate input
  if (!data.table::is.data.table(dt)) {
    warning("Input must be a data.table object. Coercing.")
    dt <- as.data.table(dt)
  }
  
  if (nrow(dt) == 0) {
    warning("Input data.table is empty. Returning unchanged.")
    return(invisible(dt))
  }
  
  # Identify all percentile columns
  percentile_cols <- grep("_percentile$", names(dt), value = TRUE)
  
  if (length(percentile_cols) == 0) {
    warning("No columns ending in '_percentile' found. Returning unchanged.")
    return(invisible(dt))
  }
  
  # Reindex all percentile columns in a single operation
  # - frank() ranks with random tie-breaking
  # - Divide by .N to normalize to 0-1
  # - Multiply by 100 and round to get integer percentiles (1-100)
  if(!avoid){
  dt[, (percentile_cols) := lapply(.SD, function(x) {
    if (all(is.na(x))) {
      # Handle all-NA columns
      return(x)
    }
    # Calculate ranks, normalize, scale to 1-100, and round
    data.table::frank(x, ties.method = "random", na.last = "keep") / 
            sum(!is.na(x)) 
  }), .SDcols = percentile_cols]
    
  }else{
    dt[target != TRUE, (percentile_cols) := lapply(.SD, function(x) {
      if (all(is.na(x))) {
        # Handle all-NA columns
        return(x)
      }
      # Calculate ranks, normalize, scale to 1-100, and round
      data.table::frank(x, ties.method = "random", na.last = "keep") / 
        sum(!is.na(x)) 
    }), .SDcols = percentile_cols ]
    }
  
  return(invisible(dt))
}

# current_population
# initial_time_zero_population %>% 
#   # filter(age>30) %>% 
#   rowwise() %>% 
#   filter(asthma_year_risk>0.01) %>%
#   # filter(1==rbinom(1,1,bmi_percentile)) %>%
#   as.data.table() %>% 
#   reindex_risk_percentile()%>%
#   ggplot() +
#   geom_histogram(aes(smoking_percentile))
#   # geom_histogram(aes(qrisk_score))

#' Example Usage and Validation
#' 
#' The following code demonstrates the survival bias problem and validates
#' the solution:
#' 
#' \dontrun{
#' # Before reindexing - distribution may be skewed due to high-risk deaths
#' hist(current_population$bmi_percentile, 
#'      main = "BMI Percentiles Before Reindexing",
#'      xlab = "Percentile", 
#'      breaks = 20)
#' 
#' # Apply reindexing
#' current_population <- reindex_percentile(current_population)
#' 
#' # After reindexing - should be uniform
#' hist(current_population$bmi_percentile, 
#'      main = "BMI Percentiles After Reindexing",
#'      xlab = "Percentile", 
#'      breaks = 20)
#' 
#' # Verify uniform distribution across all percentile columns
#' percentile_cols <- grep("_percentile$", names(current_population), value = TRUE)
#' for (col in percentile_cols) {
#'   ks_test <- ks.test(current_population[[col]], "punif", 1, 100)
#'   cat(sprintf("%s: p-value = %.4f\n", col, ks_test$p.value))
#' }
#' }