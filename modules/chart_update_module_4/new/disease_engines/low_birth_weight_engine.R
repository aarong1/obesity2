

p=0.062
# 2023 figure for NI in RUAG childrens health datafile



apply_low_birth_weight_risk <- function(input_population) {
  p=0.07 # stable since 2012
  
  input_population <- as.data.table(input_population)

  year = max(input_population$year)
  
  input_population[,low_birth_weight := ifelse(age ==0 & sample(replace = T,
                                                     x = c(T,F),
                                                     prob = c(p,1-p),
                                                     size = nrow(input_population)),
                                              year,
                                              0)] 
  input_population[is.na(low_birth_weight), low_birth_weight := 0]
  
  # Return the modified dataset
  return(input_population)
}


sum(initial_time_zero_population$low_birth_weight!=0)
sum(initial_time_zero_population$stroke!=0)

apply_low_birth_weight_risk_w_pollution <- function(input_population) {
  p=0.07 # stable since 2012
  
  input_population <- as.data.table(input_population)
  
  year = max(input_population$year)
  
  input_population[,low_birth_weight := ifelse(age ==0 & sample(replace = T,
                                                                x = c(T,F),
                                                                prob = c(p,1-p),
                                                                size = nrow(input_population)),
                                               year,
                                               0)] 
  input_population[is.na(low_birth_weight), low_birth_weight := 0]
  
  # Return the modified dataset
  return(input_population)
}

 
 
 # 
 # Effect on low birth weight: OR ≈ 1.39 per 1 µg/m³ (placeholder)


  # --- New functions: LBW risk assigned to mother based on PM3.5 exposure ---

  library(data.table)

  # Compute relative risk multiplier from PM exposure
  # rr_per_10ugm3: multiplicative RR per 10 µg/m³ increase
  # baseline_pm: theoretical minimum/benchmark PM level (µg/m³)
  lbw_rr_from_pm35 <- function(pm_value, rr_per_10ugm3 = 1.10, baseline_pm = 5) {
    # Continuous log-linear scaling
    rr <- exp(log(rr_per_10ugm3) * (pm_value - baseline_pm) / 10)
    return(rr)
  }

  # Apply LBW risk to mothers, based on their PM3.5 exposure.
  # - mothers_dt: data.frame/data.table with mother attributes
  # - pm_col: column name for PM3.5 (defaults to "pm3_5"); falls back to "pm25" if not found
  # - baseline_risk: numeric baseline LBW probability (e.g., 0.07), or
  # - baseline_risk_col: name of column providing per-row baseline probabilities (optional)
  # Returns the same table with a new column `lbw_risk_pm`.
  apply_lbw_risk_pm35 <- function(
    mothers_dt,
    pm_col = "pm3_5",
    baseline_risk = 0.07,
    baseline_risk_col = NULL,
    rr_per_10ugm3 = 1.10,
    baseline_pm = 5,
    output_col = "lbw_risk_pm",
    only_childbearing_females = TRUE
  ) {
    dt <- as.data.table(mothers_dt)

    # Determine PM column, with fallback
    pm_use_col <- pm_col
    if (!(pm_use_col %in% names(dt))) {
      if ("pm25" %in% names(dt)) {
        pm_use_col <- "pm25"
      } else {
        stop("PM column not found: ", pm_col, " and fallback 'pm25' also missing.")
      }
    }

    # Target subset: females 15–49 by default (approx. childbearing ages)
    if (only_childbearing_females) {
      if ("sex" %in% names(dt) && "age" %in% names(dt)) {
        dt[, target_row := (sex %in% c("Females", "Female")) & age >= 15 & age <= 49]
      } else {
        dt[, target_row := TRUE]
      }
    } else {
      dt[, target_row := TRUE]
    }

    # Baseline risk per row
    if (!is.null(baseline_risk_col)) {
      if (!(baseline_risk_col %in% names(dt))) stop("baseline_risk_col not found: ", baseline_risk_col)
      dt[, baseline_prob := get(baseline_risk_col)]
    } else {
      dt[, baseline_prob := baseline_risk]
    }

    # Compute RR factor and adjusted probability for target rows
    dt[target_row == TRUE, rr_factor := lbw_rr_from_pm35(get(pm_use_col), rr_per_10ugm3, baseline_pm)]
    dt[target_row == TRUE, (output_col) := pmin(pmax(baseline_prob * rr_factor, 0), 1)]

    # For non-target rows, carry baseline probability if output requested
    dt[target_row == FALSE, (output_col) := pmin(pmax(baseline_prob, 0), 1)]

    # Cleanup
    dt[, c("target_row", "baseline_prob", "rr_factor") := NULL]

    return(as.data.frame(dt))
  }

  # Simulate LBW outcome for births using the computed risk column.
  # - risk_col: name of probability column produced by apply_lbw_risk_pm35
  # - outcome_col: column to store simulated outcome (logical)
  simulate_lbw_outcome_pm35 <- function(
    mothers_dt,
    risk_col = "lbw_risk_pm",
    outcome_col = "lbw_outcome"
  ) {
    dt <- as.data.table(mothers_dt)
    if (!(risk_col %in% names(dt))) stop("risk_col not found: ", risk_col)
    dt[, (outcome_col) := runif(.N) < get(risk_col)]
    return(as.data.frame(dt))
  }

  # Example usage (runs only in interactive sessions)
  if (interactive()) {
    ex <- data.table(
      id = 1:6,
      sex = c("Females", "Females", "Females", "Males", "Females", "Females"),
      age = c(22, 35, 48, 33, 16, 52),
      pm3_5 = c(6, 8, 12, 7, 5, 9)
    )

    ex2 <- apply_lbw_risk_pm35(ex, baseline_risk = 0.07, rr_per_10ugm3 = 1.10, baseline_pm = 5)
    ex3 <- simulate_lbw_outcome_pm35(ex2)
    print(ex3)
  }