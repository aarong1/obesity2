#' HRUPoRT-style HRU transition risk + expected transitions
#'
#' Computes individual logit, probability, and expected new HRU transitions (nHRU = prob * survey_weight),
#' plus population summaries (MeanRisk and TotalHRU).
#'
#' Assumes dt is a data.table with *categorical* inputs already coded as below.
#'
#' Required columns (default names can be overridden via args):
#' - chronic:        "chronic" vs "no_chronic" (or TRUE/FALSE)
#' - sex:            "male" / "female"
#' - age_band:       "<30","30-39","40-49","50-59","60-69","70-79","80+"
#' - inc_q:          "q1","q2","q3","q4","q5","missing" (q5 is highest)
#' - ethnicity:      "white","non_white","missing"
#' - smoking:        "non_smoker","former_light","former_heavy","current_light","current_heavy","missing"
#' - food_sec:       "food_insecure","food_secure","missing"
#' - gen_health:     "excellent_very_good_good","fair","poor","missing"
#' - bmi_cat:        "<18.5","18.5-24.9","25.0-29.9","30.0-34.9","35.0-39.9","40+","missing"
#' - alcohol:        "light","moderate","heavy","non_drinker","missing"
#' - pa_q:           "q1","q2","q3","q4","missing" (q4 is highest activity)
#' - immigrant:      "canadian_born","immigrant_<10","immigrant_10+","missing"
#' - survey_weight:  numeric
#'
#' @return list(dt=dt_with_outputs, summary=data.table with MeanRisk, TotalHRU, N)
hruport_hru_transition <- function(
    dt,
    chronic_col = "chronic",
    sex_col = "sex",
    age_band_col = "age_band",
    inc_q_col = "inc_q",
    ethnicity_col = "ethnicity",
    smoking_col = "smoking",
    food_sec_col = "food_sec",
    gen_health_col = "gen_health",
    bmi_cat_col = "bmi_cat",
    alcohol_col = "alcohol",
    pa_q_col = "pa_q",
    immigrant_col = "immigrant",
    weight_col = "survey_weight",
    out_logit = "logit",
    out_prob  = "prob",
    out_nhru  = "nHRU",
    strict = TRUE
) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  dt <- data.table::as.data.table(dt)
  
  # helper: add coefficient if category matches
  add_if <- function(x, target, coef) data.table::fifelse(x == target, coef, 0)
  
  # optionally validate categories
  if (strict) {
    req <- c(chronic_col, sex_col, age_band_col, inc_q_col, ethnicity_col, smoking_col,
             food_sec_col, gen_health_col, bmi_cat_col, alcohol_col, pa_q_col,
             immigrant_col, weight_col)
    missing_cols <- setdiff(req, names(dt))
    if (length(missing_cols)) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
    if (anyNA(dt[[weight_col]])) stop(weight_col, " contains NA; please impute or set to 0.")
  }
  
  # compute logit per person
  dt[, (out_logit) := {
    l <- -5.0742
    
    # Chronic condition (baseline: no chronic = 0)
    # Accept either logical or category string.
    ch <- get(chronic_col)
    l <- l + data.table::fifelse(isTRUE(ch) | ch %in% c("chronic", "has_chronic", "has chronic condition"), 0.3617, 0)
    
    # Sex (baseline: female = 0)
    l <- l + add_if(get(sex_col), "male", 0.2425)
    
    # Age band (baseline: <30 = 0)
    ab <- get(age_band_col)
    l <- l + add_if(ab, "30-39", 0.1202)
    l <- l + add_if(ab, "40-49", 0.9213)
    l <- l + add_if(ab, "50-59", 1.6306)
    l <- l + add_if(ab, "60-69", 2.3558)
    l <- l + add_if(ab, "70-79", 2.8083)
    l <- l + add_if(ab, "80+",   3.6188)
    
    # Income quintile (baseline: highest quintile = 0)
    iq <- get(inc_q_col)
    l <- l + add_if(iq, "q1",      0.5256)  # bottom
    l <- l + add_if(iq, "q2",      0.4035)
    l <- l + add_if(iq, "q3",      0.2000)
    l <- l + add_if(iq, "q4",      0.2267)
    l <- l + add_if(iq, "missing", 0.3841)
    
    # Ethnicity (baseline: white = 0)
    eth <- get(ethnicity_col)
    l <- l + add_if(eth, "non_white", -0.2426)
    l <- l + add_if(eth, "missing",    0.00983)
    
    # Smoking (baseline: non-smoker = 0)
    sm <- get(smoking_col)
    l <- l + add_if(sm, "former_heavy",   0.2926)
    l <- l + add_if(sm, "former_light",   0.1296)
    l <- l + add_if(sm, "current_heavy",  0.4579)
    l <- l + add_if(sm, "current_light",  0.2569)
    l <- l + add_if(sm, "missing",       -0.0661)
    
    # Food security (baseline: food insecure = 0)
    fs <- get(food_sec_col)
    l <- l + add_if(fs, "food_secure", -0.2661)
    l <- l + add_if(fs, "missing",     -0.7921)
    
    # General health (baseline: excellent/very/good = 0)
    gh <- get(gen_health_col)
    l <- l + add_if(gh, "fair",    0.4080)
    l <- l + add_if(gh, "poor",    1.0628)
    l <- l + add_if(gh, "missing", 0.1470)
    
    # BMI (baseline: 18.5-24.9 = 0)
    b <- get(bmi_cat_col)
    l <- l + add_if(b, "30.0-34.9", 0.1131)
    l <- l + add_if(b, "25.0-29.9", 0.0753)
    l <- l + add_if(b, "40+",       0.6384)
    l <- l + add_if(b, "<18.5",     0.0174)
    l <- l + add_if(b, "35.0-39.9", 0.3624)
    l <- l + add_if(b, "missing",   0.0959)
    
    # Alcohol (baseline: light drinker = 0)
    al <- get(alcohol_col)
    l <- l + add_if(al, "heavy",      -0.0105)
    l <- l + add_if(al, "moderate",   -0.0757)
    l <- l + add_if(al, "non_drinker", 0.0420)
    l <- l + add_if(al, "missing",     0.0619)
    
    # Physical activity quartile (baseline: highest q4 = 0)
    pa <- get(pa_q_col)
    l <- l + add_if(pa, "q1",      0.1766)  # bottom activity
    l <- l + add_if(pa, "q2",     -0.0110)
    l <- l + add_if(pa, "q3",      0.0172)
    l <- l + add_if(pa, "missing", 0.3490)
    
    # Immigrant status (baseline: Canadian-born = 0)
    im <- get(immigrant_col)
    l <- l + add_if(im, "immigrant_<10", -0.2358)
    l <- l + add_if(im, "immigrant_10+", -0.0375)
    l <- l + add_if(im, "missing",        0.1781)
    
    l
  }]
  
  # probability using stable logistic
  dt[, (out_prob) := data.table::fifelse(
    get(out_logit) >= 0,
    1 / (1 + exp(-get(out_logit))),
    exp(get(out_logit)) / (1 + exp(get(out_logit)))
  )]
  
  # expected new transitions
  dt[, (out_nhru) := get(out_prob) * get(weight_col)]
  
  # summaries
  summary <- dt[, .(
    N = .N,
    MeanRisk = mean(get(out_prob), na.rm = TRUE),
    TotalHRU = sum(get(out_nhru), na.rm = TRUE)
  )]
  
  list(dt = dt, summary = summary)
}


library(data.table)

res <- hruport_hru_transition(pp)  # pp is your person-year table
res$summary
# res$dt now has: logit, prob, nHRU