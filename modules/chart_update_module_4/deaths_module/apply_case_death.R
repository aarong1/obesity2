library(fst)

case_fatality_rate_df <- read.fst ('case_fatality_rate_df.fst')

apply_case_death <- function(input_population, morbidity = 'diabetes') {
  
  setDT(input_population)
  
  # Lookup the case fatality rate for this morbidity
  p <- case_fatality_rate_df %>% 
    filter(morbidity == {{morbidity}}) %>% 
    pull(case_fatality_rate)
  
  # Check if morbidity exists in lookup table
  if(length(p) == 0) {
    stop(paste0("Morbidity '", morbidity, "' not found in case_fatality_rate_df"))
  }
  
  # Create the year risk variable name
  morbidity_year_risk <- paste0(morbidity, '_year_risk')
  
  # Calculate death percentile for individuals with the morbidity
  input_population[get(morbidity) != 0, 
                   `:=`(death_percentile = frank(get(morbidity_year_risk)) / 
                          max(frank(get(morbidity_year_risk))))]
  
  
  # Apply case fatality rate - assign deaths to those with morbidity
  # Higher year_risk = higher percentile = more likely to die
  input_population[
    # (runif(.N) < p) & (get(morbidity) != 0), 
    
    (.5/p) * death_percentile < runif(.N),
    
    `:=`(
      death        = max(year),     
      death_reason = morbidity
    )
  ]
  
  cat(paste0("\n=== Applied case fatality for: ", morbidity, " ===\n"))
  cat(paste0("Case fatality rate: ", round(p, 6), "\n"))
  cat(paste0("Deaths assigned: ", sum(input_population$death_reason == morbidity, na.rm = TRUE), "\n"))
  cat(paste0("Cases prevalent: ", sum(input_population$'diabetes' !=0, na.rm = TRUE), "\n"))
  
  input_population$death_percentile <- NULL
  
  return(input_population)
}
