
add_multimorbidity_fn <- function(past_populations, group_vars= as.character(), cut_off_year=NULL){
  
  if(!is.null(cut_off_year)){
    past_populations <- past_populations[year == cut_off_year,]
  }
  isCost <- past_populations %>% 
    select(
      c(
        stroke,               chd,                   diabetes,             hypothyroidism,      
        asthma,               copd,                  non_diabetic_hyperglycaemia,                  chronic_kidney_disease,      
        dementia,             heart_failure,         lung_cancer,          prostate_cancer,     
        female_breast_cancer,         colorectal_cancer,          atrial_fibrillation,  rheumatoid_arthritis,
        osteoarthritis,       epilepsy,              osteoporosis,         kidney_cancer,        
        oesophageal_cancer,   stomach_cancer,        oral_cancer,          pancreatic_cancer,   
        uterine_cancer,       ovarian_cancer,        blood_cancer
      ) ) %>% 
    as.matrix() %>%
    {.!=0}
  
  past_populations <- rowSums(isCost) %>% 
    cbind(past_populations, data.table(multimorbidity = .))
  
  past_populations
  
}

