

p=0.07
# stable since 2012

apply_low_birth_weight_risk <- function(input_population) {
  p=0.07
  year = max(input_population$year)
  
  input_population$low_birth_weight <- ifelse(age ==0 & sample(replace = T,
                                                     x = c(T,F),
                                                     prob = c(p,1-p),
                                                     size = nrow(input_population)),
                                              year,
                                              0) 
  
  # Return the modified dataset
  return(input_population)
}


 
 
 apply_low_birth_weight_risk <- function(input_population) {
 
   year = max(input_population$year)
   
   initial_time_zero_population[age==0,
                                low_birth_weight := ifelse(
                                  sample(replace = T,
                                         x = c(T,F),
                                         prob = c(p,1-p),
                                         size = .N),
                                  year,
                                  0)
   ]
   
   initial_time_zero_population[is.na(low_birth_weight), low_birth_weight := 0]
   
   return(input_population)
 }
 
 