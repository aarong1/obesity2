
transform_10y_probability_to_1y <- function(prob_10yr){

  prob_1yr = ( 1-(1-prob_10yr)^(0.1) )

  return(prob_1yr)

}


transform_probability_to_1y <- function(prob_x_years, tot_years = 10){
  
  prob_1yr = ( 1-(1-prob_x_years)^(1/tot_years) )
  
  return(prob_1yr)
  
}



##################################################
#####################  TEST ######################  
##################################################  

# transform_10y_probability_to_1y( 
#   seq(0,0.99,0.01) ) %>% 
#   plot(y=.,x=seq(0,0.99,0.01),type='b')


# transform_10y_probability_to_1y(0.5)
# # 0.06696701
# 
# (1-accumulate2(
#   .x = 1:19,
#   .y = 1-rep(0.06696701,19),
#   .init=1-0.06696701,
#   .f = function(x,y,z) { print(x);return(x*z)}
#   )) %>%  
#   plot(type = 'b')



# [1] 0.9330330 0.8705506 0.8122524 0.7578583 0.7071068 0.6597539 0.6155722 0.5743492 0.5358867
# [10] 0.5000000

# apply_cvd_event <- function(y){
#   year1 <- max(y$year)
#
#   avg <- mean(x$risk, na.rm=T,trim = 0.05)
# 
#   y <- y %>%   
#     rowwise() %>% 
#     replace_na(list(risk=avg)) %>% 
#     mutate(prob_1yr = transform_10y_probability_to_1y(risk) ) %>% 
#     mutate(cvd = year1 * rbinom(n = 1, size = 1, prob = prob_1yr)) %>% 
#     ungroup()
# 
#   return(y)
# 
# }

