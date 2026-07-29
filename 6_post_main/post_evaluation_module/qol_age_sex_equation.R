eq_fn <-  function(sex, age){
  male <-  (sex=='Males')
  eq <-  0.9508566 + 0.0212126 * male - 0.0002587 * age - 0.0000332 * age^2
}

pop <- expand.grid(sex=c('male','female'),age=10:100 )

eq_value <- pop %>% 
rowwise() %>% 
  mutate(eq=eq_fn(sex, age))

eq_value %>% 
  ggplot()+
  geom_line(aes(age,eq,lty=sex))
  
# https://stapm.gitlab.io/r-packages/qalyr/articles/qaly_estimation_report.pdf


add_modeled_eq_5d <- function(past_populations){
  
  setDT(past_populations)
  past_populations[,modeled_eq_5d := eq_fn(sex, age)]


}
