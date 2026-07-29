#age_sex_death.R 

# data_prep -------
library(readxl)
library(data.table)

 lifetables_male <- read_excel("data/lifetables.xlsx",
                                  sheet = '2017-2019', #'2020-2022',
                                  range = "A6:F107") %>% 
      mutate(sex='Males') 
 
 # extend MALE lifetables past 100 ----
 lifetables_male_over100 <- lifetables_male[rep(101,15), ] |> 
   mutate(age=101:(100+n()))
 
 lifetables_male <- rbind(lifetables_male, lifetables_male_over100)

lifetables_female <- read_excel("data/lifetables.xlsx", 
sheet = "2020-2022",
range = "H6:M107") %>% 
  mutate(sex='Females')


 # extend FEMALE lifetables past 100 ----
lifetables_female_over100 <- lifetables_female[rep(101,15), ] |> 
   mutate(age=101:(100+n()))
 
lifetables_female <- rbind(lifetables_female, lifetables_female_over100)

 
lifetables <- rbind(lifetables_female,lifetables_male)

lifetables <- lifetables[c('age','sex','qx')] %>% 
  mutate(qx = qx*0.7)

# function definition ----

# result of investigation into NAs for id, among other attributes,being produced using the coerce to data.table 
# but not from the left join. This because the behavior of 'left joins' are different.
# in data table, non matching keys are pulled in with columns as NA - (not really a left join, but OK?)
# in left_join from the tidyverse this isn't the case = proper left join
# we doubly mitigated this. we expanded the number the upper age range to 95 years of age when building our synthetic population
# and added the nomatch = 0  parameter to make the data.tables join properly 'left'.

apply_age_sex_death<- function(current_population, apply_death = F){
  
  #current_population <- initial_time_zero_population
  year1 <- max(current_population$year) #[[1]]

 # x <- left_join(current_population, lifetables,by = join_by(age,sex) )

  #current_population <- as.data.table(current_population)

  # sum(is.na(initial_time_zero_population$id))
  # sum(is.na(current_population$id))
  # sum(is.na(x$id))
  
  current_population <- select(current_population, - any_of( 'qx') )
  
  current_population <- as.data.table(current_population)[ as.data.table(lifetables), on = .( age, sex), nomatch = 0 ]

  #sum(is.na(current_population$id))

  current_population$bern_trial <- runif(n=length(current_population$qx))

  if(apply_death){
   
      current_population = current_population[, `:=` (death = year1 * (bern_trial<qx))] 
      current_population[ , death_reason := ifelse(year1==death, 'age_sex_std', NA)]
      
  }
  
  # deselect bern_trial column from select 
  # current_population <- current_population[, c("qx") := NULL] #"bern_trial",
  
  # dt[, c("b", "c") := NULL] modify inplace 
  # dt[, .SD, .SDcols = !c("b")] copy and modify
  
  # convert back into a data.frame/ tibble so we donts have to 
  # do this outsied the function after the fn call every time.
  current_population <- as.data.frame(current_population) 
  
  return(current_population)
  
  
   
}


# current_population |> apply_age_sex_death(T) |> count(death)
  

# lifetables

###########################################
############ TEST ############
###########################################

# 
 # x <- instantiate_base_pop(scale_down_factor=100)    %>%
 #  apply_age_sex_death()
 # 
 # x[,sum(death)]/2022/8
 #
# split(x,by = 'death') %>% rbindlist()
# unsplit doesnt work for data.tables
# to recombine use rbindList

