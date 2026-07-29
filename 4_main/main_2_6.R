function(){
  # Initialize DuckDB connection for storing past populations
  # con <- initialize_past_populations_db("./past_populations_db/past_populations.duckdb")
  # tbl_name_current_simulation <- paste0('past_populations_',format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  con <- dbConnect(duckdb::duckdb(), dbdir ='past_populations_db/past_populations.duckdb', read_only = FALSE); con
  cat("DuckDB database initialized for storing population data\n")
  
  latest_tbl <- sort(decreasing = T,dbListTables(con))[1]
  
  x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.',latest_tbl,' USING SAMPLE 60 PERCENT (bernoulli);'))  # Set cache size to 2MB
  x <- dbSendQuery(
    con,
    paste0(
      "SELECT *
     FROM past_populations.", latest_tbl, ";"
    )
  )
  
  # past_populations_20260116_015236
  past_populations <- dbFetch(x)
  
  dbClearResult(x)
  dbDisconnect(con, shutdown=TRUE)
  
  # setDT(past_populations)
  write.fst(past_populations, './3_pre_main/intermediate_populations/full_history_past_populations.fst')
  }
  
  count(target_populations,year,run)
  count(past_populations,year,run)
  
  t_past_populations <- #past_populations %>% 
    # filter(!target) %>%
    # bind_rows(target_populations_df ) %>%
    bind_rows(target_populations) %>%
    # target_populations_df %>% 
    mutate(intervention = 'intervention')
  
  t_past_populations <- past_populations %>% 
    filter(!target) %>%
    # bind_rows(target_populations_df ) %>%
    bind_rows(target_populations) %>%
    # target_populations_df %>% 
    mutate(intervention = 'intervention')
  
  total_pop <- bind_rows(
    past_populations %>% mutate(intervention = 'non-intervention'),
    t_past_populations
    )
  
  count(t_past_populations,year,run,intervention)
  count(total_pop,intervention,year,run)
  
  # population
  total_pop %>%
    # filter(age<70 & age>48) %>%
    # filter(target==T) %>%
    group_by(year, run, intervention) %>% 
    summarise(n=n()) %>%
    group_by(year,intervention) %>%
    summarise(n = mean(n)) %>%
    ggplot() + 
    geom_line(aes(year, n, color = intervention)) 
    # geom_line(aes(year,n, lty = as.character(run), color = intervention)) 
  
  #Total Deaths

  total_pop %>% 
    # filter(year == min(year)) %>% #count(death_reason)
    # filter(age>70) %>% 
    # filter(year!=min(year)) %>% 
    filter(target==T) %>%
    
    group_by(year, run, intervention) %>% 
    summarise(n = sum( death_reason != 'survive')) %>%
    group_by(year, intervention) %>%
    summarise(n = mean(n)) %>%
    ggplot() + 
    geom_line(aes(year, n, color = intervention)) 
  
  # Survival
  
  total_pop %>% 
    filter(age>70) %>%
    # filter(year!=min(year)) %>% 
    group_by(year, run, intervention) %>% 
    summarise(surv = sum( death == 0), tot = n()) %>%
    group_by(year, intervention) %>%
    summarise(surv = mean(surv), tot= mean(tot)) %>%
    mutate(survival_prob = surv/(tot)) %>%
    ggplot() + 
    geom_line(aes(year, survival_prob, color = intervention)) 
    
   # geom_line(aes(year,n, lty = as.character(run), color = intervention)) 
  
  #Stroke Prevalence
  total_pop %>%
    # filter(intervention=='non-intervention') %>% 
    filter(target==T) %>%
    # filter(death_reason == 'survive') %>% 
    group_by(year, run, intervention) %>% 
    summarise(s=sum(stroke != 0)) %>%
    group_by(year,intervention) %>% 
    summarise(s=mean(s)) %>%
    ggplot() + 
    geom_line(aes(year,s, color=intervention)) +
    ylim(c(0,NA))
  
  #Stroke Incidence
  total_pop %>%
    # filter(year != min(year)) %>%
    group_by(year, run, intervention) %>% 
    summarise(s=sum(stroke == year)) %>%
    group_by(year,intervention) %>% 
    summarise(s=mean(s)) %>%
    ggplot() + 
    geom_line(aes(year,s, color=intervention)) +
    ylim(c(0,NA))
  
    total_pop %>%
    # filter(target==T) %>%
    group_by(year,  intervention, run) %>% 
    summarise(stroke_dead = sum(death_reason == 'stroke')) %>%
    group_by(year, intervention) %>% 
    summarise(stroke_dead = mean(stroke_dead)) %>%
    ggplot() + 
    geom_line(aes(year, stroke_dead, color=intervention)) +
    ylim(c(0,NA))
  

540384/
  model_specification$population$scale_down_factor/
  model_specification$model$number_of_runs

target_populations_df %>% 
  filter(year != min(year)) %>%
  group_by(year) %>% 
  summarise(s=sum(stroke == year)) %>%
  ggplot() + 
  geom_line(aes(year,s))

past_populations %>% 
  filter(year != min(year)) %>%
  group_by(year) %>% 
  summarise(s=sum(stroke == year)) %>%
  ggplot() + 
  geom_line(aes(year,s))

# past_populations[,intervention := 'non-intervention']


count(total_pop, intervention)

# demographics look stable
total_pop %>% count(age20,year)

#stroke risk is going up
total_pop %>% count(year, wt = stroke_year_risk)

total_pop %>% 
  count(year,  stroke_death = death_reason=='stroke') %>% 
  filter(stroke_death==TRUE)


# strokes are going down
total_pop %>% count(year, wt = ( stroke !=0))

total_pop %>% count(year, wt = ( stroke == year))
 
total_pop %>% count(year, s = ( stroke ==year)) %>% 
  filter(s==T)

total_pop %>%
  # filter(target==T) %>%
  group_by(year, run, intervention) %>% 
  summarise(bmi = sum(bmi %in% c('overweight','obese'))) %>%
  group_by(year,intervention) %>% 
  summarise(bmi=mean(bmi)) %>% 
  pivot_wider(names_from = 'intervention', values_from = 'bmi') 



total_pop %>%
  # filter(target==T) %>%
  group_by(year, run, intervention) %>% 
  summarise(stroke_y_r = sum(stroke_year_risk)) %>%
  group_by(year,intervention) %>% 
  summarise(stroke_y_r = mean(stroke_y_r)) %>% 
  pivot_wider(names_from = 'intervention', values_from = 'stroke_y_r') 

total_pop %>%
  # filter(target==T) %>%
  group_by(year, run, intervention) %>% 
  summarise(n=n()) %>%
  group_by(year,intervention) %>% 
  summarise(n = mean(n)) %>% 
  pivot_wider(names_from = 'intervention', values_from = 'n') 

total_pop %>%
  # filter(target==T) %>%
  group_by(year, run, intervention) %>% 
  summarise(s=sum(stroke != 0)) %>%
  group_by(year,intervention) %>% 
  summarise(s=mean(s)) %>% 
  pivot_wider(names_from = 'intervention', values_from = 's') 

total_pop %>%
  # filter(target==T) %>%
  group_by(year, run, intervention) %>% 
  summarise(s=sum(stroke != 0)) %>%
  group_by(year,intervention) %>% 
  summarise(s=mean(s)) %>%
  ggplot() + 
  geom_line(aes(year,s, color=intervention)) +
  ylim(c(0,NA))


total_pop %>%
  filter(year != min(year)) %>%
  # filter(target==T) %>%
  group_by(year, run, intervention) %>% 
  summarise(s=sum(stroke == year)) %>%
  group_by(year, intervention) %>% 
  summarise(s=mean(s)) %>%
  ggplot() + 
  geom_line(aes(year,s, color=intervention)) +
    ylim(c(0,NA))
  # geom_smooth(aes(year,s, color=intervention), method='loess', span=1)

total_pop %>%
  filter(year!=min(year)) %>% 
  filter(target==T) %>% 
  group_by(year, run, intervention) %>% 
  summarise(s=sum(diabetes == year)) %>%
  group_by(year, intervention) %>% 
  summarise(s=mean(s)) %>%
  ggplot() + 
  geom_line(aes(year,s, color=intervention)) +
geom_smooth(aes(year,s, color=intervention), method='loess', span=1)+
  ylim(c(0,NA))


total_pop %>%
  filter(year!=min(year)) %>% 
  filter(target==T) %>% 
  group_by(year, run, intervention) %>% 
  summarise(s=sum(osteoarthritis == year)) %>%
  group_by(year, intervention) %>% 
  summarise(s=mean(s)) %>%
  ggplot() + 
  geom_line(aes(year,s, color=intervention)) +
  geom_smooth(aes(year,s, color=intervention), method='loess', span=1)+
  ylim(c(0,NA))

total_pop %>%
  # filter(year!=min(year)) %>% 
  filter(target==T) %>% 
  group_by(year, run, intervention) %>% 
  summarise(s=sum(osteoporosis == year)) %>%
  group_by(year, intervention) %>% 
  summarise(s=mean(s)) %>%
  ggplot() + 
  geom_line(aes(year,s, color=intervention)) +
  geom_smooth(aes(year,s, color=intervention), method='loess', span=1) + 
  ylim(c(0,NA))


total_pop %>% count(year,stroke)




