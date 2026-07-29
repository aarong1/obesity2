fixed_price <- 5e6
price_per_person <- 500
price_per_person_per_year <- 1000
price_per_year <- 1e6

fixed_price

total_time <- 3

total_people <- past_populations[
  intervention=='intervention',.(N=sum(target)) , by = .(run, intervention, year)
  ][, .(total_people = sum(N)), by = .(intervention, year)
  ]#[,.(tp=mean(total_people))]¢[['tp']]

total_people[year == min(year)+1,'fixed_cost':=fixed_price] 
total_people[year != min(year),'annual_cost':=price_per_year]
total_people[year != min(year),'ongoing_patient_cost':=total_people*price_per_person_per_year]
total_people[year == min(year)+1,'patient_cost':=total_people*price_per_person]

total_people[is.na(fixed_cost), fixed_cost:=0]
total_people[is.na(annual_cost), annual_cost:=0]
total_people[is.na(ongoing_patient_cost), ongoing_patient_cost:=0]
total_people[is.na(patient_cost), patient_cost:=0]

total_people[,total_cost := fixed_cost + annual_cost + ongoing_patient_cost + patient_cost]

total_people[,cumulative_total_cost := cumsum(total_cost)]





total_people[,year := as.character(year)]

# past_populations[, .N, by = target]
  
costs <- calculate_costs_fn(past_populations, year_cut_off = NULL)
  
qalys[disease == 'combined_uw', ] %>%
  dcast(formula = year  ~intervention, value.var = 'total_uw', fill = 0L) %>%
  mutate(year = as.character(year)) %>% 
  mutate(averted = `non-intervention` - intervention) %>%
  mutate(cumulative_averted = cumsum(averted)) %>% 
  left_join(
  costs[,.(total_cost = sum(total_cost)), by = .(intervention,year)
  ][,year := as.character(year)
  ] %>%
  dcast(formula = year  ~intervention, value.var = 'total_cost', fill = 0L) %>%
  mutate(savings = `non-intervention` - intervention) %>% 
  mutate(cumulative_savings = cumsum(savings)),
  by='year'
  ) %>% 
  mutate(cost_per_qaly_gained = cumulative_savings / cumulative_averted
  ) -> cost_per_qaly


icer <- cost_per_qaly[total_people,on='year'] %>%
  mutate(cost_per_qaly_gained = (cumulative_savings-cumulative_total_cost) / cumulative_averted) %>% 
  mutate(cumulative_monetised_qalys = cumulative_averted * 20000) %>%
  mutate(cost_per_qaly_gained_monetised = (cumulative_savings+cumulative_monetised_qalys-cumulative_total_cost) / cumulative_averted) %>% 
  
  mutate(cumulative_net_money = cumulative_savings-cumulative_total_cost) %>% 
  mutate(roi = cumulative_net_money/cumulative_total_cost)



icer %>% 
  e_charts(year) %>% 
  e_line(cumulative_total_cost) %>% 
  e_line(cumulative_savings) %>% 
  e_line(cumulative_net_money) %>% 
  e_line(cumulative_monetised_qalys) %>% #, y_index = 1
  # e_line(cost_per_qaly_gained, name = 'Cost per QALY gained') %>% 
  # e_line(cumulative_averted) %>% 
  # e_line(cost_per_qaly_gained_monetised) %>% 
  e_tooltip()

icer %>% 
  e_charts(year) %>% 
  e_line(roi) %>% 
  e_format_y_axis(suffix='x') %>% 
  e_tooltip()
  
icer %>% 
  e_charts(year) %>% 
  e_line(cost_per_qaly_gained, name = 'Cost per QALY gained') %>% 
  e_line(cumulative_averted, name = 'Cumulative QALYs', y_index = 1) %>%
  e_line(cost_per_qaly_gained_monetised, name = 'ICER') %>% 
  e_format_y_axis(suffix='£/QALY') %>% 
  e_tooltip()

icer %>% 
  select(year,
         cumulative_total_cost,
         cumulative_savings,
         cumulative_net_money,
         cumulative_monetised_qalys,
         roi,
         cost_per_qaly_gained,
         cumulative_averted,
         cost_per_qaly_gained_monetised,) %>% 
  reactable::reactable()

  
