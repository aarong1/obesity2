library(scales)

# pop_a <- pop_a1
# pop_d <- pop_d1

ppp %>% 
      select(q=`2022`, sex,age) %>% 
  mutate(age=as.numeric(age)) %>%
  mutate(
    age_group = case_when(
    age == 0 ~ "0",
    age >= 1 & age <= 4 ~ "1-4",
    age >= 5 & age <= 9 ~ "5-9",
    age >= 10 & age <= 14 ~ "10-14",
    age >= 15 & age <= 19 ~ "15-19",
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64",
    age >= 65 & age <= 69 ~ "65-69",
    age >= 70 & age <= 74 ~ "70-74",
    age >= 75 & age <= 79 ~ "75-79",
    age >= 80 & age <= 84 ~ "80-84",
    age >= 85 & age <= 89 ~ "85-89",
    age >= 90 ~ "90+",
    TRUE ~ "All Ages")) %>% 
      mutate(q = q/100000) %>% 
  filter(!age %in% c("All Ages", "Birth")) %>%
  left_join(death_rates_by_age #%>% 
              #select(q_model=qx_total,sex,age_group) 
  ) %>% 
  group_by(sex) %>% 
  e_charts(age, timeline = T) %>% 
  e_datazoom(type ='',bottom='20%') %>% 
  e_grid(bottom='35%') %>% 
  e_legend(position = 'top',bottom='10%') %>% 
  # e_line(q) %>%
  # e_line(qx_total) %>%
  e_line(qx_suicide) %>%
  e_loess(qx_suicide~age)
  
  # e_line(qx_not_suicide) %>%
  # e_line(qx_natural) %>%
  # e_line(qx_not_natural) %>%
  e_tooltip()

pop_a %>% 
  # filter(age>30) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  ggplot() +
  # geom_rect(aes(xmin = 2025, xmax = 2030, ymin = 117001, ymax = 123000),
  #           colour = 'white', 
  #           fill = 'mediumseagreen', alpha = 0.005) +
  geom_point(aes(year, n, colour = intervene)) +
  geom_line(aes(year, n, colour = intervene)) +
  scale_x_continuous(
    breaks = seq(2025, 2065, by = 5),
    minor_breaks = seq(2023, 2065, by = 1)
  ) +
  

  scale_y_continuous(
    labels = label_number(scale = 1/1000, big.mark = ","),
    # limits = c(1400, 2600)
  ) +
  labs(
    x = "Year",
    y = "Population (thousands)",
    colour = "Scenario"
  ) +
  theme_bw(base_family = "Graphik") +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    # axis.text.x  = element_text(size = 15, face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold")
  )


pop_d %>% 
  group_by(year, intervene) %>%
  summarise(n = mean(n), .groups = "drop") %>% 
  mutate(year = as.numeric(year)) %>% 
  
  ggplot() +
  
  # geom_rect(
  #   aes(xmin = 2025, xmax = 2030, ymin = 1401, ymax = 2600),
  #   colour = "white",
  #   fill = "mediumseagreen",
  #   alpha = 0.005
  # ) +

  geom_point(aes(year, n, colour = intervene), size = 2.5) +
  
  geom_line(aes(year, n, group = intervene, colour = intervene), linewidth = 1.2) +
  
  scale_x_continuous(
    breaks = seq(2025, 2065, by = 5),
    minor_breaks = seq(2023, 2065, by = 1)
  ) +
  
  scale_y_continuous(
    labels = label_number(scale = 1/1000, big.mark = ","),
    # limits = c(1400, 2600)
  ) +
  
  labs(
    x = "Year",
    y = "Population (thousands)",
    colour = "Scenario"
  ) +
  
  theme_bw(base_family = "Graphik") +
  
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    # axis.text.x  = element_text(size = 15, face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold")
  )





x %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot() +
  geom_col(aes(year, delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'Not Sig.',
    delta - natural_var * 1.96 < 0 ~ 'Down',
    delta - natural_var * 1.96 > 0 ~ 'Up'
  )))  +
  scale_fill_manual(values = c("Up" = "salmon", "Down" = "lightgreen",'Not Sig.' = 'black')) +
  geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  # geom_smooth(method = 'loess',color = 'grey', data  = x %>% filter(year>2030) %>% mutate(year=as.numeric(year)), aes(y =delta, x = year)) +
  theme_minimal() +
  labs(
    fill = 'Deaths',
    x = "Year",
    y = "Delta"
  ) + scale_x_continuous(
    breaks = seq(2025, 2065, by = 5),
    minor_breaks = seq(2023, 2065, by = 1)
  ) +
  
  scale_y_continuous(
    labels = label_number(scale = 1/1000, big.mark = ",")#,
    # limits = c(1400, 2600)
  ) +
  
  labs(
    x = "Year",
    y = "Population ",
    colour = "Scenario"
  ) +
  
  theme_bw(base_family = "Avenir") +
  
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    # axis.text.x  = element_text(size = 15, face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold")
  )


x %>% 
  mutate(year = as.numeric(year)) %>% 
  ggplot() +
  geom_rect(
    aes(xmin = 2025, xmax = 2030.5, ymin = -800, ymax = 0),
    colour = "white",
    fill = "mediumseagreen",
    alpha = 0.03
  ) +
  geom_col(aes(year, cum_delta, fill = case_when( 
    abs(delta ) <(natural_var * 1.96) ~ 'none',
    delta - natural_var * 1.96 < 0 ~ 'down',
    delta - natural_var * 1.96 > 0 ~ 'up'
  )))  +
  scale_fill_manual(values = c("up" = "salmon", "down" = "lightgreen",'none' = 'black'))+
  # geom_hline(yintercept = natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_hline(yintercept = -natural_var * 1.96, color = "grey", linetype = "dashed") +
  # geom_smooth( aes( year, delta ))
  geom_smooth(method = 'loess',color='lightgrey', 
              data  = x %>%   
                mutate(year = as.numeric(year)) %>% 
                filter(year>2030), aes(y =cum_delta, x = year)
  ) +
  theme_minimal() +
  labs(
    fill = 'Deaths',
    x = "Year",
    y = "Delta"
  ) + scale_x_continuous(
    breaks = seq(2025, 2065, by = 5),
    minor_breaks = seq(2023, 2065, by = 1)
  ) +
  
  # scale_y_continuous(
  #   # labels = label_number(scale = 1/1000, big.mark = ",")#,
  #   # limits = c(1400, 2600)
  # ) +
  
  labs(
    x = "Year",
    y = "Population ",
    colour = "Scenario"
  ) +
  
  theme_bw(base_family = "Graphik") +
  
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    # axis.text.x  = element_text(size = 15, face = "bold"),
    axis.title.y  = element_text(size = 14, face = "bold")
  )

