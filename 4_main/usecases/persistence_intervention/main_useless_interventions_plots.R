library(scales)

pop_a %>% 
  filter(age>30) %>%
  mutate(year = as.numeric(year)) %>% 
  
  group_by(year, intervene) %>%
  summarise(n = mean(n)) %>% 
  ggplot() +
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
 
 geom_rect(
       aes(xmin = 2025, xmax = 2030, ymin = 1401, ymax = 2600),
       colour = "white",
       fill = "mediumseagreen",
       alpha = 0.005
   ) +
 
 geom_point(aes(year, n, colour = intervene), size = 2.5) +
 
 geom_line(aes(year, n, group = intervene, colour = intervene), linewidth = 1.2) +
 
 scale_x_continuous(
       breaks = seq(2025, 2065, by = 5),
       minor_breaks = seq(2023, 2065, by = 1)
   ) +
 
 scale_y_continuous(
       labels = label_number(scale = 1/1000, big.mark = ","),
       limits = c(1400, 2600)
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
   geom_smooth(method = 'loess',color = 'grey', data  = x %>% filter(year>2030) %>% mutate(year=as.numeric(year)), aes(y =delta, x = year)) +
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
   
   theme_bw(base_family = "Graphik") +
   
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
 
