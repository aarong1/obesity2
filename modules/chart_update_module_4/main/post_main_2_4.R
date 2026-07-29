
# write_fst(past_populations, './past_populations/past_populations_04_01_2026.fst')

### Mortality -----

count(current_population,lung_cancer,wt=qmortality_risk)  #mean(qmortality_risk),

current_population |> 
  filter(is.na(qmortality_risk)) |> 
  apply_qmortality_mortality() |> pull(qmortality_risk)
view()

#Cancer 
yearly_dead_has_cancer <- dead_population |> 
  count( year,run, death, lung_cancer = lung_cancer != 0, name = 'count' ) |> 
  group_by( year,death, lung_cancer) |> 
  summarise( count = mean(count) ) |> 
  mutate(NI = count * model_specification$population$scale_down_factor) |> 
  filter( lung_cancer == TRUE )

cancer_mortality <- read_excel("data/NIcancer_registry/all_cancers_data_tables.xlsx", 
                               sheet = "T19", skip = 5)

names(cancer_mortality)[1:5] <- c('year', 'count', 'per100k', 'standarised_per100k','ci')

cancer_mortality |> 
  mutate(NI = 19 * per100k) |> 
  mutate(year = as.numeric(year)) |> 
  ggplot() +
  geom_line(aes(year,NI), lwd = 2)+
  geom_line(yearly_dead_has_cancer,mapping = aes(death,NI),col='orange')+
  theme_minimal()

##### STROKES are different - they are ACUTE and REPEATABLE #####
# look at instances where a persons latest stroke has occurred
# STROKES

past_populations |> 
  group_by(year,stroke,run) |> #View() 
  summarise(counted_states = sum(stroke!=0),
            .groups = "drop") |>
  group_by(year,stroke) |> 
  summarise(counted_states = mean(counted_states),
            .groups = "drop") |>  
  #filter(!stroke %in% c(0, 2017)) |> 
  ggplot() +
  geom_point(aes(year,counted_states,col = as.character(stroke))) +
  geom_line(aes(year,counted_states,group = stroke, col = as.character(stroke)))

# total number of strokes suffered by survivors that area still alive
count(past_populations,id,stroke) |> 
  filter(stroke!=0) |> #filter(id==878)
  count(id) |> 
  summarise(total_strokes_survivors_alive = sum(n),
            stroke_survivors_alive = n())
