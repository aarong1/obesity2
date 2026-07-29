# ethnicity_prep.R

### if not commented out then this function is called from main routine recursivly 
### INFINITE LOOP


#pop = instantiate_base_pop(1900)

risk_mapping <- c("white" = "white (or not stated)",
                  "irish traveller" =  "white (or not stated)",
                  "roma" = "white (or not stated)",
                  "indian" = "indian",
                  "chinese" = "chinese",
                  "filipino" = "other asian",
                  "pakistani" = "pakistani",
                  "arab" = "other asian",
                  "other asian" = "other asian",
                  "black african" = "black african",
                  "black other" = "black carribean",
                  "mixed" = "other ethnic group",
                  "other" = "other ethnic group") |> 
  data.frame() |> 
  rename('health_ethnicity' = 1) |> 
  rownames_to_column(var = 'nisra_ethnicity')

qrisk_encoding <- data.frame(
  health_ethnicity = c("white (or not stated)",
                       "indian",
                       "pakistani",
                       "bangladeshi",
                       "other asian",
                       "black caribbean",
                       "black african",
                       "chinese",
                       "other ethnic group"),
  ethnicity_encoding = 1:9)

# Ethnicity codes
# ONS2011 5+1
# ONS 2011 18+1
eth_percent  <- read_excel("data/census-2021-ethnicity_age_geo.xlsx", 
                           sheet = "MS-B26", skip = 23)
eth_percent <- eth_percent |> 
  pivot_longer(cols=-(1:2),names_sep = ': \r\n',names_to = c('age','ethnicity')) |> 
  replace_na(replace = list(ethnicity='All')) |> 
  mutate(
    # Extract the age range using regular expressions
    age_min = str_extract(`age`, "\\d+(?=-)"),     # Extracts the number before the hyphen
    age_max = str_extract(`age`, "(?<=-)\\d+"),    # Extracts the number after the hyphen
    # Convert to numeric
    age_min = as.numeric(age_min),
    age_max = as.numeric(age_max)
  ) |> 
  replace_na(list(age_min = 65, age_max = 105))

eth_percent <- eth_percent |> 
  filter( age != "All usual residents" ,
          ethnicity != "All" ,
          Geography != "Northern Ireland"
  )

eth_percent <- eth_percent |> 
  pivot_wider(names_from = 'ethnicity',
              values_from = 'value')    |> 
  rowwise() |> 
  mutate(probs =   list(
    c(
      White,
      `Irish Traveller` ,
      Roma ,
      Indian ,
      Chinese ,
      Filipino ,
      Pakistani ,
      Arab ,
      `Other Asian` ,
      `Black African` ,
      `Black Other` ,
      Mixed,      
      `Other ethnicities`
    )
  )
  ) |> 
  select(
    - c(White,`Irish Traveller` ,Roma ,Indian ,Chinese ,Filipino ,Pakistani ,Arab ,`Other Asian` ,`Black African` ,`Black Other` ,Mixed,      `Other ethnicities`) )  

#write.fst(eth_percent,'./synthetic_population/ethnicity_distribution.fst')

qs::qsave(eth_percent,'./synthetic_population/ethnicity_distribution.qs')

# pop <- left_join(pop,
#                  select(eth_percent,-c('Geography code','age')),
#                  by = join_by(LGD2014NAME == Geography,between(age,age_min,age_max))) |> 
#   select(-c(age_min,age_max)) 
# 
# print(summary(pop))
# print(pop$probs)
# 
# pop <- pop |> 
#   rowwise() |> 
#   mutate(
#     ethnicity = sample(size = 1,
#                        x =
#                          c("white",
#                            "irish traveller",
#                            "roma",     
#                            "indian", 
#                            "chinese",
#                            "filipino",
#                            "pakistani",
#                            "arab",
#                            "other asian",
#                            "black african",
#                            "black other",
#                            "mixed",          
#                            "other"),
#                        prob = probs )
#     ) |> 
#   select(-probs)
