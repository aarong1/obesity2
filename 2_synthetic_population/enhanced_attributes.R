#https://build.nisra.gov.uk/en/custom/data?d=PEOPLE&v=DEA14&v=RELIGION_BELONG_TO_AGG4&v=RACIAL_EQUALITY_TEO&v=AGE_BAND_BROAD&v=SEXUAL_ORIENTATION_DVO_AGG4

age_dea_racial_religion_sexual <- read.csv("data/ni-census21-people-dea14+religion_belong_to_agg4+racial_equality_teo+age_band_broad+sexual_orientation_dvo_agg4-34cffc63.csv")

pop$DEA2014_code

names(age_sex_racial_sexual)

join_df <- age_dea_racial_religion_sexual %>% 
  select(
'DEA2014_code' = "District.Electoral.Area.2014.Code",
religion = "Religion...4.Categories.Label",
race = "Racial.Equality..The.Executive.Office..Label",
age_group = "Age...4.Categories.Label",
sexual_orientation = "Sexual.Orientation.Label",    
"Count"
) %>% 
  mutate(broad_ethnicity =
           case_when(
             race=='Non-white ethnicity' ~ 'minority',
             race=='White ethnicity: British/Irish/Northern Irish only and Christian/no religion/religion not stated' ~ 'white',
             race=='White ethnicity: all others' ~ 'white',
             )) %>% 
  count(age_group,DEA2014_code, broad_ethnicity, sexual_orientation,religion,wt = Count)  %>% 
  mutate(
    religion =
      recode_values(
        religion,
    'Catholic' ~ 'catholic',
    'No religion/religion not stated' ~ 'none',
    'Other religions' ~ 'other',
    'Protestant and Other Christian (including Christian related)' ~ 'protestant'
    )
  )

join_df <- join_df %>% 
  mutate(total = sum(n)) %>% 
  group_by(DEA2014_code,
           age_group,
           broad_ethnicity) %>% 
  mutate(probability = n / sum(n)) %>% 
  mutate(cum_prob = cumsum(probability))


p <- pop %>% 
  mutate(
    age_group = case_when(
    age <= 15 ~ '0-15 years',   
    age <= 39 ~ '16-39 years',  
    age <= 64 ~ '40-64 years',  
    T ~ '65+ years')
  ) %>% 
  mutate(percentile = (row_number() + 1 )/n()) %>% 
  left_join(join_df,
          relationship = 'many-to-one',
          multiple = 'first',
          
          by = join_by('age_group', 
                       'broad_ethnicity',   
                       'DEA2014_code',
                       percentile < cum_prob)
) %>% 
select( - c(
  n,
  total,
  probability,
  cum_prob
  )
)





