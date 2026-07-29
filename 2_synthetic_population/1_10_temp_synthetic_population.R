# 1_10_temp_synthetic_population.R ----

library(readxl)
library(readODS)
library(readr)
library(tidyverse)
library(qs)
library(fst)

#######################################

# 1.9 -> 1.10

# Whats changed 

# MAJOR RELEASE ----
# use DZ and SDz 2021
# integrate lookups from sa -> dz and soa -> sdz
# define our own townsedn index of deprivation

#######################################

#aggregating higher level geographies from data zones  - SOA not included !!
# https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/geography-aggregating-data-zones-to-produce-statistics-for-higher-level-geographies.pdf
# https://datavis.nisra.gov.uk/census/census-2021-table-lookup.html

## GEOGRAPHY LOOKUP LINKS
# https://www.nisra.gov.uk/support/geography/geography-lookups

# build a synthetic population built on population estimates from super output area

lookup_sdz_soa <- read_fst('./2_synthetic_population/lookup_sdz_soa.fst')
lookup_dz_sa <- read_fst('./2_synthetic_population/lookup_sa_dz.fst')

instantiate_base_pop <- function(model_specification = NULL,...){
  
  start_year = model_specification$model$start_year
  scale_down_factor = model_specification$population$scale_down_factor
  
  if(is.null( model_specification)){
    print('Model configuration is not provided. Defaults applied')
    start_year = 2020
    scale_down_factor = 1900
  }
  if(is.null( model_specification$model$start_year)){
    print('start year is not provided. Defaults applied')
    start_year = 2020
  }
  #dots <- list(...)
  if(is.null( model_specification$population$scale_down_factor) #| 
     #is.null(dots$scale_down_factor) 
  ){
    print('population scale factor is not provided. Defaults applied')
    scale_down_factor = 1900
  }
  
  # modifyList()
  # coalesces a second list with the first
  
  
  dz_pop <- read_excel("data/c21/census-2021-ms-a09-DZ-SYA-sex-pop-2021.xlsx", 
                       range = 'A9:KV3789')
  dz_pop <- dz_pop |> 
    pivot_longer(-c(1,2),names_sep = ': \r\n',names_to = c('sex','age'),values_to = 'pop') |> 
    filter(sex %in% c('Male','Female')) |> 
    mutate(age = as.numeric(str_remove_all(age, "\\+|\\s*years?|\\s"))) |> 
    filter(!is.na(age))
  
  dz_pop <- dz_pop |>   
    rename(dz_name = Geography,
           dz_id  =`Geography code`
    )
  
  
  dz_lookup_admin_geo <- read_excel("data/c21/geography-data-zone-and-super-data-zone-lookups-v3.xlsx", 
                                    sheet = "DZ2021_Admin_geog_lookup")
  
  dz_lookup_urban <- read_excel("data/c21/geography-data-zone-and-super-data-zone-lookups-v3.xlsx", 
                                sheet = "DZ21_Urban_mixed_rural_lookup")
  
  dz_pop <- dz_pop |> 
    left_join(dz_lookup_admin_geo,by=c('dz_id'='DZ2021_code' , 'dz_name'='DZ2021_name')) |> 
    left_join(dz_lookup_urban,by=c('dz_id'='DZ2021_code' , 'dz_name'='DZ2021_name'))

  
  MDM17_SA <- read_excel("data/MDM17_SA.xls", 
                         sheet = "MDM")
  
  MDM17_SA <- MDM17_SA |> 
      select(
    SA2011,
    SOA2001_name,
    mdm_rank_sa = `Multiple Deprivation Measure Rank \n(where 1 is most deprived)`,
    income_dm_rank_sa = `Income Domain Rank \n(where 1 is most deprived)`,
    employ_rank_sa = `Employment Domain Rank (where 1 is most deprived)`,
    health_rank_sa = `Health Deprivation and Disability Domain Rank (where 1 is most deprived)`
  )
  
  dz_pop <- dz_pop |> 
    left_join(lookup_dz_sa[c('sa','dz')], by=c('dz_id'='dz')) #View()
    
    #using my own lookup above for sa to dz
    # AND separately for soa to sdz this means that consistent heiracrchy 
    # within census is breached
    dz_pop <- dz_pop |> 
    left_join(MDM17_SA, by=c('sa'='SA2011'))
  

  
    dz_pop <- dz_pop |> 
      left_join(lookup_sdz_soa, by=c('SDZ2021_code'='sdz_id')) 
      
  ## lookup tables
  # https://www.nisra.gov.uk/publications/geography-lookup-tables

  
  sa_nra_lookup <- read_ods(skip = 4 ,'./data/geography-lookup-sa2011-to-nra-statistical-geographies-2011-v1.ods')
  sa_nra_lookup <- sa_nra_lookup |> 
    replace_na(list(NRA = 'Non-NRA'))
  
  
  dz_pop <- dz_pop |> 
    left_join(sa_nra_lookup[1:3], by = c('sa'='SA')) 
  
  sa_ward <- read_ods(skip = 4 , './data/Lookup-Table-SA-OA-and-SOA-(statistical-geographies)-2016.ods')
  sa_ward <- sa_ward[c('SA','WARD1992',"AA1998", "AA2008")]
  
  dz_pop <- dz_pop |> 
    left_join(sa_ward, by = c('sa'='SA'),
              relationship = 'many-to-one',
              na_matches = 'never' )
  #https://statistics.ukdataservice.ac.uk/dataset/2011-uk-townsend-deprivation-scores 
  #SOA - LSOA
  
  townsend_ward <- read_csv("data/townsend_ward.csv")
  
  
  names(townsend_ward) <- paste0('townsend_', names(townsend_ward))
  
  # MDM rank 1 is the most deprived 
  # Townsend score ~ -7 is the most affluent 
  # Townsend rank 1 is the most affluent
  # We need to adapt this so townsend rank is the same as mdm rank affluence direction
  
  townsend_ward <- townsend_ward |> 
    mutate(townsend_quintile_ward = -townsend_quintile + 6 )
  
  dz_pop <- dz_pop %>% 
    left_join(townsend_ward,
              by = c('WARD1992'='townsend_GEO_CODE'),
              relationship = 'many-to-one',
              na_matches = 'never'  )
  
  townsend_dz <- read.fst('./2_synthetic_population/data_zone_townsend_deprivation.fst')
  
  custom_townsend <- townsend_dz |> 
    select( dz_id = area_code, 
            custom_townsend_score = townsend_sum,
            custom_townsend_rank = townsend_rank
    ) |> 
    mutate(custom_townsend_score_dz = custom_townsend_score + 0.25)
  
  dz_pop <- dz_pop |> 
    left_join(custom_townsend, by = c('dz_id'='dz_id'),
              relationship = 'many-to-one',
              na_matches = 'never'  )
  
  # to make it more aligned with NI offset to UK
  
  # nimdm17_2021 <- read_excel("data/census-2021-commissioned-table-ct0149.xlsx",
  #                            sheet = "Overall", skip = 5)
  # 
  # nimdm17_2021_income <- read_excel("data/census-2021-commissioned-table-ct0149.xlsx", 
  #                            sheet = "Income", skip = 5)
  # 
  # nimdm17_2021_employment <- read_excel("data/census-2021-commissioned-table-ct0149.xlsx", 
  #                                   sheet = "Employment", skip = 5)
  
  # nimdm17_2021 <- nimdm17_2021 |> 
  #   mutate(prob = `Usual residents`/sum(`Usual residents`)) |> 
  #   select(LGD2014_name = `Local Government District Name`, 
  #          LGD2014_code= `Local Government District Code`,
  #          mdm_quintile = `NIMDM2017 Quintile`,
  #          prob)
  
  
  # dz_pop <- dz_pop |> 
  #   left_join(nimdm17_2021)
  
  #https://www.nisra.gov.uk/publications/nimdm17-soa-level-results
  soa_mdm <- read_excel('./data/MDM17_SOAresults.xls',sheet='MDM',skip=0)
  soa_mdm <- soa_mdm[1:7]
  
  # 6 - Income Domain Rank
  # 7 - Employment Domain Rank
  # 8 - Health Deprivation and Disability Domain Rank
  # 9 - Education Skills and Training Domain Rank
  # 10 - Access to services Domain Rank
  # 11 - Living Environment Domain Rank
  # 12 - Crime and Disorder Domain Rank
  
  names(soa_mdm) <- 
    c("LGD2014NAME",                                                             
      "Urban",                                                
      "SOA2001",                                                                 
      "SOA2001_name",
      "mdm_rank",
      'income_dm_rank',
      'employment_dm_rank')
  
  soa_mdm <- soa_mdm |> 
    mutate(mdm_quintile_soa=cut(labels = 1:5, mdm_rank, breaks = 5)) |> 
    mutate(mdm_decile_soa = cut(labels = 1:10, mdm_rank, breaks = 10)) |> 
    mutate(income_dm_quintile_soa=cut(labels = 1:5, income_dm_rank, breaks = 5)) |> 
    mutate(income_dm_decile_soa = cut(labels = 1:10, income_dm_rank, breaks = 10)) |> 
    mutate(employment_dm_quintile_soa=cut(labels = 1:5, employment_dm_rank, breaks = 5)) |> 
    mutate(employment_dm_decile_soa = cut(labels = 1:10, employment_dm_rank, breaks = 10))
  
  soa_mdm$mdm_quintile_soa_name =  c("Most Deprived", "Quintile 2", "Quintile 3", "Quintile 4", "Least Deprived")[soa_mdm$mdm_quintile_soa]
  
  #c('Most','2','3','4','Least')
  
  dz_pop <- dz_pop %>% 
    left_join(soa_mdm, 
              by = c('soa_id' = 'SOA2001'),
              relationship = 'many-to-one',
              na_matches = 'never'  ) #%>%
   
  # rename(sa_code = `SA Code`, soa_code = SOA, soa_name = SOA2001_name) 
  
  
  
  
  #-----------#
  ### This is as far as I got ###
  #-----------#
  
  dz_pop <- dz_pop %>%
    mutate(sex = ifelse(sex == 'Male', 'Males', 'Females'))
  
   dz_pop <- dz_pop %>%
    select(
      c( sex,
         age,
         pop,
         dz_id,
         dz_name,
         sdz_code = SDZ2021_code,
         sdz_name = SDZ2021_name,
         soa_code = soa_id,
         soa_name = SOA2001_name.x,
         #sa_name,
         sa_code = sa,
         
         county = COUNTY,
         
         PARLCON2024_code,
         PARLCON2024_name,
         
         PARLCON2008_code,
         PARLCON2008_name,  
         
         SETTLEMENT2015_code,       
         SETTLEMENT2015_name,
         SETTLEMENT2015_Band,   
         
         LGD2014_code,
         LGD2014_name,
         
         LGD1992_code,
         LGD1992_name, 
         
         HSCT = HEALTH_TRUST,
         
         DEA2014_name,
         DEA2014_code,
         
         AA2008,
         AA1998,
         
         NRA_name = NRA,                       
         NRA_code = `NRA Code`,                   
         WARD1992,                  
         
         Urban_status,               
         Urban_mixed_rural_status,  
         
         mdm_rank,
         mdm_quintile_soa,
         mdm_quintile_soa_name,
         mdm_decile_soa,
         
         income_dm_rank,
         income_dm_quintile_soa,
         income_dm_decile_soa,
         
         employment_dm_rank,
         employment_dm_quintile_soa, 
         employment_dm_decile_soa,
         
         # townsend_score = 'townsend_TDS',
         # townsend_quintile,
         # townsend_ID,                
         # townsend_GEO_LABEL,        
         # townsend_TDS,               
         # townsend_quintile,         
         # townsend_quintile_ward,     
         
         
         custom_townsend_rank,       
         custom_townsend_score_dz,  
         
         # dz_population_density,   
         # sdz_population_density,
         
         # sdz_area,                  
         # sdz_pop, 
         # 
         # dz_area,                    
         # dz_pop,                    
         
         # sa_area_km2,
         # soa_area_km2,
         # sa_pop,
         # soa_pop
      ) )
  
  
  
  # change spec dataframe into synthetic population data frame -----
  pop <- dz_pop |> 
    slice_sample(weight_by = pop,
                 replace = T,
                 n = round(1903463/ scale_down_factor)) #19,000
  
  # Add meta columns ---------
  pop <- pop %>% 
    mutate(.before=1, 
           id = row_number() ,
           year = start_year ,
           run = 1 
    )
  
  pop1 <- pop|> 
    mutate(.after = 'age',
      age20 = cut(age,include.lowest = T,
                  breaks = seq(0,120,20),
                  labels = c('0-20',
                             '20-40',
                             '40-60',
                             '60-80',
                             '80-100',
                             '100-120')
      )
    )
  
  pop1 <- pop1 |> 
    mutate(.after = 'age',
           age_risk = cut(age,include.lowest = T,
  breaks = c(0, 16, 34, 44, 54, 64, 74, 110),
  labels = c("0-15", "16-34", "35-44", "45-54", "55-64", "65-74", "75-110")
           )
    )
  
  # post hoc additions and mutations to population dataframe -----
  ### ethnicity ------
  print('synthetic population - adding ethnicity')
  
  #source("./synthetic_population/ethnicity_prep.R")
  eth_percent <- qread('./2_synthetic_population/ethnicity_distribution.qs')
  
  pop1 <- left_join(pop1,
                   select(eth_percent,-c('Geography code','age')),
                   by = join_by(LGD2014_name == Geography,between(age,age_min,age_max)),
                   relationship = 'many-to-one',
                   na_matches = 'never' ) |> 
    select(-c(age_min,age_max)) 
  
  pop <- pop1 |> 
    rowwise() |> 
    mutate(
      ethnicity = sample(size = 1,
                         x =
                           c("white",
                             "irish traveller",
                             "roma",     
                             "indian", 
                             "chinese",
                             "filipino",
                             "pakistani",
                             "arab",
                             "other asian",
                             "black african",
                             "black other",
                             "mixed",          
                             "other"),
                         prob = probs )) |> 
    select(-probs)
  
  
  
  # add broad ethnicity for less fine faceting outputs
  
  pop <- pop |> 
    mutate(broad_ethnicity = ifelse(ethnicity == 'white',
                                    'white',
                                    'minority')
    )
  
  
  pop <- pop |> 
    
    mutate( 
      
      ## This column is the most important for determining prevalence
      # It is a statement of the year of entry into the suffering state for that disease
      ## It is numeric
      ## In instances that 
      ## If prevalence of disease is populated as occurring before the model instantiated
      # then the the year is listed as the year before the first modelled year.
      # This may change if we can find distribution of time for sufferers for when that 
      # number/distribution might matter.
      
      
      stroke = 0,
      chd = 0,
      diabetes = 0,
      dementia = 0,
      heart_failure = 0,
      atrial_fibrillation = 0,
      hypertension = 0,
      chronic_kidney_disease = 0,
      lung_cancer = 0,
      
      # This column helps if morbidity is repeatable
      # It is essentially a binary flag of the the above
      
      # stroke_history = NA,
      # chd_history = NA,
      # diabetes_history = NA,
      # dementia_history = NA,
      # heart_failure_history = NA,
      # atrial_fibrillation_history = NA,
      # hypertension_history = NA,
      # chronic_kidney_disease_history = NA,
      # lung_cancer_history = NA,
      
      # This column helps if the morbidity is recoverable
      
      stroke_recovered = FALSE,
      chd_recovered = FALSE,
      diabetes_recovered = FALSE,
      dementia_recovered = FALSE,
      heart_failure_recovered = FALSE,
      atrial_fibrillation_recovered = FALSE,
      hypertension_recovered = FALSE,
      chronic_kidney_disease_recovered = FALSE,
      lung_cancer_recovered = FALSE#)
      
      #df$col1 <- NULL  # deletes col1
    )
  
  
ethrisk_lookup <- tribble(
  ~ethnicity, ~ethrisk,
  'white', 1,
  'irish traveller', 1,
  'roma', 1,
  'indian', 2,
  'filipino', 5,
  'arab', 5,
  'other asian', 5,
  'pakistani', 3,
  'chinese', 8,
  'black african', 7,
  'black other', 6,
  'mixed', 9,
  'other', 9,
  )

pop <- pop |> 
  left_join(ethrisk_lookup) 
  
  return( ungroup(pop) )
  
}


###########################################
############ TEST ############
###########################################

#x <- instantiate_base_pop(test_specification)

#write.fst(x,'./synthetic_population/pop.fst')

library(readxl)

urban_attr <- read_excel("./data/Settlement15-lookup (1).xls",  skip = 3)

post_hoc_attributes <- function(){ 
sdz_w_area <- st_drop_geometry(sdz) |>
  rename(sdz_area = area) # has area

dz_w_area <- st_drop_geometry(dz_shape) |>
  select(DZ2021_cd, dz_area = area)  # has area

# area and population density
dz_pop <- dz_pop |>
  # mutate(dz_area_km2 = `Area (hectares)` * 0.01) |>
  left_join(sdz_w_area, by = c('SDZ2021_code'='sdz_id')) |>
  left_join(dz_w_area, by = c('dz_id'='DZ2021_cd'))

dz_pop <- dz_pop |>
  add_count(dz_id, wt = pop, name = 'dz_pop') |>
  add_count(SDZ2021_code, wt = pop, name = 'sdz_pop') |>

  mutate( dz_population_density = dz_pop / {dz_area} ) |>
  mutate( sdz_population_density = sdz_pop / {sdz_area} )

dz_pop |> select(
sdz_area,
sdz_pop,

dz_population_density,   
sdz_population_density,

dz_area,
dz_pop)


}


