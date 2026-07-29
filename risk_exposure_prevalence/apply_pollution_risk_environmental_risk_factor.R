# https://ifs.org.uk/data-items/pm25-exposure-income-deprivation-quintile

# Exposure to air pollution in England, 2003–23

# source('pollution_risk_exposure.R')
 lookup_dz_raster_cell <- read.fst('./1_2_utils/data/lookup_dz_raster_cell_pm25.fst')

# lookup_dz_raster_cell <- lookup_dz_raster_cell |>
#    select(dz =2, pm25g)
# 
# pop <- read.fst('./synthetic_population/pop.fst')
# 
# current_population <- pop |>
#   ungroup() |>
#   sample_frac(size = 0.01)
# 
# current_population |>
#  left_join(lookup_dz_raster_cell,
#            relationship = 'many-to-one',
#            by = join_by(dz_id == dz))
 
 apply_pollution_lifestyle_parameter_geography_constant <- function(current_population,lookup_dz_raster_cell){
     
     current_population <- current_population |> 
         select(-any_of(c('pm25g')))
    
    lookup_dz_raster_cell <- lookup_dz_raster_cell |>
       select(dz =2, pm25g)
    
    current_population |> 
     left_join(lookup_dz_raster_cell,
               relationship = 'many-to-one',
               by = join_by(dz_id == dz))
    
 }
 #current_population
 
 




