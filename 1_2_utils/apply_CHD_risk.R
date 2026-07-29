# apply_CHD_risk.R

#####--------------------------------------------

apply_chd_risk <- function(current_population) {
  
  #requires qrisk and qstroke risk to be calculated.
  if( all(! c('stroke_risk', 'qrisk_score') %in% names(current_population)) ) {
    
    stop("CHD is calculated from stroke and qrisk (Stroke +CHD scores) -check these attributes are present and 
         populated before preceding.")
  }
    
  current_population <- current_population |> 
    mutate( chd_risk = pmax((qrisk_score - stroke_risk),0))
      
  return(current_population)
}

###########################################
############ TEST ############
###########################################

not_actually_a_function_just_dont_want_contents_run <- function(){
  # all babies with age 0 = were NAN
  # risk went haywire under 25 yo
  
  full_test_population  %>% 
    #group_by(age) %>% 
    #summarise(m=mean(risk,na.rm = T)) %>% View
    ggplot() +  
    geom_boxplot(aes(group=age,age,risk))
  
  full_test_population %>% 
    mutate(townsend_quintile=as.character(townsend_quintile)) %>% 
    ggplot() +  
    geom_boxplot(aes(colour = townsend_quintile, group = townsend_quintile, townsend_quintile,risk),alpha=0.1)
  
  full_test_population  %>% 
    ggplot() +  
    geom_boxplot(aes(group=!is.na(atrial_fibrillation),!is.na(atrial_fibrillation),risk))
  
  #y <- apply_cvd_risk(x)
  
  #y[is.na(y$risk),]
  
}

