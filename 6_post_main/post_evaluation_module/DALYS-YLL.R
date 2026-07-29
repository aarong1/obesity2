library(data.table)
library(fst)
library(readxl)

lifetables <- read_excel("data/lifetables.xlsx", 
                         sheet = "2020-2022", skip = 5)

ltnames <- c('age', 'mx', 'qx', 'lx', 'dx', 'ex' ,'sex')

lifetables <- rbind(
  lifetables[1:6] %>% 
    mutate(sex = 'Males') %>% 
    setnames(ltnames),

  lifetables[8:13] %>% 
    mutate(sex = 'Females') %>% 
    setnames(ltnames)
)

lifetables <- rbind(
lifetables,
data.frame(
  101:110,
  0.443038,
  0.362694,
  739.2,
  268.1,
  0,
  'males')%>% 
  setnames(ltnames),

data.frame(
  101:110,
  0.496552,
  0.397790,
  1990.7,
  791.9,
  0,
  'females'
) %>% 
  setnames(ltnames)
)

setDT(lifetables)

# lifetables[ ,.(age,sex,ex)]
# 
# # pp <- setDT(past_populations)
# pp <- setDT(dead_population)
#
# pp[ ,.(age,sex)]
#
# pp[#death!=0,
#    lifetables[,.(age,sex,ex)],
#    on = .(age, sex),
#    ex := i.ex]
#
# deaths <- pp[,.(yll=sum(ex,na.rm=T)),.(death_reason,run,intervention,year)]
#
# reasons <- copy(pp[,.N,.(death_reason)][['death_reason']] )
#
# full_death_reasons <- CJ(year = seq(model_specification$model$start_year+1,
#                length.out=model_specification$model$duration),
#            run = 1:model_specification$model$number_of_runs,
#            death_reason = reasons
#            ) 
#
# full_death_reasons[,intervention := ifelse(run>max(run)/2,'intervention','non-intervention')]
#
# full_death_reasons[deaths,on = .(year,run,intervention,death_reason), `:=` (yll = i.yll)]
#
# full_death_reasons[is.na(yll), yll := 0]
#
# yll <- full_death_reasons[, .(yll = sum(yll,na.rm = T)), by = c('run','intervention','death_reason','year')
#                           ][,.(yll = mean(yll,na.rm = T)), by = c('intervention','death_reason','year')]
#

calculate_daly_yll <- function(past_populations, group_vars= as.character(), year_cut_off = NULL){
  
  
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  pp <- setDT(past_populations)
  
  pp <- pp[death!=0,]
  
  setDT(lifetables)

  pp <- pp[
    lifetables[,.(age,sex,ex)],
    on = .(age, sex),
    ex := i.ex]
  
  deaths <- pp[,.(.N,yll=sum(ex,na.rm=T)),by=c('death_reason','run',group_vars,'intervention','year')]
  
  # reasons <- copy(pp[,.N,.(death_reason)][['death_reason']] )
  
  # full_death_reasons <- CJ(year = seq(model_specification$model$start_year+1,
  #                                     length.out=model_specification$model$duration),
  #                          run = 1:model_specification$model$number_of_runs,
  #                          death_reason = reasons
  #                          )
  
  full_death_reasons <-  do.call(CJ, 
                   lapply(X = c('year','run','death_reason',group_vars), 
                          function(x){c(unique(deaths[[x]]))}) 
  )
  
  names(full_death_reasons) <- c('year', 'run','death_reason', group_vars)
  
  full_death_reasons[,intervention := ifelse(run>max(run)/2,'intervention','non-intervention')]
  
  full_death_reasons[deaths,on = c('year','run', group_vars, 'intervention','death_reason'), `:=` (yll = i.yll, N = i.N)] #, nomatch=0L
  
  full_death_reasons[is.na(yll), yll := 0]
  
  yll <- full_death_reasons[, .(yll = sum(yll,na.rm = T),N=sum(N)), by = c('run','intervention',group_vars,'death_reason','year')
  ][,.(yll = mean(yll,na.rm = T)), by = c('intervention',group_vars,'death_reason','year')]
  # ,N=mean(N)
  yll
  
  }

# daly_yll_sex <- calculate_daly_yll(past_populations, 'sex', 2024)
# dead_population <- pp[death!=0, ]
