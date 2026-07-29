
# Time from diagnosis
past_populations |> 
       filter(diabetes!=0) |> 
       mutate( distribution = year-diabetes) |> 
  count(year,run, wt = distribution, name='distribution') |> 
  filter(year==max(year)) |> 
  ggplot()+
  geom_density(aes(x=year,y=distribution,col=year))

# First stroke year

# Last Stroke Year 


# Time between them


# How many strokes 

past_populations[c('stroke','id','year','run' )] |>
   # filter(id==507)
  filter(stroke!=0) |> 
  count(id, run, year,incident = year==stroke) |> 
  count(year,incident, m=mean(incident) ,sort=T) |> 
  filter(incident==TRUE) |> 
  count(year, wt=m) |> 
  ggplot(aes(as.character(year),n,col=as.character(run)))+
           geom_point()+theme_minimal()
  

  library(ggplot2)
  
