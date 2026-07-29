# ~/Documents/SIB/PHM/PHModel/post_evaluation_module/post_evaluations_implementation.R

past_populations[year != min(year)&age>35,
                 .(stroke = sum(stroke == year) ,.N),
                 by = .(bmi,run)
                 ][, .(stroke = mean(stroke), N=mean(N)), by = .(bmi)
                 ][order(bmi)] %>% 
  filter(!is.na(bmi)) %>% 
  # filter(age>40) %>% 
  add_count(wt=N) %>% 
  add_count(wt = stroke) %>% 
  mutate(avg=n/3) %>% 
  mutate(avg_global=nn/n) %>% 
  mutate(stroke = stroke/N) %>%
  e_charts(bmi) %>%
  e_bar(stroke, name = "Stroke incidence") %>%
  e_mark_line(data = list(
    type = "average",
    name = "AVG"
  )) #|> 
  
  e_mark_line(data = list(y=0.004528952))

  e_mark_line(data = list(y=14212.27))
  
  past_populations[year != min(year) & age > 50&age<100,
                   .(stroke = sum(stroke == year, na.rm = TRUE),
                     N      = .N),
                   by = .(age, year, run)
  ][
    , .(stroke = mean(stroke),
        N      = mean(N)),
    by = .(year, age)
  ] [
    , inc := fifelse(N > 0, stroke, NA_real_)
  ][
    , year := as.character(year)
  ] %>%
    # group_by(year) %>%                    # important for heatmap layout
    e_charts(year) %>%
    e_heatmap(age, inc) %>%               # x=year, y=age, fill=inc
    e_visual_map(inc, calculable = TRUE)%>%
    e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
      "function(p){
      return 'Year: ' + p.name +
             '<br>Age: ' + p.value[1] +
             '<br>Incidence: ' + (p.value[2] == null ? 'NA' : (p.value[2]*1000).toFixed(2)) + ' per 1,000';
    }"
    )) %>%
    e_axis_labels(x = "Year", y = "Age") %>%
    e_y_axis(min = NA) %>%
    e_title("Stroke incidence surface (Age × Year)", "Incidence shown as per-person-year (tooltip per 1,000)")
  
  
  
  
  past_populations[year != min(year) & age > 50&age<100,
                   .(stroke = sum(stroke == year, na.rm = TRUE),
                     N      = .N),
                   by = .(age, sex, run)
  ][
    , .(stroke = mean(stroke),
        N      = mean(N)),
    by = .(sex, age)
  ] [
    , inc := fifelse(N > 0, stroke , NA_real_)
  ] %>%
    # group_by(year) %>%                    # important for heatmap layout
    e_charts(age) %>%
    e_heatmap(sex, inc) %>%               # x=year, y=age, fill=inc
    e_visual_map(inc, calculable = TRUE) %>%
    e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
      "function(p){
      return 'Year: ' + p.name +
             '<br>Age: ' + p.value[1] +
             '<br>Incidence: ' + (p.value[2] == null ? 'NA' : (p.value[2]*1000).toFixed(2)) + ' per 1,000';
    }"
    )) %>%
    e_axis_labels(x = "Year", y = "Age") %>%
    e_y_axis(min = NA) %>%
    e_title("Stroke incidence surface (Age × Year)", "Incidence shown as per-person-year (tooltip per 1,000)")
  
  
  past_populations[year != min(year) & age > 50 & age<100 & stroke == year,
  ] %>% #filter(is.na(year))
    # group_by(year) %>%    
    # filter() %>% 
    mutate(age=as.numeric(age),
           year = as.numeric(year)
           ) %>% # important for heatmap layout
    e_charts(age) %>%
    e_scatter(year) %>%               # x=year, y=age, fill=inc
    e_y_axis(min = 2021) %>%
    e_x_axis(min = 40) %>%
    e_lm(formula = year~age) #%>% 
    # e_visual_map(inc, calculable = TRUE) %>%
    e_tooltip(trigger = "item", formatter = htmlwidgets::JS(
      "function(p){
      return 'Year: ' + p.name +
             '<br>Age: ' + p.value[1] +
             '<br>Incidence: ' + (p.value[2] == null ? 'NA' : (p.value[2]*1000).toFixed(2)) + ' per 1,000';
    }"
    )) %>%
    e_axis_labels(x = "Year", y = "Age") %>%
    e_title("Stroke incidence surface (Age × Year)", "Incidence shown as per-person-year (tooltip per 1,000)")
    
    
 out <-    past_populations[year != min(year) & age > 50&age<100]%>% 
      split(.$HSCT) %>%
      lapply(.,FUN =  function(x){x[,.(stroke = sum(stroke !=0, na.rm = TRUE),
                N      = .N),
              by = .(age10, year, run)
              ][
                , .(stroke = mean(stroke),
                    N      = mean(N)),
                by = .( year, age10)
              ] [
                , prev := fifelse(N > 0, stroke , NA_real_)
              ][,year:=as.character(year)] %>% 
          group_by(age10) %>% 
          e_charts(year) %>%
          e_line( prev) %>% 
          e_legend(top=30) %>% 
          e_title(subtext =unique(x$HSCT) ) %>% 
          e_group('grp') %>% 
          e_grid(containLabel = T) %>% 
          e_y_axis(max=20) %>% 
          # e_connect('grp') %>% 
          e_connect_group('grp')
      }
          )
      
 browsable(div(style ='display:flex;flex-direction:row;',tagList(out)))

    
 out <-    past_populations[year != min(year) & age > 60&age<120]%>% 
   split(.$HSCT) %>%
   lapply(.,FUN =  function(x){x[,.(stroke = sum(stroke !=0, na.rm = TRUE),
                                    N      = .N),
                                 by = .(mdm_quintile_soa_name, year, run)
   ][
     , .(stroke = mean(stroke),
         N      = mean(N)),
     by = .( year, mdm_quintile_soa_name)
   ] [
     , prev := fifelse(N > 0, stroke/n , NA_real_)
   ][, year:=as.character(year)] %>% 
       group_by(mdm_quintile_soa_name) %>% 
       e_charts(year) %>%
       e_legend(top=30) %>% 
       e_grid(containLabel = T,top='25%') %>% 
       e_line( prev) %>% 
       # e_y_axis(max=.0030) %>% 
       e_title(subtext =unique(x$HSCT) ) 
       
   }
   )
 
 browsable(div(style ='display:flex;flex-direction:row;',tagList(out)))
 
 
 out <-    past_populations[year != min(year) & age > 50&age<100]%>% 
   # slice_sample(n=1000) %>%
   split(.$HSCT) %>%
   lapply(.,FUN =  function(x){x[,.(stroke = sum(stroke !=0, na.rm = TRUE),t=mean(custom_townsend_score_dz),     
                                    N      = .N),
                                 by = .(sdz_code, year, run)
            ][,.(stroke = mean(stroke),t=mean(t)),by = .(sdz_code, year)
            ] [, year:=as.character(year)] %>% 
       filter(year==max(year)) %>% 
       # group_by(year) %>%
       e_charts(t) %>%
       e_scatter( stroke) %>% 
       e_lm(formula = stroke~t) %>%
       e_title(subtext =unique(x$HSCT) ) 
       
   }
   )
 
 browsable(div(style ='display:flex;flex-direction:row;',tagList(out)))
 
 
  e_mark_area
  