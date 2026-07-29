

library(readxl)
library(data.table)
library(tidyverse)
episodes <- read_excel("./post_evaluation_module/hs-episode-based-activity-stats-volume-2-22-23.xls", 
                       sheet = "Diagnosis Summary", skip = 2)
# View(episode)


stats <- episodes[-1,
                  c("Title",
                    "Finished
Episodes",
                    'Emergency',
                    "Admissions",
                    
                    "Mean Episode Duration",
                    "Median Episode Duration",
                    "Bed
Days")] |> 
  rename(
    Finished_Episodes = "Finished\nEpisodes",
    Mean_Episode_Duration = "Mean Episode Duration",
    Bed_Days = "Bed\nDays"
  ) |>
  filter(
    Title %in% c(
      "Diabetes mellitus",
      "Diseases of the circulatory system (I00-I99)",
      "Neoplasms (C00-D48)",
      "Diseases of the musculoskeletal system and connective tissue (M00-M99)",
      "Diseases of the kidney",
      "Chronic lower respiratory diseases"
      
    )
  ) |> 
  mutate(
    LOS_estimate = as.numeric(Finished_Episodes)/as.numeric(Admissions) * Mean_Episode_Duration)

stats2223 <- stats

# If we know incidence for 2025 
# then we can scale to our estimates for 2035 
## and apply the multiplier 
# to bed days 
# episodes 
# and admissions 

stats2223$broad <- c( 'cancer', 'cvd', 'cvd', 'resp', 'msk', 'cvd')
setDT(stats2223)

stats2223_agg <- stats2223[
  ,
  .(
    Finished_Episodes        = sum(as.numeric(Finished_Episodes), na.rm = TRUE),
    Emergency                = sum(as.numeric(Emergency), na.rm = TRUE),
    Admissions               = sum(as.numeric(Admissions), na.rm = TRUE),
    Mean_Episode_Duration    = mean(as.numeric(Mean_Episode_Duration), na.rm = TRUE),
    `Median Episode Duration`= median(as.numeric(`Median Episode Duration`), na.rm = TRUE),
    Bed_Days                 = sum(as.numeric(Bed_Days), na.rm = TRUE),
    LOS_estimate             = mean(as.numeric(LOS_estimate), na.rm = TRUE)
  ),
  by = broad
]

write.fst(stats2223_agg,path='./6_post_main/post_evaluation_module/preprocess/stats2223_agg.fst')
#395,000,000
stats_dt <- res_dt[min(year) == year,][stats2223_agg, on = .(broad),allow.cartesian=F]

stats_dt <- stats_dt[, .(broad,
       bed_days_per_case = Bed_Days/(prevalence*model_specification$population$scale_down_factor),
       admissions_per_case = Admissions/(prevalence*model_specification$population$scale_down_factor),
       emergency_admissions_per_case = Emergency/(prevalence*model_specification$population$scale_down_factor))]


res_dt2
stats_dt

# metric_card_total_bed_days <- metric_card(obesity_stats$total_bed_days,'', 'Total Bed Days')
# metric_card_avg_w_mean_duration <-  metric_card(obesity_stats$avg_w_mean_duration , 'days', 'Average Episode')
# metric_card_avg_W_LOS_estimate <- metric_card(obesity_stats$avg_W_LOS_estimate ,'', 'Obesity weighted LOS', 'Estimate')
# metric_card_total_admissions <- metric_card(obesity_stats$total_admissions , '','Total Admissions')
# metric_card_total_episodes <- metric_card(obesity_stats$total_episodes, '', 'Total Consultant Episodes')

