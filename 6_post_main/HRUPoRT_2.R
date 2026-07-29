testp <- past_populations %>%
  as.data.table() 

testp[,loghru = 
        5.0742
      + 0.3617 * (stroke !=0 | heart_failure !=0 | diabetes | COPD | cancer | ibd | wellbeing | dementia ) 
      + 0 * no chronic condition 
      + 0.2425 * (sex == 'Males') 
      + 0 * (sex == 'Females') 
      + 0.1202 * ( age > 30 & age < 39)
      + 0.9213 * ( age > 40 & age < 49)
      + 1.6306 * ( age > 50 & age < 59)
      + 2.3558 * ( age > 60 & age < 69)
      + 2.8083 * ( age > 70 & age < 79)
      + 3.6188 * ( age > 80
      + 0 * (age < 30)
      
      + c(0.5256, 0.4035, 0.2000, 0.2267, 0 )[income_dm_quintile]
      # +0.3841*missing income quintile
      -0.2426*(broad_ethnicity == 'minority')
      # +0.00983*missing ethnicity
      +0*(broad_ethnicity == 'white')
      # +0.2926*former heavy smoker
      +0.1296 * (smoking == 'former') #former light smoker
      # +0.4579*(smoking == current heavy smoker
      +0.2569 * (smoking == 'current_smoker') # current light smoker
      # -0.0661*missing smoking status
      +0*( smoking == 'never_smoked') #non-smoker
      # -0.2661*food secure
      # -0.7921*missing food security
      # +0*food insecure
      # +0.4080*Fair general health
      # +1.0628*Poor general health
      # +0.1470*missing general health
      # +0*excellent/very/good general health
      +0.1131 * (bmi == 'obese') #BMI 30.0-34.9 
      +0.0753 * (bmi == 'obese') #BMI 25.0-29.9
      +0.6384 * (bmi == 'obese') #BMI ≥40.0
      +0.0174 * (bmi == 'obese') #BMI <18.5
      +0.0959 * (bmi == 'obese') #BMI missing
      +0.3624 * (bmi == 'obese') #BMI 35.0-39.9
      +0 * (bmi == 'normal_weght') #BMI 18.5-24.9
      -0.0105*(alcohol == 'higher_risk') #heavy drinker
      -0.0757*(alcohol == 'increased_risk') #moderate drinker
      +0.0420*(alcohol == 'no_risk') #non-drinker
      # +0.0619*(alcohol == 'no_risk') #missing alcohol status
      +0*(alcohol == 'lower_risk') #light drinker
      +0.1766* (pa == 'inactive')#Bottom physical activity quartile
      -0.0110* (pa == 'low_activity')#Physical activity quartile 2
      +0.0172* (pa == 'some_activity')#Physical activity quartile 3
      +0*(pa == 'meets_rec') #Highest physical activity quartile
      # +0.3490*missing physical activity
      # -0.2358*Immigrant <10 years 
      # -0.0375*Immigrant  ≥10 years
      # +0.1781*missing immigrant status
      # +0*Canadian-born
        ]
  

"logit =
- 5.0742
+0.3617*has chronic condition
+0*no chronic condition
+0.2425*male
+0*female
+0.1202*aged 30-39
+0.9213*aged 40-49
+1.6306*aged 50-59
+2.3558*aged 60-69
+2.8083*aged 70-79
+3.6188*aged 80+
+0*aged <30
+0.5256* bottom income quintile
+0.4035*income quintile 2
+0.2000* income quintile 3
+0.2267* income quintile 4
+0.3841*missing income quintile
+0*highest income quintile
-0.2426*non-white ethnicity
+0.00983*missing ethnicity
+0*white ethnicity
+0.2926*former heavy smoker
+0.1296*former light smoker
+0.4579*current heavy smoker
+0.2569*current light smoker
-0.0661*missing smoking status
+0*non-smoker
-0.2661*food secure
-0.7921*missing food security
+0*food insecure
+0.4080*Fair general health
+1.0628*Poor general health
+0.1470*missing general health
+0*excellent/very/good general health
+0.1131*BMI 30.0-34.9 
+0.0753*BMI 25.0-29.9
+0.6384*BMI ≥40.0
+0.0174*BMI <18.5
+0.0959*BMI missing"
+0.3624*BMI 35.0-39.9
+0*BMI 18.5-24.9
-0.0105*heavy drinker
-0.0757*moderate drinker
+0.0420*non-drinker
+0.0619*missing alcohol status
+0*light drinker
+0.1766*Bottom physical activity quartile
-0.0110*Physical activity quartile 2
+0.0172*Physical activity quartile 3
+0.3490*missing physical activity
+0*Highest physical activity quartile
-0.2358*Immigrant <10 years 
-0.0375*Immigrant  ≥10 years
+0.1781*missing immigrant status
+0*Canadian-born
# 
# elogit =  exp(logit)
# 
# prob = elogit / ( 1 + elogit) 
# 
# nHRU = prob * survey weights
# 
# 
# Variable	Definition
# prob	Risk of HRU transition 
# (calculated by HRUPoRT algorithm)
# nHRU	Expected number of new HRU transitions
# 
# Calculate average HRU risk for the population:        
#   MeanRisk =  Ʃprobi /n
# 
# Calculate total number of new HRU transitions in the population:
#   TotalHRU = ƩnHRU