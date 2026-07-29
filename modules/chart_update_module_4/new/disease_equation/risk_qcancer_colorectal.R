# Colorectal cancer
# Female

  risk_qcancer_colorectal_female <- function(
      age = NULL,
      ethrisk = 1,
      smoke_cat =1,
      alcohol_cat6 = 1,
      age_1_fh_gicancer = 0,
      age_2_fh_gicancer = 0,
      b_breastcancer = 0,
      b_cervicalcancer = 0,
      b_colitis = 0,
      b_ovariancancer = 0,
      b_polyp = 0,
      b_type2 = 0,
      b_uterinecancer = 0,
      fh_gicancer = 0,
      surv = 1
  )
{
    
  survivor = c(
    # 0,
    0.999769449234009,
    0.999535322189331,
    0.999294936656952,
    0.999053359031677,
    0.998782038688660
  )
  
  # /* The conditional arrays */
    
     Ialcohol = c(
      #0,
      0.0169323170224846170000000,
      0.0504716118793323290000000,
      0.0813022183502749850000000,
      0.3188427641003217500000000,
      0.3071138493330506400000000
     )
     
   Iethrisk = c(
    #0,
    0,
    -1.0494971746256023000000000,
    -0.7580085872167761100000000,
    -0.1620641824282297600000000,
    -0.5207551589289394200000000,
    -0.3390081073116003000000000,
    -0.3663991962571118100000000,
    -0.4962552732151416800000000,
    -0.2228977014850979700000000
   )
   
   Ismoke = c(
    #0,
    0.0679430658242369530000000,
    0.1001705670170613500000000,
    0.1940325149380217600000000,
    0.1609524248529591600000000
  )
  
  # /* Applying the fractional polynomial transforms */
  #   /* (which includes scaling)                      */
    
  dage = age
  dage = dage/10
  age_1 = dage ** (-2)
  age_2 = dage ** (-2) * log(dage)
  
  # /* Centring the continuous variables */
    
  age_1 = age_1 - 0.049712907522917
  age_2 = age_2 - 0.074606411159039
  
  # /* Start of Sum */
    a =0
  
  # /* The conditional sums */
    
  a  = a + Ialcohol[alcohol_cat6]
  a  = a + Iethrisk[ethrisk]
  a  = a + Ismoke[smoke_cat]
  
  # /* Sum from continuous values */
    
  a  = a + age_1 * 33.9278823480095400000000000
  a  = a + age_2 * -83.9240508519935560000000000
  
  # /* Sum from boolean values */
    
  a  = a + age_1_fh_gicancer * 10.9854968919725080000000000
  a  = a + age_2_fh_gicancer * -0.0948979335841994980000000
  a  = a + b_breastcancer * 0.1509613687361799900000000
  a  = a + b_cervicalcancer * 0.5513465936958277200000000
  a  = a + b_colitis * 0.5608754512000964100000000
  a  = a + b_ovariancancer * 0.6840022206721720900000000
  a  = a + b_polyp * 0.7490409118794703100000000
  a  = a + b_type2 * 0.1482878087477876300000000
  a  = a + b_uterinecancer * 0.4792395386564791100000000
  a  = a + fh_gicancer * 0.6616045248544631900000000
  
  # /* Sum from interaction terms *
    
    # /* Calculate the score itself */
    score =  1 - (survivor[surv]** exp(a)) 
              
  return( score )
  
  }


# Male

  risk_qcancer_colorectal_male <- function( age = NULL,
                                            town = NULL,
                                            ethrisk = NULL,
                                            bmi = 25,
                                            alcohol_cat6 = 1,
                                            smoke_cat = 1,
                                            age_1_fh_gicancer = 0,
                                            age_2_fh_gicancer = 0,
                                            b_bloodcancer = 0,
                                            b_colitis = 0,
                                            b_lungcancer = 0,
                                            b_oralcancer = 0,
                                            b_polyp = 0,
                                            b_type2 = 0,
                                            fh_gicancer = 0,
                                            surv = 5
                                            ) {
    survivor = c(
     # 0, 
    0.999779522418976,
    0.999549508094788,
    0.999336361885071,
    0.999095499515533,
    0.998839616775513
    )
  
  # /* The conditional arrays */
    
    Ialcohol = c(
      #0,
      0.0479227173271524270000000,
      0.1306450076527492000000000,
      0.2642416976066509500000000,
      0.4824807862366953800000000,
      0.4433887052486962800000000
    )
    
  Iethrisk = c(
    # 0,
    0,
    -0.5852347175990283400000000,
    -0.5864337780506865300000000,
    -0.8811080936145391200000000,
    -0.4825211311076027000000000,
    -0.3502985702195468600000000,
    -0.2868643042389117900000000,
    -0.2125845990082229600000000,
    -0.5250625721689498000000000
  )
  
  Ismoke = c(
    # 0,
    0.0544365203781649460000000,
    0.0669679839432098520000000,
    0.0268254930577433230000000,
    0.1230134719805828800000000
  )
  
  # /* Applying the fractional polynomial transforms */
  #   /* (which includes scaling)                      */
    
  dage = age
  dage=dage/10
  age_1 = dage
  age_2 = (dage**2)
  
  # /* Centring the continuous variables */
    
  age_1 = age_1 - 4.425716876983643
  age_2 = age_2 - 19.586969375610352
  bmi = bmi - 26.309040069580078
  town = town - 0.260672301054001

  # /* Start of Sum */
     a = 0

  # /* The conditional sums */
    
  a  = a + Ialcohol[alcohol_cat6]
  a  = a + Iethrisk[ethrisk]
  a  = a + Ismoke[smoke_cat]
  
  # /* Sum from continuous values */

  a  = a + age_1 * 2.6296558591088894000000000
  a  = a + age_2 * -0.1495728139219066300000000
  a  = a + bmi * 0.0156452119427206510000000
  a  = a + town * 0.0089992920978670451000000

  # /* Sum from boolean values */
  
  a  = a + age_1_fh_gicancer * -1.62770210208022430000000030
  a  = a + age_2_fh_gicancer * 0.1248377530713149100000000
  a  = a + b_bloodcancer * 0.4284078752708641600000000
  a  = a + b_colitis * 0.6016726220869758100000000
  a  = a + b_lungcancer * 0.6273185618450067800000000
  a  = a + b_oralcancer * 0.4825177045812838500000000
  a  = a + b_polyp * 0.4092716863938517000000000
  a  = a + b_type2 * 0.2389208244341949300000000
  a  = a + fh_gicancer * 0.7802989526167445300000000
  
  # /* Sum from interaction terms */
    
    # /* Calculate the score itself */
  score =  (1 - (survivor[surv] ** exp(a)) )
  
  return(score)
  
  }

  
  risk_qcancer_colorectal <- function(age=NULL,
                                      sex=NULL,
                                      town = NULL,
                                      bmi = 25,
                                      ethrisk = 1,
                                      alcohol_cat6 = 1,
                                      smoke_cat = 1,
                                      age_1_fh_gicancer = 0,
                                      age_2_fh_gicancer = 0,
                                      b_polyp = 0,
                                      b_type2 = 0,
                                      b_bloodcancer = 0,
                                      b_colitis = 0,
                                      b_lungcancer = 0,
                                      b_oralcancer = 0,
                                      fh_gicancer = 0,
                                      b_breastcancer = 0,
                                      b_cervicalcancer = 0,
                                      b_ovariancancer = 0,
                                      b_uterinecancer = 0,
                                      surv = 5
                                      ){
    
    if(sex=='Males'){
      
      risk_qcancer_colorectal_male(age = age,
                                    age_1_fh_gicancer = age_1_fh_gicancer,
                                    age_2_fh_gicancer = age_2_fh_gicancer,
                                    alcohol_cat6 = alcohol_cat6,
                                    town = town,
                                    b_bloodcancer = b_bloodcancer,
                                    b_colitis = b_colitis,
                                    b_lungcancer = b_lungcancer,
                                    b_oralcancer = b_oralcancer,
                                    b_polyp = b_polyp,
                                    b_type2 = b_type2,
                                    bmi = bmi,
                                    ethrisk = ethrisk,
                                    fh_gicancer = fh_gicancer,
                                    smoke_cat = smoke_cat,
                                    surv = surv
      )
    } else{
      risk_qcancer_colorectal_female(
                                    age = age,
                                    age_1_fh_gicancer = age_1_fh_gicancer,
                                    age_2_fh_gicancer = age_2_fh_gicancer,
                                    alcohol_cat6 = alcohol_cat6,
                                    b_breastcancer = b_breastcancer,
                                    b_cervicalcancer = b_cervicalcancer,
                                    b_colitis = b_colitis,
                                    b_ovariancancer = b_ovariancancer,
                                    b_polyp = b_polyp,
                                    b_type2 = b_type2,
                                    b_uterinecancer = b_uterinecancer,
                                    ethrisk = ethrisk,
                                    fh_gicancer = fh_gicancer,
                                    smoke_cat = smoke_cat,
                                    surv = surv
      )
    }
  }

  apply_colorectal_cancer_risk_wo_risk_factors <- function(input_population, intervention=1){

    postp1 <- 
      input_population %>% 
      filter(year == max(year,na.rm = TRUE))

    postp <- postp1 %>%
      mutate(
        age = ifelse(age > 90, 90, age),
        sex,
        bmi = case_when(
          bmi == "normal"     ~ 22.5,
          bmi == "overweight" ~ 28,
          bmi == "obese"       ~ 38,
          TRUE ~ NA_real_),
        alcohol_cat6 = case_when(
          alcohol == 'no_risk'  ~ 1,
          alcohol == 'lower_risk' ~ 3,
          alcohol == 'higher_risk'  ~ 5,
          TRUE                           ~ 1),
        b_type2 = case_when(
          diabetes_status == 'diagnosed_diabetes' ~ 1,
          diabetes_status == 'undiagnosed_diabetes' ~ 1,
          diabetes_status == 'no_diabetes' ~0,
          TRUE ~ 0
        ),
        smoke_cat = case_when(
          smoking == 'current_smoker' ~ 4,
          smoking == 'former' ~ 2,
          smoking == 'never_smoked' ~1,
          TRUE ~ 1
        ),
        b_bloodcancer = blood_cancer!=0,
        b_lungcancer = lung_cancer!=0,
        b_oralcancer = oral_cancer!=0,
        b_breastcancer = female_breast_cancer!=0,
        # b_cervicalcancer
        b_ovariancancer = ovarian_cancer != 0,
        b_uterinecancer = uterine_cancer != 0,
        custom_townsend_score_dz, 
        ethrisk) 

    postp <- postp %>% 
      rowwise() %>% 
      # mutate(list(print(c(age,sex) )))%>% 
      mutate(colorectal_cancer_risk =
               ifelse(age < 40, 0,
                      risk_qcancer_colorectal(  
                          age = age, 
                          sex = sex,
                          town = custom_townsend_score_dz,
                          ethrisk = ethrisk,
                          bmi = bmi,
                          alcohol_cat6 = alcohol_cat6,
                          b_type2 = b_type2,
                          smoke_cat = smoke_cat
                          
                      )
              ) *0.1
      )
    
    input_population <- input_population |> 
      select(-any_of('colorectal_cancer_risk'))
    
    input_population <- left_join(input_population,
                                  postp[c('colorectal_cancer_risk','id')], 
                                  by ='id')

    input_population <- ungroup(input_population)
    
    return(input_population)
  }
  
  # test_population |> 
  #   apply_colorectal_cancer_risk_wo_risk_factors() #|> names()
    
  
 apply_colorectal_risk <- function(y, intervention = 1 ) {
   
   postp1 <- 
     y %>% 
     filter(year == max(year,na.rm = TRUE)) %>% 
     pivot_longer(cols = - any_of(base_population_demographic_column_names)
     ) %>% 
     mutate(category = str_extract(string = name,pattern = 'cholesterol|overweight|bp|smoking|diabetic|atrial_fibrillation')) %>% 
     group_by(id,category) %>% 
     arrange(desc(value)) %>% 
     slice_head() %>%
     ungroup() %>% 
     pivot_wider(id_cols = -value,names_from = category,values_from = name) 
   
   dim(y)
   # 2117   31
   dim( postp1)
   # 2111   24
   
   postp <- postp1 %>%
     mutate(
       # sex = case_when(
       # sex == 'Women'~1,
       # sex == 'Men'),
       bp=case_when(bp == 'high_bp'~150,
                    bp == 'normal_bp'~100),
       cholesterol=case_when(
         cholesterol == 'normal_cholesterol'~3.5,
         cholesterol == 'high_cholesterol'~10),
       type2 = case_when(
         diabetic == 'non_diabetic'~0,
         diabetic == 'diabetic'~1),
       bmi=case_when(overweight=='overweight'~35,
                     overweight=='not_overweight'~20),
       smoking=case_when(
         smoking=='smoking_currently'~4,
         smoking=='smoking_used'~2,
         smoking=='smoking_never'~1),
       atrial_fibrillation=case_when(
         atrial_fibrillation=='atrial_fibrillation'~(1*intervention)) # change back to `intervention`
     ) 
   
   postp <- postp %>% 
     rowwise() %>% 
     # mutate(list(print(c(age,sex) )))%>% 
     mutate(colorectal_cancer_risk =
              case_when(age < 30 ~ 0,
                        age > 85 ~ 0.5,
                        T ~ risk_qcancer_colorectal(
                          age = age, 
                          sex = sex,
                          bmi = bmi, 
                          smoke_cat = smoking, 
                          #cholesterol_ratio = cholesterol, 
                          #sbp = bp, 
                          #b_type2 = type2, 
                          town = townsend_score, 
                          #b_AF = atrial_fibrillation 
                        )),
            valid= case_when(age < 30 ~ 'shouldnt',
                             age > 85 ~ 'shoudlnt',
                             T ~ 'should')
            
     )
   
   y <- left_join(y,postp[c('colorectal_cancer_risk','id')], by ='id')
   # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
   # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
   
   return(y)
 }
 
 # risk_qcancer_colorectal(40,'Females')
 # risk_qcancer_colorectal(40,'Males')
 # 
 # 
 # risk_qcancer_colorectal_male(40)
 # risk_qcancer_colorectal_female(40)
