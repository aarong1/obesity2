qmortality_female_risk <- function(
                       age = NULL,
                       town = NULL,
                       bmi = 25, 
                       alcohol_cat6 = 1,
                       ethrisk = 1, 
                       smoke_cat = 1, 
                       surv = 1, 
                       b_AF = 0,  
                       b_CCF = 0, #congestive cardiac failure  / heart failure
                       b_anycancer = 0,
                       b_asthmacopd = 0, 
                       # b_carehome = 0, 
                       # b_antipsychotic = 0, 
                       # b_corticosteroids = 0,
                       b_cvd = 0,
                       b_dementia = 0,
                       b_epilepsy = 0,
                       # b_learning = 0,
                       # b_legulcer = 0,
                       b_liverpancreas = 0, 
                       b_parkinsons = 0, 
                       # b_poormobility = 0, 
                       b_ra = 0, # Rheumatoid arthritis 
                       b_renal = 0, 
                       b_type1 = 0,
                       b_type2 = 0, 
                       b_vte = 0 #,
                       # c_hb = 0, 
                       # hes_admitprior_cat = 0,
                       # high_lft = 0, 
                       # high_platlet = 0,
                       # s1_appetiteloss = 0,
                       # s1_dyspnoea = 0,
                       # s1_weightloss = 0
                       ) {
  # baseline survivors at years 1 and 2 (0-indexed in C)
  survivors <- c(#0,
                 0.984712481498718,
                 0.971753656864166)
  
  # conditional coefficients (C arrays are zero-based)
  Ialcohol <- c(#0,
                -0.17435199441964358,
                -0.20379859150036400,
                -0.14878661948505154,
                0.19346168639158545,
                0.38177192393537790)
  Iethrisk <- c(#0, 
                0,
                -0.11843111221404919,
                -0.00799935876788652,
                -0.08190248439786728,
                -0.20319672746383435,
                -0.28785222648805714,
                -0.38639173968228352,
                -0.63523396862698500,
                -0.14311168318806736)
  Ihesprior <- c(#0,
                 0.71833452784520391,
                 1.09080315448708310,
                 1.54518281409233980)
  Ismoke <- c(#0,
              0.16717454421875455,
              0.58142938294258628,
              0.72878990923139852,
              0.82857372978610067)
  
  # fractional polynomial transforms
  dage   <- age / 10
  age_1  <- dage^(-2)
  age_2  <- dage^(-2) * log(dage)
  dbmi   <- bmi / 10
  bmi_1  <- dbmi^(-1)
  bmi_2  <- dbmi^(-0.5)
  
  # center continuous terms
  age_1  <- age_1  - 0.017261177301407
  age_2  <- age_2  - 0.035034108906984
  bmi_1  <- bmi_1  - 0.369150280952454
  bmi_2  <- bmi_2  - 0.607577383518219
  town   <- town   - (-0.626556754112244)  # adds 0.626556754112244
  
  # start sum
  a <- 0
  
  # conditional contributions
  a <- a + Ialcohol    [alcohol_cat6]
  a <- a + Iethrisk    [ethrisk]
  # a <- a + Ihesprior   [hes_admitprior_cat]
  a <- a + Ismoke      [smoke_cat]
  
  # continuous contributions
  a <- a + age_1  * 1508.5794831250275
  a <- a + age_2  * -1177.0684822755481
  a <- a + bmi_1  *   21.11334055392112
  a <- a + bmi_2  *  -24.573072524627506
  a <- a + town   *    0.02409076971296671
  
  # boolean contributions (multiply 0/1 flags)
  a <- a + b_AF             * 0.33252351760672627
  a <- a + b_CCF            * 0.50641683153270323
  # a <- a + b_antipsychotic  * 0.47626506440705629
  a <- a + b_anycancer      * 0.64720089405422188
  a <- a + b_asthmacopd     * 0.17799721514652003
  # a <- a + b_carehome       * 0.58739598785067482
  # a <- a + b_corticosteroids* 0.36337872520297626
  a <- a + b_cvd            * 0.27105365193809716
  a <- a + b_dementia       * 0.95977317585312327
  # a <- a + b_epilepsy       * 0.20115762112157234
  # a <- a + b_learning       * 0.13839328199993772
  # a <- a + b_legulcer       * 0.47612727692631140
  a <- a + b_liverpancreas  * 0.47403357578242461
  a <- a + b_parkinsons     * 0.59090563937374740
  # a <- a + b_poormobility   * 0.49004084666858500
  a <- a + b_ra             * 0.25677327584936344
  a <- a + b_renal          * 0.67716274437051516
  a <- a + b_type1          * 0.31278391181876186
  a <- a + b_type2          * 0.29737933061072769
  a <- a + b_vte            * 0.17800465837401402
  # a <- a + c_hb             * 0.65864248937343151
  # a <- a + high_lft         * 0.47612237163655469
  # a <- a + high_platlet     * 0.30756728264623784
  # a <- a + s1_appetiteloss  * 0.26034225706835656
  # a <- a + s1_dyspnoea      * 0.28273682823800972
  # a <- a + s1_weightloss    * 0.22416624109881766
  
  # interaction terms
  # a <- a + age_1 * (hes_admitprior_cat == 1) * 0.035158394211054912
  # a <- a + age_1 * (hes_admitprior_cat == 2) * -109.51688232773282
  # a <- a + age_1 * (hes_admitprior_cat == 3) * -351.35461904849859
  a <- a + age_1 * b_CCF            * -19.172161434263753
  # a <- a + age_1 * b_antipsychotic  * -133.23319710519135
  a <- a + age_1 * b_anycancer      * 197.79764343812712
  # a <- a + age_1 * b_carehome       *   8.764588290166934
  # a <- a + age_1 * b_corticosteroids* -84.459168120389265
  a <- a + age_1 * b_cvd            * -152.38387535241887
  a <- a + age_1 * b_dementia       * -487.37456941797240
  # a <- a + age_1 * b_legulcer       *  -18.80467316840821
  a <- a + age_1 * b_liverpancreas  * -198.99668832418308
  # a <- a + age_1 * b_poormobility   * -277.25098146493110
  a <- a + age_1 * b_renal          * -324.78820931977253
  a <- a + age_1 * b_type2          * -338.29925143948833
  # a <- a + age_1 * c_hb             * -276.08981163116158
  # a <- a + age_1 * high_lft         *  -49.23010780354005
  # a <- a + age_1 * s1_dyspnoea      * -302.95755539422584
  a <- a + age_1 * town             *  -30.92145223239321
  # a <- a + age_2 * (hes_admitprior_cat == 1) *  38.37046285901554
  # a <- a + age_2 * (hes_admitprior_cat == 2) * 124.56245187208052
  # a <- a + age_2 * (hes_admitprior_cat == 3) * 301.56145302592375
  a <- a + age_2 * b_CCF            *  35.69541514351736
  # a <- a + age_2 * b_antipsychotic  * 108.84530135283519
  a <- a + age_2 * b_anycancer      * -67.15451100028675
  # a <- a + age_2 * b_carehome       *  35.01251818709047
  # a <- a + age_2 * b_corticosteroids*  84.34640594913971
  a <- a + age_2 * b_cvd            * 107.40673575348774
  a <- a + age_2 * b_dementia       * 366.22552882732964
  # a <- a + age_2 * b_legulcer       *  43.78568959541693
  a <- a + age_2 * b_liverpancreas  * 175.39443266246101
  # a <- a + age_2 * b_poormobility   * 201.44123891115208
  a <- a + age_2 * b_renal          * 249.23729077547392
  a <- a + age_2 * b_type2          * 228.42578996830144
  # a <- a + age_2 * c_hb             * 207.01357837124425
  # a <- a + age_2 * high_lft         *  63.15071490997980
  # a <- a + age_2 * s1_dyspnoea      * 220.40087676403087
  a <- a + age_2 * town             *  22.53883663039359
  
  # final risk
  score = (1 - survivors[surv] ^ exp(a))
  return(score)
}

qmortality_male_risk <- function(
                     age =  NULL,
                     town = NULL,
                     bmi = 25,
                     alcohol_cat6 = 1,
                     smoke_cat = 1, 
                     ethrisk = 1, 
                     surv = 1,
                     b_AF = 0, 
                     b_CCF = 0, 
                     # b_antipsychotic = 0,
                     b_anycancer = 0,
                     b_asthmacopd = 0,
                     # b_carehome = 0,
                     # b_corticosteroids = 0, 
                     b_cvd = 0,
                     b_dementia = 0,
                     b_epilepsy = 0,
                     # b_learning = 0, 
                     # b_legulcer = 0,
                     b_liverpancreas = 0,
                     b_parkinsons = 0, 
                     # b_poormobility = 0, 
                     b_ra = 0,
                     b_renal = 0,
                     b_type1 = 0, 
                     b_type2 = 0, 
                     b_vte = 0 #,
                     # c_hb = 0, 
                     # hes_admitprior_cat = 0,
                     # high_lft = 0, 
                     # high_platlet = 0,
                     # s1_appetiteloss = 0,
                     # s1_dyspnoea = 0,
                     # s1_weightloss = 0
                     ) {
  survivors <- c(#0,
                 0.979335904121399,
                 0.962403774261475)
  
  Ialcohol <- c(#0,
                -0.16708444071266923,
                -0.19847754653430327,
                -0.14812069089436303,
                0.10971438529866195,
                0.15725394819345351)
  Iethrisk <- c(#0, 
                0,
                -0.24965163717239661,
                -0.26049990463239797,
                -0.24133720755286936,
                -0.40394222834821164,
                -0.34697110988245755,
                -0.34603088592097347,
                -0.44272774017815186,
                -0.28942689209683875)
  Ihesprior <- c(#0,
                 0.68934794175929393,
                 1.05376621353689950,
                 1.44060870845246410)
  Ismoke <- c(#0,
              0.16732266494368642,
              0.53626307622435287,
              0.64071300129429687,
              0.76142775355784531)
  
  dage   <- age / 10
  age_1  <- dage^3
  age_2  <- dage^3 * log(dage)
  dbmi   <- bmi / 10
  bmi_1  <- dbmi^(-2)
  bmi_2  <- dbmi^(-2) * log(dbmi)
  
  age_1  <- age_1 - 412.15957641601562
  age_2  <- age_2 - 827.26068115234375
  bmi_1  <- bmi_1 -   0.134313449263573
  bmi_2  <- bmi_2 -   0.134822428226471
  town   <- town  - (-0.768538892269135)
  
  a <- 0
  a <- a + Ialcohol    [alcohol_cat6]
  a <- a + Iethrisk    [ethrisk]
  # a <- a + Ihesprior   [hes_admitprior_cat]
  a <- a + Ismoke      [smoke_cat]
  
  a <- a + age_1  *  0.036449264416623731
  a <- a + age_2  * -0.012551397612884483
  a <- a + bmi_1  *  8.3843196334505148
  a <- a + bmi_2  * -14.491169010690488
  a <- a + town   *  0.035864309976869284
  
  a <- a + b_AF             * 0.24652795256939328
  a <- a + b_CCF            * 0.55403227487129214
  # a <- a + b_antipsychotic  * 0.46739784638231002
  a <- a + b_anycancer      * 0.71554141921220937
  a <- a + b_asthmacopd     * 0.14048940720646091
  # a <- a + b_carehome       * 0.47749740140274688
  # a <- a + b_corticosteroids* 0.44814187560602808
  a <- a + b_cvd            * 0.21949118375326637
  a <- a + b_dementia       * 0.85224240410802432
  # a <- a + b_epilepsy       * 0.21978453251695079
  # a <- a + b_learning       * 0.19790563863902949
  # a <- a + b_legulcer       * 0.50725636587795975
  a <- a + b_liverpancreas  * 0.39263687583033940
  a <- a + b_parkinsons     * 0.76990750076242376
  # a <- a + b_poormobility   * 0.46367032348911708
  a <- a + b_ra             * 0.17109771611058100
  a <- a + b_renal          * 0.62224745945823434
  a <- a + b_type1          * 0.25960721267267589
  a <- a + b_type2          * 0.25404811490464563
  a <- a + b_vte            * 0.14940777815558187
  # a <- a + c_hb             * 0.74294697835779089
  # a <- a + high_lft         * 0.51554764782943019
  # a <- a + high_platlet     * 0.32097996321768341
  # a <- a + s1_appetiteloss  * 0.30320102141305078
  # a <- a + s1_dyspnoea      * 0.25023795777686370
  # a <- a + s1_weightloss    * 0.21457581118166616
  
  # a <- a + age_1 * (hes_admitprior_cat == 1) * -0.013532242327052574
  # a <- a + age_1 * (hes_admitprior_cat == 2) * -0.016429972869646901
  # a <- a + age_1 * (hes_admitprior_cat == 3) * -0.018419712704968640
  a <- a + age_1 * b_CCF            * -0.0056375981438788851
  # a <- a + age_1 * b_antipsychotic  *  0.00095485764796618411
  a <- a + age_1 * b_anycancer      * -0.031427877556200419
  # a <- a + age_1 * b_carehome       *  0.0029194351023528052
  # a <- a + age_1 * b_corticosteroids* -0.0087098588393828367
  a <- a + age_1 * b_cvd            * -0.00092605474117238176
  a <- a + age_1 * b_dementia       * -0.0042226968131332939
  # a <- a + age_1 * b_legulcer       * -0.0043408879675686162
  a <- a + age_1 * b_liverpancreas  * -0.011168661453064168
  a <- a + age_1 * b_parkinsons     *  0.00086079795284955377
  # a <- a + age_1 * b_poormobility   * -0.0032122568132298144
  a <- a + age_1 * b_renal          * -0.0062987873913596228
  a <- a + age_1 * b_type2          *  0.00085440051789764356
  # a <- a + age_1 * c_hb             * -0.0010907260684175104
  # a <- a + age_1 * high_lft         * -0.01652938675188878
  # a <- a + age_1 * s1_dyspnoea      * -0.000075221143666536882
  a <- a + age_1 * town             * -0.00026717600937716028
  # a <- a + age_2 * (hes_admitprior_cat == 1) *  0.0052599500739404469
  # a <- a + age_2 * (hes_admitprior_cat == 2) *  0.0062723155776391801
  # a <- a + age_2 * (hes_admitprior_cat == 3) *  0.0068996134993599611
  a <- a + age_2 * b_CCF            *  0.0020360323212952713
  # a <- a + age_2 * b_antipsychotic  * -0.00059264792400968123
  a <- a + age_2 * b_anycancer      *  0.0122622021504616
  # a <- a + age_2 * b_carehome       * -0.0014190910591964706
  # a <- a + age_2 * b_corticosteroids*  0.0031204374807641592
  a <- a + age_2 * b_cvd            *  0.00028128230371338202
  a <- a + age_2 * b_dementia       *  0.0011909366650632758
  # a <- a + age_2 * b_legulcer       *  0.0014618943884676750
  a <- a + age_2 * b_liverpancreas  *  0.0042685310892839612
  a <- a + age_2 * b_parkinsons     * -0.00071904072749267352
  # a <- a + age_2 * b_poormobility   *  0.0010674563935602573
  a <- a + age_2 * b_renal          *  0.0021250162138427274
  a <- a + age_2 * b_type2          * -0.00049606890104562657
  # a <- a + age_2 * c_hb             *  0.0000336997335926709
  # a <- a + age_2 * high_lft         *  0.0065189455055333261
  # a <- a + age_2 * s1_dyspnoea      * -0.00015312682439225768
  a <- a + age_2 * town             *  0.00006758392503168273
  
  score <- (1 - survivors[surv] ^ exp(a))
  return(score)
}


qmortality_risk <- function( sex = NULL,
                             age =  NULL,
                             town = NULL, 
                             bmi = 25,
                             alcohol_cat6 = 1, 
                             smoke_cat = 1, 
                             ethrisk = 1, 
                             surv = 1,
                             
                             b_AF = 0, 
                             b_CCF = 0, 

                             b_anycancer = 0,
                             b_asthmacopd = 0,

                             b_cvd = 0,
                             b_dementia = 0,
                             b_epilepsy = 0,

                             b_liverpancreas = 0,
                             b_parkinsons = 0, 
                             
                             b_ra = 0,
                             b_renal = 0,
                             b_type1 = 0, 
                             b_type2 = 0, 
                             b_vte = 0 

                             ){
  if(sex == 'Males'){
    
    risk <- qmortality_male_risk( age = age,
                          town = town, 
                          alcohol_cat6 = alcohol_cat6, 
                          bmi = bmi,
                          smoke_cat = smoke_cat, 
                          surv = surv,
                          ethrisk = ethrisk, 
                          b_AF = b_AF, 
                          b_CCF = b_CCF, 

                          b_anycancer = b_anycancer,
                          b_asthmacopd = b_asthmacopd,

                          b_cvd = b_cvd,
                          b_dementia = b_dementia,

                          b_liverpancreas = b_liverpancreas,
                          b_parkinsons = b_parkinsons, 
                          
                          b_ra = b_ra,
                          b_renal = b_renal,
                          b_type1 = b_type1,
                          b_type2 = b_type2, 
                          b_vte = b_vte 

                          )
    
  } else if(sex == 'Females'){
    
    risk <- qmortality_female_risk(   age = age,
                              town = town, 
                              bmi = bmi,
                              alcohol_cat6 = alcohol_cat6, 
                              smoke_cat = smoke_cat, 
                              ethrisk = ethrisk, 
                              surv = surv,
                              b_AF = b_AF, 
                              b_CCF = b_CCF, 
                              # b_antipsychotic = 0,
                              b_anycancer = b_anycancer,
                              b_asthmacopd = b_asthmacopd,
                              # b_carehome = 0,
                              # b_corticosteroids = 0, 
                              b_cvd = b_cvd,
                              b_dementia = b_dementia,
                              # b_epilepsy = 0, 
                              # b_learning = 0, 
                              # b_legulcer = 0,
                              b_liverpancreas = b_liverpancreas,
                              b_parkinsons = b_parkinsons,
                              # b_poormobility = 0, 
                              b_ra = b_ra,
                              b_renal = b_renal,
                              b_type1 = b_type1,
                              b_type2 = b_type2, 
                              b_vte = b_vte #,
                              # c_hb = 0, 
                              # hes_admitprior_cat = 0,
                              # high_lft = 0, 
                              # high_platlet = 0,
                              # s1_appetiteloss = 0,
                              # s1_dyspnoea = 0,
                              # s1_weightloss = 0
                              )
    
  }
  
  # replace undefined ages with lieftables scores (absent deprivation)
  
  if( (age < 50 & sex == 'Males') | (age<60 & sex == 'Females') ){

    risk <- lifetables |>
      filter(age=={{age}},sex=={{sex}}) |> 
      pull(qx)
    
  }else if(age == 0) {
   #risk <- 0 
  }
  
  return(risk)
}




apply_qmortality_mortality <- function(input_population, apply_death = F ){
  
  max_year = max(input_population$year)
  
  postp <- input_population |>
    #select(-any_of('qx')) |> 
    mutate(bmi = case_when(
      bmi == "normal"     ~ 22.5,
      bmi == "overweight" ~ 28,
      bmi == "obese"       ~ 38,
      TRUE ~ 22),
      
      b_CCF = heart_failure != 0,
      b_asthmacopd =  (asthma!=0  | copd!=0),
      b_anycancer = cancer != 0,
      b_cvd = (chd != 0 | stroke != 0),
      b_AF = af_status == 'af',
      b_dementia = dementia != 0,
      b_type1 = 0,
      b_type2 = diabetes_status != 0,
      b_renal = ckd_status != 'no_ckd'
      ) |> 
    rowwise() |>
    mutate(qmortality_risk = 0.5 * qmortality_risk(age = age,
                                             sex = sex,
                                             town = townsend_score,
                                             bmi = bmi,
                                             b_CCF = b_CCF,
                                             b_asthmacopd = b_asthmacopd,
                                             b_anycancer = b_anycancer,
                                             b_cvd = b_cvd,
                                             b_AF = b_AF,
                                             b_dementia = b_dementia,
                                             b_type1 = 0,
                                             b_type2 = b_type2,
                                             b_renal = b_renal
    )
    ) |> 
    #mutate(qmortality_risk = ifelse(age<55, qx, qmortality_risk)) |> 
    ungroup()
  
  if (apply_death){
    
    postp <- postp |> 
      select(-any_of('death'))
    
    postp <- postp |> 
      mutate(death =  max_year * (bern_trial < qmortality_risk)) 
  }
  
  input_population <- input_population |> 
    select(-any_of(c('death','qmortality_risk')))
  
  input_population <- left_join(input_population,
                                select(postp,
                                       any_of(
                                         c('qmortality_risk',
                                           'death',
                                           'id'))), 
                                by ='id')

  # initial_time_zero_population$diabetes_risk <- postp$diabetes_risk
  # initial_time_zero_population[!initial_time_zero_population$id %in% postp$id,]
  input_population <- ungroup(input_population)
  
  return(input_population)
  
}


not_function_just_hiding_example_calls <- function(){
  
qmortality_risk(
    sex = "Males",
    age = 30,
    town = -0.5,           # Townsend score (e.g., mildly deprived)
    bmi = 27,
    alcohol_cat6 = 1,      # Non-drinker
    smoke_cat = 1,         # Non-smoker
    ethrisk = 1,           # White
    surv = 1,              # 1-year survival
    b_CCF = 1,             # Congestive heart failure
    b_asthmacopd = 1       # Asthma/COPD
)
    
  qmortality_risk( sex = "Males", age = 75, town = 0, b_CCF = 1 ) 
  qmortality_risk( sex = "Males", age = 75, town = 0, b_asthmacopd = 1 ) 
  qmortality_risk( sex = "Males", age = 75, town = 0, b_cvd = 1 ) 
  qmortality_risk( sex = "Males", age = 75, town = 0, b_AF = 1 ) 
  qmortality_risk( sex = "Males", age = 75, town = 0, b_dementia = 1 ) 
  qmortality_risk( sex = "Males", age = 75, town = 0, b_type1 = 1, b_type2 = 1 ) 
  qmortality_risk( sex = "Males", age = 75, town = 0, b_renal = 1 ) 
  
  qmortality_risk( sex = "Females", age = 66, town = -3, b_anycancer = 1, b_cvd = 1 ) 
  
  
}


wrapping_examples_in_function <- function(){


qmortality_female_risk(age = 50, 3 )

  
expt_pop <- test_population |> 
  apply_age_sex_death()  |> 
  #apply_qmortality_mortality() |> 
  names()


expt_pop |>
  select(1:39 ,qmortality_risk,qx) |> 
  filter(qmortality_risk==1,age<65) # |> 
  # view()


expt_pop |> filter(qmortality_risk!=0) |> 
  ggplot() +
  geom_point(aes(age,qmortality_risk))


  expt_pop |> 
    ggplot() +
    geom_point(aes(qx,qmortality_risk,col=( age<65 & sex=='Males')),alpha= 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "blue", linetype = "dashed") +
    labs(title = "Line y = x")
  
}

# \text{HR} = e^{\beta}
# HR = exp(beta))








# -------------------------------------------------------------------------
## ----------- females risk explodes under 40-50 years old ------------- ##
# -------------------------------------------------------------------------


# age=40:100


# dage   <- age / 10
# age_1  <- dage^(-2)
# age_2  <- dage^(-2) * log(dage)
# 
# # center continuous terms
# age_1  <- age_1  - 0.017261177301407
# age_2  <- age_2  - 0.035034108906984
# 
# a <- 0
# 
# 
# # continuous contributions
# a <- a + age_1  * 1508.5794831250275
# a <- a + age_2  * -1177.0684822755481
#              
# 
# x = (1 - 0.984712481498718 ^ exp(a))
# 
# 
# plot(40:100, age_1* 1508.5794831250275)
# plot(40:100, age_2*-1177.0684822755481)
# 
# plot(40:100, age_1* 1508.5794831250275 +
#      age_2*-1177.0684822755481
# )
# 
