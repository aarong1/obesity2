##### Cancers #########
# blood
# Breast (F)
# Colorectal
# Lung
# Oesphageal-gastric
# Oral
# Ovarian (F)
# Pancreatic
# Prostate (M)
# Renal 
# Uterine (F)

# Blood cancer ----
## ## Female ----

  bloodcancer_female_raw <- function(
    age,
    b_braincancer,
    b_ovariancancer,
    b_type1,
    bmi,
    fh_bloodcancer,
    smoke_cat,
    surv)
{
  survivor = c(
    #0,
    0.999728083610535,
    0.999484181404114,
    0.999221444129944,
    0.998965024948120,
    0.998681783676147
  )
  
  # The conditional arrays 
    
    Ismoke = c(
      #0,
      0.0067585720587060227000000,
      0.1786060334216023600000000,
      0.2000236983380095400000000,
      0.2824295828543657000000000
    )
  
  # Applying the fractional polynomial transforms 
    # (which includes scaling)                      
    
  dage = age
  dage=dage/10
  age_1 = dage** (2)
  age_2 = dage** (3)
  
  # Centring the continuous variables 
    
    age_1 = age_1 - 20.135219573974609
  age_2 = age_2 - 90.351325988769531
  bmi = bmi - 25.724174499511719
  
  # Start of Sum 
    a=0
  
  # The conditional sums 
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values 
    
    a = a + age_1 * 0.1837588586730924700000000
  a = a + age_2 * -0.0147172575575694360000000
  a = a + bmi * 0.0162413827296514320000000
  
  # Sum from boolean values 
    
    a = a + b_braincancer * 1.4166705727938707000000000
  a = a + b_ovariancancer * 0.4611088666746058200000000
  a = a + b_type1 * 0.4133397797532169000000000
  a = a + fh_bloodcancer * 1.4057490757127600000000000
  
  # Sum from interaction terms 
    
    
    # Calculate the score itself 
    score = (1 - pow(survivor[surv], exp(a)) )
  return(score)
  }

## Male ----
# 

  bloodcancer_male <- function(age,
                               b_renalcancer,
                               b_type1,
                               bmi,
                               fh_bloodcancer,
                               smoke_cat,
                               surv) {
    # 0–5 year baseline survival
    survivors <- c(
      0.0,
      0.999643683433533,
      0.999320745468140,
      0.998986303806305,
      0.998629689216614,
      0.998259723186493
    )
    
    # smoking effect for categories 0–4
    Ismoke <- c(
      0.0,
      0.021413685386272918,
      0.09966847079797983,
      0.15990839062050988,
      0.21000270935119911
    )
    
    # fractional‐polynomial transforms
    dage  <- age / 10
    age1  <- dage^2
    age2  <- dage^3
    
    # centring
    age1 <- age1 - 19.604045867919922
    age2 <- age2 - 86.799774169921875
    bmi  <- bmi  - 26.309041976928711
    
    # build linear predictor
    a <- 0
    a <- a + Ismoke[smoke_cat + 1]
    a <- a + age1 * 0.18774091714058408
    a <- a + age2 * -0.014745653676834981
    a <- a + bmi  * 0.0080906902408843691
    a <- a + b_renalcancer   * 0.37709744176579374
    a <- a + b_type1         * 0.47358388019697939
    a <- a + fh_bloodcancer  * 1.3664654694337226
    
    # pick baseline survival for chosen horizon (1…5 yrs)
    S <- survivors[surv + 1]
    
    # risk = 100 * (1 − S^exp(a))
    score <- 100 * (1 - S^exp(a))
    return(score)
    
  }

# Breast cancer ----
## Female ----
# 

  breastcancer_female <- function(age,
                                  age_1_fh_breastcancer,
                                  age_2_fh_breastcancer,
                                  alcohol_cat6,
                                  b_benignbreast,
                                  b_bloodcancer,
                                  b_cop,
                                  b_hrt_oest,
                                  b_lungcancer,
                                  b_manicschiz,
                                  b_ovariancancer,
                                  bmi,
                                  ethrisk,
                                  fh_breastcancer,
                                  surv,
                                  town_1,
                                  town_2) {
    
    # baseline survival: index 1 = 0yr, 2 = 1yr, …, 6 = 5yr
    survivors <- c(
      0.0,
      0.998234629631042,
      0.996467649936676,
      0.994715332984924,
      0.992784678936005,
      0.990664660930634
    )
    
    # categorical effects
    Ialcohol <- c(
      0.0,
      0.052608749231567525,
      0.10409227333225146,
      0.18991705189460917,
      0.27151481129254795,
      0.22557544831456663
    )
    Iethrisk <- c(
      0.0, 0.0,
      -0.31642004172228422,
      -0.33867490010482365,
      -0.94208356242121705,
      -0.24713464422456044,
      -0.19607484240207898,
      -0.29264162372426122,
      -0.31945779001361019,
      -0.18693349501071405
    )
    
    # fractional–polynomial transforms
    dage  <- age / 10
    age1  <- dage^(-1)
    age2  <- dage^(-1) * log(dage)
    dbmi  <- bmi  / 10
    bmi1  <- dbmi^(-2)
    bmi2  <- dbmi^(-2) * log(dbmi)
    
    # centring
    age1 <- age1 - 0.223646596074104
    age2 <- age2 - 0.334952861070633
    bmi1 <- bmi1 - 0.151265263557434
    bmi2 <- bmi2 - 0.142848879098892
    
    # build linear predictor
    a <- 0
    a <- a + Ialcohol[alcohol_cat6 + 1]
    a <- a + Iethrisk[ethrisk + 1]
    a <- a + age1 * -19.081587156614287
    a <- a + age2 *  18.509756179218396
    a <- a + bmi1 *  -1.4752669093749973
    a <- a + bmi2 *   3.0544989463359729
    
    a <- a + age_1_fh_breastcancer *  5.2222964960331106
    a <- a + age_2_fh_breastcancer * -8.002089077472986
    a <- a + b_benignbreast        *  0.41367643341384747
    a <- a + b_bloodcancer         *  0.44994536748262803
    a <- a + b_cop                 *  0.12610786134996657
    a <- a + b_hrt_oest            *  0.16913593645922093
    a <- a + b_lungcancer          *  0.62046068634592022
    a <- a + b_manicschiz          *  0.15117702688059084
    a <- a + b_ovariancancer       *  0.35123379560422596
    a <- a + fh_breastcancer       *  0.65834446669129887
    a <- a + town_1                *  0.00000075629653384331456
    a <- a + town_2                * -0.086746493896535493
    
    # pick the right baseline-survival (surv in 1:5 → index surv+1)
    S <- survivors[surv + 1]
    
    # 100*(1 – S^exp(a))
    score <- 100 * (1 - S^exp(a))
    return(score)
  }

# Colorectal cancer ----
## Female ----


  colorectal_female <- function(age,
                                age_1_fh_gicancer,
                                age_2_fh_gicancer,
                                alcohol_cat6,
                                b_breastcancer,
                                b_cervicalcancer,
                                b_colitis,
                                b_ovariancancer,
                                b_polyp,
                                b_type2,
                                b_uterinecancer,
                                ethrisk,
                                fh_gicancer,
                                smoke_cat,
                                surv) {
    
    # baseline survival for 0–5 years
    survivors <- c(
      0.0,
      0.999769449234009,
      0.999535322189331,
      0.999294936656952,
      0.999053359031677,
      0.998782038688660
    )
    
    # categorical effects
    Ialcohol <- c(
      0.0,
      0.016932317022484617,
      0.050471611879332329,
      0.081302218350274985,
      0.318842764100321750,
      0.307113849333050640
    )
    Iethrisk <- c(
      0.0, 
      0.0,
      -1.0494971746256023,
      -0.7580085872167761,
      -0.16206418242822976,
      -0.5207551589289394,
      -0.3390081073116003,
      -0.3663991962571118,
      -0.4962552732151417,
      -0.22289770148509797
    )
    Ismoke <- c(
      0.0,
      0.06794306582423695,
      0.10017056701706135,
      0.19403251493802176,
      0.16095242485295916
    )
    
    # fractional–polynomial transforms
    dage  <- age / 10
    fp1   <- dage^(-2)
    fp2   <- dage^(-2) * log(dage)
    
    # centre
    fp1 <- fp1 - 0.049712907522917
    fp2 <- fp2 - 0.074606411159039
    
    # linear predictor
    a <- 0
    a <- a + Ialcohol[alcohol_cat6 + 1]
    a <- a + Iethrisk[ethrisk      + 1]
    a <- a + Ismoke[smoke_cat      + 1]
    
    a <- a + fp1 *  33.92788234800954
    a <- a + fp2 * -83.92405085199356
    
    a <- a + age_1_fh_gicancer * 10.985496891972508
    a <- a + age_2_fh_gicancer * -0.0948979335841995
    a <- a + b_breastcancer     *  0.15096136873617999
    a <- a + b_cervicalcancer   *  0.55134659369582772
    a <- a + b_colitis          *  0.56087545120009641
    a <- a + b_ovariancancer    *  0.68400222067217209
    a <- a + b_polyp            *  0.74904091187947031
    a <- a + b_type2            *  0.14828780874778763
    a <- a + b_uterinecancer    *  0.47923953865647911
    a <- a + fh_gicancer        *  0.66160452485446319
    
    # pick baseline survival for year = surv (1→1yr, …, 5→5yr)
    S0 <- survivors[surv + 1]
    
    # 100 * (1 − S0^exp(a))
    score <- 100 * (1 - S0^exp(a))
    return(score)
  }

## Male ----

colorectal_male <- function(age,
                              age_1_fh_gicancer,
                              age_2_fh_gicancer,
                              alcohol_cat6,
                              b_bloodcancer,
                              b_colitis,
                              b_lungcancer,
                              b_oralcancer,
                              b_polyp,
                              b_type2,
                              bmi,
                              ethrisk,
                              fh_gicancer,
                              smoke_cat,
                              surv,
                              town) {
    
    # baseline survivors at 1–5 years
    survivors <- c(
      0.0,
      0.999779522418976,
      0.999549508094788,
      0.999336361885071,
      0.999095499515533,
      0.998839616775513
    )
    
    # categorical effects
    Ialcohol <- c(
      0.0,
      0.047922717327152427,
      0.130645007652749200,
      0.264241697606650950,
      0.482480786236695380,
      0.443388705248696280
    )
    Iethrisk <- c(
      0.0, 0.0,
      -0.585234717599028340,
      -0.586433778050686530,
      -0.881108093614539120,
      -0.482521131107602700,
      -0.350298570219546860,
      -0.286864304238911790,
      -0.212584599008222960,
      -0.525062572168949800
    )
    Ismoke <- c(
      0.0,
      0.054436520378164946,
      0.066967983943209852,
      0.026825493057743323,
      0.123013471980582880
    )
    
    # fractional‐polynomial transforms
    dage  <- age / 10
    age1  <- dage
    age2  <- dage^2
    
    # centre
    age1  <- age1 - 4.425716876983643
    age2  <- age2 - 19.586969375610352
    bmi   <- bmi   - 26.309040069580078
    town  <- town  -  0.260672301054001
    
    # build linear predictor
    a <- 0
    a <- a + Ialcohol[alcohol_cat6 + 1]
    a <- a + Iethrisk[ethrisk         + 1]
    a <- a + Ismoke[smoke_cat         + 1]
    
    a <- a + age1 *  2.6296558591088894
    a <- a + age2 * -0.1495728139219066
    a <- a + bmi   *  0.01564521194272065
    a <- a + town  *  0.00899929209786705
    
    a <- a + age_1_fh_gicancer * -1.6277021020802243
    a <- a + age_2_fh_gicancer *  0.1248377530713149
    a <- a + b_bloodcancer     *  0.4284078752708642
    a <- a + b_colitis         *  0.6016726220869758
    a <- a + b_lungcancer      *  0.6273185618450068
    a <- a + b_oralcancer      *  0.4825177045812839
    a <- a + b_polyp           *  0.4092716863938517
    a <- a + b_type2           *  0.2389208244341949
    a <- a + fh_gicancer       *  0.7802989526167445
    
    # baseline survival for the chosen horizon
    S0 <- survivors[surv + 1]
    
    # 100 * (1 − S0^exp(a))
    score <- 100 * (1 - S0^exp(a))
    return(score)
  }

# Lung cancer ----
## Female ----
# 

  
  lungcancer_female_raw <- function(
    age,
    b_asthma,
    b_bloodcancer,
    b_breastcancer,
    b_cervicalcancer,
    b_copd,
    b_oralcancer,
    b_ovariancancer,
    b_renalcancer,
    b_uterinecancer,
    bmi,
    ethrisk,
    fh_lungcancer,
    smoke_cat,
    surv,
    town_1,
    town_2
  ) {
    #--- baseline survivors at 1–5 years
    survivors <- c(
      0.999944269657135,
      0.999894261360168,
      0.999844491481781,
      0.999790668487549,
      0.999729692935944
    )
    
    #--- categorical effects (indexed 0…9 and 0…4 in C → we store with a dummy 0 up front)
    Iethrisk <- c(
      0.0,    # dummy for C index 0
      0.0,    # C[1]
      -0.61627798506718945,
      -1.0042328006097481,
      0.16208315426799977,
      -0.37698920425633897,
      -0.66172525956190675,
      -0.59279295984157099,
      0.14937367259159637,
      -0.48227787591699922
    )
    Ismoke <- c(
      0.0,    # C index 0
      0.61127545760861135,
      1.7358454024106114,
      1.8830939134170273,
      2.3993666780330876
    )
    
    #--- fractional-polynomial transforms
    dage  <- age / 10
    age_1 <- log(dage)
    age_2 <- dage^3
    dbmi  <- bmi / 10
    bmi_1 <- dbmi^-2
    bmi_2 <- dbmi^-2 * log(dbmi)
    
    #--- centre continuous variables
    age_1 <- age_1 -  1.501477479934692
    age_2 <- age_2 - 90.417015075683594
    bmi_1 <- bmi_1 -  0.151186034083366
    bmi_2 <- bmi_2 -  0.142813667654991
    
    #--- build linear predictor a
    a <- 0
    a <- a + Iethrisk[ethrisk + 1]
    a <- a + Ismoke[smoke_cat + 1]
    a <- a + age_1 *  5.4496774345837338
    a <- a + age_2 * -0.0012617980386000023
    a <- a + bmi_1 *  4.0768085177264339
    a <- a + bmi_2 * -4.3337378643456770
    
    a <- a + b_asthma        * 0.28198358853852595
    a <- a + b_bloodcancer   * 0.65896026126661100
    a <- a + b_breastcancer  * 0.42665133232679031
    a <- a + b_cervicalcancer* 0.45544235999480576
    a <- a + b_copd          * 0.67638317124027214
    a <- a + b_oralcancer    * 1.04052670667629110
    a <- a + b_ovariancancer * 0.49316194226843602
    a <- a + b_renalcancer   * 0.55370552227858116
    a <- a + b_uterinecancer * 0.42387564389296134
    a <- a + fh_lungcancer   * 0.27879775591535411
    a <- a + town_1          * 0.51358827424889841
    a <- a + town_2          * -0.57102053558987265
    
    #--- smoking × age interactions
    if (smoke_cat == 1) {
      a <- a + age_1 *  2.4893996121290236 +
        age_2 * -0.0027979410978276986
    } else if (smoke_cat == 2) {
      a <- a + age_1 *  2.4559411049193383 +
        age_2 * -0.0033489058782917372
    } else if (smoke_cat == 3) {
      a <- a + age_1 *  2.4320914848139008 +
        age_2 * -0.0028170483278367863
    } else if (smoke_cat == 4) {
      a <- a + age_1 *  1.7511080930265210 +
        age_2 * -0.0022072853540985198
    }
    
    #--- absolute risk: 100 * (1 − S0^exp(a))
    S0    <- survivors[surv]
    score <- 100 * (1 - S0^exp(a))
    return(score)
  }


## Male ----


  lungcancer_male_raw <- function(
    age,
    alcohol_cat6,
    b_asbestos,
    b_asthma,
    b_bloodcancer,
    b_colorectal,
    b_copd,
    b_oesgastric,
    b_oralcancer,
    b_renalcancer,
    bmi,
    ethrisk,
    fh_lungcancer,
    smoke_cat,
    surv,
    town_1,
    town_2
  ) {
    #--- baseline survivors at 1–5 years
    survivors <- c(
      0.999953985214233,
      0.999909281730652,
      0.999866127967834,
      0.999819517135620,
      0.999768972396851
    )
    
    #--- categorical effects (0-based in C → offset by +1 in R)
    Ialcohol <- c(
      -0.097638663966368708,
      -0.087931298983987161,
      -0.027883529699291251,
      0.124129991193016820,
      0.222669110490023580
    )
    Iethrisk <- c(
      0.000000000000000000,  # index 0
      0.000000000000000000,  # index 1
      -1.033123642879648400,
      -0.904352704432147550,
      -0.169039366531663430,
      -1.008225066466882100,
      -0.547750260348542680,
      -0.800455506851105450,
      -0.444285072825753910,
      -0.674622740769244000
    )
    Ismoke <- c(
      0.864351123217589000,
      1.780623237627574100,
      1.964284221116352900,
      2.326714881007327000
    )
    
    #--- fractional-polynomial transforms
    dage  <- age / 10
    age_1 <- dage
    age_2 <- dage * log(dage)
    dbmi  <- bmi / 10
    bmi_1 <- dbmi^-1
    bmi_2 <- dbmi^-1 * log(dbmi)
    
    #--- centre continuous variables
    age_1 <- age_1 - 4.428966999053955
    age_2 <- age_2 - 6.591039657592773
    bmi_1 <- bmi_1 - 0.380103737115860
    bmi_2 <- bmi_2 - 0.367678552865982
    
    #--- build linear predictor a
    a <- 0
    
    # conditional sums
    a <- a + Ialcohol[alcohol_cat6 + 1]
    a <- a + Iethrisk[ethrisk + 1]
    a <- a + Ismoke[smoke_cat + 1]
    
    # continuous
    a <- a + age_1 *  6.4301757663471468
    a <- a + age_2 * -1.9010396695468927
    a <- a + bmi_1 *  1.9425789668620277
    a <- a + bmi_2 * -6.8930979029871811
    
    # boolean
    a <- a + b_asbestos    * 0.6146015094622908
    a <- a + b_asthma      * 0.16718862518311295
    a <- a + b_bloodcancer * 0.64662156274347393
    a <- a + b_colorectal  * 0.25329482111372476
    a <- a + b_copd        * 0.65040052487234079
    a <- a + b_oesgastric  * 0.5814732073975124
    a <- a + b_oralcancer  * 1.0504664246702722
    a <- a + b_renalcancer * 0.4056966715141066
    a <- a + fh_lungcancer * 0.24989549701660893
    a <- a + town_1        * 0.52088338021399405
    a <- a + town_2        * -0.57005418735312507
    
    # interactions with smoking
    if (smoke_cat == 1) {
      a <- a + age_1 *  1.6228654481140201 +
        age_2 * -0.61785385907514767
    } else if (smoke_cat == 2) {
      a <- a + age_1 *  1.9122750065681651 +
        age_2 * -0.74247507854740347
    } else if (smoke_cat == 3) {
      a <- a + age_1 *  1.7352421908685334 +
        age_2 * -0.66386920199227051
    } else if (smoke_cat == 4) {
      a <- a + age_1 *  1.7644086878535417 +
        age_2 * -0.70326837252992203
    }
    
    #--- absolute risk (%) = 100 * (1 − S0^exp(a))
    S0    <- survivors[surv]
    score <- 100 * (1 - S0^exp(a))
    return(score)
  }



# Oesophageal-gastric cancer ----
## Female ----

  oesgastric_female_raw <- function(
    age,
    alcohol_cat6,
    b_barratts,
    b_bloodcancer,
    b_breastcancer,
    b_lungcancer,
    b_oralcancer,
    b_peptic,
    b_type2,
    bmi,
    smoke_cat,
    surv,
    town
  ) {
    #--- baseline survivors at 1–5 years
    survivors <- c(
      0.999951303005219,
      0.999912202358246,
      0.999867975711823,
      0.999822735786438,
      0.999769926071167
    )
    
    #--- categorical effects (0-based in C → +1 for R)
    Ialcohol <- c(
      -0.11189368983791335,
      -0.09686591768081258,
      -0.020493347098261897,
      0.69179172272655054,
      0.69149113794917483
    )
    Ismoke <- c(
      0.082454477615318322,
      0.57786728901095497,
      0.71788998349242594,
      0.87748067752842873
    )
    
    #--- fractional‐polynomial transforms
    dage  <- age / 10
    age_1 <- dage^2
    age_2 <- dage^2 * log(dage)
    dbmi  <- bmi / 10
    bmi_1 <- dbmi^-2
    bmi_2 <- dbmi^-2 * log(dbmi)
    
    #--- centre continuous variables
    age_1 <- age_1 - 20.148687362670898
    age_2 <- age_2 - 30.254657745361328
    bmi_1 <- bmi_1 - 0.151121616363525
    bmi_2 <- bmi_2 - 0.142785012722015
    town  <- town  - 0.161748945713043
    
    #--- linear predictor
    a <- 0
    
    # conditional sums
    a <- a + Ialcohol[alcohol_cat6 + 1]
    a <- a + Ismoke[smoke_cat + 1]
    
    # continuous
    a <- a + age_1 *  0.45914965886458331
    a <- a + age_2 * -0.16675870039332663
    a <- a + bmi_1  *  6.9375480360437578
    a <- a + bmi_2  * -13.15886212755024
    a <- a + town   *  0.029423061779428595
    
    # boolean
    a <- a + b_barratts     * 1.3419977053300407
    a <- a + b_bloodcancer  * 0.76256297790980909
    a <- a + b_breastcancer * 0.26785164693269614
    a <- a + b_lungcancer   * 0.82354622245478371
    a <- a + b_oralcancer   * 1.3464504475029004
    a <- a + b_peptic       * 0.25513098061052886
    a <- a + b_type2        * 0.2889222416353735
    
    # absolute risk (%) = 100 * (1 − S0^exp(a))
    S0    <- survivors[surv]
    score <- 100 * (1 - S0^exp(a))
    return(score)
  }



## Male ----
# 

  oesgastric_male_raw <- function(
    age,                 # integer (years)
    alcohol_cat6,        # 0–5  (same ordering as original model)
    b_barratts,          # 0 / 1
    b_oralcancer,        # 0 / 1
    b_pancreascancer,    # 0 / 1
    b_peptic,            # 0 / 1
    b_type2,             # 0 / 1
    bmi,                 # kg/m^2
    smoke_cat,           # 0–4
    surv,                # 1–5  (years)
    town                 # Townsend score (already centred on UK 2001 distribution)
  ) {
    ## --- 1. Baseline survivor function (years 1-5) ---
    S0 <- c(
      0.999897956848145,
      0.999810755252838,
      0.999715626239777,
      0.999619841575623,
      0.999517440795898
    )
    
    ## --- 2. Categorical log-hazard increments ---
    # NB: in the C/SQL code categories start at 0 ⇒ add +1 for R indices
    Ialcohol <- c(
      -0.060408814366972742,
      -0.120795324725350890,
      -0.064776788859829793,
      0.212448549327400200,
      0.485713933786969370
    )
    Ismoke <- c(
      0.223046313338126740,
      0.546911891270208340,
      0.614301239736905760,
      0.647849200703754400
    )
    
    ## --- 3. Fractional-polynomial transformations ---
    dage  <- age / 10
    age_1 <- dage^2
    age_2 <- dage^2 * log(dage)
    dbmi  <- bmi / 10
    bmi_1 <- dbmi^-2
    bmi_2 <- dbmi^-2 * log(dbmi)
    
    ## --- 4. Centre continuous variables ---
    age_1 <- age_1 - 19.620740890502930
    age_2 <- age_2 - 29.201423645019531
    bmi_1 <- bmi_1 - 0.144450336694717
    bmi_2 <- bmi_2 - 0.139742657542229
    town  <- town  - 0.259125590324402
    
    ## --- 5. Assemble linear predictor ---
    a <- 0
    
    # categorical (remember +1 index shift)
    a <- a + Ialcohol[alcohol_cat6 + 1]
    a <- a + Ismoke[smoke_cat      ]
    
    # continuous terms
    a <- a + age_1 *  0.55179048337676251
    a <- a + age_2 * -0.20781400681781595
    a <- a + bmi_1 *  8.44084961131547650
    a <- a + bmi_2 * -20.690908334247094
    a <- a + town  *  0.027485527684047967
    
    # binary indicators
    a <- a + b_barratts      * 1.3981857737696197
    a <- a + b_oralcancer    * 0.9751074059450815
    a <- a + b_pancreascancer* 1.4262126421694010
    a <- a + b_peptic        * 0.22446304526922312
    a <- a + b_type2         * 0.16378180506856244
    
    ## --- 6. Convert to absolute risk (%) ---
    risk <- 100 * (1 - S0[surv] ^ exp(a))
    return(risk)
  }

# Oral cancer ----
## Female ----
# 

oralcancer_female_raw <- function(
    age,              # numeric age in years (25-84 in the original validation)
    alcohol_cat6,     # factor 0-5  (0 = none, 5 = heaviest)
    b_bloodcancer,    # 0 / 1
    b_ovariancancer,  # 0 / 1
    smoke_cat,        # 0-4  (0 = never; 4 = heavy)
    surv,             # 1-5  (risk horizon in years)
    town              # Townsend score (can be negative)
  ) {
    ## --- 1. Baseline survivor function (years 1–5) ---
    S0 <- c(
      0.999955236911774,
      0.999913334846497,
      0.999870777130127,
      0.999827504158020,
      0.999773323535919
    )
    
    ## --- 2. Log-hazard increments for categorical variables ---
    # add +1 when you index because categories in the C code start at 0
    Ialcohol <- c(
      0.000000000000000000,   # cat 0
      0.027999573991790434,   # 1
      0.161952333485795570,   # 2
      0.469045561295809990,   # 3
      1.050113709562972200,   # 4
      1.477564061249553500    # 5
    )
    Ismoke <- c(
      0.000000000000000000,   # cat 0
      0.202771610564268650,   # 1
      0.751311169011947390,   # 2
      0.922381368685718290,   # 3
      1.258014320621153800    # 4
    )
    
    ## --- 3. Fractional-polynomial transforms ---
    dage  <- age / 10
    age_1 <- dage^(-2)
    
    ## --- 4. Centre continuous covariates ---
    age_1 <- age_1 - 0.049628291279078
    town  <- town  - 0.161764934659004
    
    ## --- 5. Create linear predictor ---
    a <- 0
    a <- a + Ialcohol[alcohol_cat6 + 1L]   # +1 for R indexing
    a <- a + Ismoke[smoke_cat      + 1L]
    
    # continuous terms
    a <- a + age_1 * (-31.59790890203471)
    a <- a + town  *   0.027322975582607253
    
    # binary indicators
    a <- a + b_bloodcancer  * 1.5136324928800686
    a <- a + b_ovariancancer* 1.4208557298354756
    
    ## --- 6. Convert to absolute risk (%) and return ---
    risk <- 100 * (1 - S0[surv] ^ exp(a))
    return(risk)
  }



## Male ----
# 


oralcancer_male_raw <- function(
    age,           # numeric (years) – should be 25-84 under the original validation
    alcohol_cat6,  # integer 0–5
    b_bloodcancer, # 0 / 1
    b_colorectal,  # 0 / 1
    b_lungcancer,  # 0 / 1
    bmi,           # numeric (kg/m^2)
    smoke_cat,     # integer 0–4
    surv,          # 1–5  (risk horizon in years)
    town           # numeric (can be negative)
) {
  ## --- 1. Baseline survivor function (years 1–5) ---
  S0 <- c(
    0.999936580657959,
    0.999877035617828,
    0.999810338020325,
    0.999735474586487,
    0.999666094779968
  )
  
  ## --- 2. Log-hazard increments for categorical covariates ---
  Ialcohol <- c(
    0.000000000000000000,   # cat 0
    -0.11815137221117873,    # 1
    0.017369446321814367,   # 2
    0.30919815160108927,    # 3
    0.95266877468309552,    # 4
    1.31018963772298110     # 5
  )
  Ismoke <- c(
    0.000000000000000000,   # cat 0
    0.141313630107231140,   # 1
    0.837623266183970320,   # 2
    0.854011447928299640,   # 3
    1.083175089535542300    # 4
  )
  
  ## --- 3. Fractional-polynomial transforms ---
  dage  <- age / 10
  age_1 <- dage^(-0.5)
  age_2 <- dage^(-0.5) * log(dage)
  
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^(-2)
  bmi_2 <- dbmi^(-1)
  
  ## --- 4. Centre continuous covariates ---
  age_1 <- age_1 - 0.475125968456268
  age_2 <- age_2 - 0.707154035568237
  bmi_1 <- bmi_1 - 0.144458159804344
  bmi_2 <- bmi_2 - 0.380076527595520
  town  <- town  - 0.258994281291962
  
  ## --- 5. Linear predictor ---
  a <- 0
  a <- a + Ialcohol[alcohol_cat6 + 1L]   # +1 for R’s 1-based indexing
  a <- a + Ismoke[smoke_cat      + 1L]
  
  # continuous terms
  a <- a + age_1 * ( -5.3604756931843474 )
  a <- a + age_2 * ( 18.4654946257000350 )
  a <- a + bmi_1 * ( 16.7531872033111960 )
  a <- a + bmi_2 * ( -10.3137864786102560 )
  a <- a + town  * ( 0.046676794726002648 )
  
  # binary indicators
  a <- a + b_bloodcancer * 0.85005392253691792
  a <- a + b_colorectal  * 0.48087862364839068
  a <- a + b_lungcancer  * 1.05451225353212390
  
  ## --- 6. Convert to absolute risk (%) ---
  risk <- 100 * (1 - S0[surv] ^ exp(a))
  return(risk)
}


#Ovarian cancer ----
## Female ----
# 

  
ovariancancer_female_raw <- function(
    age,                      # numeric (years 25–84)
    age_1_fh_ovariancancer,   # 0 / 1
    age_2_fh_ovariancancer,   # 0 / 1
    b_breastcancer,           # 0 / 1
    b_cervicalcancer,         # 0 / 1
    b_cop,                    # 0 / 1
    bmi,                      # numeric (kg/m²)
    fh_ovariancancer,         # 0 / 1
    surv                      # integer 1–5 (risk horizon)
) {
  # 1. Baseline survivor function values for years 1–5
  S0 <- c(
    0.999722599983215,
    0.999479830265045,
    0.999221026897430,
    0.998951792716980,
    0.998684525489807
  )
  
  # 2. Fractional-polynomial transforms (with scaling by 10 where needed)
  dage  <- age / 10
  age_1 <- dage                        # x¹
  age_2 <- dage * log(dage)            # x·log(x)
  
  # 3. Centre continuous variables
  age_1 <- age_1 - 4.487829208374023
  age_2 <- age_2 - 6.737888336181641
  bmi   <- bmi   - 25.727840423583984
  
  # 4. Build linear predictor
  a <- 0
  
  #   4a. Continuous terms
  a <- a + age_1 *  3.6496242385839661
  a <- a + age_2 * -1.1991731219303103
  a <- a + bmi   *  0.011304598769420525
  
  #   4b. Binary indicators
  a <- a + age_1_fh_ovariancancer * (-4.292666853656549)
  a <- a + age_2_fh_ovariancancer *   1.688204933293370
  a <- a + b_breastcancer         *   0.485469510959992
  a <- a + b_cervicalcancer       *   0.473056942589241
  a <- a + b_cop                  *  (-0.430066820865163)
  a <- a + fh_ovariancancer       *   1.336359483469376
  
  # 5. Absolute risk (%) over the chosen horizon
  risk <- 100 * (1 - S0[surv] ^ exp(a))
  return(risk)
}



# Pancreatic cancer ----
## Female ----
# 

  
pancreascancer_female_raw <- function(
    age,            # numeric: 25–84 y
    b_breastcancer, # 0 / 1
    b_chronicpan,   # 0 / 1 (chronic pancreatitis)
    b_renalcancer,  # 0 / 1
    b_type2,        # 0 / 1 (type-2 diabetes)
    bmi,            # numeric: kg/m²
    smoke_cat,      # integer 0–4
    surv,           # integer 1–5 (risk horizon, years)
    town            # Townsend score (continuous)
) {
  ## 1. Baseline survivor-function values (years 1 to 5)
  S0 <- c(
    0.999964594841003,
    0.999925315380096,
    0.999884843826294,
    0.999844610691071,
    0.999795138835907
  )
  
  ## 2. Smoking category coefficients
  Ismoke <- c(
    0.000000000000000000,
    0.025375758413601845,
    0.569920687829630010,
    0.639056878397405660,
    0.704289450581061290
  )
  
  ## 3. Fractional-polynomial transform (with scaling)
  dage  <- age / 10
  age_1 <- dage^(-0.5)
  
  ## 4. Centre continuous variables
  age_1 <- age_1 - 0.471977025270462
  bmi   <- bmi   - 25.729867935180664
  town  <- town  - 0.161796689033508
  
  ## 5. Build the linear predictor
  a <- 0
  a <- a + Ismoke[smoke_cat + 1]     # +1 because R indices start at 1
  a <- a + age_1 * (-23.260189976963467)
  a <- a + bmi   *  0.009781510953804978
  a <- a + town  *  0.011402050541305815
  
  a <- a + b_breastcancer * 0.3192435657707442
  a <- a + b_chronicpan   * 1.2927221506222435
  a <- a + b_renalcancer  * 0.6789970707278265
  a <- a + b_type2        * 0.4107439191649736
  
  ## 6. Absolute risk (%) at the chosen horizon
  risk <- 100 * (1 - S0[surv] ^ exp(a))
  return(risk)
}

## Male ----

pancreascancer_male_raw <- function(
    age,            # numeric, 25–84
    b_bloodcancer,  # 0 / 1
    b_chronicpan,   # 0 / 1
    b_type2,        # 0 / 1 (type-2 diabetes)
    bmi,            # numeric, kg/m²
    smoke_cat,      # 0–4  (never → heavy)
    surv            # 1–5  (risk horizon, yrs)
) {
  ## Baseline survivor at 1-5 yr
  S0 <- c(
    0.999952435493469,
    0.999906480312347,
    0.999861896038055,
    0.999808788299561,
    0.999760389328003
  )
  
  ## Smoking coefficients (same ordering as categories 0-4)
  Ismoke <- c(
    0.000000000000000000,
    0.083678878341541807,
    0.444894932058506720,
    0.672901202436161800,
    0.663625971422573510
  )
  
  ## Fractional-polynomial transforms
  dage  <- age / 10
  age_1 <- dage^(-0.5)
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^-2
  bmi_2 <- dbmi^-2 * log(dbmi)
  
  ## Centre continuous terms
  age_1 <- age_1 - 0.475096940994263
  bmi_1 <- bmi_1 - 0.144456043839455
  bmi_2 <- bmi_2 - 0.139745324850082
  
  ## Linear predictor
  a <- 0
  a <- a + Ismoke[smoke_cat + 1]
  a <- a + age_1 * (-22.388503771890694)
  a <- a + bmi_1 *  4.2251656934838371
  a <- a + bmi_2 * (-11.281643872666246)
  
  a <- a + b_bloodcancer * 0.5369701587873449
  a <- a + b_chronicpan   * 1.6922917636081924
  a <- a + b_type2        * 0.6132344324406092
  
  ## Absolute risk (%) at chosen horizon
  risk <- 100 * (1 - S0[surv] ^ exp(a))
  return(risk)
}


# Prostate cancer ----
## Male ----

prostatecancer_male_raw <- function(
    age,                         # numeric   25–84
    age_1_fh_prostatecancer,     # 0 / 1
    age_2_fh_prostatecancer,     # 0 / 1
    b_manicschiz,                # 0 / 1
    b_type1,                     # 0 / 1   (type-1 diabetes)
    b_type2,                     # 0 / 1   (type-2 diabetes)
    bmi,                         # numeric kg/m²
    ethrisk,                     # 1–9      (ONS categories)
    fh_prostatecancer,           # 0 / 1
    smoke_cat,                   # 0–4      (never → heavy)
    surv,                        # 1–5 year risk horizon
    town                         # Townsend score (centred later)
) {
  ## Baseline survivor function (1…5 years)
  S0 <- c(
    0.999701976776123,
    0.999389469623566,
    0.999051749706268,
    0.998661398887634,
    0.998229324817657
  )
  
  ## Ethnicity and smoking category coefficients
  Iethrisk <- c(
    0.000000000000000000,
    -0.508766992354229950,
    -0.866982197008514530,
    -1.236432569385744800,
    -0.778053671995835860,
    1.044086983146447900,
    0.684437419411313860,
    -0.698503085196340970,
    0.393088307774554890
  )
  
  Ismoke <- c(
    0.000000000000000000,
    0.015883779217294801,
    -0.249514465571867850,
    -0.304576145481981920,
    -0.237987860679658740
  )
  
  ## Interaction (age × smoking) coefficients
  age1_int <- c(
    0.000000000000000000,
    -2.053305941779285200,
    -2.890546310165653200,
    -9.369836980243906800,
    -8.858450699853746000
  )
  age2_int <- c(
    0.000000000000000000,
    -9.637377048311796200,
    -3.431074594677306200,
    -21.162570460923313000,
    -24.646378113213263000
  )
  
  ## Fractional-polynomial transforms (with centring)
  dage  <- age / 10
  age_1 <- dage^(-0.5)                 - 0.475571960210800
  age_2 <- dage^(-0.5) * log(dage)     - 0.706925392150879
  
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^-1                     - 0.380129784345627
  bmi_2 <- dbmi^(-0.5)                 - 0.616546630859375
  
  town_c <- town - 0.261956512928009   # centred Townsend
  
  ## Linear predictor
  a <- 0
  a <- a + Iethrisk[ethrisk]                    # ethnicity main effect
  a <- a + Ismoke[smoke_cat + 1]                # smoking main effect
  
  a <- a + age_1 * (-13.099612581816686)
  a <- a + age_2 *   55.276108637451607
  a <- a + bmi_1 * (-14.297877101339337)
  a <- a + bmi_2 *   18.293733971731786
  a <- a + town_c *  (-0.0284342617816042)
  
  a <- a + age_1_fh_prostatecancer *  2.7874621493140661 
  # age_1_fh_prostatecancer = age_1 * fh_prostatecancer
  a <- a + age_2_fh_prostatecancer * (-29.048495836727859)
  # age_2_fh_prostatecancer = age_2 * fh_prostatecancer
  # these are interaction terms NOT inputs as 
  # suggested by the function definition
  a <- a + b_manicschiz            * (-0.44046374073333017)
  a <- a + b_type1                 * (-0.55834661211309444)
  a <- a + b_type2                 * (-0.11085159340787583)
  a <- a + fh_prostatecancer       *   2.0344915550609244
  
  ## Interactions age×smoking
  a <- a + age_1 * age1_int[smoke_cat + 1]
  a <- a + age_2 * age2_int[smoke_cat + 1]
  
  ## Absolute risk (%)
  risk <- 100 * (1 - S0[surv] ^ exp(a))
  return(risk)
}

# Renal cancer ----
## Female ----
# 

renalcancer_female_raw <- function(
    age = NULL,
    b_bloodcancer = 0,
    b_braincancer = 0, 
    b_cervicalcancer = 0, 
    b_colorectal = 0,
    b_ovariancancer = 0, 
    b_type2 = 0, 
    b_uterinecancer = 0,
    bmi = 25, 
    smoke_cat = 0, 
    surv = 5, 
    town_1 = 0, 
    town_2 = 0
) {
  
  ## Baseline survivor function (index 0-5 in C → 1-6 in R)
  survivor <- c(
    # 0.000000000000000,  # dummy slot so that survivor[surv + 1] = C’s survivor[surv]
    0.999912858009338,
    0.999821424484253,
    0.999732971191406,
    0.999641656875610,
    0.999542832374573
  )
  
  ## Smoking category coefficients
  Ismoke <- c(
    # 0.0000000000000000000000000,
    0.2424368501539901600000000,
    0.5544456610978699700000000,
    0.8003428243906539600000000,
    0.8526176873713247100000000
  )
  
  ## Fractional-polynomial age transforms
  dage   <- age / 10
  age_1  <- dage
  age_2  <- dage^2
  
  ## Centre continuous variables
  age_1 <- age_1 - 4.487745761871338
  age_2 <- age_2 - 20.139862060546875
  bmi   <- bmi   - 25.724693298339844
  
  ## Linear predictor
  a <- 0
  a <- a + Ismoke[smoke_cat]                    # +1 because R is 1-based
  
  a <- a + age_1 *  1.9824578275162619
  a <- a + age_2 * -0.10496076209659227
  a <- a + bmi   *  0.010336870596784856
  
  a <- a + b_bloodcancer  * 0.48950495525763654
  a <- a + b_braincancer  * 2.3206648663049099
  a <- a + b_cervicalcancer * 0.94192979452946313
  a <- a + b_colorectal   * 0.36193845390480039
  a <- a + b_ovariancancer * 0.96187511010839699
  a <- a + b_type2        * 0.29590425304821777
  a <- a + b_uterinecancer * 0.75290270813016125
  a <- a + town_1         * 0.13132370105130087
  a <- a + town_2         * -0.40271804429320668
  
  ## Absolute 5-year risk (same as C: 1 – survivor^exp(lp))
  risk <- 1 - survivor[surv] ^ exp(a)
  return(risk)
}

renalcancer_female_raw(age = 64,town_1=-7)
## Male ----

  
  renalcancer_male_raw <- function(
    age = 0,
    b_colorectal = 0,
    b_lungcancer = 0,
    b_prostatecancer = 0,
    b_type2 = 0,
    bmi = 25,
    smoke_cat = 1,
    surv = 5,
    town = 0
  )
{
  survivor = c(
    # 0,
    0.999815762042999,
    0.999644875526428,
    0.999471843242645,
    0.999277472496033,
    0.999066352844238
  )
  
  # The conditional arrays 
    
    Ismoke = c(
      # 0,
      0.2112239987625918200000000,
      0.4980213954417773700000000,
      0.7237512139320682000000000,
      0.8124579211392302100000000
    )
  
  # Applying the fractional polynomial transforms 
    # (which includes scaling)                      
    
    dage = age
  dage=dage/10
  age_1 = dage
  age_2 = pow(dage,3)
  
  # Centring the continuous variables 
    
    age_1 = age_1 - 4.426476955413818
  age_2 = age_2 - 86.731056213378906
  bmi = bmi - 26.308208465576172
  town = town - 0.260041594505310
  
  # Start of Sum 
    a=0
  
  # The conditional sums 
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values 
    
    a = a + age_1 * 1.6395376233886698000000000
  a = a + age_2 * -0.0069787249855328588000000
  a = a + bmi * 0.0191266080190585430000000
  a = a + town * -0.0039642613172689916000000
  
  # Sum from boolean values 
    
    a = a + b_colorectal * 0.2250243172974877300000000
  a = a + b_lungcancer * 0.5785135435280358600000000
  a = a + b_prostatecancer * 0.3753145930635795000000000
  a = a + b_type2 * 0.1898117535793944500000000
  
  # Sum from interaction terms 
    
    
    # Calculate the score itself 
    score = (1 - pow(survivor[surv], exp(a)) )
  return(score)
  }

# Uterine cancer -----
## Female -----
# 

  uterinecancer_female_raw <- function(
    age,
    b_breastcancer,
    b_colorectal,
    b_endometrial,
    b_manicschiz,
    b_pos,
    b_type2,
    bmi,
    smoke_cat,
    surv
  )
{
  survivor = c(
    # 0,
    0.999801218509674,
    0.999620676040649,
    0.999434530735016,
    0.999217212200165,
    0.998992919921875
  )
  
  # The conditional arrays 
    
    Ismoke = c(
      # 0,
      -0.2020713821985857000000000,
      -0.1817584707326591600000000,
      -0.3021556609298930400000000,
      -0.4191963756060861400000000
    )
  
  # Applying the fractional polynomial transforms 
    # (which includes scaling)                      
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,.5)
  age_2 = dage
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,2)
  
  # Centring the continuous variables 
    
    age_1 = age_1 - 2.118349790573120
  age_2 = age_2 - 4.487406253814697
  bmi_1 = bmi_1 - 6.617829799652100
  
  # Start of Sum 
    a=0
  
  # The conditional sums 
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values 
    
    a = a + age_1 * 23.1774636105576340000000000
  a = a + age_2 * -4.4743743749881331000000000
  a = a + bmi_1 * 0.1463194102262433400000000
  
  # Sum from boolean values 
    
    a = a + b_breastcancer * 0.9128754137084009700000000
  a = a + b_colorectal * 0.4465582900206094300000000
  a = a + b_endometrial * 0.8550002865110695200000000
  a = a + b_manicschiz * 0.4385483915097909700000000
  a = a + b_pos * 0.6853665306190324100000000
  a = a + b_type2 * 0.2969025294844695500000000
  
  # Sum from interaction terms 
    
    
    # Calculate the score itself 
    score = (1 - pow(survivor[surv], exp(a)) )
  return(score)
  }

