qmortality_female <- function(
    age,
    town,
    alcohol_cat6,
    bmi,
    ethrisk, 
    b_AF, 
    b_CCF, 
    b_antipsychotic, 
    b_anycancer, 
    b_asthmacopd,
    b_carehome, 
    b_corticosteroids,
    b_cvd, 
    b_dementia, 
    b_epilepsy, 
    b_learning,
    b_legulcer,
    b_liverpancreas, 
    b_parkinsons, 
    b_poormobility, 
    b_ra,
    b_renal,
    b_type1,
    b_type2,
    b_vte,
    c_hb, 
    hes_admitprior_cat,
    high_lft, 
    high_platlet,
    s1_appetiteloss,
    s1_dyspnoea, 
    s1_weightloss,
    smoke_cat, 
    surv = 3
) {
  survivor <- c(0,
                0.984712481498718,
                0.971753656864166)
  
  Ialcohol   <- c(0, -0.17435199441964358, -0.2037985915003640,
                  -0.14878661948505154,  0.19346168639158545,
                  0.38177192393537790)
  Iethrisk   <- c(0, 0, -0.11843111221404919, -0.00799935876788652,
                  -0.08190248439786728, -0.20319672746383435,
                  -0.28785222648805714, -0.38639173968228352,
                  -0.63523396862698500, -0.14311168318806736)
  Ihesprior  <- c(0, 0.71833452784520391, 1.09080315448708310,
                  1.54518281409233980)
  Ismoke     <- c(0, 0.16717454421875455, 0.58142938294258628,
                  0.72878990923139852, 0.82857372978610067)
  
  # fractional polynomials
  dage  <- age / 10
  age_1 <- dage^(-2)
  age_2 <- dage^(-2) * log(dage)
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^(-1)
  bmi_2 <- dbmi^(-0.5)
  
  # centering
  age_1 <- age_1 - 0.017261177301407
  age_2 <- age_2 - 0.035034108906984
  bmi_1 <- bmi_1 - 0.369150280952454
  bmi_2 <- bmi_2 - 0.607577383518219
  town  <- town  - (-0.626556754112244)
  
  # start sum
  a <- 0
  a <- a + Ialcohol[alcohol_cat6 + 1]
  a <- a + Iethrisk[ethrisk + 1]
  a <- a + Ihesprior[hes_admitprior_cat + 1]
  a <- a + Ismoke[smoke_cat + 1]
  
  # continuous terms
  a <- a + age_1 * 1508.5794831250275
  a <- a + age_2 * -1177.0684822755481
  a <- a + bmi_1 *   21.11334055392112
  a <- a + bmi_2 *  -24.573072524627506
  a <- a + town  *    0.02409076971296671
  
  # boolean terms
  bools <- c(b_AF, b_CCF, b_antipsychotic, b_anycancer, b_asthmacopd,
             b_carehome, b_corticosteroids, b_cvd, b_dementia, b_epilepsy,
             b_learning, b_legulcer, b_liverpancreas, b_parkinsons,
             b_poormobility, b_ra, b_renal, b_type1, b_type2, b_vte,
             c_hb, high_lft, high_platlet, s1_appetiteloss,
             s1_dyspnoea, s1_weightloss)
  coefs <- c(0.33252351760672627, 0.50641683153270323,
             0.47626506440705629, 0.64720089405422188,
             0.17799721514652003, 0.58739598785067482,
             0.36337872520297626, 0.27105365193809716,
             0.95977317585312327, 0.20115762112157234,
             0.13839328199993772, 0.47612727692631140,
             0.47403357578242461, 0.59090563937374740,
             0.49004084666858500, 0.25677327584936344,
             0.67716274437051516, 0.31278391181876186,
             0.29737933061072769, 0.17800465837401402,
             0.65864248937343151, 0.47612237163655469,
             0.30756728264623784, 0.26034225706835656,
             0.28273682823800972, 0.22416624109881766)
  a <- a + sum(bools * coefs)
  
  # interaction: age_1 * ...
  a <- a + age_1 * as.numeric(hes_admitprior_cat==1) *   0.035158394211054912
  a <- a + age_1 * as.numeric(hes_admitprior_cat==2) * -109.516882327732820
  a <- a + age_1 * as.numeric(hes_admitprior_cat==3) * -351.354619048498590
  a <- a + age_1 * b_CCF          *  -19.172161434263753
  a <- a + age_1 * b_antipsychotic* -133.233197105191350
  a <- a + age_1 * b_anycancer    *  197.797643438127120
  a <- a + age_1 * b_carehome     *    8.764588290166934
  a <- a + age_1 * b_corticosteroids * -84.459168120389265
  a <- a + age_1 * b_cvd          * -152.383875352418870
  a <- a + age_1 * b_dementia     * -487.374569417972400
  a <- a + age_1 * b_legulcer     *  -18.804673168408211
  a <- a + age_1 * b_liverpancreas* -198.996688324183080
  a <- a + age_1 * b_poormobility * -277.250981464931100
  a <- a + age_1 * b_renal        * -324.788209319772530
  a <- a + age_1 * b_type2        * -338.299251439488330
  a <- a + age_1 * c_hb           * -276.089811631161580
  a <- a + age_1 * high_lft       *  -49.230107803540051
  a <- a + age_1 * s1_dyspnoea    * -302.957555394225840
  a <- a + age_1 * town           *  -30.921452232393211
  
  # interaction: age_2 * ...
  a <- a + age_2 * as.numeric(hes_admitprior_cat==1) *   38.370462859015539
  a <- a + age_2 * as.numeric(hes_admitprior_cat==2) *  124.562451872080520
  a <- a + age_2 * as.numeric(hes_admitprior_cat==3) *  301.561453025923750
  a <- a + age_2 * b_CCF          *   35.695415143517359
  a <- a + age_2 * b_antipsychotic*  108.845301352835190
  a <- a + age_2 * b_anycancer    *  -67.154511000286746
  a <- a + age_2 * b_carehome     *   35.012518187090471
  a <- a + age_2 * b_corticosteroids *  84.346405949139708
  a <- a + age_2 * b_cvd          *  107.406735753487740
  a <- a + age_2 * b_dementia     *  366.225528827329640
  a <- a + age_2 * b_legulcer     *   43.785689595416926
  a <- a + age_2 * b_liverpancreas* 175.394432662461010
  a <- a + age_2 * b_poormobility * 201.441238911152080
  a <- a + age_2 * b_renal        * 249.237290775473920
  a <- a + age_2 * b_type2        * 228.425789968301440
  a <- a + age_2 * c_hb           * 207.013578371244250
  a <- a + age_2 * high_lft       *  63.150714909979797
  a <- a + age_2 * s1_dyspnoea    * 220.400876764030870
  a <- a + age_2 * town           *  22.538836630393590
  
  score <- 100 * (1 - survivor[surv + 1]^exp(a))
  return(score)
}

qmortality_male <- function(
    age, alcohol_cat6, b_AF, b_CCF, b_antipsychotic, b_anycancer, b_asthmacopd,
    b_carehome, b_corticosteroids, b_cvd, b_dementia, b_epilepsy, b_learning,
    b_legulcer, b_liverpancreas, b_parkinsons, b_poormobility, b_ra, b_renal,
    b_type1, b_type2, b_vte, bmi, c_hb, ethrisk, hes_admitprior_cat,
    high_lft, high_platlet, s1_appetiteloss, s1_dyspnoea, s1_weightloss,
    smoke_cat, surv, town
) {
  survivor <- c(0, 0.979335904121399, 0.962403774261475)
  
  Ialcohol   <- c(0, -0.16708444071266923, -0.19847754653430327,
                  -0.14812069089436303,  0.10971438529866195,
                  0.15725394819345351)
  Iethrisk   <- c(0, 0, -0.24965163717239661, -0.26049990463239797,
                  -0.24133720755286936, -0.40394222834821164,
                  -0.34697110988245755, -0.34603088592097347,
                  -0.44272774017815186, -0.28942689209683875)
  Ihesprior  <- c(0, 0.68934794175929393, 1.05376621353689950,
                  1.44060870845246410)
  Ismoke     <- c(0, 0.16732266494368642, 0.53626307622435287,
                  0.64071300129429687, 0.76142775355784531)
  
  dage  <- age / 10
  age_1 <- dage^3
  age_2 <- dage^3 * log(dage)
  dbmi  <- bmi / 10
  bmi_1 <- dbmi^(-2)
  bmi_2 <- dbmi^(-2) * log(dbmi)
  
  age_1 <- age_1 - 412.15957641601562
  age_2 <- age_2 - 827.26068115234375
  bmi_1 <- bmi_1 -   0.134313449263573
  bmi_2 <- bmi_2 -   0.134822428226471
  town  <- town  - (-0.768538892269135)
  
  a <- 0
  a <- a + Ialcohol[alcohol_cat6 + 1]
  a <- a + Iethrisk[ethrisk + 1]
  a <- a + Ihesprior[hes_admitprior_cat + 1]
  a <- a + Ismoke[smoke_cat + 1]
  
  a <- a + age_1 *   0.036449264416623731
  a <- a + age_2 *  -0.012551397612884483
  a <- a + bmi_1 *   8.384319633450515
  a <- a + bmi_2 * -14.491169010690488
  a <- a + town  *   0.035864309976869284
  
  bools <- c(b_AF, b_CCF, b_antipsychotic, b_anycancer, b_asthmacopd,
             b_carehome, b_corticosteroids, b_cvd, b_dementia, b_epilepsy,
             b_learning, b_legulcer, b_liverpancreas, b_parkinsons,
             b_poormobility, b_ra, b_renal, b_type1, b_type2, b_vte,
             c_hb, high_lft, high_platlet, s1_appetiteloss,
             s1_dyspnoea, s1_weightloss)
  coefs <- c(0.24652795256939328, 0.55403227487129214,
             0.46739784638231002, 0.71554141921220937,
             0.14048940720646091, 0.47749740140274688,
             0.44814187560602808, 0.21949118375326637,
             0.85224240410802432, 0.21978453251695079,
             0.19790563863902949, 0.50725636587795975,
             0.39263687583033940, 0.76990750076242376,
             0.46367032348911708, 0.17109771611058100,
             0.62224745945823434, 0.25960721267267589,
             0.25404811490464563, 0.14940777815558187,
             0.74294697835779089, 0.51554764782943019,
             0.32097996321768341, 0.30320102141305078,
             0.25023795777686370, 0.21457581118166616)
  a <- a + sum(bools * coefs)
  
  # interaction: age_1
  a <- a + age_1 * as.numeric(hes_admitprior_cat==1) * -0.013532242327052574
  a <- a + age_1 * as.numeric(hes_admitprior_cat==2) * -0.016429972869646901
  a <- a + age_1 * as.numeric(hes_admitprior_cat==3) * -0.018419712704968640
  a <- a + age_1 * b_CCF          * -0.0056375981438788851
  a <- a + age_1 * b_antipsychotic*  0.00095485764796618411
  a <- a + age_1 * b_anycancer    * -0.031427877556200419
  a <- a + age_1 * b_carehome     *  0.0029194351023528052
  a <- a + age_1 * b_corticosteroids * -0.0087098588393828367
  a <- a + age_1 * b_cvd          * -0.00092605474117238176
  a <- a + age_1 * b_dementia     * -0.0042226968131332939
  a <- a + age_1 * b_legulcer     * -0.0043408879675686162
  a <- a + age_1 * b_liverpancreas* -0.011168661453064168
  a <- a + age_1 * b_parkinsons   *  0.00086079795284955377
  a <- a + age_1 * b_poormobility * -0.0032122568132298144
  a <- a + age_1 * b_renal        * -0.0062987873913596228
  a <- a + age_1 * b_type2        *  0.00085440051789764356
  a <- a + age_1 * c_hb           * -0.00109072606841751040
  a <- a + age_1 * high_lft       * -0.016529386751888780
  a <- a + age_1 * s1_dyspnoea    * -0.000075221143666536882
  a <- a + age_1 * town           * -0.00026717600937716028
  
  # interaction: age_2
  a <- a + age_2 * as.numeric(hes_admitprior_cat==1) *  0.0052599500739404469
  a <- a + age_2 * as.numeric(hes_admitprior_cat==2) *  0.0062723155776391801
  a <- a + age_2 * as.numeric(hes_admitprior_cat==3) *  0.0068996134993599611
  a <- a + age_2 * b_CCF          *  0.0020360323212952713
  a <- a + age_2 * b_antipsychotic* -0.00059264792400968123
  a <- a + age_2 * b_anycancer    *  0.012262202150461600
  a <- a + age_2 * b_carehome     * -0.0014190910591964706
  a <- a + age_2 * b_corticosteroids * 0.0031204374807641592
  a <- a + age_2 * b_cvd          *  0.00028128230371338202
  a <- a + age_2 * b_dementia     *  0.0011909366650632758
  a <- a + age_2 * b_legulcer     *  0.0014618943884676750
  a <- a + age_2 * b_liverpancreas* 0.0042685310896839612
  a <- a + age_2 * b_parkinsons   * -0.00071904072749267352
  a <- a + age_2 * b_poormobility *  0.0010674563935602573
  a <- a + age_2 * b_renal        *  0.0021250162138427274
  a <- a + age_2 * b_type2        * -0.00049606890104562657
  a <- a + age_2 * c_hb           *  0.00003369973359267090
  a <- a + age_2 * high_lft       *  0.0065189455055333261
  a <- a + age_2 * s1_dyspnoea    * -0.00015312682439225768
  a <- a + age_2 * town           *  0.00006758392503168273
  
  score <- 100 * (1 - survivor[surv + 1]^exp(a))
  return(score)
}