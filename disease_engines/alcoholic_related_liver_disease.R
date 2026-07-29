# Reference: Wang Z, Nirantharakumar K, Copland A, et al. 
# Estimating inequality in alcohol-related liver disease burden in the UK, 2009 to 2020: 
# a population-based study using routinely collected data. 
# The Lancet Primary Care 2025. https://doi.org/10.1016/j.lanprc.2025.100002
# STable 12. Annual Incidence of Probable ARLD (per 100,000 person-years)
# Data source: 2020 incidence rates (last year in study period)

# STable 12. Annual Incidence of Probable ARLD (per 100,000 person-years)
# 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
# Overall
# Number of Patients 2,070 2,034 2,189 2,056 2,313 2,295 2,764 3,076 3,389 3,656 3,888 3,382
# Person years 8757050 8853082 8929707 9058654 8972146 8945586 9090775 9318943 9510757 9704539 9712044 9671161
# Incidence 23·6 23·0 24·5 22·7 25·8 25·7 30·4 33·0 35·6 37·7 40·0 35·0
# Age, years
# 18-29
# Number of Patients 44 58 71 76 94 105 129 151 158 206 256 255
# Person years 1716525 1878509 2018919 2163630 2243458 2323625 2434790 2574405 2713636 2854526 2928436 2999280
# Incidence 2·6 3·1 3·5 3·5 4·2 4·5 5·3 5·9 5·8 7·2 8·7 8·5
# 30-39
# Number of Patients 256 244 286 274 325 336 416 502 592 589 694 622
# Person years 1612777 1638764 1660667 1698456 1697229 1712869 1761649 1829270 1896949 1965701 1996516 2025639
# Incidence 15·9 14·9 17·2 16·1 19·1 19·6 23·6 27·4 31·2 30·0 34·8 30·7
# 40-49
# Number of Patients 572 603 618 576 646 645 798 910 1,029 1,124 1,132 1,012
# Person years 1723955 1727852 1729299 1743534 1718494 1705767 1727391 1760277 1783242 1805121 1793163 1771264
# Incidence 33·2 34·9 35·7 33·0 37·6 37·8 46·2 51·7 57·7 62·3 63·1 57·1
# 50-59
# Number of Patients 574 581 644 579 678 660 807 869 907 1,029 1,067 922
# Person years 1321040 1314881 1308509 1310507 1291463 1279849 1293158 1314826 1326639 1337791 1325377 1301958
# Incidence 43·5 44·2 49·2 44·2 52·5 51·6 62·4 66·1 68·4 76·9 80·5 70·8
# 60-69
# Number of Patients 454 401 422 418 433 415 483 527 538 545 587 461
# Person years 1113693 1098206 1086347 1079607 1058407 1042718 1044072 1050528 1046308 1040356 1015505 977444
# Incidence 40·8 36·5 38·8 38·7 40·9 39·8 46·3 50·2 51·4 52·4 57·8 47·2
# 70-79
# Number of Patients 150 132 129 118 121 127 121 108 149 152 131 101
# Person years 757028 733144 710061 687937 652470 621605 601267 584717 562038 538757 507486 468770
# Incidence 19·8 18·0 18·2 17·2 18·5 20·4 20·1 18·5 26·5 28·2 25·8 21·5
# ≥80
# Number of Patients 20 15 19 15 16 7 10 9 17 11 21 9
# Person years 512123 461822 416006 375086 310719 259245 228549 205038 182085 162457 145758 127027
# Incidence 3·9 3·2 4·6 4·0 5·1 2·7 4·4 4·4 9·3 6·8 14·4 7·1

# Sex
# Male
# Number of Patients 1,504 1,458 1,560 1,470 1,686 1,610 1,959 2,194 2,416 2,541 2,758 2,276
# Person years 4364004 4410966 4442882 4505547 4461447 4462956 4538074 4654304 4757042 4861474 4871811 4850126
# Incidence 34·5 33·1 35·1 32·6 37·8 36·1 43·2 47·1 50·8 52·3 56·6 46·9
# Female
# Number of Patients 566 576 629 586 627 685 805 882 973 1,115 1,130 1,106
# Person years 4393047 4442117 4486826 4553107 4510699 4482631 4552702 4664639 4753715 4843065 4840234 4821036
# Incidence 12·9 13·0 14·0 12·9 13·9 15·3 17·7 18·9 20·5 23·0 23·3 22·9


# ARLD Incidence Data by Age Group (2020)
arld_incidence_by_age <- tribble(
  ~no, ~age_group, ~incidence_per_100k,
  1, "18-29",     8.5,
  1, "30-39",    30.7,
  1, "40-49",    57.1,
  1, "50-59",    70.8,
  1, "60-69",    47.2,
  1, "70-79",    21.5,
  1, "80+",       7.1
)

# ARLD Incidence Data by Sex (2020)
arld_incidence_by_sex <- tribble(
  ~no, ~sex,      ~incidence_per_100k,
  1, "Males",    46.9,
  1, "Females",  22.9
) %>% 
  mutate(prop = incidence_per_100k/sum(incidence_per_100k)*2) %>% 
  select(-incidence_per_100k)


arld_incidence <- arld_incidence_by_age %>% 
  full_join(arld_incidence_by_sex, by =c('no')) %>% 
  mutate(incidence_per_100k = incidence_per_100k*prop) %>% 
  mutate(arld_year_risk = incidence_per_100k/100000) 

tibble::tribble(
    ~age_group, ~incidence_per_100k, ~sex, ~prop, ~arld_year_risk,
    "18-29",  11.4,   "Males",   1.34,   0.000114,
    "18-29",  5.58,  "Females", 0.656,   0.0000558,
    "30-39",  41.3,   "Males",   1.34,   0.000413,
    "30-39",  20.1,   "Females", 0.656,   0.000201,
    "40-49",  76.7,   "Males",   1.34,   0.000767,
    "40-49",  37.5,   "Females", 0.656,   0.000375,
    "50-59",  95.1,   "Males",   1.34,   0.000951,
    "50-59",  46.5,   "Females", 0.656,   0.000465,
    "60-69",  63.4,   "Males",   1.34,   0.000634,
    "60-69",  31.0,   "Females", 0.656,   0.000310,
    "70-79",  28.9,   "Males",   1.34,   0.000289,
    "70-79",  14.1,   "Females", 0.656,   0.000141,
    "80-110",  9.54,  "Males",   1.34,   0.0000954,
    "80-110",  4.66,  "Females", 0.656,   0.0000466,
  )


apply_arld_risk <- function(input_population){
  
  arld_incidence <- tibble::tribble(
    ~age_group, ~incidence_per_100k, ~sex, ~prop, ~arld_year_risk,
    "18-29",  11.4,   "Males",   1.34,   0.000114,
    "18-29",  5.58,  "Females", 0.656,   0.0000558,
    "30-39",  41.3,   "Males",   1.34,   0.000413,
    "30-39",  20.1,   "Females", 0.656,   0.000201,
    "40-49",  76.7,   "Males",   1.34,   0.000767,
    "40-49",  37.5,   "Females", 0.656,   0.000375,
    "50-59",  95.1,   "Males",   1.34,   0.000951,
    "50-59",  46.5,   "Females", 0.656,   0.000465,
    "60-69",  63.4,   "Males",   1.34,   0.000634,
    "60-69",  31.0,   "Females", 0.656,   0.000310,
    "70-79",  28.9,   "Males",   1.34,   0.000289,
    "70-79",  14.1,   "Females", 0.656,   0.000141,
    "80-110",  9.54,  "Males",   1.34,   0.0000954,
    "80-110",  4.66,  "Females", 0.656,   0.0000466,
  ) %>% 
    select(age_arld = age_group, 
           sex = sex,
           arld_year_risk )
  
  year = max(input_population$year)
  
  input_population <- input_population %>% 
    filter(year == min(year)) %>% 
    mutate(age_arld = cut(age,breaks = c(-Inf,18, 29,39,49,59,69,79, Inf),
                         labels = c('0-18','18-29','30-39','40-49','50-59','60-69','70-79','80+')
    )) %>%
    select(-any_of('arld_year_risk'))
  
  input_population <- input_population %>% left_join(arld_incidence)
  
  input_population <- input_population %>% select(- c(age_arld))
  
  input_population <- input_population %>% 
    replace_na(list(arld_year_risk = 0)) 
}
