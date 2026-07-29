files <- dir( "data/NIcancer_registry/")
files <- files[str_detect(negate = T,files, "\\$")]

content_df <- tibble()
for( i in files){
  
  x <- readxl::read_excel( paste0("data/NIcancer_registry/", i),sheet = 'Contents',skip = 3) %>%
    dplyr::mutate(.before = 3, source_file = i) %>%
    dplyr::select( c(1,2,3)) %>% 
    # rename(year =1, prevalence =2) %>%
    print()
  
  content_df <- rbind(content_df,x)
  
}

prevalence_lookup <- filter(content_df, 
            `Table title` == "Number of cancer survivors (prevalence) by year") %>% 
  mutate(`tab` = str_remove(pattern = 'able ',string = `Table number`))

incidence_lookup <- filter(content_df, 
                           `Table title` == "Number of cases and incidence rates by year of diagnosis") %>% 
  mutate(`tab` = str_remove(pattern = 'able ',string = `Table number`))

mortality_lookup <- filter(content_df, 
                           `Table title` == "Number of deaths and mortality rates by year of death") %>% 
  mutate(`tab` = str_remove(pattern = 'able ',string = `Table number`))

incid_df <- tibble()
for( i in files){
  
  incidence_lookup %>% filter(source_file == i) %>% pull(tab) -> tab_num
  
  if( length(tab_num) ==0) next
x <- readxl::read_excel( paste0("data/NIcancer_registry/", i),sheet = tab_num ,skip = 5) %>%
  dplyr::mutate(.before = 3, source_file = i) %>%
  dplyr::select( c(1,2,3)) %>% 
  rename(year =1, incidence =2) %>%
    print()
    
incid_df <- rbind(incid_df,x)
  
}

prev_df <- tibble()
for( i in files){
  
  prevalence_lookup %>% filter(source_file == i) %>% pull(tab) -> tab_num
  
  if( length(tab_num) ==0) next
  
  x <- readxl::read_excel( paste0("data/NIcancer_registry/", i),sheet = tab_num,skip = 7,range = 'A8:C28') %>%
    dplyr::mutate(.before = 3, source_file = i) %>%
    dplyr::select( c(1,2,3)) %>% 
    rename(year =1, prevalence =2) %>%
    print()
  
  prev_df <- rbind(prev_df,x)
  
}

mortality_df <- tibble()
for( i in files){
  
  mortality_lookup %>% filter(source_file == i) %>% pull(tab) -> tab_num
  if( length(tab_num) ==0) next
  
  x <- readxl::read_excel( paste0("data/NIcancer_registry/", i),sheet = tab_num,skip = 5) %>%
    dplyr::mutate(.before = 3, source_file = i) %>%
    dplyr::select( c(1,2,3)) %>% 
    rename(year =1, deaths =2) %>%
    print()
  
  mortality_df <- rbind(mortality_df,x)
  
}

incid_df %>%
  filter(source_file != 'all_cancers_data_tables.xlsx') %>% 
  dplyr::mutate( year=as.numeric(year)) %>%
  dplyr::filter( year >= 2013) %>%
  # dplyr::group_by( year) %>%
  # dplyr::summarise( cases = sum( `cases` , na.rm = T)) %>%
  ggplot2::ggplot( ggplot2::aes( x = year, y = incidence)) +
  facet_wrap(~source_file)+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::labs( title = "NI Cancer Registry: All Malignant Tumours: Incidence",
                 y = "Number of Incident Cases",
                 x = "year")

prev_df %>%
  filter(source_file != 'all_cancers_data_tables.xlsx') %>% 
  dplyr::mutate( year=as.numeric(year)) %>%
  dplyr::filter( year >= 2013) %>%
  # dplyr::group_by( year) %>%
  # dplyr::summarise( cases = sum( `cases` , na.rm = T)) %>%
  ggplot2::ggplot( ggplot2::aes( x = year, y = prevalence)) +
  facet_wrap(~source_file) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::labs( title = "NI Cancer Registry: All Malignant Tumours: Prevalence",
                 y = "Number of Prevalent Cases",
                 x = "year")

mortality_df %>%
  filter(source_file != 'all_cancers_data_tables.xlsx') %>% 
  dplyr::mutate( year=as.numeric(year)) %>%
  dplyr::filter( year >= 2013) %>%
  # dplyr::group_by( year) %>%
  # dplyr::summarise( cases = sum( `cases` , na.rm = T)) %>%
  ggplot2::ggplot( ggplot2::aes( x = year, y = deaths)) +
  facet_wrap(~source_file)+
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::labs( title = "NI Cancer Registry: All Malignant Tumours: Deaths",
                 y = "Number of Deaths",
                 x = "year")


 
lung_case_fatality <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Lung')) %>% 
  filter(year == last(year)) %>% 
  pull(deaths )%>% divide_by(lung_prev_df%>% filter(year == last(year))%>% pull(prevalence))

colorectal_case_fatality <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Colorectal')) %>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(colorectal_prev_df%>% filter(year == last(year))%>% pull(prevalence))

oral_case_fatality <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Oral')) %>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(oral_prev_df%>% filter(year == last(year))%>% pull(prevalence))

pancreatic_case_fatality <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Pancreatic')) %>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(pancreatic_prev_df%>% filter(year == last(year))%>% pull(prevalence))

uterine_case_fatality <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Uterine')) %>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(uterine_prev_df%>% filter(year == last(year))%>% pull(prevalence))

blood_case_fatality <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Blood')) %>% 
  group_by(year) %>% 
  summarise(deaths = sum(deaths))%>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(blood_prev_df%>% filter(year == last(year))%>% pull(prevalence))

ovarian_case_fatality <- mortality_df %>%
  filter(str_detect(string = source_file, pattern = 'Ovarian'))%>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(ovarian_prev_df%>% filter(year == last(year))%>% pull(prevalence))

osteogastric_case_fatality <- mortality_df %>%
  filter(str_detect(string = source_file, pattern = 'Stomach|Oesophageal')) %>% 
  group_by(year) %>% 
  summarise(deaths = sum(deaths)) %>%
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(osteogastric_prev_df%>% filter(year == last(year))%>% pull(prevalence))

prostate_case_fatality <- mortality_df %>%
  filter(str_detect(string = source_file, pattern = 'Prostate'))%>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(prostate_prev_df%>% filter(year == last(year))%>% pull(prevalence))

breast_case_fatality <- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Female_breast_cancer'))%>% 
  filter(year == last(year))%>% 
  pull(deaths)%>% divide_by(breast_prev_df%>% filter(year == last(year))%>% pull(prevalence))

renal_case_fatality<- mortality_df %>% 
  filter(str_detect(string = source_file, pattern = 'Kidney'))%>% 
  filter(year == last(year))%>% 
  pull(deaths) %>% divide_by(renal_prev_df%>% filter(year == last(year))%>% pull(prevalence))
)

site_cancer_case_fatality <- tribble(
  ~ morbidity, ~case_fatality_rate,
'lung_cancer', lung_case_fatality,
'colorectal_cancer', colorectal_case_fatality,
'oral_cancer', oral_case_fatality,
'pancreatic_cancer', pancreatic_case_fatality,
'uterine_cancer', uterine_case_fatality,
'blood_cancer', blood_case_fatality,
'ovarian_cancer', ovarian_case_fatality,
'osteogastric_cancer', osteogastric_case_fatality,
'prostate_cancer', prostate_case_fatality,
'female_breast_cancer', breast_case_fatality,
'renal_cancer', renal_case_fatality
)

case_fatality_rate_df <- bind_rows(  
case_fatality_rate_df,
site_cancer_case_fatality
)

lung_prev_df %>% filter(year == last(year))%>% pull(prevalence)
colorectal_prev_df%>% filter(year == last(year))%>% pull(prevalence)
oral_prev_df%>% filter(year == last(year))%>% pull(prevalence)
pancreatic_prev_df%>% filter(year == last(year))%>% pull(prevalence)
uterine_prev_df%>% filter(year == last(year))%>% pull(prevalence)
blood_prev_df%>% filter(year == last(year))%>% pull(prevalence)
ovarian_prev_df%>% filter(year == last(year))%>% pull(prevalence)
osteogastric_prev_df%>% filter(year == last(year))%>% pull(prevalence)
prostate_prev_df%>% filter(year == last(year))%>% pull(prevalence)
breast_prev_df%>% filter(year == last(year))%>% pull(prevalence)
renal_prev_df%>% filter(year == last(year))%>% pull(prevalence)
