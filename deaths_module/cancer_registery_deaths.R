(all_cancer_data_tables <- read_excel("data/NIcancer_registry/all_cancers_data_tables.xlsx", 
                                          sheet = "T18", skip = 5))

(lung_cancer_data_tables <- read_excel("data/NIcancer_registry/Lung_cancer_data_tables.xlsx", 
                                          sheet = "T24", skip = 5))

(colorectal_cancer_data_tables <- read_excel("data/NIcancer_registry/Colorectal_cancer_data_tables.xlsx", 
                                          sheet = "T24", skip = 5))

(prostate_cancer_data_tables <- read_excel("data/NIcancer_registry/Prostate_cancer_data_tables.xlsx", 
                                          sheet = "T22", skip = 5))

(female_breast_cancer_data_tables <- read_excel("data/NIcancer_registry/female_breast_cancer_data_tables.xlsx", 
                                          sheet = "T24", skip = 5))

(Oral_cancer_data_tables <- read_excel("data/NIcancer_registry/oral_cancer_data_tables.xlsx", 
                                          sheet = "T18", skip = 5))

(bladder_cancer_data_tables <- read_excel("data/NIcancer_registry/Bladder cancer data tables.xlsx", 
                                          sheet = "T20", skip = 5))
(brain_cancer_data_tables <- read_excel("data/NIcancer_registry/Brain cancer data tables.xlsx", 
                                          sheet = "T18", skip = 5))

(pancreatic_cancer_data_tables <- read_excel("data/NIcancer_registry/Pancreatic_cancer_data_tables.xlsx", 
                                          sheet = "T20", skip = 5))

(uterine_cancer_data_tables <- read_excel("data/NIcancer_registry/Uterine_cancer_data_tables.xlsx", 
                                          sheet = "T20", skip = 5))

(ovarian_cancer_data_tables <- read_excel("data/NIcancer_registry/Ovarian_cancer_data_tables.xlsx", 
                                          sheet = "T20", skip = 5))

(cervical_cancer_data_tables <- read_excel("data/NIcancer_registry/Cervical cancer data tables.xlsx", 
                                          sheet = "T20", skip = 5))

(renal_cancer_data_tables <- read_excel("data/NIcancer_registry/Kidney_cancer_data_tables.xlsx", 
                                           sheet = "T20", skip = 5))


bind_rows(
  lung_cancer_data_tables,
  colorectal_cancer_data_tables,
  prostate_cancer_data_tables,
  female_breast_cancer_data_tables,
  Oral_cancer_data_tables,
  bladder_cancer_data_tables,
  brain_cancer_data_tables,
  pancreatic_cancer_data_tables,
  uterine_cancer_data_tables,
  ovarian_cancer_data_tables,
  cervical_cancer_data_tables,
  renal_cancer_data_tables) %>% 
  filter(!str_starts(string = `Age at death`, 'All ages|Notes|Annual'))