library(readxl)

stillbirths <- read_excel("data/registrar_general_annual_reports/Section 4 - Stillbirths_InfantDeaths_Tables_2023.xlsx", 
                                                             sheet = "Table 4.4", skip = 2)


infant_deaths <- read_excel("data/registrar_general_annual_reports/Section 4 - Stillbirths_InfantDeaths_Tables_2023.xlsx", 
                                                             sheet = "Table 4.6", skip = 2)
