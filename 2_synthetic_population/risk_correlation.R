library(MASS)  # for mvrnorm

select <- dplyr::select

#############  INFLATION FACTOR for KENDAL TO SPEARMAN  ############

# https://blogs.sas.com/content/iml/2023/04/05/interpret-spearman-kendall-corr.html?utm_source=chatgpt.com
# Approximations for Bivariate Normal Distributions:
# Kendall's Tau from Pearson's r: τ ≈ (2/π) * arcsin(r)
# Pearson's r from Kendall's Tau: r ≈ sin((π/2) * τ) 

kendall_pearson_convert <- function(value, from = c("tau", "r")) {
  from <- match.arg(from)
  # several.ok = TRUE #
  # choices #
  # arg #
  # Ensures the user inputs a valid option from a defined option set
  # Allows for partial matching of argument strings (e.g., "t" matches "tau" if unambiguous).
  
  if (from == "tau") {
    # Convert Kendall's tau to Pearson's r
    r <- sin((pi / 2) * value)
    return(r)
  } else {
    
    # Convert Pearson's r to Kendall's tau
    tau <- (2 / pi) * asin(value)
    return(tau)
  }
}


# Step 1: Define the correlation matrix you want to simulate
cor_matrix <- matrix(c(
  1.0, 0.7, 0.5,
  0.7, 1.0, 0.6,
  0.5, 0.6, 1.0
), nrow = 3, byrow = TRUE)

colnames(cor_matrix) <- rownames(cor_matrix) <- c("BMI", "Cholesterol", "SBP")

# Step 2: Generate multivariate normal variables with that correlation
set.seed(123)
n <- 1000  # number of synthetic individuals

normal_data <- mvrnorm(n = n, mu = c(0, 0, 0), Sigma = cor_matrix)

# Step 3: Convert to uniform [0,1] using the normal cumulative distribution function (CDF)
uniform_data <- pnorm(normal_data)

# Now `uniform_data` is a matrix of uniform(0,1) values with the desired correlation
colnames(uniform_data) <- c("BMI_q", "Chol_q", "SBP_q")

# Step 4 (Optional): Check correlation of these quantiles
cor(round(uniform_data, 2))

##################----------------------------
#ChatGPT
# https://chatgpt.com/share/6850ab10-0a98-8011-a299-65d2c886a1b5
##################----------------------------


# our example

comorbidity_matrix <- matrix(c(
  NA,  52, 14, 13, 11, 22, 13, 24, 17,  3,  9,
  18, NA,  5, 10,  6, 18,  8, 19, 14,  2, 22,
  59, 57, NA, 36, 26, 23, 18, 23, 17,  4,  3,
  29, 61,  8, NA, 13, 19, 12, 22, 21,  5,  6,
  37, 55, 21, 20, NA, 19, 13, 18, 14,  5,  7,
  23, 54,  6,  9,  6, NA,  8, 21, 18,  2, 18,
  19, 33,  6,  8,  6, 11, NA, 23, 18,  2, 14,
  16, 36,  3,  6,  3, 13, 10, NA, 31,  3, 13,
  10, 23,  2,  5,  2,  9,  7, 27, NA,  3, 25,
  21, 41,  6,  6, 18, 10, 13,  9, 17, NA,  5
), nrow = 10, byrow = TRUE)

rownames(comorbidity_matrix) <- c(
  "Coronary heart disease",
  "Hypertension",
  "Heart failure",
  "Stroke/TIA",
  "Atrial fibrillation",
  "Diabetes",
  "COPD",
  "Painful condition",
  "Depression",
  "Dementia"
)

colnames(comorbidity_matrix) <- c(
  "CHD",
  "Hypertension",
  "Heart failure",
  "Stroke",
  "Atrial fibrillation",
  "Diabetes",
  "COPD",
  "Painful condition",
  "Depression",
  "Dementia",
  "No other condition"
)

# To view it as a data frame:
comorbidity_df <- as.data.frame(comorbidity_matrix)

comorbidity_matrix[is.na(comorbidity_matrix)] <- 100

comorbidity_matrix <- comorbidity_matrix/100

comorbidity_matrix <- comorbidity_matrix[1:10,1:10]

library(Matrix)
library(corpcor)  # for make.positive.definite()


# Force symmetry: average with transpose
comorbidity_sym <- (comorbidity_matrix + t(comorbidity_matrix)) / 2

# Make it positive definite
comorbidity_pd <- make.positive.definite(comorbidity_sym)

# View eigenvalues to confirm
eigen(comorbidity_pd)$values


set.seed(123)
n <- 1000 # number of synthetic individuals

normal_data <- mvrnorm(n = n, mu = rep(0, nrow(comorbidity_matrix)), Sigma = comorbidity_pd)

# Step 3: Convert to uniform [0,1] using the normal cumulative distribution function (CDF)
uniform_data <- pnorm(normal_data)

# Now `uniform_data` is a matrix of uniform(0,1) values with the desired correlation

# Step 4 (Optional): Check correlation of these quantiles
# cor(method = 'kendall',round(uniform_data, 2)) |> 
#   corrplot::corrplot()


#######################

vals <- c(
  # row 1
  1,
  # row 2
  0.12, 1,
  # row 3
  0.038, 0.05, 1,
  # row 4
  0.001, -0.004, 0.088, 1,
  # row 5
  0.064, 0.054, 0.049, 0.047, 1,
  # row 6
  0.046, 0.051, 0.019, 0.012, 0.039, 1,
  # row 7
  0.043, -0.007, 0.019, 0.043, 0.016, 0.141, 1,
  # row 8
  0.015, 0.012, 0.025, 0.025, 0.038, 0.030, -0.093, 1,
  # row 9
  0.119, 0.064, 0.134, 0.067, 0.063, 0.208, 0.147, 0.113, 1,
  # row 10
  0.080, 0.006, 0.107, -0.007, 0.042, 0.077, 0.035, 0.024, 0.067, 1,
  # row 11
  0.072, -0.035, 0.015, -0.033, 0.026, 0.123, 0.068, 0.029, 0.057, 0.268, 1,
  # row 12
  0.012, 0.010, 0.036, 0.041, 0.039, -0.010, 0.035, -0.003, 0.019, 0.043, 0.061, 1,
  # row 13
  0.059, 0.056, 0.124, -0.013, -0.011, 0.041, 0.009, -0.007, 0.231, -0.063, -0.150, -0.031, 1,
  # row 14
  0.043, 0.013, 0.003, -0.005, 0.009, -0.017, 0.018, -0.019, 0.005, 0.008, 0.062, 0.039, -0.014, 1
)

# build the matrix
n <- 14
M <- matrix(NA_real_, n, n)
M[upper.tri(M, diag=T)] <- vals
M[lower.tri(M)] <- t(M)[lower.tri(M)]

# name the rows/cols
# vars <- paste0("V", 1:n)
vars <-c(
      "smoking", "alcohol", "diet", "physical_activity", "sleep",
      "hypertension", "diabetes", "non_HDL", "waist_to_hip",
      "education", "income", "depression", "grip", "air_pollution"
    )
our_var_names <- c("smoking", "alcohol", "diet", "physical_activity", "sleep",
"hypertension", "diabetes", "cholesterol", "bmi",
"education", "income", "depression", "grip", "air_pollution")

dimnames(M) <- list(our_var_names, our_var_names)

# view
# print(M)

###
### GENERATE correlated streams of uniformly distributed random numbers
###

# Step 2: Generate multivariate normal variables with that correlation
risks_to_include = c("smoking",
                     "alcohol", 
                     "diet", 
                     "physical_activity", 
                     "sleep",
                     "hypertension", 
                     "diabetes",
                     "cholesterol",
                     "bmi",
                     #"education",
                     #"income", 
                     #"depression",
                     #"grip",
                     "air_pollution")

set.seed(123)

#n <- 1.9e6/model_specification$population$scale_down_factor  # number of synthetic individuals
n <- 1.9e5

M <- kendall_pearson_convert(M, from = 'tau')
pearson_correlation_matrix <- M

M <- M[risks_to_include,risks_to_include]

normal_data <- mvrnorm(n = n, mu = rep(0, nrow(M)), Sigma = M)

# Step 3: Convert to uniform [0,1] using the normal cumulative distribution function (CDF)
uniform_data <- pnorm(normal_data)

# Now `uniform_data` is a matrix of uniform(0,1) values with the desired correlation
colnames(uniform_data) <- risks_to_include

# Step 4 (Optional): Check correlation of these quantiles
cor(round(uniform_data, 2))

# set.seed(123)

apply_correlated_quantiles <- function(current_population, 
                                       correlation_matrix = pearson_correlation_matrix, 
                                       model_configuration_list,
                                       risks_to_include = c("smoking",
                                                            "alcohol", 
                                                            "diet", 
                                                            "physical_activity", 
                                                            "sleep",
                                                            "hypertension", 
                                                            "diabetes",
                                                            "cholesterol",
                                                            "bmi",
                                                            #"education",
                                                            #"income", 
                                                            "depression",
                                                            #"grip",
                                                            "air_pollution")
                                       ) {
  
  our_var_names <- c("smoking", "alcohol", "diet", "physical_activity", "sleep",
                     "hypertension", "diabetes", "cholesterol", "bmi",
                     "education", "income", "depression", "grip", "air_pollution")
  
  risks_to_include <- match.arg(risks_to_include,
            several.ok = TRUE,
            choices = our_var_names)
  
  if(length(risks_to_include) == 1){
    
    current_population <- current_population |> 
      mutate('{risks_to_include}_percentile' := runif(n = n())
      )
    
    return(current_population)
    
  }
  
  #n <- 1.9e6/model_specification$population$scale_down_factor 
  
  n_ppl <- nrow(current_population)  # number of synthetic individuals
  
  
  correlation_matrix <- correlation_matrix[risks_to_include,risks_to_include]
  
  normal_data <- mvrnorm(n = n_ppl, 
                         mu = rep(0, nrow(correlation_matrix)),
                         Sigma = correlation_matrix)
  
  # Step 3: Convert to uniform [0,1] using the normal cumulative distribution function (CDF)
  uniform_data <- pnorm(normal_data)
  
  # Now `uniform_data` is a matrix of uniform(0,1) values with the desired correlation
  colnames(uniform_data) <- paste0(risks_to_include,'_percentile')
  
  
  # Step 4 (Optional): Check correlation of these quantiles
  #cor(round(uniform_data, 2))

  if(length(risks_to_include) != 1){
    
  cbind(current_population, uniform_data)
    
  }else if(length(risks_to_include) == 1){
    
    current_population |> 
      mutate('{risks_to_include}_percentile' := runif(n = n())
             )
    
    }else{
   stop( 'Ran out of conditions in logic checking loop!/n
   How many risks_to_include do you have in the function argument?')
    }
  
}


########################################################
###################### Test ############################
########################################################
# test_population <- instantiate_base_pop()
# 
# test_population <- apply_correlated_quantiles(test_population,
#                                               correlation_matrix = pearson_correlation_matrix)
# 
# 
# test_population <- apply_correlated_quantiles(test_population,
#                                               risks_to_include = 'bmi',
#                                               correlation_matrix = pearson_correlation_matrix)
# 
# view(test_population)

######################################
# as a general rule 
# if being consistent throughout the code base
######################################
# the progression goes from being 
#  lower quantile/ percentile   -> higher quantile/ percentile
#  good health behaviour/status -> worse health behaviour/status
####################################
 # smoking is distinct
 
 # it goes from being never -> used?? ->current
#########################################
############# ABOVE WORKS ###############
#########################################


# 
# v1 - V14
# 
# 
# matrix(c(
# c(1,0.12,0.038,0.001,0.064,0.046,0.043,0.015,0.119,0.08,0.072,0.012,0.059,0.043),
# c(NA,1,0.05,-0.004,0.054,0.051,-0.007,0.012,0.064,0.006,-0.036,0.01,0.056,0.013),
# c(NA,NA,1,0.088,0.049,0.019,0.019,0.025,0.134,0.107,0.015,0.036,0.124,0.003),
# c(NA,NA,NA,1,0.047,0.012,0.043,0.025,0.067,-0.007,-0.033,0.041,-0.013,-0.005),
# c(NA,NA,NA,NA,1,0.039, 0.016, 0.038,0.063,0.042,0.026,0.039,-0.011,0.009),
# c(NA,NA,NA,NA,NA,1,0.141,0.03,0.208,0.077,0.123,-0.01,0.041,-0.017),
# c(NA,NA,NA,NA,NA,NA,1,-0.093,0.147,0.035,0.068,0.035,0.009,0.018),
# c(NA,NA,NA,NA,NA,NA,NA,1,0.113,0.024,0.029,-0.003,-0.007,-0.019),
# c(NA,NA,NA,NA,NA,NA,NA,NA,1,0.067,0.057,0.019,0.231,0.005),
# c(NA,NA,NA,NA,NA,NA,NA,NA,NA,1,0.268,0.043,-0.063,0.008),
# c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,0.061,-0.15,0.062),
# c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,-0.031,0.039),
# c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,-0.014),
# c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1)
# ),nrow = 14,
# byrow=F,
# dimnames = 
# list( c(
#     "smoking", "alcohol", "diet", "physical_activity", "sleep", 
#     "hypertension", "diabetes", "non_HDL", "waist_to_hip", 
#     "education", "income", "depression", "grip", "air_pollution"
#   ),
#   c(
#     "smoking", "alcohol", "diet", "physical_activity", "sleep", 
#     "hypertension", "diabetes", "non_HDL", "waist_to_hip", 
#     "education", "income", "depression", "grip", "air_pollution"
#   ))
#   
# )





