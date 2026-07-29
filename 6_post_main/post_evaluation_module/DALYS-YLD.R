# library(readr)
# 
# # YDL
# 
# # disability_weights_gbd2019 <- read_csv("./data/disability_weights_gbd2019.csv")
# # disability_weights_gbd2019 <- read_csv('./post_evaluation_module/cancer_disability_weights_for_DALYs.csv') %>% 
# #   rename(Disease = 1, DW_point_estimate = 2, Notes = 3) %>% 
# #   bind_rows(disability_weights_gbd2019)
# 
# # dws <- disability_weights_gbd2019 %>% 
# #   select(dw = DW_point_estimate, disease = Disease)
# 
# dws <- data.table(
#   disease = c(
#     "stroke","chd","diabetes","hypothyroidism","asthma","copd",
#     "non_diabetic_hyperglycaemia","chronic_kidney_disease","dementia",
#     "heart_failure","lung_cancer","prostate_cancer","female_breast_cancer",
#     "colorectal_cancer",
#     "atrial_fibrillation","rheumatoid_arthritis","osteoarthritis",
#     "epilepsy","osteoporosis","kidney_cancer","oesophageal_cancer",
#     "stomach_cancer","oral_cancer","pancreatic_cancer","uterine_cancer",
#     "ovarian_cancer","blood_cancer"
#   ),
#   dw = c(
#     0.118,0.124,0.049,0.051,0.043,0.150,
#     0.000,0.073,0.477,
#     0.201,0.451,0.100,0.200,
#     0.288,
#     0.035,0.230,0.165,
#     0.263,0.040,0.300,0.420,
#     0.380,0.320,0.540,0.240,
#     0.360,0.310
#   )
# )
# 
# # pp <- read.fst('past_populations/past_populations_sppg_asthma_copd_depression_ndh_03_12_2025_1741.fst')
# pp <- past_populations
# 
# isCost <- pp %>% 
#   select(
#     c(
#       stroke,               chd,                   diabetes,             hypothyroidism,      
#       asthma,               copd,                  non_diabetic_hyperglycaemia,                  chronic_kidney_disease,      
#       dementia,             heart_failure,         lung_cancer,          prostate_cancer,     
#       female_breast_cancer,         colorectal_cancer,          atrial_fibrillation,  rheumatoid_arthritis,
#       osteoarthritis,       epilepsy,              osteoporosis,         kidney_cancer,        
#       oesophageal_cancer,   stomach_cancer,        oral_cancer,          pancreatic_cancer,   
#       uterine_cancer,       ovarian_cancer,        blood_cancer#`Multiple myeloma`,     Lymphoma,            
#       # Leukaemia 
#     ) ) %>% 
#   as.matrix() %>% 
#   {.!=0}
# 
# setDT(dws)
# 
# # dws[disease == 'stroke', dw]
# 
# dw_plain <- matrix( 
#   nrow = nrow(pp), ncol = ncol(isCost), byrow=F,
#   c(rep(dws[disease=='stroke',dw],nrow(pp)),
#     rep(dws[disease=='chd',dw],nrow(pp)),
#     rep(dws[disease=='diabetes',dw],nrow(pp)),
#     rep(dws[disease=='hypothyroidism',dw],nrow(pp)),
#     rep(dws[disease=='asthma',dw],nrow(pp)),
#     rep(dws[disease=='copd',dw],nrow(pp)),
#     rep(dws[disease=='non_diabetic_hyperglycaemia',dw],nrow(pp)),
#     rep(dws[disease=='chronic_kidney_disease',dw],nrow(pp)),
#     rep(dws[disease=='dementia',dw],nrow(pp)),
#     rep(dws[disease=='heart_failure',dw],nrow(pp)),
#     rep(dws[disease=='lung_cancer',dw],nrow(pp)),
#     rep(dws[disease=='prostate_cancer',dw],nrow(pp)),
#     rep(dws[disease=='female_breast_cancer',dw],nrow(pp)),
#     rep(dws[disease=='colorectal_cancer',dw],nrow(pp)),
#     rep(dws[disease=='atrial_fibrillation',dw],nrow(pp)),
#     rep(dws[disease=='rheumatoid_arthritis',dw],nrow(pp)),
#     rep(dws[disease=='osteoarthritis',dw],nrow(pp)),
#     rep(dws[disease=='epilepsy',dw],nrow(pp)),
#     rep(dws[disease=='osteoporosis',dw],nrow(pp)),
#     rep(dws[disease=='kidney_cancer',dw],nrow(pp)),
#     rep(dws[disease=='oesophageal_cancer',dw],nrow(pp)),
#     rep(dws[disease=='stomach_cancer',dw],nrow(pp)),
#     rep(dws[disease=='oral_cancer',dw],nrow(pp)),
#     rep(dws[disease=='pancreatic_cancer',dw],nrow(pp)),
#     rep(dws[disease=='uterine_cancer',dw],nrow(pp)),
#     rep(dws[disease=='ovarian_cancer',dw],nrow(pp)),
#     rep(dws[disease=='blood_cancer',dw],nrow(pp))
#   )
# )
# 
# DWFilter <- dw_plain * isCost 
# 
# # disease_total_dws <- colSums(DWFilter)
# # people_total_dws <- rowSums(DWFilter)
# 
# # 1 - colProd(1 - DWFilter)
# 
# combined_dw <- apply(simplify = F,(1-DWFilter), MARGIN = 1, function(x) prod(x,na.rm = T))
# 
# # 3.	Combine with GBD comorbidity formula:
# #   DW_{\text{combined}} = 1 - \prod_{i} (1 - DW_i)
# 
# dw_all <- cbind(pp[,.(run,year)], DWFilter,combined_dw=unlist(combined_dw))
# 
# dw_all_long <- melt(
#   dw_all,
#   id.vars = c('run','year'),
#   variable.name = 'disease',
#   value.name = 'dw'
# )
# 
# dw_all_long <- dw_all_long[, .(total_dw = sum(dw)), by=.(run,year,disease)]
# 
# dw_all_long2 <- dw_all_long[,.(total_dw = mean(total_dw)*model_specification$population$scale_down_factor) ,
#                                 by = .(year,disease)]

  # pp <- past_populations
daly_yld_fn <- function(past_populations, group_vars= as.character(), year_cut_off=NULL){
 
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }

dws <- data.table(
  disease = c( "stroke","chd","diabetes","hypothyroidism","asthma","copd", "non_diabetic_hyperglycaemia","chronic_kidney_disease","dementia", "heart_failure","lung_cancer","prostate_cancer","female_breast_cancer", "colorectal_cancer", "atrial_fibrillation","rheumatoid_arthritis","osteoarthritis", "epilepsy","osteoporosis","kidney_cancer","oesophageal_cancer", "stomach_cancer","oral_cancer","pancreatic_cancer","uterine_cancer", "ovarian_cancer","blood_cancer"
  ),
  dw = c( 0.118,0.124,0.049,0.051,0.043,0.150, 0.000,0.073,0.477, 0.201,0.451,0.100,0.200, 0.288, 0.035,0.230,0.165, 0.263,0.040,0.300,0.420, 0.380,0.320,0.540,0.240, 0.360,0.310
  )
)
  isCost <- past_populations %>% 
    select(
      c(
        stroke,               chd,                   diabetes,             hypothyroidism,      
        asthma,               copd,                  non_diabetic_hyperglycaemia,                  chronic_kidney_disease,      
        dementia,             heart_failure,         lung_cancer,          prostate_cancer,     
        female_breast_cancer,         colorectal_cancer,          atrial_fibrillation,  rheumatoid_arthritis,
        osteoarthritis,       epilepsy,              osteoporosis,         kidney_cancer,        
        oesophageal_cancer,   stomach_cancer,        oral_cancer,          pancreatic_cancer,   
        uterine_cancer,       ovarian_cancer,        blood_cancer )) %>% 
    as.matrix() %>% 
    {.!=0}
  
  setDT(dws)
  
  dw_plain <- matrix( 
    nrow = nrow(past_populations), ncol = ncol(isCost), byrow=F,
    c(rep(dws[disease=='stroke',dw],nrow(past_populations)),
      rep(dws[disease=='chd',dw],nrow(past_populations)),
      rep(dws[disease=='diabetes',dw],nrow(past_populations)),
      rep(dws[disease=='hypothyroidism',dw],nrow(past_populations)),
      rep(dws[disease=='asthma',dw],nrow(past_populations)),
      rep(dws[disease=='copd',dw],nrow(past_populations)),
      rep(dws[disease=='non_diabetic_hyperglycaemia',dw],nrow(past_populations)),
      rep(dws[disease=='chronic_kidney_disease',dw],nrow(past_populations)),
      rep(dws[disease=='dementia',dw],nrow(past_populations)),
      rep(dws[disease=='heart_failure',dw],nrow(past_populations)),
      rep(dws[disease=='lung_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='prostate_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='female_breast_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='colorectal_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='atrial_fibrillation',dw],nrow(past_populations)),
      rep(dws[disease=='rheumatoid_arthritis',dw],nrow(past_populations)),
      rep(dws[disease=='osteoarthritis',dw],nrow(past_populations)),
      rep(dws[disease=='epilepsy',dw],nrow(past_populations)),
      rep(dws[disease=='osteoporosis',dw],nrow(past_populations)),
      rep(dws[disease=='kidney_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='oesophageal_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='stomach_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='oral_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='pancreatic_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='uterine_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='ovarian_cancer',dw],nrow(past_populations)),
      rep(dws[disease=='blood_cancer',dw],nrow(past_populations))
    )
  )
  
  DWFilter <- dw_plain * isCost 
  combined_dw <- 1-unlist(apply(simplify = F,(1-DWFilter), MARGIN = 1, function(x) prod(x,na.rm = T)))
  
  dw_all <- cbind(  past_populations[,c('run', group_vars, 'intervention','year'), with = FALSE] ,
                    DWFilter,
                    combined_dw=unlist(combined_dw))
  
  print(1)
  print(names(dw_all))
  dw_all_long <- melt( dw_all, id.vars = c('run',group_vars,'intervention','year'), variable.name = 'disease', value.name = 'dw')
  print(2)
  dw_all_long <- dw_all_long[, .(total_dw = sum(dw)), by=c(group_vars,'intervention', 'run','year','disease')]
  
  dw_all_long2 <- dw_all_long[,.(total_dw = mean(total_dw)*model_specification$population$scale_down_factor), by = c(group_vars,'intervention','year','disease')]
  dw_all_long2
}

# x <- daly_yld_fn(past_populations ,'mdm_quintile_soa_name')

# count(x,disease=='combined_dw', wt=total_dw)

