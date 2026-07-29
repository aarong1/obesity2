# 1:100/10 *(-0.1609) +
# (1:100/10)^2 * 0.0129 +
#  0.5660
# 
# 1:100/10 *(-0.0080 ) +
#   (1:100/10)^2 * 0.0007 +
#   0.85
# 
# 1:100/10 *(-0.0316 ) +
#   (1:100/10)^2 * -0.0051  +
#   1.8194 
##think the above refers to the fitting polynomial transforms of the cancer risk equations


# con <- dbConnect(duckdb::duckdb(), dbdir = 'past_populations_db/past_populations.duckdb', read_only = F)
# 
# x <- dbSendQuery(con, paste0('SELECT * FROM past_populations.','past_populations_20260116_015236',';'))  # Set cache size to 2MB
# 
# past_populations_20260116_015236
# 
# past_populations <- dbFetch(x)
# dbDisconnect(con, shutdown=TRUE)

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
# uws <- dws[, .(disease, uw = 1 - dw)]
# 
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
# 
# setDT(dws)
# 
# uw_plain <- matrix( 
#   nrow = nrow(pp), ncol = ncol(isCost), byrow=F,
#   c(rep(uws[disease=='stroke',uw],nrow(pp)),
#     rep(uws[disease=='chd',uw],nrow(pp)),
#     rep(uws[disease=='diabetes',uw],nrow(pp)),
#     rep(uws[disease=='hypothyroidism',uw],nrow(pp)),
#     rep(uws[disease=='asthma',uw],nrow(pp)),
#     rep(uws[disease=='copd',uw],nrow(pp)),
#     rep(uws[disease=='non_diabetic_hyperglycaemia',uw],nrow(pp)),
#     rep(uws[disease=='chronic_kidney_disease',uw],nrow(pp)),
#     rep(uws[disease=='dementia',uw],nrow(pp)),
#     rep(uws[disease=='heart_failure',uw],nrow(pp)),
#     rep(uws[disease=='lung_cancer',uw],nrow(pp)),
#     rep(uws[disease=='prostate_cancer',uw],nrow(pp)),
#     rep(uws[disease=='female_breast_cancer',uw],nrow(pp)),
#     rep(uws[disease=='colorectal_cancer',uw],nrow(pp)),
#     rep(uws[disease=='atrial_fibrillation',uw],nrow(pp)),
#     rep(uws[disease=='rheumatoid_arthritis',uw],nrow(pp)),
#     rep(uws[disease=='osteoarthritis',uw],nrow(pp)),
#     rep(uws[disease=='epilepsy',uw],nrow(pp)),
#     rep(uws[disease=='osteoporosis',uw],nrow(pp)),
#     rep(uws[disease=='kidney_cancer',uw],nrow(pp)),
#     rep(uws[disease=='oesophageal_cancer',uw],nrow(pp)),
#     rep(uws[disease=='stomach_cancer',uw],nrow(pp)),
#     rep(uws[disease=='oral_cancer',uw],nrow(pp)),
#     rep(uws[disease=='pancreatic_cancer',uw],nrow(pp)),
#     rep(uws[disease=='uterine_cancer',uw],nrow(pp)),
#     rep(uws[disease=='ovarian_cancer',uw],nrow(pp)),
#     rep(uws[disease=='blood_cancer',uw],nrow(pp))
#   )
# )
# 
# uwFilter <- uw_plain * isCost 
# 
# # disease_total_uws <- colSums(uwFilter)
# # people_total_uws <- rowSums(uwFilter)
# 
# # 1 - colProd(1 - uwFilter)
# 
# combined_uw <- apply(simplify = F,(1-uwFilter), MARGIN = 1, function(x) prod(x,na.rm = T))
# 
# # 3.	Combine with GBD comorbidity formula:
# #   uw_{\text{combined}} = 1 - \prod_{i} (1 - uw_i)
# 
# uw_all <- cbind(pp[,.(run,year)], uwFilter,combined_uw=unlist(combined_uw))
# 
# uw_all_long <- melt(
#   uw_all,
#   id.vars = c('run','year'),
#   variable.name = 'disease',
#   value.name = 'uw'
# )
# 
# uw_all_long <- uw_all_long[, .(total_uw = sum(uw)), by=.(run,year,disease)]
# 
# uw_all_long2 <- uw_all_long[,.(total_uw = mean(total_uw)*model_specification$population$scale_down_factor) ,
#                             by = .(year,disease)]
# 



# pp <- past_populations
qaly_yld_fn <- function(past_populations, group_vars= as.character(), year_cut_off=NULL){
  
  if(!is.null(year_cut_off)){
    past_populations <- past_populations[year == year_cut_off,]
  }
  
  uws <- data.table(
    disease = c(
      "stroke","chd","diabetes","hypothyroidism","asthma","copd",
      "non_diabetic_hyperglycaemia","chronic_kidney_disease","dementia",
      "heart_failure","lung_cancer","prostate_cancer","female_breast_cancer",
      "colorectal_cancer",
      "atrial_fibrillation","rheumatoid_arthritis","osteoarthritis",
      "epilepsy","osteoporosis","kidney_cancer","oesophageal_cancer",
      "stomach_cancer","oral_cancer","pancreatic_cancer","uterine_cancer",
      "ovarian_cancer","blood_cancer"
    ),
    uw = 1-c(
      0.118,0.124,0.049,0.051,0.043,0.150,
      0.000,0.073,0.477,
      0.201,0.451,0.100,0.200,
      0.288,
      0.035,0.230,0.165,
      0.263,0.040,0.300,0.420,
      0.380,0.320,0.540,0.240,
      0.360,0.310
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
  
  setDT(uws)
  
  uw_plain <- matrix( 
    nrow = nrow(past_populations), ncol = ncol(isCost), byrow=F,
    c(rep(uws[disease=='stroke',uw],nrow(past_populations)),
      rep(uws[disease=='chd',uw],nrow(past_populations)),
      rep(uws[disease=='diabetes',uw],nrow(past_populations)),
      rep(uws[disease=='hypothyroidism',uw],nrow(past_populations)),
      rep(uws[disease=='asthma',uw],nrow(past_populations)),
      rep(uws[disease=='copd',uw],nrow(past_populations)),
      rep(uws[disease=='non_diabetic_hyperglycaemia',uw],nrow(past_populations)),
      rep(uws[disease=='chronic_kidney_disease',uw],nrow(past_populations)),
      rep(uws[disease=='dementia',uw],nrow(past_populations)),
      rep(uws[disease=='heart_failure',uw],nrow(past_populations)),
      rep(uws[disease=='lung_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='prostate_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='female_breast_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='colorectal_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='atrial_fibrillation',uw],nrow(past_populations)),
      rep(uws[disease=='rheumatoid_arthritis',uw],nrow(past_populations)),
      rep(uws[disease=='osteoarthritis',uw],nrow(past_populations)),
      rep(uws[disease=='epilepsy',uw],nrow(past_populations)),
      rep(uws[disease=='osteoporosis',uw],nrow(past_populations)),
      rep(uws[disease=='kidney_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='oesophageal_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='stomach_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='oral_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='pancreatic_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='uterine_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='ovarian_cancer',uw],nrow(past_populations)),
      rep(uws[disease=='blood_cancer',uw],nrow(past_populations))
    )
  )
  
  uwFilter <- uw_plain * isCost 
  uwFilter[uwFilter==0]=1
  
  combined_uw <- 1-unlist(apply((1-uwFilter), MARGIN = 1, function(x) prod(x,na.rm = T)))
  
  uw_all <- cbind(past_populations[,c('run', group_vars, 'intervention','year'),with = FALSE], uwFilter,combined_uw=unlist(combined_uw))
  
  uw_all_long <- melt( uw_all, id.vars = c('run',group_vars ,'intervention','year'), variable.name = 'disease', value.name = 'uw')
  
  #sum up all runs 
  uw_all_long <- uw_all_long[, .(total_uw = sum(uw)),  #,N=.N
                             by=c('run',group_vars, 'intervention','year','disease')]
  
  # mean over runs
  uw_all_long2 <- uw_all_long[,.(#N= mean(N)*model_specification$population$scale_down_factor,
                                 total_uw = mean(total_uw)*model_specification$population$scale_down_factor),
                              by = c('year',group_vars,'intervention','disease')]
  
  uw_all_long2
  
}

# 
# qaly_yld_fn <- function(past_populations, group_vars = character(), year_cut_off = NULL){
#   
#   if (!is.null(year_cut_off)) {
#     past_populations <- past_populations[year == year_cut_off, ]
#   }
#   
#   # Keep disease columns in one canonical order
#   disease_cols <- c(
#     "stroke","chd","diabetes","hypothyroidism","asthma","copd",
#     "non_diabetic_hyperglycaemia","chronic_kidney_disease","dementia",
#     "heart_failure","lung_cancer","prostate_cancer","female_breast_cancer",
#     "colorectal_cancer","atrial_fibrillation","rheumatoid_arthritis",
#     "osteoarthritis","epilepsy","osteoporosis","kidney_cancer",
#     "oesophageal_cancer","stomach_cancer","oral_cancer","pancreatic_cancer",
#     "uterine_cancer","ovarian_cancer","blood_cancer"
#   )
#   
#   # disability weights in same order as disease_cols
#   dw_vec <- c(
#     0.118,0.124,0.049,0.051,0.043,0.150,
#     0.000,0.073,0.477,0.201,0.451,0.100,0.200,
#     0.288,0.035,0.230,0.165,0.263,0.040,0.300,
#     0.420,0.380,0.320,0.540,0.240,0.360,0.310
#   )
#   uw_vec <- 1 - dw_vec
#   
#   # ensure data.table
#   data.table::setDT(past_populations)
#   
#   # matrix of condition presence (TRUE if non-zero)
#   is_case <- as.matrix(past_populations[, ..disease_cols] != 0)
#   
#   n <- nrow(past_populations)
#   k <- length(disease_cols)
#   
#   # utility per condition if present, else 1
#   # (rep uw_vec down rows)
#   uw_mat <- matrix(rep(uw_vec, each = n), nrow = n, ncol = k)
#   uw_eff <- ifelse(is_case, uw_mat, 1)
#   
#   # combined utility = product of utilities
#   # (use logs for stability)
#   uw_combined <- exp(rowSums(log(uw_eff)))
#   
#   # combined disability weight
#   dw_combined <- 1 - uw_combined
#   
#   # attach and aggregate
#   past_populations[, `:=`(
#     uw_combined = uw_combined,
#     dw_combined = dw_combined
#   )]
#   
#   # Summarise: mean over runs, scaled up
#   by_vars <- c("year", group_vars, "intervention")
#   out <- past_populations[, .(
#     mean_uw_combined = mean(uw_combined, na.rm = TRUE) * model_specification$population$scale_down_factor,
#     mean_dw_combined = mean(dw_combined, na.rm = TRUE) * model_specification$population$scale_down_factor
#   ), by = by_vars]
#   
#   out[]
# }
# 
