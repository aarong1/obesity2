## QFracture-2016: Fracture and NOF Risk Wrappers (4-year fracture & NOF)

# 1. Fracture (4-year) risk, gendered wrapper
risk_qfracture_fracture4 <- function(
    age,
    alcohol_cat6,
    b_antidepressant,
    b_anycancer,
    b_asthmacopd,
    b_corticosteroids,
    b_cvd,
    b_dementia,
    b_endocrine,
    b_epilepsy2,
    b_falls,
    b_hrt_oest,
    b_liver,
    b_malabsorption,
    b_parkinsons,
    b_ra_sle,
    b_renal,
    b_type1,
    b_type2,
    bmi,
    ethrisk,
    fh_osteoporosis,
    smoke_cat,
    surv = 4,
    sex            # 'female' or 'male'
) {
  if (tolower(sex) == 'female') {
    risk_pct <- fracture4_female_raw(
      age, alcohol_cat6, b_antidepressant, b_anycancer,
      b_asthmacopd, b_corticosteroids, b_cvd, b_dementia,
      b_endocrine, b_epilepsy2, b_falls, b_hrt_oest,
      b_liver, b_malabsorption, b_parkinsons, b_ra_sle,
      b_renal, b_type1, b_type2, bmi, ethrisk,
      fh_osteoporosis, smoke_cat, surv
    )
  } else {
    risk_pct <- fracture4_male_raw(
      age, alcohol_cat6, b_antidepressant, b_anycancer,
      b_asthmacopd, b_carehome = 0, b_corticosteroids, b_cvd,
      b_dementia, b_epilepsy2, b_falls, b_liver,
      b_malabsorption, b_parkinsons, b_ra_sle,
      b_renal, b_type1, b_type2, bmi, ethrisk,
      fh_osteoporosis, smoke_cat, surv
    )
  }
  risk <- risk_pct / 100
  return(risk)
}

# 2. NOF (hip fracture) risk, gendered wrapper
risk_qfracture_nof <- function(
    age,
    alcohol_cat6,
    b_antidepressant,
    b_anycancer,
    b_asthmacopd,
    b_corticosteroids,
    b_cvd,
    b_dementia,
    b_endocrine,
    b_epilepsy2,
    b_falls,
    b_fracture4,
    b_hrt_oest,
    b_liver,
    b_parkinsons,
    b_ra_sle,
    b_renal,
    b_type1,
    b_type2,
    bmi,
    ethrisk,
    fh_osteoporosis,
    smoke_cat,
    surv = 4,
    sex
) {
  if (tolower(sex) == 'female') {
    risk_pct <- nof_female_raw(
      age, alcohol_cat6, b_antidepressant, b_anycancer,
      b_asthmacopd, b_corticosteroids, b_cvd, b_dementia,
      b_endocrine, b_epilepsy2, b_falls, b_fracture4,
      b_hrt_oest, b_liver, b_parkinsons, b_ra_sle,
      b_renal, b_type1, b_type2, bmi, ethrisk,
      smoke_cat, surv
    )
  } else {
    risk_pct <- nof_male_raw(
      age, alcohol_cat6, b_antidepressant, b_anycancer,
      b_asthmacopd, b_carehome = 0, b_corticosteroids, b_cvd,
      b_dementia, b_epilepsy2, b_falls, b_fracture4,
      b_liver, b_parkinsons, b_ra_sle,
      b_renal, b_type1, b_type2, bmi, ethrisk,
      fh_osteoporosis, smoke_cat, surv
    )
  }
  risk <- risk_pct / 100
  return(risk)
}

# 3. Simplified wo_risk_factors wrappers (only age, sex)
risk_qfracture_fracture4_wo_rf <- function(age, sex, surv = 4) {
  risk_qfracture_fracture4(
    age = age,
    alcohol_cat6 = 0,
    b_antidepressant = 0,
    b_anycancer = 0,
    b_asthmacopd = 0,
    b_corticosteroids = 0,
    b_cvd = 0,
    b_dementia = 0,
    b_endocrine = 0,
    b_epilepsy2 = 0,
    b_falls = 0,
    b_hrt_oest = 0,
    b_liver = 0,
    b_malabsorption = 0,
    b_parkinsons = 0,
    b_ra_sle = 0,
    b_renal = 0,
    b_type1 = 0,
    b_type2 = 0,
    bmi = 25,
    ethrisk = 1,
    fh_osteoporosis = 0,
    smoke_cat = 0,
    surv = surv,
    sex = sex
  )
}

risk_qfracture_nof_wo_rf <- function(age, sex, surv = 4) {
  risk_qfracture_nof(
    age = age,
    alcohol_cat6 = 0,
    b_antidepressant = 0,
    b_anycancer = 0,
    b_asthmacopd = 0,
    b_corticosteroids = 0,
    b_cvd = 0,
    b_dementia = 0,
    b_endocrine = 0,
    b_epilepsy2 = 0,
    b_falls = 0,
    b_fracture4 = 0,
    b_hrt_oest = 0,
    b_liver = 0,
    b_parkinsons = 0,
    b_ra_sle = 0,
    b_renal = 0,
    b_type1 = 0,
    b_type2 = 0,
    bmi = 25,
    ethrisk = 1,
    fh_osteoporosis = 0,
    smoke_cat = 0,
    surv = surv,
    sex = sex
  )
}