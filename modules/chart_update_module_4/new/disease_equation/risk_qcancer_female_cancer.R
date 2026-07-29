
  # blood_cancer ----
  ## female ----  
  blood_cancer_female_raw(
    age,bmi,c_hb,new_abdopain,new_haematuria,new_necklump,new_nightsweats,new_pmb,new_vte,new_weightloss,s1_bowelchange,s1_bruising
  )

  
  # The conditional arrays
    
    
    # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    
    # Sum from continuous values
    
    a = a + age_1 * 35.9405666896283120000000000
  a = a + age_2 * -68.8496375977904480000000000
  a = a + bmi_1 * 0.0785171223057501980000000
  a = a + bmi_2 * -5.3730627788681424000000000
  
  # Sum from boolean values
    
    a = a + c_hb * 1.7035866502297630000000000
  a = a + new_abdopain * 0.3779206239385797800000000
  a = a + new_haematuria * 0.4086662974598894700000000
  a = a + new_necklump * 2.9539029476671903000000000
  a = a + new_nightsweats * 1.3792892192392403000000000
  a = a + new_pmb * 0.4689216313440992500000000
  a = a + new_vte * 0.6036630662990674100000000
  a = a + new_weightloss * 0.8963398932306315700000000
  a = a + s1_bowelchange * 0.7291379612468620300000000
  a = a + s1_bruising * 1.0255003552753392000000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -7.4207849482565749000000000
  return score
  }

# End of blood_cancer
  
  # breast_cancer
  
  breast_cancer_female_raw(
    age,alcohol_cat4,bmi,fh_breastcancer,new_breastlump,new_breastpain,new_breastskin,new_pmb,new_vte,town
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ialcohol[4] = {
      0,
      0.0543813075945134560000000,
      0.1245709972983817800000000,
      0.1855198679261514700000000
    }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  town = town - -0.383295059204102
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ialcohol[alcohol_cat4]
  
  # Sum from continuous values
    
    a = a + age_1 * -14.3029484067898500000000000
  a = a + age_2 * -25.9301811377364260000000000
  a = a + bmi_1 * -1.7540983825680900000000000
  a = a + bmi_2 * 2.0601979121740364000000000
  a = a + town * -0.0160766972632234440000000
  
  # Sum from boolean values
    
    a = a + fh_breastcancer * 0.3863899675953914000000000
  a = a + new_breastlump * 3.9278533274888368000000000
  a = a + new_breastpain * 0.8779616078329102200000000
  a = a + new_breastskin * 2.2320296233987880000000000
  a = a + new_pmb * 0.4465053002248299800000000
  a = a + new_vte * 0.2728610297213165400000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -6.1261694200869234000000000
  return score
  }

breast_cancer_female_validation(
  age,alcohol_cat4,bmi,fh_breastcancer,new_breastlump,new_breastpain,new_breastskin,new_pmb,new_vte,town,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!i_in_range(alcohol_cat4,0,3)) {
    ok=0
    strlcat(errorBuf,"error: alcohol_cat4 must be in range (0,3)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(fh_breastcancer)) {
    ok=0
    strlcat(errorBuf,"error: fh_breastcancer must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_breastlump)) {
    ok=0
    strlcat(errorBuf,"error: new_breastlump must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_breastpain)) {
    ok=0
    strlcat(errorBuf,"error: new_breastpain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_breastskin)) {
    ok=0
    strlcat(errorBuf,"error: new_breastskin must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_pmb)) {
    ok=0
    strlcat(errorBuf,"error: new_pmb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!d_in_range(town,-7,11)) {
    ok=0
    strlcat(errorBuf,"error: town must be in range (-7,11)\n",errorBufSize)
  }
  return ok
}

breast_cancer_female(
  age,alcohol_cat4,bmi,fh_breastcancer,new_breastlump,new_breastpain,new_breastskin,new_pmb,new_vte,town,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = breast_cancer_female_validation(age,alcohol_cat4,bmi,fh_breastcancer,new_breastlump,new_breastpain,new_breastskin,new_pmb,new_vte,town,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return breast_cancer_female_raw(age,alcohol_cat4,bmi,fh_breastcancer,new_breastlump,new_breastpain,new_breastskin,new_pmb,new_vte,town)
}

# End of breast_cancer
  
  # cervical_cancer
  
  cervical_cancer_female_raw(
    age,bmi,c_hb,new_abdopain,new_haematuria,new_imb,new_pmb,new_postcoital,new_vte,smoke_cat,town
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ismoke[5] = {
      0,
      0.3247875277095715300000000,
      0.7541211259076738800000000,
      0.7448343035139659600000000,
      0.6328348533913806800000000
    }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  town = town - -0.383295059204102
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values
    
    a = a + age_1 * 10.1663393107505800000000000
  a = a + age_2 * -16.9118902491100020000000000
  a = a + bmi_1 * -0.5675143308052614800000000
  a = a + bmi_2 * -2.6377586334504044000000000
  a = a + town * 0.0573200669650633030000000
  
  # Sum from boolean values
    
    a = a + c_hb * 1.2205973555195053000000000
  a = a + new_abdopain * 0.7229870191773574200000000
  a = a + new_haematuria * 1.6126499968790107000000000
  a = a + new_imb * 1.9527008812518938000000000
  a = a + new_pmb * 3.3618997560756485000000000
  a = a + new_postcoital * 3.1391568551730864000000000
  a = a + new_vte * 1.1276327958138455000000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -8.8309098444401926000000000
  return score
  }

cervical_cancer_female_validation(
  age,bmi,c_hb,new_abdopain,new_haematuria,new_imb,new_pmb,new_postcoital,new_vte,smoke_cat,town,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(c_hb)) {
    ok=0
    strlcat(errorBuf,"error: c_hb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_haematuria)) {
    ok=0
    strlcat(errorBuf,"error: new_haematuria must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_imb)) {
    ok=0
    strlcat(errorBuf,"error: new_imb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_pmb)) {
    ok=0
    strlcat(errorBuf,"error: new_pmb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_postcoital)) {
    ok=0
    strlcat(errorBuf,"error: new_postcoital must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!i_in_range(smoke_cat,0,4)) {
    ok=0
    strlcat(errorBuf,"error: smoke_cat must be in range (0,4)\n",errorBufSize)
  }
  if (!d_in_range(town,-7,11)) {
    ok=0
    strlcat(errorBuf,"error: town must be in range (-7,11)\n",errorBufSize)
  }
  return ok
}

cervical_cancer_female(
  age,bmi,c_hb,new_abdopain,new_haematuria,new_imb,new_pmb,new_postcoital,new_vte,smoke_cat,town,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = cervical_cancer_female_validation(age,bmi,c_hb,new_abdopain,new_haematuria,new_imb,new_pmb,new_postcoital,new_vte,smoke_cat,town,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return cervical_cancer_female_raw(age,bmi,c_hb,new_abdopain,new_haematuria,new_imb,new_pmb,new_postcoital,new_vte,smoke_cat,town)
}

# End of cervical_cancer
  
  # colorectal_cancer
  
  colorectal_cancer_female_raw(
    age,alcohol_cat4,bmi,c_hb,fh_gicancer,new_abdodist,new_abdopain,new_appetiteloss,new_rectalbleed,new_vte,new_weightloss,s1_bowelchange,s1_constipation
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ialcohol[4] = {
      0,
      0.2429014262884695900000000,
      0.2359224520197608100000000,
      0.4606605934539446100000000
    }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ialcohol[alcohol_cat4]
  
  # Sum from continuous values
    
    a = a + age_1 * -11.6175606616390770000000000
  a = a + age_2 * -42.9098057686870220000000000
  a = a + bmi_1 * -0.5344237822753052900000000
  a = a + bmi_2 * 2.6900552265408226000000000
  
  # Sum from boolean values
    
    a = a + c_hb * 1.4759238359186861000000000
  a = a + fh_gicancer * 0.4044501048847998200000000
  a = a + new_abdodist * 0.6630074287856559900000000
  a = a + new_abdopain * 1.4990872468711913000000000
  a = a + new_appetiteloss * 0.5068020107261922400000000
  a = a + new_rectalbleed * 2.7491673095810105000000000
  a = a + new_vte * 0.7072816884002932600000000
  a = a + new_weightloss * 1.0288860866585736000000000
  a = a + s1_bowelchange * 0.7664414123199643200000000
  a = a + s1_constipation * 0.3375158123121173600000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -7.5466948789670942000000000
  return score
  }

colorectal_cancer_female_validation(
  age,alcohol_cat4,bmi,c_hb,fh_gicancer,new_abdodist,new_abdopain,new_appetiteloss,new_rectalbleed,new_vte,new_weightloss,s1_bowelchange,s1_constipation,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!i_in_range(alcohol_cat4,0,3)) {
    ok=0
    strlcat(errorBuf,"error: alcohol_cat4 must be in range (0,3)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(c_hb)) {
    ok=0
    strlcat(errorBuf,"error: c_hb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(fh_gicancer)) {
    ok=0
    strlcat(errorBuf,"error: fh_gicancer must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdodist)) {
    ok=0
    strlcat(errorBuf,"error: new_abdodist must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_appetiteloss)) {
    ok=0
    strlcat(errorBuf,"error: new_appetiteloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_rectalbleed)) {
    ok=0
    strlcat(errorBuf,"error: new_rectalbleed must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_weightloss)) {
    ok=0
    strlcat(errorBuf,"error: new_weightloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(s1_bowelchange)) {
    ok=0
    strlcat(errorBuf,"error: s1_bowelchange must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(s1_constipation)) {
    ok=0
    strlcat(errorBuf,"error: s1_constipation must be in range (0,1)\n",errorBufSize)
  }
  return ok
}

colorectal_cancer_female(
  age,alcohol_cat4,bmi,c_hb,fh_gicancer,new_abdodist,new_abdopain,new_appetiteloss,new_rectalbleed,new_vte,new_weightloss,s1_bowelchange,s1_constipation,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = colorectal_cancer_female_validation(age,alcohol_cat4,bmi,c_hb,fh_gicancer,new_abdodist,new_abdopain,new_appetiteloss,new_rectalbleed,new_vte,new_weightloss,s1_bowelchange,s1_constipation,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return colorectal_cancer_female_raw(age,alcohol_cat4,bmi,c_hb,fh_gicancer,new_abdodist,new_abdopain,new_appetiteloss,new_rectalbleed,new_vte,new_weightloss,s1_bowelchange,s1_constipation)
}

# End of colorectal_cancer
  
  # gastro_oesophageal_cancer
  
  gastro_oesophageal_cancer_female_raw(
    age,bmi,c_hb,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_heartburn,new_indigestion,new_vte,new_weightloss,smoke_cat
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ismoke[5] = {
      0,
      0.2108835385994093400000000,
      0.4020914846651602000000000,
      0.8497119766959212500000000,
      1.1020585469724540000000000
    }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values
    
    a = a + age_1 * 5.5127932958160830000000000
  a = a + age_2 * -70.2734062916161830000000000
  a = a + bmi_1 * 2.6063377632938987000000000
  a = a + bmi_2 * -1.2389834515079798000000000
  
  # Sum from boolean values
    
    a = a + c_hb * 1.2479756970482034000000000
  a = a + new_abdopain * 0.7825304005124729100000000
  a = a + new_appetiteloss * 0.6514592236889243900000000
  a = a + new_dysphagia * 3.7751714910656862000000000
  a = a + new_gibleed * 1.4264472204617833000000000
  a = a + new_heartburn * 0.8178746069193373300000000
  a = a + new_indigestion * 1.4998439683677578000000000
  a = a + new_vte * 0.7199894658172598700000000
  a = a + new_weightloss * 1.2287925630053846000000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -8.8746031610250764000000000
  return score
  }

gastro_oesophageal_cancer_female_validation(
  age,bmi,c_hb,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_heartburn,new_indigestion,new_vte,new_weightloss,smoke_cat,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(c_hb)) {
    ok=0
    strlcat(errorBuf,"error: c_hb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_appetiteloss)) {
    ok=0
    strlcat(errorBuf,"error: new_appetiteloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_dysphagia)) {
    ok=0
    strlcat(errorBuf,"error: new_dysphagia must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_gibleed)) {
    ok=0
    strlcat(errorBuf,"error: new_gibleed must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_heartburn)) {
    ok=0
    strlcat(errorBuf,"error: new_heartburn must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_indigestion)) {
    ok=0
    strlcat(errorBuf,"error: new_indigestion must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_weightloss)) {
    ok=0
    strlcat(errorBuf,"error: new_weightloss must be in range (0,1)\n",errorBufSize)
  }
  if (!i_in_range(smoke_cat,0,4)) {
    ok=0
    strlcat(errorBuf,"error: smoke_cat must be in range (0,4)\n",errorBufSize)
  }
  return ok
}

gastro_oesophageal_cancer_female(
  age,bmi,c_hb,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_heartburn,new_indigestion,new_vte,new_weightloss,smoke_cat,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = gastro_oesophageal_cancer_female_validation(age,bmi,c_hb,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_heartburn,new_indigestion,new_vte,new_weightloss,smoke_cat,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return gastro_oesophageal_cancer_female_raw(age,bmi,c_hb,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_heartburn,new_indigestion,new_vte,new_weightloss,smoke_cat)
}

# End of gastro_oesophageal_cancer
  
  # lung_cancer
  
  lung_cancer_female_raw(
    age,b_copd,bmi,c_hb,new_appetiteloss,new_dysphagia,new_haemoptysis,new_indigestion,new_necklump,new_vte,new_weightloss,s1_cough,smoke_cat,town
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ismoke[5] = {
      0,
      1.3397416191950409000000000,
      1.9500839456663224000000000,
      2.1881694694325233000000000,
      2.4828660433307768000000000
    }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  town = town - -0.383295059204102
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values
    
    a = a + age_1 * -117.2405737502962500000000000
  a = a + age_2 * 25.1702254741268090000000000
  a = a + bmi_1 * 2.5845488133924350000000000
  a = a + bmi_2 * -0.6083523966762799400000000
  a = a + town * 0.0406920461830567460000000
  
  # Sum from boolean values
    
    a = a + b_copd * 0.7942901962671364800000000
  a = a + c_hb * 0.8627980324401628400000000
  a = a + new_appetiteloss * 0.7170232121379446200000000
  a = a + new_dysphagia * 0.6718426806077323300000000
  a = a + new_haemoptysis * 2.9286439157734474000000000
  a = a + new_indigestion * 0.3634893730114273600000000
  a = a + new_necklump * 1.2097240380091590000000000
  a = a + new_vte * 0.8907072670032341000000000
  a = a + new_weightloss * 1.1384524885073082000000000
  a = a + s1_cough * 0.6439917053275602300000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -8.6449002971789692000000000
  return score
  }

lung_cancer_female_validation(
  age,b_copd,bmi,c_hb,new_appetiteloss,new_dysphagia,new_haemoptysis,new_indigestion,new_necklump,new_vte,new_weightloss,s1_cough,smoke_cat,town,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!is_boolean(b_copd)) {
    ok=0
    strlcat(errorBuf,"error: b_copd must be in range (0,1)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(c_hb)) {
    ok=0
    strlcat(errorBuf,"error: c_hb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_appetiteloss)) {
    ok=0
    strlcat(errorBuf,"error: new_appetiteloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_dysphagia)) {
    ok=0
    strlcat(errorBuf,"error: new_dysphagia must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_haemoptysis)) {
    ok=0
    strlcat(errorBuf,"error: new_haemoptysis must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_indigestion)) {
    ok=0
    strlcat(errorBuf,"error: new_indigestion must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_necklump)) {
    ok=0
    strlcat(errorBuf,"error: new_necklump must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_weightloss)) {
    ok=0
    strlcat(errorBuf,"error: new_weightloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(s1_cough)) {
    ok=0
    strlcat(errorBuf,"error: s1_cough must be in range (0,1)\n",errorBufSize)
  }
  if (!i_in_range(smoke_cat,0,4)) {
    ok=0
    strlcat(errorBuf,"error: smoke_cat must be in range (0,4)\n",errorBufSize)
  }
  if (!d_in_range(town,-7,11)) {
    ok=0
    strlcat(errorBuf,"error: town must be in range (-7,11)\n",errorBufSize)
  }
  return ok
}

lung_cancer_female(
  age,b_copd,bmi,c_hb,new_appetiteloss,new_dysphagia,new_haemoptysis,new_indigestion,new_necklump,new_vte,new_weightloss,s1_cough,smoke_cat,town,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = lung_cancer_female_validation(age,b_copd,bmi,c_hb,new_appetiteloss,new_dysphagia,new_haemoptysis,new_indigestion,new_necklump,new_vte,new_weightloss,s1_cough,smoke_cat,town,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return lung_cancer_female_raw(age,b_copd,bmi,c_hb,new_appetiteloss,new_dysphagia,new_haemoptysis,new_indigestion,new_necklump,new_vte,new_weightloss,s1_cough,smoke_cat,town)
}

# End of lung_cancer
  
  # other_cancer
  
  other_cancer_female_raw(
    age,alcohol_cat4,b_copd,bmi,c_hb,new_abdodist,new_abdopain,new_appetiteloss,new_breastlump,new_dysphagia,new_gibleed,new_haematuria,new_indigestion,new_necklump,new_pmb,new_vte,new_weightloss,s1_constipation,smoke_cat
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ialcohol[4] = {
      0,
      0.1129292517088995400000000,
      0.1389183205617967600000000,
      0.3428114766789586200000000
    }
  Ismoke[5] = {
    0,
    0.0643839792551647580000000,
    0.1875068101660691500000000,
    0.3754052152821668000000000,
    0.5007337952210844100000000
  }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ialcohol[alcohol_cat4]
  a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values
    
    a = a + age_1 * 35.8208987302204780000000000
  a = a + age_2 * -68.3294741037719150000000000
  a = a + bmi_1 * 1.8969796480108396000000000
  a = a + bmi_2 * -3.7755945945329574000000000
  
  # Sum from boolean values
    
    a = a + b_copd * 0.2823021429107943600000000
  a = a + c_hb * 1.0476364795173587000000000
  a = a + new_abdodist * 0.9628688090459262000000000
  a = a + new_abdopain * 0.8335710066715610300000000
  a = a + new_appetiteloss * 0.8450972438476546100000000
  a = a + new_breastlump * 1.0400807427059522000000000
  a = a + new_dysphagia * 0.8905342895684595900000000
  a = a + new_gibleed * 0.3839632265134078600000000
  a = a + new_haematuria * 0.6143184647549447800000000
  a = a + new_indigestion * 0.2457016002992454300000000
  a = a + new_necklump * 2.1666504706191545000000000
  a = a + new_pmb * 0.4219383252623540900000000
  a = a + new_vte * 1.0630784861733920000000000
  a = a + new_weightloss * 1.1058752771736007000000000
  a = a + s1_constipation * 0.3780143641299491500000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -6.7864501668594306000000000
  return score
  }

other_cancer_female_validation(
  age,alcohol_cat4,b_copd,bmi,c_hb,new_abdodist,new_abdopain,new_appetiteloss,new_breastlump,new_dysphagia,new_gibleed,new_haematuria,new_indigestion,new_necklump,new_pmb,new_vte,new_weightloss,s1_constipation,smoke_cat,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!i_in_range(alcohol_cat4,0,3)) {
    ok=0
    strlcat(errorBuf,"error: alcohol_cat4 must be in range (0,3)\n",errorBufSize)
  }
  if (!is_boolean(b_copd)) {
    ok=0
    strlcat(errorBuf,"error: b_copd must be in range (0,1)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(c_hb)) {
    ok=0
    strlcat(errorBuf,"error: c_hb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdodist)) {
    ok=0
    strlcat(errorBuf,"error: new_abdodist must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_appetiteloss)) {
    ok=0
    strlcat(errorBuf,"error: new_appetiteloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_breastlump)) {
    ok=0
    strlcat(errorBuf,"error: new_breastlump must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_dysphagia)) {
    ok=0
    strlcat(errorBuf,"error: new_dysphagia must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_gibleed)) {
    ok=0
    strlcat(errorBuf,"error: new_gibleed must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_haematuria)) {
    ok=0
    strlcat(errorBuf,"error: new_haematuria must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_indigestion)) {
    ok=0
    strlcat(errorBuf,"error: new_indigestion must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_necklump)) {
    ok=0
    strlcat(errorBuf,"error: new_necklump must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_pmb)) {
    ok=0
    strlcat(errorBuf,"error: new_pmb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_weightloss)) {
    ok=0
    strlcat(errorBuf,"error: new_weightloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(s1_constipation)) {
    ok=0
    strlcat(errorBuf,"error: s1_constipation must be in range (0,1)\n",errorBufSize)
  }
  if (!i_in_range(smoke_cat,0,4)) {
    ok=0
    strlcat(errorBuf,"error: smoke_cat must be in range (0,4)\n",errorBufSize)
  }
  return ok
}

other_cancer_female(
  age,alcohol_cat4,b_copd,bmi,c_hb,new_abdodist,new_abdopain,new_appetiteloss,new_breastlump,new_dysphagia,new_gibleed,new_haematuria,new_indigestion,new_necklump,new_pmb,new_vte,new_weightloss,s1_constipation,smoke_cat,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = other_cancer_female_validation(age,alcohol_cat4,b_copd,bmi,c_hb,new_abdodist,new_abdopain,new_appetiteloss,new_breastlump,new_dysphagia,new_gibleed,new_haematuria,new_indigestion,new_necklump,new_pmb,new_vte,new_weightloss,s1_constipation,smoke_cat,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return other_cancer_female_raw(age,alcohol_cat4,b_copd,bmi,c_hb,new_abdodist,new_abdopain,new_appetiteloss,new_breastlump,new_dysphagia,new_gibleed,new_haematuria,new_indigestion,new_necklump,new_pmb,new_vte,new_weightloss,s1_constipation,smoke_cat)
}

# End of other_cancer
  
  # ovarian_cancer
  
  ovarian_cancer_female_raw(
    age,bmi,c_hb,fh_ovariancancer,new_abdodist,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_vte,new_weightloss,s1_bowelchange
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    
    # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    
    # Sum from continuous values
    
    a = a + age_1 * -61.0831814462568940000000000
  a = a + age_2 * 20.3028612701106890000000000
  a = a + bmi_1 * -2.1261135335028407000000000
  a = a + bmi_2 * 3.2168200408772472000000000
  
  # Sum from boolean values
    
    a = a + c_hb * 1.3625636791018674000000000
  a = a + fh_ovariancancer * 1.9951774809951830000000000
  a = a + new_abdodist * 2.9381020883363806000000000
  a = a + new_abdopain * 1.7307824546132513000000000
  a = a + new_appetiteloss * 1.0606947909647773000000000
  a = a + new_haematuria * 0.4958835997468107900000000
  a = a + new_indigestion * 0.3843731027493998400000000
  a = a + new_pmb * 1.5869592940878865000000000
  a = a + new_vte * 1.6839747529852673000000000
  a = a + new_weightloss * 0.4774332393821720800000000
  a = a + s1_bowelchange * 0.6849850007182314300000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -7.5609929644491318000000000
  return score
  }

ovarian_cancer_female_validation(
  age,bmi,c_hb,fh_ovariancancer,new_abdodist,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_vte,new_weightloss,s1_bowelchange,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(c_hb)) {
    ok=0
    strlcat(errorBuf,"error: c_hb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(fh_ovariancancer)) {
    ok=0
    strlcat(errorBuf,"error: fh_ovariancancer must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdodist)) {
    ok=0
    strlcat(errorBuf,"error: new_abdodist must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_appetiteloss)) {
    ok=0
    strlcat(errorBuf,"error: new_appetiteloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_haematuria)) {
    ok=0
    strlcat(errorBuf,"error: new_haematuria must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_indigestion)) {
    ok=0
    strlcat(errorBuf,"error: new_indigestion must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_pmb)) {
    ok=0
    strlcat(errorBuf,"error: new_pmb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_weightloss)) {
    ok=0
    strlcat(errorBuf,"error: new_weightloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(s1_bowelchange)) {
    ok=0
    strlcat(errorBuf,"error: s1_bowelchange must be in range (0,1)\n",errorBufSize)
  }
  return ok
}

ovarian_cancer_female(
  age,bmi,c_hb,fh_ovariancancer,new_abdodist,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_vte,new_weightloss,s1_bowelchange,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = ovarian_cancer_female_validation(age,bmi,c_hb,fh_ovariancancer,new_abdodist,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_vte,new_weightloss,s1_bowelchange,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return ovarian_cancer_female_raw(age,bmi,c_hb,fh_ovariancancer,new_abdodist,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_vte,new_weightloss,s1_bowelchange)
}

# End of ovarian_cancer
  
  # pancreatic_cancer
  
  pancreatic_cancer_female_raw(
    age,b_chronicpan,b_type2,bmi,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_indigestion,new_vte,new_weightloss,s1_bowelchange,smoke_cat
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ismoke[5] = {
      0,
      -0.0631301848152044240000000,
      0.3523695950528934500000000,
      0.7146003670327156800000000,
      0.8073207410335441200000000
    }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values
    
    a = a + age_1 * -6.8219654517231225000000000
  a = a + age_2 * -65.6404897305188650000000000
  a = a + bmi_1 * 3.9715559458995728000000000
  a = a + bmi_2 * -3.1161107999130500000000000
  
  # Sum from boolean values
    
    a = a + b_chronicpan * 1.1948138830441282000000000
  a = a + b_type2 * 0.7951745325664703000000000
  a = a + new_abdopain * 1.9230379689782926000000000
  a = a + new_appetiteloss * 1.5209568259888571000000000
  a = a + new_dysphagia * 1.0107551560302726000000000
  a = a + new_gibleed * 0.9324059153254259400000000
  a = a + new_indigestion * 1.1134012616631439000000000
  a = a + new_vte * 1.4485586969016084000000000
  a = a + new_weightloss * 1.5791912580663912000000000
  a = a + s1_bowelchange * 0.9361738611941444700000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -9.2782129678657608000000000
  return score
  }

pancreatic_cancer_female_validation(
  age,b_chronicpan,b_type2,bmi,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_indigestion,new_vte,new_weightloss,s1_bowelchange,smoke_cat,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!is_boolean(b_chronicpan)) {
    ok=0
    strlcat(errorBuf,"error: b_chronicpan must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(b_type2)) {
    ok=0
    strlcat(errorBuf,"error: b_type2 must be in range (0,1)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_appetiteloss)) {
    ok=0
    strlcat(errorBuf,"error: new_appetiteloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_dysphagia)) {
    ok=0
    strlcat(errorBuf,"error: new_dysphagia must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_gibleed)) {
    ok=0
    strlcat(errorBuf,"error: new_gibleed must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_indigestion)) {
    ok=0
    strlcat(errorBuf,"error: new_indigestion must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_weightloss)) {
    ok=0
    strlcat(errorBuf,"error: new_weightloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(s1_bowelchange)) {
    ok=0
    strlcat(errorBuf,"error: s1_bowelchange must be in range (0,1)\n",errorBufSize)
  }
  if (!i_in_range(smoke_cat,0,4)) {
    ok=0
    strlcat(errorBuf,"error: smoke_cat must be in range (0,4)\n",errorBufSize)
  }
  return ok
}

pancreatic_cancer_female(
  age,b_chronicpan,b_type2,bmi,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_indigestion,new_vte,new_weightloss,s1_bowelchange,smoke_cat,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = pancreatic_cancer_female_validation(age,b_chronicpan,b_type2,bmi,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_indigestion,new_vte,new_weightloss,s1_bowelchange,smoke_cat,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return pancreatic_cancer_female_raw(age,b_chronicpan,b_type2,bmi,new_abdopain,new_appetiteloss,new_dysphagia,new_gibleed,new_indigestion,new_vte,new_weightloss,s1_bowelchange,smoke_cat)
}

# End of pancreatic_cancer
  
  # renal_tract_cancer
  
  renal_tract_cancer_female_raw(
    age,bmi,c_hb,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_weightloss,smoke_cat
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    Ismoke[5] = {
      0,
      0.2752175727739372700000000,
      0.5498656631475861100000000,
      0.6536242182136680100000000,
      0.9053763661785879700000000
    }
  
  # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    a = a + Ismoke[smoke_cat]
  
  # Sum from continuous values
    
    a = a + age_1 * -0.0323226569626617470000000
  a = a + age_2 * -56.3551410786635780000000000
  a = a + bmi_1 * 1.2103910535779330000000000
  a = a + bmi_2 * -4.7221299079939785000000000
  
  # Sum from boolean values
    
    a = a + c_hb * 1.2666531852544143000000000
  a = a + new_abdopain * 0.6155954984707594500000000
  a = a + new_appetiteloss * 0.6842184594676019600000000
  a = a + new_haematuria * 4.1791444537241542000000000
  a = a + new_indigestion * 0.5694329224821874600000000
  a = a + new_pmb * 1.2541097882792864000000000
  a = a + new_weightloss * 0.7711610560290518300000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -8.9440775553776248000000000
  return score
  }

renal_tract_cancer_female_validation(
  age,bmi,c_hb,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_weightloss,smoke_cat,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(c_hb)) {
    ok=0
    strlcat(errorBuf,"error: c_hb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_appetiteloss)) {
    ok=0
    strlcat(errorBuf,"error: new_appetiteloss must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_haematuria)) {
    ok=0
    strlcat(errorBuf,"error: new_haematuria must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_indigestion)) {
    ok=0
    strlcat(errorBuf,"error: new_indigestion must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_pmb)) {
    ok=0
    strlcat(errorBuf,"error: new_pmb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_weightloss)) {
    ok=0
    strlcat(errorBuf,"error: new_weightloss must be in range (0,1)\n",errorBufSize)
  }
  if (!i_in_range(smoke_cat,0,4)) {
    ok=0
    strlcat(errorBuf,"error: smoke_cat must be in range (0,4)\n",errorBufSize)
  }
  return ok
}

renal_tract_cancer_female(
  age,bmi,c_hb,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_weightloss,smoke_cat,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = renal_tract_cancer_female_validation(age,bmi,c_hb,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_weightloss,smoke_cat,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return renal_tract_cancer_female_raw(age,bmi,c_hb,new_abdopain,new_appetiteloss,new_haematuria,new_indigestion,new_pmb,new_weightloss,smoke_cat)
}

# End of renal_tract_cancer
  
  # uterine_cancer
  
  uterine_cancer_female_raw(
    age,b_endometrial,b_type2,bmi,new_abdopain,new_haematuria,new_imb,new_pmb,new_vte
  )
{
  survivor[0] = {
    
  }
  
  # The conditional arrays
    
    
    # Applying the fractional polynomial transforms
    # (which includes scaling)                     
    
    dage = age
  dage=dage/10
  age_1 = pow(dage,-2)
  age_2 = pow(dage,-2)*log(dage)
  dbmi = bmi
  dbmi=dbmi/10
  bmi_1 = pow(dbmi,-2)
  bmi_2 = pow(dbmi,-2)*log(dbmi)
  
  # Centring the continuous variables
    
    age_1 = age_1 - 0.039541322737932
  age_2 = age_2 - 0.063867323100567
  bmi_1 = bmi_1 - 0.151021569967270
  bmi_2 = bmi_2 - 0.142740502953529
  
  # Start of Sum
    a=0
  
  # The conditional sums
    
    
    # Sum from continuous values
    
    a = a + age_1 * 2.7778124257317254000000000
  a = a + age_2 * -59.5333514566633330000000000
  a = a + bmi_1 * 3.7623897936404322000000000
  a = a + bmi_2 * -26.8045450074654320000000000
  
  # Sum from boolean values
    
    a = a + b_endometrial * 0.8742311851235286000000000
  a = a + b_type2 * 0.2655181024063555900000000
  a = a + new_abdopain * 0.6891953836735580400000000
  a = a + new_haematuria * 1.6798617740998527000000000
  a = a + new_imb * 1.7853122923827887000000000
  a = a + new_pmb * 4.4770199876067398000000000
  a = a + new_vte * 1.0362058616761669000000000
  
  # Sum from interaction terms
    
    
    # Calculate the score itself
    score = a + -8.9931390822564037000000000
  return score
  }

uterine_cancer_female_validation(
  age,b_endometrial,b_type2,bmi,new_abdopain,new_haematuria,new_imb,new_pmb,new_vte,char *errorBuf,errorBufSize
)
{
  ok=1
  *errorBuf=0
  if (!i_in_range(age,25,89)) {
    ok=0
    strlcat(errorBuf,"error: age must be in range (25,89)\n",errorBufSize)
  }
  if (!is_boolean(b_endometrial)) {
    ok=0
    strlcat(errorBuf,"error: b_endometrial must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(b_type2)) {
    ok=0
    strlcat(errorBuf,"error: b_type2 must be in range (0,1)\n",errorBufSize)
  }
  if (!d_in_range(bmi,20,40)) {
    ok=0
    strlcat(errorBuf,"error: bmi must be in range (20,40)\n",errorBufSize)
  }
  if (!is_boolean(new_abdopain)) {
    ok=0
    strlcat(errorBuf,"error: new_abdopain must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_haematuria)) {
    ok=0
    strlcat(errorBuf,"error: new_haematuria must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_imb)) {
    ok=0
    strlcat(errorBuf,"error: new_imb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_pmb)) {
    ok=0
    strlcat(errorBuf,"error: new_pmb must be in range (0,1)\n",errorBufSize)
  }
  if (!is_boolean(new_vte)) {
    ok=0
    strlcat(errorBuf,"error: new_vte must be in range (0,1)\n",errorBufSize)
  }
  return ok
}

uterine_cancer_female(
  age,b_endometrial,b_type2,bmi,new_abdopain,new_haematuria,new_imb,new_pmb,new_vte,*error,char *errorBuf,errorBufSize
)
{
  *error = 0	ok = uterine_cancer_female_validation(age,b_endometrial,b_type2,bmi,new_abdopain,new_haematuria,new_imb,new_pmb,new_vte,errorBuf,errorBufSize)
  if(!ok) { 
    *error = 1
    return 0.0
  }
  return uterine_cancer_female_raw(age,b_endometrial,b_type2,bmi,new_abdopain,new_haematuria,new_imb,new_pmb,new_vte)
}

# End of uterine_cancer
  
  