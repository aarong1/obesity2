# Colorectal Cancer

'Diabetes type 2'
'Alcohol'
'Physical Activity'
'BMI'
'Smoking'
'Fruit'
'Veg'
'Red Meat'

'Processed Meat'
'NSAIDS'
'Postmenopausal Hormone Therapy'
'Family History'
'Inflammatory Bowel Disease'

# Meta-analyses of Colorectal Cancer Risk Factors
# https://pmc.ncbi.nlm.nih.gov/articles/PMC4161278/#T3

# BMI
Female RR = exp (0.017*BMI)
Male exp(0.032*(BMI-22))

# PA 
RR = exp (−0.029*PA)
tribble(
~RR, ~METs, ~units,
0.978, 600  ,'METs',
0.956, 1200 ,'METs',
0.933, 1800 ,'METs',
0.883, 2400 ,'METs',
0.833, 3000 ,'METs',
0.831, 3600 ,'METs',
0.829, 4200 ,'METs')

Current Smokers
# 10 pack years
1.11 
10 years since quitting
71.91%

Alcohol
exp(0.011 × drinks/wk)

Veg
exp(−0.030 * serv/d)

Diabetes
RR 1.527
GBD
         

PARF = 1 − (n/sum_indiduals(prod ( RRi1 ∗ RRi2 ∗ RRik))
risk factors - k
individuals - i

