*Emily Patterson's do file for Associations between ACEs and gambling,
*Updated by Annie Herbert
*26.02.25

*Recode variables and impute missing data


********************************************************************************
*Prep
*Log file
log using "C:\Users\is19262\OneDrive - University of Bristol\MyFiles-Migrated\Teaching\MScs\MSc PH\2023 - Emily Patterson - ACEs & gambling\results\Data prep log file - paper_$S_DATE.log", replace

*cd "W:\working\data\cohort\Emily Patterson\"
cd "W:\data\cohort\Emily Patterson\"
use diss_cohort2.dta, clear


********************************************************************************
** Data cleaning/derivation
** recode missing variables to "." for ACE exposures and PGSI variables

*EXPOSURES AND OUTCOMES
forvalues i=0/9{
	cap replace clon10`i'=. if clon10`i'==-10 |  clon10`i'==-1
	cap replace clon11`i'=. if clon11`i'==-10 |  clon11`i'==-1
	cap replace clon12`i'=. if clon12`i'==-10 |  clon12`i'==-1
	}
forvalues i=0/2{
	cap replace CCU115`i'=. if CCU115`i'==-10 |  CCU115`i'==-1
	}
forvalues i=0/8{
	cap replace FJGA20`i'=. if FJGA20`i'==-10 |  FJGA20`i'==-1
	cap replace YPD903`i'=. if YPD903`i'==-10 |  YPD903`i'==-1
	}
** reorder PGSI variables (0 never 1 sometimes 2 most of the time 3 almost always) was previously (4 never 3 sometimes 2 most of the time 1 almost always. Reorder inline with PGSI and will be used to derive total score - for ages 17 years
forvalues i=0/8{
	cap recode FJGA20`i' (4=0) (3=1) (2=2)(1=3), gen(FJGA20`i'_reorder)
	cap tab FJGA20`i' FJGA20`i'_reorder
	cap label variable FJGA20`i'_reorder "FJGA20`i' values reordered in line with PGSI"
	cap label define FJGA20`i'_reorder_lb 0 "Never" 1 "Sometimes" 2 "Most of the Time" 3"Almost Always"
	cap label values FJGA20`i'_reorder FJGA20`i'_reorder_lb
	}

** all PGSI individual questions reordered for age 17
** generating new variable - PGSI Score at 17 by totalling the 9 PGSI question scores
gen FJGA_PGSI = (FJGA200_reorder+FJGA201_reorder+FJGA202_reorder+FJGA203_reorder+FJGA204_reorder+FJGA205_reorder+FJGA206_reorder+FJGA207_reorder+FJGA208_reorder)

**AGE 17 PGSI
label variable FJGA_PGSI "Total PGSI Score at age 17"
tab FJGA_PGSI
 **create new variable for PGSI Score at 17 split into categories defined as "0= Non-problem gambling, 1-2= Low level of problems, 3-7 = Moderate level of problems, 8+ = Problem gambling"
egen PGSI_Score_17 =cut(FJGA_PGSI), at (0,1,3,8,36)
recode PGSI_Score_17 (0=0) (1=1) (3=2) (8=3)
label variable PGSI_Score_17 "PGSI Score age 17 4-cat"
label define PGSI_Score_17_lb 0 "Non-problem gambling" 1 "Low level of problems" 2 "Moderate level of problems" 3 "Problem gambling"
label values PGSI_Score_17 PGSI_Score_17_lb

**AGE 20 PGSI
*PGSI score at age 20 already available as CCU1151
egen PGSI_Score_20 =cut(CCU1151), at (0,1,2,3,36)
label variable PGSI_Score_20 "PGSI Score age 20 4-cat"
label values PGSI_Score_20 PGSI_Score_17_lb

**AGE 24 PGSI"
** reorder PGSI Variables for age 24 not needed, already coded as (0 Never 1 Sometimes 2 Most of the time 3 Almost Always)
 ** generating new variable - PGSI Score at 24, by totaling the 9 PGSI question scores
gen YPD9083 = (YPD9030+YPD9031+YPD9032+YPD9033+YPD9034+YPD9035+YPD9036+YPD9037+YPD9038)
egen PGSI_Score_24 =cut(YPD9083), at (0,1,3,8,36)
recode PGSI_Score_24 (0=0) (1=1) (3=2) (8=3)
label variable PGSI_Score_24 "PGSI Score age 24 4-cat"
label values PGSI_Score_24 PGSI_Score_17_lb

**COVARIATES
*Updated sex and ethnicity, as coded 1,2 not 0,1.
gen female = kz021-1 
replace kz021  = . if kz021<0
gen nonwhite = c804-1
replace nonwhite = . if nonwhite<0
*Recoded household education from mother and father 5 cat to single binary
recode c645 (-9999 -1 = .) (1 2 3 = 0) (4 5 = 1), gen(mum_alevelplus)
recode pb325 (-9999 -1 = .) (1 2 3 = 0) (4 5 = 1), gen(dad_alevelplus)
gen hse_alevelplus = 0 if mum_alevelplus!=. & dad_alevelplus!=.
replace hse_alevelplus = 1 if mum_alevelplus==1 | dad_alevelplus==1
*Recode housing status
gen homeown = 0 if a006!=. & a006>=0
replace homeown = 1 if a006>=0 & a006<=1
*Parity
recode b032 (-9999/-1 = .) (0 = 0) (1 = 1) (2/22 = 2), gen(parity)
*Smoking in pregnancy
recode b671 (-9999/-1 = .) (0=0) (1/30 = 1), gen(smokpreg)
*household social class
recode c755 (-9999/-1 = .) (4/6=0) (1/3=1) (65=.), gen(mum_socclass)

*Variable to filter on includsion criteria - one non-missing ACE and one non-missing PGSI
gen cohort=1 if kz021!= . & (clon100!=. | clon101 !=. | clon102!=. | clon103!=. | clon104!=. | clon105!=. | clon106!=. | clon107 !=. | clon108!=. | clon109!= .) & (FJGA_PGSI!=. | CCU1151!=. | YPD9083!=.)

*And one to filter specifically by age 17, 20, and 24:
gen cohort_17=1 if kz021!= . & (clon100!=. | clon101 !=. | clon102!=. | clon103!=. | clon104!=. | clon105!=. | clon106!=. | clon107 !=. | clon108!=. | clon109!= .) & (FJGA_PGSI!=.)

gen cohort_20=1 if kz021!= . & (clon100!=. | clon101 !=. | clon102!=. | clon103!=. | clon104!=. | clon105!=. | clon106!=. | clon107 !=. | clon108!=. | clon109!= .) & (CCU1151!=.)

gen cohort_24=1 if kz021!= . & (clon100!=. | clon101 !=. | clon102!=. | clon103!=. | clon104!=. | clon105!=. | clon106!=. | clon107 !=. | clon108!=. | clon109!= .) & (YPD9083!=.)

save preimputation2, replace


********************************************************************************
**Imputation
use preimputation2, replace
keep if cohort==1
ice PGSI_Score_17 PGSI_Score_20 PGSI_Score_24 ///
clon100 clon101 clon102 clon103 clon104 clon105 clon106 clon107 clon108 clon109 ///
clon111 clon112 clon113 clon114 clon115 clon116 clon117 clon118 clon119 ///
a006 b032 b671 mum_alevelplus c755 nonwhite mz028b pb325 b370 pb260 kz030, ///
m(20) ///
cmd(PGSI_Score_17 PGSI_Score_20 PGSI_Score_24 a006 c755 b671 pb325: ologit, ///
clon100 clon101 clon102 clon103 clon104 clon105 clon106 clon107 clon108 clon109 ///
clon111 clon112 clon113 clon114 clon115 clon116 clon117 clon118 clon119 ///
mum_alevelplus nonwhite: logit, ///
b032 mz028b b370 pb260 kz030: regress) ///
by(female) seed(5364) saving(cohort_imputed2.dta, replace)

*Derive binary PGSI variables
use cohort_imputed2.dta, replace

tab PGSI_Score_17
** generating 2 level PGSI for at-risk+
egen PGSI_Score_17_2 =cut(FJGA_PGSI), at (0,1,36)
label variable PGSI_Score_17_2 "PGSI Score age 17 binary at risk (1+)"
label define PGSI_Score_17_2_lb 0 "Non-at-risk gambling" 1 "At Risk"
label values PGSI_Score_17_2 PGSI_Score_17_2_lb
** generating 2 level PGSI for mod-risk+
egen PGSI_Score_17_4 =cut(FJGA_PGSI), at (0,5,36)
recode PGSI_Score_17_4 (0=0) (5=1)
label variable PGSI_Score_17_4 "PGSI Score age 17 binary mod risk (5+)"
label define PGSI_Score_17_4_lb 0 "Non-problem gambling" 1 "Mod risk"
label values PGSI_Score_17_4 PGSI_Score_17_4_lb

egen PGSI_Score_20_2 =cut(CCU1151), at (0,1,3)
label variable PGSI_Score_20_2 "PGSI Score age 20 binary at risk (1+)"
label values PGSI_Score_20_2 PGSI_Score_17_2_lb
egen PGSI_Score_20_4 =cut(CCU1151), at (0,2,4)
replace PGSI_Score_20_4=1 if PGSI_Score_20_4==2
label variable PGSI_Score_20_4 "PGSI Score age 20 binary mod risk (5+)"
label values PGSI_Score_20_4 PGSI_Score_17_4_lb

egen PGSI_Score_24_2 =cut(YPD9083), at (0,1,36)
label variable PGSI_Score_24_2 "PGSI Score age 24 binary at risk (1+)"
label values PGSI_Score_24_2 PGSI_Score_17_2_lb
tab PGSI_Score_24_2
egen PGSI_Score_24_4 =cut(YPD9083), at (0,5,36)
recode PGSI_Score_24_4 (0=0) (5=1)
label variable PGSI_Score_24_4 "PGSI Score age 24 binary mod risk (5+)"
label values PGSI_Score_24_4 PGSI_Score_17_4_lb

save cohort_imputed2.dta, replace

*end