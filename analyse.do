*Emily Patterson's do file for Associations between ACEs and gambling,
*Updated by Annie Herbert
*26.02.25

*Carry out analyses to produce estimates for tables and figures

********************************************************************************
*Prep
*Log file
log using "C:\Users\is19262\OneDrive - University of Bristol\MyFiles-Migrated\Teaching\MScs\MSc PH\2023 - Emily Patterson - ACEs & gambling\results\Analysis log file - paper_$S_DATE.log", replace

*cd "W:\working\data\cohort\Emily Patterson\"
cd "W:\data\cohort\Emily Patterson\"
use diss_cohort2.dta, clear

********************************************************************************
*Tables 1 and 2: cohort characteristics and numbers of ACEs
local age "17 20 24"
local vars_cont "mz028b b032 b370 pb260"
local vars_cat "nonwhite female a006 c755 c645 pb325 PGSI_Score_17 PGSI_Score_20 PGSI_Score_24 clon100 clon101 clon102 clon103 clon104 clon105 clon106 clon107 clon108 clon109 clon123"

foreach a of local age{
	putexcel set "C:\Users\is19262\OneDrive - University of Bristol\MyFiles-Migrated\Teaching\MScs\MSc PH\2023 - Emily Patterson - ACEs & gambling\results\tables", sheet("characs_`a'") modify
	putexcel A1="variable" B1="label" C1="n" D1="p" E1="extra"
	local x=1
	use cohort_imputed2.dta, replace

	keep if cohort_`a'==1
	
	summ _mj
	local nimp=r(max)
	*Continuous vars (only one value missing in each anyway...)
	foreach v of local vars_cont{
		local med_tot=0
		local lq_tot=0
		local uq_tot=0
		forvalues j=1/`nimp'{
			summ `v' if _mj==`j' & `v'>=0 & `v'!=., det
			local median = `r(p50)'
			local lq = `r(p25)'
			local uq = `r(p75)'
			local med_tot = `med_tot'+`median'
			local lq_tot = `lq_tot'+`lq'
			local uq_tot = `uq_tot'+`uq'
			}
		local x=`x'+1
		putexcel A`x'="`v'" B`x'="." C`x'=(`med_tot'/`nimp') D`x'=(`lq_tot'/`nimp') E`x'=(`uq_tot'/`nimp')
		}
	*Categorical
	foreach v of local vars_cat{ 
		tab `v' if `v'>=0 & `v'!=. & _mj==1, m
		local total=r(N)
		levelsof `v', local(levels)
		foreach i of local levels{
			local num_tot=0
			forvalues j=1/`nimp'{
				tab `v' if `v'==`i' & `v'>=0 & `v'!=. & _mj==`j', matcell(matrix)
				local num_tot = `num_tot'+matrix[1,1]
				}
			local x=`x'+1
			putexcel A`x'="`v'" B`x'=`i' C`x'=`num_tot'/`nimp' D`x'=(`num_tot'/(`nimp'*`total')*100) E`x'=`total'
			}
		}
	}

	
********************************************************************************

*Model estimates for Fig 1+Table S2 (adj estimates, at-risk+), Fig 2+Table S4 (adj estimates, mod-risk+), Table 3 (PAFs), Table S1 (crude estimates, at-risk+), Table S3 (crude estimates, mod-risk+)

ssc install mim

putexcel set "C:\Users\is19262\OneDrive - University of Bristol\MyFiles-Migrated\Teaching\MScs\MSc PH\2023 - Emily Patterson - ACEs & gambling\results\tables", sheet("mi_reg") modify
putexcel A1="Exposure" B1="Outcome" C1="OR" D1="LCI" E1="UCI" F1="P Value" G1="Model" H1="N" I1="PAF" J1="LPAF" K1="UPAF" L1="Prop exp | outcome"

local aces_bin "clon100 clon101 clon102 clon103 clon104 clon105 clon106 clon107 clon108 clon109"
*local aces_bin "clon100 clon101 clon102 clon103 clon104 clon105 clon106"

local aces_cat "clon123"
*local outcome "17_2 20_2 24_2 17_4 20_4 24_4"
local outcome "20_4"

local x=71
use cohort_imputed2.dta, replace

foreach a of local outcome{
	*Individual binary aces
	foreach c of local aces_bin{
		*Unadjusted
		xi: mim, cat(fit): logit PGSI_Score_`a' `c'
		mim, storebv
		local beta = _b[`c']
		local or = exp(`beta')
		local se = _se[`c']
		local lci = exp(`beta'-1.96*`se')
		local uci = exp(`beta'+1.96*`se')
		test `c'		
		local pvalue = `r(p)'
		local n = `e(N)'
		local x=`x'+1
putexcel A`x'="`c'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'="unadjusted" H`x'=`n' I`x'="" J`x'="" K`x'="" L`x'=""

		xi: mim, cat(fit): logit PGSI_Score_`a' `c' female mum_alevelplus nonwhite mz028b b370 pb260 kz030
		mim, storebv
		local beta = _b[`c']
		local or = exp(`beta')
		local se = _se[`c']
		local lci = exp(`beta'-1.96*`se')
		local uci = exp(`beta'+1.96*`se')
		test `c'		
		local pvalue = `r(p)'
		local n = `e(N)'
		*Now calculating proportion of exposure among cases
		bysort _mj PGSI_Score_`a': egen denom = count(aln) if _mj != 0
		bysort _mj PGSI_Score_`a': egen num2 = count(aln) if _mj != 0 & `c'==1
		bysort _mj PGSI_Score_`a': egen num = min(num2)
		gen prop = num/denom if _mj != 0
		bysort _mj PGSI_Score_`a': egen seq = seq() if _mj != 0 
		summ prop if seq == 1 & PGSI_Score_`a'==1
		local pc = r(mean)
		drop denom num num2 prop seq
		
		*For calculating PAFs
		local or=2.718^(`beta')
		local lci=2.718^(`beta'-1.96*`se')
		local uci=2.718^(`beta'+1.96*`se')
		local paf=`pc'*(1-(1/`or'))*100
		local lpaf=`pc'*(1-(1/`lci'))*100
		local upaf=`pc'*(1-(1/`uci'))*100
						
		local x=`x'+1
		putexcel A`x'="`c'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'="adjusted" H`x'=`n' I`x'=`paf' J`x'=`lpaf' K`x'=`upaf' L`x'=`pc'*100
		}

		*4-level ACE score category as exposures
		foreach c of local aces_cat{
			*Unadjusted
			xi: mim, cat(fit): logit PGSI_Score_`a' i.`c'
			mim, storebv
			forvalues i=2/4{
				local beta = _b[_I`c'_`i']
				local or = exp(`beta')
				local se = _se[_I`c'_`i']
				local lci = exp(`beta'-1.96*`se')
				local uci = exp(`beta'+1.96*`se')
				test _I`c'_`i'		
				local pvalue = `r(p)'
				local n = `e(N)'

				local x=`x'+1
				putexcel A`x'="`c'_`i'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'=				"unadjusted" H`x'=`n' I`x'="" J`x'="" K`x'="" L`x'=""
				}
			
			*Adjusted
			xi: mim, cat(fit): logit PGSI_Score_`a' i.`c' female mum_alevelplus nonwhite mz028b b370 pb260 kz030
			mim, storebv
			forvalues i=2/4{
				local beta = _b[_I`c'_`i']
				local or = exp(`beta')
				local se = _se[_I`c'_`i']
				local lci = exp(`beta'-1.96*`se')
				local uci = exp(`beta'+1.96*`se')
				test _I`c'_`i'		
				local pvalue = `r(p)'
				local n = `e(N)'
				
				*Now calculating proportion of exposure among cases
				bysort _mj PGSI_Score_`a': egen denom = count(aln) if _mj != 0
				bysort _mj PGSI_Score_`a': egen num2 = count(aln) if _mj != 0 & _I`c'_`i'==1
				bysort _mj PGSI_Score_`a': egen num = min(num2)
				gen prop = num/denom if _mj != 0
				bysort _mj PGSI_Score_`a': egen seq = seq() if _mj != 0 
				summ prop if seq == 1 & PGSI_Score_`a'==1
				local pc = r(mean)
				drop denom num num2 prop seq
				
				*For calculating PAFs
				local or=2.718^(`beta')
				local lci=2.718^(`beta'-1.96*`se')
				local uci=2.718^(`beta'+1.96*`se')
				local paf=`pc'*(1-(1/`or'))*100
				local lpaf=`pc'*(1-(1/`lci'))*100
				local upaf=`pc'*(1-(1/`uci'))*100
								
				local x=`x'+1
				putexcel A`x'="`c'_`i'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'="adjusted" H`x'=`n' I`x'=`paf' J`x'=`lpaf' K`x'=`upaf' L`x'=`pc'*100		
			}
		}
	}

	
********************************************************************************
*Table S5: Complete case analyses
putexcel set "tables", sheet("cca_reg") modify
putexcel A1="Exposure" B1="Outcome" C1="OR" D1="LCI" E1="UCI" F1="P Value" G1="Sex" H1="Model" I1="N" J1="PAF" K1="LPAF" L1="UPAF" M1="Prop exp | outcome"

local aces_bin "clon100 clon101 clon102 clon103 clon104 clon105 clon106 clon107 clon108 clon109"
local aces_cat "clon123"
local outcome "17_2 20_2 24_2 17_4 20_4 24_4"

local x=1
use preimputation.dta, replace
foreach a of local outcome{
	foreach c of local aces_bin{
		logit PGSI_Score_`a' `c'
		local beta = _b[`c']
		local or = exp(`beta')
		local se = _se[`c']
		local lci = exp(`beta'-1.96*`se')
		local uci = exp(`beta'+1.96*`se')
		test `c'		
		local pvalue = `r(p)'
		local n = `e(N)'

		local x=`x'+1

		putexcel A`x'="`c'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'="unadjusted" H`x'=`n' I`x'="" J`x'="" K`x'="" L`x'=""

		logit PGSI_Score_`a' `c' female mum_alevelplus nonwhite mz028b b370 pb260 kz030
		local beta = _b[`c']
		local or = exp(`beta')
		local se = _se[`c']
		local lci = exp(`beta'-1.96*`se')
		local uci = exp(`beta'+1.96*`se')
		test `c'		
		local pvalue = `r(p)'
		local n = `e(N)'
		
		*Now calculating proportion of exposure among cases
		bysort PGSI_Score_`a': egen denom = count(aln)
		bysort PGSI_Score_`a': egen num2 = count(aln) if `c'==1
		bysort PGSI_Score_`a': egen num = min(num2)
		gen prop = num/denom
		bysort PGSI_Score_`a': egen seq = seq()
		summ prop if seq == 1 & PGSI_Score_`a'==1
		local pc = r(mean)
		drop denom num num2 prop seq
		
		*For calculating PAFs
		local or=2.718^(`beta')
		local lci=2.718^(`beta'-1.96*`se')
		local uci=2.718^(`beta'+1.96*`se')
		local paf=`pc'*(1-(1/`or'))*100
		local lpaf=`pc'*(1-(1/`lci'))*100
		local upaf=`pc'*(1-(1/`uci'))*100
						
		local x=`x'+1
		putexcel A`x'="`c'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'="adjusted" H`x'=`n' I`x'=`paf' J`x'=`lpaf' K`x'=`upaf' L`x'=`pc'*100
		}
		
	*4-level ACE score category as exposures
	foreach c of local aces_cat{
		*Unadjusted
		xi: logit PGSI_Score_`a' i.`c'
		forvalues i=2/4{
			local beta = _b[_I`c'_`i']
			local or = exp(`beta')
			local se = _se[_I`c'_`i']
			local lci = exp(`beta'-1.96*`se')
			local uci = exp(`beta'+1.96*`se')
			test _I`c'_`i'		
			local pvalue = `r(p)'
			local n = `e(N)'

			local x=`x'+1
			putexcel A`x'="`c'_`i'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'=				"unadjusted" H`x'=`n' I`x'="" J`x'="" K`x'="" L`x'=""
			}
		
		*Adjusted
		xi: logit PGSI_Score_`a' i.`c' female mum_alevelplus nonwhite mz028b b370 pb260 kz030
		forvalues i=2/4{
			local beta = _b[_I`c'_`i']
			local or = exp(`beta')
			local se = _se[_I`c'_`i']
			local lci = exp(`beta'-1.96*`se')
			local uci = exp(`beta'+1.96*`se')
			test _I`c'_`i'		
			local pvalue = `r(p)'
			local n = `e(N)'
			
			*Now calculating proportion of exposure among cases
			bysort _mj PGSI_Score_`a': egen denom = count(aln) if _mj != 0
			bysort _mj PGSI_Score_`a': egen num2 = count(aln) if _mj != 0 & _I`c'_`i'==1
			bysort _mj PGSI_Score_`a': egen num = min(num2)
			gen prop = num/denom if _mj != 0
			bysort _mj PGSI_Score_`a': egen seq = seq() if _mj != 0 
			summ prop if seq == 1 & PGSI_Score_`a'==1
			local pc = r(mean)
			drop denom num num2 prop seq
			
			*For calculating PAFs
			local or=2.718^(`beta')
			local lci=2.718^(`beta'-1.96*`se')
			local uci=2.718^(`beta'+1.96*`se')
			local paf=`pc'*(1-(1/`or'))*100
			local lpaf=`pc'*(1-(1/`lci'))*100
			local upaf=`pc'*(1-(1/`uci'))*100
							
			local x=`x'+1
			putexcel A`x'="`c'_`i'" B`x'="PGSI_Score_`a'" C`x'=`or' D`x'=`lci' E`x'=`uci' F`x'=`pvalue' G`x'="adjusted" H`x'=`n' I`x'=`paf' J`x'=`lpaf' K`x'=`upaf' L`x'=`pc'*100				
			}
		}
	}
	
*end