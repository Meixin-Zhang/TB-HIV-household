// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Purpose:		Generate 1000 draws of HIV prevalence in prevalent TB, and HIV prevalence in general population
// Author:		Meixin Zhang
// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Load settings

	// Clear memory and establish settings
	clear all
	
	// Define focal drives
	if c(os) == "Unix" {
		global prefix "/home/j"
		local function_prefix "/ihme/cc_resources"
		set odbcmgr unixodbc
	}
	else if c(os) == "Windows" {
		global prefix "J:"
		local function_prefix "K:"
	}

**********************************************************************************************************************
** STEP 1: Pull RR of HIV in prevalent TB vs. HIV in general pop - ZAF
**********************************************************************************************************************
// Pull data with 95% CI
import delimited "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ZAF.csv", clear

// 1000 draws:
	gen sd = (rr_upper-rr_lower)/(2*1.96)
	di in red "get 1,000 RR draws"
		forvalues k = 0/999 {	
			gen draw_`k' = rnormal(rr_mean, sd)	
		}
		forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}
				
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ZAF_draws.dta", replace

**********************************************************************************************************************
** STEP 2: Pull RR of HIV in prevalent TB vs. HIV in general pop - ETH
**********************************************************************************************************************
// hiv in prevalent TB vs. hiv in notified TB (rr)
// Pull data with 95% CI
import delimited "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH.csv", clear

// 1000 draws:
	gen sd = (rr_upper-rr_lower)/(2*1.96)
	di in red "get 1,000 RR draws"
		forvalues k = 0/999 {	
			gen draw_`k' = rnormal(rr_mean, sd)	
		}
		forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}

	save "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH_draws1.dta", replace
	
// hiv in notified TB vs. national hiv prevalence (rr)
// Pull data with 95% CI
import delimited "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH2.csv", clear

// 1000 draws:
	gen sd = (rr_upper-rr_lower)/(2*1.96)
	di in red "get 1,000 RR draws"
		forvalues k = 0/999 {	
			gen draw_`k' = rnormal(rr_mean, sd)	
		}
		forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}

	save "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH_draws2.dta", replace
		
**********************************************************************************************************************
** STEP 3: Pull HIV Prevalence in prevalent TB
**********************************************************************************************************************
// Pull data with 95% CI
import delimited "$prefix/temp/TB/mzhang25/TB_HHC/data/HIV_TB_prev_percent_ci.csv", clear

// 1000 draws:
// rename mean val 
	gen sd = (upper-lower)/(2*1.96)
	di in red "get 1,000 RR draws"
		forvalues k = 0/999 {	
			gen draw_`k' = rnormal(val, sd)	
		}
		forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}
		
	keep location_id sex_id age_group_id measurement data_source draw_* 		
	
	// ZAF 
	preserve
		keep if location_id == 196
		forvalues k = 0/999 {	
			rename draw_`k' hiv_prev_`k'
		}
		merge m:1 location_id using "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ZAF_draws.dta", keep(3)nogen
		forvalues k = 0/999 {	
			replace draw_`k' = hiv_prev_`k' * draw_`k'
		}
		keep location_id sex_id age_group_id measurement data_source draw_*
		tempfile tmp_ZAF
		save `tmp_ZAF', replace
	restore
	
	// ETH
	preserve
		keep if location_id == 179
		forvalues k = 0/999 {	
			rename draw_`k' hiv_prev_`k'
		}
		merge m:1 location_id using "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH_draws2.dta", keep(3)nogen
		forvalues k = 0/999 {	
			replace hiv_prev_`k' = hiv_prev_`k' * draw_`k'
			drop draw_`k'
		}
		keep location_id sex_id age_group_id measurement data_source hiv_prev_*
		
		merge m:1 location_id using "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH_draws1.dta", keep(3)nogen
		forvalues k = 0/999 {	
			replace draw_`k' = hiv_prev_`k' * draw_`k'
		}
		keep location_id sex_id age_group_id measurement data_source draw_*
		tempfile tmp_ETH
		save `tmp_ETH', replace
	restore

	// Append
	drop if location_id == 196 | location_id == 179
	append using "`tmp_ZAF'"
	append using "`tmp_ETH'"
	replace measurement = "% HIV in prevalent TB" if (location_id == 196 | location_id == 179)
	
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/HIV_TB_prev_percent_draws.dta", replace
**********************************************************************************************************************
** STEP 4: Pull HIV Prevalence in general population
**********************************************************************************************************************
// Pull data with 95% CI
import delimited "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_national_ci.csv", clear

// 1000 draws:
	gen sd = (upper-lower)/(2*1.96)
	di in red "get 1,000 RR draws"
		forvalues k = 0/999 {	
			gen draw_`k' = rnormal(val, sd)	
		}
		forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}
		
	keep location_id sex_id age_group_id measurement draw_* 		
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/hiv_prev_national_draws.dta", replace
	
**********************************************************************************************************************
** STEP 5: Pull Proportion of union in general population
**********************************************************************************************************************
// Pull data with 95% CI
import delimited "$prefix/temp/TB/mzhang25/TB_HHC/data/p_union_ci.csv", clear

// 1000 draws:
	gen sd = (upper-lower)/(2*1.96)
	di in red "get 1,000 RR draws"
		forvalues k = 0/999 {	
			gen draw_`k' = rnormal(val, sd)	
		}
		forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}
		
	keep location_id sex_id age_group_id measurement draw_* 		
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/p_union_draws.dta", replace
