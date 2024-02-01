// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Purpose:		Compute number and percent of sero-discordant couples in TB affected households
// Author:		Meixin Zhang
// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Load settings

	// Clear memory and establish settings
	clear all
	
	// Define focal drives
	if c(os) == "Unix" {
		global prefix "/homes/"
		local function_prefix "/ihme/cc_resources"
		set odbcmgr unixodbc
	}
	else if c(os) == "Windows" {
		global prefix "J:"
		local function_prefix "K:"
	}

**********************************************************************************************************************
** STEP 1a: compute n of people in union - prevalent scenario
**********************************************************************************************************************
// keep N of all adults in TB affected HHs: pop
use "$prefix/mzhang25/TB-HIV-household/data/prevalence_scenario/HIV_prev_TB_HH_draws.dta", clear
keep if age_group_id >0
drop draw_* hiv_*
tempfile pop_prev
save `pop_prev', replace

// Get N of all adults engaged in stable partnerships in TB affected HHs: pop
use "$prefix/mzhang25/TB-HIV-household/data/p_union_draws.dta", clear
drop measurement 
merge 1:1 location_id sex_id age_group_id using `pop_prev', keep(3)nogen
forvalues i = 0/999 {
		replace pop_`i' =  pop_`i' * draw_`i'
	}
drop draw_*

// generate all age group
collapse (sum) pop_*, by (location_id) fast
gen age_group_id=0
gen sex_id = 3
tempfile all_age_prev
save `all_age_prev', replace

**********************************************************************************************************************
** STEP 1b: prevalent scenario
**********************************************************************************************************************
// import ipos
import delimited "$prefix/mzhang25/TB-HIV-household/data/prevalence_scenario/sero_dis_cp_draws.csv", clear
forvalues i = 0/999 {
	rename iposdraw_`i' ipos_`i'
}
drop draw_*

// compute HIV+ individuals engaged in stable partnership
merge 1:1 location_id sex_id age_group_id using "$prefix/mzhang25/TB-HIV-household/data/p_union_draws.dta", keep(3)nogen
forvalues i = 0/999 {
		replace hiv_`i' =  hiv_`i' * draw_`i'
	}
drop draw_*

forvalues i = 0/999 {
		gen nsdc_`i' = hiv_`i' * ipos_`i'
	}
drop hiv_* pop_*

merge 1:1 location_id using `all_age_prev', keep(3)nogen
forvalues i = 0/999 {
		gen psdc_`i' = nsdc_`i'/(pop_`i'/2)
	}
replace measurement = "# and % of SDC"
save "$prefix/mzhang25/TB-HIV-household/data/prevalence_scenario/n_and_p_SCD_draw.dta", replace

egen mean_pop=rowmean(pop_*) 
egen upper_pop=rowpctile(pop_*), p(97.5) 
egen lower_pop=rowpctile(pop_*), p(2.5)
egen mean_nsdc=rowmean(nsdc_*) 
egen upper_nsdc=rowpctile(nsdc_*), p(97.5) 
egen lower_nsdc=rowpctile(nsdc_*), p(2.5)
egen mean_psdc=rowmean(psdc_*) 
egen upper_psdc=rowpctile(psdc_*), p(97.5) 
egen lower_psdc=rowpctile(psdc_*), p(2.5)
egen mean_ipos=rowmean(ipos_*) 
egen upper_ipos=rowpctile(ipos_*), p(97.5) 
egen lower_ipos=rowpctile(ipos_*), p(2.5)

drop ipos_* nsdc_* psdc_* pop_*
save "$prefix/mzhang25/TB-HIV-household/data/prevalence_scenario/n_and_p_SCD_summary.dta", replace

**********************************************************************************************************************
** STEP 2a: compute n of people in union-incident scenario
**********************************************************************************************************************
// keep N of all adults in TB affected HHs: pop
use "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/HIV_prev_TB_HH_draws_incident.dta", clear
keep if age_group_id >0
drop draw_* hiv_*
tempfile pop_inc
save `pop_inc', replace

// Get N of all adults engaged in stable partnerships in TB affected HHs: pop
use "$prefix/mzhang25/TB-HIV-household/data/p_union_draws.dta", clear
drop measurement 
merge 1:1 location_id sex_id age_group_id using `pop_inc', keep(3)nogen
forvalues i = 0/999 {
		replace pop_`i' =  pop_`i' * draw_`i'
	}
	drop draw_*

// generate all age group
collapse (sum) pop_*, by (location_id) fast
gen age_group_id=0
gen sex_id = 3
tempfile all_age_inc
save `all_age_inc', replace

**********************************************************************************************************************
** STEP 2b: incident scenario
**********************************************************************************************************************

import delimited "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/sero_dis_cp_draws_inc.csv", clear
forvalues i = 0/999 {
	rename iposdraw_`i' ipos_`i'
}
drop draw_*

// compute HIV+ individuals engaged in stable partnership
merge 1:1 location_id sex_id age_group_id using "$prefix/mzhang25/TB-HIV-household/data/p_union_draws.dta", keep(3)nogen
forvalues i = 0/999 {
		replace hiv_`i' =  hiv_`i' * draw_`i'
	}
drop draw_*

forvalues i = 0/999 {
		gen nsdc_`i' = hiv_`i' * ipos_`i'
	}
drop hiv_* pop_*

merge 1:1 location_id using `all_age_inc', keep(3)nogen
forvalues i = 0/999 {
		gen psdc_`i' = nsdc_`i'/(pop_`i'/2)
	}
replace measurement = "# and % of SDC"
save "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/n_and_p_SCD_draw_inc.dta", replace

egen mean_pop=rowmean(pop_*) 
egen upper_pop=rowpctile(pop_*), p(97.5) 
egen lower_pop=rowpctile(pop_*), p(2.5)
egen mean_nsdc=rowmean(nsdc_*) 
egen upper_nsdc=rowpctile(nsdc_*), p(97.5) 
egen lower_nsdc=rowpctile(nsdc_*), p(2.5)
egen mean_psdc=rowmean(psdc_*) 
egen upper_psdc=rowpctile(psdc_*), p(97.5) 
egen lower_psdc=rowpctile(psdc_*), p(2.5)
egen mean_ipos=rowmean(ipos_*) 
egen upper_ipos=rowpctile(ipos_*), p(97.5) 
egen lower_ipos=rowpctile(ipos_*), p(2.5)

drop ipos_* nsdc_* psdc_* pop_*
save "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/n_and_p_SCD_summary_inc.dta", replace
