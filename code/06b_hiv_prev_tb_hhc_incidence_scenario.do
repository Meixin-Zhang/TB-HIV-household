// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Purpose:		incidence scenario - compute the N and P of HIV+ in tb affected households
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
** STEP 1: HIV prevalence in HHC
**********************************************************************************************************************
// import HHC HIV relative risk
import delimited "$prefix/mzhang25/TB-HIV-household/data/lit_review/rr_draws.csv", clear
drop ratio lower upper sd
forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}
tempfile hhc_hiv_rr
save `hhc_hiv_rr', replace

// merge on national HIV prev
use "$prefix/mzhang25/TB-HIV-household/data/hiv_prev_national_draws.dta", clear
preserve 
	drop if location_id == 179
	forvalues i = 0/999 {
		rename draw_`i' hiv_`i'
	}
	
	merge m:1 location_id using `hhc_hiv_rr', keep(3)nogen
	forvalues i = 0/999 {
		replace draw_`i' = hiv_`i' * draw_`i'
		drop hiv_`i'
	}
	tempfile hiv_prev_hhc
	save `hiv_prev_hhc', replace
restore

drop if location_id > 179
append using `hiv_prev_hhc'
replace measurement = "% HIV in HHC"
save `hiv_prev_hhc', replace

**********************************************************************************************************************
** STEP 2: Number of HIV+ in HHC
**********************************************************************************************************************
import delimited "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/20211109_count_draw_of_NDPTB_hhc_incident.csv", clear
drop year
reshape wide count_value, i(location age sex) j(draw)
forvalues i = 0/999 {
		rename count_value`i' hhc_`i'
	}
gen location_id = 179
replace location_id = 180 if location == "Kenya"
replace location_id = 190 if location == "Uganda"
replace location_id = 196 if location == "South Africa"
drop location

gen age_group_id = 149 
replace age_group_id = 150 if age == "25_to_35"
replace age_group_id = 151 if age == "35_to_45"
replace age_group_id = 14 if age == "45_to_50"
drop age

gen sex_id = 1
replace sex_id = 2 if sex == "Female"
drop sex

// Generate 15-49 age group
preserve
collapse (sum) hhc_*, by (location_id) fast
gen age_group_id=0
gen sex_id = 3
tempfile all_age
save `all_age', replace
restore 

append using `all_age'
tempfile all_hhc
save `all_hhc', replace

merge 1:1 location_id age_group_id sex_id using `hiv_prev_hhc', keep(3)nogen
forvalues i = 0/999 {
		replace draw_`i' = hhc_`i' * draw_`i'
		drop hhc_`i'
	}

// Generate 15-49 age group
preserve
collapse (sum) draw_*, by (location_id) fast
gen age_group_id=0
gen sex_id = 3
tempfile all_age
save `all_age', replace
restore 

append using `all_age'
replace measurement = "# HIV in HHC"
tempfile hiv_hhc
save `hiv_hhc', replace

**********************************************************************************************************************
** STEP 3: Number and prevalence of HIV+ in TB affected HH
**********************************************************************************************************************
// numerator: all HIV+ cases
use "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/HIV_TB_inc_draws.dta", clear
forvalues i = 0/999 {
		rename draw_`i' hiv_tb_`i'
	}

	// Generate 15-49 age group
	preserve
	collapse (sum) hiv_tb_*, by (location_id) fast
	gen age_group_id=0
	gen sex_id = 3
	tempfile all_age
	save `all_age', replace
	restore 

	append using `all_age'

merge 1:1 location_id age_group_id sex_id using `hiv_hhc', keep(3)nogen
forvalues i = 0/999 {
		gen hiv_`i' = hiv_tb_`i' + draw_`i'
		drop hiv_tb_`i' draw_`i'
	}
	replace measurement = "# HIV+ in TB affected HH"
	tempfile all_hiv
	save `all_hiv', replace
	save "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/HIV_count_TB_HH_draws_incidence_scenario.dta", replace
	
// denominator: TB cases + HHC
use "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/all_TB_inc_draws.dta", clear
	// Generate 15-49 age group
	preserve
	collapse (sum) draw_*, by (location_id) fast
	gen age_group_id=0
	gen sex_id = 3
	tempfile all_age
	save `all_age', replace
	restore 
	append using `all_age'
	
merge 1:1 location_id age_group_id sex_id using `all_hhc', keep(3)nogen
	forvalues i = 0/999 {
		gen pop_`i' = draw_`i' + hhc_`i'
		drop draw_`i' hhc_`i'
	}

	tempfile all_pop
	save `all_pop', replace

// compute hiv prev in tb affected HH
use `all_hiv', clear
merge 1:1 location_id age_group_id sex_id using `all_pop', keep(3)nogen
forvalues i = 0/999 {
		gen draw_`i' = hiv_`i' / pop_`i'
	}
	replace measurement = "% HIV in TB HH"
	save "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/HIV_prev_TB_HH_draws_incident.dta", replace
	
	egen mean=rowmean(draw_*) 
	egen upper=rowpctile(draw_*), p(97.5) 
	egen lower=rowpctile(draw_*), p(2.5)
	egen mean_hiv=rowmean(hiv_*) 
	egen upper_hiv=rowpctile(hiv_*), p(97.5) 
	egen lower_hiv=rowpctile(hiv_*), p(2.5)
	egen mean_pop=rowmean(pop_*) 
	egen upper_pop=rowpctile(pop_*), p(97.5) 
	egen lower_pop=rowpctile(pop_*), p(2.5)

	keep age_group_id location_id sex_id measurement mean upper lower mean_* upper_* lower_*
	
	save "$prefix/mzhang25/TB-HIV-household/data/incidence_scenario/HIV_prev_TB_HH_summary_incident.dta", replace
