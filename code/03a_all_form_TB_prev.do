// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Purpose:		
// Author:		Meixin Zhang
// Description:	
// Variables:	
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
** STEP 1: Pull all form TB and aggregate by age groups
**********************************************************************************************************************

// Pull TB all-forms
adopath + "`function_prefix'/libraries/current/stata/"
get_draws, gbd_id_type(modelable_entity_id) gbd_id(9806) location_id("[179, 180, 190, 196]") year_id(2019) measure_id(5) source(epi) gbd_round_id(7) decomp_step(iterative) clear
drop measure_id metric_id modelable_entity_id year_id model_version_id
keep if age_group_id > 7 & age_group_id < 15
save "$prefix/temp/TB/mzhang25/TB_HHC/data/all_TB_prev.dta", replace

// Pull populations
get_population, location_id("-1") year_id(2019) sex_id("1 2") age_group_id("-1") gbd_round_id(7) decomp_step(iterative) clear
keep if age_group_id > 7 & age_group_id < 15
keep if location_id == 179 | location_id == 180 |location_id == 190 | location_id == 196
drop year_id run_id
tempfile pop
save `pop', replace

// Get the number of prevalent cases
use `pop', clear
merge 1:1 location_id sex_id age_group_id using "$prefix/temp/TB/mzhang25/TB_HHC/data/all_TB_prev.dta", keep(3)nogen
forvalues i = 0/999 {
		replace draw_`i' = draw_`i' * population
	}
tempfile all_TB_prev_count
save `all_TB_prev_count', replace 

// Aggregate age groups
	forvalues i=8(1)14 {
	preserve
		keep if age_group_id ==`i' 
		tempfile tmp_age_`i'
		save `tmp_age_`i'', replace
	restore
	}
	
	// Create each age group
	use `tmp_age_8', clear
	forvalues i = 0/999 {
		rename draw_`i' age_8_`i'
	}
	save `tmp_age_8', replace

	use `tmp_age_9', clear
	forvalues i = 0/999 {
		rename draw_`i' age_9_`i'
	}
	save `tmp_age_9', replace

	use `tmp_age_10', clear
	forvalues i = 0/999 {
		rename draw_`i' age_10_`i'
	}
	save `tmp_age_10', replace

	use `tmp_age_11', clear
	forvalues i = 0/999 {
		rename draw_`i' age_11_`i'
	}
	save `tmp_age_11', replace

	use `tmp_age_12', clear
	forvalues i = 0/999 {
		rename draw_`i' age_12_`i'
	}
	save `tmp_age_12', replace

	use `tmp_age_13', clear
	forvalues i = 0/999 {
		rename draw_`i' age_13_`i'
	}
	save `tmp_age_13', replace

	// create 10 year age bins
	use `tmp_age_8', clear
	merge 1:1 location_id sex_id using `tmp_age_9', keep(3)nogen
	forvalues i = 0/999 {
			gen draw_`i' = age_8_`i' + age_9_`i'
			drop age_8_`i' age_9_`i'
			replace age_group_id = 149
	}
	tempfile tmp_age_149
	save `tmp_age_149', replace

	use `tmp_age_10', clear
	merge 1:1 location_id sex_id using `tmp_age_11', keep(3)nogen
	forvalues i = 0/999 {
			gen draw_`i'=  age_10_`i' + age_11_`i'
			drop age_10_`i' age_11_`i'
			replace age_group_id = 150
	}
	tempfile tmp_age_150
	save `tmp_age_150', replace

	use `tmp_age_12', clear
	merge 1:1 location_id sex_id using `tmp_age_13', keep(3)nogen
	forvalues i = 0/999 {
			gen draw_`i'=  age_12_`i' + age_13_`i'
			drop age_12_`i' age_13_`i'
			replace age_group_id = 151
	}
	tempfile tmp_age_151
	save `tmp_age_151', replace

	
	// Append three 10 age bins and one 5 age bin
	use `tmp_age_149', clear
	append using `tmp_age_150'
	append using `tmp_age_151'
	append using `tmp_age_14'
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/TB_prev_draws.dta", replace
	
	egen mean=rowmean(draw_*) 
	egen upper=rowpctile(draw_*), p(97.5) 
	egen lower=rowpctile(draw_*), p(2.5)
	keep age_group_id location_id sex_id mean upper lower
	
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/TB_prev_summary.dta", replace
