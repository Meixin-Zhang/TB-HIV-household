// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Purpose:		The Number of people with all-form incident TB by age/sex/country (1000 draws)
//              The Number of people with incident HIV-TB by age/sex/country (1000 draws)
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
** STEP 1: Pull all form TB and aggregate by age groups
**********************************************************************************************************************

// Pull TB all-forms incidence
adopath + "`function_prefix'/libraries/current/stata/"
get_draws, gbd_id_type(modelable_entity_id) gbd_id(9806) year_id(2019) measure_id(6) source(epi) gbd_round_id(6) decomp_step(iterative) clear
keep if location_id == 179 | location_id == 180 |location_id == 190 | location_id == 196
drop measure_id metric_id modelable_entity_id year_id model_version_id
keep if age_group_id > 7 & age_group_id < 15
save "$prefix/temp/TB/mzhang25/TB_HHC/data/all_TB_inc.dta", replace

// Pull populations
get_population, location_id("-1") year_id(2019) sex_id("1 2") age_group_id("-1") gbd_round_id(6) decomp_step(iterative) clear
keep if age_group_id > 7 & age_group_id < 15
keep if location_id == 179 | location_id == 180 |location_id == 190 | location_id == 196
drop year_id run_id
save `pop', replace
// save "$prefix/temp/TB/mzhang25/TB_HHC/data/population.dta", replace

// Get the number of incident cases
use `pop', clear
merge 1:1 location_id sex_id age_group_id using "$prefix/temp/TB/mzhang25/TB_HHC/data/all_TB_inc.dta", keep(3)nogen
forvalues i = 0/999 {
		replace draw_`i' = draw_`i' * population
	}
tempfile all_tb_inc
save `all_tb_inc', replace


// Aggregate age groups
	forvalues i=8(1)14 {
	preserve
		keep if age_group_id ==`i' 
		tempfile tmp_age_`i'
		save `tmp_age_`i'', replace
	restore
	}
	//save `age_group_`i'', replace
	
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
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/all_TB_inc_draws.dta", replace
	
	egen mean=rowmean(draw_*) 
	egen upper=rowpctile(draw_*), p(97.5) 
	egen lower=rowpctile(draw_*), p(2.5)
	keep age_group_id location_id sex_id mean upper lower
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/all_TB_inc_summary.dta", replace

**********************************************************************************************************************
** STEP 2: Pull all form HIV-TB and aggregate by age groups
**********************************************************************************************************************
** get 3 forms of HIV-TB incidence
adopath + "`function_prefix'/libraries/current/stata/"
get_draws, gbd_id_type(modelable_entity_id) gbd_id(10832) year_id(2019) measure_id(6) source(epi) release_id(6) clear
keep if location_id == 179 | location_id == 180 |location_id == 190 | location_id == 196
drop measure_id metric_id modelable_entity_id year_id model_version_id
keep if age_group_id > 7 & age_group_id < 15
tempfile hiv_tb_ds
save `hiv_tb_ds', replace

get_draws, gbd_id_type(modelable_entity_id) gbd_id(10833) year_id(2019) measure_id(6) source(epi) release_id(6) clear
keep if location_id == 179 | location_id == 180 |location_id == 190 | location_id == 196
drop measure_id metric_id modelable_entity_id year_id model_version_id
keep if age_group_id > 7 & age_group_id < 15
tempfile hiv_tb_mdr
save `hiv_tb_mdr', replace

get_draws, gbd_id_type(modelable_entity_id) gbd_id(10834) year_id(2019) measure_id(6) source(epi) release_id(6) clear
keep if location_id == 179 | location_id == 180 |location_id == 190 | location_id == 196
drop measure_id metric_id modelable_entity_id year_id model_version_id
keep if age_group_id > 7 & age_group_id < 15
tempfile hiv_tb_xdr
save `hiv_tb_xdr', replace


** convert percent into count
use `hiv_tb_ds', clear
merge 1:1 location_id sex_id age_group_id using `pop', keep(3)nogen
forvalues i = 0/999 {
		replace draw_`i' = draw_`i' * population
	}
	drop population
save `hiv_tb_ds', replace

use `hiv_tb_mdr', clear
merge 1:1 location_id sex_id age_group_id using `pop', keep(3)nogen
forvalues i = 0/999 {
		replace draw_`i' = draw_`i' * population
	}
	drop population
save `hiv_tb_mdr', replace

use `hiv_tb_xdr', clear
merge 1:1 location_id sex_id age_group_id using `pop', keep(3)nogen
forvalues i = 0/999 {
		replace draw_`i' = draw_`i' * population
	}
	drop population
save `hiv_tb_xdr', replace

*** combine all forms of HIV-TB
use `hiv_tb_ds', clear
forvalues i = 0/999 {
		rename draw_`i' hiv_tb_ds_`i'
	}
merge 1:1 location_id sex_id age_group_id using `hiv_tb_mdr', keep(3)nogen
forvalues i = 0/999 {
		replace hiv_tb_ds_`i' = draw_`i' + hiv_tb_ds_`i'
		drop draw_`i'
	}
merge 1:1 location_id sex_id age_group_id using `hiv_tb_xdr', keep(3)nogen
forvalues i = 0/999 {
		replace draw_`i' = draw_`i' + hiv_tb_ds_`i'
		drop hiv_tb_ds_`i'
	}
tempfile hiv_tb
save `hiv_tb', replace

*** aggregation
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
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/HIV_TB_inc_draws.dta", replace
	
	use "$prefix/temp/TB/mzhang25/TB_HHC/data/HIV_TB_inc_draws.dta", clear
	egen mean=rowmean(draw_*) 
	egen upper=rowpctile(draw_*), p(97.5) 
	egen lower=rowpctile(draw_*), p(2.5)
	keep age_group_id location_id sex_id mean upper lower
	
	save "$prefix/temp/TB/mzhang25/TB_HHC/data/HIV_TB_inc_summary.dta", replace
