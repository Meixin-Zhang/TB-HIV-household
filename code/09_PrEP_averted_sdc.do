// ****************************************************************************************************************************************************
// ****************************************************************************************************************************************************
// Purpose:		HIV infections that could be averted through PrEP.use among serodifferent couples in TB affected households
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
** STEP 1: Generate 1000 draws of PrEP input
**********************************************************************************************************************
// Pull data with 95% CI
import delimited "$prefix/temp/TB/mzhang25/TB_HHC/data/PrEP_input.csv", clear

// 1000 draws:
	gen sd = (upper-lower)/(2*1.96)
	di in red "get 1,000 RR draws"
		forvalues k = 0/999 {	
			gen draw_`k' = rnormal(mean, sd)	
		}
		forvalues i=0/999 {
			replace draw_`i' = 0 if draw_`i' < 0
		}
		
	keep measurement draw_* 		
tempfile prep
save `prep', replace

**********************************************************************************************************************
** STEP 2a: No intervention: number of HIV negative individuals in SDC infected by HIV in 1 year (incidence scenario)
**********************************************************************************************************************
// Pull number of SDC 
use "$prefix/temp/TB/mzhang25/TB_HHC/data/n_and_p_SCD_draw_inc.dta", clear
keep location_id nsdc_* 	
gen measurement = "HIV transmission without PrEP"
tempfile nsdc
save `nsdc', replace

// Merge incident rate and N of SDC
use `prep', clear
keep if measurement == "HIV transmission without PrEP"
merge 1:m measurement using `nsdc', keep(3)nogen

        forvalues i=0/999 {
			gen HIV_transmission_`i' = nsdc_`i' * draw_`i'
		}

keep location_id HIV_transmission_*
tempfile hiv_transmission
save `hiv_transmission', replace

**********************************************************************************************************************
** STEP 2b: with intervention: number of HIV negative individuals in SDC infected by HIV in 1 year (incidence scenario)
**********************************************************************************************************************
use "$prefix/temp/TB/mzhang25/TB_HHC/data/n_and_p_SCD_draw_inc.dta", clear
keep location_id nsdc_* 	
gen measurement = "HIV transmission with PrEP"
tempfile nsdc
save `nsdc', replace

// Merge with incident rate with prep
use `prep', clear
merge 1:m measurement using `nsdc', keep(3)nogen
		forvalues i=0/999 {
			rename draw_`i' incidence_rate_prep_`i'
		}

// Merge with uptake
replace measurement = "PrEP uptake"
merge m:1 measurement using `prep', keep(3)nogen

forvalues i=0/999 {
			rename draw_`i' uptake_`i'
		}
		
// Merge with incident rate without prep
replace measurement = "HIV transmission without PrEP"
merge m:1 measurement using `prep', keep(3)nogen

forvalues i=0/999 {
			rename draw_`i' incidence_rate_`i'
		}

// calculate incidence rate (n * uptake * incident rate with prep + n(1-uptake) * incident rate without prep)
forvalues i=0/999 {
			replace nsdc_`i' = nsdc_`i' * uptake_`i'*incidence_rate_prep_`i' + nsdc_`i' * (1-uptake_`i')*incidence_rate_`i'
		}

keep location_id nsdc_*
forvalues i=0/999 {
			rename nsdc_`i' HIV_transmission_prep_`i'
		}
keep location_id HIV_transmission_prep_*
tempfile hiv_transmission_prep
save `hiv_transmission_prep', replace

**********************************************************************************************************************
** STEP 2c: subtract transmission with PrEP from transmission without PrEP
**********************************************************************************************************************
use `hiv_transmission', clear
merge 1:1 location_id using `hiv_transmission_prep', keep(3)nogen
forvalues i=0/999 {
			gen averted_transmission_`i' = HIV_transmission_`i' - HIV_transmission_prep_`i'
		}
		
egen mean_HIV_transmission=rowmean(HIV_transmission_*) 
egen upper_HIV_transmission=rowpctile(HIV_transmission_*), p(97.5) 
egen lower_HIV_transmission=rowpctile(HIV_transmission_*), p(2.5)

egen mean_HIV_transmission_prep=rowmean(HIV_transmission_prep_*) 
egen upper_HIV_transmission_prep=rowpctile(HIV_transmission_prep_*), p(97.5) 
egen lower_HIV_transmission_prep=rowpctile(HIV_transmission_prep_*), p(2.5)

egen mean_averted_transmission=rowmean(averted_transmission_*) 
egen upper_averted_transmission=rowpctile(averted_transmission_*), p(97.5) 
egen lower_averted_transmission=rowpctile(averted_transmission_*), p(2.5)

drop HIV_transmission_* HIV_transmission_prep_* averted_transmission_*
save "$prefix/temp/TB/mzhang25/TB_HHC/data/PrEP_summary_inc.dta", replace

**********************************************************************************************************************
** STEP 3a: No intervention: number of HIV negative individuals in SDC infected by HIV in 1 year (prevalence scenario)
**********************************************************************************************************************
// Pull number of SDC 
use "$prefix/temp/TB/mzhang25/TB_HHC/data/n_and_p_SCD_draw.dta", clear
keep location_id nsdc_* 	
gen measurement = "HIV transmission without PrEP"
tempfile nsdc
save `nsdc', replace

// Merge incident rate and N of SDC
use `prep', clear
merge 1:m measurement using `nsdc', keep(3)nogen

        forvalues i=0/999 {
			gen HIV_transmission_`i' = nsdc_`i' * draw_`i'
		}

keep location_id HIV_transmission_*
tempfile hiv_transmission
save `hiv_transmission', replace

**********************************************************************************************************************
** STEP 3b: with intervention: number of HIV negative individuals in SDC infected by HIV in 1 year (incidence scenario)
**********************************************************************************************************************
use "$prefix/temp/TB/mzhang25/TB_HHC/data/n_and_p_SCD_draw_inc.dta", clear
keep location_id nsdc_* 	
gen measurement = "HIV transmission with PrEP"
tempfile nsdc
save `nsdc', replace

// Merge with incident rate with prep
use `prep', clear
merge 1:m measurement using `nsdc', keep(3)nogen
forvalues i=0/999 {
			rename draw_`i' incidence_rate_prep_`i'
		}

// Merge with uptake
replace measurement = "PrEP uptake"
merge m:1 measurement using `prep', keep(3)nogen

forvalues i=0/999 {
			rename draw_`i' uptake_`i'
		}
		
// Merge with incident rate without prep
replace measurement = "HIV transmission without PrEP"
merge m:1 measurement using `prep', keep(3)nogen

forvalues i=0/999 {
			rename draw_`i' incidence_rate_`i'
		}

// calculate incidence rate (n * uptake * incident rate with prep + n(1-uptake) * incident rate without prep)
forvalues i=0/999 {
			replace nsdc_`i' = nsdc_`i' * uptake_`i'*incidence_rate_prep_`i' + nsdc_`i' * (1-uptake_`i')*incidence_rate_`i'
		}

keep location_id nsdc_*
forvalues i=0/999 {
			rename nsdc_`i' HIV_transmission_prep_`i'
		}
keep location_id HIV_transmission_prep_*
tempfile hiv_transmission_prep
save `hiv_transmission_prep', replace

**********************************************************************************************************************
** STEP 3c: subtract transmission with PrEP from transmission without PrEP
**********************************************************************************************************************
use `hiv_transmission', clear
merge 1:1 location_id using `hiv_transmission_prep', keep(3)nogen
forvalues i=0/999 {
			gen averted_transmission_`i' = HIV_transmission_`i' - HIV_transmission_prep_`i'
		}
		
egen mean_HIV_transmission=rowmean(HIV_transmission_*) 
egen upper_HIV_transmission=rowpctile(HIV_transmission_*), p(97.5) 
egen lower_HIV_transmission=rowpctile(HIV_transmission_*), p(2.5)

egen mean_HIV_transmission_prep=rowmean(HIV_transmission_prep_*) 
egen upper_HIV_transmission_prep=rowpctile(HIV_transmission_prep_*), p(97.5) 
egen lower_HIV_transmission_prep=rowpctile(HIV_transmission_prep_*), p(2.5)

egen mean_averted_transmission=rowmean(averted_transmission_*) 
egen upper_averted_transmission=rowpctile(averted_transmission_*), p(97.5) 
egen lower_averted_transmission=rowpctile(averted_transmission_*), p(2.5)

drop HIV_transmission_* HIV_transmission_prep_* averted_transmission_*
save "$prefix/temp/TB/mzhang25/TB_HHC/data/PrEP_summary_prev.dta", replace
