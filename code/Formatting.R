## EMPTY THE ENVIRONMENT
rm(list = ls())

## Plot all ages for COD models
if (Sys.info()["sysname"] == "Linux") {
  j <- "/home/j/"
  h <- paste0("/ihme/homes/", Sys.info()["user"], "/")
  l <- "/ihme/limited_use/"
  k <- "/ihme/cc_resources/libraries/"
} else {
  j <- "J:/"
  h <- "H:/"
  l <- "L:/"
  k <- "K:/libraries/"
}

## LOAD FUNCTIONS AND PACKAGES
library(readstata13)

######################
## Prevalence scenario
######################
main_dir <- paste0(h, "TB-HIV-household/data/prevalence_scenario")
## HIV prevalence in TB affected household
dt <- as.data.table(read.dta13(paste0(main_dir, "/HIV_prev_TB_HH_summary.dta")))
dt <- dt[age_group_id==0,]
dt[, mean := round(mean, digits = 4)][, lower := round(lower, digits = 4)][, upper := round(upper, digits = 4)]
dt[, mean := mean*100][, lower := lower*100][, upper := upper*100]
dt[, mean_hiv := round(mean_hiv, digits = 0)][, lower_hiv := round(lower_hiv, digits = 0)][, upper_hiv := round(upper_hiv, digits = 0)]
dt[, mean_pop := round(mean_pop, digits = 0)][, lower_pop := round(lower_pop, digits = 0)][, upper_pop := round(upper_pop, digits = 0)]
dt [, hiv_pop := paste0(mean_hiv, "(", lower_hiv, "-", upper_hiv, ")")]
dt [, total_pop := paste0(mean_pop, "(", lower_pop, "-", upper_pop, ")")]
dt [, hiv_prev := paste0(mean, "(", lower, "-", upper, ")")]

dt[, .(location_id, hiv_pop, total_pop, hiv_prev)]

## SDC n and p in TB affected household
dt <- as.data.table(read.dta13(paste0(main_dir, "/n_and_p_SCD_summary.dta")))
dt[, mean_psdc := round(mean_psdc, digits = 4)][, lower_psdc := round(lower_psdc, digits = 4)][, upper_psdc := round(upper_psdc, digits = 4)]
dt[, mean_psdc := mean_psdc*100][, lower_psdc := lower_psdc*100][, upper_psdc := upper_psdc*100]
dt[, mean_nsdc := round(mean_nsdc, digits = 0)][, lower_nsdc := round(lower_nsdc, digits = 0)][, upper_nsdc := round(upper_nsdc, digits = 0)]
dt [, psdc := paste0(mean_psdc, "(", lower_psdc, "-", upper_psdc, ")")]
dt [, nsdc := paste0(mean_nsdc, "(", lower_nsdc, "-", upper_nsdc, ")")]

dt[, .(location_id, nsdc, psdc)]

## averted HIV
dt <- as.data.table(read.dta13(paste0(main_dir, "/PrEP_summary_prev.dta")))
dt[, mean_averted_transmission := round(mean_averted_transmission, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission, digits = 0)]
dt[, mean_averted_transmission := round(mean_averted_transmission*0.75, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission*0.75, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission*0.75, digits = 0)]
dt[, mean_averted_transmission := round(mean_averted_transmission*0.5, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission*0.5, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission*0.5, digits = 0)]
dt[, mean_averted_transmission := round(mean_averted_transmission*0.25, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission*0.25, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission*0.25, digits = 0)]

dt [, averted_transmission := paste0(mean_averted_transmission, "(", lower_averted_transmission, "-", upper_averted_transmission, ")")]

dt[,.(location_id, averted_transmission)]

######################
## Incidence scenario
######################
main_dir <- paste0(h, "TB-HIV-household/data/incidence_scenario")
## HIV prevalence in TB affected household
dt <- as.data.table(read.dta13(paste0(main_dir, "/HIV_prev_TB_HH_summary_incident.dta")))
dt <- dt[age_group_id==0,]
dt[, mean := round(mean, digits = 4)][, lower := round(lower, digits = 4)][, upper := round(upper, digits = 4)]
dt[, mean := mean*100][, lower := lower*100][, upper := upper*100]
dt[, mean_hiv := round(mean_hiv, digits = 0)][, lower_hiv := round(lower_hiv, digits = 0)][, upper_hiv := round(upper_hiv, digits = 0)]
dt[, mean_pop := round(mean_pop, digits = 0)][, lower_pop := round(lower_pop, digits = 0)][, upper_pop := round(upper_pop, digits = 0)]
dt [, hiv_pop := paste0(mean_hiv, "(", lower_hiv, "-", upper_hiv, ")")]
dt [, total_pop := paste0(mean_pop, "(", lower_pop, "-", upper_pop, ")")]
dt [, hiv_prev := paste0(mean, "(", lower, "-", upper, ")")]

dt[, .(location_id, hiv_pop, total_pop, hiv_prev)]

## SDC n and p in TB affected household
dt <- as.data.table(read.dta13(paste0(main_dir, "/n_and_p_SCD_summary_inc.dta")))
dt[, mean_psdc := round(mean_psdc, digits = 4)][, lower_psdc := round(lower_psdc, digits = 4)][, upper_psdc := round(upper_psdc, digits = 4)]
dt[, mean_psdc := mean_psdc*100][, lower_psdc := lower_psdc*100][, upper_psdc := upper_psdc*100]
dt[, mean_nsdc := round(mean_nsdc, digits = 0)][, lower_nsdc := round(lower_nsdc, digits = 0)][, upper_nsdc := round(upper_nsdc, digits = 0)]
dt [, psdc := paste0(mean_psdc, "(", lower_psdc, "-", upper_psdc, ")")]
dt [, nsdc := paste0(mean_nsdc, "(", lower_nsdc, "-", upper_nsdc, ")")]

dt[, .(location_id, nsdc, psdc)]

## averted HIV
dt <- as.data.table(read.dta13(paste0(main_dir, "/PrEP_summary_inc.dta")))
dt[, mean_averted_transmission := round(mean_averted_transmission, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission, digits = 0)]
dt[, mean_averted_transmission := round(mean_averted_transmission*0.75, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission*0.75, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission*0.75, digits = 0)]
dt[, mean_averted_transmission := round(mean_averted_transmission*0.5, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission*0.5, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission*0.5, digits = 0)]
dt[, mean_averted_transmission := round(mean_averted_transmission*0.25, digits = 0)][, lower_averted_transmission := round(lower_averted_transmission*0.25, digits = 0)][, upper_averted_transmission := round(upper_averted_transmission*0.25, digits = 0)]

dt [, averted_transmission := paste0(mean_averted_transmission, "(", lower_averted_transmission, "-", upper_averted_transmission, ")")]

dt[,.(location_id, averted_transmission)]
