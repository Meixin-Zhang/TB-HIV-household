## EMPTY THE ENVIRONMENT
rm(list = ls())

## ESTABLISH FOCAL DRIVES
os <- .Platform$OS.type
if (os=="windows") {
  j <-"J:/"
  h <-"H:/"
  k <-"K:/"
} else {
  j <-"/home/j/"
  h <-paste0("homes/", Sys.info()[7], "/")
  k <-"/ihme/cc_resources/"
}

library(data.table)
#############################################################################################
###             calculate 95% CI of HIV prevalence in general population                  ###
#############################################################################################
dt <- as.data.table(read.csv(paste0(j, "temp/TB/mzhang25/TB_HHC/data/hiv_prev_national.csv")))
dt[, upper := val + qnorm(0.975)*sqrt(val*(1-val)/sample_size)]
dt[, lower := val - qnorm(0.975)*sqrt(val*(1-val)/sample_size)]
fwrite(dt, file = paste0(j, "temp/TB/mzhang25/TB_HHC/data/hiv_prev_national_ci.csv"), row.names = F)

#############################################################################################
###                        calculate HIV prevalence in prevalent TB-ZAF                   ###
#############################################################################################
## Calculate RR of HIV in prevalent TB vs. HIV in general pop
hiv_tb_p <- 55 ## South Africa TB Prevalence Results - Final
hiv_tb_n <- 136 ## South Africa TB Prevalence Results - Final
hiv_national_p <- 1231 ## DHS 2016 15+ ages
hiv_national_n <- 5353 ## DHS 2016 15+ ages
dt <- data.frame(hiv_tb_p, hiv_tb_n, hiv_national_p, hiv_national_n)
dt <- as.data.table(dt)
dt[, rr_mean := hiv_tb_p*(hiv_national_n+hiv_national_p)/(hiv_national_p*(hiv_tb_p+hiv_tb_n))]
dt[, se := sqrt((1/hiv_national_p)+(1/hiv_tb_p)-(1/(hiv_national_n+hiv_national_p))-(1/(hiv_tb_p+hiv_tb_n)))]
dt[, rr_lower := rr_mean -1.96*se][, rr_upper := rr_mean + 1.96*se]
## format
dt[, location_id := 196]
dt <- dt[, .(location_id, rr_mean, rr_lower, rr_upper)]
dt[, measurement := "hiv in prevalent TB vs. national hiv prevalence (rr)"][, data_source := "TB national household survey short report; DHS2016"]
fwrite(dt, file = paste0(j, "temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ZAF.csv"), row.names = F)

#############################################################################################
###                    calculate HIV prevalence in prevalent TB-ETH                       ###
#############################################################################################
# prevalent vs. notified
location_id <- 179
rr_mean <- 0.47
rr_lower <- 0.34
rr_upper <- 0.65
dt <- data.table(location_id, rr_mean, rr_lower, rr_upper)
dt[, measurement := "hiv in prevalent TB vs. hiv in notified TB (rr)"][, data_source := "Methods used by WHO to estimate the global burden of TB disease"]
fwrite(dt, file = paste0(j, "temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH.csv"), row.names = F)

# notified vs. general pop
hiv_tb_notified_p <- 5978 # from who notifications
hiv_tb_notified_n <- 105061 # from who notifications
hiv_national_p <- 222 # from DHS
hiv_national_n <- 24457 # from DHS
dt <- data.frame(hiv_tb_notified_p, hiv_tb_notified_n, hiv_national_p, hiv_national_n)
dt <- as.data.table(dt)
dt[, rr_mean := hiv_tb_notified_p*(hiv_national_n+hiv_national_p)/(hiv_national_p*(hiv_tb_notified_p+hiv_tb_notified_n))]
dt[, se := sqrt((1/hiv_national_p)+(1/hiv_tb_notified_p)-(1/(hiv_national_n+hiv_national_p))-(1/(hiv_tb_notified_p+hiv_tb_notified_n)))]
dt[, rr_lower := rr_mean -1.96*se][, rr_upper := rr_mean + 1.96*se]
## format
dt[, location_id := 179]
dt <- dt[, .(location_id, rr_mean, rr_lower, rr_upper)]
dt[, measurement := "hiv in notified TB vs. national hiv prevalence (rr)"][, data_source := "WHO notification; DHS2016"]
fwrite(dt, file = paste0(j, "temp/TB/mzhang25/TB_HHC/data/hiv_prev_rr_ETH2.csv"), row.names = F)

#############################################################################################
###                  calculate 95% CI of HIV prevalence in prevalent TB                   ###
#############################################################################################
dt <- as.data.table(read.csv(paste0(j, "temp/TB/mzhang25/TB_HHC/data/HIV_TB_prev_percent.csv")))
dt[, upper := val + qnorm(0.975)*sqrt(val*(1-val)/sample_size)]
dt[, lower := val - qnorm(0.975)*sqrt(val*(1-val)/sample_size)]
fwrite(dt, file = paste0(j, "temp/TB/mzhang25/TB_HHC/data/HIV_TB_prev_percent_ci.csv"), row.names = F)

#############################################################################################
###               calculate 95% CI of proportion of union in general population           ###
#############################################################################################
dt <- as.data.table(read.csv(paste0(j, "temp/TB/mzhang25/TB_HHC/data/p_union.csv")))
dt[, X:=NULL]
dt[, upper := val + qnorm(0.975)*sqrt(val*(1-val)/sample_size)]
dt[, lower := val - qnorm(0.975)*sqrt(val*(1-val)/sample_size)]
fwrite(dt, file = paste0(j, "temp/TB/mzhang25/TB_HHC/data/p_union_ci.csv"), row.names = F)

#############################################################################################
###                            calculate 95% CI of PrEP input                             ###
#############################################################################################
dt <- as.data.table(read.csv(paste0(j, "temp/TB/mzhang25/TB_HHC/data/PrEP_input.csv")))
dt[, upper := mean + qnorm(0.975)*sqrt(mean*(1-mean)/sample_size)]
dt[, lower := mean - qnorm(0.975)*sqrt(mean*(1-mean)/sample_size)]
fwrite(dt, file = paste0(j, "temp/TB/mzhang25/TB_HHC/data/PrEP_input.csv"), row.names = F)
