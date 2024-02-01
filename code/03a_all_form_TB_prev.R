## Pull all form TB prevalence by age groups/sexes/locations
rm(list=ls())
gc()
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
code.dir <- ifelse(Sys.info()[1]=="Windows", "H:/", paste0("/homes/", Sys.info()['user'], "/"))

## load functions
library(data.table)
library(readstata13)
source("/ihme/cc_resources/libraries/current/r/get_draws.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")

#######################################################
## STEP 1: Pull all form TB
#######################################################  
prev <- get_draws(gbd_id_type  = "cause_id",
                       source       = "como",
                       gbd_id       = c(934, 946:950),
                       location_id  = c(179, 180, 190, 196),
                       age_group_id = c(149:151,14),
                       year_id      = 2019,
                       sex_id       = c(1,2),
                       release_id   = 6,
                       measure_id   = 5,  # 5 = prevalence
                       metric_id    = 3) 

#######################################################
## STEP2: combine 3 forms of HIV-TB and non-HIV TB
####################################################### 
prev <- melt.data.table(prev[, .SD, .SDcols = c(paste0("draw_", 0:999), "year_id", "location_id", "age_group_id", "cause_id", "sex_id")],
                          id.vars = c("year_id", "cause_id", "location_id", "age_group_id", "sex_id"))
prev <- prev[, .(value = sum(value)), 
                 by = c("location_id", "age_group_id", "sex_id", "year_id", "variable")]
prev <- dcast.data.table(prev, location_id + age_group_id + sex_id + year_id ~ variable)
prev <- as.data.table(prev)


#######################################################
## STEP3: Pull population and convert rate to count
####################################################### 
population <- get_population(age_group_id = c(8:14), 
                             year_id = 2019, 
                             location_id = c(179, 180, 190, 196), 
                             sex_id = c(1,2),
                             release_id = 6)
pop_149 <- population[age_group_id %in% c(8,9), .(age_group_id = 149, population = sum(population)), 
                      by = c("location_id","sex_id")]
pop_150 <- population[age_group_id %in% c(10,11), .(age_group_id = 150, population = sum(population)), 
                      by = c("location_id","sex_id")]
pop_151 <- population[age_group_id %in% c(12,13), .(age_group_id = 151, population = sum(population)), 
                      by = c("location_id","sex_id")]
population <- rbind(pop_149, pop_150, pop_151, population[age_group_id==14, .(location_id, sex_id, age_group_id, population)])

## convert prevalence to prevalent cases
prev <- merge(prev, population, by = c("location_id", "age_group_id", "sex_id"))
prev[, paste0("draw_", 0:999) := lapply(0:999, function(x) { get(paste0("draw_", x)) * population})]

############################
## save data
############################
prev[, mean := rowMeans(.SD), .SDcols = (paste0("draw_", 0:999))]
prev[, lower:= apply(.SD, 1, quantile, probs = .025, na.rm = T), .SDcols = (paste0("draw_", 0:999))]
prev[, upper:= apply(.SD, 1, quantile, probs = .975, na.rm = T), .SDcols = (paste0("draw_", 0:999))]

summary <- prev[, .(age_group_id, location_id, sex_id, mean, upper, lower)]
write.csv(summary, "/homes/mzhang25/TB-HIV-household/data/prevalence_scenario/TB_prev_summary.csv", row.names = F)

prev[, mean := NULL][, upper := NULL][, lower := NULL][, cause_id := NULL][, year_id := NULL][, metric_id := NULL][, version_id := NULL][, measure_id := NULL]
save.dta13(prev, "/homes/mzhang25/TB-HIV-household/data/prevalence_scenario/TB_prev_draws.dta")
