## Pull all form TB incidence by age groups/sexes/locations
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
## STEP 1: Pull all form TB and aggregate by age groups
#######################################################  
inc_all <- get_draws(gbd_id_type  = "cause_id",
                  source       = "como",
                  gbd_id       = 297,
                  location_id  = c(179, 180, 190, 196),
                  age_group_id = c(149:151,14),
                  year_id      = 2019,
                  sex_id       = c(1,2),
                  release_id   = 6,
                  measure_id   = 6,  # 6 = incidence
                  metric_id    = 3) 

## Pull populations and aggregate by age groups
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
inc_all <- merge(inc_all, population)
inc_all[, paste0("draw_", 0:999) := lapply(0:999, function(x) { get(paste0("draw_", x)) * population})]


###########################################################
## STEP 2: Pull all form HIV-TB and aggregate by age groups
###########################################################
## get 3 forms of HIV-TB incidence
hiv_tb_ds <- get_draws(gbd_id_type  = "cause_id",
                     source       = "como",
                     gbd_id       = 948,
                     location_id  = c(179, 180, 190, 196),
                     age_group_id = c(149:151,14),
                     year_id      = 2019,
                     sex_id       = c(1,2),
                     release_id   = 6,
                     measure_id   = 6,  # 6 = incidence
                     metric_id    = 3) 

hiv_tb_mdr <- get_draws(gbd_id_type  = "cause_id",
                     source       = "como",
                     gbd_id       = 949,
                     location_id  = c(179, 180, 190, 196),
                     age_group_id = c(149:151,14),
                     year_id      = 2019,
                     sex_id       = c(1,2),
                     release_id   = 6,
                     measure_id   = 6,  # 6 = incidence
                     metric_id    = 3) 

hiv_tb_xdr <- get_draws(gbd_id_type  = "cause_id",
                     source       = "como",
                     gbd_id       = 950,
                     location_id  = c(179, 180, 190, 196),
                     age_group_id = c(149:151,14),
                     year_id      = 2019,
                     sex_id       = c(1,2),
                     release_id   = 6,
                     measure_id   = 6,  # 6 = incidence
                     metric_id    = 3) 

## combine 3 forms of HIV-TB incidence
hiv_tb <- rbind(hiv_tb_ds, hiv_tb_mdr, hiv_tb_xdr)
hiv_tb <- melt.data.table(hiv_tb[, .SD, .SDcols = c(paste0("draw_", 0:999), "year_id", "location_id", "age_group_id", "cause_id", "sex_id")],
                          id.vars = c("year_id", "location_id", "age_group_id", "cause_id", "sex_id"))
hiv_tb <- hiv_tb[, .(value = sum(value)), 
                 by = c("location_id", "age_group_id", "sex_id", "year_id", "variable")]
hiv_tb <- dcast.data.table(hiv_tb, location_id + age_group_id + sex_id + year_id ~ variable)
hiv_tb <- as.data.table(hiv_tb)
hiv_tb <- merge(hiv_tb, population, by = c("location_id", "age_group_id", "sex_id"))

## convert rate to count
hiv_tb[, paste0("draw_", 0:999) := lapply(0:999, function(x) { get(paste0("draw_", x)) * population})]
hiv_tb[, year_id := NULL][, population := NULL]

## save data
save.dta13(hiv_tb, "/homes/mzhang25/TB-HIV-household/data/incidence_scenario/HIV_TB_inc_draws.dta")


hiv_tb[, mean := rowMeans(.SD), .SDcols = (paste0("draw_", 0:999))]
hiv_tb[, lower:= apply(.SD, 1, quantile, probs = .025, na.rm = T), .SDcols = (paste0("draw_", 0:999))]
hiv_tb[, upper:= apply(.SD, 1, quantile, probs = .975, na.rm = T), .SDcols = (paste0("draw_", 0:999))]

summary <- hiv_tb[, .(age_group_id, location_id, sex_id, mean, upper, lower)]
write.csv(summary, "/homes/mzhang25/TB-HIV-household/data/incidence_scenario/HIV_TB_inc_summary.csv", row.names = F)

###########################################################
## STEP 3: combine HIV-TB and non-HIV TB
###########################################################
hiv_tb[, mean := NULL][, lower := NULL][, upper := NULL][, cause_id := 948]
inc_all[, year_id := NULL][, metric_id := NULL][, version_id := NULL][, measure_id := NULL][, population:= NULL]
inc_all <- rbind(inc_all, hiv_tb)
inc_all <- melt.data.table(inc_all[, .SD, .SDcols = c(paste0("draw_", 0:999), "location_id", "age_group_id", "cause_id", "sex_id")],
                          id.vars = c("location_id", "age_group_id", "cause_id", "sex_id"))
inc_all <- inc_all[, .(value = sum(value)), 
                 by = c("location_id", "age_group_id", "sex_id", "variable")]
inc_all <- dcast.data.table(inc_all, location_id + age_group_id + sex_id ~ variable)

## save data
save.dta13(inc_all, "/homes/mzhang25/TB-HIV-household/data/incidence_scenario/all_TB_inc_draws.dta")

inc_all[, mean := rowMeans(.SD), .SDcols = (paste0("draw_", 0:999))]
inc_all[, lower:= apply(.SD, 1, quantile, probs = .025, na.rm = T), .SDcols = (paste0("draw_", 0:999))]
inc_all[, upper:= apply(.SD, 1, quantile, probs = .975, na.rm = T), .SDcols = (paste0("draw_", 0:999))]

summary <- inc_all[, .(age_group_id, location_id, sex_id, mean, upper, lower)]
write.csv(summary, "/homes/mzhang25/TB-HIV-household/data/incidence_scenario/all_TB_inc_summary.csv", row.names = F)
