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
  h <-paste0("/homes/", Sys.info()[7], "/")
  k <-"/ihme/cc_resources/"
}

## LOAD FUNCTIONS AND PACKAGES
source(paste0(k, "libraries/current/r/get_draws.R"))
source(paste0(k, "libraries/current/r/get_population.R"))
library(haven)

## GET PREVALENCE
df <- get_draws("modelable_entity_id", 9806, location_id=c(179, 180, 190, 196),
                year_id=2019, measure_id=5, source="epi", gbd_round_id=6,
                decomp_step="step4")
df <- df[df$age_group_id > 7 & df$age_group_id < 15,]
df <- subset(df, select = -c(measure_id, metric_id, modelable_entity_id, year_id,
                             model_version_id))
write_dta(df, paste0(j,"temp/TB/mzhang25/TB_HHC/all_TB_prev_2019.dta"))

## GET POP
pop <- get_population(age_group_id=c(8,9,10,11,12,13,14), location_id=c(179, 180, 190, 196),
                      year_id=2019, sex_id=c(1,2), release_id=6, tool_type_id=3)
pop <- subset(pop, select = -c(year_id, run_id))
write_dta(pop, paste0(j,"temp/TB/mzhang25/TB_HHC/population_2019.dta"))


## Get TB incident cases
incidence_counts_male <- get_model_results('epi',
                                           9969,
                                           location_id = c(179, 180, 190, 196),
                                           sex_id=1,
                                           age_group_id=22,
                                           year_id=2019,
                                           measure_id = 6,
                                           release_id = 6)
incidence_counts_female <- get_model_results('epi',
                                             9969,
                                             location_id = c(179, 180, 190, 196),
                                             sex_id=2,
                                             age_group_id=22,
                                             year_id=2019,
                                             measure_id = 6,
                                             release_id = 6)
incidence_counts_male <- incidence_counts_male[, .(location_id, year_id, mean)]
incidence_counts_female <- incidence_counts_female[, .(location_id, year_id, mean)]
pop_male <- get_population(location_id=c(179, 180, 190, 196),
                           age_group_id=22,
                           year_id=2019,
                           sex_id=1,
                           gbd_round_id=6,
                           decomp_step='step5')
pop_female <- get_population(location_id=c(179, 180, 190, 196),
                             age_group_id=22,
                             year_id=2019,
                             sex_id=2,
                             gbd_round_id=6,
                             decomp_step='step5')
incidence_counts_male <- merge(incidence_counts_male, pop_male,
                               by = c("location_id", "year_id"))
incidence_counts_male[, TB_male := population*mean]
incidence_counts_female <- merge(incidence_counts_female, pop_female,
                                 by = c("location_id", "year_id"))
incidence_counts_female[, TB_female := population*mean]
incidence_counts_male <- incidence_counts_male[, .(location_id, year_id, TB_male)]
incidence_counts_female <- incidence_counts_female[, .(location_id, year_id, TB_female)]
incidence_counts <- merge(incidence_counts_male, incidence_counts_female,
                          by = c("location_id", "year_id"))
incidence_counts[, TB := TB_male+TB_female]

## Get HIV-DS incident cases
hivds_incidence_counts_male <- get_model_results('epi',
                                                 10832,
                                                 location_id = c(179, 180, 190, 196),
                                                 sex_id=1,
                                                 age_group_id=22,
                                                 year_id=2019,
                                                 measure_id = 6,
                                                 release_id = 6)
hivds_incidence_counts_female <- get_model_results('epi',
                                                   10832,
                                                   location_id = c(179, 180, 190, 196),
                                                   sex_id=2,
                                                   age_group_id=22,
                                                   year_id=2019,
                                                   measure_id = 6,
                                                   release_id = 6)
hivds_incidence_counts_male <- hivds_incidence_counts_male[, .(location_id, year_id, mean)]
hivds_incidence_counts_female <- hivds_incidence_counts_female[, .(location_id, year_id, mean)]
hivds_incidence_counts_male <- merge(hivds_incidence_counts_male, pop_male,
                                     by = c("location_id", "year_id"))
hivds_incidence_counts_male[, hivds_male := population*mean]
hivds_incidence_counts_female <- merge(hivds_incidence_counts_female, pop_female,
                                       by = c("location_id", "year_id"))
hivds_incidence_counts_female[, hivds_female := population*mean]
hivds_incidence_counts_male <- hivds_incidence_counts_male[, .(location_id, year_id, hivds_male)]
hivds_incidence_counts_female <- hivds_incidence_counts_female[, .(location_id, year_id, hivds_female)]
hivds_incidence_counts <- merge(hivds_incidence_counts_male, hivds_incidence_counts_female,
                                by = c("location_id", "year_id"))
hivds_incidence_counts[, hivds := hivds_male+hivds_female]

## Get HIV-MDR incident cases
hivmdr_incidence_counts_male <- get_model_results('epi',
                                                  10833,
                                                  location_id = c(179, 180, 190, 196),
                                                  sex_id=1,
                                                  age_group_id=22,
                                                  year_id=2019,
                                                  measure_id = 6,
                                                  release_id = 6)
hivmdr_incidence_counts_female <- get_model_results('epi',
                                                    10833,
                                                    location_id = c(179, 180, 190, 196),
                                                    sex_id=2,
                                                    age_group_id=22,
                                                    year_id=2019,
                                                    measure_id = 6,
                                                    release_id = 6)
hivmdr_incidence_counts_male <- hivmdr_incidence_counts_male[, .(location_id, year_id, mean)]
hivmdr_incidence_counts_female <- hivmdr_incidence_counts_female[, .(location_id, year_id, mean)]
hivmdr_incidence_counts_male <- merge(hivmdr_incidence_counts_male, pop_male,
                                      by = c("location_id", "year_id"))
hivmdr_incidence_counts_male[, hivmdr_male := population*mean]
hivmdr_incidence_counts_female <- merge(hivmdr_incidence_counts_female, pop_female,
                                        by = c("location_id", "year_id"))
hivmdr_incidence_counts_female[, hivmdr_female := population*mean]
hivmdr_incidence_counts_male <- hivmdr_incidence_counts_male[, .(location_id, year_id, hivmdr_male)]
hivmdr_incidence_counts_female <- hivmdr_incidence_counts_female[, .(location_id, year_id, hivmdr_female)]
hivmdr_incidence_counts <- merge(hivmdr_incidence_counts_male, hivmdr_incidence_counts_female,
                                 by = c("location_id", "year_id"))
hivmdr_incidence_counts[, hivmdr := hivmdr_male+hivmdr_female]

## Get HIV-XDR incident cases
hivxdr_incidence_counts_male <- get_model_results('epi',
                                                  10834,
                                                  location_id = c(179, 180, 190, 196),
                                                  sex_id=1,
                                                  age_group_id=22,
                                                  year_id=2019,
                                                  measure_id = 6,
                                                  release_id = 6)
hivxdr_incidence_counts_female <- get_model_results('epi',
                                                    10834,
                                                    location_id = c(179, 180, 190, 196),
                                                    sex_id=2,
                                                    age_group_id=22,
                                                    year_id=2019,
                                                    measure_id = 6,
                                                    release_id = 6)
hivxdr_incidence_counts_male <- hivxdr_incidence_counts_male[, .(location_id, year_id, mean)]
hivxdr_incidence_counts_female <- hivxdr_incidence_counts_female[, .(location_id, year_id, mean)]
hivxdr_incidence_counts_male <- merge(hivxdr_incidence_counts_male, pop_male,
                                      by = c("location_id", "year_id"))
hivxdr_incidence_counts_male[, hivxdr_male := population*mean]
hivxdr_incidence_counts_female <- merge(hivxdr_incidence_counts_female, pop_female,
                                        by = c("location_id", "year_id"))
hivxdr_incidence_counts_female[, hivxdr_female := population*mean]
hivxdr_incidence_counts_male <- hivxdr_incidence_counts_male[, .(location_id, year_id, hivxdr_male)]
hivxdr_incidence_counts_female <- hivxdr_incidence_counts_female[, .(location_id, year_id, hivxdr_female)]
hivxdr_incidence_counts <- merge(hivxdr_incidence_counts_male, hivxdr_incidence_counts_female,
                                 by = c("location_id", "year_id"))
hivxdr_incidence_counts[, hivxdr := hivxdr_male+hivxdr_female]

## Calculate case detection rate
case <- merge(notification_count, incidence_counts,
              by = c("location_id", "year_id"))
case <- merge(incidence_counts, hivds_incidence_counts,
              by = c("location_id", "year_id"))
case <- merge(case, hivmdr_incidence_counts,
              by = c("location_id", "year_id"))
case <- merge(case, hivxdr_incidence_counts,
              by = c("location_id", "year_id"))
case[, tb := TB + hivds + hivmdr + hivxdr]
case[, hivtb := hivds + hivmdr + hivxdr]

##
pop <- get_population(location_id=c(179, 180, 190, 196),
                      age_group_id=22,
                      year_id=2019,
                      sex_id=3,
                      gbd_round_id=6,
                      decomp_step='step5')

