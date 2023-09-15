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

## LOAD FUNCTIONS
library(msm)
library(data.table)
library(readxl)
library(ggplot2)
source(paste0(k, "libraries/current/r/get_outputs.R"))

## SOURCE MR-BRT
repo_dir <- paste0(j, "temp/reed/prog/projects/run_mr_brt/")
source(paste0(repo_dir, "run_mr_brt_function.R"))
source(paste0(repo_dir, "cov_info_function.R"))
source(paste0(repo_dir, "check_for_outputs_function.R"))
source(paste0(repo_dir, "load_mr_brt_outputs_function.R"))
source(paste0(repo_dir, "predict_mr_brt_function.R"))
source(paste0(repo_dir, "check_for_preds_function.R"))
source(paste0(repo_dir, "load_mr_brt_preds_function.R"))
source(paste0(repo_dir, "plot_mr_brt_function.R"))

#############################################################################################
###                               HELPER OBJECTS AND FUNCTIONS                            ###
#############################################################################################

## HELPER OBJECTS
main_dir    <- paste0(j, "temp/TB/mzhang25/TB_HHC/data/lit_review/")

## HELPER FUNCTION
draw_summaries <- function(x, new_col, cols_sum, se = F) {

  x[, (paste0(new_col, "_lower")) := apply(.SD, 1, quantile, probs = .025, na.rm =T), .SDcols = (cols_sum)]
  x[, (paste0(new_col, "_mean"))  := rowMeans(.SD), .SDcols = (cols_sum)]
  x[, (paste0(new_col, "_upper")) := apply(.SD, 1, quantile, probs = .975, na.rm =T), .SDcols = (cols_sum)]
  if (se == T) x[, paste0(new_col, "_se") := (get(paste0(new_col, "_upper"))-get(paste0(new_col, "_lower")))/3.92]
  if (se == T) x[get(paste0(new_col, "_se")) == 0, paste0(new_col, "_se") := apply(.SD, 1, sd, na.rm=T), .SDcols = (cols_sum)]
  x <- x[, !cols_sum, with = F]
  return(x)
}

#############################################################################################
###                                 PREP RELATIVE RISK                                    ###
#############################################################################################
dt <- as.data.table(read_excel(paste0(main_dir, "HHC_vs_national.xlsx")))
## REMOVE STUDY INCLUDE CHILDREN
dt <- dt[age_start >= 14, ]
dt[, RR_mean := (cases*sample_size_ref)/(cases_ref*sample_size)]
dt[, RR_se := sqrt((1/cases_ref)+(1/cases)-(1/sample_size)-(1/sample_size_ref))]

#############################################################################################
###                                  PREP FOR META-ANALYSIS                               ###
#############################################################################################
## LOG TRANSFORM MEAN
dt[, rr := log(RR_mean)]

## SPLIT COUNTRY
dt_UGA <- dt[location_id == 190,]
dt_ZAF <- dt[location_id == 196,]
dt_KEN <- dt[location_id == 180,]
dt_ETH <- dt[location_id == 179,]

## LOG TRANSFORM SE
dt_UGA$rr_se<- sapply(1:nrow(dt_UGA), function(i) {
  rr_i    <- dt[i, RR_mean]
  rr_se_i <- dt[i, RR_se]
  deltamethod(~log(x1), rr_i, rr_se_i^2)
})

dt_ZAF$rr_se<- sapply(1:nrow(dt_ZAF), function(i) {
  rr_i    <- dt[i, RR_mean]
  rr_se_i <- dt[i, RR_se]
  deltamethod(~log(x1), rr_i, rr_se_i^2)
})

dt_KEN$rr_se<- sapply(1:nrow(dt_KEN), function(i) {
  rr_i    <- dt[i, RR_mean]
  rr_se_i <- dt[i, RR_se]
  deltamethod(~log(x1), rr_i, rr_se_i^2)
})

dt_ETH$rr_se<- sapply(1:nrow(dt_ETH), function(i) {
  rr_i    <- dt[i, RR_mean]
  rr_se_i <- dt[i, RR_se]
  deltamethod(~log(x1), rr_i, rr_se_i^2)
})

#############################################################################################
###                           RUN UGA META-ANALYSIS IN MR-BRT                             ###
#############################################################################################
## FIT THE MODEL
fit1 <- run_mr_brt(
  output_dir  = main_dir,
  model_label = "UGA",
  data        = dt_UGA,
  mean_var    = "rr",
  se_var      = "rr_se",
  #  method      = "trim_maxL",
  study_id    = "author",
  #  trim_pct    = 0.10,
  overwrite_previous = TRUE
)

## CHECK FOR OUTPUTS
check_for_outputs(fit1)

## START PREDICTIONS
preds <- data.table(acause = "HHC")

## START PREDICTION
pred1 <- predict_mr_brt(fit1, newdata = preds, write_draws = T)

## CHECK FOR PREDICTIONS
check_for_preds(pred1)
pred_object <- load_mr_brt_preds(pred1)

## GET PREDICTION DRAWS
draws <- as.data.table(pred_object$model_draws)
draws[, paste0("draw_", 0:999) := lapply(0:999, function(x) exp(get(paste0("draw_", x))))]

## COMPUTE MEAN AND CI OF PREDICTION DRAWS
pred_summaries <- copy(draws)
pred_summaries <- draw_summaries(pred_summaries, "pred",  paste0("draw_", 0:999), T)
print(pred_summaries)

## FORMAT DRAWS
preds <- cbind(preds, draws)
preds[, `:=` (X_intercept = NULL, Z_intercept = NULL)]
setnames(preds, old = paste0("draw_", 0:999), new = paste0("rr_", 0:999))

## SAVE DRAWS
write.csv(draws, "/home/j/temp/TB/mzhang25/TB_HHC/data/lit_review/UGA_rr_draws.csv", row.names = F, na = "")

## COMPUTE PLOT
plot_mr_brt(fit1, continuous_vars = "intercept", dose_vars = "intercept")

#############################################################################################
###                                    FOREST PLOT-UGA                                    ###
#############################################################################################

## LOAD IN DATA
mod_data <- as.data.table(fit1$train_data)
mod_data[, RR_upper := RR_mean + 1.96*RR_se][, RR_lower := RR_mean - 1.96*RR_se]

## MAKE THE PLOT
f <- ggplot(mod_data, aes(ymax = RR_upper, ymin = RR_lower)) +
  geom_point(aes(y = RR_mean, x = author)) + geom_errorbar(aes(x = author), width=0) +
  theme_bw() + labs(x = "", y = "Relative Risk") + coord_flip() +
  ggtitle(paste0("MR-BeRT Meta Analysis - RR: ", round(pred_summaries$pred_mean, 4), " (", round(pred_summaries$pred_lower, 4), " to ", round(pred_summaries$pred_upper, 4), ")")) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = pred_summaries$pred_mean, col = "purple") +
  geom_rect(data = pred_summaries, aes(ymin = pred_lower, ymax = pred_upper, xmin = 0, xmax = length(mod_data$author)+1), alpha=0.2, fill="purple")

## PRINT THE FOREST PLOT
print(f)

#############################################################################################
###                           RUN ZAF META-ANALYSIS IN MR-BRT                             ###
#############################################################################################
## FIT THE MODEL
fit1 <- run_mr_brt(
  output_dir  = main_dir,
  model_label = "ZAF",
  data        = dt_ZAF,
  mean_var    = "rr",
  se_var      = "rr_se",
  #  method      = "trim_maxL",
  study_id    = "author",
  #  trim_pct    = 0.10,
  overwrite_previous = TRUE
)

## CHECK FOR OUTPUTS
check_for_outputs(fit1)

## START PREDICTIONS
preds <- data.table(acause = "HHC")

## START PREDICTION
pred1 <- predict_mr_brt(fit1, newdata = preds, write_draws = T)

## CHECK FOR PREDICTIONS
check_for_preds(pred1)
pred_object <- load_mr_brt_preds(pred1)

## GET PREDICTION DRAWS
draws <- as.data.table(pred_object$model_draws)
draws[, paste0("draw_", 0:999) := lapply(0:999, function(x) exp(get(paste0("draw_", x))))]

## COMPUTE MEAN AND CI OF PREDICTION DRAWS
pred_summaries <- copy(draws)
pred_summaries <- draw_summaries(pred_summaries, "pred",  paste0("draw_", 0:999), T)
print(pred_summaries)

## FORMAT DRAWS
preds <- cbind(preds, draws)
preds[, `:=` (X_intercept = NULL, Z_intercept = NULL)]
setnames(preds, old = paste0("draw_", 0:999), new = paste0("rr_", 0:999))

## SAVE DRAWS
write.csv(draws, "/home/j/temp/TB/mzhang25/TB_HHC/data/lit_review/ZAF_rr_draws.csv", row.names = F, na = "")

## COMPUTE PLOT
plot_mr_brt(fit1, continuous_vars = "intercept", dose_vars = "intercept")

#############################################################################################
###                                    FOREST PLOT-ZAF                                    ###
#############################################################################################

## LOAD IN DATA
mod_data <- as.data.table(fit1$train_data)
mod_data[, RR_upper := RR_mean + 1.96*RR_se][, RR_lower := RR_mean - 1.96*RR_se]

## MAKE THE PLOT
f <- ggplot(mod_data, aes(ymax = RR_upper, ymin = RR_lower)) +
  geom_point(aes(y = RR_mean, x = author)) + geom_errorbar(aes(x = author), width=0) +
  theme_bw() + labs(x = "", y = "Relative Risk") + coord_flip() +
  ggtitle(paste0("MR-BeRT Meta Analysis - RR: ", round(pred_summaries$pred_mean, 4), " (", round(pred_summaries$pred_lower, 4), " to ", round(pred_summaries$pred_upper, 4), ")")) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = pred_summaries$pred_mean, col = "purple") +
  geom_rect(data = pred_summaries, aes(ymin = pred_lower, ymax = pred_upper, xmin = 0, xmax = length(mod_data$author)+1), alpha=0.2, fill="purple")

## PRINT THE FOREST PLOT
print(f)

#############################################################################################
###                           RUN KEN META-ANALYSIS IN MR-BRT                             ###
#############################################################################################
## FIT THE MODEL
fit1 <- run_mr_brt(
  output_dir  = main_dir,
  model_label = "KEN",
  data        = dt_KEN,
  mean_var    = "rr",
  se_var      = "rr_se",
  #  method      = "trim_maxL",
  study_id    = "author",
  #  trim_pct    = 0.10,
  overwrite_previous = TRUE
)

## CHECK FOR OUTPUTS
check_for_outputs(fit1)

## START PREDICTIONS
preds <- data.table(acause = "HHC")

## START PREDICTION
pred1 <- predict_mr_brt(fit1, newdata = preds, write_draws = T)

## CHECK FOR PREDICTIONS
check_for_preds(pred1)
pred_object <- load_mr_brt_preds(pred1)

## GET PREDICTION DRAWS
draws <- as.data.table(pred_object$model_draws)
draws[, paste0("draw_", 0:999) := lapply(0:999, function(x) exp(get(paste0("draw_", x))))]

## COMPUTE MEAN AND CI OF PREDICTION DRAWS
pred_summaries <- copy(draws)
pred_summaries <- draw_summaries(pred_summaries, "pred",  paste0("draw_", 0:999), T)
print(pred_summaries)

## FORMAT DRAWS
preds <- cbind(preds, draws)
preds[, `:=` (X_intercept = NULL, Z_intercept = NULL)]
setnames(preds, old = paste0("draw_", 0:999), new = paste0("rr_", 0:999))

## SAVE DRAWS
write.csv(draws, "/home/j/temp/TB/mzhang25/TB_HHC/data/lit_review/KEN_rr_draws.csv", row.names = F, na = "")

## COMPUTE PLOT
plot_mr_brt(fit1, continuous_vars = "intercept", dose_vars = "intercept")

#############################################################################################
###                                    FOREST PLOT-KEN                                    ###
#############################################################################################

## LOAD IN DATA
mod_data <- as.data.table(fit1$train_data)
mod_data[, RR_upper := RR_mean + 1.96*RR_se][, RR_lower := RR_mean - 1.96*RR_se]

## MAKE THE PLOT
f <- ggplot(mod_data, aes(ymax = RR_upper, ymin = RR_lower)) +
  geom_point(aes(y = RR_mean, x = author)) + geom_errorbar(aes(x = author), width=0) +
  theme_bw() + labs(x = "", y = "Relative Risk") + coord_flip() +
  ggtitle(paste0("MR-BeRT Meta Analysis - RR: ", round(pred_summaries$pred_mean, 4), " (", round(pred_summaries$pred_lower, 4), " to ", round(pred_summaries$pred_upper, 4), ")")) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = pred_summaries$pred_mean, col = "purple") +
  geom_rect(data = pred_summaries, aes(ymin = pred_lower, ymax = pred_upper, xmin = 0, xmax = length(mod_data$author)+1), alpha=0.2, fill="purple")

## PRINT THE FOREST PLOT
print(f)

#############################################################################################
###                           RUN ETH META-ANALYSIS IN MR-BRT                             ###
#############################################################################################
## FIT THE MODEL
fit1 <- run_mr_brt(
  output_dir  = main_dir,
  model_label = "ETH",
  data        = dt_ETH,
  mean_var    = "rr",
  se_var      = "rr_se",
  #  method      = "trim_maxL",
  study_id    = "author",
  #  trim_pct    = 0.10,
  overwrite_previous = TRUE
)

## CHECK FOR OUTPUTS
check_for_outputs(fit1)

## START PREDICTIONS
preds <- data.table(acause = "HHC")

## START PREDICTION
pred1 <- predict_mr_brt(fit1, newdata = preds, write_draws = T)

## CHECK FOR PREDICTIONS
check_for_preds(pred1)
pred_object <- load_mr_brt_preds(pred1)

## GET PREDICTION DRAWS
draws <- as.data.table(pred_object$model_draws)
draws[, paste0("draw_", 0:999) := lapply(0:999, function(x) exp(get(paste0("draw_", x))))]

## COMPUTE MEAN AND CI OF PREDICTION DRAWS
pred_summaries <- copy(draws)
pred_summaries <- draw_summaries(pred_summaries, "pred",  paste0("draw_", 0:999), T)
print(pred_summaries)

## FORMAT DRAWS
preds <- cbind(preds, draws)
preds[, `:=` (X_intercept = NULL, Z_intercept = NULL)]
setnames(preds, old = paste0("draw_", 0:999), new = paste0("rr_", 0:999))

## SAVE DRAWS
write.csv(draws, "/home/j/temp/TB/mzhang25/TB_HHC/data/lit_review/ETH_rr_draws.csv", row.names = F, na = "")

## COMPUTE PLOT
plot_mr_brt(fit1, continuous_vars = "intercept", dose_vars = "intercept")

#############################################################################################
###                                    FOREST PLOT-UGA                                    ###
#############################################################################################

## LOAD IN DATA
mod_data <- as.data.table(fit1$train_data)
mod_data[, RR_upper := RR_mean + 1.96*RR_se][, RR_lower := RR_mean - 1.96*RR_se]

## MAKE THE PLOT
f <- ggplot(mod_data, aes(ymax = RR_upper, ymin = RR_lower)) +
  geom_point(aes(y = RR_mean, x = author)) + geom_errorbar(aes(x = author), width=0) +
  theme_bw() + labs(x = "", y = "Relative Risk") + coord_flip() +
  ggtitle(paste0("MR-BeRT Meta Analysis - RR: ", round(pred_summaries$pred_mean, 4), " (", round(pred_summaries$pred_lower, 4), " to ", round(pred_summaries$pred_upper, 4), ")")) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = pred_summaries$pred_mean, col = "purple") +
  geom_rect(data = pred_summaries, aes(ymin = pred_lower, ymax = pred_upper, xmin = 0, xmax = length(mod_data$author)+1), alpha=0.2, fill="purple")

## PRINT THE FOREST PLOT
print(f)



