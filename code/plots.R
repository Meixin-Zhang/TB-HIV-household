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

library(readstata13)
library(data.table)
library(ggplot2)

#############################################################################################
###                 HIV national prev vs. HIV prev in TB affected HH                      ###
#############################################################################################

## HELPER FUNCTIONS
main_dir <- paste0(j, "temp/TB/mzhang25/TB_HHC/data/")

## HIV PREV IN TB AFFECTED HOUSEHOLDS
hiv_hhc <- as.data.table(read.dta13(paste0(main_dir, "HIV_prev_TB_HH_summary.dta")))
hiv_hhc[, measurement := "HIV+ in TB affected HH"]

## HIV NATIONAL PREV 
hiv_national <- as.data.table(read.csv(paste0(main_dir, "hiv_prev_national_ci.csv")))
setnames(hiv_national, old = "val", new = "mean")
hiv_national[, `:=` (sample_size=NULL, hiv_positive=NULL)]
hiv_national[, measurement := "National HIV prev"]

## Append
dt <- rbind(hiv_hhc, hiv_national)
dt[sex_id==1, sex := "Male"][sex_id==2, sex := "Female"]
dt[age_group_id==149, age_group_name := "15-24"][age_group_id==150, age_group_name := "25-34"][age_group_id==151, age_group_name := "35-44"][age_group_id==14, age_group_name := "45-49"]
dt[, `:=` (mean := round(mean*100, 2), lower = lower*100, upper = upper*100)]

## PLOTS
pdf(paste0(main_dir, "/hiv_prev_comparison.pdf"), width = 14, height = 10)
  for (location in unique(dt$location_id)) {
    p <- ggplot(dt[location_id==location,], aes(x=age_group_name, y=mean, fill=measurement))+
      geom_bar(stat="identity", width=.7, position = "dodge")+
      geom_text(aes(label=mean), vjust=0, size=4.5, position = position_dodge(width = .9)) +
      # geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, position=position_dodge(.9))+
      facet_wrap( ~ sex)+
      ggtitle(location)+
      xlab("Age group")+
      ylab("HIV positive (%)")
    print(p)
  }
dev.off()

## Incidence scenario
hiv_hhc_inc <- as.data.table(read.dta13(paste0(main_dir, "HIV_prev_TB_HH_summary_incident.dta")))
hiv_hhc_inc[, measurement := "HIV+ in TB affected HH (incidence)"]
dt2 <- rbind(hiv_hhc, hiv_hhc_inc)
dt2[sex_id==1, sex := "Male"][sex_id==2, sex := "Female"]
dt2[age_group_id==149, age_group_name := "15-24"][age_group_id==150, age_group_name := "25-34"][age_group_id==151, age_group_name := "35-44"][age_group_id==14, age_group_name := "45-49"]
dt2[, `:=` (mean = round(mean*100, 2), lower = lower*100, upper = upper*100)]

## PLOTS
pdf(paste0(main_dir, "/hiv_prev_comparison2.pdf"), width = 14, height = 10)
for (location in unique(dt$location_id)) {
  p <- ggplot(dt2[location_id==location,], aes(x=age_group_name, y=mean, fill=measurement))+
    geom_bar(stat="identity", width=.7, position = "dodge")+
    geom_text(aes(label=mean), vjust=0, size=4.5, position = position_dodge(width = .9)) +
    # geom_errorbar(aes(ymin=lower, ymax=upper), width=.1, position=position_dodge(.9))+
    facet_wrap( ~ sex)+
    ggtitle(location)+
    xlab("Age group")+
    ylab("HIV positive (%)")
  print(p)
}
dev.off()

#############################################################################################
###                                         forest plot                                   ###
#############################################################################################
# forest plot
install.packages("forestplot")
library(forestplot)
# confidence interval
Ochom.tab <- matrix(c(30,183,158,2131), nrow = 2, byrow = TRUE)
epi.2by2(dat = Ochom.tab, method = "cross.sectional", conf.level = 0.95,
         units = 100, outcome = "as.columns")
Shapiro.tab <- matrix(c(373,1402,146,563), nrow = 2, byrow = TRUE)
epi.2by2(dat = Shapiro.tab, method = "cross.sectional", conf.level = 0.95,
         units = 100, outcome = "as.columns")
Thind.tab <- matrix(c(372,586,107,499), nrow = 2, byrow = TRUE)
epi.2by2(dat = Thind.tab, method = "cross.sectional", conf.level = 0.95,
         units = 100, outcome = "as.columns")
PageShipp.tab <- matrix(c(424,931,202,792), nrow = 2, byrow = TRUE)
epi.2by2(dat = PageShipp.tab, method = "cross.sectional", conf.level = 0.95,
         units = 100, outcome = "as.columns")
Velen.tab <- matrix(c(134,278,202,792), nrow = 2, byrow = TRUE)
epi.2by2(dat = Velen.tab, method = "cross.sectional", conf.level = 0.95,
         units = 100, outcome = "as.columns")
Odera.tab <- matrix(c(16,159,21,528), nrow = 2, byrow = TRUE)
epi.2by2(dat = Odera.tab, method = "cross.sectional", conf.level = 0.95,
         units = 100, outcome = "as.columns")
Summary.tab <- matrix(c(1349,3539,1672,5305), nrow = 2, byrow = TRUE)
epi.2by2(dat = Summary.tab, method = "cross.sectional", conf.level = 0.95,
         units = 100, outcome = "as.columns")

HIV_HHC_PR <-
  structure(list(
    mean  = c(NA, NA, 2.04, 1.02, 2.19, 1.54, 1.60, 2.39, NA, 1.15),
    lower = c(NA, NA, 1.42, 0.83, 1.82, 1.33, 1.33, 1.28, NA, 1.08),
    upper = c(NA, NA, 2.94, 1.27, 2.66, 1.78, 1.93, 4.48, NA, 1.22)),
    .Names = c("mean","lower","upper"),
    row.names = c(NA, -10L),
    class = "data.frame")

tabletext <- cbind(
  c("", "Year", "2016-2017", "2009",
    "2009", "2012-2014", "2013-2014", "2016-2017",
    NA, ""),
  c("", "Country", "Uganda", "SA",
    "SA", "SA", "SA", "Kenya",
    NA, ""),
  c("", "Study", "E. Ochom", "AE. Shapiro",
    "D. Thind", "L. Page-Shipp", "K. Velen", "S. Odera",
    NA, "Summary"),
  c("HIV prevalence", "(in TB contacts)", "14.1%", "21%",
    "38.8%", "31.3%", "32.5%", "9.1%",
    NA, NA),
  c("HIV prevalence", "(in general population)", "6.9%", "20.6%",
    "17.7%", "20.3%", "20.3%", "3.8%",
    NA, NA),
  c("", "PR", "2.04", "1.02",
    "2.19", "1.54", "1.60", "2.39",
    NA, 1.15))

tabletext <- cbind(
  c("", "Study", "E. Ochom", "AE. Shapiro",
    "D. Thind", "L. Page-Shipp", "K. Velen", "S. Odera",
    NA, "Summary"),
  c("HIV prevalence", "(in TB contacts)", "14.1%", "21%",
    "38.8%", "31.3%", "32.5%", "9.1%",
    NA, NA),
  c("HIV prevalence", "(in general population)", "6.9%", "20.6%",
    "17.7%", "20.3%", "20.3%", "3.8%",
    NA, NA),
  c("", "PR", "2.04", "1.02",
    "2.19", "1.54", "1.60", "2.39",
    NA, 1.15))

forestplot(tabletext,
           HIV_HHC_PR,new_page = TRUE,
           is.summary = c(TRUE,TRUE,rep(FALSE,7),TRUE),
           clip = c(0.1,5),
           xlog = TRUE,
           col = fpColors(box = "royalblue",
                          line = "darkblue",
                          summary = "royalblue"))

forestplot(tabletext,
           hrzl_lines = gpar(col = "#444444"),
           HIV_HHC_PR,new_page = TRUE,
           is.summary = c(TRUE,TRUE,rep(FALSE,7),TRUE),
           clip = c(0.1,3),
           xlog = TRUE,
           col = fpColors(box = "royalblue",
                          line = "darkblue",
                          summary = "royalblue"))