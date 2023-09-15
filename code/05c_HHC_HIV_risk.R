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
install.packages("metafor", lib = "/homes/mzhang25/rlibs")
library(metafor, lib.loc="/homes/mzhang25/rlibs/")
library(readxl)
library(data.table)

#############################################################################################
###                               HELPER OBJECTS AND FUNCTIONS                            ###
#############################################################################################

## HELPER OBJECTS
main_dir    <- paste0(j, "temp/TB/mzhang25/TB_HHC/data/lit_review/")

dt <- as.data.table(read_excel(paste0(main_dir, "HHC_vs_national.xlsx")))
dt <- dt[age_start >= 14, ]
#setnames(dt, old = c("cases", "sample_size", "cases_ref", "sample_size_ref"), new = c())
dt_UGA <- dt[location_id == 190,]
dt_ZAF <- dt[location_id == 196,]
dt_KEN <- dt[location_id == 180,]
dt_ETH <- dt[location_id == 179,]

## Kenya
m.KEN <- meta::metabin (cases,
                  sample_size,
                  cases_ref,
                  sample_size_ref,
                  studlab = paste(author),
                  data = dt_KEN,
                  sm = "RR")
m.KEN
p.KEN <- forest(m.KEN)
funnel(m.KEN)

## Uganda
m.UGA <- meta::metabin (cases,
                  sample_size,
                  cases_ref,
                  sample_size_ref,
                  studlab = paste(author),
                  data = dt_UGA,
                  sm = "RR")
m.UGA
p.UGA <- forest(m.UGA)
funnel(m.UGA)


## South Africa
m.ZAF <- meta::metabin (cases,
                  sample_size,
                  cases_ref,
                  sample_size_ref,
                  studlab = paste(author),
                  data = dt_ZAF,
                  sm = "RR")
m.ZAF
p.ZAF <- forest(m.ZAF)
funnel(m.ZAF)

# BUILD RR TABLE
KEN_rr <- data.table(location_id = 180, ratio = 4.12, lower = 1.50, upper = 11.31)
UGA_rr <- data.table(location_id = 190, ratio = 1.86, lower = 1.54, upper = 2.23)
ZAF_rr <- data.table(location_id = 196, ratio = 1.14, lower = 0.72, upper = 1.81)
rr <- rbind(KEN_rr, UGA_rr, ZAF_rr)
rr[, sd := (upper-lower)/(2*1.96)]
for (n in 1:nrow(rr)) {
  rr[n, paste0("draw_", 0:999) := lapply(0:999, function(x) { rnorm(1000, mean = rr[n,ratio], sd = rr[n,sd])[x+1]})]
}

write.csv(rr, "/home/j/temp/TB/mzhang25/TB_HHC/data/lit_review/rr_draws.csv")
