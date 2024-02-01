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
library(ggplot2)
library(lattice)
library(tidyverse)
library(RColorBrewer)
library(reshape)
library(reshape2)
library(data.table)
library(msm)
library(dplyr)

## HELPER OBJECTS
main_dir <- paste0(h, "TB-HIV-household/data/")

#############################################################################################
###                                         IPOS clean                                    ###
#############################################################################################
# manually calculated ipos
ipos <- as.data.table(read.csv(paste0(main_dir, "ipos_manually_calculated.csv")))
ipos_mc<-ipos[Year>2006,]
ipos_mc<- ipos_mc[complete.cases(ipos_mc), ]
ipos_mc<-ipos_mc[order(ipos_mc$HIVpre),]
ipos_mc[, ipos := ipos*100]
ipos_mc[, HIVpre := 2*ceiling(HIVpre*10/2)/10]

#############################################################################################
###                                         IPOS determine                                ###
#############################################################################################

# deductive  approach
# step 1
# create dataframe with alpha from 0.1-0.9
p <- seq(0,1,0.002) # HIV prevalence
df_ipos <- as.data.frame(p)
step1alphas<-seq(0.1,0.9,0.1)
# create ipos when alpha = i
for (i in seq(1,9,1)){
  df_ipos[,i+1]<-(p * (1 - p))**step1alphas[i]/p
}
for (i in seq(1,9,1)){
  colnames(df_ipos)[i+1]<-paste0("alpha",as.character(step1alphas[i]))
}
for (i in seq(2,10,1)){
  df_ipos[,i]<-df_ipos[,i]/df_ipos[2,i]*100
}
df_ipos$p<-df_ipos$p*100

# draw line in the plot
plot(ipos_mc$HIVpre,
     ipos_mc$ipos,
     xlab = "HIV prevalence", ylab = "Ipos",
     xlim = c(0,25),
     ylim = c(0,100))
cols<-brewer.pal(9,'Set3')
for (i in seq(2,10,1)) {
  lines(df_ipos[df_ipos$p>0.1,]$p, df_ipos[df_ipos$p>0.1,i],col=cols[i-1], lwd=2)
}
legend("topright",legend=c(colnames(df_ipos[,-1])),
       col=cols, lty=1, cex=0.8)

# step 1 calculate the least square
ipos_ls <- ipos_mc[, .(HIVpre, ipos)]
setnames(ipos_ls, old = c("HIVpre", "ipos"), new = c("p", "raw_ipos"))
df_ipos$p <- round(df_ipos$p, 1)
ipos_ls <- merge(ipos_ls, df_ipos)
ipos_ls[, paste0("alpha", (1:9)/10) := lapply((1:9)/10, function(x) { raw_ipos - get(paste0("alpha", x))})]
ipos_ls[, paste0("alpha", (1:9)/10) := lapply((1:9)/10, function(x) { get(paste0("alpha", x)) * get(paste0("alpha", x))})]
ipos_ls[, paste0("alpha", (1:9)/10) := lapply((1:9)/10, function(x) { sum(get(paste0("alpha", x)))})]

## Step 2: find least square between 0.7 and 0.9
step2alphas<-seq(0.72,0.88,0.02)
df_ipos <- as.data.frame(p)
for (i in seq(1,9,1)){
  df_ipos[,i+1]<-(p * (1 - p))**step2alphas[i]/p
}
for (i in seq(1,9,1)){
  colnames(df_ipos)[i+1]<-paste0("alpha",as.character(step2alphas[i]))
}
for (i in seq(2,10,1)){
  df_ipos[,i]<-df_ipos[,i]/df_ipos[2,i]*100
}
df_ipos$p<-df_ipos$p*100

# plot
plot(ipos_mc$HIVpre,
     ipos_mc$ipos,
     xlab = "HIV prevalence", ylab = "Ipos",
     xlim = c(0,25),
     ylim = c(0,100),cex.lab=1.5, cex.axis=2, cex.main=2, cex.sub=1.5)

for (i in seq(1,9,1)) {
  lines(df_ipos[df_ipos$p>0.2,]$p, df_ipos[df_ipos$p>0.2,i+1],col=cols[i], lwd=2)
}
legend("topright",legend=c(colnames(df_ipos)[seq(2,10,1)]),
       col=cols, lty=1, cex=0.8, pt.cex = 1.5)

# least square
ipos_ls2 <- ipos_mc[, .(HIVpre, ipos)]
setnames(ipos_ls2, old = c("HIVpre", "ipos"), new = c("p", "raw_ipos"))
df_ipos$p <- round(df_ipos$p, 1)
ipos_ls2 <- merge(ipos_ls2, df_ipos)
ipos_ls2[, paste0("alpha", (36:44)/50) := lapply((36:44)/50, function(x) { raw_ipos - get(paste0("alpha", x))})]
ipos_ls2[, paste0("alpha", (36:44)/50) := lapply((36:44)/50, function(x) { get(paste0("alpha", x)) * get(paste0("alpha", x))})]
ipos_ls2[, paste0("alpha", (36:44)/50) := lapply((36:44)/50, function(x) { sum(get(paste0("alpha", x)))})]

#############################################################################################
###                               STOP TO RUN PYTHON                        ###
#############################################################################################
## After python
## alpha: 0.829 [0.824 0.834]
## create table of HIV prevalence, and ipos with 95% CI
p <- seq(0,1,0.002) # HIV prevalence
step3alphas<-c(0.824,0.829,0.834) # lower ci, mean, higher ci
ipos_ci <- as.data.frame(p)
for (i in c(1,2,3)){
  ipos_ci[,i+1]<-(p * (1 - p))**step3alphas[i]/p
}

for (i in c(2,3,4)){
  ipos_ci[,i]<-ipos_ci[,i]/ipos_ci[2,i]*100
}
ipos_ci$p<-ipos_ci$p*100

colnames(ipos_ci) <- c("hiv_prev", "ipos_lower", "ipos_mean","ipos_upper")
ipos_ci$hiv_prev <- round(ipos_ci$hiv_prev, 1)

#############################################################################################
###                               IPOS + hiv prev in tb affected hh                       ###
#############################################################################################
hiv_hhc <- read.dta13(paste0(main_dir, "/prevalence_scenario/HIV_prev_TB_HH_draws.dta"))
# clean HIV prevalence data
hiv_hhc <- hiv_hhc[hiv_hhc$age_group_id==0,]
hiv_hhc <- hiv_hhc%>%select(-contains(c("hiv_prev")))
hiv_hhc_copy <- as.data.table(hiv_hhc)
hiv_hhc_copy[, age_group_id:= NULL][, sex_id := NULL][, measurement := NULL]
hiv_hhc_copy <- hiv_hhc_copy[, -c(2:2001)]
hiv_hhc_copy <- as.data.frame(hiv_hhc_copy)
for (i in seq(2,1001,1)){
  hiv_hhc_copy[, i] <- 2*round(hiv_hhc_copy[, i]*1000/2)/10
}
hiv_hhc.long <- melt(hiv_hhc_copy, id.vars = "location_id")
colnames(hiv_hhc.long) <- c("location_id", "draw", "hiv_prev")
hiv_hhc.long <- as.data.table(hiv_hhc.long)
hiv_hhc.long[hiv_prev == 0, hiv_prev := 0.2]
hiv_hhc.long[hiv_prev >= 100.0, hiv_prev := 99.8]
hiv_hhc.long[, hiv_prev := round(hiv_prev, digits = 1)]

# merge on ipos
hiv_hhc.long <- merge(hiv_hhc.long, ipos_ci)
for (i in seq(1,4000,1)){
  hiv_hhc.long[i, ipos := rtnorm(1,
                              hiv_hhc.long[i, ]$ipos_mean,
                              lower = hiv_hhc.long[i, ]$ipos_lower,
                              upper = hiv_hhc.long[i, ]$ipos_upper)]
}
hiv_hhc.long[, ipos := ipos/100]
hiv_hhc.long <- hiv_hhc.long[, .(location_id, draw, ipos)]
hiv_hhc.wide <- reshape(hiv_hhc.long, idvar = "location_id", timevar = "draw", direction = "wide")
names(hiv_hhc.wide) <- gsub("\\.", "", names(hiv_hhc.wide))
hiv_hhc <- cbind(hiv_hhc, hiv_hhc.wide)
write.csv(hiv_hhc, paste0(main_dir, "/prevalence_scenario/sero_dis_cp_draws.csv"), row.names = F)

# hiv_hhc.long <- hiv_hhc.long <- melt(hiv_hhc.wide, id.vars = "location_id")
# hiv_hhc.long <- hiv_hhc.long%>%group_by(location_id) %>%
#  mutate(ipos_mean = mean(ipos),
#         ipos_lower= quantile(ipos, probs = 0.975),
#         ipos_upper= quantile(ipos, probs = 0.025))

#############################################################################################
###                        IPOS + hiv prev in tb affected hh (incidence scenario)         ###
#############################################################################################
hiv_hhc_inc <- as.data.table(read.dta13(paste0(main_dir, "/incidence_scenario/HIV_prev_TB_HH_draws_incident.dta")))
# clean HIV prevalence data
hiv_hhc_inc <- hiv_hhc_inc[hiv_hhc_inc$age_group_id==0,]
hiv_hhc_inc_copy <- hiv_hhc_inc
hiv_hhc_inc_copy[, age_group_id:= NULL][, sex_id := NULL][, measurement := NULL][, v1 := NULL][, population := NULL]
hiv_hhc_inc_copy <- hiv_hhc_inc_copy[, -paste0("hiv_", 0:999)]
hiv_hhc_inc_copy <- hiv_hhc_inc_copy[, -paste0("pop_", 0:999)]
hiv_hhc_inc_copy <- as.data.frame(hiv_hhc_inc_copy)
for (i in seq(2,1001,1)){
  hiv_hhc_inc_copy[, i] <- 2*round(hiv_hhc_inc_copy[, i]*1000/2)/10
}
hiv_hhc_inc.long <- melt(hiv_hhc_inc_copy, id.vars = "location_id")
colnames(hiv_hhc_inc.long) <- c("location_id", "draw", "hiv_prev")
hiv_hhc_inc.long <- as.data.table(hiv_hhc_inc.long)
hiv_hhc_inc.long[hiv_prev == 0, hiv_prev := 0.2]
hiv_hhc_inc.long[hiv_prev >= 100.0, hiv_prev := 99.8]
hiv_hhc_inc.long[, hiv_prev := round(hiv_prev, digits = 1)]

##merge on ipos
hiv_hhc_inc.long <- merge(hiv_hhc_inc.long, ipos_ci)
for (i in seq(1,4000,1)){
  hiv_hhc_inc.long[i, ipos := rtnorm(1,
                                 hiv_hhc_inc.long[i, ]$ipos_mean,
                                 lower = hiv_hhc_inc.long[i, ]$ipos_lower,
                                 upper = hiv_hhc_inc.long[i, ]$ipos_upper)]
}
hiv_hhc_inc.long[, ipos := ipos/100]
hiv_hhc_inc.long <- hiv_hhc_inc.long[, .(location_id, draw, ipos)]
hiv_hhc_inc.wide <- reshape(hiv_hhc_inc.long, idvar = "location_id", timevar = "draw", direction = "wide")
names(hiv_hhc_inc.wide) <- gsub("\\.", "", names(hiv_hhc_inc.wide))
hiv_hhc_inc <- cbind(hiv_hhc_inc, hiv_hhc_inc.wide)
hiv_hhc_inc[, sex_id := 3][, age_group_id := 0]
write.csv(hiv_hhc_inc, paste0(main_dir, "/incidence_scenario/sero_dis_cp_draws_inc.csv"), row.names = F)
