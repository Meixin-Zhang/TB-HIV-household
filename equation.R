Kenya_data_spreadsheet <- read.csv("~/OneDrive - UW/TB-HIV copy/1_8/Kenya_data_spreadsheet.csv")
lpos <- read.csv("~/OneDrive - UW/TB-HIV copy/1_8/lpos.csv")
library(tidyverse)

# calculate the number of HIV infected adults in TB affected households (column N * column HIV_pre)
for(i in 1:nrow(Kenya_data_spreadsheet)){
  Kenya_data_spreadsheet$HIV_positive[i] <- 
  Kenya_data_spreadsheet$N[i] * Kenya_data_spreadsheet$HIV_pre[i]
}

# calculate the HIV prevalence in adults in TB affected households
HIVpre<-sum(Kenya_data_spreadsheet$N)/sum(Kenya_data_spreadsheet$HIV_positive)

# The linear relationship between lpos and HIV prevalence
lm_lpos<-lm(lpos$lpos~lpos$HIV_population_prevalence...)
summary(lm_lpos) # lpos=43.7406-1.3450*HIVpre

# incorporate HIVpre into the linear model
