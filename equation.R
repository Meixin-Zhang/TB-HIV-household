Kenya_data_spreadsheet <- read.csv("~/OneDrive - UW/TB-HIV copy/1_8/hhcdata.csv")
lpos <- read.csv("~/OneDrive - UW/TB-HIV copy/1_8/lpos.csv")
Ipos_Susanne <- read.csv("~/OneDrive - UW/TB-HIV copy/1_8/Ipos_Susanne.csv")
library(tidyverse)

# calculate the number of HIV infected adults in TB affected households (column N * column HIV_pre)
for(i in 1:nrow(hhcdata)){
  hhcdata$HIV_positive[i] <- 
  hhcdata$N[i] * hhcdata$HIV_pre[i]
}

# calculate the HIV prevalence in adults in TB affected households
for (country in c("Kenya","Uganda","Ethiopia","SouthAfrica")) {
  assign(paste0("HIVpre",country), with(subset(hhcdata, Country==country), sum(HIV_positive)/sum(N)))
}

# calculate the N of HIV+ in TB affected households by countries
for (country in c("Kenya","Uganda","Ethiopia","SouthAfrica")) {
  assign(paste0("NHIV",country), with(subset(hhcdata, Country==country), sum(HIV_positive)))
}

# The linear relationship between lpos and HIV prevalence
lm_lpos<-lm(lpos$lpos~lpos$HIV_population_prevalence...)
summary(lm_lpos) # lpos=43.7406-1.3450*HIVpre
plot(Ipos_Susanne$HIV_population_prevalence...,Ipos_Susanne$lpos, xlab = "HIV prevalence", ylab = "Ipos")
lm_ipos<-lm(Ipos_Susanne$lpos~Ipos_Susanne$HIV_population_prevalence...)
summary(lm_ipos) # Ipos=46.5813 - 1.4836*HIVpre
abline(lm_ipos)

# incorporate HIVpre into the linear model
HIVpredata<-data.frame(predict(lm_ipos,newdata = data.frame(
  HIV_population_prevalence...=c(HIVpreKenya, HIVpreUganda, HIVpreEthiopia, HIVpreSouthAfrica)),
  interval="confidence"))
HIVpredata$country<-c("Kenya","Uganda","Ethiopia","SouthAfrica")
HIVpredata<-HIVpredata[,c("country","fit","lwr","upr")]
HIVpredata$NHIV<-c(NHIVKenya, NHIVUganda, NHIVEthiopia, NHIVSouthAfrica)
for (i in 1:nrow(HIVpredata)) {
  HIVpredata$NSDC[i]<-
    HIVpredata$fit[i]*HIVpredata$NHIV[i]/100
  HIVpredata$NSDC_lw[i]<-
    HIVpredata$lwr[i]*HIVpredata$NHIV[i]/100
  HIVpredata$NSDC_up[i]<-
    HIVpredata$upr[i]*HIVpredata$NHIV[i]/100
}
