### Atlantis Skill assessment script
# Code necessary for PlosBiology MS
# By: Erik Olsen
# 1/15/2015

# must import data files using skill_assess_v2.R script

#---------------------------------------------------------------
### LIBRARIES AND SOURCES

library(ggplot2)
library(reshape2)
library(data.table)
library(plyr)
library(RColorBrewer)
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")
library(grid)

# Sourcing get.Ind code
source("~/Documents/G-copy/R/get_indicators_4Erik/get_indicators_4Erik_mod.R")

#source multiplot
source("~/AtlantisNEUS_R/multiplot function.R")

#---------------------------------------------------------------
### USER DEFINED FUNCTIONS
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#-----------------------------------------------------------------
# USER DEFINED GLOBAL VARIABLES
Y1<-c(1964, 1974, 1965, 1975, 1985, 1995, 2005)
Y2<-c(2004, 2003, 1974, 1984, 1994, 2004, 2013)

#-----------------------------------------------------------------
###  Importing data
setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/skill assessment/data") 

#biomass/survey/satellite PrimProd data (normalized:  x-X/X (observed - mean/mean)
model_biom<-read.table("Modeled_Biomass_std.csv", head=TRUE, sep=",")
survey_biom<-read.table("Observed_Biomass_std.csv", head=TRUE, sep=",")
species_codes<-read.csv("NEUS_species_codes.csv")
mef<-read.table("MEF_table_initial.csv", head=TRUE, sep=",", row.names=1)
PPD<-read.csv("PPD_means.csv")

#Landings data (not normalized)
landings_model<-read.table("Modeled_Landings.csv", head=TRUE, sep=",")
landing_obs<-read.table("Observed_Landings.csv", head=TRUE, sep=",")

# importing table of skill metrics (RMSE, AE, AAE, RI, MEF) from table provided by Rob
skill_table_biomass<-read.csv("Skill_table_biomass.csv", row.names=1)
skill_table_landings<-read.table("Skill_table_landings.csv", sep=";", header=TRUE, row.names=1)

# IMPORT  detailed metrics tables 
metric_biom_all<-read.csv("metric_biom_all.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_74_03<-read.csv("metric_biom_74_03.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_65_74<-read.csv("metric_biom_65_74.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_75_84<-read.csv("metric_biom_75_84.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_85_94<-read.csv("metric_biom_85_94.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_95_04<-read.csv("metric_biom_95_04.csv",head=TRUE, sep=",", nrows=23, row.names=1)

metric_land_all<-read.csv("metric_land_all.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_land_74_03<-read.csv("metric_land_74_03.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_land_65_74<-read.csv("metric_land_65_74.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_land_75_84<-read.csv("metric_land_75_84.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_land_85_94<-read.csv("metric_land_85_94.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_land_95_04<-read.csv("metric_land_95_04.csv",head=TRUE, sep=",", nrows=23, row.names=1)

metric_ecoind_all<-read.csv("metric_ecoind_all.csv",head=TRUE, sep=",",nrows=22,  row.names=1)
metric_ecoind_74_03<-read.csv("metric_ecoind_74_03.csv",head=TRUE, sep=",", nrows=22, row.names=1)
metric_ecoind_65_74<-read.csv("metric_ecoind_65_74.csv",head=TRUE, sep=",", nrows=22, row.names=1)
metric_ecoind_75_84<-read.csv("metric_ecoind_75_84.csv",head=TRUE, sep=",", nrows=22, row.names=1)
metric_ecoind_85_94<-read.csv("metric_ecoind_85_94.csv",head=TRUE, sep=",", nrows=22, row.names=1)
metric_ecoind_95_04<-read.csv("metric_ecoind_95_04.csv",head=TRUE, sep=",", nrows=22, row.names=1)

#### Import full NEUS output table & adjusting numbers for weight
Bio_m<-read.table("neus_base.csv", head=TRUE, sep=",")
Bio_o<-read.table("Observed_Biomass.csv", head=TRUE, sep=",")
Catch_m<-read.table("Modeled_Landings.csv", head=TRUE, sep=",")
Catch_o<-read.table("Observed_Landings.csv", head=TRUE, sep=",")
#import top-pred time series from EA Report data time series, ref. S. Gaichas
## NB! Seabird biomass / numbers for model region is lacking. Only have fledgling success for a few year, but very short series. 
# Will not include seabirds in analysis, but need to import them to keep column numbers correct 
# need to convert No birds/seals/whales to mt
Top_pred<-read.table("SealsBirdsWhales.csv", head=TRUE, sep=",")
RightWhaleWeight <-60 # mt source: https://en.wikipedia.org/wiki/Right_whale
#TernWeight <- 0.0001255 # mt, source: https://en.wikipedia.org/wiki/Common_tern
#PuffinWeight <- 0.0005 # mt, source: http://www.canadiangeographic.ca/kids/animal-facts/atlantic_puffin.asp
SeaWeight <- 0.1115 # mt, source: https://en.wikipedia.org/wiki/Harbor_seal
Top_pred$WHB<-Top_pred$WHB*RightWhaleWeight
Top_pred$PIN<-Top_pred$PIN*SeaWeight
Top_pred<-subset(Top_pred, Year>1963)
# only include seals and Right Whales
Bio_o<-cbind(Bio_o, Top_pred[3:6])
# add Primary produciton to observed data file 
Bio_o$PL<-NA
for (i in 1:length(PPD$Year)) {
  Bio_o$PL[(PPD$Year[i]-1963)]<-PPD$VGPM2[i] 
  }



## give real-world species names to column heads of data tables
#biomass data
NEUS.names<-colnames(model_biom)

for (j in 2:length(NEUS.names)) {
  NEUS.names[j]<-as.character(species_codes$NAME[match(NEUS.names[j], species_codes$CODE)] )
}

colnames(model_biom)<-NEUS.names
colnames(survey_biom)<-NEUS.names

#landings data
NEUS.names.landings<-colnames(landings_model)

for (j in 2:length(NEUS.names.landings)) {
  NEUS.names.landings[j]<-as.character(species_codes$NAME[match(NEUS.names.landings[j], species_codes$CODE)] )
}

colnames(landings_model)<-NEUS.names.landings
colnames(landing_obs)<-NEUS.names.landings


## giving real-world names to rows of skill table
#biomass data
NEUS.names.rows<-rownames(skill_table_biomass)

for (j in 1:length(NEUS.names.rows)) {
  NEUS.names.rows[j]<-as.character(species_codes$NAME[match(NEUS.names.rows[j], species_codes$CODE)] )
}

rownames(mef)<-NEUS.names.rows
rownames(skill_table_biomass)<-NEUS.names.rows

#landings data
NEUS.names.rows.l<-rownames(skill_table_landings)

for (j in 1:length(NEUS.names.rows.l)) {
  NEUS.names.rows.l[j]<-as.character(species_codes$NAME[match(NEUS.names.rows.l[j], species_codes$CODE)] )
}

rownames(skill_table_landings)<-NEUS.names.rows.l

#-----------------------------------------------------------------
####  CALCULATE ECOSYSTEM INDICATORS
# Seabirds not included in TEP of TotalBio group
# changes made to get_indicators_4Erik_mod.R 
### ECOLOGICAL INDICATORS for model data
setwd("~/Documents/G-copy/R/get_indicators_4Erik")
path=getwd()
comp<-c(ncol(Bio_m)-2)
#bzero<-1
bzero<-Bio_m[1,2:comp]
EcoInd_model<-get.Ind(Bio_m[1,],Catch_m[1,],bzero,path, comp)
EcoInd_model[24]<-1964
names(EcoInd_model)[24]<-"Year"
for (i in 2:nrow(Bio_m)) {
  IndYear<-get.Ind(Bio_m[i,],Catch_m[i,],bzero,path, comp)
  IndYear[24]<-Bio_m$Year[i]
  EcoInd_model<-rbind(EcoInd_model, IndYear, deparse.level = 0)
}


#### ECOLOGICAL INDICATORS for observed / landings data
bzero<-Bio_o[1,2:28]
EcoInd_obs<-get.Ind.obs(Bio_o[1,],Catch_o[1,],bzero,path)
EcoInd_obs[24]<-1964
names(EcoInd_obs)[24]<-"Year"
for (i in 2:nrow(Bio_o)) {
  IndYear<-get.Ind.obs(Bio_o[i,],Catch_o[i,],bzero,path)
  IndYear[24]<-Bio_o$Year[i]
  EcoInd_obs<-rbind(EcoInd_obs, IndYear, deparse.level = 0)
}

setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/skill assessment/data") 
EcoInd_model<-subset(as.data.frame(EcoInd_model), Year<=2014)
save(EcoInd_model, file="EcoIndicators_modelv2.RData") 
EcoInd_obs<-subset(as.data.frame(EcoInd_obs), Year<=2014)
save(EcoInd_obs, file="EcoIndicators_observedv2.RData") 



#-----------------------------------------------------------------
### CORRELATION COEFFICIENTS
# BIOMASS: Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
IndNames<-names(model_biom)[1:23]
model<-model_biom
obs<-survey_biom


for (j in 1:length(Y1)){
  cortab_b<-data.frame(S_h=numeric(23), P_h=numeric(23),  K_h=numeric(23),  row.names=IndNames)
  
  for (i in 1:length(IndNames)){
    #need to test if the inputs are NAs
    if  (length(grep(TRUE, is.finite(subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]]))) >=1) { 
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="spearman")
      cortab_b[i,1]<-result$estimate
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="pearson")
      cortab_b[i,2]<-result$estimate
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="kendall")
      cortab_b[i,3]<-result$estimate 
    } else  { 
        cortab_b[i]<-c(NA)
    }
  }
  cortab_b<-cortab_b[2:23,]
  assign(paste("correlation_biom", Y1[j],"to", Y2[j], sep="_"), cortab_b) 
  write.csv(cortab_b, file=paste("correlation_biom", Y1[j],"to", Y2[j], ".csv", sep="_"))
}


# LANDINGS: Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
IndNames<-names(landings_model)[1:22]
model<-landings_model
obs<-landing_obs


for (j in 1:length(Y1)){
  cortab_l<-data.frame(S_h=numeric(22), P_h=numeric(22),  K_h=numeric(22),  row.names=IndNames)
  
  for (i in 1:length(IndNames)){
    #need to test if the inputs are NAs
    if  (length(grep(TRUE, is.finite(subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]]))) >=1) { 
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="spearman")
      cortab_l[i,1]<-result$estimate
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="pearson")
      cortab_l[i,2]<-result$estimate
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="kendall")
      cortab_l[i,3]<-result$estimate 
    } else  { 
      cortab_l[i,]<-c(NA)
    }
  }
  
  cortab_l<-cortab_l[2:22,]
  assign(paste("correlation_landings", Y1[j],"to", Y2[j], sep="_"), cortab_l) 
  write.csv(cortab_l, file=paste("correlation_landings", Y1[j],"to", Y2[j], ".csv", sep="_"))
}



# ECOLOGICAL INDICATORS: Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
IndNames<-names(EcoInd_obs)[1:22]
model<-EcoInd_model
obs<-EcoInd_obs


for (j in 1:length(Y1)){
  cortab_i<-data.frame(S_h=numeric(22), P_h=numeric(22),  K_h=numeric(22),  row.names=IndNames)  

    for (i in 1:length(IndNames)){
      #need to test if the inputs are NAs
      if  (length(grep(TRUE, is.finite(subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]]))) >=1) { 
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="spearman")
      cortab_i[i,1]<-result$estimate
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="pearson")
      cortab_i[i,2]<-result$estimate
      result<-cor.test(subset(model, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], subset(obs, Year>=Y1[j] & Year <=Y2[j])[,IndNames[i]], method="kendall")
      cortab_i[i,3]<-result$estimate 
      } else  { 
        cortab_i[i,]<-c(NA)
      }
    }
    assign(paste("correlation_ecoind", Y1[j],"to", Y2[j], sep="_"), cortab_i) 
    write.csv(cortab_i, file=paste("correlation_EcoInd", Y1[j],"to", Y2[j], ".csv", sep="_"))
}


#-----------------------------------------------------------------
#### IMPORT metrics tables to combine with correlation tables

# select without seabirds
metric_biom_all<-metric_biom_all[!row.names(metric_biom_all) %in% c("Birds"),]
metric_biom_74_03<-metric_biom_74_03[!row.names(metric_biom_74_03) %in% c("Birds"),]
metric_biom_65_74<-metric_biom_65_74[!row.names(metric_biom_65_74) %in% c("Birds"),]
metric_biom_75_84<-metric_biom_75_84[!row.names(metric_biom_75_84) %in% c("Birds"),]
metric_biom_85_94<-metric_biom_85_94[!row.names(metric_biom_85_94) %in% c("Birds"),]
metric_biom_95_04<-metric_biom_95_04[!row.names(metric_biom_95_04) %in% c("Birds"),]


metric_ecoind_all<-metric_ecoind_all[!row.names(metric_ecoind_all) %in% c("Birds"),]
metric_ecoind_74_03<-metric_ecoind_74_03[!row.names(metric_ecoind_74_03) %in% c("Birds"),]
metric_ecoind_65_74<-metric_ecoind_65_74[!row.names(metric_ecoind_65_74) %in% c("Birds"),]
metric_ecoind_75_84<-metric_ecoind_75_84[!row.names(metric_ecoind_75_84) %in% c("Birds"),]
metric_ecoind_85_94<-metric_ecoind_85_94[!row.names(metric_ecoind_85_94) %in% c("Birds"),]
metric_ecoind_95_04<-metric_ecoind_95_04[!row.names(metric_ecoind_95_04) %in% c("Birds"),]

# combine with correlation tables
metric_biom_pred<-cbind(metric_biom_all[1:22,c(2,4,6,8)], correlation_biom_2005_to_2013)
metric_biom_all<-cbind(metric_biom_all[1:22,c(1,3,5,7)], correlation_biom_1964_to_2004)
metric_biom_74_03<-cbind(metric_biom_74_03[1:22,c(1,3,5,7)], correlation_biom_1974_to_2003)
metric_biom_65_74<-cbind(metric_biom_65_74[1:22,c(1,3,5,7)], correlation_biom_1965_to_1974)
metric_biom_75_84<-cbind(metric_biom_75_84[1:22,c(1,3,5,7)], correlation_biom_1975_to_1984)
metric_biom_85_94<-cbind(metric_biom_85_94[1:22,c(1,3,5,7)], correlation_biom_1985_to_1994)
metric_biom_95_04<-cbind(metric_biom_95_04[1:22,c(1,3,5,7)], correlation_biom_1995_to_2004)

metric_ecoind_pred<-cbind(metric_ecoind_all[1:22,c(2,4,6,8)], correlation_ecoind_2005_to_2013[(1:22),])
metric_ecoind_all<-cbind(metric_ecoind_all[1:22,c(1,3,5,7)], correlation_ecoind_1964_to_2004[(1:22),])
metric_ecoind_74_03<-cbind(metric_ecoind_74_03[1:22,c(1,3,5,7)], correlation_ecoind_1974_to_2003[(1:22),])
metric_ecoind_65_74<-cbind(metric_ecoind_65_74[1:22,c(1,3,5,7)], correlation_ecoind_1965_to_1974[(1:22),])
metric_ecoind_75_84<-cbind(metric_ecoind_75_84[1:22,c(1,3,5,7)], correlation_ecoind_1975_to_1984[(1:22),])
metric_ecoind_85_94<-cbind(metric_ecoind_85_94[1:22,c(1,3,5,7)], correlation_ecoind_1985_to_1994[(1:22),])
metric_ecoind_95_04<-cbind(metric_ecoind_95_04[1:22,c(1,3,5,7)], correlation_ecoind_1995_to_2004[(1:22),])

metric_land_pred<-cbind(metric_land_all[1:21,c(2,4,6,8)], correlation_landings_2005_to_2013)
metric_land_all<-cbind(metric_land_all[1:21,c(1,3,5,7)], correlation_landings_1964_to_2004)
metric_land_74_03<-cbind(metric_land_74_03[1:21,c(1,3,5,7)], correlation_landings_1974_to_2003)
metric_land_65_74<-cbind(metric_land_65_74[1:21,c(1,3,5,7)], correlation_landings_1965_to_1974)
metric_land_75_84<-cbind(metric_land_75_84[1:21,c(1,3,5,7)], correlation_landings_1975_to_1984)
metric_land_85_94<-cbind(metric_land_85_94[1:21,c(1,3,5,7)], correlation_landings_1985_to_1994)
metric_land_95_04<-cbind(metric_land_95_04[1:21,c(1,3,5,7)], correlation_landings_1995_to_2004)

#give new colnames that are useful for plotting
names_hind<-c("MEF_a", "AE_a", "AAE_a", "RMSE_a", "S_a", "P_a", "K_a")
names_pred<-c("MEF_p", "AE_p", "AAE_p", "RMSE_p", "S_p", "P_p", "K_p")
names_noburn<-c("MEF_b", "AE_b", "AAE_b", "RMSE_b", "S_b", "P_b", "K_b")
names_65<-c("MEF_65", "AE_65", "AAE_65", "RMSE_65", "S_65", "P_65", "K_65")
names_75<-c("MEF_75", "AE_75", "AAE_75", "RMSE_75", "S_75", "P_75", "K_75")
names_85<-c("MEF_85", "AE_85", "AAE_85", "RMSE_85", "S_85", "P_85", "K_85")
names_95<-c("MEF_95", "AE_95", "AAE_95", "RMSE_95", "S_95", "P_95", "K_95")

colnames(metric_biom_all)<-names_hind
colnames(metric_biom_pred)<-names_pred
colnames(metric_biom_74_03)<-names_noburn
colnames(metric_biom_65_74)<-names_65
colnames(metric_biom_75_84)<-names_75
colnames(metric_biom_85_94)<-names_85
colnames(metric_biom_95_04)<-names_95

colnames(metric_ecoind_all)<-names_hind
colnames(metric_ecoind_pred)<-names_pred
colnames(metric_ecoind_74_03)<-names_noburn
colnames(metric_ecoind_65_74)<-names_65
colnames(metric_ecoind_75_84)<-names_75
colnames(metric_ecoind_85_94)<-names_85
colnames(metric_ecoind_95_04)<-names_95

colnames(metric_land_all)<-names_hind
colnames(metric_land_pred)<-names_pred
colnames(metric_land_74_03)<-names_noburn
colnames(metric_land_65_74)<-names_65
colnames(metric_land_75_84)<-names_75
colnames(metric_land_85_94)<-names_85
colnames(metric_land_95_04)<-names_95

## give real-world species names to column heads of data tables
#biomass data
biom_names<-rownames(metric_biom_all)

for (j in 1:length(biom_names)) {
  biom_names[j]<-as.character(species_codes$NAME[match(biom_names[j], species_codes$CODE)] )
}

rownames(metric_biom_pred)<-biom_names
rownames(metric_biom_all)<-biom_names
rownames(metric_biom_74_03)<-biom_names
rownames(metric_biom_65_74)<-biom_names
rownames(metric_biom_75_84)<-biom_names
rownames(metric_biom_85_94)<-biom_names
rownames(metric_biom_95_04)<-biom_names


#landings data
land_names<-rownames(metric_land_all)

for (j in 1:length(land_names)) {
  land_names[j]<-as.character(species_codes$NAME[match(land_names[j], species_codes$CODE)] )
}

rownames(metric_land_pred)<-land_names
rownames(metric_land_all)<-land_names
rownames(metric_land_74_03)<-land_names
rownames(metric_land_65_74)<-land_names
rownames(metric_land_75_84)<-land_names
rownames(metric_land_85_94)<-land_names
rownames(metric_land_95_04)<-land_names



# New DATASETs: 
  # one for each metric
MEF<-rbind(cbind(metric_biom_all[1], metric_biom_pred[1], metric_biom_74_03[1], metric_biom_65_74[1], metric_biom_75_84[1], metric_biom_85_94[1], metric_biom_95_04[1]), cbind(metric_ecoind_all[1], metric_ecoind_pred[1], metric_ecoind_74_03[1], metric_ecoind_65_74[1], metric_ecoind_75_84[1], metric_ecoind_85_94[1], metric_ecoind_95_04[1]), cbind(metric_land_all[1], metric_land_pred[1], metric_land_74_03[1], metric_land_65_74[1], metric_land_75_84[1], metric_land_85_94[1], metric_land_95_04[1]))

MEF_b<-cbind(metric_biom_all[1], metric_biom_pred[1], metric_biom_74_03[1], metric_biom_65_74[1], metric_biom_75_84[1], metric_biom_85_94[1], metric_biom_95_04[1])
MEF_b[8]<-rownames(MEF_b)

MEF_e<-cbind(metric_ecoind_all[1], metric_ecoind_pred[1], metric_ecoind_74_03[1], metric_ecoind_65_74[1], metric_ecoind_75_84[1], metric_ecoind_85_94[1], metric_ecoind_95_04[1])
MEF_e[8]<-rownames(MEF_e)

MEF_l<-cbind(metric_land_all[1], metric_land_pred[1], metric_land_74_03[1], metric_land_65_74[1], metric_land_75_84[1], metric_land_85_94[1], metric_land_95_04[1])
MEF_l[8]<-rownames(MEF_l)



AE<-rbind(cbind(metric_biom_all[2], metric_biom_pred[2], metric_biom_74_03[2], metric_biom_65_74[2], metric_biom_75_84[2], metric_biom_85_94[2], metric_biom_95_04[2]), cbind(metric_ecoind_all[2], metric_ecoind_pred[2], metric_ecoind_74_03[2], metric_ecoind_65_74[2], metric_ecoind_75_84[2], metric_ecoind_85_94[2], metric_ecoind_95_04[2]), cbind(metric_land_all[2], metric_land_pred[2], metric_land_74_03[2], metric_land_65_74[2], metric_land_75_84[2], metric_land_85_94[2], metric_land_95_04[2]))

AE_b<-cbind(metric_biom_all[2], metric_biom_pred[2], metric_biom_74_03[2], metric_biom_65_74[2], metric_biom_75_84[2], metric_biom_85_94[2], metric_biom_95_04[2])
AE_b[8]<-rownames(AE_b)

AE_e<-cbind(metric_ecoind_all[2], metric_ecoind_pred[2], metric_ecoind_74_03[2], metric_ecoind_65_74[2], metric_ecoind_75_84[2], metric_ecoind_85_94[2], metric_ecoind_95_04[2])
AE_e[8]<-rownames(AE_e)

AE_l<-cbind(metric_land_all[2], metric_land_pred[2], metric_land_74_03[2], metric_land_65_74[2], metric_land_75_84[2], metric_land_85_94[2], metric_land_95_04[2])
AE_l[8]<-rownames(AE_l)

  
AAE<-rbind(cbind(metric_biom_all[3], metric_biom_pred[3], metric_biom_74_03[3], metric_biom_65_74[3], metric_biom_75_84[3], metric_biom_85_94[3], metric_biom_95_04[3]), cbind(metric_ecoind_all[3], metric_ecoind_pred[3], metric_ecoind_74_03[3], metric_ecoind_65_74[3], metric_ecoind_75_84[3], metric_ecoind_85_94[3], metric_ecoind_95_04[3]), cbind(metric_land_all[3], metric_land_pred[3], metric_land_74_03[3], metric_land_65_74[3], metric_land_75_84[3], metric_land_85_94[3], metric_land_95_04[3]))

AAE_b<-cbind(metric_biom_all[3], metric_biom_pred[3], metric_biom_74_03[3], metric_biom_65_74[3], metric_biom_75_84[3], metric_biom_85_94[3], metric_biom_95_04[3])
AAE_b[8]<-rownames(AAE_b)

AAE_e<-cbind(metric_ecoind_all[3], metric_ecoind_pred[3], metric_ecoind_74_03[3], metric_ecoind_65_74[3], metric_ecoind_75_84[3], metric_ecoind_85_94[3], metric_ecoind_95_04[3])
AAE_e[8]<-rownames(AAE_e)

AAE_l<-cbind(metric_land_all[3], metric_land_pred[3], metric_land_74_03[3], metric_land_65_74[3], metric_land_75_84[3], metric_land_85_94[3], metric_land_95_04[3])
AAE_l[8]<-rownames(AAE_l)


RMSE<-rbind(cbind(metric_biom_all[4], metric_biom_pred[4], metric_biom_74_03[4], metric_biom_65_74[4], metric_biom_75_84[4], metric_biom_85_94[4], metric_biom_95_04[4]), cbind(metric_ecoind_all[4], metric_ecoind_pred[4], metric_ecoind_74_03[4], metric_ecoind_65_74[4], metric_ecoind_75_84[4], metric_ecoind_85_94[4], metric_ecoind_95_04[4]), cbind(metric_land_all[4], metric_land_pred[4], metric_land_74_03[4], metric_land_65_74[4], metric_land_75_84[4], metric_land_85_94[4], metric_land_95_04[4]))

RMSE_b<-cbind(metric_biom_all[4], metric_biom_pred[4], metric_biom_74_03[4], metric_biom_65_74[4], metric_biom_75_84[4], metric_biom_85_94[4], metric_biom_95_04[4])
RMSE_b[8]<-rownames(RMSE_b)

RMSE_e<-cbind(metric_ecoind_all[4], metric_ecoind_pred[4], metric_ecoind_74_03[4], metric_ecoind_65_74[4], metric_ecoind_75_84[4], metric_ecoind_85_94[4], metric_ecoind_95_04[4])
RMSE_e[8]<-rownames(RMSE_e)

RMSE_l<-cbind(metric_land_all[4], metric_land_pred[4], metric_land_74_03[4], metric_land_65_74[4], metric_land_75_84[4], metric_land_85_94[4], metric_land_95_04[4])
RMSE_l[8]<-rownames(RMSE_l)


S_corr<-rbind(cbind(metric_biom_all[5], metric_biom_pred[5], metric_biom_74_03[5], metric_biom_65_74[5], metric_biom_75_84[5], metric_biom_85_94[5], metric_biom_95_04[5]), cbind(metric_ecoind_all[5], metric_ecoind_pred[5], metric_ecoind_74_03[5], metric_ecoind_65_74[5], metric_ecoind_75_84[5], metric_ecoind_85_94[5], metric_ecoind_95_04[5]), cbind(metric_land_all[5], metric_land_pred[5], metric_land_74_03[5], metric_land_65_74[5], metric_land_75_84[5], metric_land_85_94[5], metric_land_95_04[5]))

S_b<-cbind(metric_biom_all[5], metric_biom_pred[5], metric_biom_74_03[5], metric_biom_65_74[5], metric_biom_75_84[5], metric_biom_85_94[5], metric_biom_95_04[5])
S_b[8]<-rownames(S_b)

S_e<-cbind(metric_ecoind_all[5], metric_ecoind_pred[5], metric_ecoind_74_03[5], metric_ecoind_65_74[5], metric_ecoind_75_84[5], metric_ecoind_85_94[5], metric_ecoind_95_04[5])
S_e[8]<-rownames(S_e)

S_l<-cbind(metric_land_all[5], metric_land_pred[5], metric_land_74_03[5], metric_land_65_74[5], metric_land_75_84[5], metric_land_85_94[5], metric_land_95_04[5])
S_l[8]<-rownames(S_l)


P_corr<-rbind(cbind(metric_biom_all[6], metric_biom_pred[6], metric_biom_74_03[6], metric_biom_65_74[6], metric_biom_75_84[6], metric_biom_85_94[6], metric_biom_95_04[6]), cbind(metric_ecoind_all[6], metric_ecoind_pred[6], metric_ecoind_74_03[6], metric_ecoind_65_74[6], metric_ecoind_75_84[6], metric_ecoind_85_94[6], metric_ecoind_95_04[6]), cbind(metric_land_all[6], metric_land_pred[6], metric_land_74_03[6], metric_land_65_74[6], metric_land_75_84[6], metric_land_85_94[6], metric_land_95_04[6]))

K_corr<-rbind(cbind(metric_biom_all[7], metric_biom_pred[7], metric_biom_74_03[7], metric_biom_65_74[7], metric_biom_75_84[7], metric_biom_85_94[7], metric_biom_95_04[7]), cbind(metric_ecoind_all[7], metric_ecoind_pred[7], metric_ecoind_74_03[7], metric_ecoind_65_74[7], metric_ecoind_75_84[7], metric_ecoind_85_94[7], metric_ecoind_95_04[7]), cbind(metric_land_all[7], metric_land_pred[7], metric_land_74_03[7], metric_land_65_74[7], metric_land_75_84[7], metric_land_85_94[7], metric_land_95_04[7]))

 

MEF[8]<-rownames(MEF)  
MEF_e[8]<-rownames(MEF_e)


#-----------------------------------------------------------------
### Plot each metric along line
#colorblind friendly palettes
cbPalette1 <- c("#000000", "#D55E00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",  "#E69F00", "#CC79A7")
cbPalette2 <- c("#999999", "#D55E00",  "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "#CC79A7")
cbPalette3 <- c("#000000", brewer.pal(9,"Reds")[3:9])
#revert to skill/figures working dir
setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/skill assessment/figures") 

# MEF plots
#one for each data-type
MEFb_melt<-melt(MEF_b, id=c("V8"))
ggplot(MEFb_melt, aes(variable, value)) +  geom_bar(stat = "identity", aes(fill=variable)) + theme_bw() + facet_wrap(~ V8)+ theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-2, 1))  + scale_y_continuous(name="") + scale_x_discrete(breaks=c("MEF_a", "MEF_p", "MEF_b", "MEF_65", "MEF_75", "MEF_85", "MEF_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("MEF_a", "MEF_p", "MEF_b", "MEF_65", "MEF_75", "MEF_85", "MEF_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Biomass - Modelling Efficiency (MEF)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("MEF_biomass.pdf")

MEFe_melt<-melt(MEF_e, id=c("V8"))
ggplot(MEFe_melt, aes(variable, value)) +  geom_bar(stat = "identity", aes(fill=variable)) + theme_bw()  + facet_wrap(~ V8)+ theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-2, 1))+ scale_y_continuous(name="") + scale_x_discrete(breaks=c("MEF_a", "MEF_p", "MEF_b", "MEF_65", "MEF_75", "MEF_85", "MEF_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("MEF_a", "MEF_p", "MEF_b", "MEF_65", "MEF_75", "MEF_85", "MEF_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Eco. Indicators - Modelling Efficiency (MEF)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("MEF_econind.pdf")

MEFl_melt<-melt(MEF_l, id=c("V8"))
ggplot(MEFl_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity") + theme_bw()   + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-2, 1))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("MEF_a", "MEF_p", "MEF_b", "MEF_65", "MEF_75", "MEF_85", "MEF_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("MEF_a", "MEF_p", "MEF_b", "MEF_65", "MEF_75", "MEF_85", "MEF_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Landings - Modelling Efficiency (MEF)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("MEF_landings.pdf")

# AE plots
M_melt<-melt(AE_b, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw()  + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-3, 3))  + scale_y_continuous(name="") + scale_x_discrete(breaks=c("AE_a", "AE_p", "AE_b", "AE_65", "AE_75", "AE_85", "AE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("AE_a", "AE_p", "AE_b", "AE_65", "AE_75", "AE_85", "AE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Biomass - Average Error (AE)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("AE_biomass.pdf")

M_melt<-melt(AE_e, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")   + theme_bw()  + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-2, 1)) + scale_y_continuous(name="") + scale_x_discrete(breaks=c("AE_a", "AE_p", "AE_b", "AE_65", "AE_75", "AE_85", "AE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("AE_a", "AE_p", "AE_b", "AE_65", "AE_75", "AE_85", "AE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Eco.Indicators - Average Error (AE)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("AE_ecoind.pdf")

M_melt<-melt(AE_l, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")   + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-2, 1))  + scale_y_continuous(name="") + scale_x_discrete(breaks=c("AE_a", "AE_p", "AE_b", "AE_65", "AE_75", "AE_85", "AE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("AE_a", "AE_p", "AE_b", "AE_65", "AE_75", "AE_85", "AE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Landings - Average Error (AE)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("AE_landings.pdf")

#AAE plots
M_melt<-melt(AAE_b, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(0, 2))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("AAE_a", "AAE_p", "AAE_b", "AAE_65", "AAE_75", "AAE_85", "AAE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("AAE_a", "AAE_p", "AAE_b", "AAE_65", "AAE_75", "AAE_85", "AAE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Biomass - Average Absolute Error (AAE)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("AAE_biomass.pdf")

M_melt<-melt(AAE_e, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(0, 2))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("AAE_a", "AAE_p", "AAE_b", "AAE_65", "AAE_75", "AAE_85", "AAE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("AAE_a", "AAE_p", "AAE_b", "AAE_65", "AAE_75", "AAE_85", "AAE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Eco. Indicators - Average Absolute Error (AAE)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("AAE_ecoind.pdf")

M_melt<-melt(AAE_l, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(0, 2))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("AAE_a", "AAE_p", "AAE_b", "AAE_65", "AAE_75", "AAE_85", "AAE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("AAE_a", "AAE_p", "AAE_b", "AAE_65", "AAE_75", "AAE_85", "AAE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Landings - Average Absolute Error (AAE)") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("AAE_land.pdf")

#RMSE plots
M_melt<-melt(RMSE_b, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(0, 0.75))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("RMSE_a", "RMSE_p", "RMSE_b", "RMSE_65", "RMSE_75", "RMSE_85", "RMSE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("RMSE_a", "RMSE_p", "RMSE_b", "RMSE_65", "RMSE_75", "RMSE_85", "RMSE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Biomass - Root Mean Squared Error") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("RMSE_biom.pdf")

M_melt<-melt(RMSE_e, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(0, 0.75))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("RMSE_a", "RMSE_p", "RMSE_b", "RMSE_65", "RMSE_75", "RMSE_85", "RMSE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("RMSE_a", "RMSE_p", "RMSE_b", "RMSE_65", "RMSE_75", "RMSE_85", "RMSE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Eco. Indicators - Root Mean Squared Error") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("RMSE_ecoind.pdf")

M_melt<-melt(RMSE_l, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(0, 0.75))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("RMSE_a", "RMSE_p", "RMSE_b", "RMSE_65", "RMSE_75", "RMSE_85", "RMSE_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("RMSE_a", "RMSE_p", "RMSE_b", "RMSE_65", "RMSE_75", "RMSE_85", "RMSE_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Landings - Root Mean Squared Error") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("RMSE_land.pdf")

# Correlation (Spearman rank chosen as all correlations show the same c.f. PCA analysis)
M_melt<-melt(S_b, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-1, 1))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("S_a", "S_p", "S_b", "S_65", "S_75", "S_85", "S_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("S_a", "S_p", "S_b", "S_65", "S_75", "S_85", "S_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Biomass - Spearman rank correlation") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("S_corr_biom.pdf")

M_melt<-melt(S_e, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-1, 1))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("S_a", "S_p", "S_b", "S_65", "S_75", "S_85", "S_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("S_a", "S_p", "S_b", "S_65", "S_75", "S_85", "S_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Eco. Indicators - Spearman rank correlation") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("S_corr_ecoind.pdf")

M_melt<-melt(S_l, id=c("V8"))
ggplot(M_melt, aes(variable, value, fill=variable))  +  geom_bar(stat = "identity")  + theme_bw() + facet_wrap(~ V8) + theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=10)) + coord_cartesian(ylim=c(-1, 1))   + scale_y_continuous(name="") + scale_x_discrete(breaks=c("S_a", "S_p", "S_b", "S_65", "S_75", "S_85", "S_95"), labels=c("'64-'04(H)", "'05-'13(P)", "'74-'03", "'65-'74", "'75-'84", "'85-'94", "'95-'04"), name="") + scale_fill_manual(values=cbPalette3, name="Period", breaks=c("S_a", "S_p", "S_b", "S_65", "S_75", "S_85", "S_95"),labels=c("1964-2004 (Hindc.)", "2005-2013 (Pred.)", "1974-1903(No-Burn)", "1965-1974", "1975-1984", "1985-1994", "1995-2004")) +ggtitle("Landings - Spearman rank correlation") + theme(plot.title = element_text(size=16, face="bold")) 
ggsave("S_corr_landings.pdf")



#-----------------------------------------------------------------
### Forecast metric plots

# used Sean Lucey code instead



#-----------------------------------------------------------------
### PCA Analyses
# one for each data-type (biomasss, landings, eco-ind.)

biomass<-cbind(metric_biom_pred, metric_biom_all)

ecoind<-cbind(metric_ecoind_pred, metric_ecoind_all)

landings<-cbind(metric_land_pred, metric_land_all)


# full analysis of all time-periods yields initial detailed plot
#biomass<-cbind(metric_biom_pred, metric_biom_all, metric_biom_74_03, metric_biom_65_74, metric_biom_75_84, metric_biom_85_94, metric_biom_95_04)

#ecoind<-cbind(metric_ecoind_pred, metric_ecoind_all, metric_ecoind_74_03, metric_ecoind_65_74, metric_ecoind_75_84, metric_ecoind_85_94, metric_ecoind_95_04)

#landings<-cbind(metric_land_pred, metric_land_all, metric_land_74_03, metric_land_65_74, metric_land_75_84, metric_land_85_94, metric_land_95_04)

# PCA analysis of metrics
biomass_s<-biomass[complete.cases(biomass),]
biom_PC<-princomp(biomass_s, scale=TRUE)

ecoind_s<-ecoind[complete.cases(ecoind),]
ecoind_PC<-princomp(ecoind_s, scale=TRUE)

landings_s<-landings[complete.cases(landings),]
landings_PC<-princomp(landings_s, scale=TRUE)

#creating loadings plots with arrows
# null_PC<-data.frame(x=numeric(49), y=numeric(49))

BIO_PC1_2<-data.frame(biom_PC$loadings[,1:2])
BIO_PC1_2$P_A<-substrRight(rownames(BIO_PC1_2), 1)
colnames(BIO_PC1_2)<-c("PC1", "PC2", "P_A")

ECO_PC1_2<-data.frame(ecoind_PC$loadings[,1:2])
ECO_PC1_2$P_A<-substrRight(rownames(ECO_PC1_2), 1)
colnames(ECO_PC1_2)<-c("PC1", "PC2", "P_A")

LAN_PC1_2<-data.frame(landings_PC$loadings[,1:2])
LAN_PC1_2$P_A<-substrRight(rownames(LAN_PC1_2), 1)
colnames(LAN_PC1_2)<-c("PC1", "PC2", "P_A")

# set a color scheme
PC_colors<-brewer.pal(5, "Set1")

#PCA loadings plot of BIOMASS
PCplot1<-ggplot(BIO_PC1_2)

#label each metric
PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("MEF", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(BIO_PC1_2[grep("MEF", rownames(BIO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[1])
PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("AE", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(BIO_PC1_2[grep("AE", rownames(BIO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[2])
PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("AAE", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(BIO_PC1_2[grep("AAE", rownames(BIO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[3])
PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("RMSE", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(BIO_PC1_2[grep("RMSE", rownames(BIO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[4])
PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("P_", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(BIO_PC1_2[grep("P_", rownames(BIO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])
PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("S_", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(BIO_PC1_2[grep("S_", rownames(BIO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])
PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("K_", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(BIO_PC1_2[grep("K_", rownames(BIO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])

#simple labelling
#PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("MEF", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=P_A), size = 4, vjust=1, color=PC_colors[1])
#PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("AE", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=P_A), size = 4, vjust=1, color=PC_colors[2])
#PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("AAE", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=P_A), size = 4, vjust=1, color=PC_colors[3])
#PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("RMSE", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=P_A), size = 4, vjust=1, color=PC_colors[4])
#PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("P_", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=P_A), size = 4, vjust=1, color=PC_colors[5])
#PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("S_", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=P_A), size = 4, vjust=1, color=PC_colors[5])
#PCplot1 <- PCplot1 + coord_equal() + theme_bw() + geom_text(data=BIO_PC1_2[grep("K_", rownames(BIO_PC1_2)),], aes(x=PC1, y=PC2, label=P_A), size = 4, vjust=1, color=PC_colors[5])


PCplot1 <- PCplot1 + geom_segment(data=BIO_PC1_2, color="gray80", aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray80") +  ggtitle("Biomass") + theme(plot.title = element_text(size = rel(2), colour = "gray20")) + coord_cartesian(xlim = c(-1, 1), ylim = c(-0.5, 0.5) ) + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())

PCplot1
ggsave("PCA_biom_all.pdf", width=11, height=11)

#PCA loadings plot of ECOIND
PCplot2<-ggplot(ECO_PC1_2)

PCplot2 <- PCplot2 + coord_equal() + theme_bw() + geom_text(data=ECO_PC1_2[grep("MEF", rownames(ECO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(ECO_PC1_2[grep("MEF", rownames(ECO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[1])
PCplot2 <- PCplot2 + coord_equal() + theme_bw() + geom_text(data=ECO_PC1_2[grep("AE", rownames(ECO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(ECO_PC1_2[grep("AE", rownames(ECO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[2])
PCplot2 <- PCplot2 + coord_equal() + theme_bw() + geom_text(data=ECO_PC1_2[grep("AAE", rownames(ECO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(ECO_PC1_2[grep("AAE", rownames(ECO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[3])
PCplot2 <- PCplot2 + coord_equal() + theme_bw() + geom_text(data=ECO_PC1_2[grep("RMSE", rownames(ECO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(ECO_PC1_2[grep("RMSE", rownames(ECO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[4])
PCplot2 <- PCplot2 + coord_equal() + theme_bw() + geom_text(data=ECO_PC1_2[grep("P_", rownames(ECO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(ECO_PC1_2[grep("P_", rownames(ECO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])
PCplot2 <- PCplot2 + coord_equal() + theme_bw() + geom_text(data=ECO_PC1_2[grep("S_", rownames(ECO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(ECO_PC1_2[grep("S_", rownames(ECO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])
PCplot2 <- PCplot2 + coord_equal() + theme_bw() + geom_text(data=ECO_PC1_2[grep("K_", rownames(ECO_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(ECO_PC1_2[grep("K_", rownames(ECO_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])



PCplot2 <- PCplot2 + geom_segment(data=ECO_PC1_2, color="gray80", aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="firebrick1") +  ggtitle("Ecological indicators") + theme(plot.title = element_text(size = rel(2), colour = "gray20")) + coord_cartesian(xlim = c(-0.47, 0.47), ylim = c(-0.47, 0.47) )+ theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
PCplot2
ggsave("PCA_ecoind_all.pdf", width=11, height=11)

#PCA loadings plot of LANDINGS
PCplot3<-ggplot(LAN_PC1_2)

PCplot3 <- PCplot3 + coord_equal() + theme_bw() + geom_text(data=LAN_PC1_2[grep("MEF", rownames(LAN_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(LAN_PC1_2[grep("MEF", rownames(LAN_PC1_2)),])), size = 4, vjust=1, color=PC_colors[1])
PCplot3 <- PCplot3 + coord_equal() + theme_bw() + geom_text(data=LAN_PC1_2[grep("AE", rownames(LAN_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(LAN_PC1_2[grep("AE", rownames(LAN_PC1_2)),])), size = 4, vjust=1, color=PC_colors[2])
PCplot3 <- PCplot3 + coord_equal() + theme_bw() + geom_text(data=LAN_PC1_2[grep("AAE", rownames(LAN_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(LAN_PC1_2[grep("AAE", rownames(LAN_PC1_2)),])), size = 4, vjust=1, color=PC_colors[3])
PCplot3 <- PCplot3 + coord_equal() + theme_bw() + geom_text(data=LAN_PC1_2[grep("RMSE", rownames(LAN_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(LAN_PC1_2[grep("RMSE", rownames(LAN_PC1_2)),])), size = 4, vjust=1, color=PC_colors[4])
PCplot3 <- PCplot3 + coord_equal() + theme_bw() + geom_text(data=LAN_PC1_2[grep("P_", rownames(LAN_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(LAN_PC1_2[grep("P_", rownames(LAN_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])
PCplot3 <- PCplot3 + coord_equal() + theme_bw() + geom_text(data=LAN_PC1_2[grep("S_", rownames(LAN_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(LAN_PC1_2[grep("S_", rownames(LAN_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])
PCplot3 <- PCplot3 + coord_equal() + theme_bw() + geom_text(data=LAN_PC1_2[grep("K_", rownames(LAN_PC1_2)),], aes(x=PC1, y=PC2, label=row.names(LAN_PC1_2[grep("K_", rownames(LAN_PC1_2)),])), size = 4, vjust=1, color=PC_colors[5])



PCplot3 <- PCplot3 + geom_segment(data=LAN_PC1_2, color="gray80", aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="firebrick1") +  ggtitle("Landings") + theme(plot.title = element_text(size = rel(2), colour = "gray20")) + coord_cartesian(xlim = c(-0.51, 0.51), ylim = c(-0.51, 0.51) )+ theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
PCplot3
ggsave("PCA_land_all.pdf", width=11, height=11)


#need to source multiplot function
multiplot(PCplot1, PCplot2, PCplot3, cols=2)
#save manually using Export button on Plot-Viewing Window


#-----------------
#### PCA plot of scores (model components)

# Create Scores - data frames

BIO_Scores<-as.data.frame( biom_PC$scores[,1:2])
colnames(BIO_Scores)<-c("PC1", "PC2")
BIORN<-rownames(BIO_Scores)
BIORN[10]<-c("Yellowtail flnd.")
rownames(BIO_Scores)<-BIORN


LAN_Scores<-as.data.frame( landings_PC$scores[,1:2])
colnames(LAN_Scores)<-c("PC1", "PC2")
BIORN<-rownames(LAN_Scores)
BIORN[10]<-c("Yellowtail flnd.")
rownames(LAN_Scores)<-BIORN

ECO_Scores<-as.data.frame( ecoind_PC$scores[,1:2])
colnames(ECO_Scores)<-c("PC1", "PC2")

##Plot Scores

#Biomass Scores
BioScorePlot<-ggplot(BIO_Scores, aes(x=PC1, y=PC2, label=rownames(BIO_Scores)))

BioScorePlot<-BioScorePlot + coord_equal() + theme_bw() + geom_text(size=4, color="darkblue") 
#+ coord_cartesian(xlim = c(-4.1, 4), ylim = c(-4, 4) )

BioScorePlot<-BioScorePlot + geom_segment(color="gray60", aes(x = -0.2, y = 0, xend = 0.2, yend = 0)) + geom_segment(color="gray60", aes(x = 0, y = -0.2, xend = 0, yend = 0.2)) + ggtitle("Biomass component scores")

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/skill assessment/figures")
ggsave("PCA_Bio_Scores.pdf")


#Landings Scores
LanScorePlot<-ggplot(LAN_Scores, aes(x=PC1, y=PC2, label=rownames(LAN_Scores)))

LanScorePlot<-LanScorePlot + coord_equal() + theme_bw() + geom_text(size=4, color="darkorange2") 

LanScorePlot<-LanScorePlot + geom_segment(color="gray60", aes(x = -0.5, y = 0, xend = 0.5, yend = 0)) + geom_segment(color="gray60", aes(x = 0, y = -0.5, xend = 0, yend = 0.5)) + ggtitle("Landings component scores")+ coord_cartesian(xlim = c(-5, 4), ylim = c(-4, 4) )

ggsave("PCA_Lan_Scores.pdf")


#Eco indicators
EcoScorePlot<-ggplot(ECO_Scores, aes(x=PC1, y=PC2, label=rownames(ECO_Scores)))

EcoScorePlot<-EcoScorePlot + coord_equal() + theme_bw() + geom_text(size=4, color="green4") 

EcoScorePlot<-EcoScorePlot + geom_segment(color="gray60", aes(x = -0.5, y = 0, xend = 0.5, yend = 0)) + geom_segment(color="gray60", aes(x = 0, y = -0.5, xend = 0, yend = 0.5)) + ggtitle("Ecological indicators component scores")
#+ coord_cartesian(xlim = c(-5, 4), ylim = c(-4, 4) )

ggsave("PCA_Eco_Scores.pdf")



#-----------------------------------------------------------------
#### Tally when each time-series performs best 
# eg. comparing the Full Hindcast w No Burnin w Predicted, w decades
# use 'biomass', 'landings' and 'ecoind' as input data and extract to new data.frame

tabel_metric_comp<-data.frame(NoBurnin=numeric(15), Forecast=numeric(15), Forecast_NoBurn=numeric(15), D1965_74=numeric(15), D1975_84=numeric(15), D1985_94=numeric(15), D1995_04=numeric(15), row.names=c("b_MEF", "b_AE", "b_AAE", "b_RMSE", "b_SC", "l_MEF", "l_AE", "l_AAE", "l_RMSE", "l_SC", "e_MEF",   "e_AE",   "e_AAE",   "e_RMSE",   "e_SC"))

#MEF_b
i<-c(1)
j<-c(1)
tabel_metric_comp[j,1]<- nrow(subset(biomass, biomass[i+14] > biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,2]<- nrow(subset(biomass, biomass[i] > biomass[i+7] ))/nrow(biomass)
tabel_metric_comp[j,3]<- nrow(subset(biomass, biomass[i] > biomass[i+14]))/nrow(biomass)
tabel_metric_comp[j,4]<- nrow(subset(biomass, biomass[i+21] > biomass[i+7]))/nrow(biomass)
tabel_metric_comp[j,5]<- nrow(subset(biomass, biomass[i+28] > biomass[i+7]))/nrow(biomass)  
tabel_metric_comp[j,6]<- nrow(subset(biomass, biomass[i+35] > biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,7]<- nrow(subset(biomass, biomass[i+42] > biomass[i+7]))/nrow(biomass) 

#AE_b
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(biomass, abs(biomass[i+14]) < abs(biomass[i+7])))/nrow(biomass) 
tabel_metric_comp[j,2]<- nrow(subset(biomass, abs(biomass[i]) < abs(biomass[i+7]) ))/nrow(biomass)
tabel_metric_comp[j,3]<- nrow(subset(biomass, abs(biomass[i]) <  abs(biomass[i+14])))/ nrow(biomass)
tabel_metric_comp[j,4]<- nrow(subset(biomass, abs(biomass[i+21]) < abs(biomass[i+7])))/nrow(biomass)
tabel_metric_comp[j,5]<- nrow(subset(biomass, abs(biomass[i+28]) < abs(biomass[i+7])))/nrow(biomass)  
tabel_metric_comp[j,6]<- nrow(subset(biomass, abs(biomass[i+35]) < abs(biomass[i+7])))/nrow(biomass) 
tabel_metric_comp[j,7]<- nrow(subset(biomass, abs(biomass[i+42]) < abs(biomass[i+7])))/nrow(biomass) 

#AAE_b
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(biomass, biomass[i+14] < biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,2]<- nrow(subset(biomass, biomass[i] < biomass[i+7] ))/nrow(biomass)
tabel_metric_comp[j,3]<- nrow(subset(biomass, biomass[i] < biomass[i+14]))/nrow(biomass)
tabel_metric_comp[j,4]<- nrow(subset(biomass, biomass[i+21] < biomass[i+7]))/nrow(biomass)
tabel_metric_comp[j,5]<- nrow(subset(biomass, biomass[i+28] < biomass[i+7]))/nrow(biomass)  
tabel_metric_comp[j,6]<- nrow(subset(biomass, biomass[i+35] < biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,7]<- nrow(subset(biomass, biomass[i+42] < biomass[i+7]))/nrow(biomass) 

#RMSE_b
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(biomass, biomass[i+14] < biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,2]<- nrow(subset(biomass, biomass[i] < biomass[i+7] ))/nrow(biomass)
tabel_metric_comp[j,3]<- nrow(subset(biomass, biomass[i] < biomass[i+14]))/nrow(biomass)
tabel_metric_comp[j,4]<- nrow(subset(biomass, biomass[i+21] < biomass[i+7]))/nrow(biomass)
tabel_metric_comp[j,5]<- nrow(subset(biomass, biomass[i+28] < biomass[i+7]))/nrow(biomass)  
tabel_metric_comp[j,6]<- nrow(subset(biomass, biomass[i+35] < biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,7]<- nrow(subset(biomass, biomass[i+42] < biomass[i+7]))/nrow(biomass) 

#SCorr_b
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(biomass, biomass[i+14] > biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,2]<- nrow(subset(biomass, biomass[i] > biomass[i+7] ))/nrow(biomass)
tabel_metric_comp[j,3]<- nrow(subset(biomass, biomass[i] > biomass[i+14]))/nrow(biomass)
tabel_metric_comp[j,4]<- nrow(subset(biomass, biomass[i+21] > biomass[i+7]))/nrow(biomass)
tabel_metric_comp[j,5]<- nrow(subset(biomass, biomass[i+28] > biomass[i+7]))/nrow(biomass)  
tabel_metric_comp[j,6]<- nrow(subset(biomass, biomass[i+35] > biomass[i+7]))/nrow(biomass) 
tabel_metric_comp[j,7]<- nrow(subset(biomass, biomass[i+42] > biomass[i+7]))/nrow(biomass) 

# landings data
#MEF_l
i<-c(1)
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(landings, landings[i+14] > landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,2]<- nrow(subset(landings, landings[i] > landings[i+7] ))/nrow(landings)
tabel_metric_comp[j,3]<- nrow(subset(landings, landings[i] > landings[i+14]))/nrow(landings)
tabel_metric_comp[j,4]<- nrow(subset(landings, landings[i+21] > landings[i+7]))/nrow(landings)
tabel_metric_comp[j,5]<- nrow(subset(landings, landings[i+28] > landings[i+7]))/nrow(landings)  
tabel_metric_comp[j,6]<- nrow(subset(landings, landings[i+35] > landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,7]<- nrow(subset(landings, landings[i+42] > landings[i+7]))/nrow(landings) 

#AE_l
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(landings, abs(landings[i+14]) < abs(landings[i+7])))/nrow(landings) 
tabel_metric_comp[j,2]<- nrow(subset(landings, abs(landings[i]) < abs(landings[i+7]) ))/nrow(landings)
tabel_metric_comp[j,3]<- nrow(subset(landings, abs(landings[i]) < abs(landings[i+14])))/ nrow(landings)
tabel_metric_comp[j,4]<- nrow(subset(landings, abs(landings[i+21]) < abs(landings[i+7])))/nrow(landings)
tabel_metric_comp[j,5]<- nrow(subset(landings, abs(landings[i+28]) < abs(landings[i+7])))/nrow(landings)  
tabel_metric_comp[j,6]<- nrow(subset(landings, abs(landings[i+35]) < abs(landings[i+7])))/nrow(landings) 
tabel_metric_comp[j,7]<- nrow(subset(landings, abs(landings[i+42]) < abs(landings[i+7])))/nrow(landings) 

#AAE_l
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(landings, landings[i+14] < landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,2]<- nrow(subset(landings, landings[i] < landings[i+7] ))/nrow(landings)
tabel_metric_comp[j,3]<- nrow(subset(landings, landings[i] < landings[i+14]))/nrow(landings)
tabel_metric_comp[j,4]<- nrow(subset(landings, landings[i+21] < landings[i+7]))/nrow(landings)
tabel_metric_comp[j,5]<- nrow(subset(landings, landings[i+28] < landings[i+7]))/nrow(landings)  
tabel_metric_comp[j,6]<- nrow(subset(landings, landings[i+35] < landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,7]<- nrow(subset(landings, landings[i+42] < landings[i+7]))/nrow(landings) 

#RMSE_l
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(landings, landings[i+14] < landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,2]<- nrow(subset(landings, landings[i] < landings[i+7] ))/nrow(landings)
tabel_metric_comp[j,3]<- nrow(subset(landings, landings[i] < landings[i+14]))/nrow(landings)
tabel_metric_comp[j,4]<- nrow(subset(landings, landings[i+21] < landings[i+7]))/nrow(landings)
tabel_metric_comp[j,5]<- nrow(subset(landings, landings[i+28] < landings[i+7]))/nrow(landings)  
tabel_metric_comp[j,6]<- nrow(subset(landings, landings[i+35] < landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,7]<- nrow(subset(landings, landings[i+42] < landings[i+7]))/nrow(landings) 

#SCorr_l
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(landings, landings[i+14] > landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,2]<- nrow(subset(landings, landings[i] > landings[i+7] ))/nrow(landings)
tabel_metric_comp[j,3]<- nrow(subset(landings, landings[i] > landings[i+14]))/nrow(landings)
tabel_metric_comp[j,4]<- nrow(subset(landings, landings[i+21] > landings[i+7]))/nrow(landings)
tabel_metric_comp[j,5]<- nrow(subset(landings, landings[i+28] > landings[i+7]))/nrow(landings)  
tabel_metric_comp[j,6]<- nrow(subset(landings, landings[i+35] > landings[i+7]))/nrow(landings) 
tabel_metric_comp[j,7]<- nrow(subset(landings, landings[i+42] > landings[i+7]))/nrow(landings) 

# ecoindicators
#MEF_i
i<-c(1)
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(ecoind, ecoind[i+14] > ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,2]<- nrow(subset(ecoind, ecoind[i] > ecoind[i+7] ))/nrow(ecoind)
tabel_metric_comp[j,3]<- nrow(subset(ecoind, ecoind[i] > ecoind[i+14]))/nrow(ecoind)
tabel_metric_comp[j,4]<- nrow(subset(ecoind, ecoind[i+21] > ecoind[i+7]))/nrow(ecoind)
tabel_metric_comp[j,5]<- nrow(subset(ecoind, ecoind[i+28] > ecoind[i+7]))/nrow(ecoind)  
tabel_metric_comp[j,6]<- nrow(subset(ecoind, ecoind[i+35] > ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,7]<- nrow(subset(ecoind, ecoind[i+42] > ecoind[i+7]))/nrow(ecoind) 

#AE_i
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(ecoind, abs(ecoind[i+14]) < abs(ecoind[i+7])))/nrow(ecoind) 
tabel_metric_comp[j,2]<- nrow(subset(ecoind, abs(ecoind[i]) < abs(ecoind[i+7]) ))/nrow(ecoind)
tabel_metric_comp[j,3]<- nrow(subset(ecoind, abs(ecoind[i]) < abs(ecoind[i+14])))/ nrow(ecoind)
tabel_metric_comp[j,4]<- nrow(subset(ecoind, abs(ecoind[i+21]) < abs(ecoind[i+7])))/nrow(ecoind)
tabel_metric_comp[j,5]<- nrow(subset(ecoind, abs(ecoind[i+28]) < abs(ecoind[i+7])))/nrow(ecoind)  
tabel_metric_comp[j,6]<- nrow(subset(ecoind, abs(ecoind[i+35]) < abs(ecoind[i+7])))/nrow(ecoind) 
tabel_metric_comp[j,7]<- nrow(subset(ecoind, abs(ecoind[i+42]) < abs(ecoind[i+7])))/nrow(ecoind) 

#AAE_i
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(ecoind, ecoind[i+14] < ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,2]<- nrow(subset(ecoind, ecoind[i] < ecoind[i+7] ))/nrow(ecoind)
tabel_metric_comp[j,3]<- nrow(subset(ecoind, ecoind[i] < ecoind[i+14]))/nrow(ecoind)
tabel_metric_comp[j,4]<- nrow(subset(ecoind, ecoind[i+21] < ecoind[i+7]))/nrow(ecoind)
tabel_metric_comp[j,5]<- nrow(subset(ecoind, ecoind[i+28] < ecoind[i+7]))/nrow(ecoind)  
tabel_metric_comp[j,6]<- nrow(subset(ecoind, ecoind[i+35] < ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,7]<- nrow(subset(ecoind, ecoind[i+42] < ecoind[i+7]))/nrow(ecoind) 

#RMSE_i
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(ecoind, ecoind[i+14] < ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,2]<- nrow(subset(ecoind, ecoind[i] < ecoind[i+7] ))/nrow(ecoind)
tabel_metric_comp[j,3]<- nrow(subset(ecoind, ecoind[i] < ecoind[i+14]))/nrow(ecoind)
tabel_metric_comp[j,4]<- nrow(subset(ecoind, ecoind[i+21] < ecoind[i+7]))/nrow(ecoind)
tabel_metric_comp[j,5]<- nrow(subset(ecoind, ecoind[i+28] < ecoind[i+7]))/nrow(ecoind)  
tabel_metric_comp[j,6]<- nrow(subset(ecoind, ecoind[i+35] < ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,7]<- nrow(subset(ecoind, ecoind[i+42] < ecoind[i+7]))/nrow(ecoind) 

#SCorr_i
i<-i+1
j<-j+1
tabel_metric_comp[j,1]<- nrow(subset(ecoind, ecoind[i+14] > ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,2]<- nrow(subset(ecoind, ecoind[i] > ecoind[i+7] ))/nrow(ecoind)
tabel_metric_comp[j,3]<- nrow(subset(ecoind, ecoind[i] >  ecoind[i+14]))/nrow(ecoind)
tabel_metric_comp[j,4]<- nrow(subset(ecoind, ecoind[i+21] > ecoind[i+7]))/nrow(ecoind)
tabel_metric_comp[j,5]<- nrow(subset(ecoind, ecoind[i+28] > ecoind[i+7]))/nrow(ecoind)  
tabel_metric_comp[j,6]<- nrow(subset(ecoind, ecoind[i+35] > ecoind[i+7]))/nrow(ecoind) 
tabel_metric_comp[j,7]<- nrow(subset(ecoind, ecoind[i+42] > ecoind[i+7]))/nrow(ecoind) 


# Heatmap / tabel of tabel_metric_comp 
tabel_metric_comp$Metric <- rownames(tabel_metric_comp)
TMC<-cbind(tabel_metric_comp[8], tabel_metric_comp[1], tabel_metric_comp[4], tabel_metric_comp[5], tabel_metric_comp[6], tabel_metric_comp[7], tabel_metric_comp[2], tabel_metric_comp[3])
tabel_metric_comp_melt <- melt(TMC, id=c("Metric"))

#df1 <- with(tabel_metric_comp_melt, factor(Metric, levels = rev(levels(Metric))))


#creating breaks
brks<-classIntervals(tabel_metric_comp_melt$value, n=4, style="fixed", fixedBreaks=c(0, 0.25, 0.5, 0.75, 1.0)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(tabel_metric_comp_melt$value, brks, all.inside=TRUE) #assign categories

tabel_metric_comp_melt<-cbind(tabel_metric_comp_melt, catVar) #join data & categories

# Create labels from break values
intLabels <- matrix(1:(length(brks)-1))
intLabels[1,1]<-c("0-25%")
intLabels[2,1]<-c("25-50%")
intLabels[3,1]<-c("50-75%")
intLabels[4,1]<-c("75-100%")
#intLabels[5,1]<-c("80-100%")

# make heatplot of all columns
metricplot <- ggplot(tabel_metric_comp_melt, aes(variable, Metric)) + geom_tile(aes(fill = factor(catVar)), colour = "white") + scale_fill_brewer(palette="PiYG", name="% components", label=intLabels) + geom_text(aes(label = round(value, 2), size= 16), show_guide  = F) + theme( axis.text.x = element_text(angle = -45, hjust = 0, colour = "grey20", size=14))+ theme( axis.text.y = element_text(colour = "grey20", size=14)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + theme(legend.title = element_text(size=16, face="bold"))+ theme(legend.text = element_text(colour="grey20", size = 14, face = "bold")) + ggtitle("Percentage of model components with higher performance \n metrics in select time periods vs. full hindcast\n ")+ theme(legend.title = element_text(size=14, face="bold")) + theme(plot.title = element_text(size = rel(1.5), face = "bold"))

metricplot + scale_x_discrete(breaks=c("NoBurnin", "D1965_74", "D1975_84", "D1985_94", "D1995_04", "Forecast", "Forecast_NoBurn"), labels=c("No Burnin", "1965-1974", "1975-1984", "1985-1994", "1995-2004", "Forecast", "Forecast vs. No Burnin")) 

ggsave("metric_comp_heatplot.pdf", scale = 1, dpi = 400)


# Heatplot for NoBurning & Forecast ONLY
tabel_metric_comp_melt2<-subset(tabel_metric_comp_melt, (variable == "NoBurnin")|(variable =="Forecast"))

metricplot2 <- ggplot(tabel_metric_comp_melt2, aes(variable, Metric)) + geom_tile(aes(fill = factor(catVar)), colour = "white") + scale_fill_brewer(palette="PiYG", name="% components", label=intLabels) + geom_text(aes(label = round(value, 2)), size= 5, show_guide  = F) + theme( axis.text.x = element_text(angle = -45, hjust = 0, colour = "grey20", size=14))+ theme( axis.text.y = element_text(colour = "grey20", size=14)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + theme(legend.title = element_text(size=16, face="bold"))+ theme(legend.text = element_text(colour="grey20", size = 14, face = "bold")) + ggtitle("Fraction of model components \n with higher performance  metrics in \n select time periods vs. full hindcast\n ") + theme(legend.title = element_text(size=14, face="bold")) + theme(plot.title = element_text(size = rel(1.5), face = "bold"))

metricplot2 + scale_x_discrete(breaks=c("NoBurnin", "Forecast")) 

ggsave("metric_comp_heatplot_SELECTED.pdf", width =6, height = 11, dpi = 400)

