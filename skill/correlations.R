### General Correlation analysis scrips
# For use in skill assessment
# By: Erik Olsen
# 5/11/2014

# must import data files using indicators_run.R script

# more detailed subsets for 74-03 (No_Burnin), 65-74, 75-84, 85-94, 95-04
Y1<-c(1964, 1974, 1965, 1975, 1985, 1995, 2005)
Y2<-c(2004, 2003, 1974, 1984, 1994, 2004, 2013)

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
IndNames<-names(EcoInd_obs)[1:17]
model<-EcoInd_model
obs<-EcoInd_obs


for (j in 1:length(Y1)){
  cortab_i<-data.frame(S_h=numeric(17), P_h=numeric(17),  K_h=numeric(17),  row.names=IndNames)  

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

# IMPORT metrics tables to combine with correlation tables
metric_biom_all<-read.csv("metric_biom_all.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_74_03<-read.csv("metric_biom_74_03.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_65_74<-read.csv("metric_biom_65_74.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_75_84<-read.csv("metric_biom_75_84.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_85_94<-read.csv("metric_biom_85_94.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_biom_95_04<-read.csv("metric_biom_95_04.csv",head=TRUE, sep=",", nrows=23, row.names=1)
metric_ecoind_all<-read.csv("metric_ecoind_all.csv",head=TRUE, sep=",", row.names=1)
metric_ecoind_74_03<-read.csv("metric_ecoind_74_03.csv",head=TRUE, sep=",", row.names=1)
metric_ecoind_65_74<-read.csv("metric_ecoind_65_74.csv",head=TRUE, sep=",", row.names=1)
metric_ecoind_75_84<-read.csv("metric_ecoind_75_84.csv",head=TRUE, sep=",", row.names=1)
metric_ecoind_85_94<-read.csv("metric_ecoind_85_94.csv",head=TRUE, sep=",", row.names=1)
metric_ecoind_95_04<-read.csv("metric_ecoind_95_04.csv",head=TRUE, sep=",", row.names=1)
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

# data[row.names(data)%in%y[c(2,4,5,8,9)],]
## 12112014 rerun - fix rows

# combine with correlation tables
metric_biom_all<-cbind(metric_biom_all[1:22,c(1,3,5,7)], correlation_biom_1964_to_2004)
metric_biom_74_03<-cbind(metric_biom_74_03[1:22,c(1,3,5,7)], correlation_biom_1974_to_2003)
metric_biom_65_74<-cbind(metric_biom_65_74[1:22,c(1,3,5,7)], correlation_biom_1965_to_1974)
metric_biom_75_84<-cbind(metric_biom_75_84[1:22,c(1,3,5,7)], correlation_biom_1975_to_1984)
metric_biom_85_94<-cbind(metric_biom_85_94[1:22,c(1,3,5,7)], correlation_biom_1985_to_1994)
metric_biom_95_04<-cbind(metric_biom_95_04[1:22,c(1,3,5,7)], correlation_biom_1995_to_2004)
metric_ecoind_all<-cbind(metric_ecoind_all[1:17,c(1,3,5,7)], correlation_ecoind_1964_to_2004)
metric_ecoind_74_03<-cbind(metric_ecoind_74_03[1:17,c(1,3,5,7)], correlation_ecoind_1974_to_2003)
metric_ecoind_65_74<-cbind(metric_ecoind_65_74[1:17,c(1,3,5,7)], correlation_ecoind_1965_to_1974)
metric_ecoind_75_84<-cbind(metric_ecoind_75_84[1:17,c(1,3,5,7)], correlation_ecoind_1975_to_1984)
metric_ecoind_85_94<-cbind(metric_ecoind_85_94[1:17,c(1,3,5,7)], correlation_ecoind_1985_to_1994)
metric_ecoind_95_04<-cbind(metric_ecoind_95_04[1:17,c(1,3,5,7)], correlation_ecoind_1995_to_2004)

####### TODO:


# PCA analysis of metrics


# make bar plots for each indicator
