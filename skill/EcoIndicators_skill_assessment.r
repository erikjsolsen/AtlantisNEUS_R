library(plyr)
#modelIndicatorTable = load("/home/rgamble/Desktop/R/Atlantis Skill Assessment/EcoIndicators_modelv2.RData")
#observedIndicatorTable = load("/home/rgamble/Desktop/R/Atlantis Skill Assessment/EcoIndicators_observedv2.RData")

modelIndicatorTable = read.table("/home/rgamble/Desktop/R/Atlantis Skill Assessment/Indicators_Model.csv",sep=",",header=TRUE)
observedIndicatorTable = read.table("/home/rgamble/Desktop/R/Atlantis Skill Assessment/Indicators_Obs.csv",sep=",",na.strings = "NA",header=TRUE)

mean_modelIndicatorTable = colMeans(modelIndicatorTable,na.rm=TRUE)
stDev_modelIndicatorTable = colwise(sd)(modelIndicatorTable,na.rm=TRUE)
mean_observedIndicatorTable = colMeans(observedIndicatorTable,na.rm=TRUE)
stDev_observedIndicatorTable = colwise(sd)(observedIndicatorTable,na.rm=TRUE)

standardized_ModelIndicatorTable = modelIndicatorTable
standardized_ObservedIndicatorTable = observedIndicatorTable

for (i in 2:16) {
  stDevMod = stDev_modelIndicatorTable[i]
  stDevObs = stDev_observedIndicatorTable[i]
  standardized_ModelIndicatorTable[,i] = (modelIndicatorTable[,i] - mean_modelIndicatorTable[i]) / stDevMod[1,1]                                                                                                   
  standardized_ObservedIndicatorTable[,i] = (observedIndicatorTable[,i] - mean_observedIndicatorTable[i]) / stDevObs[1,1]
}

mean_standardizedModelIndicatorTable = colMeans(standardized_ModelIndicatorTable,na.rm=TRUE)
mean_standardizedObservedIndicatorTable = colMeans(standardized_ObservedIndicatorTable,na.rm=TRUE)

# Calculate MEF - tuning part of time series
MEF_tuning_table = mean_modelIndicatorTable[(2:16)]*0
MEF_tuning_total = 0
tot_tuningObsTerm = 0
tot_tuningPredTerm = 0
for (i in 2:16) {
  obsTerm = 0
  predTerm = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      obsTerm = obsTerm + (standardized_ObservedIndicatorTable[y,i] - mean_standardizedObservedIndicatorTable[i]) * (standardized_ObservedIndicatorTable[y,i] - mean_standardizedObservedIndicatorTable[i])
      predTerm = predTerm + (standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]) * (standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i])  
    }
  }  
  tot_tuningObsTerm = tot_tuningObsTerm + obsTerm
  tot_tuningPredTerm = tot_tuningPredTerm + predTerm
  MEF_index = i-1
  MEF_tuning_table[MEF_index] = (obsTerm - predTerm) / obsTerm
}
MEF_tuning_total = (tot_tuningObsTerm - tot_tuningPredTerm) / tot_tuningObsTerm
MEF_tuning_table[16] = MEF_tuning_total

# Calculate MEF - prediction part of time series

MEF_prediction_table = mean_modelIndicatorTable[(2:16)]*0
MEF_prediction_total = 0
totObsTerm = 0
totPredTerm = 0
for (i in 2:16) {
  obsTerm = 0
  predTerm = 0
  for (y in 42:50) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      obsTerm = obsTerm + (standardized_ObservedIndicatorTable[y,i] - mean_standardizedObservedIndicatorTable[i]) * (standardized_ObservedIndicatorTable[y,i] - mean_standardizedObservedIndicatorTable[i])
      predTerm = predTerm + (standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]) * (standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i])  
    }
  }  
  totObsTerm = totObsTerm + obsTerm
  totPredTerm = totPredTerm + predTerm
  MEF_index = i-1
  MEF_prediction_table[MEF_index] = (obsTerm - predTerm) / obsTerm
}
MEF_prediction_total = (totObsTerm - totPredTerm) / totObsTerm
MEF_prediction_table[16] = MEF_prediction_total
write.table(MEF_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv",sep=",",quote=FALSE,append=FALSE)
write.table(MEF_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate AE - tuning part of time series
AE_tuning_table = mean_modelIndicatorTable[(2:16)]*0
AE_tuning_total = 0
totTuningSumDifferences = 0
totTuningNumObservations = 0
for (i in 2:16) {
  sumDifferences = 0
  numObservations = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumDifferences = sumDifferences + standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]
      numObservations = numObservations + 1
      totTuningSumDifferences = totTuningSumDifferences + sumDifferences
      totTuningNumObservations = totTuningNumObservations + numObservations
    }
  }
  AE = sumDifferences / numObservations
  AE_index = i-1
  AE_tuning_table[AE_index] = AE
}
AE_tuning_total = totTuningSumDifferences / totTuningNumObservations
AE_tuning_table[16] = AE_tuning_total    


# Calculate AE - prediction part of time series
AE_prediction_table = mean_modelIndicatorTable[(2:16)]*0
totPredictionSumDifferences = 0
totPredictionNumObservations = 0
for (i in 2:16) {
  sumDifferences = 0
  numObservations = 0
  for (y in 42:50) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumDifferences = sumDifferences + standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]
      numObservations = numObservations + 1
      totPredictionSumDifferences = totPredictionSumDifferences + sumDifferences
      totPredictionNumObservations = totPredictionNumObservations + numObservations
    }
  }
  AE = sumDifferences / numObservations
  AE_index = i-1
  AE_prediction_table[AE_index] = AE
}
AE_prediction_total = totPredictionSumDifferences / totPredictionNumObservations
AE_prediction_table[16] = AE_prediction_total

write.table(AE_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(AE_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate AAE - tuning part of time series
AAE_tuning_table = mean_modelIndicatorTable[(2:16)]*0
AAE_tuning_total = 0
totTuningSumDifferences = 0
totTuningNumObservations = 0
for (i in 2:16) {
  sumDifferences = 0
  numObservations = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumDifferences = sumDifferences + abs(standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i])
      numObservations = numObservations + 1
      totTuningSumDifferences = totTuningSumDifferences + sumDifferences
      totTuningNumObservations = totTuningNumObservations + numObservations
    }
  }
  AAE = sumDifferences / numObservations
  AAE_index = i-1
  AAE_tuning_table[AAE_index] = AAE
}
AAE_tuning_total = totTuningSumDifferences / totTuningNumObservations
AAE_tuning_table[16] = AAE_tuning_total

# Calculate AAE - prediction part of time series
AAE_prediction_table = mean_modelIndicatorTable[(2:16)]*0
AAE_prediction_total = 0
totPredictionSumDifferences = 0
totPredictionNumObservations = 0
for (i in 2:16) {
  sumDifferences = 0
  numObservations = 0
  for (y in 42:50) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumDifferences = sumDifferences + abs(standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i])
      numObservations = numObservations + 1
      totPredictionSumDifferences = totPredictionSumDifferences + sumDifferences
      totPredictionNumObservations = totPredictionNumObservations + numObservations    }
  }
  AAE = sumDifferences / numObservations
  AAE_index = i-1
  AAE_prediction_table[AAE_index] = AAE
}
AAE_prediction_total = totPredictionSumDifferences / totPredictionNumObservations
AAE_prediction_table[16] = AAE_prediction_total

write.table(AAE_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(AAE_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate RMSE - tuning part of time series
RMSE_tuning_table = mean_modelIndicatorTable[(2:16)]*0
RMSE_tuning_total = 0
totSumDifferencesSquared = 0
totNumObservations = 0
for (i in 2:16) {
  sumDifferencesSquared = 0
  numObservations = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumDifferencesSquared = sumDifferencesSquared + ((standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]) * (standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]))
      numObservations = numObservations + 1
      totSumDifferencesSquared = totSumDifferencesSquared + sumDifferencesSquared
      totNumObservations = totNumObservations + numObservations
    }
  }
  RMSE = sqrt(sumDifferencesSquared) / numObservations
  RMSE_index = i-1
  RMSE_tuning_table[RMSE_index] = RMSE
}
RMSE_tuning_total = sqrt(totSumDifferencesSquared / totNumObservations)
RMSE_tuning_table[16] = RMSE_tuning_total

# Calculate RMSE - prediction part of time series
RMSE_prediction_table = mean_modelIndicatorTable[(2:16)]*0
RMSE_prediction_total = 0
totSumDifferencesSquared = 0
totNumObservations = 0
for (i in 2:16) {
  sumDifferencesSquared = 0
  numObservations = 0
  for (y in 42:50) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumDifferencesSquared = sumDifferencesSquared + ((standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]) * (standardized_ModelIndicatorTable[y,i] - standardized_ObservedIndicatorTable[y,i]))
      numObservations = numObservations + 1
      totSumDifferencesSquared = totSumDifferencesSquared + sumDifferencesSquared
      totNumObservations = totNumObservations + numObservations
    }
  }
  RMSE = sqrt(sumDifferencesSquared) / numObservations
  RMSE_index = i-1
  RMSE_prediction_table[RMSE_index] = RMSE
}
RMSE_prediction_total = sqrt(totSumDifferencesSquared / totNumObservations)
RMSE_prediction_table[16] = RMSE_prediction_total

write.table(RMSE_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(RMSE_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Indicators_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate RI - tuning part of time series
RI_tuning_table = mean_modelIndicatorTable[(2:22)]*0
for (i in 2:22) {
  sumIndex = 0
  numObservations
  for (y in 11:41) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumIndex = sumIndex + (log(standardized_ObservedIndicatorTable[y,i] / standardized_ModelIndicatorTable[y,i])) * (log(standardized_ObservedIndicatorTable[y,i] / standardized_ModelIndicatorTable[y,i]))
      numObservations = numObservations + 1
    }
  }
  RI = exp(sqrt(sumIndex / numObservations))
  RI_index = i-1
  RI_tuning_table[RI_index] = RI
}

# Calculate RI - prediction part of time series
RI_prediction_table = mean_modelIndicatorTable[(2:22)]*0
for (i in 2:22) {
  sumIndex = 0
  numObservations
  for (y in 42:49) {
    if (!(is.na(standardized_ObservedIndicatorTable[y,i]))) {
      sumIndex = sumIndex + (log(standardized_ObservedIndicatorTable[y,i] / standardized_ModelIndicatorTable[y,i])) * (log(standardized_ObservedIndicatorTable[y,i] / standardized_ModelIndicatorTable[y,i]))
      numObservations = numObservations + 1
    }
  }
  RI = exp(sqrt(sumIndex / numObservations))
  RI_index = i-1
  RI_prediction_table[RI_index] = RI
}

write.table(RI_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(RI_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)
