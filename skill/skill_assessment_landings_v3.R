library(plyr)
modelBiomassTable = read.table("/home/rgamble/Desktop/R/Atlantis Skill Assessment/Modeled_Landings.csv",sep=",",header=TRUE)
observedBiomassTable = read.table("/home/rgamble/Desktop/R/Atlantis Skill Assessment/Observed_Landings.csv",sep=",",na.strings = "NA",header=TRUE)

mean_modelBiomassTable = colMeans(modelBiomassTable)
stDev_modelBiomassTable = colwise(sd)(modelBiomassTable)
mean_observedBiomassTable = colMeans(observedBiomassTable,na.rm=TRUE)
stDev_observedBiomassTable = colwise(sd)(observedBiomassTable,na.rm=TRUE)

standardized_ModelBiomassTable = modelBiomassTable
standardized_ObservedBiomassTable = observedBiomassTable

for (i in 2:22) {
  stDevMod = stDev_modelBiomassTable[i]
  stDevObs = stDev_observedBiomassTable[i]
  standardized_ModelBiomassTable[,i] = (modelBiomassTable[,i] - mean_modelBiomassTable[i]) / stDevMod[1,1]                                                                                                   
  standardized_ObservedBiomassTable[,i] = (observedBiomassTable[,i] - mean_observedBiomassTable[i]) / stDevObs[1,1]
}

mean_standardizedModelBiomassTable = colMeans(standardized_ModelBiomassTable,na.rm=TRUE)
mean_standardizedObservedBiomassTable = colMeans(standardized_ObservedBiomassTable,na.rm=TRUE)

# Calculate MEF - tuning part of time series
MEF_tuning_table = mean_modelBiomassTable[(2:22)]*0
MEF_tuning_total = 0
tot_tuningObsTerm = 0
tot_tuningPredTerm = 0
for (i in 2:22) {
  obsTerm = 0
  predTerm = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      obsTerm = obsTerm + (standardized_ObservedBiomassTable[y,i] - mean_standardizedObservedBiomassTable[i]) * (standardized_ObservedBiomassTable[y,i] - mean_standardizedObservedBiomassTable[i])
      predTerm = predTerm + (standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]) * (standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i])  
    }
  }  
  tot_tuningObsTerm = tot_tuningObsTerm + obsTerm
  tot_tuningPredTerm = tot_tuningPredTerm + predTerm
  MEF_index = i-1
  MEF_tuning_table[MEF_index] = (obsTerm - predTerm) / obsTerm
}
MEF_tuning_total = (tot_tuningObsTerm - tot_tuningPredTerm) / tot_tuningObsTerm
MEF_tuning_table[22] = MEF_tuning_total

# Calculate MEF - prediction part of time series

MEF_prediction_table = mean_modelBiomassTable[(2:22)]*0
MEF_prediction_total = 0
totObsTerm = 0
totPredTerm = 0
for (i in 2:22) {
  obsTerm = 0
  predTerm = 0
  for (y in 42:49) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      obsTerm = obsTerm + (standardized_ObservedBiomassTable[y,i] - mean_standardizedObservedBiomassTable[i]) * (standardized_ObservedBiomassTable[y,i] - mean_standardizedObservedBiomassTable[i])
      predTerm = predTerm + (standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]) * (standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i])  
    }
  }  
  totObsTerm = totObsTerm + obsTerm
  totPredTerm = totPredTerm + predTerm
  MEF_index = i-1
  MEF_prediction_table[MEF_index] = (obsTerm - predTerm) / obsTerm
}
MEF_prediction_total = (totObsTerm - totPredTerm) / totObsTerm
MEF_prediction_table[22] = MEF_prediction_total
write.table(MEF_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv",sep=",",quote=FALSE,append=FALSE)
write.table(MEF_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate AE - tuning part of time series
AE_tuning_table = mean_modelBiomassTable[(2:22)]*0
AE_tuning_total = 0
totTuningSumDifferences = 0
totTuningNumObservations = 0
for (i in 2:22) {
  sumDifferences = 0
  numObservations = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumDifferences = sumDifferences + standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]
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
AE_tuning_table[22] = AE_tuning_total    


# Calculate AE - prediction part of time series
AE_prediction_table = mean_modelBiomassTable[(2:22)]*0
totPredictionSumDifferences = 0
totPredictionNumObservations = 0
for (i in 2:22) {
  sumDifferences = 0
  numObservations = 0
  for (y in 42:49) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumDifferences = sumDifferences + standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]
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
AE_prediction_table[22] = AE_prediction_total
}
write.table(AE_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(AE_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate AAE - tuning part of time series
AAE_tuning_table = mean_modelBiomassTable[(2:22)]*0
AAE_tuning_total = 0
totTuningSumDifferences = 0
totTuningNumObservations = 0
for (i in 2:22) {
  sumDifferences = 0
  numObservations = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumDifferences = sumDifferences + abs(standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i])
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
AAE_tuning_table[22] = AAE_tuning_total

# Calculate AAE - prediction part of time series
AAE_prediction_table = mean_modelBiomassTable[(2:22)]*0
AAE_prediction_total = 0
totPredictionSumDifferences = 0
totPredictionNumObservations = 0
for (i in 2:22) {
  sumDifferences = 0
  numObservations = 0
  for (y in 42:49) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumDifferences = sumDifferences + abs(standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i])
      numObservations = numObservations + 1
      totPredictionSumDifferences = totPredictionSumDifferences + sumDifferences
      totPredictionNumObservations = totPredictionNumObservations + numObservations    }
  }
  AAE = sumDifferences / numObservations
  AAE_index = i-1
  AAE_prediction_table[AAE_index] = AAE
}
AAE_prediction_total = totPredictionSumDifferences / totPredictionNumObservations
AAE_prediction_table[22] = AAE_prediction_total

write.table(AAE_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(AAE_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate RMSE - tuning part of time series
RMSE_tuning_table = mean_modelBiomassTable[(2:22)]*0
RMSE_tuning_total = 0
totSumDifferencesSquared = 0
totNumObservations = 0
for (i in 2:22) {
  sumDifferencesSquared = 0
  numObservations = 0
  for (y in 32:41) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumDifferencesSquared = sumDifferencesSquared + ((standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]) * (standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]))
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
RMSE_tuning_table[22] = RMSE_tuning_total

# Calculate RMSE - prediction part of time series
RMSE_prediction_table = mean_modelBiomassTable[(2:22)]*0
RMSE_prediction_total = 0
totSumDifferencesSquared = 0
totNumObservations = 0
for (i in 2:22) {
  sumDifferencesSquared = 0
  numObservations = 0
  for (y in 42:49) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumDifferencesSquared = sumDifferencesSquared + ((standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]) * (standardized_ModelBiomassTable[y,i] - standardized_ObservedBiomassTable[y,i]))
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
RMSE_prediction_table[22] = RMSE_prediction_total

write.table(RMSE_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(RMSE_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)

# Calculate RI - tuning part of time series
RI_tuning_table = mean_modelBiomassTable[(2:22)]*0
for (i in 2:22) {
  sumIndex = 0
  numObservations
  for (y in 1:41) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumIndex = sumIndex + (log(standardized_ObservedBiomassTable[y,i] / standardized_ModelBiomassTable[y,i])) * (log(standardized_ObservedBiomassTable[y,i] / standardized_ModelBiomassTable[y,i]))
      numObservations = numObservations + 1
    }
  }
  RI = exp(sqrt(sumIndex / numObservations))
  RI_index = i-1
  RI_tuning_table[RI_index] = RI
}

# Calculate RI - prediction part of time series
RI_prediction_table = mean_modelBiomassTable[(2:22)]*0
for (i in 2:22) {
  sumIndex = 0
  numObservations
  for (y in 42:49) {
    if (!(is.na(standardized_ObservedBiomassTable[y,i]))) {
      sumIndex = sumIndex + (log(standardized_ObservedBiomassTable[y,i] / standardized_ModelBiomassTable[y,i])) * (log(standardized_ObservedBiomassTable[y,i] / standardized_ModelBiomassTable[y,i]))
      numObservations = numObservations + 1
    }
  }
  RI = exp(sqrt(sumIndex / numObservations))
  RI_index = i-1
  RI_prediction_table[RI_index] = RI
}

write.table(RI_tuning_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv",sep=",",quote=FALSE,append=TRUE)
write.table(RI_prediction_table,file="/home/rgamble/Desktop/R/Atlantis Skill Assessment/v3 Outputs/Landings_Skill_table.csv", sep=",",quote=FALSE,append=TRUE)
