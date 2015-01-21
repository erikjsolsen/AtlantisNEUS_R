#Atlantis_skill_comparisons_indicator_plot.r
#Plot ecological indicators for Atlantis pre/post calibration
#for skill assessment
#11/14
#SML

#User parameters
window <- T
if(window == T){
  r.dir    <- "L:\\Rworkspace\\"
  data.dir <- "L:\\ATLANTIS\\Skill_comparisons\\"
  out.dir  <- "L:\\ATLANTIS\\Skill_comparisons\\"
  memory.limit(4000)
}
if(window == F){
  r.dir    <- "slucey/Rworkspace/"
  data.dir <- "slucey/ATLANTIS/Skill_comparisons/"
  out.dir  <- "slucey/ATLANTIS/Skill_comparisons/"
}

greens  <- c("#B2DF8A", "#33A02C")

#-------------------------------------------------------------------------------
#Required packages
library(data.table)

#-------------------------------------------------------------------------------
#User created functions
ztrans<-function(x){
  meanx<-mean(x,na.rm=T)
  sdx<-sd(x,na.rm=T)
  z<-(x-meanx)/sdx
  return(z)
}

#-------------------------------------------------------------------------------
#Indicators
#Get data
load(paste(data.dir, 'EcoIndicators_modelv2.RData', sep = ''))
load(paste(data.dir, 'EcoIndicators_observedv2.RData', sep = ''))

model <- as.data.table(EcoInd_model)
obs   <- as.data.table(EcoInd_obs)

#clean up missing indicators
model[, V23 := NULL]
model <- model[Year < 2014, ]
obs[, c(paste('V', 17:23, sep = '')) := NULL]

#Variable names for looping
EcoInd <- data.table(varname  = c('TotBio', 'TotCat', 'Cat/Bio', 
                                  'FishBio', 'DemPelFish', 'TEPs',
                                  'Seals', 'Whales', 'DemCat', 
                                  'PelCat', 'MTLBio', 'MTLCat',
                                  'Bio/PP', 'DemBio/PP', 'PelBio/PP',
                                  'Cat/PP', 'DemCat/PP', 'PelCat/PP',
                                  'FishCat', 'FishCat/FishBio', 'PropOF',
                                  'Value'), 
                     plotname = c('Total Biomass', 'Total Catch', 'Catch/Biomass', 
                                  'Fish Biomass', 'Demersal/Pelagic Ratio', 'TEPs', 
                                  'Seal Biomass', 'Whale Biomass', 'Demersal Catch', 
                                  'Pelagic Catch', 'MTL system', 'MTL Catch', 
                                  'Biomass/PP', 'Demersal Biomass/PP', 'Pelagic Biomass/PP', 
                                  'Catch/PP', 'Demersal Catch/PP', 'Pelagic Catch/PP', 
                                  'Fish Catch', 'Fish Catch/Fish Biomass', 'Proportion Overfished',
                                  'Value'))

#Standardize indicator data
mod.trans <- data.table(Year = 1964:2013)
for(i in 1:nrow(EcoInd)){
  ind.trans <- data.table(V1 = ztrans(model[, get(EcoInd[i, varname])]))
  setnames(ind.trans, "V1", EcoInd[i, varname])
  mod.trans <- cbind(mod.trans, ind.trans)
}

obs.trans <- data.table(Year = 1964:2013)
for(i in 1:nrow(EcoInd)){
  if(length(which(names(obs) == EcoInd[i, varname])) == 1){
  ind.trans <- data.table(V1 = ztrans(obs[, get(EcoInd[i, varname])]))
  setnames(ind.trans, "V1", EcoInd[i, varname])
  }else{
    ind.trans <- data.table(V1 = rep(NA, nrow(obs.trans)))
    setnames(ind.trans, "V1", EcoInd[i, varname])
  }
  obs.trans <- cbind(obs.trans, ind.trans)
}


#Plot  
png(file = paste(out.dir, "Atlantis_Ecological_Indicators_v3.png", sep = ''), width = 2000, height = 1500, res = 200)
opar <- par(oma = c(4, 4, 2, 1), mar = c(1, 1, 1, 0), mfrow = c(5, 5))

for(i in 1:nrow(EcoInd)){
  y.max <- max(c(mod.trans[, get(EcoInd[i, varname])], 
                 obs.trans[, get(EcoInd[i, varname])]), na.rm = T)
  y.min <- min(c(mod.trans[, get(EcoInd[i, varname])], 
                 obs.trans[, get(EcoInd[i, varname])]), na.rm = T)
  bot.box <- y.min - abs(.5 * y.min)
  top.box <- y.max + .5 * y.max
  
  plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '', ylim = c(y.min, y.max), xlim = c(1964, 2013))
  polygon(c(2004, 2004, 2013, 2013), c(bot.box, top.box, top.box, bot.box), col = 'light grey', lty = 0)
  lines(mod.trans[, list(Year, get(EcoInd[i, varname]))], lwd = 3, col = greens[2])
  lines(obs.trans[, list(Year, get(EcoInd[i, varname]))], lwd = 3, col = greens[1])
  
#   axis(1, cex.axis = 1.5)
#   axis(2, cex.axis = 1.5, at = axTicks(2), labels = axTicks(2) / 1e6, las = T)
  box(lwd = 2)

  mtext(3, text = EcoInd[i, plotname], line = 0.25)
  
  mtext(1, text = 'Year',  line = 1.3, cex = 2, outer = T)
  mtext(2, text = 'Index', line = 1.3, cex = 2, outer = T)
}

dev.off()
