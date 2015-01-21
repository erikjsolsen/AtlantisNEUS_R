#Atlantis_skill_comparisons_biomass_plot.r
#Plot survey data for Atlantis pre/post calibration
#for skill assessment
#9/14
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

blues   <- c("#A6CEE3", "#1F78B4") 
oranges <- c("#FDBF6F", "#FF7F00")

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
#Biomass
#Get biomass data
load(paste(data.dir, 'Species_groups.RData', sep = ''))
setkey(species, Atcode)
atl.groups <- unique(species)
atl.groups <- atl.groups[, list(Group, Atcode)]

load(paste(data.dir, 'Totbiomass_atcodes.RData', sep = ''))
biomass.mod <- as.data.table(read.csv(paste(data.dir, 'Modeled_Biomass_std.csv', sep = '')))

atcodes <- names(atl.biomass)[which(names(atl.biomass) != 'Year')]

#Standardize the biomass data
atl.trans <- data.table(Year = 1963:2013)
for(i in 1:length(atcodes)){
  sp.trans <- data.table(V1 =ztrans(atl.biomass[, get(atcodes[i])]))
  setnames(sp.trans, "V1", atcodes[i])
  atl.trans <- cbind(atl.trans, sp.trans)
}

#Plot biomass  
png(file = paste(out.dir, "Survey_biomass_v3.png", sep = ''), width = 2000, height = 1500, res = 200)
opar <- par(oma = c(4, 4, 2, 1), mar = c(1, 1, 1, 0), mfrow = c(5, 5))
for(i in 1:length(atcodes)){
  y.max <- max(c(atl.trans[, get(atcodes[i])], biomass.mod[, get(atcodes[i])]), na.rm = T)
  y.min <- min(c(atl.trans[, get(atcodes[i])], biomass.mod[, get(atcodes[i])]), na.rm = T)
  
  plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '', ylim = c(y.min, y.max), xlim = c(1963, 2013))
  polygon(c(2004, 2004, 2013, 2013), c(-20, 20, 20, -20), col = 'light grey', lty = 0)
  lines(atl.trans[, Year],   atl.trans[, get(atcodes[i])],   lwd = 3, col = blues[2])
  lines(biomass.mod[, Year], biomass.mod[, get(atcodes[i])], lwd = 3, col = blues[1])
  
#   axis(1, cex.axis = 1.5)
#   axis(2, cex.axis = 1.5, at = axTicks(2), labels = axTicks(2) / 1e6, las = T)
  box(lwd = 2)

  mtext(3, text = atl.groups[Atcode == atcodes[i], Group], line = 0.25)
  
  mtext(1, text = 'Year',    line = 1.3, cex = 2, outer = T)
  mtext(2, text = 'Biomass', line = 1.3, cex = 2, outer = T)
}

dev.off()

#Landings
load(paste(data.dir, 'Landings_atcodes.RData', sep = ''))
landings.mod <- as.data.table(read.csv(paste(data.dir, 'Modeled_Landings.csv', sep = '')))

atcodes <- atcodes[which(atcodes != 'FMM')]

#Plot landings  
png(file = paste(out.dir, "Landings_v3.png", sep = ''), width = 2000, height = 1500, res = 200)
opar <- par(oma = c(4, 4, 2, 1), mar = c(1, 1, 1, 0), mfrow = c(5, 5))
for(i in 1:length(atcodes)){
  y.max <- max(c(atl.landings[, get(atcodes[i])], landings.mod[, get(atcodes[i])]), na.rm = T)
  y.min <- min(c(atl.landings[, get(atcodes[i])], landings.mod[, get(atcodes[i])]), na.rm = T)
  
  plot(0, 0, type = 'n', axes = F, xlab = '', ylab = '', ylim = c(y.min, y.max), xlim = c(1963, 2013))
  polygon(c(2004, 2004, 2013, 2013), c(-2000, rep(y.max + .5 * y.max, 2), -2000), col = 'light grey', lty = 0)
  lines(atl.landings[, Year], atl.landings[, get(atcodes[i])], lwd = 3, col = oranges[2])
  lines(landings.mod[, Year], landings.mod[, get(atcodes[i])], lwd = 3, col = oranges[1])
  
  #   axis(1, cex.axis = 1.5)
  #   axis(2, cex.axis = 1.5, at = axTicks(2), labels = axTicks(2) / 1e6, las = T)
  box(lwd = 2)
  
  mtext(3, text = atl.groups[Atcode == atcodes[i], Group], line = 0.25)
  
  mtext(1, text = 'Year',     line = 1.3, cex = 2, outer = T)
  mtext(2, text = 'Landings', line = 1.3, cex = 2, outer = T)
}

dev.off()
