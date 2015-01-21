#Atlantis_skill_comparisons_metrics_plot.r
#Plot of metrics for Atlantis skills assessment
#for skill assessment
#1/15
#SML

#User parameters
window <- T
if(window == T){
  r.dir    <- "L:\\Rworkspace\\"
  data.dir <- "L:\\ATLANTIS\\Skill_comparisons\\"
  out.dir  <- "L:\\ATLANTIS\\Skill_comparisons\\"
  memory.limit(4000)
  data.dir <- "C:\\Users\\Sean.Lucey\\Desktop\\"
  out.dir <- data.dir
}
if(window == F){
  r.dir    <- "slucey/Rworkspace/"
  data.dir <- "slucey/ATLANTIS/Skill_comparisons/"
  out.dir  <- "slucey/ATLANTIS/Skill_comparisons/"
}

blues   <- c("#A6CEE3", "#1F78B4") 
greens  <- c("#B2DF8A", "#33A02C")  
oranges <- c("#FDBF6F", "#FF7F00") 

biomass.order <- c("Scallops", "Lobster", "Shallow macrozoobenthos", 
                   "Squid" , "Migratory mesopelagics", "Other pelagics", 
                   "Anadromous small pelagics", "Silver Hake", "Mackerel", 
                   "Herring", "Other demersals", "Goosefish", "YT", 
                   "Other demersal flatfish", "Haddock", "Cod", "White Hake", 
                   "Bluefish", "Prawns", "Spiny Dogfish", "Demersal sharks",  
                   "Skates and rays")  

landing.order <- c("Scallops", "Lobster", "Shallow macrozoobenthos", 
                   "Squid" , "Other pelagics", "Anadromous small pelagics", 
                   "Silver Hake", "Mackerel", "Herring", "Other demersals",  
                   "Goosefish", "YT", "Other demersal flatfish", "Haddock",  
                   "Cod", "White Hake", "Bluefish", "Prawns", "Spiny Dogfish", 
                   "Demersal sharks",  "Skates and rays")  

ind.order     <- c("TotBio", "Seals", "Whales", "TEPs", "FishBio", "DemPelFish", 
                   "TotCat", "DemCat", "PelCat", "FishCat", "CatBio", 
                   "FishCatFishBio", "MTLBio", "MTLCat", "Total", "Value")

#-------------------------------------------------------------------------------
#Required packages
library(data.table)

#-------------------------------------------------------------------------------
#User created functions
mybar <- function(x, mycol = plotcol, ymin, ymax, ...){
  barplot(x, ylim = c(ymin, ymax), beside = T, col = mycol, 
          axes = F, axisnames = F, ...)
  axis(2, las = T)
  box(lwd = 2)
  abline(h = 0)
}

bar.axis <- function(x, space = 0.2, ncat = 1){
  out <- 1:x
  out[1] <- space + 0.5 * ncat
  for(i in 2:x) out[i] <- out[i - 1] + space + ncat
  return(out)
}


#-------------------------------------------------------------------------------
Group.names <- data.table(atgroup   = c('Scallops', 'Lobster', 'Shallow macrozoobenthos',
                                        'Squid', 'Other pelagics', 'Silver Hake',
                                        'Other demersals', 'Goosefish', 
                                        'Anadromous small pelagics', 'YT', 'Haddock',
                                        'Cod', 'Migratory mesopelagics', 'Mackerel',
                                        'Herring', 'Other demersal flatfish',
                                        'White Hake', 'Bluefish', 'Prawns', 
                                        'Spiny Dogfish', 'Demersal sharks',
                                        'Skates and rays'),
                          plotgroup = c('Scallop', 'Lobster', 'Shall. Mac.zooben.', 
                                        'Squid', 'Other Pelagic', 'Silver Hake', 
                                        'Other Demersal', 'Goosefish', 'Anad. sm. pel.', 
                                        'Yellowtail fl.', 'Haddock', 'Atl. Cod', 
                                        'Mig. mesopel.', 'Atl. Mackerel',
                                        'Atl. Herring', 'Oth. Dem. Flatfish', 
                                        'White Hake', 'Bluefish', 'Shrimp', 
                                        'Sp. Dogfish', 'Dem. Shark', 'Skate and Ray'))


Ind.names <- data.table(atgroup   = c("TotBio", "Seals", "Whales", "TEPs", "FishBio", 
                                      "DemPelFish","TotCat", "DemCat", "PelCat", 
                                      "FishCat", "CatBio", "FishCatFishBio", "MTLBio", 
                                      "MTLCat", "Total", "Value"),
                        plotgroup = c('Total Biomass', 'Seals', 'Whales', 'TEPs',
                                      'Fish Biomass', 'Demersal/Pelagic', 
                                      'Total Catch', 'Demersal Catch', 'Pelagic Catch',
                                      'Fish Catch', 'Catch/Biomass', 
                                      'Fish Catch/Fish Bio', 'MTL System', 'MTL Catch',
                                      'Prop. Overfished', 'Value'))

#Biomass--------------------------------------------------------------------------
biomass <- as.data.table(read.csv(paste(data.dir, 'biometrics.csv', sep = '')))
biomass[, names := NULL]
setnames(biomass, 'X', 'atgroup')

plot.table <- data.table(Metric = c('MEF_a', 'MEF_p', 'AE_a', 'AE_p', 'AAE_a', 
                                    'AAE_p', 'RMSE_a', 'RMSE_p', 'S_a', 'S_p'))
for(i in 1:nrow(biomass)){
  t.data <- data.table(SP  = c(biomass[i, MEF_a],
                               biomass[i, MEF_p],
                               biomass[i, AE_a],
                               biomass[i, AE_p],
                               biomass[i, AAE_a],
                               biomass[i, AAE_p],
                               biomass[i, RMSE_a],
                               biomass[i, RMSE_p],
                               biomass[i, S_a],
                               biomass[i, S_p]))
  setnames(t.data, 'SP', as.character(biomass[i, atgroup]))
  plot.table <- cbind(plot.table, t.data)
}

setcolorder(plot.table, c('Metric', biomass.order))

MEF  <- plot.table[Metric %in% c('MEF_a',  'MEF_p'),  
                   which(names(plot.table) != 'Metric'), with = F]
AE   <- plot.table[Metric %in% c('AE_a',   'AE_p'),   
                   which(names(plot.table) != 'Metric'), with = F]
AAE  <- plot.table[Metric %in% c('AAE_a',  'AAE_p'),  
                   which(names(plot.table) != 'Metric'), with = F]
RMSE <- plot.table[Metric %in% c('RMSE_a', 'RMSE_p'), 
                   which(names(plot.table) != 'Metric'), with = F]
S    <- plot.table[Metric %in% c('S_a',    'S_p'),    
                   which(names(plot.table) != 'Metric'), with = F]

plotcol    <- blues
plot.names <- Group.names[atgroup == biomass.order, plotgroup]

png(file = paste(out.dir, 'Atlantis_skill_metrics_biomass.png', sep = ''), 
    height = 2000, width = 2500, res = 300)

par(mfcol = c(5, 1), oma = c(8, 4, 1, 1), mar = c(1, 0, 0, 0))

mybar(as.matrix(AAE), ymin = 0, ymax = 2.4)
legend('topleft', legend = 'AAE', bty = 'n')
legend('topright', legend = c('hindcast', 'forecast'), fill = blues, 
       bty = 'n', ncol = 2)

mybar(as.matrix(AE), ymin = -2.4, ymax = 2.4)
legend('topleft', legend = 'AE', bty = 'n')

mybar(as.matrix(MEF), ymin = -3.2, ymax = 1.4, xpd = F)
legend('topleft', legend = 'MEF', bty = 'n')

mybar(as.matrix(RMSE), ymin = 0, ymax = 1.4)
legend('topleft', legend = 'RMSE', bty = 'n')

mybar(as.matrix(S), ymin = -1.4, ymax = 1.4)
legend('topleft', legend = 'S', bty = 'n')
axis(1, at = bar.axis(22, space = 1, ncat = 2), label = plot.names, las = 2)

dev.off()

#Landings----------------------------------------------------------------------------
landings <- as.data.table(read.csv(paste(data.dir, 'landmetrics.csv', sep = '')))
landings[, names := NULL]
setnames(landings, 'X', 'atgroup')

plot.table <- data.table(Metric = c('MEF_a', 'MEF_p', 'AE_a', 'AE_p', 'AAE_a', 
                                    'AAE_p', 'RMSE_a', 'RMSE_p', 'S_a', 'S_p'))
for(i in 1:nrow(landings)){
  t.data <- data.table(SP  = c(landings[i, MEF_a],
                               landings[i, MEF_p],
                               landings[i, AE_a],
                               landings[i, AE_p],
                               landings[i, AAE_a],
                               landings[i, AAE_p],
                               landings[i, RMSE_a],
                               landings[i, RMSE_p],
                               landings[i, S_a],
                               landings[i, S_p]))
  setnames(t.data, 'SP', as.character(landings[i, atgroup]))
  plot.table <- cbind(plot.table, t.data)
}

setcolorder(plot.table, c('Metric', landing.order))

MEF  <- plot.table[Metric %in% c('MEF_a',  'MEF_p'),  
                   which(names(plot.table) != 'Metric'), with = F]
AE   <- plot.table[Metric %in% c('AE_a',   'AE_p'),   
                   which(names(plot.table) != 'Metric'), with = F]
AAE  <- plot.table[Metric %in% c('AAE_a',  'AAE_p'),  
                   which(names(plot.table) != 'Metric'), with = F]
RMSE <- plot.table[Metric %in% c('RMSE_a', 'RMSE_p'), 
                   which(names(plot.table) != 'Metric'), with = F]
S    <- plot.table[Metric %in% c('S_a',    'S_p'),    
                   which(names(plot.table) != 'Metric'), with = F]

plotcol    <- oranges
plot.names <- Group.names[atgroup == landing.order, plotgroup]

png(file = paste(out.dir, 'Atlantis_skill_metrics_landings.png', sep = ''), 
    height = 2000, width = 2500, res = 300)

par(mfcol = c(5, 1), oma = c(8, 4, 1, 1), mar = c(1, 0, 0, 0))

mybar(as.matrix(AAE), ymin = 0, ymax = 2.4)
legend('topleft', legend = 'AAE', bty = 'n')
legend('topright', legend = c('hindcast', 'forecast'), fill = plotcol, 
       bty = 'n', ncol = 2)

mybar(as.matrix(AE), ymin = -2.4, ymax = 2.4)
legend('topleft', legend = 'AE', bty = 'n')

mybar(as.matrix(MEF), ymin = -3.2, ymax = 1.8, xpd = F)
legend('topleft', legend = 'MEF', bty = 'n')

mybar(as.matrix(RMSE), ymin = 0, ymax = 1.4)
legend('topleft', legend = 'RMSE', bty = 'n')

mybar(as.matrix(S), ymin = -1.4, ymax = 1.4)
legend('topleft', legend = 'S', bty = 'n')
axis(1, at = bar.axis(21, space = 1, ncat = 2), label = plot.names, las = 2)

dev.off()

#Indicators----------------------------------------------------------------------------
indicators <- as.data.table(read.csv(paste(data.dir, 'ecometrics.csv', sep = '')))
indicators[, names := NULL]
setnames(indicators, 'X', 'ind')

plot.table <- data.table(Metric = c('MEF_a', 'MEF_p', 'AE_a', 'AE_p', 'AAE_a', 
                                    'AAE_p', 'RMSE_a', 'RMSE_p', 'S_a', 'S_p'))
for(i in 1:nrow(indicators)){
  t.data <- data.table(SP  = c(indicators[i, MEF_a],
                               indicators[i, MEF_p],
                               indicators[i, AE_a],
                               indicators[i, AE_p],
                               indicators[i, AAE_a],
                               indicators[i, AAE_p],
                               indicators[i, RMSE_a],
                               indicators[i, RMSE_p],
                               indicators[i, S_a],
                               indicators[i, S_p]))
  setnames(t.data, 'SP', as.character(indicators[i, ind]))
  plot.table <- cbind(plot.table, t.data)
}

setcolorder(plot.table, c('Metric', ind.order))

MEF  <- plot.table[Metric %in% c('MEF_a',  'MEF_p'),  
                   which(names(plot.table) != 'Metric'), with = F]
AE   <- plot.table[Metric %in% c('AE_a',   'AE_p'),   
                   which(names(plot.table) != 'Metric'), with = F]
AAE  <- plot.table[Metric %in% c('AAE_a',  'AAE_p'),  
                   which(names(plot.table) != 'Metric'), with = F]
RMSE <- plot.table[Metric %in% c('RMSE_a', 'RMSE_p'), 
                   which(names(plot.table) != 'Metric'), with = F]
S    <- plot.table[Metric %in% c('S_a',    'S_p'),    
                   which(names(plot.table) != 'Metric'), with = F]

plotcol    <- greens
plot.names <- Ind.names[atgroup == ind.order, plotgroup]

png(file = paste(out.dir, 'Atlantis_skill_metrics_indicators.png', sep = ''), 
    height = 2000, width = 2500, res = 300)

par(mfcol = c(5, 1), oma = c(8, 4, 1, 1), mar = c(1, 0, 0, 0))

mybar(as.matrix(AAE), ymin = 0, ymax = 2.4)
legend('topleft', legend = 'AAE', bty = 'n')
legend('topright', legend = c('hindcast', 'forecast'), fill = plotcol, 
       bty = 'n', ncol = 2)

mybar(as.matrix(AE), ymin = -2.4, ymax = 2.4)
legend('topleft', legend = 'AE', bty = 'n')

mybar(as.matrix(MEF), ymin = -3.2, ymax = 1.8, xpd = F)
legend('topleft', legend = 'MEF', bty = 'n')

mybar(as.matrix(RMSE), ymin = 0, ymax = 1.4)
legend('topleft', legend = 'RMSE', bty = 'n')

mybar(as.matrix(S), ymin = -1.4, ymax = 1.4)
legend('topleft', legend = 'S', bty = 'n')
axis(1, at = bar.axis(16, space = 1, ncat = 2), label = plot.names, las = 2)

dev.off()
