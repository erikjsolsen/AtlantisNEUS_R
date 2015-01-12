### Skill assessment
#  - Calculating metrics (correlations)
#  - creating heat-maps
#  - PCA analysis
#
# By: Erik Olsen
# Date: 30.06.2014
# Updated: 07.11.2014


### Setting up libraries
library(ggplot2)
library(reshape2)
library(data.table)
library(plyr)
library(RColorBrewer)
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")
library(grid)

# Sourcing get.Ind code
source("~/Documents/G-copy/R/get_indicators_4Erik/get_indicators_4Erik_mod.R")



### Importing data files and species codes
setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/skill assessment/data") 



###### ##### #####
###  Importing data

#biomass/survey data (normalized:  x-X/X (observed - mean/mean)
model_biom<-read.table("Modeled_Biomass_std.csv", head=TRUE, sep=",")
survey_biom<-read.table("Observed_Biomass_std.csv", head=TRUE, sep=",")
species_codes<-read.csv("NEUS_species_codes.csv")
mef<-read.table("MEF_table_initial.csv", head=TRUE, sep=",", row.names=1)

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

metric_ecoind_all<-read.csv("metric_ecoind_all.csv",head=TRUE, sep=",",nrows=16,  row.names=1)
metric_ecoind_74_03<-read.csv("metric_ecoind_74_03.csv",head=TRUE, sep=",", nrows=16, row.names=1)
metric_ecoind_65_74<-read.csv("metric_ecoind_65_74.csv",head=TRUE, sep=",", nrows=16, row.names=1)
metric_ecoind_75_84<-read.csv("metric_ecoind_75_84.csv",head=TRUE, sep=",", nrows=16, row.names=1)
metric_ecoind_85_94<-read.csv("metric_ecoind_85_94.csv",head=TRUE, sep=",", nrows=16, row.names=1)
metric_ecoind_95_04<-read.csv("metric_ecoind_95_04.csv",head=TRUE, sep=",", nrows=16, row.names=1)

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


#########
### BIOMASS and LANDINGS PLOTS OVER TIME
# Sean Luceys code was used in MS


#########  ECOL INDICATORS
# Seabirds not included in TEP of TotalBio group
# changes made to get_indicators_4Erik_mod.R 
### ECOLOGICAL INDICATORS for model data
setwd("~/Documents/G-copy/R/get_indicators_4Erik")
path=getwd()
#bzero<-1
bzero<-Bio_m[1,2:36]
EcoInd_model<-get.Ind(Bio_m[1,],Catch_m[1,],bzero,path)
EcoInd_model[24]<-1964
names(EcoInd_model)[24]<-"Year"
for (i in 2:nrow(Bio_m)) {
  IndYear<-get.Ind(Bio_m[i,],Catch_m[i,],bzero,path)
  IndYear[24]<-Bio_m$Year[i]
  EcoInd_model<-rbind(EcoInd_model, IndYear, deparse.level = 0)
}


#### ECOLOGICAL INDICATORS for observed / landings data
bzero<-Bio_o[1,2:27]
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



#######
### Calculate metrics 
### CORRELATIONS
# r- Spearman, Pearson

# create subsets of data tuned vs predicted
model_biom_pred<-subset(model_biom, Year>2004)
survey_biom_pred<-subset(survey_biom, Year>2004)

#same for landings-daa
model_land_pred<-subset(landings_model, Year>2004)
obs_land_pred<-subset(landing_obs, Year>2004)


#empty data frame for correlation analyses
cortab_bb<-data.frame( S_P=numeric(23),  P_P=numeric(23),  K_P=numeric(23), row.names=NEUS.names)

cortab_ll<-data.frame( S_P=numeric(22), P_P=numeric(22), K_P=numeric(22), row.names=NEUS.names.landings)

# BIOMASS: Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
for (i in 1:length(NEUS.names)){
  result<-cor.test(model_biom_pred[,NEUS.names[i]], survey_biom_pred[,NEUS.names[i]], method="spearman")
  cortab_bb[i,1]<-result$estimate
  result<-cor.test(model_biom_pred[,NEUS.names[i]], survey_biom_pred[,NEUS.names[i]], method="pearson")
  cortab_bb[i,2]<-result$estimate
  result<-cor.test(model_biom_pred[,NEUS.names[i]], survey_biom_pred[,NEUS.names[i]], method="kendall")
  cortab_bb[i,3]<-result$estimate
}

cortab_bb<-cortab_bb[2:23,]
write.csv(cortab_bb, file="correlations_biomass_pred.csv")


#LANDINGS: Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
for (i in 1:length(NEUS.names.landings)){
  result<-cor.test(model_land_pred[,NEUS.names.landings[i]], obs_land_pred[,NEUS.names.landings[i]], method="spearman")
  cortab_ll[i,1]<-result$estimate
  result<-cor.test(model_land_pred[,NEUS.names.landings[i]], obs_land_pred[,NEUS.names.landings[i]], method="pearson")
  cortab_ll[i,2]<-result$estimate
  result<-cor.test(model_land_pred[,NEUS.names.landings[i]], obs_land_pred[,NEUS.names.landings[i]], method="kendall")
  cortab_ll[i,3]<-result$estimate
}

write.csv(cortab_ll, file="correlations_landings_pred.csv")
cortab_ls<-cortab_ll[2:22,]



### Creating joint tables of all metrics
# predicted values
metrics_comb_biom_pred<-cbind(cortab_bb, skill_table_biomass[,c(2,4,6,8)])
metrics_comb_land_pred<-cbind(cortab_ls, skill_table_landings[,c(2,4,6,8)])
# hindcast values
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


#export table
write.csv(skill_metrics_combined, file="skill_metrics_combined.csv")
write.csv(skill_metrics_combined_landings, file="skill_metrics_combined_landings.csv")


### COMPARE Predicted versus Hindcast
# set up relevant data-frames
skill_metrics_comparison<-data.frame(Spearman=numeric(22), Pearson=numeric(22),  MEF=numeric(22),  AAE=numeric(22), RMSE=numeric(22), row.names=NEUS.names.rows)
skill_metrics_comparison_landings<-data.frame(Spearman=numeric(21), Pearson=numeric(21),  MEF=numeric(21),  AAE=numeric(21), RMSE=numeric(21), row.names=NEUS.names.rows.l)

#Spearman rank correlation (non-parametric) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,1]<-((1-skill_metrics_combined[,1])/(1-skill_metrics_combined[,2]))
skill_metrics_comparison_landings[,1]<-((1-skill_metrics_combined_landings[,1])/(1-skill_metrics_combined_landings[,2]))

#Pearson correlation  (parametric) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,2]<-((1-skill_metrics_combined[,3])/(1-skill_metrics_combined[,4]))
skill_metrics_comparison_landings[,2]<-((1-skill_metrics_combined_landings[,3])/(1-skill_metrics_combined_landings[,4]))

# Modelling efficiency (MEF) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,3]<-(1-skill_metrics_combined[,7])/(1-skill_metrics_combined[,8])
skill_metrics_comparison_landings[,3]<-((1-skill_metrics_combined_landings[,7])/(1-skill_metrics_combined_landings[,8]))

# Average absolute error (AAE) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,4]<-(skill_metrics_combined[,11])/(skill_metrics_combined[,12])
skill_metrics_comparison_landings[,4]<-((1-skill_metrics_combined_landings[,11])/(1-skill_metrics_combined_landings[,12]))

# Root Mean Squared Error (RMSE)) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,5]<-(skill_metrics_combined[,13])/(skill_metrics_combined[,14])
skill_metrics_comparison_landings[,5]<-((1-skill_metrics_combined_landings[,13])/(1-skill_metrics_combined_landings[,14]))


## All skill metrics melting & categories
# melting all skill metrics
skills.melt<-skill_metrics_comparison
skills.melt[6]<-rownames(skills.melt)
skills.melt<-melt(skills.melt, id=c("V6"))
# landings data
skills.melt.l<-skill_metrics_comparison_landings
skills.melt.l[6]<-rownames(skills.melt.l)
skills.melt.l<-melt(skills.melt.l, id=c("V6"))

#creating breaks - skills - Biomass
brks<-classIntervals(skills.melt$value, n=5, style="fixed", fixedBreaks=c(0.1, 0.5, 0.9, 1.1, 1.5, 19)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(skills.melt$value, brks, all.inside=TRUE) #assign categories

skills.m<-cbind(skills.melt, catVar) #join data & categories
a<-colnames(skills.m)
a[1]<-c("Species")
colnames(skills.m)<-a # give new name to variables



# creating breaks - Landings
brks<-classIntervals(skills.melt.l$value, n=5, style="fixed", fixedBreaks=c(-13, 0.5, 0.9, 1.1, 1.5, 65)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(skills.melt.l$value, brks, all.inside=TRUE) #assign categories

skills.m.l<-cbind(skills.melt.l, catVar) #join data & categories
a<-colnames(skills.m.l)
a[1]<-c("Species")
colnames(skills.m.l)<-a # give new name to variables



# Create labels from break values
intLabels <- matrix(1:(length(brks)-1))
intLabels[1,1]<-c("Hindcast much better fit than Forecast (F<50% of H)")
intLabels[2,1]<-c("Hindcast better than Forecast (F is10%-50% of H)")
intLabels[3,1]<-c("Hindcast and forecast are similar (+/- 10%)")
intLabels[4,1]<-c("Forecast better than Hindcast (F is 110%-150% of H)")
intLabels[5,1]<-c("Forecast much better than Hindcast (F>150% of H")

#making plot - biomass
heatplot1 <- ggplot(skills.m, aes(variable, Species)) + geom_tile(aes(fill = factor(catVar)), colour = "white") + scale_fill_brewer(palette="PRGn", name="Legend", label=intLabels) + theme( axis.text.x = element_text(angle = -45, hjust = 0, colour = "grey20", size=14))+ theme( axis.text.y = element_text(colour = "grey20", size=14)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + theme(legend.title = element_text(size=16, face="bold"))+ theme(legend.text = element_text(colour="grey20", size = 14, face = "bold"))

ggsave("SkillsHeatPlot_biomass.pdf", scale = 1, dpi = 400)

#making plot - landings
heatplot <- ggplot(skills.m.l, aes(variable, Species)) + geom_tile(aes(fill = factor(catVar)), colour = "white") + scale_fill_brewer(palette="PRGn", name="Legend", label=intLabels) + theme( axis.text.x = element_text(angle = -45, hjust = 0, colour = "grey20", size=14))+ theme( axis.text.y = element_text(colour = "grey20", size=14)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + theme(legend.title = element_text(size=16, face="bold"))+ theme(legend.text = element_text(colour="grey20", size = 14, face = "bold"))

ggsave("SkillsHeatPlot_landings.pdf", scale = 1, dpi = 400)


##### Heatplot using all metrics
# See "indicator_run.R"



########
#### TAylor Diagrams & TARGET diagrams

# one versionusing plotrix package
library(plotrix)
pdf(file="taylor test.pdf")
#taylor.diagram(survey_biom$YT, model_biom$YT)
#taylor.diagram(survey_biom$Haddock, model_biom$Haddock, add=TRUE, col="blue")

#plotting all model- observed comparisons in one plot
taylor.diagram(subset(survey_biom, Year<2004)[,2], subset(model_biom, Year<2004)[,2], normalize=TRUE, col="orange",  main="Atlantis Modelled versus Survey data")
taylor_colors<-rainbow(length(NEUS.names)-1)
for (i in 3:length(NEUS.names)){
  taylor.diagram(subset(survey_biom, Year<2004)[,i], subset(model_biom, Year<2004)[,i], add=TRUE, normalize=TRUE, col="orange")
}

for (i in 2:length(NEUS.names)){
  taylor.diagram(subset(survey_biom, Year>=2004)[,i], subset(model_biom, Year>=2004)[,i], add=TRUE, normalize=TRUE, col="slateblue")
}


dev.off()

## another version using openair package
# create a joint data-frame of model and survey data
s_names<-colnames(survey_biom)
s_names<-paste(s_names, "S", sep="_")
m_names<-colnames(model_biom)
m_names<-paste(m_names, "M", sep="_")
colnames(survey_biom)<-s_names
colnames(model_biom)<-m_names
survey_model<-cbind(survey_biom, model_biom)

library(openair)

# this function only allows single comparisons, or at most 1 observed and 2 models - Disregard

TaylorDiagram(survey_model, obs="FDS_S", mod="FDS_M")
TaylorDiagram(survey_model, obs="FDO_S", mod="FDO_M")
TaylorDiagram(survey_model, obs=c("FDS_S", "FDO_S"), mod=c("FDS_M","FDO_M")

#######
### Multivariate stats: PCAs etc

# Create dataset useful for multivariate analysis
# Biomass data-set
biom_hind<-cbind(skill_metrics_combined_[1], skill_metrics_combined[3],skill_metrics_combined[5],skill_metrics_combined[7],skill_metrics_combined[9],skill_metrics_combined[11], skill_metrics_combined[13])
biom_fore<-cbind(skill_metrics_combined[2], skill_metrics_combined[4],skill_metrics_combined[6],skill_metrics_combined[8],skill_metrics_combined[10],skill_metrics_combined[12], skill_metrics_combined[14])

#Landings data-set
landings_hind<-cbind(skill_metrics_combined_landings[1], skill_metrics_combined_landings[3],skill_metrics_combined_landings[5],skill_metrics_combined_landings[7],skill_metrics_combined_landings[9],skill_metrics_combined_landings[11], skill_metrics_combined_landings[13])
landings_fore<-cbind(skill_metrics_combined_landings[2], skill_metrics_combined_landings[4],skill_metrics_combined_landings[6],skill_metrics_combined_landings[8],skill_metrics_combined_landings[10],skill_metrics_combined_landings[12], skill_metrics_combined_landings[14])

#giving simpler variable names
met_names<-c("S", "P", "K", "MEF", "AE", "AAE", "RMSE" )
colnames(biom_hind)<-met_names
colnames(biom_fore)<-met_names
colnames(landings_hind)<-met_names
colnames(landings_fore)<-met_names

##PCA analysis
pc_biom_hind<-prcomp(biom_hind, scale=TRUE)
pc_biom_fore<-prcomp(biom_fore, scale=TRUE)
pc_landings_hind<-prcomp(landings_hind, scale=TRUE)
pc_landings_fore<-prcomp(landings_fore, scale=TRUE)

#pca<-prcomp(biom_hind, scale=TRUE)

#summary PCA outputs
summary(pc_biom_fore)
summary(pc_biom_hind)
summary(pc_landings_fore)
summary(pc_landings_hind)

#plotting PCA outputs as biplots
biplot(pc_biom_hind)
biplot(pc_biom_fore)
biplot(pc_landings_fore)
biplot(pc_landings_hind)

#extract loading data and combine in one data frame
B_H_load<-data.frame(varnames=rownames(pc_biom_hind$rotation),BL="Biomass",HF="Hindcast", pc_biom_hind$rotation)
B_F_load<-data.frame(varnames=rownames(pc_biom_fore$rotation),BL="Biomass",HF="Forecast", pc_biom_fore$rotation)
L_H_load<-data.frame(varnames=rownames(pc_landings_hind$rotation),BL="Landings",HF="Hindcast", pc_landings_hind$rotation)
L_F_load<-data.frame(varnames=rownames(pc_landings_fore$rotation),BL="Landings",HF="Forecast", pc_landings_fore$rotation)

load_all<-rbind(B_F_load,B_H_load,L_F_load,L_H_load)

#plotting panelled ggplot
layout(matrix(1), widths = lcm(15), heights = lcm(15))
load_plot<-ggplot(load_all, aes(PC1, PC2)) + coord_equal() + geom_text(data=load_all, aes(x=PC1, y=PC2, label=varnames), size = 5, vjust=1, color="firebrick3") + geom_segment(data=load_all, aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="grey40") 
load_plot + facet_grid(HF ~ BL) + theme_bw() + theme(strip.text.x = element_text(size=14, face="bold", color="white"), strip.text.y = element_text(size=14, face="bold", color="white"), strip.background = element_rect( fill="grey50")) + ggtitle("Loadings for model skill-metrics") + theme(plot.title = element_text(size = rel(1.5), face = "bold")) + coord_fixed(ratio = 2/3)

ggsave("PCA_loadings_all.pdf", scale =1,  dpi = 600)


##plotting PCA loadings 
# biomass forecast
datapc <- data.frame(varnames=rownames(pc_biom_fore$rotation), pc_biom_fore$rotation)
plot<-ggplot(datapc)

plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=PC1, y=PC2, label=varnames), size = 5, vjust=1, color="firebrick4")
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="firebrick1") +  ggtitle("Biomass Forecast, loadings plot") + theme(plot.title = element_text(size = rel(2), colour = "midnightblue"))
plot
ggsave("PCA_load_biom_fore.pdf", scale = 1, dpi = 400)

#biomass hindcast
datapc <- data.frame(varnames=rownames(pc_biom_hind$rotation), pc_biom_hind$rotation)
plot<-ggplot(datapc)

plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=PC1, y=PC2, label=varnames), size = 5, vjust=1, color="firebrick4")
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="firebrick1") +  ggtitle("Biomass Hindcast, loadings plot") + theme(plot.title = element_text(size = rel(2), colour = "midnightblue"))
plot
ggsave("PCA_load_biom_hind.pdf", scale = 1, dpi = 400)

#landings fore
datapc <- data.frame(varnames=rownames(pc_landings_fore$rotation), pc_landings_fore$rotation)
plot<-ggplot(datapc)

plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=PC1, y=PC2, label=varnames), size = 5, vjust=1, color="firebrick4")
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="firebrick1") +  ggtitle("Landings Forecast, loadings plot") + theme(plot.title = element_text(size = rel(2), colour = "midnightblue"))
plot
ggsave("PCA_load_land_fore.pdf", scale = 1, dpi = 400)

# landings hindcast
datapc <- data.frame(varnames=rownames(pc_landings_hind$rotation), pc_landings_hind$rotation)
plot<-ggplot(datapc)

plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=PC1, y=PC2, label=varnames), size = 5, vjust=1, color="firebrick4")
plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="firebrick1") +  ggtitle("Landings hindcast, loadings plot") + theme(plot.title = element_text(size = rel(2), colour = "midnightblue"))
plot
ggsave("PCA_load_land_hind.pdf", scale = 1, dpi = 400)
