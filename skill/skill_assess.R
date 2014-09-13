### Skill assessment
#  - Calculating metrics
#  - plotting biomass
#  - creating heat-maps
#
# By: Erik Olsen
# Date: 30.06.2014
# Updated: 04.09.2014
# Using Robs normalized data sets
# adding catch (landings data)


### Setting up libraries
library(ggplot2)
library(reshape2)
library(data.table)
library(plyr)
library(RColorBrewer)
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")
library(grid)



### Don' use any more
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

# - OLD not working 
#for (i in 1:length(species_codes)) {
#  biom_plot<-ggplot(survey_biom, aes(x=Year, y=species_codes[i])) + geom_line(color="red") + theme_bw()
#  biom_plot + geom_line(data=model_biom, aes(x=Year, y=species_codes[i]), color="blue")
#}

#biom_plot<-ggplot(survey_biom, aes(x=Year, y=species_codes[i])) + geom_line(color="red") + theme_bw()
#biom_plot + geom_line(data=model_biom, aes(x=Year, y=species_codes_used[i]), color="blue")


## Make flattened file
survey.melt<-melt(survey_biom, id=c("Year"))
model.melt<-melt(model_biom, id=c("Year"))
landings_mod.melt<-melt(landings_model, id=c("Year"))
landings_obs.melt<-melt(landing_obs, id=c("Year"))

## Faceted plots
#BIOMASS survey/model plots 
facet_biom_plot<-ggplot(survey.melt, aes(x=Year, y=value)) + geom_line(color="red") + theme_bw() + facet_wrap(~variable, scales = "free_y")

facet_biom_plot + geom_segment(aes(x = 2004, y = 0, xend = 2013, yend = 0)) 
facet_biom_plot +  geom_line(data=model.melt, aes(x=Year, y=value), color="blue") + geom_segment(aes(x = 2004, y = -2, xend = 2013, yend = -2)) + scale_y_continuous(breaks=NULL)+ scale_x_continuous(breaks=c(1964,2004,2013))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(paste("BIOMASS_model_survey_faceted.png"), scale = 1, dpi = 400)


#LANDINGS survey/model plots 
facet_land_plot<-ggplot(landings_obs.melt, aes(x=Year, y=value)) + geom_line(color="red") + theme_bw() + facet_wrap(~variable, scales = "free_y")
#facet_land_plot<-ggplot(landings_obs.melt, aes(x=Year, y=value)) + geom_rect(color="gray60", fill="gray90", aes(xmin = 2004, ymin = -2, xmax = 2013, ymax = 20000)) + geom_line(color="red") + theme_bw() + facet_wrap(~variable, scales = "free_y")

facet_land_plot + geom_segment(aes(x = 2004, y = 0, xend = 2013, yend = 0)) 
facet_land_plot +  geom_line(data=landings_mod.melt, aes(x=Year, y=value), color="blue") + geom_segment(aes(x = 2004, y = -2, xend = 2013, yend = -2)) + scale_y_continuous(breaks=NULL)+ scale_x_continuous(breaks=c(1964,2004,2013))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
#facet_land_plot +  geom_line(data=landings_mod.melt, aes(x=Year, y=value), color="blue") + geom_rect(color="gray60", fill="gray90", aes(xmin = 2004, ymin = -2, xmax = 2013, ymax = 20000)) + scale_y_continuous(breaks=NULL)+ scale_x_continuous(breaks=c(1964,2004,2013))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggsave(paste("LANDINGS_model_survey_faceted.png"), scale = 1, dpi = 400)


#######
### Calculate metrics 
# r- Spearman, Pearson

# create subsets of data tuned vs predicted
model_biom_tuned<-subset(model_biom, Year<2005)
model_biom_pred<-subset(model_biom, Year>2004)
survey_biom_tuned<-subset(survey_biom, Year<2005)
survey_biom_pred<-subset(survey_biom, Year>2004)

#same for landings-daa
model_land_tuned<-subset(landings_model, Year<2005)
model_land_pred<-subset(landings_model, Year>2004)
obs_land_tuned<-subset(landing_obs, Year<2005)
obs_land_pred<-subset(landing_obs, Year>2004)


#empty data frame for correlation analyses
cortab<-data.frame(S_T=numeric(23), S_P=numeric(23), P_T=numeric(23), P_P=numeric(23), K_T=numeric(23), K_P=numeric(23), row.names=NEUS.names)

cortab_l<-data.frame(S_T=numeric(22), S_P=numeric(22), P_T=numeric(22), P_P=numeric(22), K_T=numeric(22), K_P=numeric(22), row.names=NEUS.names.landings)

# BIOMASS: Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
for (i in 1:length(NEUS.names)){
  result<-cor.test(model_biom_tuned[,NEUS.names[i]], survey_biom_tuned[,NEUS.names[i]], method="spearman")
  cortab[i,1]<-result$estimate
  result<-cor.test(model_biom_tuned[,NEUS.names[i]], survey_biom_tuned[,NEUS.names[i]], method="pearson")
  cortab[i,3]<-result$estimate
  result<-cor.test(model_biom_tuned[,NEUS.names[i]], survey_biom_tuned[,NEUS.names[i]], method="kendall")
  cortab[i,5]<-result$estimate
  result<-cor.test(model_biom_pred[,NEUS.names[i]], survey_biom_pred[,NEUS.names[i]], method="spearman")
  cortab[i,2]<-result$estimate
  result<-cor.test(model_biom_pred[,NEUS.names[i]], survey_biom_pred[,NEUS.names[i]], method="pearson")
  cortab[i,4]<-result$estimate
  result<-cor.test(model_biom_pred[,NEUS.names[i]], survey_biom_pred[,NEUS.names[i]], method="kendall")
  cortab[i,6]<-result$estimate
}

cortab<-cortab[2:23,]
write.csv(cortab, file="correlations_biomass.csv")


#LANDINGS: Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
for (i in 1:length(NEUS.names.landings)){
  result<-cor.test(model_land_tuned[,NEUS.names.landings[i]], obs_land_tuned[,NEUS.names.landings[i]], method="spearman")
  cortab_l[i,1]<-result$estimate
  result<-cor.test(model_land_tuned[,NEUS.names.landings[i]], obs_land_tuned[,NEUS.names.landings[i]], method="pearson")
  cortab_l[i,3]<-result$estimate
  result<-cor.test(model_land_tuned[,NEUS.names.landings[i]], obs_land_tuned[,NEUS.names.landings[i]], method="kendall")
  cortab_l[i,5]<-result$estimate
  result<-cor.test(model_land_pred[,NEUS.names.landings[i]], obs_land_pred[,NEUS.names.landings[i]], method="spearman")
  cortab_l[i,2]<-result$estimate
  result<-cor.test(model_land_pred[,NEUS.names.landings[i]], obs_land_pred[,NEUS.names.landings[i]], method="pearson")
  cortab_l[i,4]<-result$estimate
  result<-cor.test(model_land_pred[,NEUS.names.landings[i]], obs_land_pred[,NEUS.names.landings[i]], method="kendall")
  cortab_l[i,6]<-result$estimate
}

write.csv(cortab_l, file="correlations_landings.csv")
cortab_ls<-cortab_l[2:22,]

## Creating one joint table of all metrics
skill_metrics_combined<-cbind(cortab, skill_table_biomass)
skill_metrics_combined_landings<-cbind(cortab_ls, skill_table_landings)

#export table
write.csv(skill_metrics_combined, file="skill_metrics_combined.csv")
write.csv(skill_metrics_combined_landings, file="skill_metrics_combined_landings.csv")

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

#export skill metrics table 

# Calculing predicted Correlation Coeff. as fraction of Tuned
#correlations<-data.frame(Pearson=numeric(22), Spearman=numeric(22), Kendall=numeric(22),  row.names=NEUS.names)
#correlations$Pearson<-cortab$pred_P/cortab$tuned_P
#correlations$Spearman<-cortab$pred_S/cortab$tuned_S
#correlations$Kendall<-cortab$pred_K/cortab$tuned_K


#### HEATMAP of skill metrics
#### creating heatmap using ggplot
#corr.melt<-correlations
#corr.melt[4]<-rownames(corr.melt)
#corr.melt<-melt(corr.melt, id=c("V4"))


#creating breaks - correlations
#brks<-classIntervals(corr.melt$value, n=3, style="fixed", fixedBreaks=c(-80, 0, 1, 5)) #define categories
#brks <- round(brks$brks,digits=2) #round
#catVar<-findInterval(corr.melt$value, brks, all.inside=TRUE) #assign categories

#corr.m<-cbind(corr.melt, catVar) #join data & categories
#a<-colnames(corr.m)
#a[1]<-c("Species")
#colnames(corr.m)<-a # give new name to variables
#corr.m<-subset(corr.m, Species!="Year")


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
