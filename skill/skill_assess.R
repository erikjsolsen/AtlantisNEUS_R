### Skill assessment
#  - Calculating metrics
#  - plotting biomass
#  - creating heat-maps
#
# By: Erik Olsen
# Date: 30.06.2014
# Updated: 23.07.2014
# Using Robs normalized data sets


### Setting up libraries
library(ggplot2)
library(reshape2)
library(data.table)
library(plyr)
library(RColorBrewer)
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")



### Don' use any more
### Importing data files and species codes
setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/skill assessment/data") 



###### ##### #####
###  Importing Robs normalized data
### x-X/X (observed - mean/mean)
model_biom<-read.table("Modeled_Biomass_std.csv", head=TRUE, sep=",")
survey_biom<-read.table("Observed_Biomass_std.csv", head=TRUE, sep=",")
species_codes<-read.csv("NEUS_species_codes.csv")
mef<-read.table("MEF_table_initial.csv", head=TRUE, sep=",", row.names=1)

# importing table of skill metrics (RMSE, AE, AAE, RI, MEF) from table provided by Rob
skill_table_biomass<-read.csv("Skill_table_biomass.csv", row.names=1)


# give real-world species names to column heads
NEUS.names<-colnames(model_biom)

for (j in 2:length(NEUS.names)) {
  NEUS.names[j]<-as.character(species_codes$NAME[match(NEUS.names[j], species_codes$CODE)] )
}

colnames(model_biom)<-NEUS.names
colnames(survey_biom)<-NEUS.names

# giving real-world names to rows
NEUS.names.rows<-rownames(skill_table_biomass)

for (j in 1:length(NEUS.names.rows)) {
  NEUS.names.rows[j]<-as.character(species_codes$NAME[match(NEUS.names.rows[j], species_codes$CODE)] )
}

rownames(mef)<-NEUS.names.rows
rownames(skill_table_biomass)<-NEUS.names.rows


### BIOMASS PLOTS OVER TIME
for (i in 1:length(species_codes_used)) {
  biom_plot<-ggplot(survey_biom, aes(x=Year, y=species_codes_used[i])) + geom_line(color="red") + theme_bw()
  biom_plot + geom_line(data=model_biom, aes(x=Year, y=species_codes_used[i]), color="blue")
}

biom_plot<-ggplot(survey_biom, aes(x=Year, y=species_codes_used[i])) + geom_line(color="red") + theme_bw()
biom_plot + geom_line(data=model_biom, aes(x=Year, y=species_codes_used[i]), color="blue")

# Make flattened file
survey.melt<-melt(survey_biom, id=c("Year"))
model.melt<-melt(model_biom, id=c("Year"))

# Faceted survey/model plots 
facet_biom_plot<-ggplot(survey.melt, aes(x=Year, y=value)) + geom_line(color="red") + theme_bw() + facet_wrap(~variable, scales = "free_y")

facet_biom_plot + geom_segment(aes(x = 2004, y = 0, xend = 2013, yend = 0)) 
facet_biom_plot +  geom_line(data=model.melt, aes(x=Year, y=value), color="blue") + geom_segment(aes(x = 2004, y = -2, xend = 2013, yend = -2)) + scale_y_continuous(breaks=NULL)+ scale_x_continuous(breaks=c(1964,2004,2013))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(paste("model_survey_faceted.png"), scale = 1, dpi = 400)

### Calculate metrics
# r- Spearman, Pearson

# create subsets of data tuned vs predicted
model_biom_tuned<-subset(model_biom, Year<2005)
model_biom_pred<-subset(model_biom, Year>2004)
survey_biom_tuned<-subset(survey_biom, Year<2005)
survey_biom_pred<-subset(survey_biom, Year>2004)

#empty data frame for correlation analyses
cortab<-data.frame(S_T=numeric(23), S_P=numeric(23), P_T=numeric(23), P_P=numeric(23), K_T=numeric(23), K_P=numeric(23), row.names=NEUS.names)

#Calculating Correlation Coefficients using both Spearman, Pearson and Kendall methods
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
write.csv(cortab, file="correlations.csv")

## Creating one joint table of all metrics
skill_metrics_combined<-cbind(cortab, skill_table_biomass)

#export table
write.csv(skill_metrics_combined, file="skill_metrics_combined.csv")

skill_metrics_comparison<-data.frame(Spearman=numeric(22), Pearson=numeric(22),  MEF=numeric(22),  AAE=numeric(22), RMSE=numeric(22), row.names=NEUS.names.rows)

#Spearman rank correlation (non-parametric) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,1]<-((1-skill_metrics_combined[,1])/(1-skill_metrics_combined[,2]))

#Pearson correlation  (parametric) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,2]<-((1-skill_metrics_combined[,3])/(1-skill_metrics_combined[,4]))

# Modelling efficiency (MEF) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,3]<-(1-skill_metrics_combined[,7])/(1-skill_metrics_combined[,8])

# Average absolute error (AAE) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,4]<-(skill_metrics_combined[,11])/(skill_metrics_combined[,12])

# Root Mean Squared Error (RMSE)) comparison of Tuned vs Predicted
# values > 1 show that Predicted is better than Tuned
skill_metrics_comparison[,5]<-(skill_metrics_combined[,13])/(skill_metrics_combined[,14])

#export skill metrics table 

# Calculing predicted Correlation Coeff. as fraction of Tuned
correlations<-data.frame(Pearson=numeric(22), Spearman=numeric(22), Kendall=numeric(22),  row.names=NEUS.names)
correlations$Pearson<-cortab$pred_P/cortab$tuned_P
correlations$Spearman<-cortab$pred_S/cortab$tuned_S
correlations$Kendall<-cortab$pred_K/cortab$tuned_K

#### creating heatmap using ggplot
corr.melt<-correlations
corr.melt[4]<-rownames(corr.melt)
corr.melt<-melt(corr.melt, id=c("V4"))

#creating breaks - correlations
brks<-classIntervals(corr.melt$value, n=3, style="fixed", fixedBreaks=c(-80, 0, 1, 5)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(corr.melt$value, brks, all.inside=TRUE) #assign categories

corr.m<-cbind(corr.melt, catVar) #join data & categories
a<-colnames(corr.m)
a[1]<-c("Species")
colnames(corr.m)<-a # give new name to variables
corr.m<-subset(corr.m, Species!="Year")


## All skill metrics melting & categories
# melting all skill metrics
skills.melt<-skill_metrics_comparison
skills.melt[6]<-rownames(skills.melt)
skills.melt<-melt(skills.melt, id=c("V6"))

#creating breaks - skills
brks<-classIntervals(skills.melt$value, n=5, style="fixed", fixedBreaks=c(0.1, 0.5, 0.9, 1.1, 1.5, 19)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(skills.melt$value, brks, all.inside=TRUE) #assign categories

skills.m<-cbind(skills.melt, catVar) #join data & categories
a<-colnames(skills.m)
a[1]<-c("Species")
colnames(skills.m)<-a # give new name to variables




# Create labels from break values
intLabels <- matrix(1:(length(brks)-1))
intLabels[1,1]<-c("T much better than P (P<50% of T)")
intLabels[2,1]<-c("T better than P (P is10%-50% of T)")
intLabels[3,1]<-c("T and P are similar (+/- 10%)")
intLabels[4,1]<-c("P better than T (P is 110%-150% of T)")
intLabels[5,1]<-c("P much better than T (P>150% of T")

#making plot
heatplot <- ggplot(skills.m, aes(variable, Species)) + geom_tile(aes(fill = factor(catVar)), colour = "white") + scale_fill_brewer(palette="PRGn", name="Legend", label=intLabels) + theme( axis.text.x = element_text(angle = -45, hjust = 0, colour = "grey20", size=14))+ theme( axis.text.y = element_text(colour = "grey20", size=14)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + theme(legend.title = element_text(size=16, face="bold"))+ theme(legend.text = element_text(colour="grey20", size = 14, face = "bold"))

ggsave("SkillsHeatPlot.pdf", scale = 1, dpi = 400)



#### TAylor Diagrams & other multivariate stats

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

