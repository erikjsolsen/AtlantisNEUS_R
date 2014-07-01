### Skill assessment
#  - Calculating metrics
#  - plotting biomass
#  - creating heat-maps
#
# By: Erik Olsen
# Date: 30.06.2014
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
load("Totbiomass_atcodes.RData")
survey_biom<-atl.biomass
# must converst survey_biom to tons
survey_biom<-cbind(survey_biom[1], survey_biom[2:23]/1000)
load("Species_groups.RData")
model_biom<-read.table("Modeled_Biomass.csv", head=TRUE, sep=" ")
species_codes<-read.csv("NEUS_species_codes.csv")

## normalizing data
model_biom_means<-as.matrix(colMeans(model_biom[2:23]))
survey_biom_means<-as.matrix(colMeans(survey_biom[2:23]))
model_biom_n<-model_biom[2:23]
survey_biom_n<-survey_biom[2:23]

model_norm<-model_biom_n
for (i in 1:50){
  model_norm[i,]<-model_biom_n[i,]/model_biom_means
  }

survey_norm<-survey_biom_n
for (i in 1:51){
  survey_norm[i,]<-survey_biom_n[i,]/survey_biom_means
}

model_norm<-cbind(model_biom[1], model_norm)
survey_norm<-cbind(survey_biom[1], survey_norm)




###### ##### #####
###  Importing Robs normalized data
### x-X/X (observed - mean/mean)
model_biom<-read.table("Modeled_Biomass_std.csv", head=TRUE, sep=",")
survey_biom<-read.table("Observed_Biomass_std.csv", head=TRUE, sep=",")
species_codes<-read.csv("NEUS_species_codes.csv")
mef<-read.table("MEF_table_initial.csv", head=TRUE, sep=",")

# give real-world species names
NEUS.names<-colnames(model_biom)

for (j in 2:length(NEUS.names)) {
  NEUS.names[j]<-as.character(species_codes$NAME[match(NEUS.names[j], species_codes$CODE)] )
}

colnames(model_biom)<-NEUS.names
colnames(survey_biom)<-NEUS.names


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
cortab<-data.frame(tuned_S=numeric(23), pred_S=numeric(23), tuned_P=numeric(23), pred_P=numeric(23), tuned_K=numeric(23), pred_K=numeric(23), row.names=NEUS.names)

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

write.csv(cortab, file="correlations.csv")

# Calculing predicted Correlation Coeff. as fraction of Tuned
correlations<-data.frame(Pearson=numeric(23), Spearman=numeric(23), Kendall=numeric(23),  row.names=NEUS.names)
correlations$Pearson<-cortab$pred_P/cortab$tuned_P
correlations$Spearman<-cortab$pred_S/cortab$tuned_S
correlations$Kendall<-cortab$pred_K/cortab$tuned_K

#### creating heatmap using ggplot
corr.melt<-correlations
corr.melt[4]<-rownames(corr.melt)
corr.melt<-melt(corr.melt, id=c("V4"))

#creating breaks
brks<-classIntervals(corr.melt$value, n=3, style="fixed", fixedBreaks=c(-80, 0, 1, 5)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(corr.melt$value, brks, all.inside=TRUE) #assign categories

corr.m<-cbind(corr.melt, catVar) #join data & spatial info
a<-colnames(corr.m)
a[1]<-c("Species")
colnames(corr.m)<-a # give new name to variables
corr.m<-subset(corr.m, Species!="Year")

# Create labels from break values
intLabels <- matrix(1:(length(brks)-1))
for(i in 1:length(intLabels )){intLabels [i] <- paste(as.character(brks[i]),"-",as.character(brks[i+1]))}
intLabels[1,1]<-c("Diverging")
intLabels[2,1]<-c("T>P")
intLabels[3,1]<-c("P>T")


#making plot
heatplot <- ggplot(corr.m, aes(variable, Species)) + geom_tile(aes(fill = factor(catVar)), colour = "white") + scale_fill_brewer(palette="Set1", name="Legend", label=intLabels) + theme( axis.text.x = element_text(angle = 0, hjust = 0, colour = "grey20", size=14))+ theme( axis.text.y = element_text(colour = "grey20", size=14)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + theme(legend.title = element_text(size=16, face="bold"))+ theme(legend.text = element_text(colour="grey20", size = 14, face = "bold"))

ggsave("CorrHeatPlot.pdf", scale = 1, dpi = 400)

