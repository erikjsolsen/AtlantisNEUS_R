# R-script for creating Heat-map /Quilt plot of table of variables vs Scenarios
# FIRST: must create at table of Variables vs. Scenarios using "running biomultiplot.r" script
# 25/3/2014
# By Erik Olsen

library(ggplot2)
library(reshape2)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Analysis") #output catalogue from biomultiplot analysis

SEY<-read.table("ScenarioEndYears.csv", sep=";") #read data table to variable

SEY2<-SEY #make copy of SEY

for (i in 2:length(SEY2)){
  SEY2[i]<-SEY2[i]/SEY2[1] #divide each Scenario by base-case  
}

#Make heatmap like plot using ggplot2
SEY2[20]<-rownames(SEY2) #include variables as a column
SEY2<-SEY2[2:20]
aa<-c(1, 6,10, 14, 19) # columns w 100% closure - selct only scenarios yielding real results
SEY2<-SEY2[aa] # create a subset w only real scenarios

SEY.melt<-melt(SEY2, id=c("V20"))

#create categories
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")

brks<-classIntervals(SEY.melt$value, n=7, style="fixed", fixedBreaks=c(0, 0.95, 1.05, 2, 3, 4, 5, 20)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(SEY.melt$value, brks, all.inside=TRUE) #assign categories



SEY.m<-cbind(SEY.melt, catVar) #join data & spatial info
a<-colnames(SEY.m)
a[1]<-c("Variable")
colnames(SEY.m)<-a # give new name to variables

# Create labels from break values
intLabels <- matrix(1:(length(brks)-1))
for(i in 1:length(intLabels )){intLabels [i] <- paste(as.character(brks[i]),"-",as.character(brks[i+1]))}
intLabels[1,1]<-c("< 0.8")
intLabels[7,1]<-c(">5")

#making plot
heatplot <- ggplot(SEY.m, aes(variable, Variable)) + geom_tile(aes(fill = catVar), colour = "white") + scale_fill_gradientn(colours=brewer.pal(7, "PiYG"), guide="legend", label=intLabels) + theme( axis.text.x = element_text(angle = 330, hjust = 0, colour = "grey50"))

#guide="legend", label=intLabels

heatplot
ggsave("ScenarioHeatPlots.pdf", scale = 1, dpi = 400)