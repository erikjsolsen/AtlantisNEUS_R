# ' Working Script: Mapping spatial scenarios
#' @author Erik Olsen
#' @docType Working Script
#' @note date created: 24.09.2015
#' 


#' -----------------
#' DEPENDENCIES
#' must run the 'mappingNEUS.R' script first

#' -----------------
#' LIBRARIES
library(ggplot2)
library(reshape2)
library(classInt)

#' -----------------
#' IMPORT BOX DATA FILES
setwd("~/Documents/Research/Atlantis NEUS Spatial")

slist<-c("GoM10", "GoM20", "GoM50", "GoM100", "All20", "All50", "All100", "CW50", "CW100", "GB10", "GB20", "GB50")

#' Biomass files
base.case.box<-read.table("Base_case_new/output/neusDynEffort_BASE_BoxBiomass.txt", header=TRUE, sep=" ")
GoM10<-read.table("GoM10/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
GoM100<-read.table("GoM100/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
GoM20<-read.table("GoM20/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
GoM50<-read.table("GoM50/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
All20<-read.table("ALL20/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
All50<-read.table("ALL50/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
All100<-read.table("ALL100/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
CW50<-read.table("CW50/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
CW100<-read.table("CW100/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
GB10<-read.table("GB10/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
GB20<-read.table("GB20/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")
GB50<-read.table("GB50/neusDynEffort_spatial_BoxBiomass.txt", header=TRUE, sep=" ")

#' Give real world names
names.a<-colnames(base.case.box)
func.group.names<-read.table("FuncGroupNamesInPlotOrderNEUS_GF20130416.csv",as.is = TRUE,header=TRUE,sep=",")
for (j in 2:length(names.a)) {
  if (is.na(match(names.a[j], func.group.names$CODE))){ 
    names.a[j]<-names.a[j] }
  else { 
    names.a[j]<-func.group.names$EMOCCName[match(names.a[j], func.group.names$CODE)] }
}

colnames(base.case.box)<-names.a

for (i in 1:length(slist)){
  x<-get(slist[i])
  colnames(x)<-names.a
  assign(slist[i], x)
}

#' no Catch-box-files are available
#' 

#' -------------------
#' PLOTTING ON MAP of NEUS Atlantis area

for (i in 1:length(slist)){ 
  #' Creating data sets for plotting
  bc<-base.case.box[c(3,4,5,6,11,13,14,15,16,17,18,21,22,23,25,26,27,28,31,33)]
  sc<-get(slist[i])[c(3,4,5,6,11,13,14,15,16,17,18,21,22,23,25,26,27,28,31,33)]
  rel.bio<-cbind(base.case.box[1:2], sc/bc)
  bc.melt<-melt(subset(rel.bio, Time==18249.5), id = c("Box"))
  bc.melt<-subset(bc.melt, variable!="Time")
  colnames(bc.melt)<-c("id", "variable", "data")
  plot.merge<- merge(NEUS.f, bc.melt, by.x = "id")
  
  #' create categories
  brks<-classIntervals(plot.merge$data, n=3, style="fixed", fixedBreaks=c(min(plot.merge$data, na.rm=TRUE), 0.9, 0.95, 1.05,  1.1, max(plot.merge$data, na.rm=TRUE))) 
  brks <- round(brks$brks,digits=2) 
  catVar<-findInterval(plot.merge$data, brks, all.inside=TRUE) 
  plot.merge.cat<-cbind(plot.merge, catVar) 
  
  #' actual plotting step
  map<-ggplot(data = plot.merge.cat, aes(x = long, y = lat, fill = factor(catVar), group = group)) +   geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() + labs(x = "", y = "", fill = "Spatial/Base") +   ggtitle(paste("BIOMASS Comparison base case vs spatial case: \n", slist[i])) +facet_wrap(~variable, as.table=FALSE) + scale_fill_brewer(palette="RdYlGn", label=c("<90%", "90-95%", "95-105%", "105-110%", ">110%")) + theme_bw() + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) + theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +  theme(strip.text.x = element_text(size=7)) 
  
  map #plot maps
  setwd("/Users/eriko/Documents/Research/Atlantis NEUS Spatial/plots/maps")
  ggsave(paste(slist[i],"ScenMap.png", sep=""), scale = 1, dpi = 400)
}
