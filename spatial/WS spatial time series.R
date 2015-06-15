# ' Working Script: Analyzing spatial time-series scenarios
#' @author Erik Olsen
#' @docType Working Script
#' @note date created: 23.03.2015
#' 



#' -----------------
#' LIBRARIES
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")
library("roxygen2", lib.loc="/Users/eriko/Library/R/3.0/library")
library(reshape2)


#' ----------------------
#' SOURCING FUNCTIONS
#' 
#' Sourcing get.Ind code
source("~/Documents/G-copy/R/get_indicators_4Erik/get_indicators_4Erik_mod.R")



#' ------------------
#' IMPORTING DATA FILES

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/")

slist<-c("GoM10", "GoM20", "GoM50", "GoM100")
slist.catch <- c("GoM10.catch", "GoM20.catch", "GoM50.catch", "GoM100.catch")

#' Biomass files
base.case.box<-read.table("Base_case_new/output/neusDynEffort_BASE_BoxBiomass.txt", header=TRUE, sep=" ")
base.case<-read.table("Base_case_new/output/neusDynEffort_BASE_BiomIndx.txt", header=TRUE, sep=" ")
GoM10<-read.table("GoM10/output/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GoM100<-read.table("GoM100/output/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GoM20<-read.table("GoM20/output/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GoM50<-read.table("GoM50/output/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")

#' Catch files
base.case.catch<-read.table("Base_case_new/output/neusDynEffort_BASE_Catch.txt", header=TRUE)
GoM10.catch<-read.table("GoM10/output/neusDynEffort_spatial_Catch.txt", header=TRUE)
GoM100.catch<-read.table("GoM100/output/neusDynEffort_spatial_Catch.txt", header=TRUE)
GoM20.catch<-read.table("GoM20/output/neusDynEffort_spatial_Catch.txt", header=TRUE)
GoM50.catch<-read.table("GoM50/output/neusDynEffort_spatial_Catch.txt", header=TRUE)

#' ----------------------
#'Exploring catches

codc <- cbind(base.case.catch$FDS, GoM10.catch$FDS, GoM20.catch$FDS, GoM50.catch$FDS,GoM100.catch$FDS) 
summary(codc)

#' ------------------
#' PLOTTING TIME-SERIES OF CATCH & BIOMASS
mypalette<-brewer.pal(5,"Greens")

timeplot<-ggplot(data = base.case, aes(x = as.integer(Time/365+1964), y = FDS)) + geom_line(aes(colour="1")) + geom_line(data=GoM10, aes(x=as.integer(Time/365+1964), y=FDS, colour="2")) + geom_line(data=GoM20, aes(x=as.integer(Time/365+1964), y=FDS, colour="3")) + geom_line(data=GoM50, aes(x=as.integer(Time/365+1964), y=FDS, colour="4"))  + geom_line(data=GoM100, aes(x=as.integer(Time/365+1964), y=FDS, colour="5")) +xlab("Year") + ylab("Atlantic Cod") + ggtitle("Georges Bank, Spatial Management") + theme_bw()
timeplot + scale_colour_manual(values=c("black",mypalette[2:5]), name="Fishery closures", labels=c("No Closure", "10%", "20%", "50%", "100%")) 

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Analysis")
ggsave("Time Series GB Cod Closures.png")


#' ----------------------
#' PLOTTING ECOSYSTEM INDICATORS

path <- c("~/Documents/G-copy/R/get_indicators_4Erik")
components <- c(48)
bzero<-base.case[1,2:components]

#' Indicators BaseCase 
Ind.base<-get.Ind(base.case[1,],base.case.catch[1,],bzero,path, components)
Ind.base[23]<-as.integer(1964)
names(Ind.base)[23]<-"Year"
for (i in 2:nrow(base.case)) {
  IndYear<-get.Ind(base.case[i,],base.case.catch[i,],bzero,path, components)
  IndYear[23]<-as.integer(base.case$Time[i]/365)+1964
  Ind.base<-rbind(Ind.base, IndYear, deparse.level = 0)
}

#' Indicators Scenarios
for (j in 1:length(slist)) {
  IB<-get.Ind(get(slist[j])[1,],get(slist.catch[j])[1,],bzero,path, components)
  IB[23]<-as.integer(1964)
  names(IB)[23]<-"Year"

  for (i in 2:nrow(get(slist[j]))) {
    IndYear<-get.Ind(get(slist[j])[i,],get(slist.catch[j])[i,],bzero,path, components)
    IndYear[23]<-as.integer(get(slist[j])$Time[i]/365)+1964
    IB<-rbind(IB, IndYear, deparse.level = 0)
  }

  assign(paste("Ind.", slist[j], sep=""), IB)
}

#' Time-series plots
Ind.base.m<-melt(Ind.base, id=c("Year"))
colnames(Ind.base.m)<-c("Year", "Comp", "value")
Ind.base.m<-subset(Ind.base.m, Comp!="Year")

ilist<-c("Ind.GoM10", "Ind.GoM20", "Ind.GoM50", "Ind.GoM100")

for (i in 1:length(ilist)){
  MD<-melt(get(ilist[i]), id=c("Year"))
  colnames(MD)<-c("Year", "Comp", "value")
  MD<-subset(MD, Comp!="Year")
  assign(paste(ilist[i], ".m", sep=""), MD)
}

indplot<-ggplot(data=Ind.base.m, aes(x = as.integer(Year+1963), y = value)) + geom_line(aes(colour="1")) + facet_wrap(~Comp, scales="free_y") + theme_bw() + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank())  + xlab(" ") + ylab("") +  theme(strip.text.x = element_text(size=12)) 
indplot
indplot +  geom_line(data=Ind.GoM10.m, aes(x=as.integer(Year+1963), y=value, colour="2")) +  geom_line(data=Ind.GoM20.m, aes(x=as.integer(Year+1963), y=value, colour="3")) +  geom_line(data=Ind.GoM50.m, aes(x=as.integer(Year+1963), y=value, colour="4")) +  geom_line(data=Ind.GoM100.m, aes(x=as.integer(Year+1963), y=value, colour="5")) +  scale_colour_manual(values=c("black",mypalette[2:5]), name="Fishery closures", labels=c("No Closure", "10%", "20%", "50%", "100%")) + ggtitle("GoM Fishery closure")
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Analysis")
ggsave("indicator_time_series_GoM.png")
