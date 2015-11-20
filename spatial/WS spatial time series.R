# ' Working Script: Analyzing spatial time-series scenarios
#' @author Erik Olsen
#' @docType Working Script
#' @note date created: 23.03.2015
#' 
#' @note Updated 24.09.2015 
#' @note Only prints base case and full closure, due to oddities with partial closures. 



#' -----------------
#' LIBRARIES
library("RColorBrewer")
library("roxygen2")
library(reshape2)
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("grid", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")



#' ----------------------
#' SOURCING FUNCTIONS
#' 
#' Sourcing get.Ind code
source("~/AtlantisNEUS_R/spatial/get_indicators_4Erik/get_indicators_4Erik.r")
source('~/AtlantisNEUS_R/multiplot function.R')



#' ------------------
#' IMPORTING DATA FILES

setwd("~/Documents/Research/Atlantis NEUS Spatial")

slist<-c("GoM10", "GoM20", "GoM50", "GoM100", "All20", "All50", "All100", "CW50", "CW100", "GB10", "GB20", "GB50")
slist.catch <- c("GoM10.catch", "GoM20.catch", "GoM50.catch", "GoM100.catch", "All20.catch", "All50.catch", "All100.catch", "CW50.catch", "CW100.catch", "GB10.catch", "GB20.catch", "GB50.catch")

#' Biomass files
base.case.box<-read.table("Base_case_new/output/neusDynEffort_BASE_BoxBiomass.txt", header=TRUE, sep=" ")
base.case<-read.table("Base_case_new/output/neusDynEffort_BASE_BiomIndx.txt", header=TRUE, sep=" ")
GoM10<-read.table("GoM10/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GoM100<-read.table("GoM100/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GoM20<-read.table("GoM20/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GoM50<-read.table("GoM50/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
All20<-read.table("ALL20/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
All50<-read.table("ALL50/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
All100<-read.table("ALL100/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
CW50<-read.table("CW50/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
CW100<-read.table("CW100/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GB10<-read.table("GB10/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GB20<-read.table("GB20/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")
GB50<-read.table("GB50/neusDynEffort_spatial_BiomIndx.txt", header=TRUE, sep=" ")


#' Catch files
base.case.catch<-read.table("Base_case_new/output/neusDynEffort_BASE_Catch.txt", header=TRUE)
GoM10.catch<-read.table("GoM10/neusDynEffort_spatial_Catch.txt", header=TRUE)
GoM100.catch<-read.table("GoM100/neusDynEffort_spatial_Catch.txt", header=TRUE)
GoM20.catch<-read.table("GoM20/neusDynEffort_spatial_Catch.txt", header=TRUE)
GoM50.catch<-read.table("GoM50/neusDynEffort_spatial_Catch.txt", header=TRUE)
All20.catch<-read.table("ALL20/neusDynEffort_spatial_Catch.txt", header=TRUE)
All50.catch<-read.table("ALL50/neusDynEffort_spatial_Catch.txt", header=TRUE)
All100.catch<-read.table("ALL100/neusDynEffort_spatial_Catch.txt", header=TRUE)
CW50.catch<-read.table("CW50/neusDynEffort_spatial_Catch.txt", header=TRUE)
CW100.catch<-read.table("CW100/neusDynEffort_spatial_Catch.txt", header=TRUE)
GB10.catch<-read.table("GB10/neusDynEffort_spatial_Catch.txt", header=TRUE)
GB20.catch<-read.table("GB20/neusDynEffort_spatial_Catch.txt", header=TRUE)
GB50.catch<-read.table("GB50/neusDynEffort_spatial_Catch.txt", header=TRUE)

#' Give real world names
names.a<-colnames(base.case)
names.c<-colnames(base.case.catch)
func.group.names<-read.table("FuncGroupNamesInPlotOrderNEUS_GF20130416.csv",as.is = TRUE,header=TRUE,sep=",")
for (j in 2:length(names.a)) {
  if (is.na(match(names.a[j], func.group.names$CODE))){ 
  names.a[j]<-names.a[j] }
  else { 
    names.a[j]<-func.group.names$EMOCCName[match(names.a[j], func.group.names$CODE)] }
}

for (i in 1:length(slist)){
  x<-get(slist[i])
  colnames(x)<-names.a
  assign(slist[i], x)
}

for (j in :length(names.c)) {
  if (is.na(match(names.c[j], func.group.names$CODE))){ 
    names.c[j]<-names.c[j] }
  else { 
    names.c[j]<-func.group.names$EMOCCName[match(names.c[j], func.group.names$CODE)] }
}

for (i in 1:length(slist.catch)){
  x<-get(slist.catch[i])
  colnames(x)<-names.c
  assign(slist.catch[i], x)
}

#Remove redundant variables and convert time to years
bc<-cbind(base.case[1:48], base.case[96:101])
bc[,1]<-as.integer(bc[,1]/365+1964)

bc.c <- base.case.catch[1:36]
bc.c[,1]<-as.integer(bc.c[,1]/365+1964)

for (i in 1: length(slist)){
  sl<-cbind(get(slist[i])[1:48], get(slist[i])[96:101])
  sl[,1]<-as.integer(sl[,1]/365+1964)
  assign(slist[i], sl)
}

for (i in 1: length(slist.catch)){
  sl<-get(slist.catch[i])[1:36]
  sl[,1]<-as.integer(sl[,1]/365+1964)
  assign(slist.catch[i], sl)
}

#' Extracting the last year for each scenario and putting it in a table
#' Biomass data
biom.end.table<-data.frame()
for (i in 1:length(slist)){ 
  get(slist[i][51,] 
  }




#' ----------------------
#'Exploring catches

#' ------------------
#' PLOTTING TIME-SERIES OF BIOMASS & CATCH
#mypalette<-brewer.pal(5,"Greens")
mypalette<-brewer.pal(7,"Paired")

#'
#' BIOMASS PLOTS

#' make species-specific data sets
names.select<-c(names.a[1:48], names.a[96:101])

for (i in 2:length(names.select)){ 
  #speciestable<-cbind(bc[1], bc[i], GoM20[i], GoM50[i], GB20[i], GB50[i], All20[i], All50[i], All100[i])
  #colnames(speciestable)<-c("Time", "Base", "GoM20", "GoM50", "GB20", "GB50", "All20", "All50", "All100")
  speciestable<-cbind(bc[1], bc[i], All100[i])
  colnames(speciestable)<-c("Time", "Base", "All100")
  assign(names.select[i], speciestable)
}
#' empty list for storage
gg_list <- list()

for (i in 2:length(names.select)){ 

  #Create long data sets using melt
  species.melt<-melt(get(names.select[i]), id.vars=c("Time") )
  

  p <- ggplot(species.melt, aes(x=Time, y=value, group=variable, colour=variable)) + geom_line(size=2) 
  p <- p + xlab("Year") + ylab("Tons - biomass") 
 # p <- p+ scale_colour_manual(values=c("black", mypalette[1:7]), name="% Fishery closures", labels=c("Base Case", "GoM 20%", "GoM 50%", "GB 20%", "GB 50%", "All 20%", "All 50%", "All 100%")) 
  p <- p+ scale_colour_manual(values=c("black", mypalette[1]), name="% Fishery closures", labels=c("Base Case", "All 100%")) 
  p <- p + ggtitle(names.select[i])  + theme(axis.title.x = element_text(face="bold", size=20), axis.title.y = element_text(face="bold", size=20), axis.text.x  = element_text(size=16), axis.text.y  = element_text(size=16), plot.title = element_text(size=24, face="bold"), legend.title = element_text(size=14, face="bold"), legend.text = element_text( size = 14), legend.position="none")
  
  j<-c(i-1)
  gg_list[[j]] <- p
  p
  setwd("/Users/eriko/Documents/Research/Atlantis NEUS Spatial/plots/TS")
  ggsave(paste(names.select[i], "BIOM Spatial MGMNT.png",sep=""))
}


# BIOMASS multiplots of the single plots
#choose species based on Skill assessment work: Cod, Haddock, Spiny Dogfish, Herring, White hake, Lobster
multiplot(gg_list[[11]], gg_list[[14]], gg_list[[16]], gg_list[[2]],gg_list[[3]], gg_list[[29]], cols=3)
# must save manually using Plots menue (choose 1600 w)


#' 
# CATCH Plots

#' make catch specific data set
names.select.c<-names(GoM10.catch)

for (i in 2:length(names.select.c)){ 
 # catchtable<-cbind(bc.c[1], bc.c[i], GoM20.catch[i], GoM50.catch[i], GB20.catch[i], GB50.catch[i], All20.catch[i], All50.catch[i], All100.catch[i])
  # colnames(catchtable)<-c("Time", "Base", "GoM20", "GoM50", "GB20", "GB50", "All20", "All50", "All100")
  catchtable<-cbind(bc.c[1], bc.c[i], All100.catch[i])
  colnames(catchtable)<-c("Time", "All100")
  assign(names.select.c[i], catchtable)
}

#' empty list for storage
gg_list <- list()

for (i in 2:length(names.select.c)){ 
  
  #Create long data sets using melt
  species.melt<-melt(get(names.select.c[i]), id.vars=c("Time") )
  
  
  p <- ggplot(species.melt, aes(x=Time, y=value, group=variable, colour=variable)) + geom_line(size=2) 
  p <- p + xlab("Year") + ylab("Tons - catch") 
  #p <- p + scale_colour_manual(values=c("black", mypalette[1:7]), name="% Fishery closures", labels=c("Base Case", "GoM 20%", "GoM 50%", "GB 20%", "GB 50%", "All 20%", "All 50%", "All 100%")) 
  p <- p + scale_colour_manual(values=c("black", mypalette[1:7]), name="% Fishery closures", labels=c("Base Case", "All 100%")) 
  p <- p + ggtitle(names.select[i])  + theme(axis.title.x = element_text(face="bold", size=20), axis.title.y = element_text(face="bold", size=20), axis.text.x  = element_text(size=16), axis.text.y  = element_text(size=16), plot.title = element_text(size=24, face="bold"), legend.title = element_text(size=14, face="bold"), legend.text = element_text( size = 14), legend.position="none")
  
  j<-c(i-1)
  gg_list[[j]] <- p
  p
  setwd("/Users/eriko/Documents/Research/Atlantis NEUS Spatial/plots/TS")
  ggsave(paste(names.select[i], "CATCH Spatial MGMNT.png",sep=""))
}


# BIOMASS multiplots of the single plots
#choose species based on Skill assessment work: Cod, Haddock, Spiny Dogfish, Herring, White hake, Lobster
multiplot(gg_list[[11]], gg_list[[14]], gg_list[[16]], gg_list[[2]],gg_list[[3]], gg_list[[29]], cols=3)
# must save manually using Plots menue (choose 1600 w)




#' ----------------------
#' CALCULATE ECOSYSTEM INDICATORS

path <- c("~/AtlantisNEUS_R/spatial/get_indicators_4Erik")
components <- c(48)
bzero<-base.case[1,2:components]

#' Indicators BaseCase 
Ind.base<-get.Ind(base.case[1,],base.case.catch[1,],bzero,path)
Ind.base[23]<-as.integer(1964)
names(Ind.base)[23]<-"Year"
for (i in 2:nrow(base.case)) {
  IndYear<-get.Ind(base.case[i,],base.case.catch[i,],bzero,path)
  IndYear[23]<-as.integer(base.case$Time[i]/365)+1964
  Ind.base<-rbind(Ind.base, IndYear, deparse.level = 0)
}

#' Indicators Scenarios
for (j in 1:length(slist)) {
  IB<-get.Ind(get(slist[j])[1,],get(slist.catch[j])[1,],bzero,path)
  IB[23]<-as.integer(1964)
  names(IB)[23]<-"Year"

  for (i in 2:nrow(get(slist[j]))) {
    IndYear<-get.Ind(get(slist[j])[i,],get(slist.catch[j])[i,],bzero,path)
    IndYear[23]<-as.integer(get(slist[j])$Time[i]/365)+1964
    IB<-rbind(IB, IndYear, deparse.level = 0)
  }

  assign(paste("Ind.", slist[j], sep=""), IB)
}



#make indicator specific data sets, one per indicator
names.select.i<-colnames(Ind.GB20)

for (i in 1:22){ 
  # indtable<-cbind(Ind.base[,23], Ind.base[,i], Ind.GoM20[,i], Ind.GoM50[,i], Ind.GB20[,i], Ind.GB50[,i], Ind.All20[,i], Ind.All50[,i], Ind.All100[,i])
 # colnames(indtable)<-c("Time", "Base", "GoM20", "GoM50", "GB20", "GB50", "All20", "All50", "All100")
  indtable<-cbind(Ind.base[,23], Ind.base[,i], Ind.All100[,i])
  colnames(indtable)<-c("Time", "Base", "All100")
  assign(names.select.i[i], indtable)
}


#' make TS plots
#' 

#' empty list for storage
gg_list <- list()

for (i in 1:(length(names.select.i)-1)){ 
  
  #Create long data sets using melt
  species.melt<-melt(as.data.frame(get(names.select.i[i])), id.vars=c("Time") )
  
  
  p1 <- ggplot(species.melt, aes(x=Time, y=value, group=variable, colour=variable)) + geom_line(size=2) 
  p1 <- p1 + xlab("Year") + ylab("") + scale_colour_manual(values=c("black", mypalette[1:7]), name="% Fishery closures", labels=c("Base Case","GoM 20%", "GoM 50%", "GB 20%", "GB 50%", "All 20%", "All 50%", "All 100%")) + ggtitle(names.select.i[i])  + theme(axis.title.x = element_text(face="bold", size=20), axis.title.y = element_text(face="bold", size=20), axis.text.x  = element_text(size=16), axis.text.y  = element_text(size=16), plot.title = element_text(size=24, face="bold"), legend.title = element_text(size=14, face="bold"), legend.text = element_text( size = 14), legend.position="none")
  
  gg_list[[i]] <- p1
  p1
  setwd("/Users/eriko/Documents/Research/Atlantis NEUS Spatial/plots/Ind")
  ind.file.names<-names.select.i
  ind.file.names[3]<-c("Cat_Bio")
  ind.file.names[14]<-c("Bio_PP")
  ind.file.names[15]<-c("DemBio_PP")
  ind.file.names[16]<-c("PelBio_PP")
  ind.file.names[17]<-c("Cat_PP")
  ind.file.names[18]<-c("DemCat_PP")
  ind.file.names[19]<-c("PelCat_PP")
  ind.file.names[21]<-c("FishCat_FishBio")
  ggsave(paste(ind.file.names[i], "Spatial MGMNT.png",sep=""))
}


# INDICATOR multiplots of the single plots
#choose species based on Skill assessment work: 
multiplot(gg_list[[1]], gg_list[[2]], gg_list[[3]], gg_list[[6]],gg_list[[13]], gg_list[[21]], cols=3)
# must save manually using Plots menue (choose 1600 w)

