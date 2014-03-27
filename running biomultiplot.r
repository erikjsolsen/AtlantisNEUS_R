# Running biomultplot
# 2/5/2014
# By Erik Olsen

## RUN biomultiplot for one case (from the base-case scenario catalogue)
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Base_case_test")

#set up DataOutTable
#DOT<-data.frame(ncol=, )

## Setting up list of scenarios and file names (due to some misnaming during running Atlantis)
#SNames<-c("GoM10", "GoM25", "GoM50", "GoM100", "GB10", "GB25", "GB50", "GB100", "CW10", "CW25", "CW50", "CW100", "ALL10", "ALL25", "ALL50", "ALL100") #list of scenario names - for use to find catalogues and give names to output
#Fnames<-c("GB10", "GB10", "GB10", "GB10", "GB10", "GB25", "GB50", "GB100", "GB10", "GB10", "GB10", "GB10", "GB10", "GB10", "GB10", "GB10") #list of scenario FILE names - some misnaming during running Atlantis made most runs have the same output file names
SNames<-c( "GoM100", "GB100",  "CW100",  "ALL100", "A5C100") 
Fnames<-c( "GB10", "GB100",  "GB10",  "GB10", "A5C100") 

## Source biomultiplot
source("/Users/eriko/AtlantisNEUS_R/biomultiplot.r")
 
# must run biomultiplot ONCE to get CMSBase (which is pushed from the function)
# This is not good programming practice, but don't know how to push several variables to global.
paste(SNames[4], "_Scen")<-biomultiplot ("neusDynEffort_Base_Effort_.nc", "/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GoM10/neusGB10_out.nc",  "/Users/eriko/AtlantisNEUS_R/data_files/FuncGroupNamesInPlotOrderNEUS.csv", NEUS.f, "outTEST3")

CMB<-as.data.frame(CMSBase) #convert to data.frame





setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Analysis") # output catalogue for plots etc. 

# RUNNING biomultiplot for all scenarios
for (i in 1:length(SNames)){
  #SN<-paste("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/",SNames[i],"/neus",Fnames[i], "_out.nc", sep="")
  
a<-assign(paste0(SNames[i], "_S"), biomultiplot ("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Base_case_test/neusDynEffort_Base_Effort_.nc", paste("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/",SNames[i],"/neus",Fnames[i], "_out.nc", sep=""),  "/Users/eriko/AtlantisNEUS_R/data_files/FuncGroupNamesInPlotOrderNEUS.csv", NEUS.f, SNames[i]))

CMSpat<-as.data.frame(a) #convert to data frame
colnames(CMSpat)<-SNames[i] # give variable as col-names

CMB<-cbind(CMB,CMSpat) #combine with other variables
}

CMB<-as.data.frame(CMB)
write.table(CMB, file="ScenarioEndYears.csv", sep=";")


