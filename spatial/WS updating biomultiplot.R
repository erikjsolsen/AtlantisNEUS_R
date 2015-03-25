# Updating biomultiplot to new outputs:
# created: 19.03.2015
# by: Erik Olsen

#must run 'mappingNEUS.R' first to generate NEUS.f 

source("/Users/eriko/AtlantisNEUS_R/biomultiplot.r")

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Analysis")

baseNC<-c("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Base_case_new/output/neusDynEffort_BASE_.nc")
altNC<-c("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GoM100/output/neusDynEffort_spatial_.nc")
FuncNameFile<-c("/Users/eriko/AtlantisNEUS_R/data_files/FuncGroupNamesInPlotOrderNEUS.csv")

AreaPoly<-NEUS.f

PlotOutFile<-c("Test23Mar")


biomultiplot (base, new,  csv, NEUS.f, "TestMarch15")

#it Works!

###Running all scenarios
SNames<-c("GoM10", "GoM20", "GoM50", "GoM100", "GB10", "GB20", "GB50", "GB100", "CW10", "CW20", "CW50", "CW100", "ALL10", "ALL20", "ALL50", "ALL100")

SNames<-c("GoM10", "GoM20", "GoM50", "GoM100", "GB10", "GB20", "GB50", "GB100", "CW10", "CW20", "CW50", "CW100")

for (i in 1:length(SNames)){
  #SN<-paste("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/",SNames[i],"/neus",Fnames[i], "_out.nc", sep="")
  
biomultiplot ("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Base_case_new/output/neusDynEffort_BASE_.nc", paste("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/",SNames[i],"/output/neusDynEffort_spatial_.nc", sep=""),  "/Users/eriko/AtlantisNEUS_R/data_files/FuncGroupNamesInPlotOrderNEUS.csv", NEUS.f, SNames[i])
  
  #CMSpat<-as.data.frame(a) #convert to data frame
  #colnames(CMSpat)<-SNames[i] # give variable as col-names
  
 #CMB<-cbind(CMB,CMSpat) #combine with other variables
}


mapX<-ggplot(data = PMD, aes(x = long, y = lat, fill = data, group = group)) + 
  geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() + labs(x = "", y = "", fill = "Spatial/Base") + 
  ggtitle(paste("BIOMASS Comparison base case vs spatial case: \n", PlotOutFile)) +facet_wrap(~variable, as.table=FALSE) + scale_fill_brewer(palette="RdYlGn", label=c("<90%", "90-95%", "95-105%", "105-110%", ">110%")) + theme_bw() + theme(axis.ticks.y = element_blank(),axis.text.y = element_blank()) + theme(axis.ticks.x = element_blank(),axis.text.x = element_blank()) +  theme(strip.text.x = element_text(size=7)) 

mapX