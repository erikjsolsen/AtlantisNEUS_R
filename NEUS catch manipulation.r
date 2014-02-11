# R-script for manipulating catch data from Atlantis
# 2/3/2014
# By: Erik Olsen

library(ncdf)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Base_case_test")

# catchNC<-open.ncdf("neusDynEffort_Base_Effort_TOTCATCH.nc") # By species and fleet - don't use this file due to mapping errors. 
catchTotNC<-open.ncdf("neusDynEffort_Base_Effort_TOTCATCH.nc") # By species, summed over fleets

#Open Scenario .NC file
#AltCatchTotNC<-open.ncdf("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Area5close100/neusA5C100_outTOTCATCH.nc")
AltCatchTotNC<-open.ncdf("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GB10/neusGB10_outTOTCATCH.nc")

#use str (structure to look at .nc structure), eg:
str(catchTotNC$var[[4]])


#Must sum over species to get data for each fleet. 

## VARIABLES CATCH
#Long-names
catchTotNC$var[[8]]$longname

# make list of all variables in the NCDF file
#nvarlist<-c(0)

#nv1<-catchNC$nvars #number of variables
#for (i in 1:nv1) { # loop that prints all variable names
 # nvarlist[i]<-(catchNC$var[[i]]$name) # name of variable number i
# this creates a list of 823 names - too many to work with. Need to trim down to the essential variables. 

## VARIABLES TOTAL-CATCH
# make list of all variables in the NCDF file
nvarlist2<-c(0)

nv2<-catchTotNC$nvars #number of variables
for (i in 1:nv2) { # loop that prints all variable names
  nvarlist2[i]<-(catchTotNC$var[[i]]$name) # name of variable number i
} # this creates a list of 219 names - too many to work with. Need to trim down to the essential variables. 


###  Commercial Catches for all biological components
varlines<-intersect(grep("_Catch", nvarlist2),grep("Tot", nvarlist2)) #identify var_number for Commercial catches
VarNames<-nvarlist2[varlines] #names of selected variables
ns<-substr(VarNames,5,nchar(VarNames)-6) #extract only varible name (removing "Tot_" and "Catch")

# Giving real-world names to variables
for (j in 1:length(ns)) {
  ns[j]<-FuncGroupNames$EMOCCName[match(ns[j], FuncGroupNames$CODE)] #replaces NTCDF names with real-world names from FuncGrouNames file
}
ns[8]<-c("MA")
ns[9]<-c("Shrimp")



## BASE CASE
#set up matrix to store variable data
CatchBox2004<-matrix(nrow=30, ncol=(length(varlines)+1)) #create an empty matrix to store the EndYear data for each box-area
CatchBox2004[,1]<-c(0:29) #enter area names NB - area numbering starts with 0 in the ATLANTIS model
colnames(CatchBox2004)<-c("id",ns) #give colname for the 'id' column and the variables

la<-length(varlines)

# Extract last 4 -year average for BASE-CASE .NC file
for (i in 1:la) {
  VD<-get.var.ncdf(catchTotNC, VarNames[i]) #for variable i
  for (j in 1:30) { 
    #if(nrow(VD)==5) {
     # VDB<-apply(VD[,j,], 2, sum, na.rm=TRUE) # data for box j for all years
     # CatchBox2004[j,i+1]<-mean(VD[197:204]) # transfer variable status to matrix)
  #  } else { #option for some variables that are not depth stratified.
      
      CatchBox2004[,i+1]<-rowMeans(VD[,197:204])} # transfer column of values to status to matrix)
}

## SCENARIO CASE
#set up matrix to store variable data
AltCatchBox2004<-matrix(nrow=30, ncol=(length(varlines)+1)) #create an empty matrix to store the EndYear data for each box-area
AltCatchBox2004[,1]<-c(0:29) #enter area names NB - area numbering starts with 0 in the ATLANTIS model
colnames(AltCatchBox2004)<-c("id",ns) #give colname for the 'id' column and the variables

la<-length(varlines)

# Extract last 4 -year average for BASE-CASE .NC file
for (i in 1:la) {
  VD<-get.var.ncdf(AltCatchTotNC, VarNames[i]) #for variable i
  for (j in 1:30) { 
    #if(nrow(VD)==5) {
    # VDB<-apply(VD[,j,], 2, sum, na.rm=TRUE) # data for box j for all years
    # CatchBox2004[j,i+1]<-mean(VD[197:204]) # transfer variable status to matrix)
    #  } else { #option for some variables that are not depth stratified.
    
    AltCatchBox2004[,i+1]<-rowMeans(VD[,197:204])} # transfer column of values to status to matrix)
}

## COMPARE Base & SCenario
## Divide Scenario by Base Case

CatchCompBvS<-as.data.frame(AltCatchBox2004/CatchBox2004) #dividing Spatial case by Base case

CatchCompBvS$id<-CatchBox2004[,1] #give correct area numbers
naCatchTest<-grep("NaN", colMeans(CatchCompBvS, na.rm=T), invert=T ) #identify variables with no numbers (NaN)

## Plotting Results in panelled multi-plots
CBS2<-CatchCompBvS[,naCatchTest] #select other variables

#CBS3<-CBS2[2:22,] # select areas except 0 (border area)

NEUS.melt2 <-melt(CBS2, id = c("id")) # Convert Columns to rows

colnames(NEUS.melt2)<-c("id", "variable", "data") # Give new columns meaningful names
plot.melt2.data<-merge(NEUS.f, NEUS.melt2, by.x = "id") #combine with NEUS spatial data
plot.melt2.data$data<-replace(plot.melt2.data$data, grep("Inf", plot.melt2.data$data), NaN )

#create categories
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")

brks<-classIntervals(plot.melt2.data$data, n=7, style="fixed", fixedBreaks=c(0.0, 0.8, 0.9, 0.95, 1.05, 1.07, 1.1, Inf)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(plot.melt2.data$data, brks, all.inside=TRUE) #assign categories

PMD<-cbind(plot.melt2.data, catVar) #join data & spatial info

# Create labels from break values
intLabels <- matrix(1:(length(brks)-1))
for(i in 1:length(intLabels )){intLabels [i] <- paste(as.character(brks[i]),"-",as.character(brks[i+1]))}
intLabels[1,1]<-c("< 0.5")
intLabels[7,1]<-c(">1.1")

#actual plotting step
map5<-ggplot(data = PMD, aes(x = long, y = lat, fill = catVar, group = group)) + 
  geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() + labs(x = "LON", y = "LAT", fill = "Spatial/Base") + 
  ggtitle("Comparison of fishery catches by species") +
  facet_wrap(~variable)+  scale_fill_gradientn(colours=brewer.pal(7, "PiYG"), guide="legend", label=intLabels)  # creates a faceted - multi plot

map5
ggsave(paste("catchmap1.pdf"), scale = 1, dpi = 400)

### Effort by fishery and area




