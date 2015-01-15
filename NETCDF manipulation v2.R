# Manipulating the Atlantis .NETCDF files
  # - extracts main variable names
  # - extracts last 10 year average for each variable values summed across depth strata for both the BASE case and the SPATIAL case
  # - compares the Base case with Spatial case by dividing Spatial/Base
  # - plots both BASE and SPATIAL and the COMPARISON in a MULTI-FACETED plot
# By Erik Olsen
# Modified: 2/3/2014
# showed GitHub func


library(ncdf)
library(ggplot2)
# library(ncdf4) new library - have not tried out the new commands yet

### EXTRACTING BASE_RUN DATA 
## ---------------------------

# Set working directory
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Base_case_test/Output_analysis")

#import the .NC file
ThisNC.nc<-open.ncdf("neusDynEffort_Base_Effort_.nc")  

# STILL looking for a way to list the variable names in the .nc file
# get them using GREP

# in the meawhile use the variable names in:     FuncGroupNamesInPlotOrderNEUS.csv
FuncGroupNames<-read.table("FuncGroupNamesInPlotOrderNEUS.csv",as.is = TRUE,header=TRUE,sep=";") # For the Biomass vs Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank. 
FuncGroupNames<-FuncGroupNames[1:4] #remove redundant columns

## Old test of commands
# extracting variables from .NC file
#volumeData <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
#pinnipedData<-get.var.ncdf(ThisNC.nc, "Pinniped_N") # extracts the "Pinniped_N" variable from the NC file
#dim(pinnipedData) #dimensions for variable, 3 values representing: DepthLayers, Boxes, Timesteps (quarterly in this example) - length of timesteps can be extracted from the .NC file where each timestep is given a timeID (sec from 01.01.1964) by subtracting two consecutive timesteps

## VARIABLES
# make list of all variables in the NCDF file
nvarlist<-c(0)

nv1<-ThisNC.nc$nvars #number of variables
for (i in 1:nv1) { # loop that prints all variable names
  nvarlist[i]<-(ThisNC.nc$var[[i]]$name) # name of variable number i
  } # this creates a list of 823 names - too many to work with. Need to trim down to the essential variables. 

NoTxt<-c("_StructN", "_ResN", "_Nums", "_Det_N")

AVarNames<-grep(NoTxt[4], (grep(NoTxt[3], (grep(NoTxt[2], (grep(NoTxt[1], nvarlist, value=TRUE, invert=TRUE)), value=TRUE, invert=TRUE)), value=TRUE, invert=TRUE)), value=TRUE, invert=TRUE) # extract main variable names - need refinement
AVarNames<-c(AVarNames[25:104]) #select only the biological components (not stuff like volume, vflux etc)
AVarNames<-grep("_N", AVarNames, value=TRUE) #further refinement to only key variables

VarBox2004<-matrix(nrow=30, ncol=(length(AVarNames)+1)) #create an empty matrix to store the EndYear data for each box-area
colnames(VarBox2004)<-c("id", AVarNames) # set column names (variables)
VarBox2004[,1]<-c(0:29) #enter area names NB - area numbering starts with 0 in the ATLANTIS model
la<-length(AVarNames)

## VALUES (of variables)
## Extracting varible values as the average over the last 10 years for all variables
# double for-loop with an else sentence
for (i in 1:la) {
  VD<-get.var.ncdf(ThisNC.nc, AVarNames[i]) #for variable i
  for (j in 1:30) { 
    if(nrow(VD)==5) {
      VDB<-apply(VD[,j,], 2, sum, na.rm=TRUE) # data for box j for all years
      VarBox2004[j,i+1]<-mean(VDB[163:204]) # transfer variable status to matrix)
    } else { #option for some variables that are not depth stratified.
      
       VarBox2004[,i+1]<-rowMeans(VD[,163:204])} # transfer column of values to status to matrix)
    }
  }

# Giving real-world names to variables
VBnames<-colnames(VarBox2004)
for (j in 2:length(VBnames)) {
  VBnames[j]<-FuncGroupNames$EMOCCName[match(VBnames[j], FuncGroupNames$NetCDFName)] #replaces NTCDF names with real-world names from FuncGrouNames file
  print(j)
  }

colnames(VarBox2004)<-VBnames #replace colnames to the matrix with data



####
### - END BASE RUN EXTRACTION--
####


### EXTRACTING DATA FROM SPATIAL CASE
## - may be able to automate these across tables


# Set working directory to spatial case
#setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/Area5close100") #WD with spatial case - closure in Area5 100%
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GB100") #WD with spatial case - GoM10%

#import the .NC file
#SpatialNC.nc<-open.ncdf("neusA5C100_out.nc")   #import spatial case NCDF file
SpatialNC.nc<-open.ncdf("neusGB100_out.nc") 

# make list of all variables in the NCDF file
nvarlist<-c(0)

nv1<-SpatialNC.nc$nvars #number of variables
for (i in 1:nv1) { # loop that prints all variable names
  nvarlist[i]<-(SpatialNC.nc$var[[i]]$name) # name of variable number i
} # this creates a list of 823 names - too many to work with. Need to trim down to the essential variables. 

NoTxt<-c("_StructN", "_ResN", "_Nums", "_Det_N")

AVarNames<-grep(NoTxt[4], (grep(NoTxt[3], (grep(NoTxt[2], (grep(NoTxt[1], nvarlist, value=TRUE, invert=TRUE)), value=TRUE, invert=TRUE)), value=TRUE, invert=TRUE)), value=TRUE, invert=TRUE) # extract main variable names - need refinement

AVarNames<-c(AVarNames[25:104]) #select only the biological components (not stuff like volume, vflux etc)
AVarNames<-grep("_N", AVarNames, value=TRUE) #further refinement to only key variables

SVarBox2004<-matrix(nrow=30, ncol=(length(AVarNames)+1)) #create an empty matrix to store the EndYear data for each box-area
colnames(SVarBox2004)<-c("id", AVarNames) # set column names (variables)
SVarBox2004[,1]<-c(0:29) #enter area names
la<-length(AVarNames)

## Extracting varible values at the last time-step (204) for 80 variables
# double for-loop with an else sentence
for (i in 1:la) {
  VD<-get.var.ncdf(SpatialNC.nc, AVarNames[i]) #for variable i
  for (j in 1:30) { 
    if(nrow(VD)==5) {
      VDB<-apply(VD[,j,], 2, sum, na.rm=TRUE) # data for box j for all years
      SVarBox2004[j,i+1]<-mean(VDB[163:204]) # transfer variable status to matrix)
    } else { #option for some variables that are not depth stratified.
      
      SVarBox2004[,i+1]<-rowMeans(VD[,163:204])} # transfer column of values to status to matrix)
  }
}

# Giving real-world names to variables
VBnames<-colnames(SVarBox2004)
for (j in 2:length(VBnames)) {
  VBnames[j]<-FuncGroupNames$EMOCCName[match(VBnames[j], FuncGroupNames$NetCDFName)] #replaces NTCDF names with real-world names from FuncGrouNames file
}

colnames(SVarBox2004)<-VBnames #replace colnames to the matrix with data

### --- FINISHED SPATIAL CASE


### COMPARING Base-case with Spatial case
#   Divide each component of Spatial Case by Base Case
if(ncol(SVarBox2004)==ncol(VarBox2004)) {
  print("TRUEEEE")
                                           }#check dimensions of  # doesn't work properly

SVarBox2004<-as.data.frame(SVarBox2004)  #change to data.frame format
compBvS<-as.data.frame(SVarBox2004/VarBox2004) #dividing Spatial case by Base case

compBvS$id<-SVarBox2004$id #give correct area numbers

naTest1<-grep("NaN", colMeans(compBvS, na.rm=T), invert=T ) #identify variables with no numbers (NaN)
naTest2<-grep("Inf", colMeans(compBvS, na.rm=T), invert=T ) #identify variables with no numbers (NaN)
naTestBiol<-intersect(naTest1, naTest2) #combined

### END comparing Spatial/Base


### - PLOTTING 
### MAKING MULTIPLE PLOTS
### Combine data and spatial NEUS polygons
# look only at Fish variables
VBdf<-as.data.frame(VarBox2004) # convert data into data.frame allowing for variable manipulation
VBdf<-VBdf[,c(1,16:36)] #selects only the fish variables

NEUS.f$id %in% VBdf$id #compare variables to check that they are alike

# SINGLE VARIABLE PLOT
# Plots data for a single variable from the combined data set using ggplot2
plot.data <- merge(NEUS.f, VBdf, by.x = "id") #merges data by id
colnames(plot.data)
# Creating a test-plot with one species "Demersal_B_Fish_N"
map2<- ggplot(plot.data, aes(long, lat, group = group, fill =Haddock )) + geom_polygon() + coord_equal() + labs(x = "Easting (m)", y = "Northing (m)") + 
  ggtitle("NEUS map")

map2



# MULTIPLE VARIABLE PLOTS
# making multiple plots of single scenario output
library(reshape2) 

NEUS.melt <-melt(VBdf, id = c("id")) # Convert Columns to rows
colnames(NEUS.melt)<-c("id", "variable", "data") # Give new columns meaningful names
plot.melt.data<-merge(NEUS.f, NEUS.melt, by.x = "id") #combine with NEUS spatial data

map3<-ggplot(data = plot.melt.data, aes(x = long, y = lat, fill = data, group = group)) + 
  geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() + 
  facet_wrap(~variable) # creates a faceted - multi plot

map3 #plot maps



## MULTIPLE PLOTS - Comparison SPATIAL / BASE case
# making multiple plots of comparison of Base case with spatial case

#CBS<-compBvS[,c(1,16:36)] #select fish variables
#CBS<-compBvS[,c(1,4,5,6,7,9,43,46,48,49,52,53,54)] #select other variables
CBS<-compBvS[,naTestBiol] #select other variables

NEUS.melt2 <-melt(CBS, id = c("id")) # Convert Columns to rows
colnames(NEUS.melt2)<-c("id", "variable", "data") # Give new columns meaningful names
plot.melt2.data<-merge(NEUS.f, NEUS.melt2, by.x = "id") #combine with NEUS spatial data

#create categories
library("classInt", lib.loc="/Users/eriko/Library/R/3.0/library")
library("RColorBrewer", lib.loc="/Users/eriko/Library/R/3.0/library")

brks<-classIntervals(plot.melt2.data$data, n=7, style="fixed", fixedBreaks=c(0.5, 0.8, 0.9, 1.1, 1.2, 2, 5, 2230000000)) #define categories
brks <- round(brks$brks,digits=2) #round
catVar<-findInterval(plot.melt2.data$data, brks, all.inside=TRUE) #assign categories

PMD<-cbind(plot.melt2.data, catVar) #join data & spatial info

# Create labels from break values
intLabels <- matrix(1:(length(brks)-1))
for(i in 1:length(intLabels )){intLabels [i] <- paste(as.character(brks[i]),"-",as.character(brks[i+1]))}
intLabels[1,1]<-c("< 0.8")
intLabels[7,1]<-c(">5")


#actual plotting step
map4<-ggplot(data = PMD, aes(x = long, y = lat, fill = catVar, group = group)) +geom_polygon() + geom_path(colour = "grey", lwd = 0.1) + coord_equal() + labs(x = "LON", y = "LAT", fill = "Spatial/Base") + 
  ggtitle("BIOMASS  Comparison base case w GB100 scenario") +facet_wrap(~variable)  + scale_fill_gradientn(colours=brewer.pal(7, "PiYG"), guide="legend", label=intLabels) # creates a faceted - multi plot
  #facet_wrap(~variable)+ scale_fill_gradient(limits=c(0.95, 1.05), low="firebrick4") +  theme(text = element_text(size=14))  # creates a faceted - multi plot
  #facet_wrap(~variable)+ scale_fill_gradient(limits=c(0.9, 8), low="firebrick4") +  theme(text = element_text(size=14))  # creates a faceted - multi plot
#facet_wrap(~variable)+ scale_fill_gradient2(midpoint=1, limits=c(0, 8), low="firebrick4", high="navy3") +  theme(text = element_text(size=14))  # creates a faceted - multi plot
  

#map4 + scale_fill_gradientn(colours=brewer.pal(7, "Spectral"),guide="legend",label=intLabels,name="Scenario/Base",min(PMD$catVar),max(PMD$catVar))
 
map4 #plot maps
ggsave("A5C100vsBase other.pdf", scale = 1, dpi = 400)