# Adjusting .PRM input files for Atlantis NEUS model 
#By: Erik Olsen 
#First created: 5.12.2013 
#Modified: 21.01.2014

### Importing HARVEST .PRM file
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/ALL10") # Set WD to correct folder
hh<-readLines("at_harvest_dynhist_neus_base_effort.prm", n=-1) #Reads the harvest.prm file into R, can adjust length of reading by n argument. May take some time to run as the .PRM file is over 15000 lines

### SETTING the closure rates for the different areas
# NEUS Areas:     0   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29
closurestring<-c("0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0") #Whole NEUS area closures
#closurestring<-c("0.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0") #Cape Wind - area 5 and 8
#closurestring<-c("0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.75 0.5 1.0 0.5 0.5 0.5 0.5 1.0 1.0 0.5 0.5 1.0 1.0 0.5 1.0 1.0 1.0 1.0 1.0") #Gulf of Maine closures
#closurestring<-c("0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0") #Base-case closures
#closurestring<-c("0.0 1.0 1.0 1.0 1.0 1.0 1.0 0.5 0.0 0.0 0.0 0.5 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0") #Georges Bank closures


# Need to create list of the MPA_Fisheries codes
ff<-grep("MPA", hh, value=TRUE) #find lines with MPA
ff1<-ff[4:36]
ff2<-strsplit(ff1, "[[:blank:]]")
MPAFisheriesNames<-vector("list", length(ff2))
for (i in 1:length(ff2)) #make a list of the MPA variables
{ 
	MPAFisheriesNames[i]<-c(ff2[[i]][1])
	}

#command to find lines where we can adjust fisheries
for (i in 1:length(MPAFisheriesNames)) #loops through all Fisheries and changes closures 'closurestring'
{ 
	ln<-c(grep(MPAFisheriesNames[i], hh,)+1) #use Grep to find the linenumber for the line with the fishery
	hh[ln]<-closurestring #changes fisheries in the designated areas to the designated value
	}

# Write to file
write(hh, file="at_harvest_dynhist_NEUS_ALL100.prm") 

