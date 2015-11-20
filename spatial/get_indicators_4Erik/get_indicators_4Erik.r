########################
#' @name get.Ind
#' @author Gavin Fay
#' Function takes a vector of biomass and catch (by species) and returns a 
#' list of indicators.
#' If B0 estimates are provided, will also give B/B0 status
#' Argument 'path' points to a directory containing the following 3 files:
#' 1. compare_trophiclevels.csv
#' 2. longnames.csv
#' 3. AtlantisIOpaperspeciespricetable.csv
######################### 
get.Ind <- function(Bio,Cat,bzero=NA,path=getwd())
{
  isfish <- 2:20
  isTEP <- c(19,21:25)
  isBio <- c(2:28,30:32)
  istargBio <- c(2:20,26:28,30:32)
  isCatch <- isBio
  #isDemBio <- c(4,6,10,12:20,27:28,30:31)
  #isPelBio <- c(2,3,4,7:9,11,19)
  isDemBio <- c(4,6,10,12:18,20,27:28,30:31)
  isPelBio <- c(2,3,4,5,7:9,11,19,26,32)
  isDemCatch <- isDemBio
  isPelCatch <- isPelBio
  isPP <- 37:39
  isPelFish <- c(2,3,4,5,7:9,11,19)
  isDemFish <- c(4,6,10,12:20)
  
  #if(Sys.info()[['sysname']]=="Linux") 
  #  TL <- read.csv("/media/My\ Passport/NEFSC/ATLANTIS/2013/OA/Trophiclevels.csv",header=TRUE)
  #else
  #  TL <- read.csv("J:/NEFSC/ATLANTIS/2013/OA/trophiclevels.csv",header=TRUE)  
  #trophic.levels <- read.csv("H:/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  #trophic.levels <- read.csv("/Volumes/MyPassport/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  trophic.levels <- read.csv(paste(path,"/compare_trophiclevels.csv",sep=""))
  
  
  TL <- trophic.levels$New.ATL.TL
  
  Ind <- c(sum(Bio[isBio]),sum(Cat[isCatch]),sum(Cat[isCatch])/sum(Bio[istargBio]),sum(Bio[isfish]),sum(Bio[isDemFish])/sum(Bio[isPelFish]),sum(Bio[isTEP]),as.numeric(Bio[21]),as.numeric(Bio[22]),sum(Bio[24:25]),sum(Cat[isDemBio]),sum(Cat[isPelBio]),sum(Bio[istargBio]*TL[istargBio-1],na.rm=TRUE)/sum(Bio[istargBio]),
           sum(Cat[istargBio-1]*TL[istargBio-1],na.rm=TRUE)/sum(Cat[istargBio-1]),sum(Bio[isBio])/sum(Bio[isPP]),sum(Bio[isDemBio])/sum(Bio[isPP]),sum(Bio[isPelBio])/sum(Bio[isPP]),sum(Cat[isCatch])/sum(Bio[isPP]),sum(Cat[isDemCatch])/sum(Bio[isPP]),sum(Cat[isPelCatch])/sum(Bio[isPP]),
           sum(Cat[isfish]),sum(Cat[isfish])/sum(Bio[isfish]))
  
  labels <- c("TotBio","TotCat","Cat/Bio","FishBio","DemPelFish","TEPs","Birds","Seals","Whales","DemCat","PelCat","MTLBio","MTLCat","Bio/PP","DemBio/PP","PelBio/PP","Cat/PP","DemCat/PP","PelCat/PP","FishCat","FishCat/FishBio")
  
  if (length(bzero)>0)
  {
    depletion <- Bio[2:48]/bzero[1:47]
    prop.of <- length(depletion[depletion<0.368/2])/length(depletion)
    Ind <- c(Ind,prop.of)
    labels <- c(labels,"PropOF")
  }
  
  #Commenting out value estimation - not working 24.09.2015
  #Ind <- c(Ind,get.value(Cat,path))
  #labels <- c(labels,"Value")
  
  
  names(Ind) = labels
  return(Ind)
}

########################
#' @name get.value
#' @author Gavin Fay
#' called by get.Ind()
#' Function calculates value (in 2008 $) of catch vector by species
#' Argument 'path' points to a directory containing the following 2 files:
#' 1. longnames.csv
#' 2. AtlantisIOpaperspeciespricetable.csv
######################### 
get.value <- function(catch,path=getwd())
{
  #long.names <-read.csv("/media/My\ Passport/NEFSC/ATLANTIS/2013/OA/longnames.csv")
  #prices <- read.csv("/media/My\ Passport/NEFSC/ATLANTIS/2012/IOoutput/AtlantisIOpaperspeciespricetable.csv")
  #long.names <-read.csv("H:/NEFSC/ATLANTIS/2013/OA/longnames.csv")
  #prices <- read.csv("H:/NEFSC/ATLANTIS/2012/IOoutput/AtlantisIOpaperspeciespricetable.csv")
  #long.names <-read.csv("/Volumes/MyPassport/NEFSC/ATLANTIS/2013/OA/longnames.csv")
  #prices <- read.csv("/Volumes/MyPassport/NEFSC/ATLANTIS/2012/IOoutput/AtlantisIOpaperspeciespricetable.csv")
  long.names <-read.csv(paste(path,"/longnames.csv",sep=""))
  prices <- read.csv(paste(path,"/AtlantisIOpaperspeciespricetable.csv",sep=""))
  prices$code <- as.character(long.names$CODE[match(prices$species,
                                                    long.names$EMOCCName)])
  prices$code[6] <- "BC"
  prices$code[10] <- "BD"
  #prices$price[10] <- 0
  
  #biomass <- neusDynEffort_Oct23_08d_BiomIndx.txt
  #if (ncol(biomass)>47) biomass <- biomass[,1:47]
  if (length(catch)>36) catch <- catch[1:36]
  #fleet.catch <- read.table("neusDynEffort_Base_Effort_CatchPerFishery.txt",header=TRUE)
  #effort <- read.table("neusDynEffort_Base_Effort_Effort.txt",header=TRUE)
  
  #match(colnames(catch),prices$code)
  #species.value <- 
  xx <- as.numeric(na.omit(match(names(catch),prices$species)))
  yy <- catch
  #for (irow in 1:nrow(yy)) 
  yy <- yy*2204.62*prices$price[xx]
  temp <- as.character(long.names$EMOCCName)
  names(yy) <- temp[match(names(yy),long.names$CODE)]
  value <- sum(yy,na.rm=TRUE)
  return(value)
}

 
######################
#' example call to run get.Ind()
#
#  aa <- NULL
#  bb <- NULL
#  cc <- NULL

  #switch to directory with Atlantis Output in it
#  root <- "/Volumes/MyPassport/NEFSC/ATLANTIS/"
#  scen <- "neusDynEffort_Base_Effort"
#  setwd(paste(root,"Scenarios/Levels\ of\ Tuning/Base Effort/",sep=""))
  
  #read the biomass and catch files
#  Bio <- read.table(paste(scen,"_BiomIndx.txt",sep=""),header=TRUE)
#  Cat <- read.table(paste(scen,"_Catch.txt",sep=""),header=TRUE) 
#  numrow <- nrow(Bio)
  #get the relevant row, loop over this to get indicators for each time step
#  row.use <- numrow-1
#  BioUse <- Bio[row.use,1:48]
#  CatUse <- Cat[row.use,1:48]

#  path <- "/Atlantis/sandbox/" #directory where price and trophic level info are.

#  aa <- rbind(aa,c(eff.mult,get.Ind(BioUse,CatUse,bzero,path)))
#  bb <- rbind(bb,c(eff.mult,BioUse))
#  cc <- rbind(cc,c(eff.mult,CatUse))
  


