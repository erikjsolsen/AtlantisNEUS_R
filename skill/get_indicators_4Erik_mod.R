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
#' updated by Erik Olsen - 12.11.2014 to account for lack of seabird data (seabird removed from analysis)
######################### 
get.Ind <- function(Bio,Cat,bzero=NA,path=getwd(), components)
{
  # Modified the categories using species codes and only those species for which we have survey data or data from Ecosystem report, see notes in notebook. Also modified species selction for catch data to reflect those w landings. 
  # have removed seabirds from all  relevant categories
  isfish <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK")
  isfishCat <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK")
  #isTEP <- c("SB",  "PIN", "WHB")
  isTEP <- c("PIN", "WHB")
  #isBio <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "SB",  "PIN", "WHB", "CEP", "BFS",  "BML", "BMS", "PWN")
  isBio <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK",   "PIN", "WHB", "CEP", "BFS",  "BML", "BMS", "PWN")
  istargBio <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "CEP", "BFS",  "BML", "BMS", "PWN")
  istargCat <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "CEP", "BFS",  "BML", "BMS", "PWN")
  isCatch <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "CEP", "BFS",  "BML", "BMS", "PWN")
  isDemBio <- c("FVD", "FVB", "FDD", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "BFS", "BML", "BMS")
  isPelBio <- c("FPL", "FPS", "FVD", "FMM", "FBP", "FDE", "SHP")
  isDemBio <- c("FVD", "FVB", "FDD", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "BFS", "BML", "BMS")
  isPelBio <- c("FPL", "FPS", "FVD", "FMM", "FBP", "FDE",  "CEP", "PWN")
  isDemCatch <- isDemBio
  isPelCatch <- c("FPL", "FPS", "FVD", "FBP", "FDE",  "CEP", "PWN")
  isPP <- c("PL", "DF", "PS")
  isPelFish <- c("FPL", "FPS", "FVD", "FMM", "FBP", "FDE")
  isDemFish <- c("FVD", "FVB", "FDD", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK")
  isSeal <- c("PIN")
  isWhale <- c("WHB")
  
  #if(Sys.info()[['sysname']]=="Linux") 
  #  TL <- read.csv("/media/My\ Passport/NEFSC/ATLANTIS/2013/OA/Trophiclevels.csv",header=TRUE)
  #else
  #  TL <- read.csv("J:/NEFSC/ATLANTIS/2013/OA/trophiclevels.csv",header=TRUE)  
  #trophic.levels <- read.csv("H:/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  #trophic.levels <- read.csv("/Volumes/MyPassport/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  trophic.levels <- read.csv(paste(path,"/compare_trophiclevels.csv",sep=""))
  
  TL <- trophic.levels$New.ATL.TL
  
  # add names to TL
  names(TL)<-trophic.levels$ATL.code
  
  # change istargBio-1 to istargBio
 # Ind <- c(sum(Bio[isBio]),sum(Cat[isCatch]),sum(Cat[isCatch])/sum(Bio[istargBio]),sum(Bio[isfish]),sum(Bio[isDemFish])/sum(Bio[isPelFish]),sum(Bio[isTEP]),as.numeric(Bio[21]),as.numeric(Bio[22]),sum(Bio[24:25]),sum(Cat[isDemBio]),sum(Cat[isPelCatch]),sum(Bio[istargBio]*TL[istargBio],na.rm=TRUE)/sum(Bio[istargBio]), sum(Cat[istargCat]*TL[istargCat],na.rm=TRUE)/sum(Cat[istargCat]),sum(Bio[isBio])/sum(Bio[isPP]),sum(Bio[isDemBio])/sum(Bio[isPP]),sum(Bio[isPelBio])/sum(Bio[isPP]),sum(Cat[isCatch])/sum(Bio[isPP]),sum(Cat[isDemCatch])/sum(Bio[isPP]),sum(Cat[isPelCatch])/sum(Bio[isPP]),sum(Cat[isfishCat]),sum(Cat[isfishCat])/sum(Bio[isfish]))
  Ind <- c(sum(Bio[isBio]),sum(Cat[isCatch]),sum(Cat[isCatch])/sum(Bio[istargBio]),sum(Bio[isfish]),sum(Bio[isDemFish])/sum(Bio[isPelFish]),sum(Bio[isTEP]),as.numeric(Bio[isSeal]),sum(Bio[isWhale]),sum(Cat[isDemBio]),sum(Cat[isPelCatch]),sum(Bio[istargBio]*TL[istargBio],na.rm=TRUE)/sum(Bio[istargBio]), sum(Cat[istargCat]*TL[istargCat],na.rm=TRUE)/sum(Cat[istargCat]),sum(Bio[isBio])/sum(Bio[isPP]),sum(Bio[isDemBio])/sum(Bio[isPP]),sum(Bio[isPelBio])/sum(Bio[isPP]),sum(Cat[isCatch])/sum(Bio[isPP]),sum(Cat[isDemCatch])/sum(Bio[isPP]),sum(Cat[isPelCatch])/sum(Bio[isPP]),sum(Cat[isfishCat]),sum(Cat[isfishCat])/sum(Bio[isfish]))
  
  labels <- c("TotBio","TotCat","Cat/Bio","FishBio","DemPelFish","TEPs","Seals","Whales","DemCat","PelCat","MTLBio","MTLCat","Bio/PP","DemBio/PP","PelBio/PP","Cat/PP","DemCat/PP","PelCat/PP","FishCat","FishCat/FishBio")
  
  if (length(bzero)>0)
  {
    depletion <- Bio[2:components]/bzero
    prop.of <- length(depletion[depletion<0.368/2])/length(depletion)
    Ind <- c(Ind,prop.of)
    labels <- c(labels,"PropOF")
  }
  
  Ind <- c(Ind,get.value(Cat,path))
  labels <- c(labels,"Value")
  
  
  names(Ind) = labels
  return(Ind)
}


#################
## ind-Get for Observed data where PP is lacking

get.Ind.obs <- function(Bio,Cat,bzero=NA,path=getwd())
{
  # Modified the categories using species codes and only those species for which we have survey data or data from Ecosystem report, see notes in notebook. Also modified species selction for catch data to reflect those w landings. 
  isfish <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK")
  isfishCat <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK")
  #isTEP <- c("SB",  "PIN", "WHB")
  isTEP <- c(  "PIN", "WHB")
 # isBio <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "SB",  "PIN", "WHB", "CEP", "BFS",  "BML", "BMS", "PWN")
  isBio <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK",  "PIN", "WHB", "CEP", "BFS",  "BML", "BMS", "PWN")
  istargBio <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FMM", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "CEP", "BFS",  "BML", "BMS", "PWN")
  istargCat <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "CEP", "BFS",  "BML", "BMS", "PWN")
  isCatch <- c("FPL", "FPS", "FVD", "FVS", "FVB", "FBP", "FDD", "FDE", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "CEP", "BFS",  "BML", "BMS", "PWN")
  isDemBio <- c("FVD", "FVB", "FDD", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "BFS", "BML", "BMS")
  isPelBio <- c("FPL", "FPS", "FVD", "FMM", "FBP", "FDE", "SHP")
  isDemBio <- c("FVD", "FVB", "FDD", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK", "BFS", "BML", "BMS")
  isPelBio <- c("FPL", "FPS", "FVD", "FMM", "FBP", "FDE",  "CEP", "PWN")
  isDemCatch <- isDemBio
  isPelCatch <- c("FPL", "FPS", "FVD", "FBP", "FDE",  "CEP", "PWN")
  #isPP <- c("PL", "DF", "PS")
  isPelFish <- c("FPL", "FPS", "FVD", "FMM", "FBP", "FDE")
  isDemFish <- c("FVD", "FVB", "FDD", "FDS", "FDB", "FDC", "FDO", "FDF", "SHB", "SHD", "SSK")
  isSeal <- c("PIN")
   isWhale <- c("WHB")
  isPP <- c("PL")
  
  #if(Sys.info()[['sysname']]=="Linux") 
  #  TL <- read.csv("/media/My\ Passport/NEFSC/ATLANTIS/2013/OA/Trophiclevels.csv",header=TRUE)
  #else
  #  TL <- read.csv("J:/NEFSC/ATLANTIS/2013/OA/trophiclevels.csv",header=TRUE)  
  #trophic.levels <- read.csv("H:/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  #trophic.levels <- read.csv("/Volumes/MyPassport/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  trophic.levels <- read.csv(paste(path,"/compare_trophiclevels.csv",sep=""))
  
  TL <- trophic.levels$New.ATL.TL
  
  # add names to TL
  names(TL)<-trophic.levels$ATL.code
  
  # change istargBio-1 to istargBio
# Ind <- c(sum(Bio[isBio]),sum(Cat[isCatch]),sum(Cat[isCatch])/sum(Bio[istargBio]),sum(Bio[isfish]),sum(Bio[isDemFish])/sum(Bio[isPelFish]),sum(Bio[isTEP]),as.numeric(Bio[21]),as.numeric(Bio[22]),sum(Bio[24:25]),sum(Cat[isDemBio]),sum(Cat[isPelCatch]),sum(Bio[istargBio]*TL[istargBio],na.rm=TRUE)/sum(Bio[istargBio]), sum(Cat[istargCat]*TL[istargCat],na.rm=TRUE)/sum(Cat[istargCat]),sum(Cat[isfishCat]),sum(Cat[isfishCat])/sum(Bio[isfish]))
  Ind <- c(sum(Bio[isBio]),sum(Cat[isCatch]),sum(Cat[isCatch])/sum(Bio[istargBio]),sum(Bio[isfish]),sum(Bio[isDemFish])/sum(Bio[isPelFish]),sum(Bio[isTEP]),as.numeric(Bio[isSeal]),sum(Bio[isWhale]),sum(Cat[isDemBio]),sum(Cat[isPelCatch]),sum(Bio[istargBio]*TL[istargBio],na.rm=TRUE)/sum(Bio[istargBio]), sum(Cat[istargCat]*TL[istargCat],na.rm=TRUE)/sum(Cat[istargCat]),sum(Cat[isfishCat]),sum(Cat[isfishCat])/sum(Bio[isfish]),  sum(Bio[isBio])/sum(Bio[isPP]),sum(Bio[isDemBio])/sum(Bio[isPP]),sum(Bio[isPelBio])/sum(Bio[isPP]),sum(Cat[isCatch])/sum(Bio[isPP]),sum(Cat[isDemCatch])/sum(Bio[isPP]),sum(Cat[isPelCatch])/sum(Bio[isPP]))
  
  labels <- c("TotBio","TotCat","Cat/Bio","FishBio","DemPelFish","TEPs","Seals","Whales","DemCat","PelCat","MTLBio","MTLCat","FishCat","FishCat/FishBio","Bio/PP","DemBio/PP","PelBio/PP","Cat/PP","DemCat/PP","PelCat/PP")
  
  if (length(bzero)>0)
  {
    depletion <- Bio[2:28]/bzero
    prop.of <- length(depletion[depletion<0.368/2])/length(depletion)
    Ind <- c(Ind,prop.of)
    labels <- c(labels,"PropOF")
  }
  
  Ind <- c(Ind,get.value(Cat,path))
  labels <- c(labels,"Value")
  
  
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
  xx <- as.numeric(na.omit(match(names(catch),prices$code)))
  yy <- catch
  #for (irow in 1:nrow(yy)) 
  yy <- yy*2204.62*prices$price[xx]
  temp <- as.character(long.names$EMOCCName)
  names(yy) <- temp[match(names(yy),long.names$CODE)]
  value <- sum(yy,na.rm=TRUE)
  return(value)
}

 

