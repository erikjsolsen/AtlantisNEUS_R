#' modifyPRM.r
#'
#'
#' 
#' Created: 09.02.2015
#' @param PRMfile - Atlantis .PRM file that should be modified
#' @param SearchStr  - text-string to search for in the .PRM file to find the parameter that should be adjusted
#' @param AreaList - list of integers of Atlantis areas for which the parameter needs to be changed
#' @param Plevel - the new levels of the parameter
#' @return a modified .PRM file
#' @export
#' @author Erik Olsen <erik.js.olsen@@gmail.com>
#' @examples modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(7,8), 0.9)
# 


library("roxygen2", lib.loc="/Users/eriko/Library/R/3.0/library") 

modifyPRM <- function(PRMfile, SearchStr, AreaList, Plevel) {
  hh<-readLines(PRMfile, n=-1)
  
  ff<-grep(SearchStr, hh, value=TRUE) 
  #' character string length is hardwired and needs to be changed for other parameters 
  ff2<-strsplit(ff[grep(19,nchar(ff))],  "[[:blank:]]")
  
  PRM<-vector("list", length(ff2))
    for (i in 1:length(ff2)) 
    { 
     PRM[i]<-c(ff2[[i]][1])
    }
  
  #define closurestring based on AreaList and Plevel
  closurestring<-hh[c(grep(PRM[1], hh,)+1)]
  closurestring<-strsplit(closurestring, fixed=TRUE, " ")
  AreaList<-AreaList+1
  
  for (i in 1:length(AreaList)) 
    { closurestring[[1]][AreaList[i]]<-Plevel    
    }
  
  CLS<-paste(unlist(closurestring), sep="", collapse=" ")
  
  for (i in 1:length(PRM)) #loops through all Fisheries and changes closures 'closurestring'
    { 
      ln<-c(grep(PRM[i], hh,)+1) #use Grep to find the linenumber for the line with the fishery
      hh[ln]<-CLS #changes fisheries in the designated areas to the designated value
    }
  
  # Write to file
  write(hh, file=PRMfile) 
    
  
}