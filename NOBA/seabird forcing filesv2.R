#' @title  Creating NCDF forcing file for Increasing sea-bird mortality to replicate potential effects of wind farms in coastal areas of Norway 
#' @details creates a .ncdf file with mortality forcing data pr. time step, box, species age group
#' @author Erik Olsen


seabird_force <- function(rscale, boxes, ncfile){
  
  #' setting vars to run function
    #' boxes<-c(1, 2, 3, 4, 28, 29)
    #' rscale<-1.5
    #' ncfile<-c("seabird_m15")

#' LIBRARY and SOURCES
#' ---------------------
library(roxygen2)
library(ncdf4)
library(ncdf)


#' SETTING UP ARRAYS
#' --------------------
#' Set up array with changes in mortality (increase: dM) in areas (boxes: B) at each time step (T) of the species_age_grouo(S_N)
#' 
#' Use Repro_forcing_NC as basis, but change just mortality  (eg - name, change of variables from _rec to _mort)

  #' ----------------------
  #' Setting up arrays of reproduction scalars
  
  #rscale<-c(0.95)
  love.box<-rep(1,60)
  for (i in 1:length(boxes)){
    #love.box[i+1]<-rscale #NB! subsetting one higher than boxno because boxno starts with 0
    love.box[boxes[i]+1]<-rscale
  }
  rmat<-matrix(love.box, nrow=8, ncol=60, byrow=TRUE) # creates a matrix of same size as group with scalars for reprodution in the right boxes
  rmat.def<-matrix(1, nrow=8, ncol=60)
  #group.list<-list(rmat.def, rmat, rmat.def) # list of three matrices, one for each time-step
  
  #' must create array with 2 time steps and mortality scalar in step 2 - to be able to start mortality after burn-in
  rep_nc<-array(rmat.def, c(8,60,2))
  rep_nc[,,2]<-rmat
  
  
  #' ------------------------------- 
  #' Create a list of groups_X_age_rec_ff to use to name variables in .NC file
  NOBA.groups<-read.csv("/Users/eriko/ownCloud/Research/atlantis/NOBA/NOBA_run_files/nordic_groups_v03.csv")
  #' select only seabirds
  NOBA.groups<-subset(NOBA.groups, Code==c("SBA", "SBB")) 
  group.names<-data.frame("t")
  colnames(group.names)<-c("GName")
  attach(NOBA.groups)
  
  for (i in 1:length(NumCohorts)){
    if (NumCohorts[i]==1) {
      GN<-as.data.frame(paste(Code[i], "rec_ff", sep=""))
      colnames(GN)<-c("GName")
      group.names<-rbind(group.names, GN)
    } else {
      ag<-c(1:NumCohorts[i])
      for (j in 1:length(ag)){
        GN<-as.data.frame(paste(Code[i],ag[j],"rec_ff", sep=""))
        colnames(GN)<-c("GName")
        group.names<-rbind(group.names, GN)
      }
    }
  }
  detach(NOBA.groups)
  group.names.compl<-group.names
  group.names<-as.vector(group.names[2:length(group.names$GName),])
  
  #' ------------------------------ 
  #' define dimensions, year 0, year 25
  t_step<-c(0, 788400000)
  dimb=ncdim_def("b","",1:60)
  dimz=ncdim_def("z","",1:8)
  dimt=ncdim_def("t","",t_step,unlim=TRUE)#,create_dimvar=FALSE)
  
  #Create the list.
  varList  = vector('list', length(group.names))
  
  for (i in 1:length(group.names)){
    varList[[i]] = ncvar_def(group.names[i],"PropDef",list(dimz,dimb,dimt),0,prec="double")
  }
  
  nc_rep=nc_create(paste(ncfile,".nc", sep=""),varList)
  
  #' step 3: assign global attributes to temp file
  ncatt_put(nc_rep,0,"title","Temperature file, NoBa")
  ncatt_put(nc_rep,0,"geometry","Nordic.bgm")
  ncatt_put(nc_rep,0,"parameters","")
  
  #ncatt_put(nc_rep,'t',"dt",86400,prec="double")
  ncatt_put(nc_rep,'t',"dt",86400,prec="double")
  ncatt_put(nc_rep, 't', "units", "seconds since 1983-01-01 00:00:00 +10", prec="text")
  
  #' step 4: put NC variables into NC file variables 
  #' for loop over all variables
  
  for (i in 1:length(varList)){
    ncvar_put(nc_rep,varList[[i]],rep_nc)
  }
  
  #' test prints of file structure
  print(paste("The file has", nc_rep$nvars,"variables and", nc_rep$ndim,"dimensions"))
  
  #' close NC file
  nc_close(nc_rep) # this also writes final changes to the .nc file
  
} # end function repro_force

