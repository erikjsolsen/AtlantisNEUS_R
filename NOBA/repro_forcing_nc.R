#' @title Function for creating Oil Spill reproduction forcing .NC file for NOBA Atlantis model
#' @description Function  for creating a forcing .NETCDF file for reduced reproduction (larval survival)following an oil spill. 
#' @details NOBA size and scale is hard-coded, so needs to be changed in function code to work on other models
#' @param rscale = single scalar for reproduction used in boxes affected by pollution event
#' @param boxes = single boxnumber or vector of boxnumbers for boxes affected by pollution event
#' @param ncfile = filename for NETCDFfile to be created
#' 
#' @author Erik Olsen

repro_force <- function(rscale, boxes, ncfile){

#' ----------------------
#' Libraries
library(roxygen2)
library(ncdf4)
library(ncdf)

#' ----------------------
#' Setting up arrays of reproduction scalars

#rscale<-c(0.95)
love.box<-rep(1,60)
for (i in 1:length(boxes)){
love.box[i+1]<-rscale #NB! subsetting one higher than boxno because boxno starts with 0
}
rmat<-matrix(love.box, nrow=8, ncol=60, byrow=TRUE) # creates a matrix of same size as group with scalars for reprodution in the right boxes
rmat.def<-matrix(1, nrow=8, ncol=60)
#group.list<-list(rmat.def, rmat, rmat.def) # list of three matrices, one for each time-step

#' must create array with 3 time steps and reproduction scalar in step 2
rep_nc<-array(rmat.def, c(8,60,3))
rep_nc[,,2]<-rmat


#' ------------------------------- 
#' Create a list of groups_X_age_rec_ff to use to name variables in .NC file
NOBA.groups<-read.csv("/Users/eriko/NOBA_sv/runs/NOBAtest1/nordic_groups_v02.csv")
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
#' define dimensions
dimb=ncdim_def("b","",1:60)
dimz=ncdim_def("z","",1:8)
dimt=ncdim_def("t1","",1:3,unlim=TRUE)#,create_dimvar=FALSE)


#' Setting up 3D arrays as NC variables for all groups
var.t=ncvar_def("t","seconds since 2008-01-01 00:00:00 +10",dimt,0,prec="double")
nc.var.list<-c("var.t")
for (i in 1:length(group.names)){
  assign(paste("var.", group.names[i], sep=""), ncvar_def(group.names[i],"PropDef",list(dimz,dimb,dimt),0,prec="double"))
  #assign(paste("var.", group.names[i], sep=""), ncvar_def("RatioS","PropDef",list(dimz,dimb,dimt),0,prec="double"))
  nc.var.list[i+1]<-paste("var.", group.names[i], sep="")
}



#' ------------------------------
#' Creating .NC file with all variables
#' step 1: make a list of all NCDF variables 
nl<-list(get(nc.var.list[1]))
for (i in 2:length(nc.var.list)){
  nl<-c(nl, list(get(nc.var.list[i])))
}

#' step 2: create of the actual NC file with all variables
nc_rep=nc_create(paste(ncfile,"nc", sep=""),nl)

#' step 3: assign global attributes to temp file
ncatt_put(nc_rep,0,"title","Temperature file, NoBa")
ncatt_put(nc_rep,0,"geometry","Nordic.bgm")
ncatt_put(nc_rep,0,"parameters","")

#' step 4: put NC variables into NC file variables 
#' for loop over all variables

for (i in 2:length(nc.var.list)){
  ncatt_put(nc_rep,get(nc.var.list[i]),"dt",86400,prec="double")
  ncvar_put(nc_rep,get(nc.var.list[i]),rep_nc)
}

#' test prints of file structure
print(paste("The file has", nc_rep$nvars,"variables and", nc_rep$ndim,"dimensions"))

#' close NC file
nc_close(nc_rep) # this also writes final changes to the .nc file

} # end function repro_force