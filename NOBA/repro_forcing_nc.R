#' @title Creating Oil Spill reproduction forcing .NC file
#' @description script for creating a forcing .NETCDF file for reduced reproduction (larval survival)following an oil spill. 
#' @author Erik Olsen

#' ----------------------
#' Libraries
library(roxygen2)
library(ncdf4)
library("ncdf", lib.loc="/Users/eriko/Library/R/3.0/library")

#' ----------------------
#' Importing test-file to get structure
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/OilSpill")
Ext.Mort<- nc_open("externalMort-2.nc")
print(Ext.Mort)
#' t = time-step (3 time-steps)
#' b = box number (66 boxes)
#' z = depth layers (7 depths)

#' Extract one variable
LPPmort_ff<-ncvar_get(Ext.Mort, "LPPmort_ff")

#' test prints of file structure
print(paste("The file has", Ext.Mort$nvars,"variables"))
#[1] "The file has 1 variables"
print(paste("The file has", Ext.Mort$ndim,"dimensions"))


#' ----------------------
#' importing NOBA initial conditions file
setwd("~/NOBA_sv/runs/NOBAtest1")
NOBA.Init<-nc_open("nordic_biol_v19.nc")
print(NOBA.Init)
#' t = time-steps 1 (but unlimited)
#' b = box numbers (60)
#' z = depth layers (8)
NOBA.names<-rownames(summary(NOBA.Init$var))
Cod.Test<-ncvar_get(NOBA.Init, "North_atl_cod4_Nums")

#' ----------------------
#' Creating input to NC forcing file

rscale<-c(0.95)
love.box<-rep(1,60)
love.box[6]<-rscale #NB! subsetting one higher than boxno because boxno starts with 0
love.box[7]<-rscale
goliat.box<-rep(1,60)
goliat.box[28]<-rscale
goliat.box[31]<-rscale
goliat.box[42]<-rscale
rmat<-matrix(love.box, nrow=7, ncol=60, byrow=TRUE) # creates a matrix of same size as group with scalars for reprodution in the right boxes
rmat.def<-matrix(1, nrow=7, ncol=60)
group.list<-list(rmat.def, rmat, rmat.def) # list of three matrices, one for each time-step

#' must create array with 3 time steps and reproduction scalar in step 2
rep_nc<-array(rmat.def, c(7,60,3))
rep_nc[,,2]<-rmat

#' Example, setting up ONE single forcing array, and exporting this to a NETCDF file
#' creating NETCDF File
#' define dimensions
dimb=ncdim_def("b","",1:60)
dimz=ncdim_def("z","",1:7)
dimt=ncdim_def("t1","",1:3,unlim=TRUE)#,create_dimvar=FALSE)

#' setting up variables
var.t=ncvar_def("t","seconds since 2008-01-01 00:00:00 +10",dimt,0,prec="double")
var.rep=ncvar_def("ReproductionScalar","PropDef",list(dimz,dimb,dimt),0,prec="double")
#' create loop for each group as variable name

dt=86400

#' create file
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/OilSpill")
nc_rep1=nc_create("rep_scalar_test.nc",list(var.t,var.rep))

#' assign global attributes to temp file
ncatt_put(nc_rep1,0,"title","Temperature file, NoBa")
ncatt_put(nc_rep1,0,"geometry","Nordic.bgm")
ncatt_put(nc_rep1,0,"parameters","")

#' assign attributes to variables
ncatt_put(nc_rep1,var.rep,"dt",86400,prec="double")

#' assign variables to file
#' This writes a .NC file with 1 single 3D array 
#' need to loop over variables to put into NCDF file. 
#' 

ncvar_put(nc_rep1,var.rep,rep_nc)

#' test prints of file structure
print(paste("The file has", nc_rep1$nvars,"variables"))
#[1] "The file has 1 variables"
print(paste("The file has", nc_rep1$ndim,"dimensions"))


nc_close(nc_rep1)


#' Need to create a .NC with 1 3D array pr species group
#' 
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

#' create one 3D array per group. 
#' 
#' Setting up NC variables for all groups
nc.var.list[1]<-c("var.t")
for (i in 1:length(group.names)){
  assign(paste("var.", group.names[i], sep=""), ncvar_def(group.names[i],"PropDef",list(dimz,dimb,dimt),0,prec="double"))
  #assign(paste("var.", group.names[i], sep=""), ncvar_def("RatioS","PropDef",list(dimz,dimb,dimt),0,prec="double"))
  nc.var.list[i+1]<-paste("var.", group.names[i], sep="")
}

#var.rep=ncvar_def("ReproductionScalar","PropDef",list(dimz,dimb,dimt),0,prec="double")

#' Very clunky, outputs as long character string
#' paste(as.character(nc.var.list), collapse=", ")

#' PUT all varibles into .NC file
#' 
#' Create .NC file with all variables
nl<-list(get(nc.var.list[1]))
for (i in 2:length(nc.var.list)){
  nl<-c(nl, list(get(nc.var.list[i])))
}

#' create NC file
nc_rep=nc_create("rep_scalar.nc",nl)

#' assign global attributes to temp file
ncatt_put(nc_rep,0,"title","Temperature file, NoBa")
ncatt_put(nc_rep,0,"geometry","Nordic.bgm")
ncatt_put(nc_rep,0,"parameters","")

#' put array values into NC file variables 
#' for loop over all variables

for (i in 2:length(nc.var.list)){
  ncatt_put(nc_rep,get(nc.var.list[i]),"dt",86400,prec="double")
  ncvar_put(nc_rep,get(nc.var.list[i]),rep_nc)
}

#' test prints of file structure
print(paste("The file has", nc_rep$nvars,"variables"))
#[1] "The file has 1 variables"
print(paste("The file has", nc_rep$ndim,"dimensions"))

#' close NC file
nc_close(nc_rep)
