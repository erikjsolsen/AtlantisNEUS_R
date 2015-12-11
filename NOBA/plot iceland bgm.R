#' @title Plotting Iceland model BGM file with NOBA geometry
#' @author Erik Olsen
#' 
#' Use rBGM package from github
#' install.packages("devtools")
#' ended up using 'BGM_to_map' function by Alex Keth


setwd("~/ownCloud/Research/atlantis/iceland")

library(rbgm)
library(proj4)
library(maps)
library(maptools)
library(ggplot2)

#' Source functions
#devtools::install_github("mdsumner/rbgm")
source("~/AtlantisNEUS_R/NOBA/BGM to map.r")
source("~/AtlantisNEUS_R/NOBA/str_split.R")


#' Read Iceland and Norwegian BGM files
#ICL<- read_bgm("atlantis_L93.bgm")
#ICL_face<-read.faces("atlantis_L93.bgm")
#ICL_proj<-proj4string(ICL_face)
#ICL_face2<-spTransform(ICL_face, CRS("+proj=longlat +ellps=GRS80"))
#ICL.f<-fortify(ICL_face2, region="id")
ICL<-bgm_to_map("atlantis_L93.bgm", vert)
colnames(ICL)<-c("x","y", "group", "long", "lat")

#NOR<-read_bgm("~/ownCloud/Research/atlantis/NOBA/NOBA_run_files/Nordic02.bgm")
#NOR_face<-read.faces("~/ownCloud/Research/atlantis/NOBA/NOBA_run_files/Nordic02.bgm")
#NOR_proj<-proj4string(NOR_face)
#NOR_face2<-spTransform(NOR_face, CRS("+proj=longlat +ellps=GRS80"))
#NOR.f<-fortify(NOR_face2, region="id")

NOR<-bgm_to_map("~/ownCloud/Research/atlantis/NOBA/NOBA_run_files/Nordic02.bgm", vert)
colnames(NOR)<-c("x","y", "group", "long", "lat")

#' build BGM 


#' Plot Polygons on world map
world<- map_data("world") 
map1 <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_polygon(colour="gray65", fill="gray65") +  coord_cartesian(xlim = c(-50, 70), ylim=c(58, 85))
map1

map1 <- map1 + geom_polygon(data=NOR, aes(x=long, y=lat, group=group),colour="red", fill="slategray1", label=id) +theme_bw() + ggtitle("ICL - NOR ATLANTIS model area") + theme(plot.title = element_text(size=16, face="bold")) 
map1

map1 <- map1 + geom_polygon(data=ICL, aes(x=long, y=lat, group=group),colour="blue", fill=NA, label=id) +theme_bw()
map1
ggsave("ICE_NOR_atlantis_areas.pdf")
