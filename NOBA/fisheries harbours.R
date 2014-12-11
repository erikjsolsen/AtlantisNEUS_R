### NOBA Atlantis FISHERIES HARBOURS 
# analysis of Norwegian fisheries harbours
# By: Erik Olsen
# Created: 4.11.2014
# Updated: 5.12.2014

library(lattice) #load lattice library
library(RColorBrewer)
library(plyr)
library(ggplot2)
library("sp", lib.loc="/Users/eriko/Library/R/3.0/library")
library("geosphere", lib.loc="/Users/eriko/Library/R/3.0/library")
library(rgdal)
library(rgeos)
library("maps", lib.loc="/Users/eriko/Library/R/3.0/library")


setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/fiskeri")

#harbours<-read.csv("fiskerianlegg.csv")
harbours<-read.csv("fiskerianlegg.csv")
postcodes<-read.table("postnummer.csv", header=TRUE, sep="\t")



#add LAT and LON to the harbours file
harbours$LAT <-0
harbours$LON <-0

for (i in 1 : length(harbours$POSTCODE)){ 
  harbours$LAT[i] <- postcodes$LAT[grep(harbours$POSTCODE[i], postcodes$POSTNR)]
  harbours$LON[i] <- postcodes$LON[grep(harbours$POSTCODE[i], postcodes$POSTNR)]
  }

harbours$group<-0

#subset most important harbours

harbours10<-harbours[1:10,]
harbours20<-harbours[1:20,]
harbours30<-harbours[1:30,]

### use  rbm.r code by M.SUmmer
# must run code manually, step by step. 
# must manually specify the prjstring:
#prjstring <- c("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs") 

# 
#NOBAsp<-spdf 

###import NOBA shape-file instead of the BGM file
 
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/nordic_grid_220812")
NOBAsp <- readOGR(dsn = ".", "MENUIIareasPolNewId_grass_tol0p01")
setwd("~/Documents/G-copy/USA studieopphold/atlantis/NOBA atlantis/fiskeri")

slotNames(NOBAsp) # look at the slotnames
names(NOBAsp)
str(NOBAsp, max.level=3)

#changes projection to LAT LON using projection information gathered from the 'slotNames' command above
NOBAsp2 <- spTransform(NOBAsp, CRS("+proj=longlat +ellps=GRS80"))

NOBA.f<-fortify(NOBAsp2) #creates X - Y points of the polygons

cnames <- aggregate(cbind(long, lat) ~ id, data=NOBA.f, FUN=function(x)mean(range(x)))

#legge til land
world<- map_data("world") 
NOBAmap <- ggplot(world, aes(x=long, y=lat, group=group)) + geom_path(colour="gray50") + scale_y_continuous(breaks=(-2:2) * 30) + scale_x_continuous(breaks=(-4:4) * 45) + xlim(-23,70) +ylim(58,85)
NOBAmap <- NOBAmap + geom_polygon(data=NOBA.f, aes(x=long, y=lat, group=group),colour="burlywood2", fill=NA) +theme_bw() + ggtitle("NOBA map with fisheries harbours ")

# 10, 20 og 30 viktigste havner
HCol<-rev(brewer.pal(3, "YlGnBu"))
NOBAmap + geom_point(data=harbours30, aes(x=LON, y=LAT, group=group, colour="30"), size=4) + geom_point(data=harbours20, aes(x=LON, y=LAT, group=group, colour="20"), size=4) + geom_point(data=harbours10, aes(x=LON, y=LAT, group=group, colour="10"), size=4) + guides(fill=guide_legend(title="Fisheries harbours")) + scale_color_manual(name = "Fisheries harbours", labels = c(10, 20, 30), values = HCol)

ggsave("fisheries harbours.pdf", scale = 1, dpi = 400)







### OLD  ##
## Barplot of harbours by landings
harbours.bar<-subset(harbours, Landings>1000) 

sort.harbours<-harbours.bar[order(harbours.bar$Landings,harbours.bar$Vessels,-harbours.bar$Fishers),]


harbours.names<-rownames(sort.harbours)
op <- par(oma=c(5,7,1,1))
par(mar = rep(2, 4))
barplot(sort.harbours$Landings, names.arg=harbours.names, horiz=TRUE, col="lightskyblue2", cex.names=0.6, las=1, main="Landings(t) pr. county (kommune) > 1000t annual landing")
