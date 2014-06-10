### Skill assessment
#  - Calculating metrics
#  - plotting biomass
#  - creating heat-maps
#
# By: Erik Olsen
# Date: 06.06.2014

### Setting up libraries
library(ggplot2)
library(reshape2)



### Importing data files and species codes
setwd("~/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/skill assessment/data") 
load("Totbiomass_atcodes.RData")
survey_biom<-atl.biomass
# must converst survey_biom to tons
survey_biom<-cbind(survey_biom[1], survey_biom[2:23]/1000)
load("Species_groups.RData")
model_biom<-read.table("Modeled_Biomass.csv", head=TRUE, sep=" ")
species_codes<-read.csv("NEUS_species_codes.csv")

# give real-world species names
NEUS.names<-colnames(model_biom)

for (j in 2:length(NEUS.names)) {
  NEUS.names[j]<-as.character(species_codes$NAME[match(NEUS.names[j], species_codes$CODE)] )
}

colnames(model_biom)<-NEUS.names
colnames(survey_biom)<-NEUS.names

### BIOMASS PLOTS OVER TIME
for (i in 1:length(species_codes_used)) {
  biom_plot<-ggplot(survey_biom, aes(x=Year, y=species_codes_used[i])) + geom_line(color="red") + theme_bw()
  biom_plot + geom_line(data=model_biom, aes(x=Year, y=species_codes_used[i]), color="blue")
}

biom_plot<-ggplot(survey_biom, aes(x=Year, y=species_codes_used[i])) + geom_line(color="red") + theme_bw()
biom_plot + geom_line(data=model_biom, aes(x=Year, y=species_codes_used[i]), color="blue")

# Make flattened file
survey.melt<-melt(survey_biom, id=c("Year"))
model.melt<-melt(model_biom, id=c("Year"))

# Faceted survey/model plots 
facet_biom_plot<-ggplot(survey.melt, aes(x=Year, y=value)) + geom_line(color="red") + theme_bw() + facet_wrap(~variable, scales = "free_y")

facet_biom_plot + geom_segment(aes(x = 2004, y = 0, xend = 2013, yend = 0)) 
facet_biom_plot +  geom_line(data=model.melt, aes(x=Year, y=value), color="blue") + geom_segment(aes(x = 2004, y = 0, xend = 2013, yend = 0)) 

ggsave(paste("model_survey_faceted.png"), scale = 1, dpi = 400)




