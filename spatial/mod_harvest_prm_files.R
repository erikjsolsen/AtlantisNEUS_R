#' Script used to run modifyPRM function to set up all necessary harves.PRM files for the spatial scenarios
#' @author: Erik Olsen
#' created: 10.02.2015
#' 

source("/Users/eriko/AtlantisNEUS_R/modifyPRM.R")

#' All areas:
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/ALL10")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(0:29), 0.9)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/ALL20")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(0:29), 0.8)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/ALL50")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(0:29), 0.5)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/ALL100")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(0:29), 0.0)


#' Cape Wind (CW areas)
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/CW10")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(7,8), 0.9)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/CW20")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(7,8), 0.8)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/CW50")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(7,8), 0.5)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/CW100")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(7,8), 0.0)


#' Georges Bank (GB)
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GB10")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(8, 9, 12, 13, 14, 15), 0.9)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GB20")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(8, 9, 12, 13, 14, 15), 0.8)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GB50")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(8, 9, 12, 13, 14, 15), 0.5)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GB100")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(8, 9, 12, 13, 14, 15), 0.0)


#' Gulf of Maine (GoM)
setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GOM10")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(10, 11, 12, 16, 17, 18, 19,  20), 0.9)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GOM20")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(10, 11, 12, 16, 17, 18, 19,  20), 0.8)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GOM50")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(10, 11, 12, 16, 17, 18, 19,  20), 0.5)

setwd("/Users/eriko/Documents/G-copy/USA studieopphold/atlantis/Atlantis NEUS/NEUS_Spatial/GOM100")
modifyPRM("at_harvest_neus_v15_spatial.prm", "MPA", c(10, 11, 12, 16, 17, 18, 19,  20), 0.0)

