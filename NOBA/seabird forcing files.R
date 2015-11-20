#' @title  Creating NCDF forcing file for Increasing sea-bird mortality to replicate potential effects of wind farms in coastal areas of Norway 
#' @details creates a .ncdf file with mortality forcing data pr. time step, box, species age group
#' @author Erik Olsen


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
#' 
#' 