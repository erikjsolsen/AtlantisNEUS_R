AtlantisNEUS_R
==============

This is a collection on R functions and R scripts for interacting and analyzing the output of the Atlantis NEUS (Northeast USA) marine end2end ecosystem model. The scripts analyze the NETCDF output files for the biological components as well as the catches and plots/analyzes them in various ways. 

The repository also includes R scripts for modifying the input .PRM files

Once we're confident that the scripts and functions are working they will be uploaded to the Atlantis WiKi. 

Current index (March 2014):
- analyzingScenarioTable.r      -- Script that creates heat-plots of biomass at end-time of different scenarios
- animation script.r            -- Unfinished script that creates animated plots
- biomultiplot.r                -- Function that creates faceted spatial plots of biomass of different components
- catchmultiplot.r              -- Function that creates faceted spatial plots of catches of different components
- mappinNEUS.R                  -- Script that takes shape-file of NEUS region, creates R-objects that can be used by biomultiplot and catchmultiplot
- ModifyInputPRM.R              -- Script used to change MPA settings in 'harvest.prm' file 
- NETCDF manipulation v2.R      -- "Working script" (draft) that does the same as biomultiplot - DON'T RUN!
- NEUS catch manipulation.r     -- "Working script" (draft) that does the same as catchmultiplot - DON'T RUN
- PlotOutNC_NandRNandNums_NEUS  -- Biomass/N plotting function developed by Isaac Kaplan, NOAA, NWFSC
- running biomultiplot.r        -- script for running biomultiplot
- running catch_multiplot.r     -- script for running catchmultiplot
- VertNamesInPlotOrderNEUS      -- data file needed to run PlotOUt...



