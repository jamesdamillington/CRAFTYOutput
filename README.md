# CRAFTYOutput
**Scripts to analyse output from CRAFTY (Brazil)**

The [individual analysis scripts](#individual-analysis-scripts) described below produce numerous model output analysis files. Many scripts have dependencies on files produced by other scripts. The number prefixing a filename (e.g. 1_summarise...) indicates the order in which files should be excuted.

To ease use however, the [A_allOutputAnalysis.r](A_allOutputAnalysis.r) script combines several of these scripts (2,3,4,7,8) into a single script in the correct order. Note that this combined file assumes output from a model run in which CRAFTY and STELLA are linked. When STELLA is not used, the final section of the script will fail (i.e. code from [8_analyseProduction.r](8_analyseProduction.r)); in this case [8a_analyseProduction_solo.r](8a_analyseProduction_solo.r) on completion of the combined script. 

To use any of the files for analysing output from a given model run, the following values must be set (all as character strings):

- `scenario`: the scenario CRAFTY parameter (set in Scenario.xml) used to name the run (e.g. *"Testing_2018-08-12"*)
- `runID`: the CRAFTY ID of current run (*runNumber*-*randomSeed*) (e.g. *"0-0"*)
- `cl`: the land cover classification used in model data (from [MapBiomas data](http://mapbiomas.org); e.g.  *"PastureB"*)

Other items that can be specified include:

- `yrs`: a list of integers indicating the years for which the model was run (e.g. *seq(2000, 2015, 1)*)
- `calib_yrs`: a list of integers indicating the years calibration analysis; ususally this will be a selection of `yrs` (e.g. *c(2005, 2010, 2015)*)
- `sim_yrs`: a list of integers indicating the years to include in any video made (will usually be identical to `yrs` above)
- `fig_yrs`: a list of integers indicating the years for which figures should be output (often the same as `calib_yrs`) 
- `pdfprint`: a boolean indicating whether pdf files should be output (usually this should be *TRUE*
- `video_output`: a boolean indicating whether video files should be made (can be slow so often set to *FALSE*) 

The `data_dir` variable can also be changed, but this is currently set to assume output scenario directory is copied to *CRAFTY_testing/CRAFTYOutput/Data/*. Files assumed to be in this directoy are the CRAFTY cell values csv files for each year (e.g. *Testing_2018-08-12-0-0-Cell-2001.csv*) and the *region.csv* that was used to initialise the run. Other files assumed to exist are:

- *sim10_BRmunis_latlon_5km_2018-04-27.asc* (currently) in a directory named *ObservedLCmaps* (in the Data directory
- summary tables (produced by [1_summarise_LCmaps_5LCs.r](1_summarise_LCmaps_5LCs.r)) in a directory named *SummaryTables*

## Individual analysis scripts

- [1_summarise_LCmaps_5LCs.r](1_summarise_LCmaps_5LCs.r): needed only once to produce SummaryTables, but prior to other CRAFTYOutput scripts. Takes the MapBiomas input maps [(currently version 2.3)](http://mapbiomas.org/pages/database/mapbiomas_collection) and converts them to tables of summary data by municipality
- [2_CRAFTYsummary_5LCs.r](2_CRAFTYsummary_5LCs.r): takes CRAFTY cell values csv files for each year and combines them with region.csv to summarise data to municipality level. Two summary files are produced:

+ summary of Services and Capitals (ending *CRAFTYmunisServCap.csv*)
+ summary of Land Cover (ending *CRAFTYmunisLC.csv*)

- [3_LCcalibrationAnalysis_5LCs.r](3_LCcalibrationAnalysis_5LCs.r): comparison analysis of modelled vs observed land cover; reads a file created by [2_CRAFTYsummary_5LCs.r](2_CRAFTYsummary_5LCs.r) and creates a pdf file (name ending  *_LCcomparisonAnalysis.pdf*)
- [4_LCcalibrationMaps_5LCs.r](4_LCcalibrationMaps_5LCs.r): creates comparison analysis maps of modelled vs observed land cover; reads a file created by [2_CRAFTYsummary_5LCs.r](2_CRAFTYsummary_5LCs.r) and creates a pdf file (name ending  *_LCcomparisonMaps.pdf*)
- [5_LCcalibrationShiny_5LCs.r](5_LCcalibrationShiny_5LCs.r): creates interactive Shiny maps for comparison of modelled vs observed land cover (**possibly currently incomplete, needs checking**)
- [6_LCcalibrationAnalysis_singleMuni_5LCs.r](6_LCcalibrationAnalysis_singleMuni_5LCs.r): analysis as for [3_LCcalibrationAnalysis_5LCs.r](3_LCcalibrationAnalysis_5LCs.r) but for a single municipality (specified by `target_muni` variable)
- [7_outputRasterAnalysis.r](7_outputRasterAnalysis.r): produces figures (images) and videos of land cover, Services and Capitals for both vector (Municipality) and raster (cell-by-cell) data. Requires files from multiple other individual analysis scripts. Will also output comparison matrices for pairs of ratser maps **Not yet complete!**
- [8_analyseProduction.r](8_analyseProduction.r): analyses production data, reading values from *FromMaestro####.csv* files from *StellaData* directory
- [8a_analyseProduction_solo.r](8a_analyseProduction_solo.r): version of [8_analyseProduction.r](8_analyseProduction.r) script but using Services calculated from the CRAFTY cell values csv file. Used with data from runs that do not use STELLA (i.e. no *FromMaestro####.csv* files created) 
