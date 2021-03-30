
# Documentation for MetroScope forecast tool, used by Oregon Metro
 
## Overview 
MetroScope is an integrated set of econometric, land use, and transport models, currently used at Metro for producing regional forecasts and evaluating a wide range of policy scenarios.  

For each forecast year, regional control totals for demographics, households, and employment are provided from an econometric model.  The land use model takes these forecasts and distributes them spatially over the buildable land capacity for the entire region.  A transport module is integrated with the land use model, so that the household and employment distributions can respond to changes in travel time.  

For both the core residential and non-residential modules, market forces determine both the supply and demand for real estate.  If supply is less than demand, then prices are increased to make it more profitable to build. Likewise, if supply is greater than demand, prices are decreased to make housing more affordable.  The modules are then iterated until supply matches demand.   

## Documentation Files in this directory
* Operator Guide
* Technical Overview
* Input/Output Tables
* Functions

## R Code

