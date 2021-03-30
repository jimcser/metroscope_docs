# G35_res_supply_module0.R
# maintained by Jim Cser jim.cser@oregonmetro.gov
# last updated 3/12/2013

# MetroScope Land Use / Transportation Model. Copyright (C) 2013 Metro
#
# You can redistribute this program and/or modify it under the terms of the
# GNU General Public License as published by the Free Software Foundation
# (http://www.gnu.org/copyleft/gpl.html).
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the file LICENSE.html for copyright
# and licensing information, and the file ACKNOWLEDGMENTS.html for funding and
# other acknowledgments.
#

runResSupply <- function(iLoops){
# runResSupply()
# The residential supply module for MetroScope.
#   
# The raw buildable acres available are the acres remaining from the previous model year plus
# then new acres added in the current model year.  This amount is "throttled" by both an assumed
# base fraction (not all units will be put on the market every year) and a price factor (as prices
# go up, more units go on the market).  
#
# General steps for res supply calculations:
# 1) Calculate effective lot size for each zone class and housing type,
# 2) Use lot size and  buildable acres to determine eligible new DU supply.
# 3) For each housing price bin, compare the cost to build a DU to the amount a household is willing to pay.
# 4) If construction cost is less than bid price, then build, otherwise don't build. 
# 5) Calculate the total acres consumed by prorating by the ratio of built and unbuilt DU supply.
#
# Input Parameters:
# iLoops = current iteration of res demand module
#
# Calls:
# calcResFeasibleSupply()
# calcResAcresConsumed()
#
# Returns:
# newSupply = new units of regular DU supply
# newSupplyUR = new units of UR DU supply
# res_supply_Rz = total DU supply
# acresConsumed = res regular acres consumed in current model year
# acresRemaining = res regular acres remaining in current model year
# acresConsumedUR = res regular acres consumed in current model year
# acresRemainingUR = res UR acres remaining in current model year


    ##### parameters
        
	# named paramters, single value 
	 
    res_general_params_in <- readTable(scenarioID,"inputs_res","res_general_params")    
    rownames(res_general_params_in) <- noquote(res_general_params_in$parameter)
    res_general_params <- res_general_params_in[,"value",F]
       
       
       
    # changed to have different values for sf, mf  JBC 2014-02-17 1:04:21 PM
    #res_feesubsidy_in <- readTable(scenarioID,"inputs_res","res_supply_feesubsidy")
    #res_fee <- res_feesubsidy_in$fee                  # increases cost to build
     
    res_feesubsidy_in <- readTable(scenarioID,"inputs_res","res_supply_feesubsidy")
    res_fee <- as.matrix(res_feesubsidy_in[,c("fee_sf","fee_mf","fee_sf","fee_mf")])         # increases cost to build
    res_subsidy <- -1 * as.matrix(res_feesubsidy_in[,c("subsidy_sf","subsidy_mf","subsidy_sf","subsidy_mf")])    # decreases cost to build
        
    res_zclass_lotsize_in <- readTable(scenarioID,"inputs_res","res_supply_zclass_lotsize")
    res_zclass_lotsize <- res_zclass_lotsize_in[,2:4]    
  	
    res_base_bldgcost_in <- readTable(scenarioID,"inputs_res","res_supply_base_bldgcost")
    res_base_bldgcost <- res_base_bldgcost_in$bldgcost
  	
    # res land use accounting
    acresAdded_in <- readTable(scenarioID,"inputs_res",paste("res_acres_added_year",currentYear,sep=""))
    acresAdded <- acresAdded_in[,3:6]
    acresAddedUR <- acresAdded_in[,7:10]
   
    if (currentYear == 1) {
        acresStock <- 0
        acresStockUR <- 0             
    } else {
        acresStock_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("res_acres_remaining_year",(currentYear-1),sep=""))
        acresStock <- acresStock_in[,3:6]
        acresStockUR <- acresStock_in[,7:10]
    }

	acresAvail <-  acresStock + acresAdded
    acresAvailUR <-  acresStockUR + acresAddedUR
	 	 
    ## get vintage supply from year N-1.  At end of iterations, write res_supply for current year to /inputs_res
    res_supply_vintage_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("res_totalsupply_year",(currentYear-1),sep=""))    
	res_supply_vintage <- res_supply_vintage_in[,2:5]

    
    res_base_salesfraction_in <- readTable(scenarioID,"inputs_res","res_supply_base_salesfraction")
    res_base_salesfraction <- res_base_salesfraction_in[,2:5]
      
      
    # load location price, shared between supply and demand modules
    # at first iteration of model year, use location price from previous year, otherwise use the updated value
    if (iLoops == 1 & iLanduseLoops == 1){
        res_locationprice_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("res_locationprice_year",(currentYear-1),sep=""))
    } else {
        res_locationprice_in <- readTable(scenarioID,"shared_res",paste("res_locationprice_year",currentYear,sep=""))
    }

    res_location_price <- as.matrix(res_locationprice_in[,2:5]) 
 
    res_calibration_price_in <- readTable(scenarioID,"outputs_year0","res_locationprice_year0")
    res_calibration_price <- as.matrix(res_calibration_price_in[,2:5]) 
    
    # not needed  JBC 2014-02-17 1:11:09 PM
#    # ezone x rzone    ezone_from, rzone_to
#    if (iMainLoops == 1){
#        res_traveltime_yearN_in <-  readTable(scenarioID,"shared_transport",paste("res_traveltime_year",(currentYear-1),sep=""))
#    } else {
#        res_traveltime_yearN_in <-  readTable(scenarioID,"shared_transport",paste("res_traveltime_year",currentYear,sep=""))
#    }
#
### fixed travel times
##    res_traveltime_yearN_in <-  readTable(scenarioID,"shared_transport",paste("res_traveltime_year",currentYear,sep=""))
#
#   res_traveltime_yearN <- res_traveltime_yearN_in[,3]
        
    
   #lotPrice_in <- read.csv("inputs/res_base_lotprice.csv")
   lotPrice_in  <- readTable(scenarioID,"inputs_res","res_supply_base_lotprice")
   lotPrice <- lotPrice_in$baselotprice

   # from demand module
   avgHouseSizeBin <- readTable(scenarioID,"shared_res",paste("avgHouseSizeBin_year",currentYear,sep=""))
   avgLotSizeBin <- readTable(scenarioID,"shared_res",paste("avgLotSizeBin_year",currentYear,sep=""))
   avgHedonicBin <- readTable(scenarioID,"shared_res",paste("avgHedonicBin_year",currentYear,sep=""))
  
 ################
    feasibleSupply <- calcResFeasibleSupply(res_general_params,res_location_price, res_calibration_price, as.matrix(res_base_salesfraction), as.matrix(acresAvail), as.matrix(acresAvailUR), res_zclass_lotsize)
    
    supplyFeasible <- feasibleSupply$supplyFeasible
    supplyFeasibleUR <- feasibleSupply$supplyFeasibleUR
    acresAvail <- feasibleSupply$acresAvail
    acresAvailUR <- feasibleSupply$acresAvailUR
    lotPriceChange <- feasibleSupply$lotPriceChange
     
    # unneeded  JBC 2014-02-17 2:03:14 PM       
    #res_base_salesfraction_in <- readTable(scenarioID,"inputs_res","res_supply_base_salesfraction")

    supplyRegular <- calcResAcresConsumed(res_general_params, as.matrix(lotPrice), lotPriceChange, 
                as.matrix(avgLotSizeBin), as.matrix(avgHouseSizeBin), as.matrix(avgHedonicBin),res_base_bldgcost, 
                res_fee, totalCostFraction, supplyFeasible,  acresAvail,iLoops)
                
    supplyUR <- calcResAcresConsumed(res_general_params, as.matrix(lotPrice), lotPriceChange, 
                as.matrix(avgLotSizeBin), as.matrix(avgHouseSizeBin), as.matrix(avgHedonicBin),res_base_bldgcost, 
                res_subsidy, totalCostFraction, supplyFeasibleUR,  acresAvailUR, iLoops)

                  
	acresConsumed <- supplyRegular$acresConsumed   # Rzones x Htypes x Zclass		
			
	acresConsumedUR <- supplyUR$acresConsumed
	
	acresInput <-  acresStock + acresAdded
    acresInputUR <- acresStockUR + acresAddedUR

 	acresRemaining <- acresInput - acresConsumed
    acresRemainingUR <- acresInputUR - acresConsumedUR

    acresRemaining[acresRemaining < 0.001] <- 0
    acresRemainingUR[acresRemainingUR < 0.001] <- 0
	
	newSupply <- supplyRegular$newSupply
	newSupplyUR <- supplyUR$newSupply

	newSupplyRzZc <- supplyRegular$newSupplyRzZc
	newSupplyURRzZc <- supplyUR$newSupplyRzZc
    
	newSupplyRzBin<- supplyRegular$newSupplyRzBin
	newSupplyURRzBin <- supplyUR$newSupplyRzBin

	#Calculate total supply (rzone x htype)
	supplyRz <- supplyRegular$newSupply + supplyUR$newSupply + res_supply_vintage

    # added Supply Rz Zc    2014-08-20
    # supply rz bin convert to vector JBC 2015-05-22 4:08:37 PM
    resultsList <- list(newSupply=newSupply,newSupplyUR=newSupplyUR, res_supply_Rz=supplyRz, 
                        acresConsumed=acresConsumed,acresConsumedUR=acresConsumedUR, 
                        acresRemaining=acresRemaining, acresRemainingUR=acresRemainingUR,
                        newSupplyRzZc=newSupplyRzZc,newSupplyURRzZc=newSupplyURRzZc,
                        newSupplyRzBin=newSupplyRzBin,newSupplyURRzBin=newSupplyURRzBin)
    return(resultsList)


}



