# G35_res_supply_functions.R
# MetroScope functions for residential demand module
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


calcResFeasibleSupply <- function(res_general_params,res_location_price, res_calibration_price, res_base_salesfraction, res_landsupply_reg, res_landsupply_ur, res_zclass_lotsize) {
# calcFeasibleSupply()
# Calculates the supply availble to the residential real estate market.
#   
# The raw buildable acres available are the acres remaining from the previous model year plus
# then new acres added in the current model year.  This amount is "throttled" by both an assumed
# base fraction (not all units will be put on the market every year) and a price factor (as prices
# go up, more units go on the market).  An effective lot size for each zone class and housing type,
# a function of location price, then converts the buildable acres into potential new DU supply.
#
# Parameters:
# res_general_params = general parameters for residential supply module
# res_location_price = residential location price for current model year
# res_calibration_price = residential location price for calibration year
# res_base_salesfraction = fraction of residential acreage introduced to the model, i.e. the "throttle"
# res_landsupply_reg = regular land supply input to the model
# res_landsupply_ur  = regular land supply input to the model
# res_zclass_lotsize = minium and maximum lot sizes for each zoning class
#
# Calls:
# NONE
#
# Returns: 
# acresAvail = regular acres available to the market
# acresAvailUR = UR acres available to the market
# supplyFeasible = potential new regular DU to be built, subject to the market
# supplyFeasibleUR = potential new regular DU to be built, subject to the market
# lotPriceChange = parameter to adjust building cost as location price changes


# TODO  replace parameters

#	 sfland <- res_general_params["sfland",]
#	 sfbldg <- res_general_params["sfbldg",]
#	 cost_land <- res_general_params["cost_land",]
#	 cost_bldg <- res_general_params["cost_bldg",]
#	 capland_sub <- res_general_params["capland_sub",]
#	 landPriceChangeA <- res_general_params["landPriceChangeA",]
#	 landPriceChangeB <- res_general_params["landPriceChangeB",]
#	 landPriceChangeC <- res_general_params["landPriceChangeC",]
#  
#	#Calculate a couple of parameters	 
#	sfb_sfl <- sfbldg / sfland
#	kfactor <- sfb_sfl / (cost_bldg / cost_land) ^ capland_sub
#
#	#Calculate Feasible Supply
#	supplyHrx <- as.matrix(res_location_price/res_calibration_price) 
#	
#	landPriceChange <- exp(landPriceChangeA + landPriceChangeB * log(supplyHrx)) + landPriceChangeC
#	newRatio <- kfactor * (cost_bldg / (cost_land * landPriceChange)) ^ capland_sub
#	newsfland <- sfbldg / newRatio
#	pct_change <- (newsfland / sfland) - 1
#	landquant_chg <- 1 + pct_change
#	lotprice_chg <- (landPriceChange * landquant_chg) - 1
     
     #save(list = ls(), envir = environment(), file = "lotpricechange.RData") 
     #browser()
     
# sfbldg	2000
# sfland	6500
# cost_bldg	100
# cost_land	14.5
# capland_sub	-0.6
# landPriceChangeA	0.0017882
# landPriceChangeB	1.599215
# landPriceChangeC	-0.00179

   supply_price <- as.matrix(res_location_price/res_calibration_price) 

   landprice_chg <- exp(1.599 * log(supply_price))
   cap_to_land_ratio <- 0.98018 * ((100/(14.5 * landprice_chg))^-0.6)
   new_land_quantity <- 2000 / cap_to_land_ratio
   landquant_chg <-  new_land_quantity / 6500
   lotprice_chg <- landprice_chg * landquant_chg - 1
    
    #salesFraction <- res_base_salesfraction * landPriceChange ^ 1.5
    salesFraction <- res_base_salesfraction * landprice_chg ^ 1.5
	salesFraction[salesFraction > 1] <- 1

		
	# hack output
	throttle_out <- cbind(1:numRzones,salesFraction)
    colnames(throttle_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID, throttle_out, paste("outputs_year",currentYear,sep=""), paste("res_supply_throttle_year",currentYear,sep=""))
	

	#Process a few inputs
	acresInArray <- array(res_landsupply_reg,c(numZclass, numRzones, numHtypes))
	acresInURArray <- array(res_landsupply_ur,c(numZclass, numRzones, numHtypes))

	#For each zone class, calculate acres available
	avgLotSizeZclass <- res_zclass_lotsize$nomsize %o% landquant_chg
	
	minSize <- array(res_zclass_lotsize$minsize, dim(avgLotSizeZclass))
	avgLotSizeZclass[avgLotSizeZclass < minSize] <- minSize[avgLotSizeZclass < minSize]
	
	maxSize <- array(res_zclass_lotsize$maxsize, dim(avgLotSizeZclass))
	avgLotSizeZclass[avgLotSizeZclass > maxSize] <- maxSize[avgLotSizeZclass > maxSize]
	    
   	acresAvail <- aperm(acresInArray, c(2,3,1)) * array(salesFraction, c(dim(salesFraction), numZclass))
	acresAvailUR <- aperm(acresInURArray, c(2,3,1)) * array(salesFraction, c(dim(salesFraction), numZclass))
   

   	supplyFeasible <- acresAvail * 43560 / aperm(avgLotSizeZclass, c(2,3,1))
	supplyFeasibleUR <-  acresAvailUR * 43560 / aperm(avgLotSizeZclass, c(2,3,1))
	
	return (list(acresAvail=acresAvail, acresAvailUR=acresAvailUR, supplyFeasible=supplyFeasible, supplyFeasibleUR=supplyFeasibleUR, lotPriceChange=lotprice_chg) )
}


calcResAcresConsumed <- function(res_general_params, lotPrice, lotprice_chg, avgLotSizeBin, avgHouseSizeBin, avgPriceRentBin, res_base_bldgcost, rzoneFeeSubsidy, totalCostFraction, supplyFeasible,  acresAvail,iLoops) {
# calcAcresConsumed()
# Calculates the acres consumed by residential real estate market
#   
# For each housing price bin, compare the cost to build a DU to the amount a household is willing to pay.
# If construction cost is less than bid price, then build, otherwise don't build. 
# Finally, calculate the total acres consumed by prorating by the ratio of built and unbuilt DU supply.
# The function is called once for regular acres, and once for UR acres.
#
# Parameters:
# res_general_params = general residential parameters
# lotPrice = base residential lot price
# lotprice_chg = parameter to adjust building cost as location price changes
# avgLotSizeBin = average lot size, by housing bin
# avgHouseSizeBin = average house size, by housing bin
# avgPriceRentBin = average hedonic, by housing bin
# res_base_bldgcost = base residential building cost
# rzoneFeeSubsidy = fee added or subsidy subtracted from building cost
# totalCostFraction = factor to adjust total building cost. UNUSED  
# supplyFeasible = potential new regular DU to be built
# acresAvail = acres available to the market
#
# Calls:
# NONE
#
# Returns: 
#' @return newSupply = new supply built in current year, by rzone
#' @return acresConsumed = acres consumed in current model year
#' @return newSupplyRzZc = new supply built in current year, by rzone and zone class
#' @return newSupplyRzBin = new supply built in current model year, by rzone and housing bin
#' @return newSupplyRzBinZc = new supply built in current model year, by rzone, housing bin, and zone class
#' @return totalCost = total cost of production

	housesize_cost_exponent <- res_general_params["housesize_cost_exponent",]
	supply_utility_exponent <- res_general_params["supply_utility_exponent",]
	#renter_baselotprice_factor <- res_general_params["renter_baselotprice_factor",]
	

	#Calculate bin weights (utility)
	lotPriceArray <- array(lotPrice, c(numZclass, numRzones, numHtypes))
	
	### HACK ###
	# lower the renter cost to 0.8 * default
	#lotPriceArray <- lotPriceArray_in
	#lotPriceArray[,,c(3,4)] <- lotPriceArray_in[,,c(3,4)] * renter_baselotprice_factor
	
	
	lotprice_chgArray <- array(lotprice_chg, c(dim(lotprice_chg), numZclass))
	

	
	landCost <- aperm(lotPriceArray, c(2,3,1)) * (lotprice_chgArray + 1)  
	landCost <- array(landCost, c(dim(landCost), numBins))    #425 4 34 8
    
    avgHouseSizeBin <- array(avgHouseSizeBin,c(numBins,numRzones,numHtypes))  # 8 425 4  
    avgLotSizeBin <- array(avgLotSizeBin,c(numBins,numRzones,numHtypes))  # 8 425 4 
    avgPriceRentBin <- array(avgPriceRentBin,c(numBins,numRzones,numHtypes))  # 8 425 4 
 
	avgLotSizeBinZclass1Rep <- aperm(array(avgLotSizeBin[1,,], c(dim(avgLotSizeBin)[c(2,3,1)])), c(3,1,2))


	buildingCost1 <- avgHouseSizeBin * (avgLotSizeBinZclass1Rep / avgLotSizeBin) ^ housesize_cost_exponent
	buildingCost <- res_base_bldgcost %o% buildingCost1
	

    # fee, subsidy now rzone x htype  JBC 2014-02-17 2:27:14 PM
  	# totalCostFraction went away
    #totalCost <- sweep(aperm(landCost, c(3,4,1,2)) + buildingCost, 3, rzoneFeeSubsidy, "+")
    totalCost_in <- sweep(aperm(landCost, c(3,4,1,2)) + buildingCost, c(3,4), rzoneFeeSubsidy, "+")
    totalCost <- totalCost_in
    totalCost[,,,c(3,4)] <- totalCost_in[,,,c(3,4)] # * renter_baselotprice_factor
    
    totalCost[totalCost < 1] <- 1   # 34 8 425 4
	 
	avgPriceRentBinZclass <- aperm(array(avgPriceRentBin, c(dim(avgPriceRentBin), numZclass)), c(4,1,2,3))

	#If total cost > price/rent then use all feasible supply, else 0
	supplyFeasibleBin <- aperm(array(supplyFeasible, c(dim(supplyFeasible), numBins)), c(3,4,1,2))
	supplyFeasibleBin[totalCost > avgPriceRentBinZclass] <- 0
	supplyUtilityBin <- supplyFeasibleBin * (avgPriceRentBinZclass / totalCost) ^ supply_utility_exponent
	supplyUtility <- apply(supplyUtilityBin, c(1,3,4), function(x) sum(x, na.rm=T)) #collapse across bin
	supplyUtility <- aperm(array(supplyUtility, c(dim(supplyUtility), numBins)), c(1,4,2,3))

	#Allocate supply to bins and collapse to rzone by htype
	supplyAllocationBin <- supplyFeasibleBin * supplyUtilityBin / supplyUtility
	newSupply <- apply(supplyAllocationBin, c(3,4), function(x) sum(x, na.rm=T))

    # convert to vector JBC 2015-05-22 4:08:37 PM
	newSupplyRzBin = apply(supplyAllocationBin, c(2,3,4), function(x) sum(x, na.rm=T))
	dim(newSupplyRzBin) <- c(8*494,4) 

	#Prorate acres (calculate acres consumed)
	supplyAllocationZclass <- apply(supplyAllocationBin, c(1,3,4), function(x) sum(x, na.rm=T))
	supplyAllocationZclass <- aperm(supplyAllocationZclass, c(2,3,1))
	acresConsumed <- acresAvail * supplyAllocationZclass / supplyFeasible
	acresConsumed[is.na(acresConsumed)] <- 0

	acresConsumed <- aperm(acresConsumed,c(3,1,2))
	dim(acresConsumed) <- c(numRzones * numZclass, numHtypes)

    #calc newSupply by Rzone, zclass, for mapback use  added 7/27/07 jbc
    newSupplyRzZc <- apply(supplyAllocationBin, c(1,3,4), function(x) sum(x, na.rm=T))
    dim(newSupplyRzZc) <- c(numRzones * numZclass, 4)

totalCostFraction <- 0

     #save(list = ls(), envir = environment(), file = "calcResAcresConsumed.RData") 
	#browser()

    if (iLoops == numResLoops) {  
        

       #save(totalCost,envir=environment(),file=paste("totalCost_year",currentYear,".RData",sep=""))
       #save(avgPriceRentBinZclass,envir=environment(),file=paste("avgPriceRentBinZclass_year",currentYear,".RData",sep=""))
       #browser()
       
    }
    

	return (list(newSupply=newSupply, acresConsumed=acresConsumed, newSupplyRzZc = newSupplyRzZc, newSupplyRzBin = newSupplyRzBin, newSupplyRzBinZc = supplyAllocationBin, totalCost = totalCost) )

}
