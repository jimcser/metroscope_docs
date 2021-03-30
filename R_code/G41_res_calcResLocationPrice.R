# G35_res_calcResLocationPrice.R
# MetroScope residential location price calculation
# maintained by Jim Cser jim.cser@oregonmetro.gov
# last updated 3/8/2013

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
	
calcResLocationPrice <- function(iLoops, supplyRz, demandRz){
# calcResLocationPrice()
# Uses the calculated DU supply and demand to adjust the non-residential location price.
# 
# Input Parameters:
# iLoops = iteration of res module
# supply_sqft = total DU supply
# demand_sqft = total DU demand
#
# Calls:
# NONE
#
# Returns:
# totalSumSq = diagnostic, sum of (demand - supply)^2
# res_locationprice_new = new res location price
# res_locationprice = old res location price


# 02  now returns SumSq array!   2014-12-08 5:33:17 PM

    # load location price, shared between supply and demand modules
    # at first iteration of model year, use location price from previous year, otherwise use the updated value
    if (iLoops == 1 & iLanduseLoops == 1){
            res_locationprice_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("res_locationprice_year",(currentYear-1),sep=""))

    } else {
        res_locationprice_in <- readTable(scenarioID,"shared_res",paste("res_locationprice_year",currentYear,sep=""))
    }
    
    res_locationprice <- as.matrix(res_locationprice_in[,2:(numHtypes+1)])     #[ezone x re]

    res_general_params_in <- readTable(scenarioID,"inputs_res","res_general_params")    
    rownames(res_general_params_in) <- noquote(res_general_params_in$parameter)
    res_general_params <- res_general_params_in[,"value",F]

	alphaParamLP <- res_general_params["alphaParamLP",]
	betaParamLP <- res_general_params["betaParamLP",]

	#Calculate Sum of Squares, catLine outliers
	sumSq <- (demandRz - supplyRz)^2
	totalSumSq <- round(sum((demandRz - supplyRz)^2))
		
	rzVal <- array(1:numRzones, dim = c(numRzones,numHtypes))
	htVal <- array(1:numHtypes, dim = c(numHtypes,numRzones))
	htVal <- t(htVal)


	#if supply is within 1% of demand, do nothing, else adjust all location prices
	#demand > supply, increase price
	#demand < supply, decrease price

	totalDemand <- sum(demandRz)
	totalSupply <- sum(supplyRz)

	difference <- totalSupply - totalDemand
	interval <- 0.01 * totalDemand


	if(difference > interval) { 
		bump <- 1 - (0.01 * difference / interval)
	} else if(difference < -interval) {
		bump <- 1 - (0.01 * difference / interval)		
	} else {
		bump <- 1
	}
    
 
	# calculate change in location price for matrix
	delta <- alphaParamLP * betaParamLP * (demandRz - supplyRz)   
	# err on the side of more supply 
	#surplus <- supplyRz - demandRz
	#surplus2 <- supplyRz - (demandRz * 1.05) 
	#delta[ (surplus > 0) & (surplus2 < 0) ] <- 0 
    
	# limit large changes
	delta[delta > 0.05] <- 0.05
	delta[delta < -0.05] <- -0.05
	
		
	res_locationprice_new <- bump * (res_locationprice + delta)
	#set lower bound of 0.1
	res_locationprice_new[res_locationprice_new < 0.1] <- 0.1
	
    list(res_locationprice_new=res_locationprice_new, sumSq=sumSq, totalSumSq=totalSumSq)
    	
}

