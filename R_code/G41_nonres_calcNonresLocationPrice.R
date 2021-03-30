# G35_nonres_calcNonresLocationPrice.R
# MetroScope non-residential location price calculation
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

calcNonresLocationPrice <- function(iLoops,supply_sqft,demand_sqft) {



# calcNonresLocationPrice()
# Uses the calculated sqft supply and demand to adjust the non-residential location price.
# 
# Input Parameters:
# iLoops = iteration of nonres module
# supply_sqft = total nonres sqft supply
# demand_sqft = total nonres sqft demand
#
# Calls:
# NONE
#
# Returns:
# totalSumSq = diagnostic, sum of (demand - supply)^2
# nonres_locationprice_new = new nonres location price
# nonres_locationprice = old nonres location price


    # load location price, shared between supply and demand modules
    # at first iteration of model year, use location price from previous year, otherwise use the updated value
    if (iLoops == 1 & iLanduseLoops == 1){
#        if (currentYear == 1){
#            nonres_locationprice_in <- readTable(scenarioID,"inputs_nonres",paste("nonres_locationprice_year",0,sep=""))
#        } else {
#        }

        nonres_locationprice_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_locationprice_year",(currentYear-1),sep=""))

    } else {
        nonres_locationprice_in <- readTable(scenarioID,"shared_nonres",paste("nonres_locationprice_year",currentYear,sep=""))
    }

    nonres_locationprice <- as.matrix(nonres_locationprice_in[,2:(numRE+1)])     #[ezone x re]

	#if supply is within 1% of demand, do nothing, else adjust all location prices
	#demand > supply, increase price
	#demand < supply, decrease price
					
	demandEz <- demand_sqft
	supplyEz <- supply_sqft 
	d_over_s <- demandEz / supplyEz
	s_over_d <- supplyEz / demandEz

	delta <- d_over_s
	delta[delta > 1.01] <- 1.01
	delta[delta < 0.9] <- 0.9
#	delta[s_over_d < 1.05 & s_over_d > 0.985] <- 1
    
	#Calculate Sum of Squares, catLine outliers
	sumSq <- (demandEz - supplyEz)^2 / 1000000000
	totalSumSq <- round(sum((sumSq)))

	# calculate final price -- delta = multiplier ( in res, delta = addition)	
	nonres_locationprice_new <- as.matrix(delta * nonres_locationprice)

	#set lower bound of 0.0001  jbc 2011-09-01
	nonres_locationprice_new[nonres_locationprice_new < 0.0001] <- 0.0001
	
	nonres_locationprice_new[,numRE] <- 1
	
	#if (iLoops > 50){browser() }
				    			    
    #return
	list(nonres_locationprice_new=nonres_locationprice_new, sumSq=sumSq, totalSumSq=totalSumSq)

}
