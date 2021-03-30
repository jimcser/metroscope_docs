# G35_res_demand_module.R
# maintained by Jim Cser jim.cser@oregonmetro.gov
# last updated 12/23/2013

# notes
# KHIA sort now by owner and renter


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



runResDemand <- function(iLoops){
# runResDemand()
# The residential demand module for MetroScope.
#   
# This function distributes the regional control total of dwelling unit 
# demand over the residential zones, housing types, tenure, and KHIA 
# categories.  The demand is then compared to the results of the supply
# module, and difference is used to adjust the residential location price.
# 
#  
# General sequence of the residential demand calculations is:
# 1. Zone-to-zone utility based on travel time
# 2. Distribution over each KHIA market segment
# 3. Tenure choice (owners vs. renters)
# 4. Housing type choice (single- vs. multi-family)
# 5. Location choice
# 6. Allocation by zone and housing value bin 
#
# Input Parameters:
# iLoops = current iteration of res demand module
# 
# Calls:
# calcHouseLotSize()			
# calcHouseLotSize()
# calcHedonic()
# calcResTravelUtility()
# calcTenureChoice()
# calcTypeChoice()
# calcLocationChoice()
# calcBinAvg()
# calcBinSum()
#
# Returns:
# res_demand_Rz -- DU demand, by rzone. requried to update res location price
# res_demand_KHIARz -- DU demand, by rzone and KHIA
# res_demand_EzRz --  DU demand, by rzone and ezone
# res_demand_Ez -- DU demand, by ezone. Required by nonres model
# res_demand_EzChild -- Child DU demand, by ezone. Required by nonres model
# avgHouseSizeBin -- Average house size, by rzone and bin. Requried by res supply module
# avgLotSizeBin -- Average lot size, by rzone and bin. Requried by res supply module
# avgHedonicBin -- Average hedonic price, by rzone and bin. Requried by res supply module
# res_demand_RzBin -- DU demand, by rzone and housing bin.  Required by nonres model
# res_demand_binshares -- share of DU demand in each rzone, for each housing bin and type

    
    # data shared between res and nonres models   
    res_emp_in <- readTable(scenarioID,paste("outputs_year",currentYear,sep=""),paste("res_demand_emp_year",currentYear,sep=""))        
    #res_emp_in <- readTable(scenarioID,"shared_nonres",paste("res_demand_emp_year",currentYear,sep=""))     
 
    ##### parameters
	# named paramters, single value 
	 
    res_general_params_in <- readTable(scenarioID,"inputs_res","res_general_params")    
    rownames(res_general_params_in) <- noquote(res_general_params_in$parameter)
    res_general_params <- res_general_params_in[,"value",F]
       
	res_tenure_params_in <- readTable(scenarioID,"inputs_res","res_demand_tenure_params")  
    rownames(res_tenure_params_in) <- noquote(res_tenure_params_in$parameter)
    res_tenure_params <- res_tenure_params_in[,"value",F]


	# named paramters, by housing types 

	res_housesize_params_in <- readTable(scenarioID,"inputs_res","res_demand_housesize_params") 
    rownames(res_housesize_params_in) <- noquote(res_housesize_params_in$parameter)
    res_housesize_params <- res_housesize_params_in[,2:5]
	 
	res_lotsize_params_in <- readTable(scenarioID,"inputs_res","res_demand_lotsize_params") 
    rownames(res_lotsize_params_in) <- noquote(res_lotsize_params_in$parameter)
    res_lotsize_params <- res_lotsize_params_in[,2:5]
	    
	res_hedonic_params_in <- readTable(scenarioID,"inputs_res","res_demand_hedonic_params") 
    rownames(res_hedonic_params_in) <- noquote(res_hedonic_params_in$parameter)
    res_hedonic_params <- res_hedonic_params_in[,2:5]
    
    res_housingtype_params_in <- readTable(scenarioID,"inputs_res","res_demand_housingtype_params") 
    rownames(res_housingtype_params_in) <- noquote(res_housingtype_params_in$parameter)
    res_housingtype_params <- res_housingtype_params_in[,2:5]

  	res_locationchoice_params_in <- readTable(scenarioID,"inputs_res","res_demand_locationchoice_params") 
    rownames(res_locationchoice_params_in) <- noquote(res_locationchoice_params_in$parameter)
    res_locationchoice_params <- res_locationchoice_params_in[,2:5]
  
        
    # load location price, shared between supply and demand modules
    # at first iteration of model year, use location price from previous year, otherwise use the updated value
    # new directory for year 0]

    if (iLoops == 1 & iLanduseLoops == 1){
        res_locationprice_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("res_locationprice_year",(currentYear-1),sep=""))
    } else {
        res_locationprice_in <- readTable(scenarioID,"shared_res",paste("res_locationprice_year",currentYear,sep=""))
    }

    res_location_price <- as.matrix(res_locationprice_in[,2:5]) 
 

    res_calibration_price_in <- readTable(scenarioID,"outputs_year0","res_locationprice_year0") 
    res_calibration_price <- as.matrix(res_calibration_price_in[,2:5]) 
 
    res_kshare_params_in <- readTable(scenarioID,"inputs_res","res_demand_kshare_params") 
    res_kshare_params <- res_kshare_params_in[,2:5]
    
    ## get vintage supply from year N-1.  At end of iterations, write res_supply for current year to /inputs_res
    res_supply_vintage_yearN_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("res_totalsupply_year",(currentYear-1),sep=""))              
    res_supply_vintage_yearN <- res_supply_vintage_yearN_in[,2:5]
     

    
    # by rzone   
	res_accessindex_nscore <- readTable(scenarioID,"inputs_res","res_demand_accessindex_nscore") 
    res_accessindex <- res_accessindex_nscore[,"accessindex"]
    res_nscore <- res_accessindex_nscore[,"nscore"]
     
    # read year N-1 restored for tandem mode JBC  2014-02-17 1:15:47 PM 
    # ezone x rzone    ezone_from, rzone_to
    
    if (isDefaultTT == T){   
        res_traveltime_in <- readTable(scenarioID,"shared_transport",paste("res_traveltime_year",currentYear,sep=""))
    } 
  
    if (isDefaultTT == F){     
        if (iMainLoops == 1){
            res_traveltime_in <- readTable(scenarioID,"shared_transport",paste("res_traveltime_year",(currentYear-1),sep=""))
        } else {
            res_traveltime_in <- readTable(scenarioID,"shared_transport",paste("res_traveltime_year",currentYear,sep=""))
        }
     }

    res_traveltime_yearN <- res_traveltime_in[,3]
    
    res_emp_yearN <- res_emp_in[,"emp"]
    
    # res hh control
    res_hh_controls_in <-  readTable(scenarioID,"inputs_res","res_demand_hh_controls")
    res_hh_control <- res_hh_controls_in[currentYear,"totalhh"]
    res_du_control <- res_hh_controls_in[currentYear,"totaldu"]
      
    ## get vintage binshares from year N-1.  
    #by Rzone, Bin, Htypes
    res_binshares_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("res_demand_binshares_year",(currentYear-1),sep=""))
    res_binshares <- res_binshares_in[,3:6]
    
    #by KHIA
    res_khia_shares_in <-  readTable(scenarioID,"inputs_res",paste("res_demand_khia_shares_year",currentYear,sep="")) 
    res_khia_shares <- res_khia_shares_in[,"share"]
    
    
    ### define k, h, i, a market segments
    #categores of each K,H,I,A
  	res_khia_categories_in <- readTable(scenarioID,"inputs_res","res_demand_khia_categories")
       
    khiavars <- res_khia_categories_in$khiavar
    cats_k <- res_khia_categories_in[khiavars=="k",]$nomval
    cats_h <- res_khia_categories_in[khiavars=="h",]$nomval
    cats_i <- res_khia_categories_in[khiavars=="i",]$nomval
    cats_a <- res_khia_categories_in[khiavars=="a",]$nomval
      
      
    khia_in <- readTable(scenarioID,"inputs_res","res_demand_khia_params")
    khia <- as.matrix(khia_in[,c("k","h","i","a")])
    
    #now different for owners and renters
    khiaSort_own <- khia_in[,"sort_own"]
    khiaSort_rent <- khia_in[,"sort_rent"]
    

        
    khia_marketsegments <- array(0,dim(khia))
    colnames(khia_marketsegments) <- colnames(khia)
        
    for (idx in 1:numKHIA){
        khia_marketsegments[idx,"k"] <- cats_k[khia[idx,"k"]+1]   #k has values 0 and 1
        khia_marketsegments[idx,"h"] <- cats_h[khia[idx,"h"]]
        khia_marketsegments[idx,"i"] <- cats_i[khia[idx,"i"]]
        khia_marketsegments[idx,"a"] <- cats_a[khia[idx,"a"]]
    }

    

    #Calculate Travel Utility
    travelUtil <- calcResTravelUtility(res_location_price, res_traveltime_yearN, res_supply_vintage_yearN)
    travelWeight <- travelUtil$travelWeight
    travelWeight_denom <- travelUtil$travelWeight_denom

    
    # estimate shares of regional households by Ezone and KHIA
    # ezone shares and KHIA shares each sum to 1
    
    
    ezone_shares <- res_emp_yearN / sum(res_emp_yearN)
    du_EzKHIA <- res_du_control * (ezone_shares %o% res_khia_shares)
    
    #Calculate Tenure Choice (Own, Rent)
    tenureChoice <- calcTenureChoice(du_EzKHIA, travelWeight_denom, travelWeight, as.matrix(khia_marketsegments), as.matrix(res_tenure_params))

    #Calculate Type Choice (Own - SF, MF, Rent - SF, MF)
    typeChoice <- calcTypeChoice(tenureChoice$ownUnits, tenureChoice$rentUnits, travelWeight_denom, travelWeight, as.matrix(khia_marketsegments), as.matrix(res_housingtype_params))
    demandEzKHIA <- array(c(typeChoice$osfUnits, typeChoice$omfUnits, typeChoice$rsfUnits, typeChoice$rmfUnits), c(dim(typeChoice$osfUnits), 4))
    
    #Calculate Bin Assigned to each KHIA
    # write.csv(tenureChoice$ownUnits,"ownUnits.csv",row.names=F)
    # write.csv(tenureChoice$rentUnits,"rentUnits.csv",row.names=F)
    ownKhiaBinByEz <-   t(apply(tenureChoice$ownUnits, 1, function(x) cut(cumsum(x[order(khiaSort_own)])/sum(x),binBreaks)[khiaSort_own]))
    rentKhiaBinByEz <- t(apply(tenureChoice$rentUnits, 1, function(x) cut(cumsum(x[order(khiaSort_rent)])/sum(x),binBreaks)[khiaSort_rent]))
    
    # change in latest R  JBC   2015-05-22 6:06:17 PM
    ownTemp <- ownKhiaBinByEz
    rentTemp <- rentKhiaBinByEz
    
    for (ez in seq(1:numEzones)){
        ownKhiaBinByEz[ez,] <- factor(ownTemp[ez,], labels=seq(1:numBins))
        rentKhiaBinByEz[ez,] <- factor(rentTemp[ez,], labels=seq(1:numBins))
    }


  
    #Calculate bin by KHIA only (not by ezone as well), calc khia -> bin xwalk
    ownUnitsByHIA <- colSums(tenureChoice$ownUnits)
    rentUnitsByHIA <- colSums(tenureChoice$rentUnits)
    ownKhiaBin <- as.numeric(cut(cumsum(ownUnitsByHIA[order(khiaSort_own)])/sum(ownUnitsByHIA),binBreaks)[khiaSort_own])
    rentKhiaBin <- as.numeric(cut(cumsum(rentUnitsByHIA[order(khiaSort_rent)])/sum(rentUnitsByHIA),binBreaks)[khiaSort_rent])
 

   
    # calc house size, lot size, hedonic
    # house and lot size are too high- hack by using multiplier dervied from Gamma forecast
     
    #bin 	hsize_osf	hsize_omf	hsize_rsf	hsize_rmf	lsize_osf	lsize_omf	lsize_rsf	lsize_rmf
    res_sizefactor_in <- readTable(scenarioID,"inputs_res","res_demand_sizefactor")  
    housesize_factor <- as.matrix(res_sizefactor_in[,2:5])
    lotsize_factor <- as.matrix(res_sizefactor_in[,6:9])
    
    housesize_factor_khia <- array(0, c(numKHIA, numHtypes))
    lotsize_factor_khia <- array(0, c(numKHIA, numHtypes))
    
    khia_bin_lut <- cbind(ownKhiaBin, ownKhiaBin, rentKhiaBin, rentKhiaBin)
    for (bin in 1:numBins){
        housesize_factor_khia[ownKhiaBin == bin,1]  <- housesize_factor[bin,1]
        housesize_factor_khia[ownKhiaBin == bin,2]  <- housesize_factor[bin,2]
        housesize_factor_khia[rentKhiaBin == bin,3]  <- housesize_factor[bin,3]
        housesize_factor_khia[rentKhiaBin == bin,4]  <- housesize_factor[bin,4]

        lotsize_factor_khia[ownKhiaBin == bin,1]  <- lotsize_factor[bin,1]
        lotsize_factor_khia[ownKhiaBin == bin,2]  <- lotsize_factor[bin,2]
        lotsize_factor_khia[rentKhiaBin == bin,3]  <- lotsize_factor[bin,3]
        lotsize_factor_khia[rentKhiaBin == bin,4]  <- lotsize_factor[bin,4]
    
    }
   

    # dim = 5, 8, 5, 2, 4, 425    
    houseSize <- calcHouseLotSize2(as.matrix(khia_marketsegments), as.matrix(res_housesize_params), as.matrix(housesize_factor_khia))			
    lotSize <- calcHouseLotSize2(as.matrix(khia_marketsegments), as.matrix(res_lotsize_params), as.matrix(lotsize_factor_khia))	
           
    hedonic <- calcHedonic(res_hedonic_params, as.matrix(res_accessindex), as.matrix(res_nscore), houseSize, lotSize, as.matrix(res_location_price),as.matrix(res_calibration_price))     
	# dim = 425 4 400
	hedonicHIA <- apply(hedonic, c(5,6), as.vector)
	hedonicHIA  <- aperm(hedonicHIA , c(3,2,1))


    # determine average bin to send to supply module
    avgHouseSizeBin  <- calcBinAvg(houseSize, ownKhiaBin, rentKhiaBin)
    avgLotSizeBin <- calcBinAvg(lotSize, ownKhiaBin, rentKhiaBin)
    avgHedonicBin <- calcBinAvg(hedonic, ownKhiaBin, rentKhiaBin)
    
   
    locationChoiceList <- calcLocationChoice(res_location_price, res_traveltime_yearN, ownKhiaBinByEz, rentKhiaBinByEz, res_binshares, typeChoice, as.matrix(res_kshare_params), res_accessindex, res_nscore, as.matrix(khia_marketsegments), as.matrix(res_locationchoice_params))
    
    res_demand_KHIARz <- locationChoiceList$res_demand_KHIARz
    res_demand_EzRz <- locationChoiceList$res_demand_EzRz
    res_demand_Rz <- locationChoiceList$res_demand_Rz
    res_demand_Ez <- locationChoiceList$res_demand_Ez
    res_demand_EzChild <- locationChoiceList$res_demand_EzChild
    
   
	#/
	#Calculate New Bin Shares
	#/

    #Collapse demand by rzone by khia by htype to rzone by bin by htype    
   
    res_demand_KHIARzAperm <- aperm(res_demand_KHIARz, c(3,1,2))   # 425 4 400
    demandBinTotal <- calcBinSum(res_demand_KHIARzAperm, ownKhiaBin, rentKhiaBin)  # 8 425 4    
   
    #Replace 0 with 0.001 and sum to bin by htype
    #demandBinshare[demandBinshare = 0] <- 0.001   causes underflow
    demandBinTotal[demandBinTotal < 0.0001] <- 0.0001       # OK 8 425 4    
    binSumByHtype <- apply(demandBinTotal, c(1,3), sum)     # OK 8  4
   
    #Calculate total demand by bin, and bin shares (rzone_bin_yearX)
    newbinshares <- aperm(demandBinTotal, c(1,3,2)) / array(binSumByHtype, c(dim(binSumByHtype), numRzones))
    newbinshares <- newbinshares * numRzones    # 8 4 425
    
    newbinshares_out <- cbind(as.vector(newbinshares[,1,]),as.vector(newbinshares[,2,]),as.vector(newbinshares[,3,]),as.vector(newbinshares[,4,]))
    
    newbins <- aperm(demandBinTotal, c(1,3,2))   #8 425 4
	 
    results <- list(res_demand_Rz=res_demand_Rz, res_demand_KHIARz=res_demand_KHIARz, res_demand_EzRz=res_demand_EzRz, res_demand_Ez=res_demand_Ez, res_demand_EzChild=res_demand_EzChild, avgHouseSizeBin = avgHouseSizeBin, avgLotSizeBin = avgLotSizeBin, avgHedonicBin = avgHedonicBin, res_demand_RzBin=demandBinTotal, res_demand_binshares=newbinshares_out)
    return(results)

}

