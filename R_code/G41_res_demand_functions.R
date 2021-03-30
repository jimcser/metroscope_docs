# G35_res_demand_functions.R
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


calcHouseLotSize2 <- function(marketData, params, factor_khia) {
# calcHouseLotSize()
# Calculates house size or lot size for each KHIA market segment
#   
# Both the house size and lot size calculations have the same functional form but with different 
# parameters, so for efficiency a single function is used. The results are used for calculating
# the hedonic value matrix
#
# Parameters
# marketData = matrix of values for each KHIA market segment
# params = coefficients for house size, lot size equations 
#
# Calls:
# NONE
#
# Returns: 
# result = lot,house size by KHIA segment, zone, housing type


	#Process marketData
	#marketData <- as.matrix(marketData[,c("k","h","i","a")])

	#Market Segment Calculations
	calcMSV <- function(row, params) {
        ## returns value for osf, omf, rsf, rmf
        temp <- 
            params["constant",] + 
            params["hhSize",] * log(row["h"]) +
            params["age",] * log(row["a"]) + 
            params["ageSq",] * log(row["a"]) ^ 2 +
            params["income",] * log(row["i"] / 1000) + 
            params["child",] * row["k"] +
            params["incomeChild",] * log(row["i"] / 1000) * row["k"] 
	   
	    #return value
	    temp
	    #as.matrix(temp)	
	}
	
	
	#Call function and return array khia by h type AND ADD ZONE dimension
	marketData <- marketData[,c("k","h","i","a")]
	marketData <- as.matrix(t(apply(marketData, 1, function(x) calcMSV(x, params))))
	marketData <- array(marketData, c(numA, numI, numH, numK, numHtypes, numRzones))
	
	#Exp result
	marketData <- exp(marketData)
	
	# apply factor
	factor_khia <- array(factor_khia, c(numA, numI, numH, numK, numHtypes, numRzones))

    result <- marketData * factor_khia
   
	return(result)
}

	

calcHedonic <- function(params, accessindex, nscore, houseSize, lotSize, res_location_price, res_calibration_price) {
# calcHedonic()
# Calculates residential hedonic price matrix
#   
# The hedonic matrix is the price households are willing to pay for new housing,
# based on the average lot size, average house size, neighborhood score, and
# residential location price 
#
# params = parameters for hedonic equation
# accessindex = relative measure of the proximity of a zone to all other zones
# nscore = residential neighborhood score
# houseSize == average house size
# lotSize = average lot size
# res_location_price = current residential location price
# res_calibration_price = residential location price from calibration year
#
# Calls:
# NONE
#
# Returns: 
# result = hedonic matrix by KHIA market segment, rzone, housing type

	#Calculate multi-dimensional variables  #dim(lotsize) = 5   8   5   2   4 425 
	lotSize <- sweep(log(lotSize), 5, as.matrix(params["lotSize",]), "*")
	houseSize <- sweep(log(houseSize), 5, as.matrix(params["hhSize",]), "*")
	accessIndex <- outer(log(accessindex), as.matrix(params["access",]))
	neigh <- outer(nscore, as.matrix(params["neighQual",]))
	lp <- log(res_location_price/res_calibration_price)   


	#Calculate resulting array
	result <- lotSize
	result[] <- 0
	result2 <- lotSize
	result2[] <- 0

	result2 <- result2 + sweep(result, 5, as.matrix(params["constant",]), "+")   	
	result2 <- result2 + lotSize
	result2 <- result2 + houseSize
	result2 <- result2 + sweep(result, 5, as.matrix(params["attachedDU",]), "+")
	result2 <- result2 + sweep(result, c(6,5), accessIndex, "+")
	result2 <- result2 + sweep(result, c(6,5), neigh, "+")
	result2 <- result2 + sweep(result, c(6,5), lp, "+")   	
	result  <- result2
	rm(result2)

	#Return exp result
	exp(result)

}


calcResTravelUtility <- function(locationPrice, res_traveltime, vintageSupply) {
# calcResTravelUtility()
# Calculates utility based on residential zone-to-zone travel times
#   
# Parameters:
# locationPrice = current residential location price
# res_traveltime = ezone-to-rzone travel time from transport module
# vintageSupply = residential supply from previous model year
#
# Calls:
# NONE
#
# Returns: 
# travelWeight = travel time utility, used in type and tenure choice
# travelWeightDenom = travel time utility, used in type and tenure choice




    # use for TAZ-based times
    res_traveltime[res_traveltime < 2] <- 2

    travelTimeByEz <- array(as.matrix(res_traveltime), c(numRzones,numEzones,numHtypes))  # rz x ez x ht
    travelTimeByEz <- aperm(travelTimeByEz, c(2,1,3)) # ez x rz x ht
    
    vintageSupply <- array(as.matrix(vintageSupply), c(numRzones,numHtypes,numEzones))   # rz x ht x ez
    vintageSupply <- aperm(vintageSupply, c(3,1,2)) # ez x rz x ht
    
    locationPrice <- array(as.matrix(locationPrice), c(numRzones,numHtypes,numEzones))   # rz x ht x ez
    locationPrice <- aperm(locationPrice, c(3,1,2)) # ez x rz x ht
   
    travelWeight_denom <- vintageSupply / travelTimeByEz   # ez x rz x ht
    travelWeight_denom <- apply(travelWeight_denom, c(1,3), "sum") # ez x ht
    
    travelWeight_numer <- (vintageSupply * locationPrice) / travelTimeByEz   # ez x rz x ht
    travelWeight_numer <- apply(travelWeight_numer, c(1,3), "sum") # ez x ht
    
    travelWeight <- travelWeight_numer / travelWeight_denom
 
	#Return result
	list(travelWeight=travelWeight, travelWeight_denom=travelWeight_denom)
}


calcTenureChoice <- function(dwellingUnitsEzoneKHIA, travelWeight_denom, travelWeight, marketData, params) {
# calcTenureChoice()
# Determines which households choose to be owners or renters
#   
# Parameters:
# dwellingUnitsEzoneKHIA = DU demand by ezone and KHIA
# travelWeight_denom = travel time utility, used in type and tenure choice
# travelWeight = travel time utility, used in type and tenure choice
# marketData = matrix of values for each KHIA market segment
# params = coefficients for tenure choice equation 
#
# Calls:
# NONE
#
# Returns: 
# ownUnits = owner choice, by ezone and KHIA
# rentUnits = renter choice, by ezone and KHIA

	#Market Segment Calculations
	calcMSV <- function(row, params) {
        ## returns value for osf, omf, rsf, rmf
        temp <- 
            params["constant",] + 
            params["age",] * log(row["a"]) +
            params["ageSq",] * log(row["a"]) ^ 2 +
            params["income",] * log(row["i"]) + 
            params["incomeSq",] * log(row["i"]) ^ 2 + 
            params["hhSize",] * log(row["h"]) +
            params["child",] * row["k"]	    
	    #return value
	    temp	
	}
	  	
	#Call function and return array khia by h type AND ADD ZONE dimension
	marketData <- as.matrix(t(apply(marketData, 1, function(x) calcMSV(x, params))))
	
	
	# htypes are OSF, OMF, RSF, RMF
	colnames(travelWeight_denom) <- c("OSF", "OMF", "RSF", "RMF")
	colnames(travelWeight) <- c("OSF", "OMF", "RSF", "RMF")
	

	#Weight access indices   Collapse OSF and OMF to Own and RSF and RMF to Rent
	ownPrice <- (travelWeight_denom[,"OSF"] * travelWeight[,"OSF"] + 
		travelWeight_denom[,"OMF"] * travelWeight[,"OMF"]) / 
		(travelWeight_denom[,"OSF"] + travelWeight_denom[,"OMF"])

	rentPrice <- (travelWeight_denom[,"RSF"] * travelWeight[,"RSF"] + 
		travelWeight_denom[,"RMF"] * travelWeight[,"RMF"]) / 
		(travelWeight_denom[,"RSF"] + travelWeight_denom[,"RMF"])

	accessTenure <- (travelWeight_denom[,"OSF"] + travelWeight_denom[,"OMF"]) / 
		(travelWeight_denom[,"OSF"] + travelWeight_denom[,"OMF"] + 
		travelWeight_denom[,"RSF"] + travelWeight_denom[,"RMF"])

	#EZone Calculations
	ownPrice <- params["priceChoiceOwn",] * log(ownPrice)
	rentPrice <- params["priceSubRent",] * log(rentPrice)
	temp <- params["priceCompAuto",] * log(1) 		#debug
	temp2 <- params["priceSubCompTransit",] * log(1) 	#debug
	accessTenure <- params["accessTenure",] * log(accessTenure)


	#Add zones to market segments data
	marketData <- as.matrix(outer(marketData, (ownPrice + rentPrice + temp + temp2 + accessTenure), "+"))
	
	######## need to make work with Gen4 ########
	dim(marketData) <- c(numKHIA,numEzones)
	

	#Exp result to get utility and calculate probability
	#Transpose to change khia by ezone to ezone by khia
	ownUtility <- t(exp(as.matrix(marketData)))
	probOwn <- ownUtility / (1 + ownUtility)
	probRent <- 1 - probOwn

	#Calculate # of Own and Rent Dwelling Units
	ownUnits <- probOwn * dwellingUnitsEzoneKHIA
	rentUnits <- probRent * dwellingUnitsEzoneKHIA

	#Return list of results
	list(ownUnits=ownUnits, rentUnits=rentUnits)

}
 

calcTypeChoice <- function(ownUnits, rentUnits, travelWeightEzHt, travelWeightEzHtProd, marketData, params) {
# calcTypeChoice()
# Determines which households choose to live single- or multi-family units
#   
# Parameters:
# ownUnits = DU demand for owners
# rentUnits = DU demand for owners
# travelWeightEzHt = travel time utility, used in type and tenure choice
# travelWeightEzProd = travel time utility, used in type and tenure choice
# marketData = matrix of values for each KHIA market segment
# params = coefficients for type choice equation 
#
# Calls:
# NONE
#
# Returns: 
# ownUnits = owner choice, by ezone and KHIA
# rentUnits = renter choice, by ezone and KHIA

	#Market Segment Calculations
	calcMSV <- function(row, params) {

		params["constant",] + 
		params["age",] * log(row["a"]) +
		params["ageSq",] * log(row["a"]) ^ 2 +
		params["hhSize",] * log(row["h"]) + 
		params["income",] * log(row["i"]) + 
		params["childIncome",] * row["k"] * log(row["i"]) 

	}
	#Call function and return array khia by ZONE dimension
	marketData <- as.matrix(t(apply(marketData, 1, function(x) calcMSV(x, params))))
	
	#Add travelWeight by htype and ezone to market segments data
	travelWeight <- params["travelWeight",] * t(log(travelWeightEzHt)) 
	marketData <- array(marketData, c(length(marketData[,1]), ncol(params), nrow(travelWeightEzHt)))
	marketData <- sweep(marketData, c(2,3), travelWeight, "+")
    
	colnames(travelWeightEzHtProd) <- c("OSF", "OMF", "RSF", "RMF")

	#Calculate part of utility by tenure type by ezone
	ezoneData <- NULL
	ezoneData$osf <- params["sf","osf"] * log(travelWeightEzHtProd[,"OSF"]) + params["mf","osf"] * log(travelWeightEzHtProd[,"OMF"])
	ezoneData$omf <- params["sf","omf"] * log(travelWeightEzHtProd[,"OSF"]) + params["mf","omf"] * log(travelWeightEzHtProd[,"OMF"])
	ezoneData$rsf <- params["sf","rsf"] * log(travelWeightEzHtProd[,"RSF"]) + params["mf","rsf"] * log(travelWeightEzHtProd[,"RMF"])
	ezoneData$rmf <- params["sf","rmf"] * log(travelWeightEzHtProd[,"RSF"]) + params["mf","rmf"] * log(travelWeightEzHtProd[,"RMF"])
	ezoneData <- as.matrix(as.data.frame(ezoneData))
	marketData <- sweep(marketData, c(3,2), ezoneData, "+")  

  
	#Exp result to get utility and calculate probability
	#aperm to change to ez,khia,htype
	utility <- aperm(exp(marketData), c(3,1,2))
	probOSF <- utility[,,1] / (utility[,,1] + utility[,,2])
	probOMF <- utility[,,2] / (utility[,,1] + utility[,,2])
	probRSF <- utility[,,3] / (utility[,,3] + utility[,,4])
	probRMF <- utility[,,4] / (utility[,,3] + utility[,,4])

	#Calculate units by type
	osfUnits <- probOSF * ownUnits
	omfUnits <- probOMF * ownUnits
	rsfUnits <- probRSF * rentUnits
	rmfUnits <- probRMF * rentUnits


	return(  list(osfUnits=osfUnits, omfUnits=omfUnits, rsfUnits=rsfUnits, rmfUnits=rmfUnits) )

}


calcLocationChoice <- function(locationPrice, ezoneRzoneTravelTime, ownKhiaBinByEz, rentKhiaBinByEz, binshares, typeChoice, kshare, res_accessindex, res_nscore, marketData, params) {
# calcLocationChoice()
# Determines where households choose to live
#
# Note: this is by far the slowest module in MetroScope, and needs optimizing
#   
# Parameters:
# locationPrice = current residential location price
# ezoneRzoneTravelTime = ezone-to-rzone travel time from transport module
# ownKhiaBinByEz = owner demand, by ezone and value bin
# rentKhiaBinByEz = owner demand, by ezone and value bin
# binshares = TODO describe bin shares
# typeChoice = demand by housing type [see calcTypeChoice]
# kshare = weight for K demand TODO describe Kshare
# res_accessindex = relative measure of the proximity of a zone to all other zones
# res_nscore = residential neighborhood score
# marketData = matrix of values for each KHIA market segment
# params = coefficients for type choice equation 
#
# Calls:
# NONE
#
# Returns: 
# res_demand_Rz = demand by rzone and housing type, is compared with supply to adjust location price [see calcResLocationPrice]
# res_demand_KHIARz = demand by rzone, KHIA and housing type, output only
# res_demand_EzRz = demand by rzone, ezone and housing type, output only
# res_demand_Ez = demand by ezone, used by non-res module
# res_demand_Ezchild = demand by ezone, used by non-res module

	#Preprocess variables
	logAccess <- log(res_accessindex)
	nHood <- res_nscore
	logDemandHrx <- log(locationPrice) 
	kshare <- kshare

	child <- marketData[,"k"] #note not log()
	logHHSize <- log(marketData[,"h"])
	logIncome <- log(marketData[,"i"]) #not /1000
	logAge <- log(marketData[,"a"])


	#collapse typeChoice to array
	typeChoice <- array(c(typeChoice$osfUnits, typeChoice$omfUnits, typeChoice$rsfUnits, 
	typeChoice$rmfUnits), c(dim(typeChoice$osfUnits), 4))
    
	#get all variables into same dimensions
	
	#travel time to each rzone for each ezone   
	logTravelTimeByEz <- log(ezoneRzoneTravelTime)   #ez*rz
	logTravelTimeByEz <- array(logTravelTimeByEz, c(numRzones,numEzones,numHtypes,numKHIA)) #rz x ez x ht x khia
	logTravelTimeByEz <- aperm(logTravelTimeByEz, c(3,4,1,2)) #ht x khia x rz x ez

	
	   loc_const <- array(params["constant",],c(numHtypes,numKHIA,numRzones))  #ht.khia.rz
	   loc_logTT <- array(params["logTT",],c(numHtypes,numKHIA,numRzones))     #ht.khia.rz
	   loc_logTTSq <- array(params["logTTSq",],c(numHtypes,numKHIA,numRzones)) #ht.khia.rz

	   loc_logHrx <- params["logHrx",] * t(logDemandHrx)           #ht.rz
	   loc_logHrx <- array(loc_logHrx,c(numHtypes, numRzones, numKHIA))   #ht.rz.khia
	   loc_logHrx <- aperm(loc_logHrx,c(1,3,2))   #ht.khia.rz

	   loc_childTT <- params["childLogTT",] %o% child # ht.khia
	   loc_ageTT <- params["logAgeLogTT",] %o% logAge # ht.khia
	   loc_hhsizeTT <- params["logHHSizeLogTT",] %o% logHHSize # ht.khia
	   loc_incomeTT <- params["logIncomeLogTT",] %o% logIncome # ht.khia

	   loc_childTT <- array(loc_childTT, c(numHtypes,numKHIA,numRzones)) # ht.khia.rz
	   loc_ageTT <- array(loc_ageTT, c(numHtypes,numKHIA,numRzones)) # ht.khia.rz
	   loc_hhsizeTT <- array(loc_hhsizeTT, c(numHtypes,numKHIA,numRzones)) # ht.khia.rz
	   loc_incomeTT <- array(loc_incomeTT, c(numHtypes,numKHIA,numRzones)) # ht.khia.rz

	   loc_incomeNhood <- params["logIncomeNhood",] %o% logIncome  %o% nHood #ht.khia.rz

	   loc_child <- child %o% kshare #khia.rz.ht
	   loc_child <- aperm(loc_child, c(3,1,2)) #ht.khia.rz
 
	#Loop by ezone
  	print("Calculating location choice...")

	res_demand_KHIARz <- array(0, c(numHtypes, numKHIA, numRzones))
	res_demand_EzRz <- array(0, c(numEzones,numRzones,numHtypes) )
	
	
 	
	for(ez in 1:numEzones) {
        
	  	loc_logTTbyEzone <- logTravelTimeByEz[,,,ez]
 
		#catLine status message
		#cat(".")
		
		# ht.khia.rz
    		utility <- exp(loc_const
		+ loc_logTT * loc_logTTbyEzone
		+ loc_logTTSq * loc_logTTbyEzone * loc_logTTbyEzone
		+ loc_logHrx
		+ loc_childTT * loc_logTTbyEzone
		+ loc_ageTT * loc_logTTbyEzone
		+ loc_hhsizeTT * loc_logTTbyEzone
		+ loc_incomeTT * loc_logTTbyEzone
    		+ loc_incomeNhood
    		+ loc_child )
		
		totalUtility <- apply(utility, c(1,2), sum) # ht.khia  sum across rzone to get total utility

		#Weight each hia's utility by its binshares
		 binsharesArray <- array(t(binshares),c(numHtypes, numBins, numRzones))

		 #KHIA bin lookup

		 osfBinShare <- binsharesArray[1, as.numeric(ownKhiaBinByEz[ez,]),]
		 omfBinShare <- binsharesArray[2, as.numeric(ownKhiaBinByEz[ez,]),]
		 rsfBinShare <- binsharesArray[3, as.numeric(rentKhiaBinByEz[ez,]),]
		 rmfBinShare <- binsharesArray[4, as.numeric(rentKhiaBinByEz[ez,]),]
		 
		 binshareKHIA <- array(c(osfBinShare, omfBinShare, rsfBinShare, rmfBinShare), c(numKHIA, numRzones, numHtypes))
		 rm(osfBinShare, omfBinShare, rsfBinShare, rmfBinShare)

		 #Multiply each util by its binshare to weight it
		 #weighted prob = weighted util / total util
		 #Calculate total weighted prob
		 #demand = weighted prob * demand / total weighted prob
        
		 weightedUtility <- aperm(utility,c(1,3,2)) * aperm(binshareKHIA, c(3,2,1)) # ht.rz.khia
		 weightedProb <- aperm(weightedUtility,c(1,3,2)) / array(totalUtility, c(dim(totalUtility), numRzones))

		 totalWeightedProb <- apply(weightedProb, c(1,2), sum)

		 demand <- array(aperm(typeChoice[ez,,],c(2,1)), dim(weightedProb))
		 ezoneResult <- weightedProb * demand / array(totalWeightedProb, c(dim(totalWeightedProb), numRzones))

		 #Result is dwelling units by htype x khia x rzone
		 #Need to add result to rzone array since allocating from ezone to rzones
		 res_demand_KHIARz <- res_demand_KHIARz + ezoneResult
		 
		 res_demand_EzRz[ez,,] <- t(apply(ezoneResult,c(1,3),sum))	 

	}
  	cat("\n")

	# Rz = 425,4   KHIA Rz = 4 400 425    Ez Rz = 72 425 4   Ez = 72
	res_demand_Rz <- t(apply(res_demand_KHIARz , c(1,3), sum))
   
 	# for determining child HH, k = 0
	khia_in <- readTable(scenarioID,"inputs_res","res_demand_khia_params")
    idxK <- khia_in[,"k"]
    res_demand_RzChild <- t(apply(res_demand_KHIARz[,idxK==1,], c(1,3), sum))
    
    
    RzEz_in <- readTable(scenarioID,"inputs_res","res_general_rzone_lut")
    idxRzEz <- RzEz_in[,"ezone"]

 	res_demand_Ez <- array(0,numEzones)
	res_demand_EzChild <- array(0,numEzones)
    for (ez in 1:numEzones){
        res_demand_Ez[ez] <- sum(res_demand_Rz[idxRzEz == ez,])
        res_demand_EzChild[ez] <- sum(res_demand_RzChild[idxRzEz == ez,])
    }
 

	#Return results	
	list(res_demand_Rz=res_demand_Rz, res_demand_KHIARz=res_demand_KHIARz, res_demand_EzRz=res_demand_EzRz, res_demand_Ez=res_demand_Ez, res_demand_EzChild=res_demand_EzChild)
}


calcBinAvg <- function(arrayData, ownKhiaBin, rentKhiaBin) {
# calcBinAvg()
# Calculates average of input array, by housing bin
#   
# Parameters:
# arrayData = matrix of values for each KHIA market segment, same as marketData
# ownKhiaBin = owner demand by KHIA, bin
# rentKhiaBin = renter demand by KHIA, bin
#
# Calls:
# NONE
#
# Returns: 
# arrayDataBin -- demand by housing type, KHIA, bin

    dim(arrayData) <- c(numKHIA,numHtypes,numRzones)   
    arrayData <- aperm(arrayData,c(3,2,1))
    
	arrayDataOSF <- apply(arrayData[,1,], 1, function(x) tapply(x, ownKhiaBin, mean)) 
	arrayDataOMF <- apply(arrayData[,2,], 1, function(x) tapply(x, ownKhiaBin, mean)) 
	arrayDataRSF <- apply(arrayData[,3,], 1, function(x) tapply(x, rentKhiaBin, mean)) 
	arrayDataRMF <- apply(arrayData[,4,], 1, function(x) tapply(x, rentKhiaBin, mean))

	missingBinsOSF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataOSF)[[1]])]
	missingBinsOMF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataOMF)[[1]])]
	missingBinsRSF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataRSF)[[1]])]
	missingBinsRMF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataRMF)[[1]])]

	#Make sure each one has the same bins
	zeroRows <- matrix(0, length(missingBinsOSF),numRzones)
	rownames(zeroRows) <- missingBinsOSF
	arrayDataOSF <- rbind(arrayDataOSF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataOSF)[[1]])
	arrayDataOSF <- arrayDataOSF[newOrder,]

	zeroRows <- matrix(0, length(missingBinsOMF), numRzones)
	rownames(zeroRows) <- missingBinsOMF
	arrayDataOMF <- rbind(arrayDataOMF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataOMF)[[1]])
	arrayDataOMF <- arrayDataOMF[newOrder,]

	zeroRows <- matrix(0, length(missingBinsRSF), numRzones)
	rownames(zeroRows) <- missingBinsRSF
	arrayDataRSF <- rbind(arrayDataRSF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataRSF)[[1]])
	arrayDataRSF <- arrayDataRSF[newOrder,]

	zeroRows <- matrix(0, length(missingBinsRMF), numRzones)
	rownames(zeroRows) <- missingBinsRMF
	arrayDataRMF <- rbind(arrayDataRMF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataRMF)[[1]])
	arrayDataRMF <- arrayDataRMF[newOrder,]

	arrayDataBin <- array(c(arrayDataOSF, arrayDataOMF, arrayDataRSF, arrayDataRMF), c(dim(arrayDataOSF),4))
	arrayDataBin

}


calcBinSum <- function(arrayData, ownKhiaBin, rentKhiaBin) {
# calcBinSum()
# Calculates sum of input array, by housing bin
#   
# Parameters:
# arrayData = matrix of values for each KHIA market segment, same as marketData
# ownKhiaBin = owner demand by KHIA, bin
# rentKhiaBin = renter demand by KHIA, bin
#
# Calls:
# NONE
#
# Returns: 
# arrayDataBin -- demand by housing type, KHIA, bin

	arrayDataOSF <- apply(arrayData[,1,], 1, function(x) tapply(x, ownKhiaBin, sum)) 
	arrayDataOMF <- apply(arrayData[,2,], 1, function(x) tapply(x, ownKhiaBin, sum)) 
	arrayDataRSF <- apply(arrayData[,3,], 1, function(x) tapply(x, rentKhiaBin, sum)) 
	arrayDataRMF <- apply(arrayData[,4,], 1, function(x) tapply(x, rentKhiaBin, sum))

	 
	missingBinsOSF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataOSF)[[1]])]
	missingBinsOMF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataOMF)[[1]])]
	missingBinsRSF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataRSF)[[1]])]
	missingBinsRMF <- (1:numBins)[!(1:numBins %in% dimnames(arrayDataRMF)[[1]])]

	#Make sure each one has the same bins
	zeroRows <- matrix(0, length(missingBinsOSF),numRzones)
	rownames(zeroRows) <- missingBinsOSF
	arrayDataOSF <- rbind(arrayDataOSF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataOSF)[[1]])
	arrayDataOSF <- arrayDataOSF[newOrder,]

	zeroRows <- matrix(0, length(missingBinsOMF), numRzones)
	rownames(zeroRows) <- missingBinsOMF
	arrayDataOMF <- rbind(arrayDataOMF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataOMF)[[1]])
	arrayDataOMF <- arrayDataOMF[newOrder,]

	zeroRows <- matrix(0, length(missingBinsRSF), numRzones)
	rownames(zeroRows) <- missingBinsRSF
	arrayDataRSF <- rbind(arrayDataRSF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataRSF)[[1]])
	arrayDataRSF <- arrayDataRSF[newOrder,]

	zeroRows <- matrix(0, length(missingBinsRMF), numRzones)
	rownames(zeroRows) <- missingBinsRMF
	arrayDataRMF <- rbind(arrayDataRMF, zeroRows)
	newOrder <- match(1:numBins, dimnames(arrayDataRMF)[[1]])
	arrayDataRMF <- arrayDataRMF[newOrder,]

	arrayDataBin <- array(c(arrayDataOSF, arrayDataOMF, arrayDataRSF, arrayDataRMF), c(dim(arrayDataOSF),4))
	arrayDataBin

}

