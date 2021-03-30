# G40_nonres_supply_module.R
# maintained by Jim Cser jim.cser@oregonmetro.gov
# last updated 2013-10-14

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



runNonresSupply <- function(iLoops) {
# runNonresSupply()
# The non-residential supply module for MetroScope.
#
# This module loads the required inputs, and then calls the functions to calculate the nonres supply.
# All the heavy lifting is done by calcNonresSqft(), once for non-subsidized "regular" supply
# and once for subsidized "UR" (for urban renweal) supply.

# General steps for calculating non-residential supply:
# 
# Input Parameters:
# iLoops = current iteration of nonres module
#
# Calls:
# calcNonresSupply()
#
# Returns:
# TODO
    
    sqftSupplyResults_reg <- calcNonresSqft("reg",iLoops)
    sqftSupplyResults_ur <- calcNonresSqft("ur",iLoops)
        
    sqft_vintagesupply <- sqftSupplyResults_reg$sqft_vintagesupply
    
            
    sqft_newsupply_reg <- sqftSupplyResults_reg$sqft_newsupply
    sqft_newsupply_ur <- sqftSupplyResults_ur$sqft_newsupply
    
    nonres_acresconsumed_reg <- sqftSupplyResults_reg$nonres_acresconsumed
    nonres_acresconsumed_ur <- sqftSupplyResults_ur$nonres_acresconsumed

    nonres_acresremaining_reg <- sqftSupplyResults_reg$nonres_acresremaining
    nonres_acresremaining_ur <- sqftSupplyResults_ur$nonres_acresremaining

    supplyResults_out <- list(sqft_vintagesupply=sqft_vintagesupply, sqft_newsupply_reg=sqft_newsupply_reg, sqft_newsupply_ur=sqft_newsupply_ur, 
                              nonres_acresconsumed_reg=nonres_acresconsumed_reg, nonres_acresconsumed_ur=nonres_acresconsumed_ur,
                              nonres_acresremaining_reg=nonres_acresremaining_reg, nonres_acresremaining_ur=nonres_acresremaining_ur )

#
#    #### TEST HARNESS
#if (iLoops == 1) {   
#    print("nonres sqft newsupply reg")
#    table_ref <- read.csv("test_G40_core/nonres_sqft_newsupply_reg_ref.csv")
#    table_in <- sqft_newsupply_reg
#    print(as.vector(colSums(table_ref)))
#    print(as.vector(colSums(table_in)))
#   
#    print("nonres sqft newsupply ur")
#    table_ref <- read.csv("test_G40_core/nonres_sqft_newsupply_ur_ref.csv")
#    table_in <- sqft_newsupply_ur
#    print(as.vector(colSums(table_ref)))
#    print(as.vector(colSums(table_in)))
# 
#   
#    print("nonres acresconsumed reg")
#    table_ref <- read.csv("test_G40_core/nonres_acresconsumed_reg_ref.csv")
#    table_in <- nonres_acresconsumed_reg
#    print(as.vector(colSums(table_ref)))
#    print(as.vector(colSums(table_in)))
#   
#    print("nonres acresconsumed ur")
#    table_ref <- read.csv("test_G40_core/nonres_acresconsumed_ur_ref.csv")
#    table_in <- nonres_acresconsumed_ur
#    print(as.vector(colSums(table_ref)))
#    print(as.vector(colSums(table_in)))
#   
#
#    ####
#}
    return ( supplyResults_out )


}



calcNonresSqft <- function(landtype,iLoops) {

    nonres_general_params_in <- readTable(scenarioID,"inputs_nonres","nonres_general_params")
    rows <- nonres_general_params_in$parameter
    rownames(nonres_general_params_in) <- noquote(rows)
    
    nonres_general_params <- nonres_general_params_in[,"value",F]
    
    capcost_ind1_param <- nonres_general_params["capcost_ind1_param",]
    capcost_ind2_param <- nonres_general_params["capcost_ind2_param",]
    capcost_com1_param <- nonres_general_params["capcost_com1_param",]
    capcost_com2_param <- nonres_general_params["capcost_com2_param",]
    capcost_res1_param <- nonres_general_params["capcost_res1_param",]
    capcost_res2_param <- nonres_general_params["capcost_res2_param",]
    
    landcost_ind1_param <- nonres_general_params["landcost_ind1_param",]
    landcost_com1_param <- nonres_general_params["landcost_com1_param",]
    landcost_res1_param <- nonres_general_params["landcost_res1_param",]
    
    marketprice_man1_param <- nonres_general_params["marketprice_man1_param",]
    marketprice_man2_param <- nonres_general_params["marketprice_man2_param",]
    marketprice_war1_param <- nonres_general_params["marketprice_war1_param",]
    marketprice_war2_param <- nonres_general_params["marketprice_war2_param",]
    marketprice_flex1_param <- nonres_general_params["marketprice_flex1_param",]
    marketprice_flex2_param <- nonres_general_params["marketprice_flex2_param",]
    
    marketprice_ret1_param <- nonres_general_params["marketprice_ret1_param",]
    marketprice_ret2_param <- nonres_general_params["marketprice_ret2_param",]
    marketprice_gen1_param <- nonres_general_params["marketprice_gen1_param",]
    marketprice_gen2_param <- nonres_general_params["marketprice_gen2_param",]
    marketprice_med1_param <- nonres_general_params["marketprice_med1_param",]
    marketprice_med2_param <- nonres_general_params["marketprice_med2_param",]
    
    marketprice_resland1_param <- nonres_general_params["marketprice_resland1_param",]
    marketprice_resland2_param <- nonres_general_params["marketprice_resland2_param",]
    marketprice_resnoland1_param <- nonres_general_params["marketprice_resnoland1_param",]
    marketprice_resnoland2_param <- nonres_general_params["marketprice_resnoland_param",]
    
    ind_capital_land_sub_param <- nonres_general_params["ind_capital_land_sub_param",]
    com_capital_land_sub_param <- nonres_general_params["com_capital_land_sub_param",]
    res_capital_land_sub_param <- nonres_general_params["res_capital_land_sub_param",]
    
    landprice1_param <- nonres_general_params["landprice1_param",]
    landprice2_param <- nonres_general_params["landprice2_param",]
    landprice3_param <- nonres_general_params["landprice3_param",]
    
    ########
    
    if (landtype == "reg"){
        ind_costsqft_offset <- 0
        com_costsqft_offset <- 0
        res_costsqft_offset <- 0
    }  else {
        ind_costsqft_offset <- nonres_general_params["ind_ur_costsqft_offset",]
        com_costsqft_offset <- nonres_general_params["com_ur_costsqft_offset",]
        res_costsqft_offset <- nonres_general_params["res_ur_costsqft_offset",]
    }
    
    
    
    ##########
    
    if (currentYear == 1){
        landsupply_stock <- 0
    } else {
        landsupply_stock <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_acres_remaining_year",(currentYear-1),sep=""))
    }
        
        landsupply_added <- readTable(scenarioID,"inputs_nonres",paste("nonres_acres_added_year",currentYear,sep=""))
        landsupply_in <- landsupply_stock + landsupply_added
        
    if (landtype == "reg"){
        landsupply_init <- landsupply_in[,3:5]
     }  else {
        landsupply_init <- landsupply_in[,6:8]
    }
    
    ###### USE DEFAULT METERING OF ACRES BY YEAR, NO PRICE COMPONENT
    landsupply <- landsupply_init * 0.4
    ######
    
    ##### calculate capital cost by FAR    
    # load FAR parameters, expand [far] vectors to [ez x far] array

    far_params_in <- readTable(scenarioID,"inputs_nonres","nonres_supply_farclass_params")
    
    avgfar <- far_params_in$avg_far
    avgfar_array <- t( array(avgfar,c(numFAR,numEzones)) )
    
    capitalcostsqft_far1_in <- readTable(scenarioID,"inputs_nonres","nonres_supply_capitalcost")
    capitalcostsqft_far1 <- capitalcostsqft_far1_in[,2:4]  
    
    
    # base ind capitalcostsqft by far = base capitalcostsqft + base ind capitalcostsqft
    # enforce minimum value
    
    # industrial
    base_capitalcostsqft_far1 <- array( t(capitalcostsqft_far1[,1]) ,c(numEzones,numFAR))
    base_ind_capitalcostsqft_far1 <- t( array(log(avgfar) * capcost_ind1_param  ,c(numFAR,numEzones)) )
    
    base_ind_capitalcostsqft_far <- base_capitalcostsqft_far1 + base_ind_capitalcostsqft_far1
    base_ind_capitalcostsqft_far[base_ind_capitalcostsqft_far < capcost_ind2_param] <- capcost_ind2_param
    
    # commerical
    base_capitalcostsqft_far1 <- array( t(capitalcostsqft_far1[,2]) ,c(numEzones,numFAR))
    base_com_capitalcostsqft_far1 <- t( array(log(avgfar) * capcost_com1_param  ,c(numFAR,numEzones)) )
    
    base_com_capitalcostsqft_far <- base_capitalcostsqft_far1 + base_com_capitalcostsqft_far1
    base_com_capitalcostsqft_far[base_com_capitalcostsqft_far < capcost_com2_param] <- capcost_com2_param
    
    # residential
    base_capitalcostsqft <- array( t(capitalcostsqft_far1[,3]) ,c(numEzones,numFAR))
    base_res_capitalcostsqft <- t( array(log(avgfar) * capcost_res1_param  ,c(numFAR,numEzones)) )
    
    base_res_capitalcostsqft_far <- base_capitalcostsqft + base_res_capitalcostsqft
    base_res_capitalcostsqft_far[base_res_capitalcostsqft_far < capcost_res2_param] <- capcost_res2_param
    
    
    ##### calculate land cost by FAR
    
    landcostsqft_far1_in <- readTable(scenarioID,"inputs_nonres","nonres_supply_landcost")
    landcostsqft_far1 <- landcostsqft_far1_in[,2:4]  
    
    # base ind landcostsqft by far = base landcostsqft + base ind landcostsqft
    # enforce minimum value
    
    # load minimum base land cost
    min_landcostsqft_in <- readTable(scenarioID,"inputs_nonres","nonres_supply_minlandcost")
    
    ind_min_landcostsqft <- min_landcostsqft_in[,2]
    ind_min_landcostsqft <- array(ind_min_landcostsqft, c(numEzones,numFAR))
    
    com_min_landcostsqft <- min_landcostsqft_in[,3]
    com_min_landcostsqft <- array(com_min_landcostsqft, c(numEzones,numFAR))
    
    res_min_landcostsqft <- min_landcostsqft_in[,4]
    res_min_landcostsqft <- array(res_min_landcostsqft, c(numEzones,numFAR))
    
    
    # industrial
    base_landcostsqft_far1 <- array( t(landcostsqft_far1[,1]) ,c(numEzones,numFAR))
    base_ind_landcostsqft_far1 <- t( array(log(avgfar) * landcost_ind1_param  ,c(numFAR,numEzones)) )
    
    
    base_ind_landcostsqft_far <- base_landcostsqft_far1 + base_ind_landcostsqft_far1
    flag <- base_ind_landcostsqft_far < ind_min_landcostsqft
    base_ind_landcostsqft_far[flag] <- ind_min_landcostsqft[flag]
    
    
    # commerical
    base_landcostsqft_far1 <- array( t(landcostsqft_far1[,2]) ,c(numEzones,numFAR))
    base_com_landcostsqft_far1 <- t( array(log(avgfar) * landcost_com1_param  ,c(numFAR,numEzones)) )
    
    base_com_landcostsqft_far <- base_landcostsqft_far1 + base_com_landcostsqft_far1
    flag <- base_com_landcostsqft_far < com_min_landcostsqft
    base_com_landcostsqft_far[flag] <- com_min_landcostsqft[flag]
    
    # residential
    base_landcostsqft <- array( t(landcostsqft_far1[,3]) ,c(numEzones,numFAR))
    base_res_landcostsqft <- t( array(log(avgfar) * landcost_res1_param  ,c(numFAR,numEzones)) )
    
    base_res_landcostsqft_far <- base_landcostsqft + base_res_landcostsqft
    flag <- base_res_landcostsqft_far < res_min_landcostsqft
    base_res_landcostsqft_far[flag] <- res_min_landcostsqft[flag]
    
    
    
    
    ##### calculate market price by FAR
    marketpricesqft_far1_in <- readTable(scenarioID,"inputs_nonres","nonres_supply_marketprice")
    marketpricesqft_far1 <- marketpricesqft_far1_in[,2:9]  #man,war,flex,ret,gen,med,resland,resnoland
    
    
    # base ind marketpricesqft by far = base marketpricesqft + base ind marketpricesqft
    
    # industrial -- manufacturing
    
    # expand [ez] vector to [ez x far] array
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,1]) ,c(numEzones,numFAR))
    # expand [far] vector to [ez x far] array
    base_man_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_man1_param  ,c(numFAR,numEzones)) )
    # add
    base_man_marketpricesqft_far <- base_marketpricesqft_far1 + base_man_marketpricesqft_far1
    # impose minimum value
    base_man_marketpricesqft_far[base_man_marketpricesqft_far < marketprice_man2_param] <- marketprice_man2_param
    
    # industrial -- warehousing
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,2]) ,c(numEzones,numFAR))
    base_war_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_war1_param,c(numFAR,numEzones)) )
    base_war_marketpricesqft_far <- base_marketpricesqft_far1 + base_war_marketpricesqft_far1
    base_war_marketpricesqft_far[base_war_marketpricesqft_far < marketprice_war2_param] <- marketprice_war2_param
    
    # industrial -- flex space
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,3]) ,c(numEzones,numFAR))
    base_flex_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_flex1_param  ,c(numFAR,numEzones)) )
    base_flex_marketpricesqft_far <- base_marketpricesqft_far1 + base_flex_marketpricesqft_far1
    base_flex_marketpricesqft_far[base_flex_marketpricesqft_far < marketprice_flex2_param] <- marketprice_flex2_param
    
    # commercial -- retail/services
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,4]) ,c(numEzones,numFAR))
    base_ret_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_ret1_param  ,c(numFAR,numEzones)) )
    base_ret_marketpricesqft_far <- base_marketpricesqft_far1 + base_ret_marketpricesqft_far1
    base_ret_marketpricesqft_far[base_ret_marketpricesqft_far < marketprice_ret2_param] <- marketprice_ret2_param
    
    # commercial -- general office
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,5]) ,c(numEzones,numFAR))
    base_gen_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_gen1_param  ,c(numFAR,numEzones)) )
    base_gen_marketpricesqft_far <- base_marketpricesqft_far1 + base_gen_marketpricesqft_far1
    base_gen_marketpricesqft_far[base_gen_marketpricesqft_far < marketprice_gen2_param] <- marketprice_gen2_param
    
    # commercial -- medical/institutional
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,6]) ,c(numEzones,numFAR))
    base_med_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_med1_param  ,c(numFAR,numEzones)) )
    base_med_marketpricesqft_far <- base_marketpricesqft_far1 + base_med_marketpricesqft_far1
    base_med_marketpricesqft_far[base_med_marketpricesqft_far < marketprice_med2_param] <- marketprice_med2_param
    
    # residential -- land
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,7]) ,c(numEzones,numFAR))
    base_resland_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_resland1_param  ,c(numFAR,numEzones)) )
    base_resland_marketpricesqft_far <- base_marketpricesqft_far1 + base_resland_marketpricesqft_far1
    base_resland_marketpricesqft_far[base_resland_marketpricesqft_far < marketprice_resland2_param] <- marketprice_resland2_param
    
    # residential -- no land
    base_marketpricesqft_far1 <- array( t(marketpricesqft_far1[,8]) ,c(numEzones,numFAR))
    base_resnoland_marketpricesqft_far1 <- t( array(log(avgfar) * marketprice_resnoland1_param  ,c(numFAR,numEzones)) )
    base_resnoland_marketpricesqft_far <- base_marketpricesqft_far1 + base_resnoland_marketpricesqft_far1
    base_resnoland_marketpricesqft_far[base_resnoland_marketpricesqft_far < marketprice_resnoland2_param] <- marketprice_resnoland2_param
    
    
    ##### calculate weighted price ratio
    # wt_priceratio = sumprod(price ratio, vintage supply) / sum(vintage supply)    [ez x re]
    
    # load location price, shared between supply and demand modules
    # at first iteration of model year, use location price from previous year, otherwise use the updated value
    if (iLoops == 1 & iLanduseLoops == 1){
#        if (currentYear == 1){
#            nonres_locationprice_in <- readTable(scenarioID,"inputs_nonres",paste("nonres_locationprice_year",0,sep=""))
#        } else {
#            nonres_locationprice_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_locationprice_year",(currentYear-1),sep=""))
#        }

     nonres_locationprice_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_locationprice_year",(currentYear-1),sep=""))


    } else {
        nonres_locationprice_in <- readTable(scenarioID,"shared_nonres",paste("nonres_locationprice_year",currentYear,sep=""))
    }        
    
    #### debug -- sonny XL had a year 1 location price already
    #nonres_locationprice_in <- readTable(scenarioID,"inputs_nonres","_nonres_locationprice_year1_SONNY")
           
    nonres_location_price <- as.matrix(nonres_locationprice_in[,2:(numRE+1)])     #[ezone x re]
      
    #nonres_calibration_price_in <- readTable(scenarioID,"inputs_nonres","nonres_locationprice_year0") 
    
    nonres_calibration_price_in <- readTable(scenarioID,"outputs_year0","nonres_locationprice_year0") 
    nonres_calibration_price <- nonres_calibration_price_in[,2:(numRE+1)] 
    
    priceratio <- nonres_location_price / nonres_calibration_price
    
    
    ### read supply from previous year
    
        # vintage employment for access calculation 
#    if (currentYear == 1){
#       vintagesupply_in <- readTable(scenarioID,"inputs_nonres",paste("nonres_supply_totalsupply_year",0,sep=""))     
#    } else {
#       vintagesupply_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_supply_totalsupply_year",(currentYear-1),sep=""))     
#    }

    vintagesupply_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_supply_totalsupply_year",(currentYear-1),sep=""))     

    vintagesupply <- vintagesupply_in[,2:9]  #man,war,flex,ret,gen,med,resland,resnoland
    
    ind_wt_priceratio <-  rowSums( priceratio[,1:3] * vintagesupply[,1:3]) / rowSums(vintagesupply[,1:3])    # [ez]
    com_wt_priceratio <-  rowSums( priceratio[,4:6] * vintagesupply[,4:6]) / rowSums(vintagesupply[,4:6])    
    #res_wt_priceratio <-  rowSums( priceratio[,7:8] * vintagesupply[,7:8]) / rowSums(vintagesupply[,7:8]) 
    res_wt_priceratio <-  priceratio[,7]
    
    
    ##### calculate weighted land price ratio     
    ind_wt_landpriceratio <-  exp(landprice1_param + landprice2_param * log(ind_wt_priceratio) - landprice3_param)  # [ez] 
    com_wt_landpriceratio <-  exp(landprice1_param + landprice2_param * log(com_wt_priceratio) - landprice3_param)
    res_wt_landpriceratio <-  exp(landprice1_param + landprice2_param * log(res_wt_priceratio) - landprice3_param)
    
    
    ##### calculate market adjusted FAR
    # avgfar * ind_wt_landpriceratio ^  ind_capital_land_sub_param        
    
    
    # expand [ez] vector to [ez x far] array
    ind_wt_landpriceratio_far  <- array( t(ind_wt_landpriceratio) ,c(numEzones,numFAR))
    # apply fomula
    ind_marketadjusted_far <- avgfar_array * (ind_wt_landpriceratio_far) ^ ind_capital_land_sub_param     
             
    com_wt_landpriceratio_far  <- array( t(com_wt_landpriceratio) ,c(numEzones,numFAR))
    com_marketadjusted_far <- avgfar_array * (com_wt_landpriceratio_far) ^ com_capital_land_sub_param     
    
    res_wt_landpriceratio_far  <- array( t(res_wt_landpriceratio) ,c(numEzones,numFAR))
    res_marketadjusted_far <- avgfar_array * (res_wt_landpriceratio_far) ^ res_capital_land_sub_param     
    
    
    
    ##### calculate market adjusted capacity by ind,com,res
    ##### calculate market adjusted capacity by re types (man,war,flex,ret,gen,med,resland,resnoland)
    ##### calculate feasible share by re types (man,war,flex,ret,gen,med,resland,resnoland)
    ##### final new supply for each real estate type = feasible share * market capacity


   ##### Calc square footage supply

    maxfar <- far_params_in$max_far
    maxfar_array <- t( array(maxfar,c(numFAR,numEzones)) )  
    
    # enforce maximum far for market capacity calc, leave as is for feasible share calc
    ind_marketadjusted_farCAPPED <- ind_marketadjusted_far
    com_marketadjusted_farCAPPED <- com_marketadjusted_far
    res_marketadjusted_farCAPPED <- res_marketadjusted_far
    
    ind_marketadjusted_farCAPPED[ind_marketadjusted_far > maxfar_array] <- maxfar_array[ind_marketadjusted_far > maxfar_array]
    com_marketadjusted_farCAPPED[com_marketadjusted_far > maxfar_array] <- maxfar_array[com_marketadjusted_far > maxfar_array]
    res_marketadjusted_farCAPPED[res_marketadjusted_far > maxfar_array] <- maxfar_array[res_marketadjusted_far > maxfar_array]
    
    
    # far_cap_array = gross-to-net

    far_cap_param <- far_params_in$far_cap_param
    far_cap_array <- t( array(far_cap_param,c(numFAR,numEzones)) )  
    
    ind_landsupply <- t (array(landsupply[,1], c(numFAR,numEzones)) )    
    ind_marketadjusted_capacity <- ind_landsupply * ind_marketadjusted_farCAPPED * 43560 * far_cap_array 
    
    com_landsupply <- t (array(landsupply[,2], c(numFAR,numEzones)) )     
    com_marketadjusted_capacity <- com_landsupply * com_marketadjusted_farCAPPED * 43560 * far_cap_array 
    
    res_landsupply <- t (array(landsupply[,3], c(numFAR,numEzones)) )     
    res_marketadjusted_capacity <- res_landsupply * res_marketadjusted_farCAPPED * 43560 * far_cap_array 
    
    
    
    ##### calculate market adjusted capacity by re types (man,war,flex,ret,gen,med,resland,resnoland)
    
    # calc adjusted total production cost by ind,com,res
    # ind_production_cost <- (base capital cost + base land cost * wt. land price ratio) * ( market far / avg far )

    
    ind_production_cost <- (base_ind_capitalcostsqft_far + base_ind_landcostsqft_far * ind_wt_landpriceratio) *
                               (ind_marketadjusted_far / avgfar_array)
    
    com_production_cost <- (base_com_capitalcostsqft_far + base_com_landcostsqft_far * com_wt_landpriceratio) *
                               (com_marketadjusted_far / avgfar_array)
    
    res_production_cost <- (base_res_capitalcostsqft_far + base_res_landcostsqft_far * res_wt_landpriceratio) *
                               (res_marketadjusted_far / avgfar_array)
                             
    # for regular acres, no change in production cost
    # for ur acres, production_cost  <- production_cost - subsidy                        
    
    
    ind_production_cost <- ind_production_cost - ind_costsqft_offset
    com_production_cost <- com_production_cost - com_costsqft_offset                        
    res_production_cost <- res_production_cost - res_costsqft_offset              
    
    ##### calculate feasible share by re types (man,war,flex,ret,gen,med,resland,resnoland)
          
    # calc share numerators for real estate types                           

    # build where price > cost:  if (price ratio * base market price) >= production cost 
    # then feasible share numerator = (price ratio * base market price * vintage supply), else = 0
    
    # feasible share denominator = sum of real estate types
    # total market capacity * share of each re type *  = re type market capacity
    
    # ind -- reg
    man_priceratio_far  <- array( t(priceratio[,1]) ,c(numEzones,numFAR))
    man_feasibleshare_numerator <- man_priceratio_far * base_man_marketpricesqft_far * vintagesupply[,1]
    man_feasibleshare_numerator[man_priceratio_far * base_man_marketpricesqft_far < ind_production_cost] <- 0
    
    war_priceratio_far  <- array( t(priceratio[,2]) ,c(numEzones,numFAR))
    war_feasibleshare_numerator <- war_priceratio_far * base_war_marketpricesqft_far * vintagesupply[,2]
    war_feasibleshare_numerator[war_priceratio_far * base_war_marketpricesqft_far < ind_production_cost] <- 0
    
    flex_priceratio_far  <- array( t(priceratio[,3]) ,c(numEzones,numFAR))
    flex_feasibleshare_numerator <- flex_priceratio_far * base_flex_marketpricesqft_far * vintagesupply[,3]
    flex_feasibleshare_numerator[flex_priceratio_far * base_flex_marketpricesqft_far < ind_production_cost] <- 0
    
    ind_feasibleshare_demoninator <- man_feasibleshare_numerator + war_feasibleshare_numerator + flex_feasibleshare_numerator
    
    
    # com -- reg
    ret_priceratio_far  <- array( t(priceratio[,4]) ,c(numEzones,numFAR))
    ret_feasibleshare_numerator <- ret_priceratio_far * base_ret_marketpricesqft_far * vintagesupply[,4]
    ret_feasibleshare_numerator[ret_priceratio_far * base_ret_marketpricesqft_far < com_production_cost] <- 0
    
    gen_priceratio_far  <- array( t(priceratio[,5]) ,c(numEzones,numFAR))
    gen_feasibleshare_numerator <- gen_priceratio_far * base_gen_marketpricesqft_far * vintagesupply[,5]
    gen_feasibleshare_numerator[gen_priceratio_far * base_gen_marketpricesqft_far < com_production_cost] <- 0
    
    med_priceratio_far  <- array( t(priceratio[,6]) ,c(numEzones,numFAR))
    med_feasibleshare_numerator <- med_priceratio_far * base_med_marketpricesqft_far * vintagesupply[,6]
    med_feasibleshare_numerator[med_priceratio_far * base_med_marketpricesqft_far < com_production_cost] <- 0
    
    com_feasibleshare_demoninator <- ret_feasibleshare_numerator + gen_feasibleshare_numerator + med_feasibleshare_numerator
    
    # res -- reg
    resland_priceratio_far  <- array( t(priceratio[,7]) ,c(numEzones,numFAR))
    resland_feasibleshare_numerator <- resland_priceratio_far * base_resland_marketpricesqft_far * vintagesupply[,7]
    resland_feasibleshare_numerator[resland_priceratio_far * base_resland_marketpricesqft_far < res_production_cost] <- 0
    
#    resnoland_priceratio_far  <- array( t(priceratio[,8]) ,c(numEzones,numFAR))
#    resnoland_feasibleshare_numerator <- resnoland_priceratio_far * base_resnoland_marketpricesqft_far * vintagesupply[,8]
#    resnoland_feasibleshare_numerator[resnoland_priceratio_far * base_resnoland_marketpricesqft_far < res_production_cost] <- 0
    
    res_feasibleshare_demoninator <- resland_feasibleshare_numerator
    
    
    ##### final new supply for each real estate type = feasible share * market capacity
    
    # ind
    man_feasibleshare <- man_feasibleshare_numerator / ind_feasibleshare_demoninator
    man_feasibleshare[ind_feasibleshare_demoninator == 0] <- 0
    man_supply <- man_feasibleshare * ind_marketadjusted_capacity
    
    war_feasibleshare <- war_feasibleshare_numerator / ind_feasibleshare_demoninator
    war_feasibleshare[ind_feasibleshare_demoninator == 0] <- 0
    war_supply <- war_feasibleshare * ind_marketadjusted_capacity
    
    flex_feasibleshare <- flex_feasibleshare_numerator / ind_feasibleshare_demoninator
    flex_feasibleshare[ind_feasibleshare_demoninator == 0] <- 0
    flex_supply <- flex_feasibleshare * ind_marketadjusted_capacity
    
    ind_supply <- cbind( rowSums(man_supply), rowSums(war_supply), rowSums(flex_supply))
     
    ind_acres_ratio <- array(1,c(numEzones,numFAR))
    ind_acres_ratio[man_feasibleshare + war_feasibleshare + flex_feasibleshare == 0] <- 0
    
    
    # com
    ret_feasibleshare <- ret_feasibleshare_numerator / com_feasibleshare_demoninator
    ret_feasibleshare[com_feasibleshare_demoninator == 0] <- 0
    ret_supply <- ret_feasibleshare * com_marketadjusted_capacity
    
    gen_feasibleshare <- gen_feasibleshare_numerator / com_feasibleshare_demoninator
    gen_feasibleshare[com_feasibleshare_demoninator == 0] <- 0
    gen_supply <- gen_feasibleshare * com_marketadjusted_capacity
    
    med_feasibleshare <- med_feasibleshare_numerator / com_feasibleshare_demoninator
    med_feasibleshare[com_feasibleshare_demoninator == 0] <- 0
    med_supply <- med_feasibleshare * com_marketadjusted_capacity
    
    com_supply <- cbind( rowSums(ret_supply), rowSums(gen_supply), rowSums(med_supply))
    
    com_acres_ratio <- array(1,c(numEzones,numFAR))
    com_acres_ratio[ret_feasibleshare + gen_feasibleshare + med_feasibleshare == 0] <- 0
    
    # res
    resland_feasibleshare <- resland_feasibleshare_numerator / res_feasibleshare_demoninator
    resland_feasibleshare[res_feasibleshare_demoninator == 0] <- 0
    resland_supply <- resland_feasibleshare * res_marketadjusted_capacity
    
#    resnoland_feasibleshare <- resnoland_feasibleshare_numerator / res_feasibleshare_demoninator
#    resnoland_feasibleshare[res_feasibleshare_demoninator == 0] <- 0
#    resnoland_supply <- resnoland_feasibleshare * res_marketadjusted_capacity
    
#    res_supply <- cbind( rowSums(resland_supply), rowSums(resnoland_supply))
    
#    res_acres_ratio <- array(1,c(numEzones,numFAR))
#    res_acres_ratio[resland_feasibleshare + resnoland_feasibleshare == 0] <- 0
    
    
    resnoland_supply <- array(0,dim(resland_supply))
    res_supply <- cbind( rowSums(resland_supply), rowSums(resnoland_supply))
    
    res_acres_ratio <- array(1,c(numEzones,numFAR))
    res_acres_ratio[resland_feasibleshare == 0] <- 0
    
    # combine
    sqft_newsupply <- cbind(ind_supply, com_supply, res_supply)
    
    
    ##### Calc acres supply consumed
    # use ratio of capacity built / total market capacity to determine acres used
    nonres_acresratio <- cbind(as.vector(t(ind_acres_ratio)),as.vector(t(com_acres_ratio)),as.vector(t(res_acres_ratio)))
    
    nonres_acresconsumed <- array(0, dim(landsupply))
    nonres_acresconsumed <- landsupply * nonres_acresratio
    
    
    ## use initial supply
    nonres_acresremaining <- landsupply_init - nonres_acresconsumed
    nonres_acresremaining[nonres_acresremaining < 0.001] <- 0
    
    
     #if (iLoops == 1){browser()}
    return ( list(sqft_vintagesupply=vintagesupply, sqft_newsupply=sqft_newsupply, nonres_acresconsumed=nonres_acresconsumed , nonres_acresremaining=nonres_acresremaining) )
    
}

