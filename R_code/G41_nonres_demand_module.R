# G35_nonres_demand_module.R
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


runNonresDemand <- function(iLoops){
# runNonresDemand()
# The non-residential demand module for MetroScope.
#  
# This function distributes the regional control total of employment by employment classes. 
# Both employment demand and square footage demand are calculated and distributed
# over employment zones and real estate types.
#  
# Non-residential demand =
#   Employment Demand *
#   Baseline Square Feet per Employee (by emp class, re type) *
#   Location Price (by ezone, re type) ^ Direct Sqft Elasticity (by emp class, re type) *
#   Location Price (by ezone, re type) ^ Direct Price Elasticity (by emp class) 
#
# Details of terms of the non-residential demand equations =
#   employment controls for year N [empclass] * baseline distribution param [empclass x re] * 
#   product over re ( location price [ez x re] ^ price elasticity [eclass x re x re2] ) [empclass x re] *  
#   baseline sqft/emp [empclass x re] * location price [ez x re] ^ direct sqft elasticity [empclass x re] *
#   location price [ez x re] ^ direct price elasticity [empclass] * access weights
#
# access weights = 
#   (access weight, total emp [ez] * access_param_totalemp [empclass]) + 
#   (access weight, total hh [ez] * access_param_totalhh [empclass]) + 
#   (access weight, empclass emp [ez x empclass] * access_param_empclass [empclass])   
# 
# where [ ] denotes the dimension of the input matrix
#
# Input Parameters:
# iLoops = current iteration of nonres demand module
# 
# Calls:
# calcNonresAccess()
# 
# Returns:
# sqftDemandcalc = sqft demand by ezone, empclass, retypes
# sqftDemandEzRe = sqft demand by ezone, retypes
# empDemand_calc = emp demand by ezone, empclass, retypes
# empDemandEzEc = emp demand by ezone, empclass
# empDemandEz = emp demand by ezone
      

    # data shared between res and nonres models   
    # nonres module is run first, so on first land use loop you must use hh from previous year
      
#    if (currentYear == 1){
#        if (iLanduseLoops == 1){
#            res_currentyear_hh_in <- readTable(scenarioID,"inputs_nonres",paste("nonres_demand_hh_year",0,sep=""))     
#        } else {
#            res_currentyear_hh_in <- readTable(scenarioID,paste("outputs_year",currentYear,sep=""), paste("nonres_demand_hh_year",currentYear,sep=""))     
#        }
#    }
#    if (currentYear > 1){
#        if (iLanduseLoops == 1){
#            res_currentyear_hh_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_demand_hh_year",(currentYear-1),sep=""))     
#        } else {
#            res_currentyear_hh_in <- readTable(scenarioID,paste("outputs_year",currentYear,sep=""),paste("nonres_demand_hh_year",currentYear,sep=""))     
#        }
#    }
#     
      
        if (iLanduseLoops == 1){
            res_currentyear_hh_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_demand_hh_year",(currentYear-1),sep=""))     
        } else {
            res_currentyear_hh_in <- readTable(scenarioID,paste("outputs_year",currentYear,sep=""),paste("nonres_demand_hh_year",currentYear,sep=""))     
        }
   
      
      
      
    #res_currentyear_hh_in <- readTable(scenarioID,"shared_res",paste("nonres_demand_hh_year",currentYear,sep=""))     

    # vintage employment for access calculation 
#    if (currentYear == 1){
#       nonres_vintage_emp_in <- readTable(scenarioID,"inputs_nonres",paste("nonres_demand_empclass_emp_year",0,sep=""))     
#    } else {
#       nonres_vintage_emp_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_demand_empclass_emp_year",(currentYear-1),sep=""))     
#    }
       
       nonres_vintage_emp_in <- readTable(scenarioID,paste("outputs_year",(currentYear-1),sep=""),paste("nonres_demand_empclass_emp_year",(currentYear-1),sep=""))     
    
 
    
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
    nonres_location_price <- array(nonres_location_price, c(numEzones, numRE, numEmpclass))  #[ezone x re x empclass]
    nonres_location_price <- aperm(nonres_location_price, c(1,3,2))   #[ezone x empclass x re]
    
    
    # restored for tandem mode   JBC  2014-02-17 1:20:43 PM
    # load travel time, for access calculations    
       
    if (isDefaultTT == T){   
       nonres_traveltime_in <- readTable(scenarioID,"shared_transport",paste("nonres_traveltime_year",currentYear,sep=""))
    } 
  
    if (isDefaultTT == F){     
        if (iMainLoops == 1){
            nonres_traveltime_in <- readTable(scenarioID,"shared_transport",paste("nonres_traveltime_year",(currentYear-1),sep=""))
        } else {
            nonres_traveltime_in <- readTable(scenarioID,"shared_transport",paste("nonres_traveltime_year",currentYear,sep=""))
        }
     }

    
    access_param_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_access_param")   
    access_time_param_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_access_time_param")

     
    ###(1) employment controls for year N
    ### changed to one for all model years (finally!)  2014-04-28
    
    #emp_controls_yearN_in <- readTable(scenarioID,"inputs_nonres",paste("nonres_demand_emp_controls_year",currentYear,sep=""))
    #emp_controls_yearN <- emp_controls_yearN_in[,2]           #[empclass]
    emp_controls_yearN_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_emp_controls")
    emp_controls_yearN <- emp_controls_yearN_in[,currentYear+2]           #[empclass]
    emp_controls_yearN <- array(emp_controls_yearN, c(numEmpclass, numEzones, numRE))  #[empclass x ezone x re]
    emp_controls_yearN <- aperm(emp_controls_yearN, c(2,1,3))    #[ezone x empclass x re ]
    
    ###(2) baseline distribution param
    baseline_dist_param_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_baseline_distribution_param")
    baseline_dist_param <- as.matrix(baseline_dist_param_in[,2:(numRE+1)])         #[empclass x re]
    baseline_dist_param <- array(baseline_dist_param, c(numEmpclass, numRE, numEzones))  #[empclass x re x ezone]
    baseline_dist_param <- aperm(baseline_dist_param, c(3,1,2))    #[ezone x empclass x re ]
    
    ###(3) calc cross price term
    # create expanded location price matrix
    # defined above:  nonres_location_price_array  #[ezone x empclass x re]
    nonres_location_price2 <- array(nonres_location_price,c(numEzones,numEmpclass,numRE,numRE)) #[ezone x empclass x re x re2]
    
    #create crossprice elasticity matrix
    crossprice_elasticity_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_crossprice_elasticity")
    crossprice_elasticity <- as.matrix(crossprice_elasticity_in[,3:(numRE+2)])  #[re,empclass x re2]
    crossprice_elasticity <- array(crossprice_elasticity, c(numRE,numEmpclass,numRE,numEzones)) #[re x empclass x re2 x ezone]
    crossprice_elasticity <- aperm(crossprice_elasticity, c(4,2,1,3)) #[ezone x empclass x re x re2] 
    
    # combine and take product over re dimension
    tempval <- nonres_location_price2 ^ crossprice_elasticity 
    crossprice_term <- apply(tempval, c(1,2,4), prod)
    
    
    ###(4) baseline sqft per employee
    baseline_sqftemp_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_baseline_sqft_emp")
    baseline_sqftemp <- as.matrix(baseline_sqftemp_in[,2:(numRE+1)])         #[empclass x re]
    baseline_sqftemp <- array(baseline_sqftemp, c(numEmpclass, numRE, numEzones))  #[empclass x re x ezone]
    baseline_sqftemp <- aperm(baseline_sqftemp, c(3,1,2))    #[ezone x empclass x re ]
    
    
    ###(5) direct sqft term
    directsqft_elasticity_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_directsqft_elasticity")
    directsqft_elasticity <- as.matrix(directsqft_elasticity_in[,2:(numRE+1)])         #[empclass x re]
    directsqft_elasticity <- array(directsqft_elasticity, c(numEmpclass, numRE, numEzones))  #[empclass x re x ezone]
    directsqft_elasticity <- aperm(directsqft_elasticity, c(3,1,2))    #[ezone x empclass x re ]
    
    directsqft_term <- nonres_location_price ^ directsqft_elasticity
    
    ###(6) direct price term
    directprice_elasticity_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_directprice_elasticity")
    directprice_elasticity <- directprice_elasticity_in[,2]            #[empclass]
    directprice_elasticity <- array(directprice_elasticity, c(numEmpclass, numEzones, numRE))  #[empclass x ezone x re]
    directprice_elasticity<- aperm(directprice_elasticity, c(2,1,3))    #[ezone x empclass x re ]
    
    directprice_term <- nonres_location_price ^ directprice_elasticity
    
    ###(7) access weights
    # load params
    access_param_in <- readTable(scenarioID,"inputs_nonres","nonres_demand_access_param")
    
    
    #############
    ### for calculation, get everything into [ezone x empclass x re ]
       
    
    access_param_totalemp <- access_param_in$totalemp  #[empclass]
    access_param_totalemp <- array(access_param_totalemp, c(numEmpclass, numEzones,  numRE))  #[empclass x ezone x re]
    access_param_totalemp <- aperm(access_param_totalemp, c(2,1,3))
    
    access_param_totalhh <- access_param_in$totalhh
    access_param_totalhh <- array(access_param_totalhh, c(numEmpclass, numEzones,  numRE))  #[empclass x ezone x re]
    access_param_totalhh <- aperm(access_param_totalhh, c(2,1,3))
    
    
    access_param_empclassemp <- access_param_in$empclassemp
    access_param_empclassemp <- array(access_param_empclassemp, c(numEmpclass, numEzones,  numRE))  #[empclass x ezone x re]
    access_param_empclassemp <- aperm(access_param_empclassemp, c(2,1,3))
    
    # calculate access weights 
                              
    combined_access_weights <- calcNonresAccess(nonres_traveltime_in, nonres_vintage_emp_in, res_currentyear_hh_in, access_param_in, access_time_param_in)                       
                                  
    ####  put it all together    
    nonres_sqftdemand <- emp_controls_yearN * baseline_dist_param * crossprice_term * baseline_sqftemp * 
                     directsqft_term * directprice_term * combined_access_weights
    

    ##############  Calc Adjusted Employment, Sqft
    # = sqft demand [ezone x empclass x re]  / 
    #   (baseline sqft/emp [empclass x re] * nonres location price [ezone x re] ^ direct sqft elasticity [ empclass x re]
    
    nonres_empdemand <- nonres_sqftdemand / (baseline_sqftemp * nonres_location_price ^ directsqft_elasticity)
    
       
    #adjusted employment = calc employment [ezone x empclass x re] * empcontrol [empclass] / sum of calc emp [empclass]
    
    nonres_empdemand_sum <- apply(nonres_empdemand,c(2),sum)  #[empclass]
    nonres_empdemand_sum <- array(nonres_empdemand_sum, c(numEmpclass, numEzones, numRE)) #[empclass x ezone x re]
    nonres_empdemand_sum <- aperm(nonres_empdemand_sum, c(2,1,3)) #[ezone x empclass x re] 
    
    nonres_empdemandADJ <- nonres_empdemand * emp_controls_yearN / nonres_empdemand_sum
        
    #adjusted employment = calc employment [ezone x empclass x re] * empcontrol [empclass] / sum of calc emp [empclass]
    nonres_sqftdemandADJ <- nonres_sqftdemand * emp_controls_yearN / nonres_empdemand_sum
    
    
    # format for output    
    sqftDemandcalc <-  aperm(nonres_sqftdemandADJ,c(2,1,3))
    dim(sqftDemandcalc) <- c(numEzones * numEmpclass,numRE)
 
    sqftDemandEzRe <- apply(nonres_sqftdemandADJ,c(1,3),sum) 
        
    empDemandcalc <-  aperm(nonres_empdemandADJ,c(2,1,3))
    dim(empDemandcalc) <- c(numEzones * numEmpclass,numRE)

    empDemandEzEc <- apply(nonres_empdemandADJ,c(1,2),sum)   
    
    empDemandEz <- apply(nonres_empdemandADJ,c(1),sum)   

    #if (iLoops == 1){browser()}
    demandResults_out <- list(sqftDemandcalc=sqftDemandcalc,sqftDemandEzRe=sqftDemandEzRe,empDemand_calc=empDemandcalc,empDemandEzEc=empDemandEzEc, empDemandEz=empDemandEz)
    

#    
#    #### TEST HARNESS
#if (iLoops == 1){   
#    print("nonres sqft demand")
#    table_ref <- read.csv("test_G40_core/nonres_sqftdemand_ref.csv")
#    table_in <- sqftDemandEzRe
#    print(as.vector(colSums(table_ref)))
#    print(as.vector(colSums(table_in)))
#
#    print("nonres emp demand")
#    table_ref <- read.csv("test_G40_core/nonres_empdemand_ref.csv")
#    table_in <- empDemandEzEc
#    print(as.vector(colSums(table_ref)))
#    print(as.vector(colSums(table_in)))
#    
#    ####
#}
#    # return result

    return(demandResults_out)

   

}

