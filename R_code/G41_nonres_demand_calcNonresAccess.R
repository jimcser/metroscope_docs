# G35_nonres_demand_calcNonresAccess.R
# MetroScope non-residential travel access weight calculation
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



calcNonresAccess <- function(nonres_traveltime_in, nonres_vintage_emp_in, res_currentyear_hh_in, access_param_in, access_time_param_in){
# calcNonresAccess()
# Calculates non-residential travel access weights
#  
# Access is an inverse function of travel time-- the longer the commute, the less probablity of choosing that location
#  
# access weights for total employment  = total employment [ezone] / 
#  (travel time [ezone1 x ezone2] * time param + travel time ^2 [ezone1 x ezone2] * timesq param
#
#  access weights for total households  =  = base year total households [ezone] / 
#  (travel time [ezone1 x ezone2] * time param + travel time ^2 [ezone1 x ezone2] * timesq param
#
#  access weights for employment by employment class = empclass emp [ezone] / 
#  (travel time [ezone1 x ezone2] * time param + travel time ^2 [ezone1 x ezone2] * timesq param
#
# ... except for Government- Education access [empclass 14 in gen4]
# where access weights [ezone] = coeff * childhh [ezone] ^ exponent
#
# Input Parameters:
# nonres_traveltime_in = nonres travel times 
# nonres_vintage_emp_in = employment from previous model year
# res_currentyear_hh_in = households by ezone
# access_param_in = emp, emplcass emp, and hh access parameters
# access_time_param_in = travel time access parameters
# 
# Calls:
# NONE
#
# Returns:
# combined_access_weights = total of access weight components
        
    nonres_traveltime <- t( array(nonres_traveltime_in[,3],c(numEzones,numEzones)) )  # [ezone1 x ezone2]
    
    #debug
    #nonres_traveltime[nonres_traveltime < 5] <- 5
    
            
    nonres_access <- array(0,c(numEzones,numEmpclass))
        
    totalhh_ezone <- res_currentyear_hh_in$totalhh
    childhh_ezone <- res_currentyear_hh_in$childhh
    
    empclass_emp_ezone <- nonres_vintage_emp_in[,2:(numEmpclass+1)]
    totalemp_ezone <- rowSums(empclass_emp_ezone)
   
    access_param_totalemp <- access_param_in$totalemp  #[empclass]
    access_param_totalemp <- array(access_param_totalemp, c(numEmpclass, numEzones,  numRE))  #[empclass x ezone x re]
    access_param_totalemp <- aperm(access_param_totalemp, c(2,1,3))
    
    access_param_totalhh <- access_param_in$totalhh
    access_param_totalhh <- array(access_param_totalhh, c(numEmpclass, numEzones,  numRE))  #[empclass x ezone x re]
    access_param_totalhh <- aperm(access_param_totalhh, c(2,1,3))
      
    access_param_empclassemp <- access_param_in$empclassemp
    access_param_empclassemp <- array(access_param_empclassemp, c(numEmpclass, numEzones,  numRE))  #[empclass x ezone x re]
    access_param_empclassemp <- aperm(access_param_empclassemp, c(2,1,3))


    access_time <- access_time_param_in$time       #[empclass]
    access_timesq <- access_time_param_in$timesq   #[empclass] 
    
    # first two table rows are emp,HH   flag which empclass is the "child"
    empclassChild <- grep(TRUE,access_time_param_in$childhh_coeff != 0) - 2
    
    childhh_coeff <- access_time_param_in$childhh_coeff[empclassChild + 2]  
    childhh_exp <- access_time_param_in$childhh_exp[empclassChild + 2]
    
    #  access matrix = base year total employment [ezone] / 
    #  (travel time [ezone1 x ezone2] * time param + travel time ^2 [ezone1 x ezone2] * timesq param
    #
    #  access weights [ezone]  = normalized row sums of access matrix
    
    
    totalemp_ezone <- t(array(totalemp_ezone,c(numEzones,numEzones)))  # [ezone1 x ezone2]
    access_weights_totalemp_matrix <-  
            totalemp_ezone / (nonres_traveltime * access_time[1] + nonres_traveltime^2 * access_timesq[1])
            
    access_weights_totalemp <- rowSums(access_weights_totalemp_matrix) / sum(access_weights_totalemp_matrix )
    
    
    ### calc access matrix -- households
    #
    #  access matrix = base year total households [ezone] / 
    #  (travel time [ezone1 x ezone2] * time param + travel time ^2 [ezone1 x ezone2] * timesq param
    #
    #  access weights [ezone]  = normalized row sums of access matrix
    
    totalhh_ezone <- t(array(totalhh_ezone,c(numEzones,numEzones)))  # [ezone1 x ezone2]
    access_weights_totalhh_matrix <-  
            totalhh_ezone / (nonres_traveltime * access_time[2] + nonres_traveltime^2 * access_timesq[2])
            
    access_weights_totalhh <- rowSums(access_weights_totalhh_matrix) / sum(access_weights_totalhh_matrix )
    
    
    ### calc access matrix -- empclass  (Gov-Edu empclass written with dummy values, to be overwritten below)
    #  access matrix = empclass emp [ezone] / 
    #  (travel time [ezone1 x ezone2] * time param + travel time ^2 [ezone1 x ezone2] * timesq param
    #
    #  access weights [ezone]  = normalized row sums of access matrix
    #
    #
    # ... except for Government- Education access weights
    # access weights [ezone] = coeff * childhh [ezone] ^ exponent
    
    
    access_weights_empclassemp <- array(0,c(numEzones,numEmpclass))
    
    for (empclass in 1:numEmpclass){
    
        if (empclass != empclassChild) {


            empclass_emp <- empclass_emp_ezone[,empclass]
            empclass_emp <- t(array(empclass_emp,c(numEzones,numEzones))) # [ezone1 x ezone2]
            access_weights_matrix <-  
                empclass_emp / (nonres_traveltime * access_time[empclass+2] + nonres_traveltime^2 * access_timesq[empclass+2])
                
            access_weights_empclassemp[,empclass] <- rowSums(access_weights_matrix) / sum(access_weights_matrix )
            
            #if(empclass ==5){browser()}
        
        }  else  {
        
             # weights = a * childHH ^ b
            access_weights_vector <- childhh_coeff * childhh_ezone ^ childhh_exp
            access_weights_empclassemp[,empclass] <- access_weights_vector / sum(access_weights_vector)
            
        }
    }


    access_weights_totalemp <- array(access_weights_totalemp, c(numEzones, numEmpclass, numRE))  #[ezone x empclass x re]
    
    access_weights_totalhh <- array(access_weights_totalhh, c(numEzones, numEmpclass, numRE))  #[ezone x empclass x re]
    
    access_weights_empclassemp <- array(access_weights_empclassemp, c(numEzones, numEmpclass, numRE))  #[ezone x empclass x re]
    
    combined_access_weights <-  (access_weights_totalemp * access_param_totalemp + access_weights_totalhh * access_param_totalhh + 
                                 access_weights_empclassemp * access_param_empclassemp)

    return(combined_access_weights)


}

