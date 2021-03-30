# G40_IO_functions.R
# MetroScope Input/Output functions
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

## functions03  now with sumsq!


catLine <- function(myString) {

	cat( paste(myString, "\n"))
}

readTable <- function(scenarioID, relPath, tableName) {
# readTable()
# Reads table from CSV format file
# 
# Input Parameters:
# scenarioID = scenario ID number
# relPath = path relative to home directory
# tableName = name of CSV file
#
# Calls:
# NONE
#
# Returns:
# R data frame


   #table_out <- read.csv( paste("scen",scenarioID,"_data/",relPath,"/",tableName,".csv",sep="") )   

   # remove header rows beginning with "#"
   table_in <- paste("scen",scenarioID,"_data/",relPath,"/",tableName,".csv",sep="")
   table_out <- read.table(file=table_in, sep=",", header=T, comment.char = "#", blank.lines.skip = TRUE)
   return(table_out)

}

writeTable <- function(scenarioID, dataArray, relPath, tableName) {
# writeTable()
# Writes array to CSV format file
# 
# Input Parameters:
# scenarioID = scenario ID number
# dataArray = array to be written
# relPath = path relative to home directory
# tableName = name of CSV file
#
# Calls:
# NONE
#
# Returns:
# NONE

   write.csv(dataArray, paste("scen",scenarioID,"_data/",relPath,"/",tableName,".csv",sep=""),row.names=F )
}



updateResDemandData_yearN <- function(resDemandResults){ 
# updateResDemandData_yearN() 
# Writes residential demand module arrays to temporary directory
#
# Input Parameters:
# resDemandResults
#     $avgHouseSizeBin = average house size, by housing bin
#     $avgLotSizeBin = average lot size, by housing bin
#     $avgHedonicBin = average hedonic price, by housing bin
#
# Calls:
# writeTable()
#
# Returns:
# NONE
   
    writeTable(scenarioID,resDemandResults$avgHouseSizeBin,"shared_res",paste("avgHouseSizeBin_year",currentYear,sep=""))
    writeTable(scenarioID,resDemandResults$avgLotSizeBin,"shared_res",paste("avgLotSizeBin_year",currentYear,sep=""))
    writeTable(scenarioID,resDemandResults$avgHedonicBin,"shared_res",paste("avgHedonicBin_year",currentYear,sep=""))

}

updateNonresDemandData_yearN <- function(nonresDemandResults){

# updateNonresDemandData_yearN()
# Writes non-residential demand module arrays to temporary directory  
# 
# Input Parameters:
# nonresDemandResults 
#     $sqftDemandEzRe = sqft demand
#
# Calls:
# writeTable()
#
# Returns:
# NONE
      
    writeTable(scenarioID,cbind(idxEz,nonresDemandResults$sqftDemandEzRe),"shared_nonres",paste("nonres_demand_year",currentYear,sep=""))

}


outputResData_yearN <- function(resDemandResults,resSupplyResults,resLocpriceResults) {
# outputResData_yearN()
# Writes outputs of residential module 
# 
# Input Parameters:
# resDemandResults
#     $res_demand_Rz = DU demand by rzone
#     $res_demand_KHIARz = DU demand by rzone, KHIA
#     $res_demand_EzRz = DU demand by ezone, rzone
#     $res_demand_RzBin = DU demand by ezone, housing bin
#     $res_demand_Ez = DU demand by ezone
#     $avgHouseSizeBin = average house size, by housing bin
#     $avgLotSizeBin = average lot size, by housing bin
#     $avgHedonicBin = average hedonic price, by housing bin
#     $res_demand_binshares = DU demand shares by housing bin
# resSupplyResults
#     $newSupply = new regular DU supply increment
#     $newSupplyUR = new UR DU supply increment
#     $res_supply_Rz = total DU supply
#     $acresConsumed = new regular acres consumed
#     $acresConsumedUR = new UR acres consumed
#     $acresRemaining = total regular acres remaining
#     $acresRemainingUR = total UR acres remaining
# resLocpriceResults
#     $res_locationprice_new = updated location price
#
# Calls:
# writeTable()
#
# Returns:
# NONE
          
    outputDir <- paste("outputs_year",currentYear,sep="")
        
    ##### demand outputs

    
    table_out <- cbind(idxRz,resDemandResults$res_demand_Rz)
    colnames(table_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_rzone_year",currentYear,sep=""))
    
    
    
    array_in <- resDemandResults$res_demand_KHIARz
    array_in <- aperm(array_in,c(2,3,1))
    dim(array_in) <- c(numRzones*numKHIA,numHtypes)
    table_out <- cbind(idxRzKHIA,array_in)
    colnames(table_out) <- c("rzone","khianum","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_RzKHIA_year",currentYear,sep=""))
        
    array_in <- resDemandResults$res_demand_EzRz
    dim(array_in) <- c(numRzones*numEzones,numHtypes)
    table_out <- cbind(idxRzEz,array_in)
    colnames(table_out) <- c("rzone","ezone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_RzEz_year",currentYear,sep=""))
    
    array_in <- resDemandResults$res_demand_RzBin
    dim(array_in) <- c(numRzones*numBins,numHtypes)
    table_out <- cbind(idxRzBin,array_in)
    colnames(table_out) <- c("rzone","bin","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_RzBin_year",currentYear,sep=""))

    # for nonresidential model
    table_out <- cbind(idxEz,resDemandResults$res_demand_Ez,resDemandResults$res_demand_EzChild)
    colnames(table_out) <- c("ezone","totalhh","childhh")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_demand_hh_year",currentYear,sep=""))
        
    array_in <- resDemandResults$avgHouseSizeBin
    dim(array_in) <- c(numRzones*numBins,numHtypes)
    table_out <- cbind(idxRzBin,array_in)
    colnames(table_out) <- c("rzone","bin","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_avghousesize_bin_year",currentYear,sep=""))
    
    array_in <- resDemandResults$avgLotSizeBin
    dim(array_in) <- c(numRzones*numBins,numHtypes)
    table_out <- cbind(idxRzBin,array_in)
    colnames(table_out) <- c("rzone","bin","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_avglotsize_bin_year",currentYear,sep=""))
    
    array_in <- resDemandResults$avgHedonicBin
    dim(array_in) <- c(numRzones*numBins,numHtypes)
    table_out <- cbind(idxRzBin,array_in)
    colnames(table_out) <- c("rzone","bin","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_avghedonic_bin_year",currentYear,sep=""))
       
    array_in <- resDemandResults$res_demand_binshares
    dim(array_in) <- c(numRzones*numBins,numHtypes)
    table_out <- cbind(idxRzBin,array_in)
    colnames(table_out) <- c("rzone","bin","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_binshares_year",currentYear,sep=""))
     
    ##### supply outputs

    table_out <- cbind(idxRz,resSupplyResults$newSupply)
    colnames(table_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_newsupply_year",currentYear,sep=""))
    
    table_out <- cbind(idxRz,resSupplyResults$newSupplyUR)
    colnames(table_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_newsupplyUR_year",currentYear,sep=""))
        
    table_out <- cbind(idxRz,resSupplyResults$res_supply_Rz)
    colnames(table_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_totalsupply_year",currentYear,sep=""))
           
    table_out <- cbind(idxRzZc,resSupplyResults$acresConsumed,resSupplyResults$acresConsumedUR)
    colnames(table_out) <- c("rzone","zclass","osf","omf","rsf","rmf","osfUR","omfUR","rsfUR","rmfUR")
    writeTable(scenarioID,table_out,outputDir,paste("res_acres_consumed_year",currentYear,sep=""))
    
    table_out <- cbind(idxRzZc,resSupplyResults$acresRemaining,resSupplyResults$acresRemainingUR)
    colnames(table_out) <- c("rzone","zclass","osf","omf","rsf","rmf","osfUR","omfUR","rsfUR","rmfUR")
    writeTable(scenarioID,table_out,outputDir,paste("res_acres_remaining_year",currentYear,sep=""))
    
    ## added 2014-08-20
    table_out <- cbind(idxRzZc,resSupplyResults$newSupplyRzZc,resSupplyResults$newSupplyURRzZc)
    colnames(table_out) <- c("rzone","zclass","osf","omf","rsf","rmf","osfUR","omfUR","rsfUR","rmfUR")
    writeTable(scenarioID,table_out,outputDir,paste("res_newsupply_RzZc_year",currentYear,sep=""))

    # Add supply by rz bin JBC  2015-05-22 4:17:14 PM
    table_out <- cbind(idxRzBin,resSupplyResults$newSupplyRzBin,resSupplyResults$newSupplyURRzBin)
    colnames(table_out) <- c("rzone","bin","osf","omf","rsf","rmf","osfUR","omfUR","rsfUR","rmfUR")
    writeTable(scenarioID,table_out,outputDir,paste("res_newsupply_RzBin_year",currentYear,sep=""))



    ##### location price
    
    table_out <- cbind(idxRz,resLocpriceResults$res_locationprice_new)
    colnames(table_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_locationprice_year",currentYear,sep=""))
    
    table_out <- cbind(idxRz,resLocpriceResults$sumSq)
    colnames(table_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_sumsq_year",currentYear,sep=""))
    

    calib_price_in <- readTable(scenarioID,"outputs_year0","res_locationprice_year0")
    calib_price <- calib_price_in[,2:5]
    supply_price <- resLocpriceResults$res_locationprice_new / calib_price
    table_out <- cbind(idxRz, supply_price)
    colnames(table_out) <- c("rzone","osf","omf","rsf","rmf")
    writeTable(scenarioID,table_out,outputDir,paste("res_supplyprice_year",currentYear,sep=""))


}


outputNonresData_yearN <- function(nonresDemandResults,nonresSupplyResults,nonresLocpriceResults) {
# outputResData_yearN()
# Writes outputs of residential module 
# 
# Input Parameters:
# nonresDemandResults
#     $sqftDemandcalc = total sqft demand
#     $empDemand_calc = total employment demand by ezone, emplcass, retype
#     $empDemandEzEc = total employment demand by ezone, emplcass
#     $empDemandEz = total employment demand by ezone, emplcass
# nonresSupplyResults
#     $sqftNewSupply = new regular sqft supply increment by ezone, emplcass, retype
#     $sqftNewSupply_UR = new UR sqft supply increment by ezone, emplcass, retype
#     $sqftNewSupplyEzRe = new regular sqft supply increment by ezone, retype
#     $sqftNewSupplyEzRe_UR = new regular sqft supply increment by ezone, retype
#     $sqftVintageSupplyEzRe = total sqft supply from previous model year
#     $acresConsumed = new regular acres consumed
#     $acresConsumedUR = new UR acres consumed
#     $acresRemaining = total regular acres remaining
#     $acresRemainingUR = total UR acres remaining
# nonresLocpriceResults
#     $nonres_locationprice_new = updated location price
#
# Calls:
# writeTable()
#
# Returns:
# NONE
    
    outputDir <- paste("outputs_year",currentYear,sep="")
    
    ##### demand results    
    
    table_out <- cbind(idxEzEc,nonresDemandResults$sqftDemandcalc)
    colnames(table_out) <- c("ezone","empclass","man","war","flex","ret","gen","med","resland","resnoland")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_demand_sqft_year",currentYear,sep=""))
    
    table_out <- cbind(idxEzEc,nonresDemandResults$empDemand_calc)
    colnames(table_out) <- c("ezone","empclass","man","war","flex","ret","gen","med","resland","resnoland")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_demand_emp_year",currentYear,sep=""))
    
    table_out <- cbind(idxEz,nonresDemandResults$empDemandEzEc)
    colnames(table_out) <- c("ezone",paste("empclass",1:numEmpclass,sep=""))
    writeTable(scenarioID,table_out,outputDir,paste("nonres_demand_empclass_emp_year",currentYear,sep=""))
    
    
    # for residential model
    #### TODO  fix idxEz
    table_out <- cbind(idxEz,nonresDemandResults$empDemandEz)
    colnames(table_out) <- c("ezone","emp")
    writeTable(scenarioID,table_out,outputDir,paste("res_demand_emp_year",currentYear,sep=""))
   
    
    #print("nonres demand   OK")
    ##### supply results    
    
    array_in <- nonresSupplyResults$sqft_newsupply_reg
    table_out <- cbind(idxEz,array_in)
    colnames(table_out) <- c("ezone","man","war","flex","ret","gen","med","resland","resnoland")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_sqft_newsupply_reg_year",currentYear,sep=""))
    #print("nonres_sqft_newsupply_reg_year   OK")    
        
    array_in <- nonresSupplyResults$sqft_newsupply_ur
    table_out <- cbind(idxEz,array_in)
    colnames(table_out) <- c("ezone","man","war","flex","ret","gen","med","resland","resnoland")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_sqft_newsupply_ur_year",currentYear,sep=""))
    #print("nonres_sqft_newsupply_ur_year   OK")    
    
    table_out <- cbind(idxEzFAR,nonresSupplyResults$nonres_acresconsumed_reg,nonresSupplyResults$nonres_acresconsumed_ur)
    colnames(table_out) <- c("ezone","far","ind","com","res","indUR","comUR","resUR")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_acres_consumed_year",currentYear,sep=""))
    #print("nonres_acres_consumed_year   OK")    
       
    table_out <- cbind(idxEzFAR,nonresSupplyResults$nonres_acresremaining_reg,nonresSupplyResults$nonres_acresremaining_ur)
    colnames(table_out) <- c("ezone","far","ind","com","res","indUR","comUR","resUR")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_acres_remaining_year",currentYear,sep=""))
    #print("nonres_acres_remaining_year   OK")  
            
    newRegSupply <- nonresSupplyResults$sqft_newsupply_reg
    newURSupply <- nonresSupplyResults$sqft_newsupply_ur
    vintageSupply <- nonresSupplyResults$sqft_vintagesupply
    totalSupply <- vintageSupply + newRegSupply + newURSupply
    
    
 
    table_out <- cbind(idxEz,totalSupply)
    colnames(table_out) <- c("ezone","man","war","flex","ret","gen","med","resland","resnoland")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_supply_totalsupply_year",currentYear,sep=""))
    #print("nonres_supply_totalsupply_year   OK")    
    
    ##### location price
       
    table_out <- cbind(idxEz,nonresLocpriceResults$nonres_locationprice_new)
    colnames(table_out) <- c("ezone","man","war","flex","ret","gen","med","resland","resnoland")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_locationprice_year",currentYear,sep=""))
    #print("nonres_locationprice_year   OK")    
    

    calib_price_in <- readTable(scenarioID,"outputs_year0","nonres_locationprice_year0")
    calib_price <- calib_price_in[,2:9]
    supply_price <- nonresLocpriceResults$nonres_locationprice_new / calib_price
    table_out <- cbind(idxEz, supply_price)
    colnames(table_out) <- c("ezone","man","war","flex","ret","gen","med","resland","resnoland")
    writeTable(scenarioID,table_out,outputDir,paste("nonres_supplyprice_year",currentYear,sep=""))


}


