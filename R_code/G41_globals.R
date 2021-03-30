# G40_globals.R
# MetroScope global constants
# maintained by Jim Cser jim.cser@oregonmetro.gov
# last updated 10/14/2013

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


##### global constants

# metroscope version
metroscopeVersion <- "3.4.0"

# residential market segments (KHIA): Child, HH Size, Income, Age of HH Head
numK <- 2  
numH <- 5
numI <- 8
numA <- 5
numKHIA <- numK * numH * numI * numA

# residential zones	 
numRzones <- 494
# residential zoning classes
numZclass <- 34
# residential housing types
numHtypes <- 4
# residential housing bins
numBins <- 8
# residential bin breaks (eighths from 0 to 1)
binBreaks <- c(-0.01, 0.125, 0.250, 0.375, 0.500, 0.625, 0.75, 0.875, 1.01)

# nonres employment zones
numEzones <- 72
# nonres employment classes
numEmpclass <- 14
# nonres real estate types
numRE <- 8    
# nonres floor-area-ratio (FAR) classes
numFAR <- 8


##### indicies for operation on arrays and labeling output tables
# rzones, ezones
idxRz <- 1:numRzones
idxEz <- 1:numEzones

# rzones * zclass
idxRzZc_rz <- rep(1:numRzones,each=numZclass)   # 11111 22222 33333
idxRzZc_zc <- rep(1:numZclass,times=numRzones)   # 12345 12345 12345
idxRzZc <- cbind(idxRzZc_rz,idxRzZc_zc)

# rzones * KHIA
idxRzKHIA_rz <- rep(1:numRzones,each=numKHIA)   # 11111 22222 33333
idxRzKHIA_khia <- rep(1:numKHIA,times=numRzones)   # 12345 12345 12345
idxRzKHIA <- cbind(idxRzKHIA_rz,idxRzKHIA_khia)

# rzones * bin
idxRzBin_rz <- rep(1:numRzones,each=numBins)   # 11111 22222 33333
idxRzBin_bin <- rep(1:numBins,times=numRzones)   # 12345 12345 12345
idxRzBin <- cbind(idxRzBin_rz,idxRzBin_bin)

# rzones * ezones
idxRzEz_rz <- rep(1:numRzones,each=numEzones)   # 11111 22222 33333
idxRzEz_ez <- rep(1:numEzones,times=numRzones)   # 12345 12345 12345
idxRzEz <- cbind(idxRzEz_rz,idxRzEz_ez)


# ezones * empclass
idxEzEc_ez <- rep(1:numEzones,each=numEmpclass)   # 11111 22222 33333
idxEzEc_ec <- rep(1:numEmpclass,times=numEzones)   # 12345 12345 12345
idxEzEc <- cbind(idxEzEc_ez, idxEzEc_ec)

# ezones * FAR
idxEzFAR_ez <- rep(1:numEzones,each=numFAR)   # 11111 22222 33333
idxEzFAR_far <- rep(1:numFAR,times=numEzones)   # 12345 12345 12345
idxEzFAR <- cbind(idxEzFAR_ez, idxEzFAR_far)


# travel times
idxEzEz_from <- rep(1:numEzones,each = numEzones)
idxEzEz_to <- rep(1:numEzones,times = numEzones)

idxEzRz_from <- rep(1:numEzones,each = numRzones)
idxEzRz_to <- rep(1:numRzones,times = numEzones)
