##=======================================================================================
## Implementation of one dimensional outlier identifying algorithms in R
## Copyright (C) 2013  Anton Antonov
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.
## 
## Written by Anton Antonov, 
## antononcube@gmail.com,
## Windermere, Florida, USA.
##
##=======================================================================================
##
## This script of R functions re-implements this Mathematica package:
##
## [1] Anton Antonov,  Implementation of one dimensional outlier identifying algorithms in Mathematica,
##     Mathematica package OutlierIdentifiers.m, (2013), MathematicaForPrediction project at GitHub,
##     https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m .
##  
## It was easier for me to implement in R the one-dimensional outlier detection functions
## in [1] than to comprehend the signatures of the R different libraries.
##=======================================================================================

#' @description Find an Hampel outlier threshold for a data vector
#' @param dataVec data vector
HampelIdentifierParameters <- function( dataVec ) {
  x0 <- median(dataVec)
  md <- 1.4826 * median(abs(dataVec - x0));
  c(x0 - md, x0 + md)
}

#' @description Find an Quartile outlier for a data vector
#' @param dataVec dataVec vector
QuartileIdentifierParameters <- function( dataVec ) {
  res <- quantile( dataVec, c( 1/4, 1/2, 3/4 ) )
  xL <- res[[1]]
  x0 <- res[[2]]
  xU <- res[[3]]
  c( x0 - (xU - xL), x0 + (xU - xL) )
}


#' @description Find an SPLUS Quartile outlier for a data vector
#' @param dataVec dataVec vector
SPLUSQuartileIdentifierParameters <- function( dataVec ) {
  if ( length(dataVec) <=4 ) {
    xL <- min(dataVec)
    xU <- max(dataVec)
  } else {
    res <- quantile( dataVec, c( 1/4, 3/4 ) )
    xL <- res[[1]]
    xU <- res[[2]]
  }
  c( xL - 1.5*(xU-xL), xU + 1.5*(xU-xL) )
}


#' @description Find an outlier threshold for a data vector
#' @param dataVec data vector
#' @param lowerAndUpperThresholds outlier identifier parameters
OutlierIdentifier <- function( dataVec, lowerAndUpperThresholds ) {
  dataVec[ dataVec <= lowerAndUpperThresholds[[1]] | dataVec >= lowerAndUpperThresholds[[2]] ]
}

#' @description Find the top outliers for a data vector
#' @param dataVec data vector
#' @param lowerAndUpperThresholds outlier identifier parameters
TopOutlierIdentifier <- function( dataVec, lowerAndUpperThresholds ) {
  dataVec[dataVec >= lowerAndUpperThresholds[[2]] ]
}

#' @description Find the bottom outliers for a data vector
#' @param dataVec data vector
#' @param lowerAndUpperThresholds outlier identifier parameters
BottomOutlierIdentifier <- function( dataVec, lowerAndUpperThresholds ) {
  dataVec[dataVec <= lowerAndUpperThresholds[[1]] ]
}

#' @description Find the outlier positions in a data vector
#' @param dataVec data vector
#' @param outlierIdentifier outlier identifier function
OutlierPosition <- function( dataVec, outlierIdentifier = HampelIdentifierParameters ) {
  cls <- outlierIdentifier(dataVec)
  which( dataVec <= cls[[1]] | dataVec >= cls[[2]] )
}

#' @description Find the top outlier positions in a data vector
#' @param dataVec data vector
#' @param outlierIdentifier outlier identifier function
TopOutlierPosition <- function( dataVec, outlierIdentifier = HampelIdentifierParameters ) {
  cls <- outlierIdentifier(dataVec)
  which( dataVec >= cls[[2]] )
}

#' @description Find the bottom outlier positions in a data vector
#' @param dataVec data vector
#' @param outlierIdentifier outlier identifier function
BottomOutlierPosition <- function( dataVec, outlierIdentifier = HampelIdentifierParameters ) {
  cls <- outlierIdentifier(dataVec)
  which( dataVec <= cls[[1]] )
}



