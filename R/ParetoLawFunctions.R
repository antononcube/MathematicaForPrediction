##=======================================================================================
## Pareto law functions in R
## Copyright (C) 2015-2016  Anton Antonov
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
## antononcube @ gmail . com,
## Windermere, Florida, USA.
##
##=======================================================================================
## 
## This R script has function definitions for Pareto law adherence verification and analysis.
## Date started: April, 2014
## Updated February 2015, March, 2015, January 2016
##=======================================================================================

library(plyr)

#' @description Make a plot that demonstrates the adherence to the Pareto law of the unique entries
#' of a categorical vector.
#' @param dataVec a vector with categorical entries
#' @param plotTitle a string for the title of the plot
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
ParetoLawForCountsPlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", ... ) {
    taCounts <- plyr::count(dataVec)  
    taCounts <- taCounts[rev(order(taCounts$freq)),]
    ParetoLawPlot( taCounts$freq, main, xlab=xlab, ylab=ylab, ...)
}

#' @description Make a plot that demonstrates the adherence to the Pareto law of a numerical vector
#' with contingency values (counts).
#' @param dataVec a numerical vector 
#' @param plotTitle a string for the title of the plot
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
ParetoLawPlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", xFraction = 0.2, yFraction = 0.8, ... ) {
    dataVec <- rev( sort(dataVec) )
    plot( cumsum( dataVec ) / sum( dataVec ), type='l', main=main, xlab=xlab, ylab=ylab, ... )
    grid()
    ## grid(nx=4,ny=4)
    abline( v=c(xFraction*length(dataVec)), h=c(yFraction), col="red", lty="dotted" )
}

#' @description Sorts the tally of given categorical data descendingly and computes the list of the cumulative sums.
#' @param dataVec a vector with categorical entries
ParetoLawData <- function( dataVec ) {
  dTally <- plyr::count( dataVec )
  if( class(dataVec) == "character" ) { dTally[[1]] <- as.character(dTally[[1]]) }  
  dTally <- dTally[ rev(order(dTally[,c(2)])), ]
  cumSums <- cumsum(dTally[,c(2)])/sum(dTally[,c(2)])
  data.frame(cbind( dTally[1], ParetoFraction = cumSums ))
}

#' @description Apply the ParetoLawData function over a list of names.
#' @param data a data frame 
#' @param colNames a list of column names corresponding to categrical columns in \param data
ParetoLawDataForColumns <- function( data, colNames ) {
  llply(colNames, function(c) {t<-ParetoLawData(data[[c]]); cbind(1:length(t[,1]), t[,2])})
}


#' @description Finds the Pareto items in a column of a data frame.
#' @param data a data frame
#' @param colName the column name of items to be counted
#' @param paretoFraction a number between 0 and 1 specifying the Pareto fraction
#' @return A data frame with columns c( "Item", "Score", "CumSums" ).
ParetoItems <- function( data, colName, paretoFraction ) {

  paretoItemsCount <- plyr::count( data[colName] )
  paretoItemsCount[[1]] <- as.character( paretoItemsCount[[1]] )

  paretoItemsCount <- paretoItemsCount[ order( -paretoItemsCount[,2] ), ]
  cumSums <- cumsum( paretoItemsCount[,2] ) / sum( paretoItemsCount[,2] )
  paretoItemsCount <- cbind( paretoItemsCount, cumSums = cumSums, stringsAsFactors = FALSE )

  paretoItems <- paretoItemsCount[[1]][ paretoItemsCount$cumSums <= paretoFraction ]

  paretoItemsCount <- paretoItemsCount[ paretoItemsCount[[1]] %in% paretoItems, ]
  paretoItemsCount <- paretoItemsCount[ order(- paretoItemsCount$freq), ]
  names(paretoItemsCount) <- c( "Item", "Score", "CumSums" )

  paretoItemsCount
}


#' @description Finds the Pareto items in a column of a data frame.
#' @param dataVec a data vector
#' @param paretoFraction a number between 0 and 1 specifying the Pareto fraction
#' @return A data frame with columns c( "Index", "Score", "CumSums" ).
#' @details "CumSums" should be "CumSumFractions" but it is left to be "CumSums" for backward compatibility.
ParetoPositions <- function( dataVec, paretoFraction = 1 ) {

  paretoItemsCount <- data.frame( Index = 1:length(dataVec), Score = dataVec )
  paretoItemsCount <- paretoItemsCount[ rev( order( paretoItemsCount[,2] ) ), ]
  cumSums <- cumsum( paretoItemsCount[,2] ) / sum( paretoItemsCount[,2] )
  paretoItemsCount <- cbind( paretoItemsCount, CumSums = cumSums )

  paretoItems <- paretoItemsCount[[1]][ paretoItemsCount$CumSums <= paretoFraction ]

  paretoItemsCount <- paretoItemsCount[ paretoItemsCount[[1]] %in% paretoItems, ]
  paretoItemsCount <- paretoItemsCount[ order( -paretoItemsCount$Score ), ]
  names(paretoItemsCount) <- c( "Index", "Score", "CumSums" )

  paretoItemsCount
}
