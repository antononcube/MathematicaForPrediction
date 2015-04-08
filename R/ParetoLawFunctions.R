##=======================================================================================
## Pareto law functions in R
## Copyright (C) 2015  Anton Antonov
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
## 7320 Colbury Ave, 
## Windermere, Florida, USA.
##
##=======================================================================================
## 
## This R script has function definitions for Pareto law adherence verification and analysis.
## Date started: April, 2014
## Updated February, March, 2015
##=======================================================================================

#' @description Make a plot that demonstrates the adherence to the Pareto law of the unique entries of a categorical vector.
#' @param dataVec a vector with categorical entries
#' @param plotTitle a string for the title of the plot
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
ParetoLawForCountsPlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", ... ) {
    taCounts <- count(dataVec)  
    taCounts <- taCounts[rev(order(taCounts$freq)),]
    ParetoLawPlot( taCounts$freq, main, xlab=xlab, ylab=ylab, ...)
}

#' @description Make a plot that demonstrates the adherence to the Pareto law of the unique entries of a categorical vector.
#' @param dataVec a numerical vector 
#' @param plotTitle a string for the title of the plot
#' @param xlab label for the x-axis
#' @param ylab label for the y-axis
ParetoLawPlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", ... ) {
    plot( cumsum( dataVec ) / sum( dataVec ), type='l', main=main, xlab=xlab, ylab=ylab, ... )
    grid()
    ## grid(nx=4,ny=4)
    abline( v=c(0.2*length(dataVec)), h=c(0.8), col="red", lty="dotted" )
}

#' @description Sorts the tally of given categorical data descendingly and computes the list of the cumulative sums.
#' @param dataVec a vector with categorical entries
#' @param plotTitle a string for the title of the plot
ParetoLawData <- function( dataVec ) {
  dTally <- count( dataVec )
  dTally <- dTally[ rev(order(dTally[,c(2)])), ]
  cumSums <- cumsum(dTally[,c(2)])/sum(dTally[,c(2)])
  data.frame(cbind( dTally[1], cumSums ))
}

#' @description Apply the ParetoLawData function over a list of names.
#' @param data a data frame 
#' @param colNames a list of column names corresponding to categrical columns in \param data
ParetoLawDataForColumns <- function( data, colNames ) {
  llply(colNames, function(c) {t<-ParetoLawData(data[[c]]); cbind(1:length(t[,1]), t[,2])})
}

