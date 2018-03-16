##=======================================================================================
## Pareto law functions in R
## Copyright (C) 2015-2017  Anton Antonov
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
library(ggplot2)

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

#' @description Make a plot that demonstrates the adherence to the Pareto law of a numerical vector
#' with contingency values (counts).
#' @param dataDF a two column data frame with the first column of a variable name and second column a numerical vector c("Name", "Value")
#' @param plotTitle a string for the title of the plot
ParetoLawGGPlot <- function( dataDF, main = NULL, scales = "free_x", ...  ) {
  qdf <- 
    ddply( dataDF, c("Name"), function(x) {
      dataVec <- rev( sort(x$Value) )
      dataVec <- cumsum( dataVec ) / sum( dataVec )
      pres <- data.frame( Name = x$Name[[1]], Index = 1:nrow(x), ParetoFraction = dataVec )
      qdf <- cbind( pres,
                    p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )
    })

  
  ggplot(qdf) +
    geom_line( aes( x = Index, y = ParetoFraction) ) +
    geom_vline( aes( xintercept = p10), linetype = 3 ) +
    geom_vline( aes( xintercept = p20), linetype = 3 ) +
    geom_vline( aes( xintercept = p30), linetype = 3 ) +
    geom_vline( aes( xintercept = p40), linetype = 3 ) +
    geom_vline( aes( xintercept = p50), linetype = 3 ) +
    facet_wrap( ~ Name, scales = scales ) +
    theme( ... )
}

#' @description Sorts the tally of given categorical data descendingly and computes the list of the cumulative sums.
#' @param dataVec a vector with categorical entries
#' @param weightVec a vector of weights for the values of dataVec; see the argument wt_var of plyr::count
ParetoLawData <- function( dataVec, weightsVec = NULL ) {
  if ( is.null(weightsVec) ) {
    dTally <- plyr::count( dataVec ) 
  } else {
    weightsVec[ is.na(weightsVec) ] <- 0
    dTally <- plyr::count( df = data.frame( dataVec = dataVec, weightsVec = weightsVec ), vars = "dataVec", wt_var = "weightsVec" )
    dTally <- dTally[ !is.na( dTally[[1]] ), ]
  }
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

#' @description Apply the ParetoLawData function over a list of names and make a multi-panel ggplot.
#' @param data a data frame
#' @param colNames a list of column names corresponding to categrical columns in \param data
#' @param ... arguments for ggplot2::facet_wrap .
ParetoLawPlotForColumns <- function( data, colNames = colnames(data), weights = NULL, ... ) {
  
  if( is.null(weights) ) {
    weightsVec <- NULL
  } else if ( is.character(weights) && ( weights %in% colnames(data) ) ) {   
    weightsVec <- data[[weights]] 
  } else if ( is.numeric(weights) && length(weights) == nrow(data) ) {
    weightsVec <- weights
  } else {
    stop( "The argument weights is expected to be NULL, a column name of the argument data, or a numerical vector with length equal to nrow(data).", call. = T)
  }
  
  qdf <-
    ldply( colNames, function(cn) {
      pres <- ParetoLawData( data[[cn]], weightsVec = weightsVec )
      cbind( ColName = cn, Index = 1:nrow(pres), pres,
             p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )
    }, .progress = "none" )
  
  ggplot(qdf) +
    geom_line( aes( x = Index, y = ParetoFraction) ) +
    geom_vline( aes( xintercept = p10), linetype = 3 ) +
    geom_vline( aes( xintercept = p20), linetype = 3 ) +
    geom_vline( aes( xintercept = p30), linetype = 3 ) +
    geom_vline( aes( xintercept = p40), linetype = 3 ) +
    geom_vline( aes( xintercept = p50), linetype = 3 ) +
    facet_wrap( ~ ColName, ... )
}
