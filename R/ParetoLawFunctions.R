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

library(dplyr)
library(purrr)
library(ggplot2)

#' @description Make a plot that demonstrates the adherence to the Pareto law 
#' of the unique entries of a categorical vector.
#' @param dataVec A vector with categorical entries.
#' @param plotTitle A string for the title of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
ParetoLawForCountsPlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", ... ) {
  taCounts <- plyr::count(dataVec)
  taCounts <- taCounts[rev(order(taCounts$freq)),]
  ParetoLawPlot( taCounts$freq, main, xlab=xlab, ylab=ylab, ...)
}

#' @description Make a plot that demonstrates the adherence to the Pareto law of 
#' a numerical vector with contingency values (counts).
#' @param dataVec A numerical vector.
#' @param plotTitle A string for the title of the plot.
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
ParetoLawPlot <- function( dataVec, main = NULL, xlab="|levels|", ylab="%", xFraction = 0.2, yFraction = 0.8, ... ) {
  dataVec <- rev( sort(dataVec) )
  plot( cumsum( dataVec ) / sum( dataVec ), type='l', main=main, xlab=xlab, ylab=ylab, ... )
  grid()
  ## grid(nx=4,ny=4)
  abline( v=c(xFraction*length(dataVec)), h=c(yFraction), col="red", lty="dotted" )
}

#' @description Make a plot that demonstrates the adherence to the Pareto law of a 
#' a data frame with item names and weight values.
#' @param dataDF A two column data frame with the first column of a variable name 
#' and second column a numerical vector \code{c("Name", "Value")}.
#' @param main A string for the title of the plot.
#' @param separatedPlotsQ Should the plotted Pareto curves be separated or not?
#' @param scales Sames as \code{scales} of \code{ggplot2::facet_wrap}.
#' @param nrow Sames as \code{nrow} of \code{ggplot2::facet_wrap}.
#' @param ncol Sames as \code{ncol} of \code{ggplot2::facet_wrap}.
ParetoLawGGPlot <- function( dataDF, main = NULL, separatedPlotsQ = TRUE, scales = "fixed", nrow = NULL, ncol = NULL, ...  ) {
  qdf <- 
    purrr::map_df( split( dataDF, dataDF[["Name"]] ), function(x) {
      dataVec <- rev( sort(x$Value) )
      dataVec <- cumsum( dataVec ) / sum( dataVec )
      pres <- data.frame( Name = x$Name[[1]], Index = 1:nrow(x), ParetoFraction = dataVec, stringsAsFactors = FALSE )
      cbind( pres,
             p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )
    })

  if( separatedPlotsQ ) {
    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::facet_wrap( ~ Name, scales = scales, nrow = nrow, ncol = ncol  ) +
      ggplot2::theme( ... )
  } else {
    ggplot2::ggplot(qdf) +
      ggplot2::geom_line(  ggplot2::aes( x = Index, y = ParetoFraction, color = Name) ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p10), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p20), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p30), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p40), linetype = 3 ) +
      ggplot2::geom_vline( ggplot2::aes( xintercept = p50), linetype = 3 ) +
      ggplot2::theme( ... )
  }  
}

#' @description Sorts the tally of given categorical data descendingly and computes the list of the cumulative sums.
#' @param dataVec A vector with categorical entries.
#' @param weightVec A vector of weights for the values of \code{dataVec}; 
#' see the argument wt_var of \code{plyr::count}.
ParetoLawData <- function( dataVec, weightsVec = NULL ) {
  
  if ( is.numeric(dataVec) ) {
    stop("The argument dataVec is expected to be a categorical vector.", call. = TRUE )
  }
  
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
#' @param data A data frame.
#' @param colNames A list of column names corresponding to categrical columns in \code{data}.
ParetoLawDataForColumns <- function( data, colNames ) {
  res <- purrr::map(colNames, function(c) {t<-ParetoLawData(data[[c]]); cbind(1:length(t[,1]), t[,2])})
  names(res) <- colNames
  res
}


#' @description Finds the Pareto items in a column of a data frame.
#' @param data A data frame.
#' @param colName The column name of items to be counted.
#' @param paretoFraction A number between 0 and 1 specifying the Pareto fraction.
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
#' @param dataVec A data vector.
#' @param paretoFraction A number between 0 and 1 specifying the Pareto fraction.
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
#' @param data A data frame.
#' @param colNames A list of column names corresponding to categrical columns in \code{data}.
#' @param ... Arguments for \code{ggplot2::facet_wrap}.
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
    purrr::map_df( colNames, function(cn) {
      pres <- ParetoLawData( data[[cn]], weightsVec = weightsVec )
      cbind( ColName = cn, Index = 1:nrow(pres), pres,
             p10 = 0.1*nrow(pres), p20 = 0.2*nrow(pres), p30 = 0.3*nrow(pres),  p40 = 0.4*nrow(pres),  p50 = 0.5*nrow(pres) )
    })
  
  ggplot(qdf) +
    geom_line( aes( x = Index, y = ParetoFraction) ) +
    geom_vline( aes( xintercept = p10), linetype = 3 ) +
    geom_vline( aes( xintercept = p20), linetype = 3 ) +
    geom_vline( aes( xintercept = p30), linetype = 3 ) +
    geom_vline( aes( xintercept = p40), linetype = 3 ) +
    geom_vline( aes( xintercept = p50), linetype = 3 ) +
    facet_wrap( ~ ColName, ... )
}
