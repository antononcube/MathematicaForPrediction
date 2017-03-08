#=======================================================================================
# Time series recommender framework in R
# Copyright (C) 2016-2017  Anton Antonov
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# Written by Anton Antonov,
# antononcube @ gmail . com,
# Windermere, Florida, USA.
#
#=======================================================================================

#=======================================================================================
# This file contains code of a framework for time series search and recommendation.
# The file includes functions for ground analysis and pre-processing that are most likely 
# needed in order to derive effective recommendations.
#
# For the SMR* functions see the file:
# https://github.com/antononcube/MathematicaForPrediction/blob/master/R/SparseMatrixRecommender.R
#
#=======================================================================================


#' @description Finds Time Series (TS) recommendations for specified search row ID and / or search vector ID
#' @param timeSeriesMat a matrix with TS signals
#' @param smr an SMR object (e.g. based on time averaged signals)
#' @param itemIDtoNameRules item ID's to names rules
#' @param searchRowID search row ID that is in rownames(timeSeriesMat) == rownames(smr$M) or NULL
#' @param searchVector search vector with dimension ncol(timeSeriesMat) == ncol(smr$M) or NULL
#' @param nrecs number of recommendations to return; if NULL all recommendations are returned
#' @param smr.nrecs number of recommendations for the intermediate candidate finding using the SMR object
#' @param method one of { 'dot', 'pearson', 'spearman', 'kendall' }
#' @details The rownames of timeSeriesMat are composed of channels and item IDs. An item ID can be in several
#' different channels. Hence we have row IDs like "NYSE:APPL" or "NASDAQ:APPL".
TSPSRCorrelationNNs <- function( timeSeriesMat, smr, itemIDtoNameRules, searchRowID = NULL, searchVector = NULL, nrecs = 12, smr.nrecs = 2000, method = 'pearson' ) {
  
  if ( is.null(searchRowID) && is.null(searchVector) ) {
    stop( "At least one of the arguments searchRowID or searchVector has to be non-NULL.", call. = TRUE )
  }
  
  ## Search vector for the SMR object
  if ( !is.null( smr ) ) { 
    
    if ( is.numeric(searchVector) ) {
      searchVectorMat <- sparseMatrix( i = rep(1, length(searchVector) ), j = 1:length(searchVector), x = searchVector, dims = c( 1, ncol(smr$M) ) )
    } else {
      if ( is.null(searchRowID) ) {
        stop( "If searchRowID is NULL then searchVector is expected to be numeric.", call. = TRUE )
      }
      searchVectorMat <- NULL
    }
    
    if ( !is.null( searchRowID ) && !is.null( searchVectorMat ) )  {
      searchVectorMat <- smr$M[ searchRowID, , drop = FALSE ] + searchVectorMat
    } else if ( is.null(searchVectorMat) ) {
      searchVectorMat <- smr$M[ searchRowID, , drop = FALSE ]
    }
    
  } 
  
  ## Search vector for the correlation matrices
  if ( !is.null( searchRowID ) && !is.null( searchVector ) )  {
    searchVector <- timeSeriesMat[ searchRowID, ] + searchVector
  } else if ( is.null(searchVector) ) {
    searchVector <- timeSeriesMat[ searchRowID, ]
  }  
  
  if ( is.null(smr) ) {
    ## Use only the correlations of the TS matrix rows
    
    recVec <- cor( as.matrix( t(timeSeriesMat) ), searchVector, method = method )
    recVec <- recVec[ order(-recVec[,1]), ,drop=FALSE]
    recsItemSplit <- setNames( ldply( strsplit( rownames(recVec), ":" ), function(x) x ), c( "channel", "itemId" ) )
    corRecs <- data.frame( Score = recVec, 
                           channel.itemId = rownames(recVec), 
                           recsItemSplit, 
                           itemName = itemIdtoNameRules[ recsItemSplit$itemId ],
                           stringsAsFactors = FALSE, row.names = NULL  )
    if ( is.null(nrecs)) { corRecs } else { corRecs[ 1:nrecs, ] }
    
  } else {
    ## Use the SMR object first, and then correlations between the TS matrix rows.
    
    recs <- SMRRecommendationsByProfileVector( smr, searchVectorMat, nrecs = smr.nrecs )
    recsItemSplit <- setNames( ldply( strsplit( recs$Item, ":" ), function(x) x ), c( "channel", "itemId" ) )
    dotRecs <- cbind( Score = recs$Score, 
                      channel.itemId = recs$Item, 
                      recsItemSplit, 
                      itemName = itemIDtoNameRules[ recsItemSplit$itemId ], 
                      stringsAsFactors = FALSE, row.names = NULL ) 
    
    if ( method != 'dot' ) {
      
      recVec <- cor( as.matrix( t(timeSeriesMat[ dotRecs$channel.itemId, ]) ), searchVector, method = method )
      recVec <- recVec[ order(-recVec[,1]), ,drop=FALSE]
      recsItemSplit <- setNames( ldply( strsplit( rownames(recVec), ":" ), function(x) x ), c( "channel", "itemId" ) )
      corRecs <- data.frame( Score = recVec, 
                             channel.itemId = rownames(recVec), 
                             recsItemSplit, 
                             itemName = itemIDtoNameRules[ recsItemSplit$itemId ], 
                             stringsAsFactors = FALSE, row.names = NULL  )      
      if ( is.null(nrecs)) { corRecs } else { corRecs[ 1:nrecs, ] }
      
    } else {
      if ( is.null(nrecs)) { dotRecs } else { dotRecs[ 1:nrecs, ] }
    }
  }
}