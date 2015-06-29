##=======================================================================================
## Implementation of a Hub-Item Dynamic Ranking Algorithm in R
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
## R is a language and environment for statistical computing and
## graphics. It is a GNU project which is similar to the S language
## and environment which was developed at Bell Laboratories (formerly
## AT&T, now Lucent Technologies) by John Chambers and colleagues. R
## can be considered as a different implementation of S.
##
## R is available as Free Software under the terms of the Free
## Software Foundation's GNU General Public License in source code
## form.
##
## For more details see http://www.r-project.org/ .
##
## =======================================================================================
## The Hub-Item Dynamic Ranking Algorithm (HIDRA) can be seen as an
## extension of PageRank.  See http://en.wikipedia.org/wiki/PageRank .
## The difference is that HIDRA works on a bi-partite graph and
## multiple rank vectors are calculated using different biasing based on
## predicates over the nodes. For a particular query a linear
## combination of the closest ranks vectors is used in order to obtain
## the response ranks. (That is the dynamic ranking part.)
##
## There are at least two other similar algorithms:
## 1. Hyperlink-Induced Topic Search (HITS), http://en.wikipedia.org/wiki/HITS_algorithm, and
## 2. Topic-Sensitive PageRank, http://en.wikipedia.org/wiki/Topic-Sensitive_PageRank . 
##=======================================================================================
## Version 0.4
## This implementation started as re-implementation of a Mathematica implementaion of HIDRA.
## At some point it became better and more useful than the Mathematica one.
##=======================================================================================
## TODO
## 1. Explanation of the flow of computations and how to be used.
## 2. Explanations why Power Method is used over the whole matrix instead on
##    the matrix hub-item and item-hub blocks. (As in HITS.)
## 3. Review of the dynamic ranking queries functions.
##=======================================================================================

require(plyr)
require(reshape2)
require(Matrix)


#' @description Find the Euclidean norm of a vector.
vnorm <- function(x) { sqrt( sum(x*x) ) }

#' @description Find the eigenvector with the largest eigenvalue using the Power method.
#' @param mat sparse matrix
#' @param bvec biasing vector (a guess of the probabilities)
#' @param alpha significance of the \parm pmat
#' @param maxSteps the maximum number of iteration steps
PowerMethod <- function( mat, bvec, alpha, maxSteps = 100, tol = 10^(-6) ) {
  n <- ncol(mat)
  v <- runif( n = n, min = 0, max = 1 )
  v <- v / max(v)
  v <- sparseMatrix( i=1:n, j=rep(1,n), x=v )
  k <- 0
  vold <- sparseMatrix( i=1:n, j=rep(1,n), x=rep(1,n) )
  if ( is.numeric(bvec) ) {
    bvec <- sparseMatrix( i=1:n, j=rep(1,n), x=bvec )
  }
  while ( norm( v - vold, "F" ) > tol && k < maxSteps ) {
    vold <- v
    v <- alpha * mat %*% v + ( 1 - alpha ) * ( bvec * sum(v) )
    v <- v / max(abs(v))
    k <- k + 1
  }
  rownames(v) <- rownames(mat)
  list( Vector = v, Iterations = k, ResidualNorm = norm( v - vold, "F" ) )
}

#' @description Makes the adjacency matrix of a bi-partite graph connecting.
#' @param hubItemScoresArray data frame with columns HubID, ItemID, Score, Awarded
MakeBiPartiteGraphMatrix <- function ( hubItemScoresArray ) {
  
  hubIDs = unique( hubItemScoresArray[,c("HubID")] )
  itemIDs = unique( hubItemScoresArray[,c("ItemID")] )
  
  names(hubItemScoresArray) <- c( "HubID", "ItemID", "HubItemScore", "ItemHubScore" )
  
  
  hubDF <- data.frame( HubID = hubIDs, HubIndex = 1:length(hubIDs) )
  itemDF <- data.frame( ItemID = itemIDs, ItemIndex = length(hubIDs) + ( 1:length(itemIDs) ) )
  
  tempPTA <- hubItemScoresArray[,c( "HubID", "ItemID", "HubItemScore", "ItemHubScore" )]
  
  tempPTA <- join( x = tempPTA, y = hubDF, by = "HubID" )
  tempPTA <- join( x = tempPTA, y = itemDF, by = "ItemID" )
  
  tempPTA <- tempPTA[, c("HubIndex", "ItemIndex", "HubItemScore", "ItemHubScore") ]
  
  
  tempPTA <- rbind( setNames( tempPTA[, c( "HubIndex", "ItemIndex", "HubItemScore" ) ], c( "Index1", "Index2", "Score" ) ),
                    setNames( tempPTA[, c( "ItemIndex", "HubIndex", "ItemHubScore" ) ], c( "Index1", "Index2", "Score" ) ) )
  
  bmat <- xtabs( Score ~ Index2 + Index1, tempPTA, sparse=TRUE )
  
  # make the matrix column stochastic
  colNorms <- sqrt( colSums( bmat * bmat ) )
  bmat <- bmat %*% Diagonal( x = 1 / ifelse( colNorms > 0, colNorms, 1) )
  
  list( M = bmat, Hubs = hubDF, Items = itemDF )
}

#' @description Remove the zero entries from a sparse matrix.
#' @ smat a sparse matrix
RemoveZeroEntries <- function( smat ) {
  df <- summary(smat)
  smatDF <- data.frame( i=df$i, j=df$j, x=df$x )
  smatDF <- smatDF[ smatDF$x > 0, ]
  smatRes <- sparseMatrix( i = smatDF$i, j = smatDF$j, x = smatDF$x, dims = c( nrow(smat), ncol(smat) ) )
  rownames(smatRes) <- rownames(smatRes)
  colnames(smatRes) <- colnames(smatRes)
  smatRes
}

#' @description Calculate the hub-item ranks for a given adjacency matrix of bi-partite graph and characterizing tags of the nodes.
#' @param hubsAndItemsMat a bipartite graph adjacency matrix of hub-item connections 
#' with rownames and colnames corresponding to the hub and item ID's.
#' @param hubIDs a data frame with columns c('HubID','HubIndex') mapping ID's to indexes in hubsAndItemsMat
#' @param hubTags a list of tags lists, each characterizing its corresponding hub in hubIDs
#' @param tagSets a list of (frequent) metadata sets
#' @param tagPresenseFraction what fraction of each tag set is in the hub tags
#' @param bias a numerical vector with biasing values for each hub and item
#' @param normalizationFunc normalization function applied to the ranks vector for each tag set
#' @return A list of sparse arrays corresponding to the item ranks for tagSets.
HubItemRanks <- function( hubsAndItemsMat, hubIDs, hubTags, tagSets, 
                          tagPresenseFraction = 0.8, bias = NULL, normalizationFunc = max,
                          maxSteps = 100, sumFactor = 0.2, numberOfHubRanks = 200, numberOfItemRanks = 200 ) {
  
  ## Hubs are assumed to be in the front of the matrix
  if( !is.data.frame(hubIDs) ) {
      stop( "The argument hubIDs is expected to be a data frame with columns c('HubID','HubIndex').", call. = TRUE )
  } 

  # preliminary
  n <- ncol(hubsAndItemsMat)
  
  # find eigenvectors
  rankVecs <- llply( tagSets,
                     function(ts) {
                       
                       if ( length(ts) == 1 ) {
                         hubInds <- laply( hubTags, function(x) length( intersect( ts, x ) ) )
                         hubInds <- hubInds > 0
                         hubInds <- (1:length(hubTags))[ hubInds ]
                       } else {
                         hubInds <- laply( hubTags, function(x) length( intersect( ts, x ) ) / length(ts) >= tagPresenseFraction )
                         hubInds <- (1:length(hubTags))[ hubInds ]                          
                       } 

                       # hubsAndItemsMat is a square matrix
                       bvec <- rep(0.0, n ); names(bvec) <- rownames(hubsAndItemsMat)
                       bvec[ hubIDs$HubIndex[hubInds] ] <- 1
                 
                       if ( !is.null(bias) ) { 
                         bvec <- bvec * bias 
                         if( max(bvec) > 0 ) { bvec <- bvec / max(bvec) }
                       }

                       ## Find the ranks
                       res <- PowerMethod( hubsAndItemsMat, bvec, sumFactor, maxSteps=maxSteps)
                       
                       ## Normalize
                       uEVec <- res$Vector
                       if ( !is.null(normalizationFunc) ) {
                         uEVec <- uEVec / normalizationFunc(uEVec)
                       }
                       
                       ## Find the threshold for hubs
                       ranksPerVec <- min( nrow(hubIDs), numberOfHubRanks )
                       vecTh <- sort(uEVec@x[1:nrow(hubIDs)])[nrow(hubIDs)-ranksPerVec+1]
                       
                       ## Remove the zeroes from the sparse pattern obtained with uEVec@x[ uEVec@x > vecTh ] <- 0
                       pickInds <- (uEVec@x >= vecTh) & ( (1:n) <= nrow(hubIDs) )
                       hubsVec <- sparseMatrix( i = (1:n)[pickInds], j = rep(1, sum(pickInds) ), x = uEVec@x[pickInds], dims = c(n,1) )
                       rownames(hubsVec) <- rownames(hubsAndItemsMat) ## should be equal to rownames(res$Vector)

                       ## Find the threshold for items
                       ranksPerVec <- min( n-nrow(hubIDs), numberOfItemRanks )
                       vecTh <- sort(uEVec@x[(nrow(hubIDs)+1):n])[(n-nrow(hubIDs))-ranksPerVec+1]
                       
                       ## Remove the zeroes from the sparse pattern obtained with uEVec@x[ uEVec@x > vecTh ] <- 0
                       pickInds <- (uEVec@x >= vecTh) & ( (1:n) > nrow(hubIDs) )
                       itemsVec <- sparseMatrix( i = (1:n)[pickInds], j = rep(1, sum(pickInds) ), x = uEVec@x[pickInds], dims = c(n,1) )
                       rownames(itemsVec) <- rownames(hubsAndItemsMat) ## should be equal to rownames(res$Vector)
                       
                       ## Result 
                       list( HubRanks = hubsVec, ItemRanks = itemsVec )
                     }
  )
  
  llply( 1:length(tagSets), function(i) list( TagSet=tagSets[[i]], HubRankVector=rankVecs[[i]]$HubRanks, ItemRankVector=rankVecs[[i]]$ItemRanks ) )
  
}

#' @description Finds the closest lists of strings to a list of strings
#' @param tagSets list of lists of strings
#' @param tset list of strings
NearestTagSets <- function(tagSets, tset) {
  ds <- dlply( tagSets, function(x) c( length(pmatch( tset, x )) / length(x), x ) )
  names(ds) <- c("Score","TagSet")
  ds <- ds[ rev(order(ds$Score)) ]
  if ( ds$Score[1] == 0 ) {
    NULL
  } else {
    ds[ ds$Score == ds$Score[1], ]
  }
}

#' @description Find the dynamic ranks
#' @param tagSet
#' @param tagSetAndRankVectorPairs
DynamicRank <- function(tagSet, tagSetAndRankVectorPairs ) {
  s <- NearestTagSets( llply( tagSetAndRankVectorPairs, function(x) x[[1]] ), tagSet )
  if ( is.null(s) ) {
    rep( 0, tagSetAndRankVectorPairs[[1]][[2]] )
  } else {
    
  }
}
