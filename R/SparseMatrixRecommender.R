#=======================================================================================
# Sparse matrix recommender framework in R
# Copyright (C) 2014-2016  Anton Antonov
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
# Initially this code was made to resemble the Sparse Matrix Recommender Mathematica
# package [1] as closely as possible, but an approach more inherent to R was taken.
# Namely, the columns and the rows of the metadata matrix are named, and because of this
# tag-index and item-index rules are not required.

# The tag-index and item-index rules are made with integer arrays with named entries.

# I did consider programming and using a S4 object, but that requires the declaration of
# too many generic functions. And because inheritance is not essential I kept the object
# in a list.

# There should be separate files (packages) for term weights and outlier detection.
# See the notes below.

# [1] Anton Antonov, Sparse matrix recommender framework in Mathematica,
#     SparseMatrixRecommenderFramework.m at MathematicaForPrediction project at GitHub, (2014).
#     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/SparseMatrixRecommenderFramework.m
#
# History
# Started: November 2013,
# Updated: December 2013, May 2014, June 2014, July 2014, December 2014,
# January 2016, June 2016, September 2016.
#=======================================================================================
#
# TODO Argument type and ranks check
# Mathematica has pattern matching for the arguments, here I have to make type checks.
# Note that S4 provides some of this functionality.
#
# TODO Union of two SMRs by sub-matrices 
# [ ] Given two matrices of the same tag type data for two SMRs make
#     one matrix that have the union of the rownames and colnames.
#     - Take care of collisions.
#       - Trivially done with matrix summation and clipping.
# [ ] Make the SMR sub-matrix union for
#     - a given pair of SMRs, and
#     - a given a list of pairs of tag types.
#---------------------------------------------------------------------------------------

# 05/02/14
# I am not sure:
# 1. should the recommendation request functions take data frames,
# 2. should the scores be the first column (as in the Mathematica code).
# These points need more design effort.

# 05/12/14
# After a conversation with a coworker: it is better instead of an array for tag type
# offsets to use a data frame with the column ranges of the tag types.

# 07/30/14
# 1. Refactored the code for creation of SMR objects: two signatures from transactions,
# and from matrices.
# 2. Extracted the document-term weight functions in a separate file:
# DocumentTermWeightFunctions.R
#
# 12/23/14
# Added the function SMRReorderRecommendations that re-orders recommendations according
# to scores from common tags.
#
# 2016-01-05
# Introduced of S3 OOP (i.e. generic function) implementations for the recommender objects.
# A recommender object is just a list with named elements. It is turned into an
# S3 object by assigning "SMR" to the attribute class.
# This file has only set-up and implementations for SMR's. Other type of recommenders
# have to provided the corresponding generic functions.
# At this point they are three:
# 1. Recommendations (e.g. Recommendations.SMR )
# 2. RecommenderTags (e.g. RecommenderTags.SMR )
# 3. RecommenderItems (e.g. RecommenderItems.SMR )
#
# 2016-06-05
# Added an implementation of the Composite pattern for combined recommendations.
# - Application of the Composite Design Pattern for a collection of recommenders using S3 objects
#   For example, SMR, PageRank recommenders, and HubItemDynamicRanks recommender.
# - Implemented a function for the combination of recommendations from many recommenders.
# - There is an argument allowing the merging of the recommendations to be done
#   according to different types normalizations.
#
# 2016-06-08
# Added functions for converting the SMR sparse matrices data into data frames
# both (long and wide forms).
#
# 2016-09-12
# Added a classification computation function for a profile vector based on
# specified number of top NNs.
#=======================================================================================

#' @detail Required libraries
require(plyr)
require(reshape2)
require(Matrix)

#' @detail Read weight functions application definitions
# source("./DocumentTermWeightFunctions.R")

#' @description Convert to contingency matrix from item consumption "transactions" (e.g. instances of movie watching)
#' @param dataRows a data frame corresponding to a item consumption metadata table
#' @param itemColumnName name of the column of dataRows the values of which correspond to the rows of the returned matrix
#' @param tagType name of the column of dataRows the values of which correspond to the columns of the returned matrix
#' @param sparse a logical, should the returned matrix be sparse or not
#' @return a matrix
SMRCreateItemTagMatrix <- function( dataRows, itemColumnName, tagType, sparse=TRUE ) {
  frequencies <- plyr::count(dataRows, vars=c(itemColumnName, tagType))
  formulaString <- paste("freq ~", itemColumnName, "+", tagType)
  xtabs(as.formula(formulaString), frequencies, sparse=sparse )
}

#' @description Creates a sparse matrix recommender from transactions data and a list of tag types
#' @param dataRows transaction data frame
#' @param tagTypes the name of the column containing the categorical tags
#' @param itemColumnName the name of the column containing the unique items
#' @return An S3 object is returned that is list with class attribute set to "SMR".
SMRCreate <- function(dataRows, tagTypes, itemColumnName ){
  matrices <- alply(tagTypes, 1, function(x){
    SMRCreateItemTagMatrix(dataRows, tagType=x, itemColumnName=itemColumnName)
  })
  
  SMRCreateFromMatrices(matrices, tagTypes, itemColumnName)
}

#' @description Creates a sparse matrix recommender from a list of matrices and a corresponding list of tag types
#' @param matrices matrices to be spliced into a metadata matrix
#' @param tagTypes the name of the column containing the categorical tags
#' @param itemColumnName the name of the column containing the unique items
#' @return An S3 object is returned that is list with class attribute set to "SMR".
SMRCreateFromMatrices <- function( matrices, tagTypes, itemColumnName ){
  
  if ( length(matrices) != length(tagTypes)  ) {
    stop("The same number of matrices and tag types is required.", call.=TRUE)
  }
  
  m <- do.call(cBind, matrices)
  
  widths <- laply(matrices, function(x){ncol(x)})
  ends <- cumsum(widths)
  begins <- ends - widths + 1
  ranges <- data.frame(Begin=begins, End=ends)
  rownames(ranges)=tagTypes
  
  tagToIndexRules <- 1:ncol(m)
  names(tagToIndexRules) <- colnames(m)
  
  itemToIndexRules <- 1:nrow(m)
  names(itemToIndexRules) <- rownames(m)
  
  res <- list( M=m, M01=m, TagTypeRanges=ranges, TagTypes=tagTypes, ItemColumnName=itemColumnName,
               TagToIndexRules=tagToIndexRules, ItemToIndexRules=itemToIndexRules )
  class(res) <- "SMR"
  res
}


#' @description Creates a sparse matrix recommender from transactions-like data and a meta-data specifiction.
#' @param data transactions-like data frame
#' @param metaDataSpec a data frame with specifications of which columns of \param data to be used and with what weight functions
#' @param itemCol the name of the column containing the unique items
SMRCreateFromSpecification <- function( data, metaDataSpec, itemCol, .progress="none", .verbose = FALSE ) {
  
  if( class(data) != "data.frame" || class(metaDataSpec) != "data.frame" ) {
    stop("The first and second arguments are expected to be data frames.")
  }
  
  if(.verbose){
    cat("\t\tCreate item-tag matrices for meta data types.\n")
  }
  
  matrices <- alply(as.character(metaDataSpec$ColumnName), 1, function(x){
    SMRCreateItemTagMatrix( dataRows = data, tagType = x, itemColumnName = itemCol, sparse = T)
  }, .progress=.progress)
  
  
  if(.verbose){
    cat("\t\tApply weight terms to each tag sub-matrix\n")
  }
  
  if( !( "ValueColumnName" %in% colnames(metaDataSpec) ) ) { 
    metaDataSpec <- cbind( metaDataSpec, ValueColumnName = NA )
  }
  

  matrices <-
    dlply( metaDataSpec, c("ColumnName", "ValueColumnName"), function(x) {

      if ( is.null(x$ValueColumnName) || is.na(x$ValueColumnName) ) {
        smat <- SMRCreateItemTagMatrix( dataRows = data, tagType = x$ColumnName[[1]], itemColumnName = itemCol, sparse = TRUE )
      } else {
        smat <- xtabs( as.formula( paste( x$ValueColumnName[[1]], "~", itemCol, "+", x$ColumnName[[1]] ) ), data = data, sparse = TRUE )
      }
      
      smat <- SMRApplyTermWeightFunctions( smat, 
                                           x$GlobalWeightFunction[[1]],
                                           x$LocalWeightFunction[[1]], 
                                           x$NormalizingFunction[[1]] )      
      
      if ( !is.null(x$NormalizeByMax[[1]]) && metaDataSpec$NormalizeByMax[[1]] ) { 
        smat <- smat / max( smat )
      }
      
      smat      
    }, .progress = .progress )
  
  names(matrices) <- gsub( "\\.NA$", "", names(matrices) )
  
  allRowIDs <- unique( unlist( llply(matrices, function(x) rownames(x) ) ) )
  
  nms <- names(matrices)
  matrices <- llply( matrices, function(x) ImposeRowIDs( rowIDs = allRowIDs, smat = x) )
  names(matrices) <- nms
  
  SMRCreateFromMatrices( matrices = matrices, tagTypes = names(matrices), itemColumnName = itemCol )
}


#' @description Changes the weights of the tags of a sparse matrix recommender object
#' @param smr a sparse matrix recommender object (list with named elements)
#' @param weights a list of weights to be applied
SMRApplyTagWeights <- function( smr, weights ) {
  if ( length(weights) < ncol(smr$M01) ) {
    weights <- rep( weights, ncol(smr$M01) )
  } else if ( length(weights) > ncol(smr$M01) ) {
    weights <- weights[1:ncol(smr$M01)]
  }
  W <- Diagonal(x=weights)
  smr$M01 %*% W
}

#' @description Makes all sub-matrices to have elements between 0 and 1
#' @param smr a sparse matrix recommender object (list with named elements)
SMRNormalizeSubMatricesByMaxEntry <- function( smr ) {
  mWeights <- laply( smr$TagTypes, function(tt) max( SMRSubMatrix(smr, tt) ) )
  mWeights[ mWeights == 0 ] <- 1
  SMRApplyTagTypeWeights( smr, 1 / mWeights )
}

#' @description Changes the weights of tag types of a sparse matrix recommender object
#' @param smr a sparse matrix recommender object (list with named elements)
#' @param weights a list of weights to be applied
SMRApplyTagTypeWeights <- function( smr, weights ) {
  if ( length(weights) < length(smr$TagTypes) ) {
    weights <- rep(weights, length(smr$TagTypes) )
  } else if ( length(weights) > length(smr$TagTypes) ) {
    weights <- weights[1:length(smr$TagTypes)]
  }
  #wvec <- unlist(mlply(cbind(smr$TagTypeRanges,W=weights), function(Begin,End,W) rep(W,End-Begin+1)))
  wvec <- llply( 1:nrow(smr$TagTypeRanges), function(i) rep( weights[i], smr$TagTypeRanges[i,]$End - smr$TagTypeRanges[i,]$Begin + 1 ) )
  wvec <- do.call( c, wvec )
  SMRApplyTagWeights( smr, wvec )
}


#' @description Returns the sub-matrix of the SMR metadata matrix that corresponds to a tag type
#' @param smr a sparse matrix recommender object (list with named elements)
#' @param tagType a tag type
SMRSubMatrix <- function(smr, tagType ){
  smr$M[,smr$TagTypeRanges[tagType, "Begin"]:smr$TagTypeRanges[tagType, "End"], drop = FALSE ]
}

#' @description Returns the sub-matrix of a matrix that corresponds to a tag type in an SMR object
#' @param M a sparse matrix (in a sparse matrix recommender object)
#' @param ranges column ranges of sub-matrices (in a sparse matrix recommender object)
#' @param tagType a tag type
SMRSubMatrixOfMatrix <- function( M, ranges, tagType ) {
  M[,ranges[tagType, "Begin"]:ranges[tagType, "End"]]
}

#' @description Finds the current significance factors in a SMR object
#' @param smr a sparse matrix object
SMRCurrentTagTypeSignificanceFactors <- function(smr) {
  sfs01 <- laply( smr$TagTypes, function(tc) sum( SMRSubMatrixOfMatrix( smr$M01, smr$TagTypeRanges, tc ) ) )
  sfs01[ sfs01 == 0 ] <- 1
  res <- laply( smr$TagTypes, function(tc) sum( SMRSubMatrix( smr, tc ) ) ) / sfs01
  setNames( res, smr$TagTypes )
}


#' @description Restrict the recommendations vector by additional parameters and convert to a data frame.
#' @param rvec recommendations vector
#' @param history history of items
#' @param nrecs number of recommendations to be returned
#' @param removeHistory logical should the history be dropped or not
SMRRecommendationsVectorToDF <- function( rvec, history, nrecs, removeHistory ) {
  rvec <- as.numeric(rvec)
  if ( is.null(nrecs) ) {
    ## take all non-zero 
    recInds <- rev(order(rvec))
    recInds <- recInds[ rvec[recInds] > 0 ]
    nrecs <- length(recInds)
  } else {
    recInds <- rev(order(rvec))[1:(nrecs + length(history))]
  }
  
  if ( removeHistory ) {
    dropInds <- recInds %in% history
    recInds <- recInds[ ! dropInds ]
  }
  
  if ( nrecs < length(recInds) ) {
    recInds <- recInds[1:nrecs]
  } 
  recScores <- rvec[ recInds ]
  
  data.frame( Score = recScores, Index = recInds, stringsAsFactors=FALSE )
}


#' @description Recommend items based on a sparse matrix and user history of consumption
#' @param smr sparse matrix recommender
#' @param userHistoryItems the items the user has consumed / purchased
#' @param userRatings ratings of the history items
#' @param nrecs number of recommendations to be returned
#' @param removeHistory should the history be removed from the recommendations
SMRRecommendations <- function( smr, userHistoryItems, userRatings, nrecs, removeHistory=TRUE ) {
  
  if ( class(userHistoryItems) != "integer" && class(userHistoryItems) != "numeric" ) {
    userHistoryItems <- match( userHistoryItems, rownames(smr$M) )
  }
  if ( class(userHistoryItems) != "integer" && class(userHistoryItems) != "numeric" ) {
    stop("Row ID's (names or indices) are expected for the argument userHistoryItems.", call.=TRUE)
  }
  if ( class(userRatings) != "numeric" && class(userRatings) != "integer") {
    stop("Positive real numbers are expected for the argument userRatings.", call.=TRUE)
  }
  if ( length(userRatings) < length(userHistoryItems) ) {
    userRatings <- rep( userRatings, length(userHistoryItems) )
  }
  if ( length(userRatings) > length(userHistoryItems) ) {
    userRatings <- userRatings[1:length(userHistoryItems)]
  }
  
  hvec <- sparseMatrix(i=rep(1,length(userHistoryItems)), j=userHistoryItems, x=userRatings, dims=c(1,dim(smr$M)[1]))
  rvec <- smr$M %*% t(hvec %*% smr$M)
  rvec <- as.array(rvec)
  recInds <- rev(order(rvec))[1:(nrecs+length(userHistoryItems))]
  
  if ( removeHistory ) {
    dropInds <- recInds %in% userHistoryItems
    recInds <- recInds[ ! dropInds ]
  }
  
  if ( nrecs < length(recInds) ) {
    recInds <- recInds[1:nrecs]
  }
  recScores <- rvec[ recInds ]
  
  res<-as.data.frame(cbind(recScores,recInds), stringsAsFactors=FALSE)
  res<-cbind(res,rownames(smr$M)[recInds], stringsAsFactors=FALSE)
  names(res)<-c("Score","Index",smr$ItemColumnName)
  res
}


#' @description Recommend items based on a sparse matrix and user history of consumption
#' @param smr sparse matrix recommender
#' @param history a data frame of rated items with colums("Ratings",<some-item-ID>)
#' @param nrecs number of recommendations to be returned
#' @param removeHistory should the history be removed from the recommendationsa
SMRRecommendationsDF <- function( smr, history, nrecs, removeHistory=TRUE ) {
  if ( is.numeric(history[,2]) ) {
    res <- SMRRecommendations( smr, history[,2], history[,1], nrecs )
  } else {
    inds <- match(  history[,2], rownames( smr$M ) )
    if (  NA %in% inds ) {
      stop("Some of the items are not in the sparse matrix recommender object.")
    }
    res <- SMRRecommendations( smr, inds, history[,1], nrecs, removeHistory )
  }
  names(res) <- c( names(res)[1:2], names(history)[[2]] )
  res
}

#' @description Recommend items based on a sparse matrix and a specified profile
#' @param smar sparse matrix recommender
#' @param profile data frame of scored tags, profile of a user with column names c( "Score", "Tag" | "Index" )
#' @param nrecs number of recommendations to be returned
#' @return Returns a data frame.
SMRRecommendationsByProfileDF <- function( smr, profile, nrecs ) {
  if ( names(profile) == c( "Tag", "Score" ) || names(profile) == c( "Index", "Score" ) ) {
    profile <- profile[,c(2,1)]
  }
  if ( is.numeric( profile[,2] ) ) {
    res <- SMRRecommendationsByProfile( smr, profile[,2], profile[,1], nrecs )
  } else {
    inds <- match(  profile[,2], colnames( smr$M ) )
    if (  NA %in% inds ) {
      stop("Some of the tags are not in the sparse matrix recommender object.")
    }
    res <- SMRRecommendationsByProfile( smr, inds, profile[,1], nrecs )
  }
  res
}


#' @description Recommend items based on a sparse matrix and a specified profile indices and scores
#' @param smr sparse matrix recommender
#' @param profileInds metadata indices corresponding to the columns of \param smr$M
#' @param profileRatings ratings of the profile metadata
#' @param nrecs number of recommendations to be returned
#' @return Returns a data frame.
SMRRecommendationsByProfile <- function( smr, profileInds, profileRatings, nrecs ) {
  pvec <- sparseMatrix(i=rep(1,length(profileInds)), j=profileInds, x=profileRatings, dims=c(1,dim(smr$M)[2]))
  SMRRecommendationsByProfileVector( smr, pvec, nrecs )
}


#' @description Recommend items based on a sparse matrix and specified profile
#' @param smar sparse matrix recommender
#' @param profileVec is a sparse matrix with 1 row (a row from a sparse matrix)
#' @param nrecs number of recommendations to be returned
SMRRecommendationsByProfileVector <- function( smr, profileVec, nrecs ) {
  if ( dim( profileVec )[[2]] == dim( smr$M )[[2]] ) {
    profileVec <- t(profileVec)
  }
  rvec <- smr$M %*% profileVec
  rvec <- as.array(rvec)
  recInds <- rev(order(rvec))
  recScores <- rvec[recInds]
  if ( nrecs > length(rvec) ) {
    nrecs <- length(rvec)
  }
  res <- data.frame( Score = recScores[1:nrecs], Index = recInds[1:nrecs], stringsAsFactors = FALSE )
  res <- cbind( res, Item = rownames(smr$M)[recInds[1:nrecs]], stringsAsFactors = FALSE )
  names(res)<-c( "Score", "Index", "Item" )
  res
}


#' @description Classify a profile vector into the column names of a tag type sub-matrix.
#' @param smar sparse matrix recommender
#' @param tagType tag type for which the classification is done
#' @param profileVec is a sparse matrix with 1 row (a row from a sparse matrix)
#' @param nTopNNs number of top nearest neighbors to be used in the derive the classificationß
#' @param voting boolean should simple voting be used or a weighted sum
SMRClassifyByProfileVector <- function( smr, tagType, profileVec, nTopNNs, voting = FALSE, dropZeroScoredLabels = TRUE ) {
 
  recs <- SMRRecommendationsByProfileVector( smr = smr, profileVec = profileVec, nrecs = nTopNNs )
  
  ## Assuming the class labels sub-matrix is relatively small we can do this:
  ## clMat <- SMRSubMatrix( smr = smr, tagType = tagType )
  ## It can be optimized  using a class label matrix member inside the SMR object.
  ## Hopefully, this is quick enough in most cases:
  clMat <- smr$M[ recs$Item, smr$TagTypeRanges[tagType, "Begin"] : smr$TagTypeRanges[tagType, "End"], drop=F ]
  
  if ( voting ) {
    clMat@x[ clMat@x > 0 ] <- 1
    recs$Score <- 1
  }
  s <- (recs$Score / max(recs$Score) ) %*% clMat[ recs$Item, , drop=F]
  s <- data.frame( Score = s[1,], Label = colnames(s) )
  s <- s[ order(-s[,1]), ]

  if( dropZeroScoredLabels ) { s[ s$Score > 0, ] }
  else { s }
}


#' @description Calculate profile vector from item history
#' @param smr a sparse matrix recommendation object
#' @param itemHistory a data frame with items history with column names c("Rating", "Item")
SMRProfileVector <- function( smr, itemHistory ) { 
  pinds <- match( itemHistory[,2], rownames(smr$M) )
  names(itemHistory) <- c("Rating", "Item")
  hvec <- sparseMatrix( i=rep(1,nrow(itemHistory)), j=pinds, x=itemHistory$Rating, dims=c(1,dim(smr$M)[1]) )
  pvec <- hvec %*% smr$M
  t(pvec)
}


#' @description Calculate profile from item history
#' @param smr a sparse matrix recommendation object
#' @param itemHistory a data frame with item history with column names c("Rating", "Item")
SMRProfileDF <- function( smr, itemHistory ) {
  pvec <- SMRProfileVector( smr, itemHistory )
  pvecInds <- which( pvec > 0 )
  pvecScores <- pvec[ pvecInds ]
  res <- data.frame( Score = pvecScores, Index = pvecInds, stringsAsFactors = FALSE  )
  res <- cbind( res, Tag = colnames(smr$M)[ pvecInds ], stringsAsFactors = FALSE )
  names(res) <- c("Score","Index","Tag")
  res[ rev( order(res$Score) ),]
}


#' @description Return a data frame corresponding to a profile vector
#' @param smr a sparse matrix recommendation object
#' @param pvec a sparse matrix with one column
SMRProfileDFFromVector <- function( smr, pvec ) {
  pvecInds <- which( pvec > 0 )
  pvecScores <- pvec[ pvecInds ]
  res <- data.frame( Score = pvecScores, Index = pvecInds, stringsAsFactors = FALSE )
  res <- cbind( res, Tag = colnames(smr$M)[ pvecInds ], stringsAsFactors = FALSE )
  names(res) <- c("Score","Index","Tag")
  res[ rev( order(res$Score) ), ]
}


#' @description Return a data frame corresponding to a profile vector
#' @param smr a sparse matrix recommendation object
#' @param profile a data frame with names c( "Score", "Index", "Tag" )
#' @param tagType tag type over which the vector is made
#' @param uniqueColumns boolean should the tags in the profile have unique indices in the columns of smr$M 
#' @return a sparse matrix with one column
SMRProfileDFToVector <- function( smr, profileDF, tagType = NULL, uniqueColumns = TRUE ) {
  if ( length( intersect( names(profileDF), c("Score", "Index" ) ) ) == 2 ) {
    sparseMatrix( i = profileDF$Index, j = rep(1,nrow(profileDF)), x = profileDF$Score, dims = c( ncol(smr$M), 1 ) )
  } else if ( length( intersect( names(profileDF), c("Score", "Tag" ) ) ) == 2  ) {
    if ( is.null(tagType) ) {
      inds <- which( colnames( smr$M ) %in% profileDF$Tag )
      if ( uniqueColumns ) { 
        if (length(inds) != nrow(profileDF) ) {
          stop( "Not all tags are known in the SMR object or some SMR tags are repeated.", call. = TRUE )
        }
        sparseMatrix( i = inds, j = rep(1,nrow(profileDF)), x = profileDF$Score, dims = c( ncol(smr$M), 1 ) )
      } else {
        if ( length(inds) < nrow(profileDF) ) {
          stop( "Not all tags are known in the SMR object.", call. = TRUE )
        }
        ## tagInds <- which( profileDF$Tag %in% colnames(smr&M)[inds] )
        df <- 
          ldply( 1:nrow(profileDF), function(i) { 
            data.frame( Index = which( colnames( smr$M ) %in% profileDF$Tag[i] ), 
                        Weight = profileDF$Score[[i]]) } )
        sparseMatrix( i = df$Index, j = rep(1,nrow(df)), x = df$Weight, dims = c( ncol(smr$M), 1 ) )
      }
    } else {
      if ( sum( tagType %in% smr$TagTypes ) == 0 ) {
        stop( "Unknown tag type value for the argument 'tagType'.", call. = TRUE )
      }
      cnames <- colnames(smr$M)[ smr$TagTypeRanges[tagType,"Begin"] : smr$TagTypeRanges[tagType,"End"] ]
      profileDF <- profileDF[ profileDF$Tag %in% cnames, ]
      if ( nrow(profileDF) == 0 ) {
        warning( "None of the given tags belong to the specified tag type. Returning 0.", call. = TRUE )
        return( 0 )
      }
      inds <- which( cnames %in% profileDF$Tag )
      inds <- inds + (smr$TagTypeRanges[tagType,"Begin"] - 1)
      sparseMatrix( i = inds, j = rep(1,nrow(profileDF)), x = profileDF$Score, dims = c( ncol(smr$M), 1 ) )
    }
  } else {
    stop( "Expected a data frame with names c('Score','Index','Tag'), c('Score','Index'), or c('Score','Tag').", call. = TRUE )
  }
}


#' @description Gives the interpetation of a data frame of recommendations with sparse matrix recommender object
#' @param smr sparse matrix recommender object
#' @param recs a data frame of recommendations with column names Score and Index
#' @parame tagTypes which tag types to use
SMRItemData <- function(smr, recs, tagTypes=NULL) {
  if ( is.null(tagTypes) ) {
    sm <- smr$M[recs$Index,]
  } else {
    sm <- smr$M[recs$Index, ]
    sms <- llply( tagTypes, function(tg) sm[,smr$TagTypeRanges[tg, "Begin"]:smr$TagTypeRanges[tg, "End"]] )
    sm <- do.call(cBind, sms)
  }
  pt <- as.data.frame(summary(sm))
  pt <- pt[ order(pt[,1]), ]
  pt[,1]<-rownames(sm)[pt[,1]]
  pt[,2]<-colnames(sm)[pt[,2]]
  names(pt) <- c(names(recs)[[3]], "Metadata", "Weight")
  # Now we can use split(pt, factor(pt$Item))
  unique(pt)
}


#' @description Finds the tag type of a tag
#' @param smr a sparse matrix recommender object
#' @param tag a tag (string) for which we want to find the tag type
#' @param tag type ID (string) or NULL
SMRTagType <- function( smr, tag ) {
  if ( is.numeric(tag) || is.integer(tag) ) {
    tagInd <- tag
  } else {
    if ( tag %in% colnames(smr$M) ) {
      tagInd <- which( colnames(smr$M)==tag )
    } else if ( tag %in% rownames(smr$M) ) {
      return(smr$ItemColumnName)
    } else {
      return("None")
    }
  }
  
  if ( length(tagInd) == 1 ) {
    tagTypeInd <- which( smr$TagTypeRanges$Begin <= tagInd & tagInd <= smr$TagTypeRanges$End  )
  } else {
    tagTypeInd <- laply( tagInd, function(x) which( smr$TagTypeRanges$Begin <= x & x <= smr$TagTypeRanges$End ) )
  }
  
  if ( length( tagTypeInd ) >= 1 ) {
    smr$TagTypes[ tagTypeInd ]
  } else {
    "None"
  }
}

#' @description Re-orders a list of recommendations according to their weighted intersection with a list of tags.
#' @param smr a sparse matrix recommender object
#' @param recs a data frame recommended items, the second column being row names or row indices
#' @param tagIDs a vector tag ID's of indices with which the recommendations are scored
#' @detail The first column is expected to be of scores. The original Mathematica package function is named InterfaceUserToLoveFiltered.
SMRReorderRecommendations <- function( smr, recs, tagIDs ) {
  if ( is.character( tagIDs ) && length( tagIDs ) > 0 ) {
    ## Assuming column ID's of smr$M
    tagInds <- which( colnames(smr$M) %in% tagIDs )
  } else if ( is.numeric( tagIDs ) && length( tagIDs ) > 0 ) {
    tagInds <- tagIDs
  } else {
    stop( "The third argument, tagIDs, is expected to be a non-empty vector of column indices or column ID's.", call.=TRUE )
  }
  
  profileVec <- sparseMatrix( i=tagInds, j=rep(1,length(tagInds)), x=rep(1,length(tagInds)), dims = c( ncol(smr$M), 1 ) )
  
  newOrder <- smr$M[recs[[2]], ] %*% profileVec
  
  if ( sum( newOrder ) > 0 ) {
    newOrder <- rev( order( as.vector(newOrder) ) )
    recs[ newOrder, ]
  } else {
    recs
  }
}


#' @description Find the metadata tags that would explain or justify the recommendations
#' @param smr a sparse matrix recommendation object
#' @param toBeLovedItem an ID of a item or its index in smr$M
#' @param profile a data frame that is the profile of the customer with columns c("Score", "Index", "Tag" )
#' @param normalizeScores logical value should the scores be normalized with max(res$Score)
#' @param style is one of "intersection", "multiplication"
#' @return a data frame with columns names c("Score", "Index", "Tag" )
SMRMetadataProofs <- function( smr, toBeLovedItem, profile,
                               normalizeScores = TRUE,
                               style = "intersection" ) {
  
  if ( is.null(style) ) {
    style = "intersection"
  }
  
  prodVec <- smr$M[ toBeLovedItem, , drop = FALSE ]
  
  if ( style == "intersection" ) {
    prodVec@x <- rep(1, length(prodVec@x) )
  }
  
  pvec <- SMRProfileDFToVector( smr, profile )
  
  ## SMRProfileDFToVector returns a column vector that is why its result is transposed here
  pvec <- prodVec * t(pvec)
  
  res <- SMRProfileDFFromVector( smr, pvec )
  
  ## guarding a bug where res is a rowless data frame
  if(nrow(res) > 0){
    if (normalizeScores ) {
      res$Score <- res$Score / max(res$Score)
    }
    return( res )
  } else {
    return( NULL )
  }
}


#' @description Find the items of the history that are the closest to a recommendation
#' @param smr a sparse matrix recommendation object
#' @param toBeLovedItem an ID of a item or its index in smr$M
#' @param history a data frame that is the customer purchasing history with columns c( Score, <some-item-ID> )
#' @param normalizeScores logical value should the scores be normalized with max(res$Score)
#' @return a data frame with columns names c("Score", <some-item-id> )
SMRHistoryProofs <- function( smr, toBeLovedItem, history, normalizeScores=TRUE ) {
  
  # there should be a better way of making sparse matrix or vector
  # from a row of a sparse matrix
  #   prodRow <- smr$M[toBeLovedInd,]
  # Replace with  smr$M[toBeLovedItem,,drop=FALSE]
  prodRow <- smr$M[toBeLovedItem,]
  nzInds <- which( prodRow > 0 )
  prodVec <- sparseMatrix( i=nzInds, j=rep(1,length(nzInds)), x = prodRow[nzInds], dims=c( ncol(smr$M), 1 ) )
  
  vInds <- laply( history[,2], function(x) which(rownames(smr$M)==x) )
  scores <- smr$M[ vInds, ] %*% prodVec
  scores <- scores * history[,1]
  
  nzInds <- which( scores > 0 )
  
  # if all scores are zero give a warning and return an empty data frame
  if ( length(nzInds) == 0 ) {
    warning("All scores are zero", call.=TRUE)
    res <- data.frame( Score=numeric(0), Index=integer(0), y=character(0) )
    names(res) <- c("Score", "Index", names(history)[[2]] )
    return(res)
  }
  
  prods <- rownames(smr$M)[vInds][ nzInds ]
  prodInds <- (1:nrow(smr$M))[vInds][ nzInds ]
  scores <- scores[ nzInds ]
  
  res <- as.data.frame( scores );
  res <- cbind( res, prodInds, prods )
  names(res) <- c("Score", "Index", names(history)[[2]] )
  if ( normalizeScores ) {
    if ( as.numeric( t(prodVec) %*% prodVec ) > 0 ) {
      res$Score <- res$Score / ( max(history[,1]) * as.numeric( t(prodVec) %*% prodVec ) )
    } else {
      res$Score <- res$Score / max(res$Score)
    }
  }
  
  res <- res[rev(order(res$Score)),]
  res
}


#' @description Creates an SMR object from a given SMR object by removing specified tag types
#' @param smr a sparse matrix recommender object
#' @param removeTagTypes a list of tag types to be removed from smr
SMRRemoveTagTypes <- function( smr, removeTagTypes ) {
  
  ## Copy of the SMR
  newSMR <- smr
  
  ## There are several ways to do this:
  ## 1. Work with newSMR$TagTypeRanges, take the indices corresponding to tag types not to be removed.
  ## 2. Construct a metadata matrix by taking sub-matrices of the tag types not to be removed.
  pos <- ! ( newSMR$TagTypes %in% removeTagTypes )
  
  applySFs <- SMRCurrentTagTypeSignificanceFactors( newSMR )[pos]
  
  newSMR$M01 <-
    Reduce( function( mat, tt )
      if ( is.null(mat) ) { newSMR$M01[, newSMR$TagTypeRanges[tt,]$Begin : newSMR$TagTypeRanges[tt,]$End ] }
      else { cBind( mat, newSMR$M01[, newSMR$TagTypeRanges[tt,]$Begin : newSMR$TagTypeRanges[tt,]$End ] ) },
      newSMR$TagTypes[pos], NULL )
  newSMR$TagTypeRanges <- newSMR$TagTypeRanges[pos, ]
  newSMR$TagTypes <- newSMR$TagTypes[pos]
  
  widths <- newSMR$TagTypeRanges$End - newSMR$TagTypeRanges$Begin + 1
  ends <- cumsum(widths)
  begins <- ends - widths + 1
  newSMR$TagTypeRanges <- data.frame( Begin=begins, End=ends)
  rownames(newSMR$TagTypeRanges) <- newSMR$TagTypes
  
  newSMR$TagToIndexRules <- setNames( 1:ncol(newSMR$M01), colnames(newSMR$M01) )
  newSMR$ItemToIndexRules <- setNames( 1:nrow(newSMR$M01), rownames(newSMR$M01) )  
  
  newSMR$M <- SMRApplyTagTypeWeights( newSMR, applySFs )
  
  newSMR
}

##===========================================================
## SMR algebra operations
##===========================================================  

#' @description Makes sure that the rows of a matrix are in 1-to-1 correspondence to an array of row ID's
#' @param rowIDs an array of row ID's
#' @param smat a matrix with named rows
SMRImposeRowIDs <- function( rowIDs, smat ) {
  
  missingRows <- setdiff( rowIDs, rownames(smat) )
  nMissingRows <- length( missingRows )
  
  if ( nMissingRows > 0 ) {
    # Rows are missing in the matrix
    complMat <- sparseMatrix(i=c(1), j=c(1), x=c(0), dims = c( nMissingRows, ncol(smat) ) )
    
    rownames(complMat) <- missingRows
    colnames(complMat) <- colnames(smat)
    
    smat <- rBind( smat, complMat )
  }
  # At this point each element of rowIDs should have a corresponding row in the matrix
  smat[rowIDs,,drop=FALSE]
}

#' @description Makes sure that the rows of a matrix are in 1-to-1 correspondence to an array of row ID's
#' @param colIDs an array of col ID's
#' @param smat a matrix with named columns
SMRImposeColumnIDs <- function( colIDs, smat ) {
  
  t( SMRImposeRowIDs( colIDs, t(smat)) )
}

#' @description Annex a sub-matrix to the metadata matrix of an SMR object.
#' @param smr a sparse matrix recommender object
#' @param newSubMat the new sub-matrix to be annexed
#' @param newTagType the tag type associated with the new sub-matrix
SMRAnnexSubMatrix <- function( smr, newSubMat, newTagType ) {
  
  if ( nrow( newSubMat ) != nrow( smr$M ) ) {
    stop( "The metadata matrix of the SMR object and the new sub-matrix should have the same number of rows.", call. = TRUE )
  }
  
  newSMR <- smr
  
  newSMR$TagTypeRanges <- rbind( newSMR$TagTypeRanges, data.frame( Begin = ncol(newSMR$M) + 1, End = ncol(newSMR$M) + ncol(newSubMat) ) )
  rownames(newSMR$TagTypeRanges) <- c( rownames(newSMR$TagTypeRanges)[-nrow(newSMR$TagTypeRanges)], newTagType )
  
  newSMR$M <- cBind( newSMR$M, newSubMat )
  newSMR$M01 <- cBind( newSMR$M01, newSubMat )
  
  newSMR$TagTypes <- c( newSMR$TagTypes, newTagType )
  
  newSMR
}


#' @describtion Join two SMR objects
#' @param smr1 the first SMR object
#' @param smr2 the second SMR object
#' @param colnamesPrefix1 the prefix to be concatenated to the colnames of the first SMR object
#' @param colnamesPrefix2 the prefix to be concatenated to the colnames of the second SMR object
SMRJoin <- function( smr1, smr2, colnamesPrefix1 = NULL, colnamesPrefix2 = NULL ) {
  
  if ( nrow( smr1$M ) != nrow( smr2$M ) ) {
    ## The rownames should be the same too.
    stop( "The metadata matrices of the SMR objects have to have the same number of rows.", call. = TRUE )
  }
  
  ## The rownames should be the same too.
  if ( mean( rownames( smr1$M ) == rownames( smr2$M ) ) < 1 ) {
    stop( "The metadata matrices of the SMR objects should have the same rownames.", call. = TRUE )
  }
  
  newSMR <- smr1
  
  ranges <- smr2$TagTypeRanges
  ranges$Begin <- ranges$Begin + smr1$TagTypeRanges$End[nrow(smr1$TagTypeRanges)]
  ranges$End <- ranges$End + smr1$TagTypeRanges$End[nrow(smr1$TagTypeRanges)]
  
  newSMR$TagTypeRanges <- rbind( smr1$TagTypeRanges, ranges )
  rownames(newSMR$TagTypeRanges) <- c( paste( colnamesPrefix1, rownames(smr1$TagTypeRanges), sep=""), paste( colnamesPrefix2, rownames(smr2$TagTypeRanges), sep="") )
  
  newSMR$M <- cBind( smr1$M, smr2$M )
  newSMR$M01 <- cBind( smr1$M01, smr2$M01 )
  
  newSMR$TagTypes <- c( paste( colnamesPrefix1, smr1$TagTypes, sep=""), paste( colnamesPrefix2, smr2$TagTypes, sep="") )
  
  colnames(newSMR$M) <- c( paste( colnamesPrefix1, colnames(smr1$M), sep="" ), paste( colnamesPrefix2, colnames(smr2$M), sep="" ) )
  colnames(newSMR$M01) <- c( paste( colnamesPrefix1, colnames(smr1$M01), sep="" ), paste( colnamesPrefix2, colnames(smr2$M01), sep="" ) )
  
  newSMR
}

##===========================================================
## Transformations to data frames
##===========================================================  

#' @description Makes a data frame of a sparse matrix
#' @param smr a sparse matrix object
#' @param tagType tag type 
SMRSparseMatrixToDF <- function( smr, tagType  ) {
  
  if( !(tagType %in% smr$TagTypes) ) {
    stop("The parameter tagType is not of the tag types of the SMR object.")
  }
  
  smat <- SMRSubMatrix( smr = smr, tagType = tagType )
  df <- summary(smat) 
  df <- df[ df$x > 0, ]
  
  df <- data.frame(  Rownames = rownames(smat)[df$i], Colnames = colnames(smat)[df$j], Weight = df$x, stringsAsFactors = FALSE )
  
  names(df) <- c( smr$ItemColumnName, tagType, "Weight")
  df
}

#' @description Long form of the data frame
#' @param smr a sparse matrix object
#' @param tagTypes the tag types to make the data frame with
#' @param .progress progress argument for plyr::llply
SMRMatricesToLongDF <- function( smr, tagTypes = NULL, .progress = "none" ) {
  
  if ( is.null(tagTypes) ) { tagTypes = smr$TagTypes }

  dfs <- 
    llply( tagTypes, function(tt) {
      df <- SMRSparseMatrixToDF(smr, tt)
      if ( nrow(df) == 0 ) { NULL }
      else {
        names(df) <- c( smr$ItemColumnName, "Value", "Weight")
        cbind(df, TagType = tt, stringsAsFactors = FALSE ) 
      }
    },.progress = .progress )
  
  dfs <- dfs[ !is.null(dfs) ]
  do.call( rbind, dfs )
}

#' @description Long form of the data frame
#' @param smr a sparse matrix object
#' @param tagTypes the tag types to make the data frame with
#' @param .progress progress argument for plyr::llply
SMRMatricesToWideDF <- function( smr, tagTypes = NULL, sep = ", ", .progress = "none" ) {
  df <- SMRMatricesToLongDF( smr, tagTypes, .progress = .progress )
  dfCast <- reshape2::dcast( data = df, 
                             formula = as.formula( paste( smr$ItemColumnName, " ~ TagType " ) ), 
                             value.var = "Value", fun.aggregate = function(x) paste(x, collapse = sep ) )
}


##===========================================================
## Overloading predict
##===========================================================  

#' @description Classify a data frame or matrix based on a SMR object.
#' @param smr a SMR object
#' @param data a matrix or a data frame
#' @param type what kind of result to be returned: 'raw' returns a matrix, 'decision' a vector of labels
#' @param normalized should the results be normalized or not if type = 'raw'
#' @details The SMR object can have additional parameter tucked-in, see smr['ClassifierParameters'].
predict.SMR <- function( smr, data, type = "decision", normalized = TRUE, ... ) {

  if( !is.data.frame(data) && !is.matrix(data) ) {
    stop( "The second argument is expected to be a matrix or a data frame.", call. = TRUE )
  }
  
  if( is.data.frame(data) ) {
    dataMat <- SMRCreate( dataRows = data, tagTypes = setdiff( colnames(data), smr$ItemColumnName), itemColumnName = smr$ItemColumnName )
    dataMat <- dataMat$M
  }
  
  ## There should be a check is dataMat a sparse matrix.
  dataMat <- SMRImposeColumnIDs( colIDs = colnames(smr$M), smat = dataMat )
    
  dotArgs <- list(...)
  
  clParams <- if( "ClassifierParameters" %in% names(smr) ) { smr["ClassifierParameters"] } else { NULL }

  tagType <- if( "tagType" %in% names(dotArgs) ) { dotArgs[["tagType"]] } 
  else if( "tagType" %in% names(clParams) ) { clParams[["tagType"]] } 
  else { smr$TagTypes[[length(smr$TagTypes)]] }
  
  nTopNNs <- if( "nTopNNs" %in% names(dotArgs) ) { dotArgs[["nTopNNs"]] } 
  else if( "nTopNNs" %in% names(clParams) ) { clParams[["nTopNNs"]] } 
  else { 20 }
  
  voting <- if( "voting" %in% names(dotArgs) ) { dotArgs[["voting"]] } 
  else if( "voting" %in% names(clParams) ) { clParams[["voting"]] } 
  else { FALSE }
  
  dropZeroScoredLabels <- if( "dropZeroScoredLabels" %in% names(dotArgs) ) { dotArgs[["dropZeroScoredLabels"]] } 
  else if( "dropZeroScoredLabels" %in% names(clParams) ) { clParams[["dropZeroScoredLabels"]] } 
  else { TRUE }
  
  if( tolower(type) == "decision" ) { 
  
    laply( 1:nrow(data), function(i) {

      pvec <- dataMat[i,,drop=F]
      
      recs <- SMRClassifyByProfileVector( smr = smr, tagType = tagType, profileVec = pvec, 
                                         nTopNNs = nTopNNs, voting = voting, 
                                         dropZeroScoredLabels = dropZeroScoredLabels)
      
      if( length(recs) == 0 || is.null(recs)) { NA } else { as.character(recs$Label)[[1]] }

    } )
      
  } else if ( tolower(type) %in% c( "raw", "scores" ) ) {
    
    res <-
      ldply( 1:nrow(data), function(i) {
      
        pvec <- dataMat[i,,drop=F]
        
        recs <- SMRClassifyByProfileVector( smr = smr, tagType = tagType, profileVec = pvec, 
                                            nTopNNs = nTopNNs, voting = voting, 
                                            dropZeroScoredLabels = dropZeroScoredLabels)
        
        if( normalized && sum(recs$Score) > 0 ) { recs$Score <- recs$Score / sum(recs$Score)}
        
        cbind( Index = i, recs )
    } )
    
    as.matrix( xtabs( Score ~ Index + Label, res, sparse = T ) )
  }
    
}

#=======================================================================================
# Object-Oriented Programming (OOP) implementations
#=======================================================================================


##===========================================================
## Generic function definition
##===========================================================

## Note that in the functions below the data frames with the recommendations results have (only) the columns "Score" and "Item".
## The more basic recommendations functions return data frames that also have the column "Index", but the indices are not invariant
## across the recommenders. The item names are.

#' @description The generic function for calculating recommendations by history.
#' @param x a recommender object
#' @param historyItems a list of history items (indices or ID's)
#' @param historyRatings a list of history ratings
#' @param nrecs number of required recommendations
#' @param removeHistory should the history be dropped or not
#' @return A data frame with the columns c("Score", "Item")
Recommendations <- function( x, historyItems, historyRatings, nrecs, removeHistory = TRUE, ... ) UseMethod( "Recommendations" )

#' @description Specialization of Recommendations for SMR objects.
Recommendations.SMR <- function( x, historyItems, historyRatings, nrecs, removeHistory = TRUE, ... ) {
  ## Needs handling of the argument tuningParametes.
  res <- SMRRecommendations( smr = x, userHistoryItems = historyItems, userRatings = historyRatings,
                             nrecs = nrecs, removeHistory = removeHistory )
  setNames( res[, c(1,3)], c("Score", "Item") )
}

#' @description The generic function for calculating recommendations by profile.
#' @param x a recommender object
#' @param profileTags a list of profile tags
#' @param profileTagScores a list of scores corresponding to the profile tags
#' @param nrecs number of required recommendations
#' @return A data frame with the columns columns c("Score", "Item")
RecommendationsByProfile <- function( x, profileTags, profileTagScores, nrecs, ... ) UseMethod( "RecommendationsByProfile" )

#' @description Specialization of RecommendationsByProfile for SMR objects.
RecommendationsByProfile.SMR <- function ( x, profileTags, profileTagScores, nrecs, ... ) {
  ## Needs handling of the argument tuningParametes.
  res <- SMRRecommendationsByProfileDF( smr = x,
                                        profile = data.frame( Score = profileTagScores, Tag = profileTags, stringsAsFactors=FALSE),
                                        nrecs = nrecs )
  res[, c("Score", "Item")]
}

#' @description The generic function for calculating a consumption profile.
#' @param x a recommender object
#' @param historyItems a list of history items (indices or ID's)
#' @param historyRatings a list of history ratings
#' @param allColumns a logical are all columns of the results returned or not
#' @return A data frame with the first columns being "Score" and "Tag".
ConsumptionProfile <- function( x, historyItems, historyRatings, allColumns = FALSE, ... ) UseMethod( "ConsumptionProfile" )

ConsumptionProfile.SMR <- function( x, historyItems, historyRatings, allColumns = FALSE, ... ) {
  if( missing(historyRatings) || is.null(historyRatings) ) {
    historyRatings <- rep( 1, length(historyItems) )
  }
  if( allColumns ) {
    SMRProfileDF( x, data.frame( Rating = historyRatings, Item = historyItems, stringsAsFactors = FALSE ) )[, c("Score", "Tag", "Index")]
  } else {
    SMRProfileDF( x, data.frame( Rating = historyRatings, Item = historyItems, stringsAsFactors = FALSE ) )[, c("Score", "Tag")]
  }
}

#' @description Classify a profile vector into the column names of a tag type sub-matrix.
#' @param x recommender object
#' @param tagType tag type for which the classification is done
#' @param profileVec is a sparse matrix with 1 row (a row from a sparse matrix)
#' @param nTopNNs number of top nearest neighbors to be used in to derive the classification
#' @param voting boolean should simple voting be used or a weighted sum
ClassifyByProfileVector <- function( x, tagType, profileVec, nTopNNs, voting = FALSE ) UseMethod( "ClassifyByProfileVector" )

#' @description Specialization of ClassifyByProfileVector for SMR objects.
ClassifyByProfileVector.SMR <- function ( x, tagType, profileVec, nTopNNs, voting = FALSE ) {
  SMRClassifyByProfileVector( smr = x, tagType = tagType, profileVec = profileVec, nTopNNs = nTopNNs, voting = voting )
}

##===========================================================
## Composite pattern for recommenders combination
##===========================================================
## Here is way to construct a composite recommender object:

# rcObj <- list( Recommenders = list( "SMR1" = smr1, "SMRFreq1" = smrFreq1, "SMR2" = smr2, "SMR3" = smr3 ), Weights = c(1,0.5,1,1), 
#               NormalizationType = "quantileIntervals", MergeFunction = length )
# class(rcObj) <- "CompositeRecommender"

#' @description Calculate recommendations over a composite recommender object.
#' @param x a recommender object
#' @param historyItems a list of history items
#' @param historyRatings a list of history ratings
#' @param nrecs number of required recommendations
#' @param removeHistory should the history be removed or not
#' @param normalizationType normalization type, one of NULL, 'none', 'max', 'rank', 'quantileIntervals', 
#' or 'shiftAndRescale' (same as NULL)
#' @param mergeFunction a function to merge the recommendations lists, a function that can be applied to 
#' a vector of scores corresponding to an item.
#' @details If the argument normalizationType is NULL, then the object's element 'NormalizationType' is used. 
#' If that is NULL too, then 'shiftAndRescale' is used. Examples of values of mergeFunction are 'sum', 
#' 'max', 'mean', 'median', 'length'. If mergeFunction is NULL, then the object's element 'MergeFunction' 
#' is used. If that is NULL too, then sum is used.
Recommendations.CompositeRecommender <- function( x, historyItems, historyRatings, nrecs, removeHistory = TRUE, 
                                                  normalizationType = NULL, mergeFunction = NULL, ... ) {
  
  ## Computing recommendations with each recommender
  allRecs <- llply( x$Recommenders, function(recObj) Recommendations( recObj, 
                                                                      historyItems = historyItems, 
                                                                      historyRatings = historyRatings, 
                                                                      nrecs = nrecs, 
                                                                      removeHistory = removeHistory, ... ) )
  
  ## Determine weights for the recommenders
  weights <- x$Weights
  if ( is.null( weights ) ) { weights <- rep(1, length( x$Recommenders ) ) }
  if ( length( weights ) < length( x$Recommenders ) ) { weights <- rep_len( weights, length.out = length( x$Recommenders ) ) }
  
  ## Default normalizationType if NULL
  if ( is.null( normalizationType ) ) { normalizationType <- x$NormalizationType }
  if ( is.null( normalizationType ) ) { normalizationType <- "shiftAndRescale" }
  
  ## Default mergeFunction if NULL
  if ( is.null( mergeFunction ) ) { mergeFunction <- x$MergeFunction }
  if ( is.null( mergeFunction ) ) { mergeFunction <- sum }
  
  ## Normalization of scores
  ## Weights for the different recommenders can be used.
  if ( normalizationType == "max" ) {
    
    allRecsDF <- ldply( 1:length(allRecs), function(i) { x <- allRecs[[i]]; x$Score <- weights[i] * ( x$Score / max(x$Score) ); x } )
    
  } else if ( normalizationType == "rank" ) {
    
    maxNRow <- max( laply( allRecs, nrow ) )
    allRecsDF <- ldply( 1:length(allRecs), function(i) { x <- allRecs[[i]]; x$Score <- weights[i] * ( maxNRow - (0:(nrow(x)-1)) ); x } )
    
  } else if ( normalizationType == "quantileIntervals" ) {
    
    ## Note that here are handled quantile levels "probs" if given as an argument.
    args <- list(...)
    if ( !("probs" %in% names(args)) ) { probs <- seq(0,1,0.2) }
    
    allRecsDF <- 
      ldply( 1:length(allRecs), function(i) { 
        x <- allRecs[[i]]
        qs <- quantile( x$Score, probs, na.rm = TRUE )
        x$Score <- weights[i] * findInterval( x = x$Score, vec = qs )
        x
      } )
    
  } else if ( normalizationType == "shiftAndRescale" ) {
    
    ## May be just using scale would suffice.
    ## Note that bottom outliers are removed.
    allRecsDF <- 
      ldply( 1:length(allRecs), function(i) { 
        x <- allRecs[[i]]
        qs <- quantile( x$Score, seq(0,1,0.25), na.rm = TRUE ); 
        if ( qs[4] - qs[2] > 0 ) {
          x$Score <- ( x$Score - qs[3] ) / ( qs[4] - qs[2] ) / 2 + 1
        }
        x$Score[ x$Score < 0 ] <- 0  
        x$Score <- weights[i] * x$Score
        x
      })
    
  } else if ( normalizationType == "none" ) {
    
    allRecsDF <- do.call( rbind, allRecs )
    
  } else {
    stop( "The argument 'normalizationType' is not one of: NULL, 'none', 'max', 'rank', 'quantileIntervals', 'shiftAndRescale'.", call. = TRUE )
  }
  
  allRecsDF <- allRecsDF[ allRecsDF$Score > 0, ]
  ## Note, the merging here is with merge-sum. Other merging can be applied.
  res <- ddply( allRecsDF, "Item", function(x) { data.frame( Score = mergeFunction(x$Score), Item = x$Item[1], stringsAsFactors = FALSE ) } )  
  res <- res[ order(-res$Score), ]
  
  res
}


##===========================================================
## Recommenders items and tags query methods
##===========================================================

RecommenderTags <- function( recommender )  UseMethod("RecommenderTags")
RecommenderTags.SMR <- function( recommender ) colnames( recommender$M )
RecommenderTags.CompositeRecommender <- function( recommender ) unique( unlist( llply( recommender$Recommenders, RecommenderTags ) ) )

RecommenderItems <- function( recommender ) UseMethod("RecommenderItems")
RecommenderItems.SMR <- function( recommender ) rownames( recommender$M )
RecommenderItems.CompositeRecommender <- function( recommender ) unique( unlist( llply( recommender$Recommenders, RecommenderItems ) ) )


