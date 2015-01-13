#=======================================================================================
# Implementation of document-term matrix re-weighting functions in R
# Copyright (C) 2014  Anton Antonov
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
# antononcube@gmail.com, 
# 7320 Colbury Ave, 
# Windermere, Florida, USA.
#

#=======================================================================================
# The developement of this code started with the intention that it will resemble the 
# Mathematica package [1] as closely as possible, but the applications of functions 
# per row or column is very slow in R, so only weight functions that are identified by 
# strings are implemented, like, "IDF", "Cosine", etc.
#
# [1] Anton Antonov, Implementation of document-term matrix construction and re-weighting functions in Mathematica, 
#     Mathematica package, DocumentTermMatrixConstruction.m at MathematicaForPrediction at GitHub , (2013).
#     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m .
#
# History
# Started: September 2013
# Updated: May 2014, June 2014, July 2014, December 2014, January 2015.
##=======================================================================================
## 2014.12.17
## Implemented "Entropy". Fixed implementation of "Normal".
## Added "Binary" (same as "None").
##
##=======================================================================================
## 2015.01.12
## Implemented local weights. Changed the default values for the functons to be NULL.
##
##=======================================================================================


#' @detail Required libraries
require(plyr)
require(reshape2)
require(Matrix)

# Defined for legacy code purposes
SMRApplyGlobalWeightFunction <- function( docTermMat, globalWeightFunction, normalizerFunction ) {
  SMRApplyTermWeightFunctions( docTermMat, globalWeightFunction, NULL, normalizerFunction )
}

#' @description Applies the global weight functions like Inverse Document Frequency (IDF) to the entries of a sparse matrix.
#' @param docTermMat a document-term sparse matrix (dgCMatrix)
#' @param globalWeightFunction global weight finction ID (a string, one of "IDF", "GFIDF", "Normal", "None")
#' @param localWeightFunction global weight finction ID (a string, one of "Binary", "TermFrequency", "Log", "None")
#' @param normalizerFunction normalization weight finction ID (a string, one of "Cosine", "Sum", "None")
#' @return a sparse matrix of class dgCMatrix
#' @detail The implemented global weight function ID's are "IDF", "GFIDF", "Normal", "None".
#' @detail The implemented local weight function ID's are "Binary", "TermFrequency", "Log", "Logarithmic", "None".
#' @detail The implemented normalization function ID's are "Cosine", "Sum", "None".
SMRApplyTermWeightFunctions <- function( docTermMat, globalWeightFunction = NULL, localWeightFunction = NULL, normalizerFunction = NULL ) {
  
  if ( class(docTermMat) != "dgCMatrix" || nrow(docTermMat) < 2 || ncol(docTermMat) < 2 ) {
    stop( "The argument docTermMat is expected to be a sparse matrix with number of rows and columns greater than two.", call.=TRUE)
  }
  
  mat <- docTermMat
  
  if ( globalWeightFunction == "IDF" ) {
    
    # The following line seem to work, but gives messages. Using a direct access assignment instead.
    # mat[ mat>0 ] <- 1
    mat@x <- rep(1,length(mat@x))
    globalWeights <- colSums(mat)
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- log( nrow(mat) / globalWeights )
    
    # restore the original matrix
    mat <- docTermMat
    
  } else if ( globalWeightFunction == "GFIDF" ) {
    
    freqSums <- colSums(mat)
    mat@x <- rep(1,length(mat@x))
    globalWeights <- colSums(mat)
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- freqSums / globalWeights
    
    # restore the original matrix
    mat <- docTermMat
    
  } else if ( globalWeightFunction == "Normal" ) {
    
    globalWeights <- sqrt( colSums( mat*mat ) )
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- 1 / globalWeights
    
  } else if ( globalWeightFunction == "None" || globalWeightFunction == "Binary" ) {
    
    globalWeights <- rep(1, ncol(mat) )
    
  } else if ( globalWeightFunction == "ColumnStochastic" ) {
    
    globalWeights <- colSums(mat)
    globalWeights[ globalWeights == 0 ] <- 1
    globalWeights <- 1 / globalWeights
    
  } else if ( globalWeightFunction == "Entropy" ) {
    
    gfs <- colSums(mat)
    gfs[ gfs == 0 ] <- 1
    pmat <- mat %*% Diagonal( ncol(mat), 1 / gfs )
    lpmat <- pmat
    lpmat@x <- log( lpmat@x ) 
    globalWeights <- 1 + colSums( pmat * lpmat ) / log( nrow(mat) )
    
  } else {
    stop( "Unknown global weight function specification for the argument globalWeightFunction.", call.=TRUE)
  } 
  
  # local weights
  if( missing(localWeightFunction) || is.null(localWeightFunction) ) {
    
    if ( ! ( is.null(globalWeightFunction) || missing(globalWeightFunction) ) ) {
      
      diagMat <- Diagonal(ncol(docTermMat), globalWeights)
      mat <- mat %*% diagMat
      
    }    
    
  } else {
    
    if ( localWeightFunction == "TermFrequency" ||  localWeightFunction == "None" ) {
      ## There is nothing to be done if localWeightFunction is "None" or "TermFrequency".
      
    } else if ( localWeightFunction == "Binary" ) {
      
      mat@x <- rep(1, length(mat@x) )
      
    } else if ( localWeightFunction == "Log" || localWeightFunction == "Logarithmic"  ) {
      
      mat@x <- log( mat@x + 1 )
      
    } else {
      stop( "Unknown local weight function specification for the argument localWeightFunction.", call.=TRUE)
    }
    
    ## Multiply with the global weights
    if ( ! ( is.null(globalWeightFunction) || missing(globalWeightFunction) ) ) {
      
      diagMat <- Diagonal(ncol(docTermMat), globalWeights)
      mat <- mat %*% diagMat
      
    }
  }
  
  # normalizing
  if( !( missing(normalizerFunction) || is.null(normalizerFunction) ) ) { ## || normalizerFunc == identity
    
    if( class(normalizerFunction)[[1]]=="character" ) {
      if ( normalizerFunction == "None" ) {
        
        ## do nothing
        
      } else if ( normalizerFunction == "Cosine" ) {
        
        svec <- sqrt( rowSums( mat * mat ) )
        svec <- ifelse( svec > 0, svec, 1 )
        mat <- mat / svec
        
      } else if ( normalizerFunction == "Sum" || normalizerFunction == "RowStochastic" ) {
        
        svec <- rowSums( mat )
        svec <- ifelse( svec > 0, svec, 1 )
        mat <- mat / svec
        
      } else {
        stop( "Unknown normalizing function specification for the argument normalizerFunction.", call.=TRUE)
      }
    }
  }
  
  # result
  mat
}

#' @description Makes the matrix argument a column stochastic matrix (the sum of each column is 1)
#' @param mat a matrix (sparse or dense)
SMRMakeColumnStochastic <- function( mat ){
  if ( !( class(mat) == "Matrix" || class(mat) == "dgCMatrix") ) {
    stop("Matrix is expected as an argument", call.=TRUE)
  }
  globalWeights <- colSums(mat)
  globalWeights[ globalWeights == 0 ] <- 1
  globalWeights <- 1 / globalWeights
  
  diagMat <- Diagonal(ncol(mat), globalWeights)
  mat <- mat %*% diagMat
  mat
}

#' @description Makes the matrix argument a row stochastic matrix (the sum of each row is 1)
#' @param mat a matrix (sparse or dense)
SMRMakeRowStochastic <- function( mat ){
  if ( !( class(mat) == "Matrix" || class(mat) == "dgCMatrix") ) {
    stop("Matrix is expected as an argument", call.=TRUE)
  }
  globalWeights <- rowSums(mat)
  globalWeights[ globalWeights == 0 ] <- 1
  globalWeights <- 1 / globalWeights
  
  diagMat <- Diagonal(nrow(mat), globalWeights)
  mat <- diagMat %*% mat
  mat
}
