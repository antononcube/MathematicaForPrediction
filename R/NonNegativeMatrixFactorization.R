##=======================================================================================
## Implementation of the Non-Negative Matrix Factorization algorithm in R
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
## This code has NNMF functions re-programmed from the Mathematica package
## NonNegativeMatrixFactorization.m,
## "Implementation of the Non-Negative Matrix Factorization algorithm in Mathematica",
## https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m .
##
##=======================================================================================
##
## The implementation follows the description of the hybrid algorithm
## GD-CLS (Gradient Descent with Constrained Least Squares) in the article:
##
## Shahnaz, F., Berry, M., Pauca, V., Plemmons, R., 2006.
## Document clustering using nonnegative matrix factorization. Information Processing & Management 42 (2), 373-386.
##
##=======================================================================================
##
## Version 1.0 
## This package contains definitions for the application of 
## Non-Negative Matrix Factorization (NNMF).
##
## The functions is based on the sparse matrix library in R. 
## (The reason I wrote it is there was no R library for NNMF using sparse matrices.)
##
##=======================================================================================
## TODO: Make these functions into an actual R package.
##=======================================================================================

library("Matrix")

#' @description Returns the pair of matrices {W,H} such that V = W H and
#' the number of the columns of W and the number of rows of H are k. 
#' The method used is Gradient Descent with Constrained Least Squares.
#' @param V a sparse matrix
#' @param k the rank for the factorization
#' @param maxSteps maximum steps for the iteration process 
#' @param nonNegative should the factorization be non-negative or not
#' @param epsilon 
#' @param regularizationParameter
#' @param tolerance tolerance for the factorization approximation
#' @param profiling should profiling information be printed during execution or not
NNMF <- function( V, k, maxSteps = 200, nonNegative = TRUE, epsilon = 10^-9., regularizationParameter = 0.01, tolerance = 0.0001, profiling = FALSE ) {
  
  ## Initial values
  nSteps <- 0
  lbd <- regularizationParameter
  m <- nrow(V)
  n <- ncol(V)
  
  ## Initialization
  W <- matrix( runif( n = m*k, min = 0, max = 1 ), nrow = m, ncol = k )
  H <- matrix( rep( 0, k*n ), nrow = k, ncol = n )
  normV <- norm( V, "F"); diffNorm <- 10 * normV;
  
  while ( nSteps < maxSteps && normV > 0 && ( diffNorm / normV > tolerance ) ) {
    
    nSteps <- nSteps + 1
    
    A <- t(W) %*% W + lbd * diag(k)
    T <- t(W)
    bMat <- T %*% V 
    H = solve( A, bMat )
    H = as( H, "sparseMatrix" )
    
    if ( nonNegative ) {
      H@x[ H@x < 0 ] <- 0
    }
    
    W = W * ( V %*% t(H) ) / ( W %*% ( H %*% t(H) ) + epsilon )
    
    diffNorm <- norm( V - W %*% H, "F" )
    
    if( profiling && ( nSteps < 100  || ( nSteps %% 100 == 0) ) ) {
      cat( "\n\tStep = ", nSteps, ", diffNorm = ", diffNorm )
    }
  }

  if ( nonNegative ) {
     W@x[ W@x < 0  ] <- 0
  }
  
  list( W = W, H = H )
}


#' @description Returns a pair of matrices (W1,H1) such that W1 %*% H1 = W %*% H and 
#' the norms of the columns of W1 are 1 if normalizeLeft = TRUE,
#' if normalizeLeft = FALSE the norms of the rows of H1 are 1.
#' @param W the right matrix factor
#' @param H the left matrix factor
#' @param normalizeLeft which factor would have 
NNMFNormalizeMatrixProduct <- function( W, H, normalizeLeft = TRUE ) {
  
  if ( normalizeLeft ) {
    
    d <- laply( 1:ncol(W), function(i) sqrt( W[,i] %*% W[,i] ) )
    S <- Diagonal( x = d )
    dinv <- 1 / ifelse( d == 0, 1, d ); dinv[ d==0 ] = 0
    SI <- Diagonal( x = dinv ) 
    
    list( W = W %*% (SI), H = S %*% H )
    
  } else {
    
    d <- laply( 1:nrow(H), function(i) sqrt( H[i,] %*% H[i,] ) )
    S <- Diagonal( x = d )
    dinv <- 1 / ifelse( d == 0, 1, d ); dinv[ d==0 ] = 0
    SI <- Diagonal( x = dinv ) 
    
    list( W = W %*% S, H = SI %*% H )
  }
}

#' @description Takes the n largest coordinates of vec, finds the corresponding elements in a list of interpretation terms,
#' corresponding to the vector coordinates. By default the interpretation terms are the names of the vector elements.
#' @param n number of elements to be interpreted
#' @param interpretationTerms interpretation terms
NNMFBasisVectorInterpretation <- function( vec, n, interpretationTerms = NULL ) {
  if ( is.null(interpretationTerms) ) { interpretationTerms <- names(vec) }
  inds = rev( order( vec ) )[1:n]
  setNames( vec[inds], interpretationTerms[inds] )
}

