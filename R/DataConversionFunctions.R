##=======================================================================================
## Data conversion functions in R
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
## Start date: April 2014
##
## This file has R functions that help the ingesting of data in R.
## Some of the functions address problematic reading of TSV / CSV files.
## Some are for field paritioning.
## Some are for conversion to sparse matrices.
##
##=======================================================================================

# Load libraries
library(plyr)
library(stringr)
library(reshape2)
library(Matrix)

#' @description Partition combined columns
#' @param dataColumn data vector
#' @param numberOfSplits number of strings into which is field is splitted
#' @param emptyStringReplacement with what value empty strings are replaced
SplitColumnOfTags <- function( dataColumn, numberOfSplits, sep = ",", emptyStringReplacement = "null"  ) {
  spCol <- str_split_fixed( dataColumn, sep, numberOfSplits )
  spCol[ spCol == "" ] <- emptyStringReplacement
  #spCol[ spCol == "null"] <- NA
  ##spCol <- apply( spCol, c(1,2), str_trim)
  spCol <- apply( spCol, c(2), str_trim)
  spCol
}

#' @description Tag table (an R-matrix) into binary incedence sparse matrix
#' @param itemTagMat item-tag table
TagBasketsMatrixIntoItemTagMatrix <- function( itemTagMat ) {
  # Convert to long form
  itRows <-
    adply(itemTagMat, c(1), function(x) {
      row <- x[2:length(x)]
      row <- row[ !is.na(row) ]
      ldply( row, function(y) c(x[1], y))
    })
  itRows$V2[ itRows$V2 == "" ] <- "null"
  # Convert long form to sparse martix
  SMRCreateItemTagMatrix( itRows, "V1", "V2" )
}


#' @description Given a file name reads the data into column data frame
#' @param fname filename assumed with suffix "TSV"
#' @param sep separator
#' @param header a logical value indicating whether the file contains the names of 
#' the variables as its first line
FileColumnsIngest <- function( fname, sep="\t", expectedColumns=3, header=TRUE, apply.iconv = TRUE ) {
  con <- file(fname, "rb")
  if ( apply.iconv ) {
    rawContent <- iconv( readLines(con) )
  } else {
    rawContent <- readLines(con)
  }
  close(con)  # close the connection to the file, to keep things tidy

  # for each line in rawContent
  # count the number of delims and compare that number to expectedColumns
  indexToOffenders <-
    laply(rawContent, function(x) {
      length(gregexpr(sep, x)[[1]]) != (expectedColumns-1)
    })

  # triplets <- read.csv2(rawContent[-indxToOffenders], header=TRUE, sep=cdelim)
  if ( sum(indexToOffenders) > 0 ) {
    rawContent <- rawContent[-indexToOffenders]
  }

  triplets <-
    ldply( rawContent, function(x) {
      if ( is.null(x) ) {
        NULL
      } else {
        res <- strsplit(x, sep )
        if ( is.null(res) || length(res) == 0 ) { NULL } else { res[[1]] }
      }
    } )

  if ( header ) {
    names(triplets) <- triplets[1,]
    triplets <- triplets[-1,]
  }
  triplets
}

#' @description Given a file name reads the data into three-column data frame, removes rows with NA and "null"
#' @param fname file name
#' @param sep separator
FileTripletsIngest <- function( fname, sep="\t" ) {
  lines <- FileColumnsIngest(fname, sep, 3)
  lines[,3] <- as.numeric( lines[,3] )
  lines[,2] <- as.character( lines[,2] )
  lines[,1] <- as.character( lines[,1] )
  lines[ is.na( lines[,3]), 3] <- 0
  lines <- lines[ nchar( lines[,2] ) > 0,  ]
  lines <- lines[ nchar( lines[,1] ) > 0,  ]
  lines <- lines[ complete.cases(lines), ]
}

#' @description Turns the triplet records of a file into a sparse matrix
#' @param fname file name
#' @param sep field separator
#' @param propertiesToStrings should all properties be turned into strings
FileTriplets <-  function( fname, sep="\t", propertiesToStrings=TRUE ) {
  df <- FileTripletsIngest( fname, sep )
  TripletsToSparseArray( df )
}

#' @description Combining the triplets of two files into a sparse matrix
#' @param fname1 name of the first file
#' @param fname2 name of the second file
#' @param sep field separator
#' @param propertiesToStrings should the properties be converted into stings
TwoFilesTriplets <- function( fname1, fname2, sep="\t", propertiesToStrings=TRUE ) {
  df1 <- FileTripletsIngest( fname1, sep )
  df2 <- FileTripletsIngest( fname2, sep )
  TripletsToSparseArray( rbind(df1, df2) )
}

#' @description Turns a data frame of three columns (triplets) into a sparse matrix
#' @param triplets a data frame with three columns
TripletsToSparseArray <-  function( triplets ) {
  itemIDs <- unique( triplets[,1] )
  propertyIDs <- unique( triplets[,2] )
  itemIDToIndex <- 1:length(itemIDs)
  names(itemIDToIndex) <- itemIDs
  propertyIDToIndex <- 1:length(propertyIDs)
  names(propertyIDToIndex) <- propertyIDs
  smat <- sparseMatrix( i=itemIDToIndex[ triplets[,1] ],
                        j=propertyIDToIndex[ triplets[,2] ],
                        x=triplets[,3],
                        dims=c( length(itemIDs), length(propertyIDs) )  )
  rownames(smat) <- itemIDs
  colnames(smat) <- propertyIDs

  # I don't think we need the rules arrays. We can always re-create them if needed.
  #list( Matrix=smat, ItemIDToIndex=itemIDToIndex, PropertyIDToIndex=propertyIDToIndex )
  smat
}

#' @description Converts a sparse matrix to triplets
#' @param smat a sparse matrix
#' @return a data frame of triplets 
SparseMatrixToTriplets <- function( smat ) {
  # Use summary() over sparse matrix.
  # Then using rules over the indices.
  triplets <- summary(smat)

  # Rules
  if( !is.null(colnames(smat)) && !is.null(rownames(smat)) ) {
    rowRules <- 1:nrow(smat)
    names(rowRules) <- rownames( smat )
    colRules <- 1:ncol(smat)
    names(colRules) <- colnames( smat )
    triplets$i <- names( rowRules[ triplets$i ] )
    triplets$j <- names( colRules[ triplets$j ] )
  }

  triplets
}


#' @description Makes sure that the rows of a matrix are in 1-to-1 correspondence to an array of row ID's
#' @param rowIDs an array of row ID's
#' @param smat a matrix with named rows
ImposeRowIDs <- function( rowIDs, smat ) {

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
ImposeColumnIDs <- function( colIDs, smat ) {

  t( ImposeRowIDs( colIDs, t(smat)) )
}

#' @description Make piecewise function for a list of values.
#' The names of the values are used as function's result.
#' If the names are NULL they are automatically assign to be ordinals starting from 1.
#' Similar behavior is provided by the base function findInterval.
#' @param points a list of named values; if the values are not named automatic naming is used
#' @param tags the values to be returned for the ranges defined by points.
#' @details length(points) == length(tags) - 1
MakePiecewiseFunction <- function( points, tags=NULL ) {
  if ( length(points) ==0 || is.null(points) ) {
    warning("NULL of an empty list is given as an argument.", call.=TRUE )
    return( NULL )
  }
  if ( !is.numeric(points) ) {
    warning("The first argument is expected to be a numeric list.", call. =TRUE )
    return( NULL )
  }
  if ( !is.null(tags) && !is.numeric(tags) ) {
    warning("The second argument is expected to be NULL or a numeric list.", call.=TRUE )
    return( NULL )
  }

  points <- sort(points)

  if ( is.null( tags ) ) {
    tags <- 0:length(points)
  }

  funcStr <- paste( "function(x){ ( x <=" , points[1], " ) *", tags[1] )

  for( i in 1:(length(points)-1) ) {
    funcStr <- paste( funcStr, "+  (", points[i], "< x & x <=", points[i+1], ")*", tags[i+1] )
  }

  funcStr <- paste( funcStr, "+ (", points[length(points)], " < x )*", tags[length(points)+1], "}" )

  eval( parse( text=funcStr ) )
}

#' @param itemRows a data frame of flat content data
#' @param tagTypeColName column name of the relationship to be ingested inge in itemRows
#' @param itemIDName 
#' @param nTagsPerField number of tags per field of the column colName in itemRows
IngestMultiValuedDataColumn <- function( itemRows, tagTypeColName, itemIDColName = "ID", nTagsPerField = 12, split = "," ) {

  spdf <- str_split_fixed( itemRows[, tagTypeColName], pattern = split, n=nTagsPerField )
  spdf <- as.data.frame( spdf, stringsAsFactors = FALSE )
  for( i in 1:ncol(spdf) ) {
    spdf[[i]] <- gsub( pattern = "^\\W", replacement = "", spdf[[i]] )
  }
  names( spdf ) <- paste( "tag", 1:nTagsPerField, sep="_" )

  tags.itemRows <- cbind( "id"=itemRows[[itemIDColName]], spdf )

  tags.itemRows$'tag_1'[ tags.itemRows$'tag_1' == "N/A" ] <- NA
  tags.itemRows <- tags.itemRows[ !is.na( tags.itemRows$'tag_1' ), ]

  ## In order to fit the sparse matrix creation
  tags <- unique( do.call(c, spdf) )
  tags <- data.frame( 'id'=1:length(tags), 'name'=tags[order(tags)] )
  tagToIDRules <- tags$id; names(tagToIDRules) <- tags$name
  for( i in 2:ncol(tags.itemRows) ) {
    tags.itemRows[[i]] <- tagToIDRules[ tags.itemRows[[i]] ]
  }
  names(tags.itemRows) <- c( "id", paste( "tag_id", 1:nTagsPerField, sep="_" ) )

  # result
  list( tags = tags, tags.items = tags.itemRows )
}

## For backward compatibility
IngestMovieDataColumn <- IngestMultiValuedDataColumn

#' @param Multi-column data frame id-tag relationship
#' @param idColName the column name of the item ID
#' @param tagTypeColNames names of the tag type column names 
#' @details This does not work if the tagTypeColNames have dash in them.
#' I assume because of the string-to-formula conversion in SMRCreateItemTagMatrix.
#' Obviously, the dependence of the SMRCreateItemTagMatrix can be removed.
ConvertMultiColumnDataFrameToSparseMatrix <- function( multiColDF, itemColName, tagTypeColNames ) {

  emptyColumns <- laply( tagTypeColNames, function(tt) mean( is.na( multiColDF[,tt] ) ) == 1 )

  if ( sum( !emptyColumns ) < 1 ) {
    stop( "All tag columns are empty.", call. = TRUE )
  }
  tagTypeColNames <- tagTypeColNames[ !emptyColumns ]

  ## Find all the sub-matrices with for the tag types
  gmats <- llply( tagTypeColNames, function( tt ) {
    SMRCreateItemTagMatrix( dataRows = multiColDF, itemColumnName = itemColName, tagType = tt )
  } )

  ## Find all tags
  allTags <- unique( unlist( llply( gmats, colnames ) ) )
  allIDs <- unique( unlist( llply( gmats, rownames ) ) )

  ## Impose the tags to all tags matrices
  gmats <- llply( gmats, function(m) { ImposeRowIDs( allIDs, ImposeColumnIDs( allTags, m ) ) })

  ## Sum the tag matrices into one matrix
  gmat <- gmats[[1]]
  for( i in 2:length(gmats) ) { gmat <- gmat + gmats[[i]] }

  gmat
}



#' @description Make categorical representation of the numerical values of a column in a data frame and 
#' produce a matrix with the derived categorical tags as columns and values of a specified data column as rows. 
#' @param data a data frame
#' @param colNameForRows a column name in data for the rows of the result matrix
#' @param colNameForColumns a column name in data for the columns of the result matrix
#' @param breaks the points over which the breaking of data[colNameForColumns] is done
#' @param leftOverlap vector of weights for the neighboring columns to left
#' @param rightOverlap vector of weights for the neighboring columns to right
#' @param colnamesPrefix prefix for the columns names
MakeMatrixByColumnPartition <- function( data, colNameForRows, colNameForColumns, breaks = 10, leftOverlap = NULL, rightOverlap = NULL, colnamesPrefix = "" ) {

  if( is.numeric( breaks ) && length( breaks ) == 1 ) {
    d0 <- min(data[[colNameForColumns]]); d1 <- max(data[[colNameForColumns]])
    breaks <- seq( d0, d1, (d1-d0)/(breaks-1) )
  }

  smat <- data[ , c(colNameForRows, colNameForColumns) ]
  qF <- MakePiecewiseFunction( breaks )
  smat <- cbind( smat, parts = laply( smat[[colNameForColumns]], qF ) )
  smat <- xtabs( as.formula( paste( "~", colNameForRows, "+ parts") ), smat, sparse = TRUE )
  colnames(smat) <- paste( colnamesPrefix, colnames(smat), sep="" )

  if ( !is.null( leftOverlap ) && !is.null( rightOverlap ) ) {
    genMat <- smat
  }

  if ( !is.null( leftOverlap ) ) {

    addMat <- smat
    zeroCol <- sparseMatrix(  i = c(1), j = c(1), x = 0, dims = c( nrow(smat), 1 ) )

    for( w in rev(leftOverlap) ) {
      addMat <- addMat[,2:ncol(addMat)]
      addMat <- cBind( addMat, zeroCol )
      smat <- smat + w * addMat
    }
  }

  if ( !is.null( rightOverlap ) ) {

    if ( is.null( leftOverlap ) ) { addMat <- smat } else { addMat <- genMat }

    zeroCol <- sparseMatrix(  i = c(1), j = c(1), x = 0, dims = c( nrow(smat), 1 ) )

    for( w in rightOverlap ) {
      addMat <- addMat[,1:(ncol(addMat)-1)]
      addMat <- cBind( zeroCol, addMat )
      smat <- smat + w * addMat
    }
  }

  smat
}

#' @description Replaces each a column of a integer matrix with number of columns corresponding to the integer values.
#' The matrix [[2,3],[1,2]] is converted to [[0,1,0,0,0,1],[1,0,0,0,1,0]] .
#' @param mat an integer matrix to be converted to column value incidence matrix.
#' @param rowNames boolean to assign or not the result matrix row names to be the argument matrix row names
#' @param colNames boolean to assign or not the result matrix column names derived from the argument matrix column names
ToColumnValueIncidenceMatrix <- function( mat, rowNames = TRUE, colNames = TRUE ) {

   tmat <- as( mat, "dgCMatrix")
   df <- summary(tmat)
   df <- data.frame(df)
   minInt <- min(mat); maxInt <- max(mat)
   step <- maxInt - minInt + 1

   if( min(df$x) <= 0 ) {
      warning( "The non-zero values of the matrix are expected to be positive integers.", call. = TRUE)
   }

   df$j <- ( df$j - 1 ) * step + df$x
   ## In other words we are doing this:
   ## triplets <- ddply( .data = df, .variables = .(i,j),
   ##                   .fun = function(row) { c(row[[1]], (row[[2]]-1)*step + row[[3]], 1) })

   ## Convinient way to check the implmentation:
   ## sparseMatrix( i = df$i, j = df$j, x = df$x, dims = c( nrow(mat), ncol(mat)*step ) )
   resMat <- sparseMatrix( i = df$i, j = df$j, x = rep(1,length(df$x)), dims = c( nrow(mat), ncol(mat)*step ) )
   
   if ( rowNames ) { rownames(resMat) <- rownames(mat) }
   
   if ( colNames ) { 
     colnames(resMat) <- laply( colnames(mat), function(x) paste(x, minInt:maxInt, sep = ".") )
   }
   
   resMat
}
