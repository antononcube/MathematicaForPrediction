#=======================================================================================
# Extra functions for the Sparse matrix recommender framework in R
# Copyright (C) 2018  Anton Antonov
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
#
# This file has several functions for extra functionalities for the
# Sparse matrix recommender framework implmented in R, implemented in:
#   https://github.com/antononcube/MathematicaForPrediction/blob/master/R/SparseMatrixRecommender.R
# 
# The first version of this file is based on the functional parsers implemented in:
#   https://github.com/antononcube/MathematicaForPrediction/blob/master/R/FunctionalParsers/FunctionalParsers.R
#
# TODO:
#   1. [ ] Rename the parsers into (much more) specific names.
#   2. [X] Move functions from SparseMatrixRecommender.R to here.
#
# Anton Antonov
# 2018.01.18
#=======================================================================================

library(plyr)
library(devtools)

if( !exists("p.pack") ) {
  source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/FunctionalParsers/FunctionalParsers.R")
}


#' @description Parses a search string into a vector of values or into a named list of tag-type-and-value pairs.
#' @param smr a sparse matrix object
#' @param search search string
#' @details The original version is to parse the string 
#' "State=1, Default=0.2"
#' into
#' c( State=1, City=0.2, ZipCode=0.2 )                 
SMRParseTagTypeValues <- function( tagTypes, search ) {
  
  pres <- eval( parse( text= paste( "c(", search, ")" ) ) )
  
  if ( is.null( names(pres) ) ) {
    pres <- setNames( c( pres, rep( 0, length(tagTypes) - length(pres) ) ), tagTypes )
    return( pres )
  } 
  
  default <- if ( is.na( pres["Default"] ) ) { 0 } else { pres["Default"] }
  cnames <- intersect( tagTypes, names(pres) )
  diffnames <- setdiff( tagTypes, cnames )
  pres <- c( pres[cnames], setNames( rep( default, length(diffnames) ), diffnames ) )
  pres <- pres[ tagTypes ]
  
  pres
}


#' @description Parse a profile specifications of named elements into key-value pairs.
#' @param smr a sparse matrix recommender object; can be NULL
#' @param spec a specification given in the form "Actor=Pitt, Director=Scott"
#' @details Note that repetions of tag type names are ignored.
#' (Because of using parseQueryString.)
SMRParseProfileSpecification <- function( tagTypes, spec ) {
  
  ## Using Shiny's function made for similar purposes.
  pres <- parseQueryString(gsub(",", "&", gsub( "\\s", "", spec ) ) )
  
  if( is.null(smr) ) { return(pres) }
  
  cnames <- intersect( tagTypes, names(pres) )
  
  if( length(cnames) == 0 ) {
    warning( "Empty parse result for the given SMR object.", call. = TRUE )
    return(NULL)
  }
  
  pres[ cnames ]
}  


#' @details Finds the indices corresponding tag specified in a data frame with columns c("TagType", "Tag").
#' @param smr a sparse matrix recommender object
#' @param spec a data frame with columns c("TagType", "Tag")
#' @param ... options to be passed to grep
#' @return Adds the column "Index" to spec. 
#' The column "Index" has column indices of smr$M corresponding to the tags in spec.
SMRFindTagIndexes <- function( smr, spec, ... ) {
  
  ddply( spec, c("TagType","Tag"), function(x) {
    
    if( !( x$TagType[[1]] %in% smr$TagTypes ) ) { 
      warning( paste0( "Not a known tag type: '", x$TagType[[1]], "' ."), call. = TRUE )
      return(NULL) 
    }
    
    a <- smr$TagTypeRanges[ x$TagType[[1]], "Begin" ]
    b <- smr$TagTypeRanges[ x$TagType[[1]], "End" ]
    
    inds <- grep( pattern = x$Tag[[1]], x = colnames(smr$M)[a:b], ... )
    
    if ( length(inds) == 0 ) { 
      warning( paste0( "The tag specification '", x$Tag[[1]], "' for tag type: '", x$TagType[[1]], "' produced an empty match result."), call. = TRUE )
      return(NULL)
    }
    
    data.frame( TagType = x$TagType, TagSpec = x$Tag, Tag = colnames(smr$M)[a:b][inds], Index = inds + a - 1, stringsAsFactors = FALSE )                        
  })
}


#' @details 
#' @param smr a sparse matrix recommender object
#' @param spec a data frame with columns c("TagType", "Tag")
#' @return Adds the column "Index" to spec. 
#' The column "Index" has column indices of smr$M corresponding to the tags in spec.
SMRFindTagIndexesFirst <- function( smr, spec ) {
  
  ddply( presDF, "TagType", function(x) {
    
    a <- smr$TagTypeRanges[ x$TagType[[1]], "Begin" ]
    b <- smr$TagTypeRanges[ x$TagType[[1]], "End" ]
    
    inds <- which( tolower(colnames(smr$M)[a:b]) %in% tolower(x$Tag) )
    inds <- inds + a - 1
    
    data.frame( TagType = x$TagType, Tag = x$Tag, Index = inds, stringsAsFactors = FALSE )                        
  })
}

##===========================================================
## Parsing 
##===========================================================

pWord <- p.apply( function(x) paste(x,collapse=""), p.many( p.pred(function(x) grepl("[[:alnum:]]|[\\.\\|\\*\\^\\$\\-]",x) ) %|%  p.symbol(".") ) )

pEqualSign <- p.symbol("=") %|% p.symbol(":")
pSeparator <- p.symbol(",")
pSpace <- p.symbol(" ")
pSpaces <- p.many( p.symbol(" ") %|% p.symbol(",") )

pQuoteMark <- p.symbol("'") %|% p.symbol("\"") 
pCompWord <- pWord %|% p.apply( function(x) paste(x, collapse = " "), p.pack( pQuoteMark, p.listof( pWord, pSpace ), pQuoteMark ) )

pPair <- 
  p.apply( 
    function(x) { 
      PPAIRRESDF <<- rbind( PPAIRRESDF, data.frame( TagType = unlist(x)[[1]], Tag = unlist(x)[[2]], stringsAsFactors = F))
      x
    },
    p.shortest( pCompWord %&% ( pEqualSign %&>% pCompWord ) )
  )

pSpec <- pPair %&% p.option( p.many( pSpaces %&>% pPair ) )


#' @description Parse key-value specification into a data frame with columns c( "TagType", "TagSpec", "Tag", "Index" )
#' @param smr a sparse matrix recommende object
#' @param spec a specification string
SMRParseTagValueQueryDF <- function( spec ) {
  
  toTokens <- strsplit(spec, "")[[1]]
  
  PPAIRRESDF <<- NULL
  pres <- p.shortest( pSpec )( toTokens )
  
  PPAIRRESDF
}


#' @description 
#' @param smr a sparse matrix recommender object
#' @param geoCoordMat a matrix with geographic coordinates of items
#' @param specGeoCoords coordinates to find profile for
#' @param milesBreaks a sorted vector of distances in miles that specifies how to categorize geo distances
#' @param units can be 'mile', 'miles', 'kilometer', 'kilometers', 'km'
SMRGeoProfile <- function( smr, geoCoordMat, specGeoCoords, milesBreaks = c(2,5,10,15,20,40,50,100,200,300) ) {
  
  toMeters <- 1609.344
  milesIntervalNames = as.character( cut(milesBreaks,c(0,milesBreaks)) )
  
  if( !is.null(geoCoordMat) && ( is.null(rownames(geoCoordMat)) || sum( rownames(geoCoordMat) %in% rownames(smr$M) ) == 0 ) ) {
    warning( "The row names of the geo coordinates matrix, geoCoordMat, do not intersect with the recomender item IDs.", call. = T )
    return(NULL)
  }
  
  dres <- distm( x = matrix( c(specGeoCoords$Lon, specGeoCoords$Lat), ncol = 2), y = geoCoordMat, fun=distGeo )
  names(dres) <- rownames(geoCoordMat)
  
  idres <- length(milesIntervalNames) - findInterval( dres, milesBreaks * toMeters )
  idres <- idres / max(idres)
  
  if( sum(idres > 0) == 0 ) { return(NULL) }
  
  data.frame( Score = idres[ idres > 0 ], Tag = names(dres)[ idres > 0 ], stringsAsFactors = F )
}


#' @description 
#' @param smr a sparse matrix recommender object
#' @param spec a string with key-value specification for recommendations
#' @param nrecs number of recommendations
#' @details If geoCoordMat is NULL or specGeoCoords is NULL the geo recommendations are not computed.
#' If spec is NULL the tag recommendations are not computed.
#' The argument tagGeoSliderRatio is number in [0,1].
SMRGeoSpecRecommendations <- function( smr, spec, nrecs, 
                                       geoCoordMat = NULL, specGeoCoords = NULL,
                                       tagGeoSliderRatio = 0.5,
                                       milesBreaks = c(2,5,10,15,20,40,50,100,200,300)
) {
  
  ## Tags spec vector
  if( !is.null(spec) ) {
    
    qSpecProf <- SMRParseTagValueQueryDF( spec )
    qProf <- SMRFindTagIndexes( smr, qSpecProf, ignore.case = T )
    
    qProf <- data.frame( Score = 1, Index = qProf$Index, Tag = qProf$Tag, stringsAsFactors = F )
    
    tagVec <- SMRProfileDFToVector( smr = smr, profileDF = qProf ) 
    
  } else {
    
    qSpecProf <- NULL
    tagVec <- sparseMatrix( i = c(1), j = c(1), x = c(0), dims = c(ncol(smr$M), 1) )
    
  }
  
  ## Geo coordinates vector
  geoProf <- NULL
  
  if( !is.null(geoCoordMat) && !is.null(specGeoCoords) ) {
    
    geoProf <- SMRGeoProfile( smr, geoCoordMat, specGeoCoords, milesBreaks )
    
  } else if ( !is.null(qSpecProf) && !is.null(geoCoordMat) && sum( c("Lat", "Lon") %in%  qSpecProf$TagType ) == 2 ) {

    lon <- qSpecProf[ qSpecProf$TagType == "Lon", "Tag" ][[1]]
    lat <- qSpecProf[ qSpecProf$TagType == "Lat", "Tag" ][[1]]

    specGeoCoords <- list( Lat=as.numeric(lat), Lon=as.numeric(lon) )
    geoProf <- SMRGeoProfile( smr, geoCoordMat, specGeoCoords, milesBreaks )
  }
  
  if( !is.null(geoProf) ) {
    
    geoVec <- SMRProfileDFToVector( smr = smr, profile = geoProf )
    
  } else {
    
    geoVec <- sparseMatrix( i = c(1), j = c(1), x = c(0), dims = c(ncol(smr$M), 1) )
    
  }
  
  ## Combined recommendations
  SMRRecommendationsByProfileVector( smr = smr, profileVec = tagGeoSliderRatio * tagVec + (1-tagGeoSliderRatio) * geoVec, nrecs = nrecs )
}