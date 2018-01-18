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
# The first version of this file is based in the functional parsers implemented in:
#   https://github.com/antononcube/MathematicaForPrediction/blob/master/R/FunctionalParsers/FunctionalParsers.R
#
# Anton Antonov
# 2018.01.18
#=======================================================================================

library(plyr)
library(devtools)
        
if( !exists("p.pack") ) {
  source_url("https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/R/FunctionalParsers/FunctionalParsers.R")
}

#' @details 
#' @param smr a sparse matrix recommender object
#' @param spec a data frame with columns c("TagType", "Tag")
#' @param ... options to be passed to grep
#' @return Adds the column "Index" to spec. 
#' The column "Index" has column indices of smr$M corresponding to the tags in spec.
FindTagIndexes <- function( smr, spec, ... ) {
  
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
FindTagIndexesFirst <- function( smr, spec ) {
  
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

pWord <- p.apply( function(x) paste(x,collapse=""), p.many( p.pred(function(x) grepl("[[:alnum:]]|[\\.\\|\\*\\^\\$]",x) ) %|%  p.symbol(".") ) )

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
ParseTagValueQueryDF <- function( smr, spec ) {

  toTokens <- strsplit(spec, "")[[1]]
  
  PPAIRRESDF <<- NULL
  pres <- p.shortest( pSpec )( toTokens )
  
  FindTagIndexes( smr, PPAIRRESDF, ignore.case = T )
}