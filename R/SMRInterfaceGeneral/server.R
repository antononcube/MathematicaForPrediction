##=======================================================================================
## General Sparse Matrix Recommender Interface, server side
## Copyright (C) 2017  Anton Antonov
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
## This Shiny interface is made to be used with the recommender system implemented in:
##  https://github.com/antononcube/MathematicaForPrediction/blob/master/R/SparseMatrixRecommender.R
##
## The two central objects are:
## 1. 'itemData' -- a data frame with rows corresponding to items, and
## 2. 'itemSMR' -- a sparse matrix recommender object.
##
## The following variables have to be set:
## 1. searchColName -- which column of itemData is going to be used for searching
##      (e.g. "title" );
## 2. itemDataIDColName -- which column of itemData is with item IDs;
##      these IDs are also rownames of itemSMR$M;
## 3. itemDataColNames -- which column names of itemData should be used in the search
##      and recommendation results.
##
##=======================================================================================


library(shiny)
library(DT)


if( !exists("searchColName") || !( searchColName %in% colnames(itemData) ) ) {
  stop( "The variable searchColName is not defined or it is not a column name in itemData.")
}

if( !exists("itemDataIDColName") || !( itemDataIDColName %in% colnames(itemData) ) ) {
  stop( "The variable itemDataIDColName is not defined or it is not a column name in itemData.")
}

if( !exists("itemDataColNames") || mean( itemDataColNames %in% colnames(itemData) ) < 1 ) {
  stop( "The variable itemDataColNames is not defined or not all of its values are column names in itemData.")
}

# searchColName <- "title"
# itemDataIDColName <- "id"
# itemDataColNames <- c("id", "title", "year", "rated", "imdb_rating" )

shinyServer(function(input, output) {

  tagTypeSFactors <- reactive({

    pres <- eval( parse( text= paste( "c(", input$tagTypeSFactors, ")" ) ) )
    if ( is.null( names(pres) ) ) {
      pres <- setNames( c( pres, rep( 0, length(itemSMR$TagTypes) - length(pres) ) ), itemSMR$TagTypes )
    } else {
      default <- if ( is.na( pres["Default"] ) ) { 0 } else { pres["Default"] }
      cnames <- intersect( itemSMR$TagTypes, names(pres) )
      diffnames <- setdiff( itemSMR$TagTypes, cnames )
      pres <- c( pres[cnames], setNames( rep( default, length(diffnames) ), diffnames ) )
      pres <- pres[ itemSMR$TagTypes ]
    }
    pres
  })


  itemListIDs <- reactive({
    ss <- strsplit( input$itemList, split = "\\W", fixed = FALSE )[[1]]
    ss[ nchar(ss) > 0 ]
  })

  itemListRatings <- reactive({
    res <- strsplit( x = input$itemRatings, split = "\\W", fixed = FALSE )[[1]]
    res <- as.numeric( res[ nchar(res) > 0 ] )
    if ( length(res) < length( itemListIDs() ) ) {
      res <- c( res, rep(3, length( itemListIDs() ) - length(res) ) )
    } else if ( length(res) > length( itemListIDs() ) ) {
      res <- res[1:length( itemListIDs() )]
    }
    res
  })

  itemListInds <- reactive({
    which( itemData[[ itemDataIDColName ]] %in% itemListIDs() )
    # pmatch( itemListIDs(), itemData[[ itemDataIDColName ]] )
  })

  mHist <- reactive({
    setNames(
      data.frame( Rating = itemListRatings(),
                  ItemID = as.character( itemListIDs() ),
                  stringsAsFactors = FALSE),
      c("Rating", itemSMR$ItemColumnName) )
  })

  # selectedProfileTags <- reactive({
  #   as.integer( eval( parse( text = input$selectedProfileTags ) ) )
  # })

  selectedProfileTags <- reactive({
    ss <- str_split( input$selectedProfileTags, pattern = ",|\\W")[[1]]
    ss <- ss[ nchar(ss) > 0 ]
    if ( length(ss) == 0 ) { NULL }
    else { as.integer( ss ) }
  })


  output$significanceFactors <- DT::renderDataTable({ datatable({
    data.frame( TagType = names(tagTypeSFactors()), S.Factor = tagTypeSFactors() )
  }, rownames = FALSE, options = list(pageLength = 8 ) ) })

  ## Recommendations
  recommendations <- reactive({

    nrecs <- max( 100, input$nrecs )
    itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors()  )

    if( FALSE ) {
      ## This is much simple for SMR objects:
      res <- SMRRecommendationsDF( itemSMR, mHist(), nrecs )
    } else {
      res <- Recommendations( x = itemSMR, historyItems = mHist()[[2]], historyRatings = mHist()[[1]], nrecs = nrecs )
      ## Because the general S3 function removes the indices we have to add them in order the rest of the code to work
      res <- data.frame( Score = res$Score, Index = pmatch( res$Item, rownames(itemSMR$M), nomatch = 0 ), Item = as.character(res$Item), stringsAsFactors=FALSE)
      names(res) <- c("Score", "Index", itemSMR$ItemColumnName)
    }

    if ( length( selectedProfileTags() ) > 0 ) {
      res <- SMRReorderRecommendations( itemSMR, res, selectedProfileTags() )
    }
    res[1:input$nrecs,]
  })

  userProfile <- reactive({
    itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors() )
    SMRProfileDF( itemSMR, mHist() )
  })

  extendedItemData <- reactive({
    ## Extended data
    tags <- laply( 1:nrow(mHist()), function(i) {
      pdf <- SMRMetadataProofs( smr = itemSMR, toBeLovedItem = mHist()[i,2], profile = userProfile(), normalizeScores = TRUE )
      paste( pdf$Tag, collapse="; ")
    })
    res <- cbind( mHist(), Tags = tags, stringsAsFactors = FALSE )
    res
  })

  ## The list of user consumed items
  output$itemList <- DT::renderDataTable({ datatable({
    res <- itemData[ itemListInds(), itemDataColNames ]
    res <- cbind( res, StarRating=itemListRatings() )
    res
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })

  output$itemListData <- DT::renderDataTable({ datatable({
    extendedItemData()
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })

  ## Using simple title search
  output$view <-  DT::renderDataTable({ datatable({
    inds <- grep( pattern=input$search, iconv( itemData[[searchColName]] ), ignore.case=TRUE )
    res <- itemData[ inds, itemDataColNames ]
    res
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 4, autoWidth = FALSE) ) })


  output$recs <-
    DT::renderDataTable({ datatable({
      ## Do extensions of the recommendations with needed
      resIDs <- recommendations()[[3]]
      res <- merge( x = recommendations(), y = itemData[, itemDataColNames ], by.x = itemSMR$ItemColumnName, by.y = itemDataIDColName, all.x = TRUE )
      ## This is done because merge breaks the order.
      res[ match( resIDs, res[[1]] ), ]
    }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })

  output$recsProofs <-
    DT::renderDataTable({ datatable({
      itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors()  )
      proofs <- laply( recommendations()[[3]], function(x) {
        pdf <- SMRMetadataProofs( smr = itemSMR, toBeLovedItem = x, profile = userProfile(), normalizeScores = TRUE )
        if ( length( selectedProfileTags() ) > 0 ) {
          pdf <- pdf[ pdf$Index %in% selectedProfileTags(), ]
        }
        paste( pdf$Tag, collapse="; ")
      })
      ## Recommendations extensions with proofs
      res <- cbind( recommendations(), Proofs=proofs, stringsAsFactors = FALSE )
      res[-2]
    }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })

  output$recsScoresPlot <- renderPlot({
    plot( recommendations()$Score, main="Recommendation scores", xlab="recommendations row number", ylab="score")
  })

  output$uprofile <- DT::renderDataTable({ datatable({
    itemSMR$M <- SMRApplyTagTypeWeights( itemSMR, tagTypeSFactors() )
    res <- SMRProfileDF( itemSMR, mHist() )
    res$Tag <-  iconv( res$Tag )
    cbind( res, TagType = laply( res$Index, function(x) SMRTagType( itemSMR, x ) ) )
  }, rownames = FALSE, filter = 'top', options = list(pageLength = 12, autoWidth = FALSE) ) })

})
