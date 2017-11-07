##=======================================================================================
## Time series recommender interface, server side
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
## The code in this interface is to be used with [1].
##
## In order to run this interface.
##
## 1. Create a time series SMR object. 
##    Assign to the varible 'tsSMR' .
##
## 2. Create a time series matrix.
##    Assign to the variable 'tsMat'.
##
## 3. Create a vector with named elements that maps column names to time interval boundaries.
##    Assign to the variable 'tibNameToTIBRulesRules' .
##
## 4. Create a vector with named elements that maps itemID to item names.
##    Assign to the variable 'tibNameToTIBRules' .
## 
## 5. If desired create a another/different list of search vectors to be used in the interface. 
##    Each element of that list is named. Assign to 'tsSearchVectors' .
##    Such vector can be created with:
##       tsSearchVectors <- MakeTimeSeriesSearchVectors( tsMat )
##    using a definition from [1] (which also defines TSPSRCorrelationNNs.)
##
##=======================================================================================
## References
##=======================================================================================
## [1] Anton Antonov, Time series recommender framework in R, (2017), GitHub,
##     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/R/TimeSeriesRecommender.R .
##=======================================================================================


## server.R
library(shinydashboard)
library(DT)

##===========================================================
## server function
##===========================================================
function(input, output, session)  {
  
 
  recResNNs <- reactive(

    TSPSRCorrelationNNs( timeSeriesMat = tsMat, smr = tsSMR, 
                         itemIDtoNameRules = itemIDtoNameRules,
                         searchRowID = input$searchID, nrecs = input$numberOfNNs )
    
  )

  recResSVec <- reactive(
    
    TSPSRCorrelationNNs( timeSeriesMat = tsMat, smr = tsSMR, 
                         itemIDtoNameRules = itemIDtoNameRules,
                         searchVector = tsSearchVectors[[ input$searchVectorName ]], nrecs = input$numberOfSearchResults )
    
  )
  
  recResNNsExtended <- reactive( 
 
    setNames( SparseMatrixToTriplets( smat = tsMat ), c("Entety", "TimeIntervalBoundaryName", "Value" ) ) %>% 
      # mutate( TimeIntervalBoundary = as.POSIXct( TimeIntervalBoundary, format="%Y-%m-%d") ) %>%
      mutate( TimeIntervalBoundary = tibNameToTIBRules[ TimeIntervalBoundaryName ] ) %>%
      dplyr::filter( Entety %in% recResNNs()$ItemID ) %>%
      inner_join( recResNNs(), by = c("Entety" = "ItemID" ) ) %>% 
      arrange( Score, Entety, TimeIntervalBoundary) %>%
      group_by( Entety ) %>%
      mutate( Value.ma = roll_mean(Value, 12, align="right", fill=0) ) %>%
      ungroup()
    
  )
  
  recResSVecExtended <- reactive( 
    
    setNames( SparseMatrixToTriplets( smat = tsMat ), c("Entety", "TimeIntervalBoundaryName", "Value" ) ) %>% 
      # mutate( TimeIntervalBoundary = as.POSIXct( TimeIntervalBoundary, format="%Y-%m-%d") ) %>%
      mutate( TimeIntervalBoundary = tibNameToTIBRules[ TimeIntervalBoundaryName ] ) %>%
      dplyr::filter( Entety %in% recResSVec()$ItemID ) %>%
      inner_join( recResSVec(), by = c("Entety" = "ItemID" ) ) %>% 
      arrange( Score, Entety, TimeIntervalBoundary) %>%
      group_by( Entety ) %>%
      mutate( Value.ma = roll_mean(Value, 12, align="right", fill=0) ) %>%
      ungroup()
    
  )
  
  ## Entety NNs plot 
  output$entetyNNsPlot <- renderPlot( {
    
    ggplot( recResNNsExtended() ) +
      geom_line( aes_string( x = "TimeIntervalBoundary", y = input$nnsValueColName, color = "ItemName" ), na.rm = T ) +
      facet_wrap( ~ reorder(ItemName, -Score), ncol = 2, scales = "free" )
   
  })
  
  
  searchVecPlotDF <- reactive(
    data.frame( TimeIntervalBoundary = tibNameToTIBRules[ colnames(tsMat) ], Value = tsSearchVectors[[input$searchVectorName]] ) 
  )
  
  ## Search vector plot 
  output$searchVectorPlot <- renderPlot( {

    ggplot( searchVecPlotDF() ) +
      geom_line( aes( x = TimeIntervalBoundary, y = Value ), na.rm = T )
    
  })
  
  
  ## Search vector NNs plot 
  output$searchVectorNNsPlot <- renderPlot( {
    
    # ggplot( recResSVecExtended() ) +
    #   geom_line( aes( x = TimeIntervalBoundary, y = Value.ma, color = ItemName ), na.rm = T ) +
    #   facet_wrap( ~ reorder(ItemName, -Score), ncol = 2, scales = "free" )

    valueColumnName <- input$svecValueColName
    searchVecPlotDF2 <- 
      ddply( recResSVecExtended(), "ItemName", function(x) {
        vec = searchVecPlotDF()$Value
        vec = ( vec - min(vec) ) / ( max(vec) - min(vec) ) 
        vec = vec * (max(x[[valueColumnName]]) - min(x[[valueColumnName]])) + min(x[[valueColumnName]])
        res <- data.frame( Score = 1, TimeIntervalBoundary = searchVecPlotDF()$TimeIntervalBoundary, Value.ma = vec, ItemName = x$ItemName[[1]] ) 
        colnames(res) <- c( "Score", "TimeIntervalBoundary", valueColumnName, "ItemName" )
        res
      } )
    
    ggplot( recResSVecExtended()  ) +
      geom_line( aes_string( x = "TimeIntervalBoundary", y = valueColumnName, color = "ItemName" ), na.rm = T ) +
      facet_wrap( ~ reorder(ItemName, -Score), ncol = 2, scales = "free" ) +
      geom_line( data = searchVecPlotDF2, aes_string( x = "TimeIntervalBoundary", y = valueColumnName), color = input$searchVectorColor )
    
    
  })
  
}

