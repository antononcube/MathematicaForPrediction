##=======================================================================================
## Time series recommender interface, ui side
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
##=======================================================================================
## References
##=======================================================================================
## [1] Anton Antonov, Time series recommender framework in R, (2017), GitHub,
##     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/R/TimeSeriesRecommender.R .
##=======================================================================================

library(shinydashboard)
library(DT)
library(arules)

dashboardPage(
  dashboardHeader(title = "Time series dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Nearest neighbors", tabName = "NNs"),
      menuItem("Trend finding", tabName = "TrendFinding")
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem( tabName = "NNs",
               
               selectInput( "searchID", "Entety (diagnose, race, zip code):", rownames(tsSMR$M) ),
               
               numericInput( "numberOfNNs", "Number of NNs:", 12 ),
               
               selectInput( "nnsValueColName", "Value plotting:", c( "Raw" = "Value", "Smoothed" = "Value.ma" ) ),
               
               hr(),
               
               plotOutput( "entetyNNsPlot", height = "1000px" )
               
      ),
      
      tabItem( tabName = "TrendFinding",
               
               selectInput( "searchVectorName", "Search vector type:", names(tsSearchVectors) ),
               
               numericInput( "numberOfSearchResults", "Number of search results:", 12 ),
               
               selectInput( "svecValueColName", "Value plotting:", c( "Raw" = "Value", "Smoothed" = "Value.ma" ) ),
               
               selectInput( "searchVectorColor", "Search vector color:", c("blue", "lightblue", "black", "gray10", "gray25", "gray50", "gray75", "gray90" ), selected = "gray75" ),
               
               hr(),
              
               plotOutput( "searchVectorPlot", height = "150px", width = "550px" ),
               
               plotOutput( "searchVectorNNsPlot",  width = "1200px", height = "800px" )
               
               
      )
    ) 
  )
)