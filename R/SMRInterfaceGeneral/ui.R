##=======================================================================================
## General Sparse Matrix Recommender Interface, ui side
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
##=======================================================================================

library(shiny)

shinyUI(
  fluidPage(
    
    titlePanel("Item recommendations interface"),
    
    fluidRow(
      
      column( 3,
              submitButton( "Update", icon = icon("refresh") ),
              
              textInput("search", "Search items with:", "A.*"),
        
              textInput("itemList", "Item list:", rownames(itemSMR$M)[1] ),
              
              textInput("itemRatings", "Item star ratings:", "3"),
              
              numericInput("nrecs", "Number of recommendations:", 20)
              
      ),
      
      column( 6,
              h4("Search results"),
              DT::dataTableOutput("view"),
              
              h4("Consumed items list"),
              tabsetPanel(
                tabPanel( "short", DT::dataTableOutput("itemList") ),
                tabPanel( "extended", DT::dataTableOutput("itemListData") )
              )
              
      ),
      
      column( 3, 
              textInput("tagTypeSFactors", "Tag type significance factors:", 
                        paste( "c(", paste( SMRCurrentTagTypeSignificanceFactors( itemSMR ), collapse = ", " ), ")" ) ),
              DT::dataTableOutput("significanceFactors")
      )
    ),
    
    fluidRow( 
      column( 9,
              h4("Recommendations"),
              tabsetPanel(
                tabPanel( "main", DT::dataTableOutput("recs") ),
                tabPanel( "proofs", DT::dataTableOutput("recsProofs") ),
                tabPanel( "scores plot", plotOutput("recsScoresPlot") )
              )
      ),
      column( 3,
              textInput("selectedProfileTags", "Selected tags:", ""),
              
              h4("Profile"),
              DT::dataTableOutput("uprofile")
      )
    )
  )
)

