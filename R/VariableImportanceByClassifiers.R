##==========================================================================
##  Variable importance determination by classifiers implementation in R
##  Copyright (C) 2017 Anton Antonov
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>.
##
##  Written by Anton Antonov,
##  antononcube @ gmail . com,
##  Windermere, Florida, USA.
##
##==========================================================================
##   There is a similar Mathematica package in MathematicaForPrediction at GitHub:
##
##   https://github.com/antononcube/MathematicaForPrediction/blob/master/VariableImportanceByClassifiers.m
##
##==========================================================================
##
##   This file has a function that can be used to find the importance of 
##   variables in a data set; see AccuracyByVariableShuffling . 
##
##   This file also has a function for visual representation of 
##   computed accuracies; see VariableImportancePlot .
##
##   -------------------
##   Procedure outline
##   -------------------
##   1. Build a classifier with the training set.
##   2. Verify using the test set that good classification results are obtained.
##   3. If the number of variables (attributes) is k for each i, 1≤i≤k :
##   3.1. Shuffle the values of the i-th column of the test data and find 
##        the classification success rates.
##   4. Compare the obtained k classification success rates between each other
##      and with the success rates obtained by the un-shuffled test data.
##
##   The variables for which the classification success rates are the worst are 
##   the most decisive.
##
##   -------------------
##   Details
##   -------------------
##   AccuracyByVariableShuffling returns two types of data frames:
##     1) one with columns "Variable", "TPR", "FPR", and 
##     2) another with column "Variable", "Accuracy".
##
##   The first type is suitable for Receiver Operating Characteristic (ROC) 
##   interpretation for a given 'success' class label. 
##
##   Because of the ROC approach to classification results the function 
##   has the arguments: successLabel, classificationType, classificationThreshold .
##
##   AccuracyByVariableShuffling also has an argument for variable name prefixes 
##   (with default value NULL) that allows the calculation of the importance
##   of groups of variables. In that case the returned data frame has 
##   the column "VariablePrefix" instead of "Variable".
##
##   There are certain assumptions about the classifier object argument 
##   of AccuracyByVariableShuffling.
##     1) The object has a corresponding S3 definition of predict .
##     2) Object's predict can take the argument type = 'prob', and
##     3) with that argument would return a matrix with columns 
##        (and column names) corresponding to the class labels and
##        entries corresponding to probabilities.
##
##==========================================================================
##   ToDo:
##   1. Program the use of more than success label.
##   2. Provide examples of what to do when predict(... type="prob")
##      does not return a matrix.
##   3. Provide examples of usage.
##
##==========================================================================
library(plyr)
library(ggplot2)

#' @description 
#' @param classifier a classifier object for which there is an S3 predict definition
#' @param testData a data frame with test records, the last column is assumed to have the class labels
#' @param trainColInds training column indices, if NULL all columns of testData are used
#' @param successLabel the label to measure success against, if NULL all class labels are used
#' @param classificationType a value for the type argument of predict
#' @param classificationThreshold if classificationType is "prob" the threshold is used to determine the success label
#' @param varTypePrefixes prefixes of variable types, if NULL the importance of individual variables is returned
#' @param .progress is passed to plyr::ldply
#' @return Several types of data frames a returned depending on the arguments.
#' If varTypePrefixes == NULL and successLabel != NULL a data frame with column names c("Variable", "TPR", "FPR") .
#' If varTypePrefixes == NULL and successLabel == NULL a data frame with column names c("Variable", "Accuracy") .
#' If varTypePrefixes != NULL and successLabel != NULL a data frame with column names c("VariablePrefix", "TPR", "FPR") .
#' If varTypePrefixes != NULL and successLabel == NULL a data frame with column names c("VariablePrefix", "Accuracy") .
AccuracyByVariableShuffling <- function( classifier,
                                         testData, trainColInds = NULL,
                                         predictFunction = function(object, df) predict( object = object, df,  type = "prob" ),
                                         successLabel = NULL, classificationType = "prob", classificationThreshold = 0.5,
                                         varTypePrefixes = NULL, .progress = "none" ) {

  if ( is.null( varTypePrefixes ) ) {
    loopVarNames <-names(testData)[1:(ncol(testData)-1)]
    varColName <- "Variable"
  } else {
    loopVarNames <- varTypePrefixes
    varColName <- "VariablePrefix"
  }

  if ( is.null(trainColInds) ) {
    testDF <- testData
  } else {
    testDF <- testData[ , trainColInds ]
  }

  dataLabelIndex <- ncol(testDF)

  res <-
  ldply( c( "None", loopVarNames ), function(vn) {

    if ( is.null(varTypePrefixes) ) {

      if( vn %in% colnames(testDF) ) {
        testDF[ , vn ] <- sample( testDF[ , vn ] )
      }

    } else {

      if ( vn != "None" ) {
        testDF <- Reduce( f = function(rdf, v) { rdf[, v] <- sample( rdf[, v]); rdf },
        init = testDF,
        x =  grep( pattern = paste0( "^", vn), x = names(testDF), fixed = F ) )
      }

    }

    if ( !is.null( successLabel ) ) {

      # This is expected to return a matrix the columns of which correspond to class labels
      # and the entries are probabilities.
      ## clRes <- predict( classifier, testDF[, -dataLabelIndex ],  type = "prob" )
      clRes <- predictFunction( classifier, testDF[, -dataLabelIndex ] )

      # The alternative of this check is more robust.
      # if ( ! is.null(classfier$classes) && !( successLabel %in% classfier$classes ) ) {
      if ( ! (successLabel %in% colnames(clRes) ) ) {
        stop( "The given successLabel is not in classifier$classes", call. = TRUE )
      }

      nonSuccessLabel <- paste0( "Not.", successLabel )

      testLabels <- as.character(testDF[ , dataLabelIndex])
      testLabels[ testLabels != successLabel ] <- nonSuccessLabel

      clSuccessDF <- xtabs( ~  Label + Predicted,
      data.frame( Predicted = ifelse( clRes[,successLabel] >= classificationThreshold, successLabel, nonSuccessLabel ),
      Label = testLabels,
      stringsAsFactors = FALSE) )
      clSuccessDF <- clSuccessDF / rowSums(clSuccessDF)

      if( !( successLabel %in% colnames(clSuccessDF) ) ) {
        clSuccessDF <- cbind( successLabel = 0, clSuccessDF)
        colnames( clSuccessDF ) <- c( successLabel, colnames(clSuccessDF)[-1] )
      }
      if( !( nonSuccessLabel %in% colnames(clSuccessDF) ) ) {
        clSuccessDF <- cbind( nonSuccessLabel = 0, clSuccessDF )
        colnames( clSuccessDF ) <- c( nonSuccessLabel, colnames(clSuccessDF)[-1] )
      }

      data.frame( "Variable" = vn, "TPR" = clSuccessDF[ successLabel, successLabel ], "FPR" = clSuccessDF[ nonSuccessLabel, successLabel ],
      stringsAsFactors = FALSE)

    } else {

      clRes <- predict( classifier, testDF[, -dataLabelIndex ] )
      # clSuccessDF <- xtabs( ~  Label + Predicted,  data.frame( Predicted = clRes, Label = testDF[ , -dataLabelIndex] ) )
      clSuccess <- as.character(clRes) == as.character( testDF[ , dataLabelIndex] )

      data.frame( "Variable" = vn, "Accuracy" = mean(clSuccess), stringsAsFactors = FALSE )
    }

  }, .progress = .progress )

  colnames(res) <- c( varColName, colnames(res)[-1] )

  ## Sort descendingly, but keep 'None' on top.
  res[ c( 1, 1 + order( res[,2][-1] ) ), ]
}


#' @description
#' @param classResMat classification matrix with probabilities
#' @param range range of thresholds
#' @param .progress parameter for ldply
#' @return Returns a data frame with three columns: Threshold, TPR, FPR.
#' @details The first column of the argument classResMat is assumed to be the success label.
ROCValues <- function( classResMat, testLabels, range = seq(0,1,0.05), .progress = "none" ) {

  successLabel <- colnames(classResMat)[[1]]
  nonSuccessLabel <- paste0( "Not.", successLabel )

  testLabels[ testLabels != successLabel ] <- nonSuccessLabel

  ldply( range, function(th) {

    clSuccessDF <- xtabs( ~  Label + Predicted,
    data.frame( Predicted = ifelse( classResMat[,successLabel] >= th, successLabel, nonSuccessLabel ),
    Label = testLabels,
    stringsAsFactors = FALSE) )
    clSuccessRateDF <- clSuccessDF / rowSums(clSuccessDF)

    if( !( successLabel %in% colnames(clSuccessRateDF) ) ) {
      clSuccessRateDF <- cbind( successLabel = 0, clSuccessRateDF)
      colnames( clSuccessRateDF ) <- c( successLabel, colnames(clSuccessRateDF)[-1] )
    }
    if( !( nonSuccessLabel %in% colnames(clSuccessRateDF) ) ) {
      clSuccessRateDF <- cbind( nonSuccessLabel = 0, clSuccessRateDF )
      colnames( clSuccessRateDF ) <- c( nonSuccessLabel, colnames(clSuccessRateDF)[-1] )
    }

    data.frame( "Threshold" = th,
    "TPR" = clSuccessRateDF[ successLabel, successLabel ],
    "FPR" = clSuccessRateDF[ nonSuccessLabel, successLabel ],
    "TP"  = clSuccessDF[ successLabel, successLabel ],
    "TN"  = clSuccessDF[ nonSuccessLabel, nonSuccessLabel ],
    "FP"  = clSuccessDF[ nonSuccessLabel, successLabel ],
    "FN"  = clSuccessDF[ successLabel, nonSuccessLabel ],
    stringsAsFactors = FALSE)

  }, .progress = .progress )
}

#' @description 
#' @param rocDF a data frame with columns "Threshold", "FPR", "TPR", and optionally a model IDs column
#' @param title title of the plot
#' @param point.text should the par
#' @param modelColumnName the name of the column that has the model IDs
ROCPlot <- function( rocDF, title = NULL, point.text = TRUE, modelColumnName = "Model" ) {
  pres <- 
    if( modelColumnName %in% colnames(rocDF) ) { 
      ggplot(data = rocDF ) + geom_line(aes_string( x = "FPR", y = "TPR", color = modelColumnName ) ) 
    } else { 
      ggplot(data = rocDF ) + geom_line(aes( x = FPR, y = TPR) ) 
    }
  
  pres <- pres +
    xlim(0,1) + ylim(0,1)  +
    ggtitle( title ) + 
    xlab("False Positive Rate (FPR)") + ylab("True Positive Rate (TPR)")
  
  if( point.text ) {
    if( modelColumnName %in% colnames(rocDF) ) { 
      pres <- pres + geom_point( aes_string( x = "FPR", y = "TPR", color = modelColumnName ) )
    } else {
      pres <- pres + geom_point( aes( x = FPR, y = TPR) )
    }
    pres <- pres + geom_text(aes(label = Threshold, x = FPR, y = TPR), hjust=0.5, vjust=-0.5)
  }
  
  pres
}


#' @description Gives visual representation of the vatiable importance data.
#' @param varImportanceData a data frame with variable importance calculations
#' @param nTopVars for how many of the most decisive variables to plot their names
#' @param title title of the plot
#' @details The function produces different plots depending on the columns of 
#' the argument varImportanceData.
VariableImportancePlot <- function( varImportanceData, nTopVars = min(30,nrow(varImportanceData) ), title = NULL ) {

  if( mean( c( "FPR", "TPR" )  %in% colnames( varImportanceData ) ) == 1 ) {

    ggplot( varImportanceData ) +
    geom_point( aes( x = FPR, y = TPR ), color='blue' ) +
    geom_text( data = varImportanceData[1:nTopVars,],
    aes(label = varImportanceData[1:nTopVars,1], x = FPR, y = TPR),
    hjust = 1, vjust = runif(min=-2,max=2,n=nTopVars) ) +
    geom_point( data = varImportanceData[ varImportanceData[,1] == "None", ], aes(x = FPR, y = TPR), color="red" ) +
    ggtitle( title )

  } else {

    df <- cbind( SortIndex = 1:nrow(varImportanceData), varImportanceData[ order(varImportanceData$Accuracy), ] )
    ggplot(df) +
    geom_point( aes( x = SortIndex, y = Accuracy ), color='blue' ) +
    geom_text( data = df[1:nTopVars,],
    aes(label = df[1:nTopVars,2], x = SortIndex, y = Accuracy ),
    hjust = -0.5, vjust = runif(min=-2,max=2,n=nTopVars) ) +
    geom_point( data = df[ df[,1] == "None", ], aes(x = SortIndex, y = Accuracy), color="red" ) +
    ggtitle( title )

  }

}


