#=======================================================================================
# Recommender test execution functions in R
# Copyright (C) 2014-2016  Anton Antonov
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
# Windermere, Florida, USA.
#
#=======================================================================================


library(parallel)
library(lattice)
library(assertthat)


## Data strucuture
## list(TestID = testID, TestGroupID = testGroupID,
##      PremiseItems = data.frame( Items=c(itemID1,...), Rating=<numeric> ),
##      PremiseTags = data.frame( Tag=c(tag1,...), Score=<numeric> ),
##      NRecs = 20, SignificanceFactors = NULL,
##      ResponseItems = data.frame( Items=c(itemID1,...), Rating=<numeric> ),
##      ResponseTags = data.frame( Tag=c(tag1,...), Score=<numeric> ),
##      CombinationParameter = 0.5,
##      TestingMeasure = "AveragePrecision", TestPassThreshold = 0.4  )
##
## The elements PremiseItems, PremiseTags, ResponseItems, and ResponseTags are expected to be data frames.
## As such they naturally represnt in JSON.
## PremiseItems and ResponseItems must have columns with names "Score" and "Item".
## PremiseTags and ResponseTags must have columns with names "Score" and "Tag".
## The columns "Score" can be interpretted as frequencies when TestingMeasure == "Frequency".

#' @description Provide empty data structure for a test specification.
EmptyTestSpecification <- function() {
  list( TestID = NULL, TestGroupID = NULL,
        PremiseItems = NULL,
        PremiseTags = NULL,
        ResponseItems = NULL,
        ResponseTags = NULL,
        NRecs = 0, SignificanceFactors = NULL,
        CombinationParameter = 0,
        RemoveHistory = FALSE,
        TestingMeasure = "Intersection",
        AveragePrecisionDenominator = "faithful",
        IntersectionFractionDenominator = "response",
        TestPassThreshold = 0
  )
}

#' @description Computes the average precision measure for specified recommendations and relevant items.
#' @param recommendations data frame of scored recommended ID's
#' @param relevantIDs a list/vector of relevant ID's
#' @param topn a number of observed top recommendations; if NULL all are taken
#' @param denominator takes NULL or strings 'faithful'/'relevant', 'recommendations'/'recs'
AveragePrecision <- function( recommendations, relevantIDs, topn=NULL, denominator=NULL ) {
  if ( is.null(recommendations) ) {
    warning("The argument 'recommendations' is NULL.", call. = TRUE )
    return(0)
  }
  if ( is.null(topn) || is.numeric(topn) && ( topn > nrow(recommendations) || topn < 1 ) ) {
    topn <- nrow(recommendations)
    pr <- ifelse( recommendations[,2] %in% relevantIDs, 1, 0)
  } else {
    pr <- ifelse( recommendations[1:topn,2] %in% relevantIDs, 1, 0)
  }
  
  if ( is.null(denominator) ) {
    ##denominator <- 'faithful'
    denominator <- 'recommendations'
  }
  
  if ( is.character(denominator) && ( denominator == 'faithful' || denominator == 'relevant' ) ) {
    1/length(relevantIDs) * sum( cumsum(pr) / (1:topn) * pr )
  } else if ( is.character(denominator) && ( denominator == 'recommendations' || denominator == 'recs' ) ) {
    1/topn * sum( cumsum(pr) / (1:topn) * pr )
  } else if ( is.character(denominator) && denominator == 'automatic' ) {
    topn <- min( topn, length(relevantIDs) )
    1/topn * sum( cumsum(pr[1:topn]) / (1:topn) * pr[1:topn] )
  } else {
    stop( "Unknown value for the argument 'denominator'.", call. = TRUE  )
  }
}


#' @description Runs a recommender over a set of tests and computes test passign statistics
#' @param recommenderObject a recommender object
#' @param recommenderObjectFreq a recommender object based on, say, a contingency metadata matrix for calculation of frequency tests;
#' if NULL recommenderObject is used instead
#' @param tests a list of item tests
#' @param mc.cores if mc.cores is given as an argument mclapply is used, otherwise llply is used
#' @detail If smr is NULL no recommendations are computed, the measures are done over the premise items and response items
#' or premise tags and response tags.
RecommenderTestRun <- function( recommenderObject, recommenderObjectFreq = NULL, testSpecs, ... ) {
  
  execFunc <- if ( "mc.cores" %in% names(list(...)) ) { mclapply } else { llply }
  
  if ( is.null(recommenderObject) ) {
    
    testSpecsRecs <- execFunc( testSpecs, function( tspec ) {
      
      if ( ! is.null( tspec$PremiseItems ) && ! is.null( tspec$ResponseItems ) ) {
        recs <- tspec$PremiseItems
      } else if ( ! is.null( tspec$PremiseTags ) && ! is.null( tspec$ResponseTags ) ) {
        recs <- NA
      } else {
        stop( "Test measures between items data and tags data cannot be computed when recommenderObject is NULL.", call. = TRUE)
      }
      
    },...)
    
  } else {
    
    testSpecsRecs <-
      execFunc( testSpecs, function( tspec ) {
        
        removeHistory <- if( is.null( tspec$DropHistory ) ) { FALSE } else { tspec$DropHistory }
        
        if ( ! is.null( tspec$PremiseItems ) && ( is.null( tspec$PremiseTags ) || tspec$CombinationParameter == 0 ) ) {
          ## Recommendations by history

          ## Just in case some premise items are not known by the recommender object.
          inds <- which( tspec$PremiseItems$Item %in% RecommenderItems(recommenderObject) )
          histSpec <- tspec$PremiseItems[ inds, ]
          
          recs <- Recommendations( x = recommenderObject,
                                   historyItems = histSpec$Item,
                                   historyRatings = histSpec$Score,
                                   nrecs = tspec$NRecs,
                                   removeHistory = removeHistory,
                                   tuningParameters = tspec$TuningParameters )
          
        } else if ( ! is.null( tspec$PremiseTags && ( is.null( tspec$PremiseItems ) || tspec$CombinationParameter == 1 ) ) ) {
          ## Recommendations by profile

          ## Just in case some premise profile tags are not known by the recommender object.
          inds <- which( tspec$PremiseTags$Tag %in% RecommenderTags(recommenderObject) )
          prof <- tspec$PremiseTags[ inds, ]

          recs <- RecommendationsByProfile( recommenderObject, prof$Tag, prof$Score, nrecs = tspec$NRecs,
                                            tuningParameters = tspec$TuningParameters )
          
        } else {
          ## Combined recommendations
          
          ## To be implemented...
        }

        ## At this point recs is a data frame with columns c( "Score", "Item" ).
        
        recs
      }, ... )
  }
  
  interemediateResList <- vector( mode = "list", length = length(testSpecs) )
  
  testResults <-
    execFunc( 1:length(testSpecs), function( i ) {
      
      ## print( testSpecs[[i]]$TestID )
      
      ## Find the profile from recommendations if test response tags are specified.
      if ( length( testSpecs[[i]]$ResponseTags ) > 0 ) {
        if ( is.null(recommenderObject) ) {
          prof <- testSpecs[[i]]$PremiseTags
        } else if ( ! is.null(recommenderObjectFreq) ) {
          profHist <- data.frame( Rating=testSpecsRecs[[i]]$Score, Item=testSpecsRecs[[i]]$Item, stringsAsFactors = FALSE )
          prof <- SMRProfileDF( recommenderObjectFreq, profHist )
        } else {
          profHist <- data.frame( Rating=testSpecsRecs[[i]]$Score, Item=testSpecsRecs[[i]]$Item, stringsAsFactors = FALSE )
          prof <- SMRProfileDF( recommenderObject, profHist )
        }
      }
      
      ## Calculate specified tests statistics
      if ( testSpecs[[i]]$TestingMeasure == "Intersection" && !is.null( testSpecs[[i]]$ResponseItems ) ) {
        
        intermediateRes <- testSpecsRecs[[i]]
        
        res <- length( intersect( testSpecsRecs[[i]]$Item, testSpecs[[i]]$ResponseItems$Item ) )
        
      } else if ( testSpecs[[i]]$TestingMeasure == "IntersectionFraction" && !is.null( testSpecs[[i]]$ResponseItems ) ) {
        
        intermediateRes <- testSpecsRecs[[i]]
        
        if ( is.null( testSpecs[[i]]$IntersectionFractionDenominator ) ) { intFracDenominator <- "response" }
        else { intFracDenominator <- testSpecs[[i]]$IntersectionFractionDenominator }
        
        if ( intFracDenominator == "recommendations" ) {
          res <- length( intersect( testSpecsRecs[[i]]$Item, testSpecs[[i]]$ResponseItems$Item ) ) / nrow( testSpecsRecs[[i]] )
        } else {
          res <- length( intersect( testSpecsRecs[[i]]$Item, testSpecs[[i]]$ResponseItems$Item ) ) / nrow( testSpecs[[i]]$ResponseItems )
        }
        
      } else if ( testSpecs[[i]]$TestingMeasure == "AveragePrecision" && !is.null( testSpecs[[i]]$ResponseItems ) ) {
        
        intermediateRes <- testSpecsRecs[[i]]
        
        if ( is.null( testSpecs[[i]]$AveragePrecisionDenominator ) ) { avgPrecDenominator <- "recommendations" }
        else { avgPrecDenominator <- testSpecs[[i]]$AveragePrecisionDenominator }
        
        res <- AveragePrecision( recs = testSpecsRecs[[i]][,c("Score","Item")],
                                 relevantIDs = testSpecs[[i]]$ResponseItems$Item,
                                 topn = NULL, denominator = avgPrecDenominator )
        
      } else if ( testSpecs[[i]]$TestingMeasure == "Correlation" && length( testSpecs[[i]]$ResponseTags ) > 0 ) {
        ## Correlation
        
        if ( nrow( testSpecs[[i]]$ResponseTags ) > 0 || sd( prof$Score ) > 0 ) {
          ## Handling the main case
          
          ## The test spec field CorrelationMethod is used to specify the "cosine" similarity or
          ## the cor() methods: “pearson”, “kendall”, “spearman”.

          inds <- which( prof$Tag %in% testSpecs[[i]]$ResponseTags$Tag )
          if ( length(inds) == 0 ) { 
            
            warning( paste( "No matching of respose profiles tags with recommendations profile tags for", testSpecs[[i]]$TestID, "." ),
                     call. = FALSE )
            res <- 0
            
          } else { 
            
            prof <- prof[ inds, ] 
            
            prof <- prof[, c("Tag","Score")]
            prof <- prof[ !duplicated( prof$Tag ), ]
            
            if ( nrow(prof) < nrow( testSpecs[[i]]$ResponseTags ) ) {
              prof <- rbind( prof, data.frame( Tag=setdiff( testSpecs[[i]]$ResponseTags$Tag, prof$Tag ), Score=c(0) ) )
            }
            
            intermediateRes <- prof
            
            inds <- match( testSpecs[[i]]$ResponseTags$Tag, prof$Tag )
            prof <- prof[inds,]
            
            if ( sum( is.na(prof$Tag) ) > 0 ) {

              warning( paste( "Matching of profiles tags produced NA for", testSpecs[[i]]$TestID, "." ), call. = FALSE )
              res <- 0
              
            } else {
              
              if ( is.null( testSpecs[[i]]$CorrelationMethod ) ) { corMethod <- "pearson" }
              else { corMethod <- testSpecs[[i]]$CorrelationMethod }
              
              if ( corMethod == "cosine" ) {
                n1 <- sqrt( sum( prof$Score * prof$Score ) )
                n2 <- sqrt( sum( testSpecs[[i]]$ResponseTags$Score * testSpecs[[i]]$ResponseTags$Score ) )
                if ( n1 == 0 || n2 == 0 ) { res <- 0 }
                else {
                  res <-  prof$Score %*% testSpecs[[i]]$ResponseTags$Score / ( n1 * n2 )
                }
              } else {
                if ( nrow( testSpecs[[i]]$ResponseTags ) == 1 ) {
                  res <- as.numeric( ( prof[1,"Score"] > 0  && testSpecs[[i]]$ResponseTags[1,"Score"] > 0 ) ||
                                       ( prof[1,"Score"] == 0  && testSpecs[[i]]$ResponseTags[1,"Score"] == 0 ) )
                } else if ( sd( prof$Score, na.rm = TRUE ) > 0 && sd( testSpecs[[i]]$ResponseTags$Score, na.rm = TRUE ) > 0 ) {
                  res <- cor( prof$Score, testSpecs[[i]]$ResponseTags$Score, method = corMethod )
                } else {
                  warning( paste( "Standard deviation is zero for testID", testSpecs[[i]]$TestID ), call. = FALSE )
                  res <- 0
                }
              }
            
          }
          }
        } else if ( sd( prof$Score ) == 0 && sd( testSpecs[[i]]$ResponseTags$Score ) == 0 ) {
          ## Handling one of the corner cases
          
          intermediateRes <- NULL
          res <- 1
          
        } else {
          res <- 0
          intermediateRes <- NULL
          warning( paste( "Unhandled case", "Correlation", "for testID", testSpecs[[i]]$TestID ), call. = FALSE )
        }
        
      } else if (  testSpecs[[i]]$TestingMeasure == "Frequency" && length( testSpecs[[i]]$ResponseTags ) > 0 ) {
        ## Frequency of profile
        
        if ( nrow( testSpecs[[i]]$ResponseTags ) >= 0 ) {
          
          ## This code block is the same as above
          inds <- which( prof$Tag %in% testSpecs[[i]]$ResponseTags$Tag )
          if ( length(inds) == 0 ) { prof <- data.frame( Tag = testSpecs[[i]]$ResponseTags$Tag, Score = c(0) ) }
          else { prof <- prof[ inds, ] }
          
          prof <- prof[, c("Tag","Score")]
          prof <- prof[ !duplicated( prof$Tag ), ]
          
          if ( nrow(prof) < nrow( testSpecs[[i]]$ResponseTags ) ) {
            prof <- rbind( prof, data.frame( Tag=setdiff( testSpecs[[i]]$ResponseTags$Tag, prof$Tag ), Score=c(0) ) )
          }
          
          intermediateRes <- prof
          
          inds <- match( prof$Tag, testSpecs[[i]]$ResponseTags$Tag )
          prof <- prof[inds,]
          
          res <- mean( ( ( prof$Score - testSpecs[[i]]$ResponseTags$Score ) / testSpecs[[i]]$ResponseTags$Score ) >= -testSpecs[[i]]$Tolerance )
          
        } else {
          res <- 0
          intermediateRes <- NULL
          warning( paste( "No response tags in the frequency test", testSpecs[[i]]$TestID, "." ), call. = FALSE )
        }
        
      } else {
        res <- 0
        intermediateRes <- NULL
        warning( paste( "Un-handled case:", testSpecs[[i]]$TestingMeasure,
                        ", dim(ResponseTags)=", dim(testSpecs[[i]]$ResponseTags),
                        ", dim(ResponseItems)=", dim(testSpecs[[i]]$ResponseItems), "\n" ), call. = FALSE )
      }
      
      list( IntermediateResults = intermediateRes,
            TestResults = data.frame( TestID = testSpecs[[i]]$TestID,
                                      Score = res,
                                      TestPassThreshold = testSpecs[[i]]$TestPassThreshold,
                                      Pass = ( res >= testSpecs[[i]]$TestPassThreshold ) )
      )
      
    }, ... )
  
  names(testResults) <- laply( testSpecs, function(x) x$TestID )
  testResults
}


#' @description Compute statistics over a list of tests
#' @param testSpecs a list of tests
#' @param addHistograms a logical should histograms be added to the result
#' @return A list of four elements with different statistics computed: length, quantiles, histogram.
TestSpecStatistics <- function( testSpecs, addHistograms = TRUE ) {
  
  fieldNames <- c( "PremiseItems", "ResponseItems", "PremiseTags", "ResponseTags" )
  tdLengths <-
    llply( fieldNames, function ( fieldName ) {
      dl <- llply( testSpecs, function(x) { nrow( x[[fieldName]] ) } )
      dl <- as.numeric( ifelse( laply( dl, is.null),  0, dl ) )
      list( Lengths = dl,
            Quantiles = quantile( dl, probs=seq(0,1,0.1) ),
            Histogram = if( addHistograms ) {
                           histogram( dl, main = paste( "Distribution of", fieldName, "lengths" ) ,
                                      xlab = paste( "nrow of", fieldName ), breaks = 20 )
                        } else { NULL }
      )
    })
  names(tdLengths) <- fieldNames
  
  tdLengths
}



#' @description Calculates the success rates over an array of test specifications ...
#' @param smr sparse matrix recommender
#' @param testSpecs a list of test specifications
#' @param tagTypes1 first group of tag types
#' @param tagTypes2 second group of tag types
#' @param mergeWeights merge weights between the two groups of tag types
#' @param mc.cores number of cores to be used for RecommenderTestRun
#' @param .progress progress parameter for llply
#' @detail Changes the tag type significance factors and does not restore them (for optimization).
SMRPairedTagTypesRetrievalStatistics <- function( smr, testSpecs, tagTypes1, tagTypes2,
                                                  mergeWeights = seq(0,1,0.1),
                                                  initialSignificanceFactors = NULL,
                                                  mc.cores = 4, .progress = 'none' ) {
  
  zeroSFs <- setNames( rep( 0, length( smr$TagTypes ) ), smr$TagTypes )
  if( is.null( initialSignificanceFactors ) ) {
    initialSignificanceFactors <- zeroSFs
    initialSignificanceFactors[ TagTypes1 ] <- 1
    initialSignificanceFactors[ TagTypes2 ] <- 1
  }
  
  llply( mergeWeights, function(x) {
    experimentSFs <- zeroSFs
    experimentSFs[ tagTypes2 ] <- initialSignificanceFactors[ tagTypes2 ] * (1-x)
    experimentSFs[ tagTypes1 ] <- initialSignificanceFactors[ tagTypes1 ] * x
    ## print(as.data.frame(experimentSFs))
    smr$M <- SMRApplyTagTypeWeights( smr, experimentSFs )
    RecommenderTestRun( recommenderObject = smr, recommenderObjectFreq = NULL, testSpecs = testSpecs, mc.cores = mc.cores )
  }, .progress = .progress )
  
}
