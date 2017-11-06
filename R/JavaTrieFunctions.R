#   Java tries with frequencies R package
#   Copyright (C) 2017  Anton Antonov
#   
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#   
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#   
#   Written by Anton Antonov,
#   antononcube @ gmai l . c om,
#   Windermere, Florida, USA.
#===============================================================================
#
# This package provides R functions for easier use of the Java implementation of tries with frequencies:
#   
#   https://github.com/antononcube/MathematicaForPrediction/tree/master/Java/TriesWithFrequencies
# 
# Although the functions in this package are very close to the ones in the R package TriesWithFrequencies.R:
#   
#   https://github.com/antononcube/MathematicaForPrediction/blob/master/R/TriesWithFrequencies.R
# 
# the functions in this package can be used independently of TriesWithFrequencies.R -- that is why I made this
# separate package.
# 
# Before using the package there are two necessary steps:
#   
#   1. the building of the jar file TriesWithFrequencies.jar, and
# 
#   2. the appropriate Java installation in R (with the library rJava).
# 
# Prescriptions for Step 1 are given in :
#   
#   https://github.com/antononcube/MathematicaForPrediction/blob/master/Java/TriesWithFrequencies/README.md
#
#===============================================================================
#
# ToDo:
#   1. Threshold removal functions.
#   2. Pareto removal functions.
#   3. Remove the dependency of plyr.
# 
#===============================================================================

library(plyr)
library(rJava)

##===========================================================
## Java load and JAR file hook-up
##===========================================================
if( FALSE || !exists(".jniInitialized") ) {
  # install.packages("rJava")  
  Sys.getenv("JAVA_HOME")
  Sys.setenv(JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.8.0_112.jdk/Contents/Home")
  
  # Did not work initially. Worked after running the command:
  #
  #  sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
  #
  # See https://stackoverflow.com/q/30738974 .
  
  .jinit('.',force.init = T)
  .jaddClassPath( path = '~/MathematicaForPrediction/Java/TriesWithFrequencies/TriesWithFrequencies.jar' )

}
  
##===========================================================
## JavaTrie basic functions
##===========================================================

jTRIEFUNCTIONS <- .jnew( "TrieFunctions" )

#' @description Clone a trie.
JavaTrieClone <- function( jTr ) {jTr$clone()}

#' @description Create a trie from a list of strings using splitting.
JavaTrieCreateBySplit <- function( words, regex ) {
  
  jWordsToSplit <- jTRIEFUNCTIONS$wordArrayToList( .jarray(words) )
  jSplitCharacter <- .jnew("java/lang/String", regex)
  
  jTRIEFUNCTIONS$createBySplit( jWordsToSplit, jSplitCharacter )
}

#' @description Creates an array-list from array of strings. 
#' @param words a character array/vector
#' @detail Too slow to use; for profiling purposes only.
JavaToListOfStrings <-function ( words ) {
  
  jArrListClass <- J( "java.util.ArrayList" )
  jArrList <- new(jArrListClass)
  
  Map(f = function(x) jArrList$add(x), words )
  jArrList
}

#' @description Are two Java tries equal or not.
JavaTrieEqualQ <- function( jTr1, jTr2 ) { jTr1$equals( jTr2) }

#' @description Merges two tries.
JavaTrieMerge <- function(jTr1, jTr2) {
  jTRIEFUNCTIONS$merge(jTr1, jTr2)
}

#' @description Insert a word (list of strings) in a Java trie.
JavaTrieInsert <- function(jTr, word ) {
  jTRIEFUNCTIONS$insert(jTr, jTRIEFUNCTIONS$wordArrayToList( .jarray(word) ) );
}

#' @description Insert a list of word (list of list of strings) in a Java trie.
JavaTrieInsertList <- function(jTr, words) {  
  jTr2 = JavaTrieCreate(words)
  JavaTrieMerge(jTr, jTr2)
}

##===========================================================
## JavaTrie utilities functions
##===========================================================

#' @description Counts of Java trie nodes.
JavaTrieNodeCounts <- function( jTr ) {
  c("total","internal","leaves")
  jTRIEFUNCTIONS$nodeCounts(jTr)
}

#' @description Gives the corresponding Java trie with node frequencies 
#' converted to probabilities.
JavaTrieNodeProbabilities <- function( jTr ) {
  jTRIEFUNCTIONS$nodeProbabilities( jTr )
}

#' @description Convert a Java trie to JSON string.
#' @param jTr a Java tries
#' @param maxLevel integer for max level
JavaTrieToJSON <- function( jTr, maxLevel = NULL) { 
  if( is.null( maxLevel ) ) {
    jTr$toJSON() 
  } else if ( is.numeric(maxLevel) && maxLevel >= 0 ) {
    jTr$toJSON( as.integer(maxLevel) ) 
  } else {
    stop( "The argument maxLevel is expected to be NULL or a positive integer.", call. = TRUE )
    NULL
  }
}

#' @description Converts a java list of lists to R list of character vectors.
#' @details Each character vector represents a "word" stored in a Java trie.
JavaListOfListsToRListOfVectors <- function( jListOfLists ) {
  llply( as.list( jListOfLists ), function(x) laply( as.list(x), function(y) .jstrVal(y) ))
}

##===========================================================
## JavaTrie retrieval functions
##===========================================================

#' @description Retrieve a sub-trie corresponding to a word.
JavaTrieRetrieve <- function( jTr, word = NULL, words = NULL ) {

  if( is.character(word) ) {
    jTRIEFUNCTIONS$retrieve( jTr, jTRIEFUNCTIONS$wordArrayToList( .jarray(word) ) );
  } else if ( is.list(words) ) {
    ## JavaTrieMapOptimizationCall[TrieFunctions`mapRetrieve, jTr, swords];
    stop("No implementation for a list of words to be retrieved.", call. = T )
  }
}

#' @description Is a word ( a character list) a key in a Java trie.
JavaTrieKeyQ <- function(jTr, word ) {
  jTRIEFUNCTIONS$isKey( jTr, jTRIEFUNCTIONS$wordArrayToList( .jarray(word) ) )
}

#' @description Is a word ( a character list) a complete match in a Java trie.
JavaTrieHasCompleteMatchQ <- function(jTr, word ) {
  jTRIEFUNCTIONS$hasCompleteMatch( jTr, jTRIEFUNCTIONS$wordArrayToList( .jarray(word) ) )
}

#' @description Does a Java trie contain a word ( a character list).
JavaTrieContains <- function(jTr, word ) {
  jTRIEFUNCTIONS$contains( jTr, jTRIEFUNCTIONS$wordArrayToList( .jarray(word) ) )
}

JavaTrieMemberQ <- JavaTrieContains

#' @description Random choice of one or many root-to-leaf paths.
#' @param jTr a Java trie
#' @param word searh word; can be NULL
JavaTrieGetWords <- function( jTr, word = NULL ) {
  
  if( is.null( word ) ) {
    jRes <- jTRIEFUNCTIONS$getWords( jTr )
  } else if ( is.character(word) ) {
    jRes <- jTRIEFUNCTIONS$getWords( jTr, jTRIEFUNCTIONS$wordArrayToList( .jarray(word) ) )
  } else {
    stop( "The argument word is expected to be NULL or a character vector.", call. = TRUE )
    NULL
  }
  
  JavaListOfListsToRListOfVectors( jRes )
}

#' @description Gives lists of key-value pairs corresponding to the root-to-leaf paths 
#' in a given trie."
JavaTrieRootToLeafPaths <- function( jTr ) {
  jTRIEFUNCTIONS$pathsToJSON( jTRIEFUNCTIONS$rootToLeafPaths( jTr ) )
}

#' @description Random choice of one or many root-to-leaf paths.
#' @details Example: laply( JavaTrieRandomChoice(jTr1,12), function(x) paste(x,collapse = ""))
JavaTrieRandomChoice <- function( jTr, n = 1, weighted = TRUE ) {
  JavaListOfListsToRListOfVectors( jTRIEFUNCTIONS$randomChoice( jTr, as.integer(n), as.logical(weighted) ) )
}


##===========================================================
## JavaTrie shrinking functions
##===========================================================

JavaTrieShrink <- function( jTr, sep = "", threshold = NULL ) {
  if( is.null(threshold) ) {
    jTRIEFUNCTIONS$shrink( jTr, as.character(sep) )
  } else if ( is.numeric(th) && 0 <= threshold && threshold <= 1 ) {
    jTRIEFUNCTIONS$shrink( jTr, as.character(sep), as.numeric(threshold) )
  } else {
    stop( "The argument threshold is expected to be NULL or a number between 0 and 1.", call. = TRUE )
    NULL
  }
}

JavaTrieShrinkInternalNodes <- function( jTr, sep = "", threshold = NULL ) {
  if( is.null(threshold) ) {
    jTRIEFUNCTIONS$shrinkInternalNodes( jTr, as.character(sep), as.numeric(1.0) )
  } else if ( is.numeric(threshold) && 0 <= threshold && threshold <= 1 ) {
    jTRIEFUNCTIONS$shrinkInternalNodes( jTr, as.character(sep), as.numeric(threshold) )
  } else {
    stop( "The argument threshold is expected to be NULL or a number between 0 and 1.", call. = TRUE )
    NULL
  }
}

##===========================================================
## JavaTrie removal functions
##===========================================================

#' @description Convert a Java trie to JSON string.
#' @param jTr a Java tries
#' @param maxLevel integer for max level
JavaTriePrune <- function( jTr, maxLevel = NULL) { 
 
  if ( is.numeric(maxLevel) && maxLevel >= 0 ) {
    jTRIEFUNCTIONS$prune( jTr, as.integer(maxLevel) ) 
  } else {
    stop( "The argument maxLevel is expected to be NULL or a positive integer.", call. = TRUE )
    NULL
  }
}
