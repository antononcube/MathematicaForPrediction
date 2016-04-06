##================================================================================
##  Functional parsers in R
##  Copyright (C) 2015  Anton Antonov
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
##================================================================================
##  Version 0.7
##  
##  This R script provides an implementation of a system of functional parsers. 
##  The implementation follows closely the article:
##    
##    "Functional parsers" by Jeroen Fokker .
##  
##  The script also follows closely the implementation in the Mathematica package FunctionalParsers.m:
##
##  [1] Anton Antonov, Functional parsers Mathematica package,
##      https://github.com/antononcube/MathematicaForPrediction/blob/master/FunctionalParsers.m , (2014).
##
##  The parsers are categorized in the groups: basic, combinators, and transformers.
##  The basic parsers parse specified strings and strings adhering to predicates.
##  The combinator parsers allow sequential and alternative combinations of parsers.
##  The transformer parsers change the input or the output of the parsers that are transformed.
##  
##  A basic or a combinator parser takes a list of strings and returns a list of pairs, {{o1,r1},{o2,r2},...}. 
##  Each pair has as elements a parsed output and the rest of the input list.
##  
##  Functions for splitting the input text into tokens are provided.
##
##  The parsers have long descriptive names. Short names are programmed for convenience.
##  For example, "p.apply" is the short name of "ParseApply".
##  "p.seql" is the short name of "ParseSequentialCompositionPickLeft".
##  For some parsers there are infix operators. For example, "@" can be used for "ParseApply".
##  See the sections "Shorter parser names" and "Infix notation" below.
##  
##  There is a plan the package also have functions to generate parsers from a string of the
##  Extended Backus-Naur Form (EBNF) definition of a grammar.
##  
##================================================================================
##  This version of the R functions / package does not have functions for parser 
##  generation from of EBNF specification (which are implemented in the Mathematica 
##  package FunctionalParsers.m .)
##  This preliminary release is put on GitHub because I developed R functions 
##  6 months ago (end of December 2014), I want to start a process of reviewing
##  and completing the functionality and creating an actual R package.
##================================================================================

## The dependency from 'plyr' can be probably eleminated.
library('plyr')


##============================================================
## Basic parsers
##============================================================

ParseSymbol <- function(a) {
  force(a)
  function(xs) {
    ## cat("ParseSymbol::xs="); print(xs)
    ## cat("ParseSymbol::a="); print(a)
    if( length(xs) > 0 && xs[1] == a ) { list( list( stream=xs[-1], parsed=c(a) ) ) } else { NULL }
  }
}

ParsePredicate <- function(predFunc) {
  force(predFunc)
  function(xs) {
    ##cat( "xs[1]=", xs[1], " predFunc(xs[1])=", predFunc(xs[1]), "\n" )
    if( length(xs) > 0 && predFunc(xs[1]) ) { list( list( stream=xs[-1], parsed=c(xs[1]) ) ) } else { NULL }
  }
}

ParseEpsilon <- function(xs) { list( list( stream=xs, parsed=NULL ) ) }

ParseSucceed <- function(v) { force(v); function(xs) { list( list( stream=xs, parsed=v ) ) } }

ParseFailed <- function(xs) {NULL}



##============================================================
## Parser combinators
##============================================================
ParseComposeWithResults <- function(p, res) {
  ##cat("ParseComposeWithResults::res="); print(res); cat("\n")
  if ( length(res) == 0 ) {
    NULL 
  } else {
    pRes <-
      llply( res,
             function(r) {
               if ( is.null(r) ) { NULL } else {
                 ##cat("ParseComposeWithResults::r= "); print(r)
                 pr <- p( r$stream )
                 ##cat("ParseComposeWithResults::pr= "); print(pr)
                 if ( is.null( pr ) || is.na(pr) || length( pr ) == 0 ) { NULL } else {
                   llply( pr, function(x) list( stream = x$stream, parsed = list( r$parsed, x$parsed ) ) )
                 }
               }   
             })
    ##pRes <- Filter(Negate(is.null), pRes)
    pRes <- pRes[ !sapply(pRes, function(x) is.null(x) || is.na(x) ) ]
    ##cat("ParseComposeWithResults::pRes= "); print(ListToString(pRes))
    if ( length(pRes) == 0 ) { NULL }
    else {
      ##cat("ParseComposeWithResults::unlist(pRes)= "); print(ListToString(unlist( pRes, recursive = FALSE )))
      unlist( pRes, recursive = FALSE )
    }
  }
}

ParseSequentialComposition <- function( parsers ) {
  force(parsers)
  ##if( exists('fclosure') ) { fclosure$parsers <- parsers }
  if ( class(parsers) == "function" ) { function(xs) parsers(xs) }
  else if ( class(parsers) == "list" && length(parsers) == 1 ) { function(xs) parsers[[1]](xs) }
  else {
    function(xs) {
      ##cat("ParseSequentialComposition::parsers[[1]](xs)="); print( parsers[[1]](xs) )
      res <- Reduce( function(x,y) { ParseComposeWithResults(y, x) }, parsers[-1], parsers[[1]](xs) )
      ##cat("ParseSequentialComposition::res="); print(res)
      res
    }
  }
}

ParseAlternativeComposition <- function( parsers ) {
  force(parsers)
  function(xs) {
    pRes <- llply( parsers, function(f) f(xs) )
    pRes <- pRes[ !sapply(pRes, function(x) is.null(x) || is.na(x) || length(pRes)==0 ) ]
    ##cat("ParseAlternativeComposition::res= "); print(ListToString( pRes ) )
    unlist( pRes, recursive=FALSE )
  }
}

##============================================================
## Next combinators
##============================================================

ParseJust <- function(p) {
  force(p)
  function(xs) { Filter( function(x) is.null( x$stream ) || length(x$stream) == 0, p(xs) ) }
}

ParseApply <- function(f, p) {
  force(f); force(p)
  function(xs) { llply( p(xs), function(x) list( stream=x$stream, parsed=f(x$parsed) ) ) }
}

ParseSome <- function(p) {
  force(p) 
  function(xs) {
    pres <- ParseJust(p)(xs)
    if ( length(pres) > 0 ) { pres[1] } else { pres }
  }
}

ParseShortest <- function(p) {
  force(p)
  function(xs) {
    pres <- p(xs)     
    if ( is.null(pres) ) { NULL }
    else {
      pres <- pres[ laply( pres, function(x) !is.null(x) ) ]
      ##cat("ParseShortest::pres=");print(pres)
      if ( length(pres) > 0 ) {
        lens <- laply( pres, function(x) length(x$stream) )
        ##cat("ParseShortest::lens=");print(lens)
        pres[ order( lens ) ][1]
      } else {
        NULL
      }
    }
  }
}

ParseSequentialCompositionPickLeft <- function( p1, p2 ) {
  force(p1); force(p2)
  function(xs) {
    ParseApply( function(x) x[[1]], ParseSequentialComposition( c( p1, p2 ) ) ) (xs)
  }
}

ParseSequentialCompositionPickRight <- function( p1, p2 ) {
  force(p1); force(p2)
  function(xs) {
    ParseApply( function(x) x[[2]], ParseSequentialComposition( c( p1, p2 ) ) ) (xs)
  }
}

ParseChoice <- function( ) {
  
}

##============================================================
## Second next combinators
##============================================================

ParsePack <- function( s1, p, s2 ) {
  force(s1); force(p); force(s2)
  ##print( ListToString( ParseSequentialCompositionPickRight( s1, p ) ) )
  ParseSequentialCompositionPickLeft( ParseSequentialCompositionPickRight( s1, p ), s2 )
}

ParseParenthesized <- function( p ) { ParsePack ( ParseSymbol("("), p, ParseSymbol(")") ) }
ParseBracketed <- function( p ) { ParsePack ( ParseSymbol("["), p, ParseSymbol("]") ) }
ParseCurlyBracketed <- function( p ) { ParsePack ( ParseSymbol("{"), p, ParseSymbol("}") ) }

ParseOption <- function(p) {
  force(p)
  ParseAlternativeComposition( c( ParseApply( list, p ) , 
                                  ParseSucceed( list() )  ) )
}

ParseOption1 <- function(p) {
  force(p)
  function(xs) {
    res <- p(xs)
    if ( is.null(res) ) { list( list(stream=xs, parsed=NULL) ) } else { res }
  }
}

ParseMany1 <- function(p) {
  force(p)
  function(xs) {
    oldStream <- xs
    accRes <- NULL
    res <- ParseShortest( ParseOption1( p ) ) ( xs )
    while ( !( is.null(res) || length(res) == 0 || is.null( res[[1]]$parsed ) ) ) {
      if ( is.null( accRes ) ) {
        accRes = c( res[[1]]$parsed )
      } else {
        accRes <- c( accRes, res[[1]]$parsed )
      }
      oldStream <- res[[1]]$stream
      res <- ParseShortest( ParseOption1( p ) ) ( res[[1]]$stream )
    }
    if ( is.null(accRes) ) { list( list( stream=xs, parsed=NULL ) ) }
    else { list( list( stream=oldStream, parsed=accRes ) ) }
  }
}

ParseMany <- function(p) { ParseAlternativeComposition( c( ParseMany1(p), ParseSucceed(NULL) ) ) }

## I am not sure that ParseApply is needed
ParseListOf <- function( p, sepParser ) {
  force(p); force(sepParser)
  
  ParseAlternativeComposition( c(
    ParseApply(
      function(x) c( list( x[[1]] ), x[[2]] ),
      ParseSequentialComposition( c( p,
                                     ParseMany1( ParseSequentialCompositionPickRight( sepParser, p ) ) )
      ) ),
    ParseSucceed( NULL ) ) )
}

ParseChain <- function( p, sepParser, right = FALSE ) {
  force(p); force(sepParser); force(right)
  ParseAlternativeComposition( c(
    ParseApply( 
      function(x) {
        if ( length(x)==1 ) { 
          x 
        } else if ( is.null(x[[2]]) ) {
          x[1]
        } else { ## at this point length(x) >= 3 and length(x) == 2*k-1
          ## op <- x[[2]][[1]]
          ops <-x[[2]][ seq( 1, length(x[[2]]), 2 ) ]
          elems <- c(x[[1]], x[[2]][ seq( 2, length(x[[2]]), 2 ) ] )
          if ( right ) {
            ## Reduce( f = function(e,a) list(op, a, e[[1]]), x = elems[-length(elems)], init = elems[[length(elems)]], right = right ) 
            Reduce( f = function(e,a) list(ops[[e]], a, elems[[e]]), x = 1:(length(elems)-1), init = elems[[length(elems)]], right = right ) 
          } else {
            ## Reduce( f = function(a,e) list(op, a, e[[1]]), x = elems[-1], init = elems[[1]], right = right ) 
            Reduce( f = function(a,e) list(ops[[e-1]], a, elems[[e]]), x = 2:length(elems), init = elems[[1]], right = right ) 
          }
        }
      },
      ParseSequentialComposition( c( p,
                                     ParseMany( ParseSequentialComposition( c( sepParser, p ) ) )
      )
      )
    ),
    ParseSucceed( NULL ) ) )
}

ParseChainLeft <- function( p, sp ) ParseChain( p, sp, right=FALSE )

ParseChainRight <- function( p, sp ) ParseChain( p, sp, right=TRUE )

##============================================================
## Infix notation
##============================================================

'%&%' <- function( p1, p2 ) { ParseSequentialComposition( c( p1, p2 ) ) }
'%<&%' <- function( p1, p2 ) { ParseSequentialCompositionPickLeft( p1, p2 ) }
'%&>%' <- function( p1, p2 ) { ParseSequentialCompositionPickRight( p1, p2 ) }

'%|%' <- function( p1, p2 ) { ParseAlternativeComposition( c( p1, p2 ) ) }

'%@%' <- function( p1, p2 ) { ParseApply( p1, p2 ) }
'%@>%' <- '%@%'
'%<@%' <- function( p1, p2 ) { ParseApply( p2, p1 ) }

##============================================================
## Shorter parser names
##============================================================

p.symbol <- ParseSymbol
p.satisfy <- ParsePredicate
p.pred <- ParsePredicate
p.fail <- ParseFailed
p.seq <- ParseSequentialComposition
p.alt <- ParseAlternativeComposition

p.just <- ParseJust
p.apply <- ParseApply
p.some <- ParseSome
p.shortest <- ParseShortest

p.seql <- ParseSequentialCompositionPickLeft
p.seqr <- ParseSequentialCompositionPickRight

p.pack <- ParsePack
p.parenthesized <- ParseParenthesized
p.bracketed <- ParseBracketed
p.curlybracketed <- ParseCurlyBracketed

p.option <- ParseOption
p.many <- ParseMany
p.many1 <- ParseMany1
p.listof <- ParseListOf

p.chainl <- ParseChainLeft
p.chainr <- ParseChainRight


##============================================================
## List print-out
##============================================================

ListToString <- function( arg, leftBracket="{", rightBracket="}", headLeftBracket=NULL, headRightBracket=NULL, head=NULL, level=0 ) {
  if ( is.null(arg) ) {
    ""
  } else if( length(arg) == 0 ) {
    "{}" 
  } else {
    res <-
      Reduce( f = function(a,e) {
        pe <- 
          if( is.list(e) ) { 
            ListToString(e, leftBracket = leftBracket, rightBracket = rightBracket ) 
          } else { 
            paste( e, sep = ",", collapse = "" ) 
          }
        if ( is.null(a) ) { pe } else { paste( a, pe, sep="," ) }
      },
      x = arg,
      init = NULL )
    if ( is.vector(arg) && level == 0 && !is.null(headLeftBracket) && !is.null(headRightBracket) ) {
      paste( headLeftBracket, res, headRightBracket, sep = "" )
    } else if ( is.vector(arg) && level == 0 && !is.null(head)) {
      paste( head, "[", res, "]", sep = "" )
    } else if ( is.list(arg)  ) {
      paste( leftBracket, res, rightBracket, sep = "" )
    } else {
      res
    }
  }
}


##============================================================
##  EBNF Parsers with parenthesis, <& and &>
##============================================================
##  All parsers start with the prefix "pG" followed by a capital letter. 
##  ("p" is for "parser", "G" is for "grammar".)


EBNFSymbolTest <- function(x) { is.character(x) && ( x == "|" || x == "," || x == "=" || x == ";" || x == "&>" || x == "<&" || x == "<@" ) }

NonTerminalTest <- function(x) grepl( pattern = "^<(\\w|\\s|\\-|\\_)*>$", x = x )

InQuotesTest <- function(x) grepl( pattern = "^(\'|\")(.*)(\'|\")$", x = x )

# pGTerminal <- p.pred( function(x) ( is.character(x) && InQuotesTest(x) && ! EBNFSymbolTest(x) ) )
pGTerminal <- p.apply( function(x) paste( unlist(x), collapse="" ), p.seq( c( p.symbol("'"), p.many1( p.pred( function(x) grepl( pattern = "^(\\w|\\s|\\-)$", x = x ) ) ), p.symbol("'") ) ) )

# pGNonTerminal <- p.pred( function(x) ( is.character(x) && NonTerminalTest(x) && ! EBNFSymbolTest(x) ) ) )
pGNonTerminal <- p.apply( function(x) paste( unlist(x), collapse="" ), p.seq( c( p.symbol("<"), p.many1( p.pred( function(x) grepl( pattern = "^(\\w|\\s|\\-)$", x = x ) ) ), p.symbol(">") ) ) )

pGRCode <- p.apply( function(x) paste( x, collapse="" ), p.many1( p.pred( function(x) grepl( pattern = "[^;]", x = x ) ) ) )

  
##pGNode[xs_] := (EBNFTerminal<@>pGTerminal<+>EBNFNonTerminal<@>pGNonTerminal<+>ParseParenthesized[pGExpr]<+>pGRepetition<+>pGOption)[xs];

EBNFTerminal <- function(x) ListToString( x, head="EBNFTerminal")
EBNFNonTerminal <- function(x) ListToString( x, head="EBNFNonTerminal")
pGNode <- function(xs) p.alt( c( p.apply( EBNFTerminal, pGTerminal), 
                                 p.apply( EBNFNonTerminal, pGNonTerminal ), 
                                 p.parenthesized( pGExpr ),
                                 pGRepetition, 
                                 pGOption ) ) (xs)

##pGTerm = EBNFSequence<@>ParseChainRight[pGNode, ParseSymbol[","]<+>ParseSymbol["<&"]<+>ParseSymbol["&>"]];

EBNFSequence <- function(x) ListToString( x, head="EBNFSequence")
# pGTerm <- p.apply( EBNFSequence, p.chainr( pGNode, p.alt( c( p.symbol(","), p.symbol("<&"), p.symbol("&>") ) ) ) )
pGTerm <- EBNFSequence %@% p.chainr( pGNode, p.alt( c( p.symbol(","), p.symbol("&") %&% p.symbol(">"), p.symbol("<") %&% p.symbol("&") ) ) )


##pGExpr = EBNFAlternatives<@>ParseListOf[pGTerm, ParseSymbol["|"]];

EBNFAlternatives <- function(x) ListToString( x, head="EBNFAlternatives")
pGExpr <- EBNFAlternatives %@% p.listof( pGTerm, p.symbol("|") )

EBNFOption <- function(x) ListToString( x, head="EBNFOption")
pGOption <- EBNFOption %@% ParseBracketed( pGExpr )

EBNFRepetition <- function(x) ListToString( x, head="EBNFRepetition")
pGRepetition <- EBNFRepetition %@% ParseCurlyBracketed( pGExpr )

##pGRule = EBNFRule<@>(pGNonTerminal<*>(ParseSymbol["="] &> pGExpr)<*>(ParseSymbol[";"]<+>(ParseSymbol["<@"]<*>ParsePredicate[StringQ[#] &] <& ParseSymbol[";"])));

EBNFRule <- function(x) ListToString( x, head="EBNFRule")
pGRule <- p.apply( EBNFRule, p.seq( c( pGNonTerminal, 
                                       p.seqr( p.symbol("="), pGExpr ), 
                                       p.alt( c( p.symbol(";"), 
                                                 p.seq( c( p.symbol("<"), p.symbol("@"), pGRCode, p.symbol(";") ) ) ) 
                                       ) ) ) )

##pEBNF = EBNF<@>ParseMany1[pGRule];

EBNF <- function(x) ListToString( x, head="EBNF" )
pEBNF <- EBNF %@% p.many1( pGRule )


##============================================================
## Grammar parser generators with <& and &>
##============================================================

EBNFMakeSymbolName <- function(p) paste( "p" , toupper( gsub( pattern="\\<|\\>|\\_|\\-", replacement = "", p) ), sep="", collapse="" )


