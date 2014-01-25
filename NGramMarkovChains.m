(*
    N-gram Markov chains Mathematica package
    Copyright (C) 2014  Anton Antonov

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

	Written by Anton Antonov, 
	antononcube@gmail.com, 
	7320 Colbury Ave, 
	Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2014 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 1.0 *)
(* 
   This package provides an implementation of Markov chains training and generation using the N-gram model.
   The implementation utilizes Mathematica's high-dimensional sparse array data structure (SparseArray) and 
   weighted random sampling (RandomSample).

   For the building of an N-gram model of a list or text an N-1 dimensional sparse array is used.
   Note that dimensions up to 5 are allowed, but that can be easily changed.
*)

BeginPackage["NGramMarkovChain`"]

NGramMarkovChain::usage = "NGramMarkovChain[ws_Lists, ngram_Integer, nwords_Integer] finds Markov chain probabilities from ws for each ngram number of words and using those probabilities generates a list of nwords."

NGramMarkovChainText::usage = "NGramMarkovChainText[textArg_String, ngram_Integer, nwords_Integer] splits the string textArg according to the value given to the option WordSeparators, finds the ngram Markov chain probabilities, and generates text of nwords."

Begin["`Private`"]

Clear[NGramMarkovChain]
Options[NGramMarkovChain] = {"StartNGram" -> Automatic};
NGramMarkovChain[textWords_List, numberOfPreviousWords_Integer, numberOfWords_Integer, opts : OptionsPattern[]] :=
  Module[{words, PickWord, markovMat, wordToIndexRules, indexToWordRules, ntuples, inds, randomTextWords, startNGram, startNGramOpt = OptionValue["StartNGram"]},
    words = Union[textWords];
    wordToIndexRules = Dispatch[Thread[words -> Range[Length[words]]]];
    indexToWordRules = Dispatch[Thread[Range[Length[words]] -> words]];
    PickWord[inds_] := RandomSample[Normal[markovMat[[Sequence @@ inds]]] -> words, 1][[1]];
    PickWord[ss : {_String ..}] := PickWord[ss /. wordToIndexRules];
    ntuples = Partition[textWords, numberOfPreviousWords + 1, 1];
    
    markovMat = 
     SparseArray[{}, 
      Table[Length[words], {numberOfPreviousWords + 1}]];

    Do[
     inds = Apply[Sequence, t /. wordToIndexRules];
     markovMat[[inds]] = markovMat[[inds]] + 1,
     {t, ntuples}];

    startNGram = 
     If[NumberQ[startNGramOpt] && startNGramOpt <= Length[ntuples], 
      ntuples[[startNGramOpt]], 
      RandomSample[ntuples, 1][[1]]
     ];

    randomTextWords = 
     Nest[Append[#, PickWord[Take[#, -numberOfPreviousWords]]] &, Most@startNGram, numberOfWords];
    randomTextWords
  ] /; 2 <= numberOfPreviousWords <= 5;


Clear[NGramMarkovChainText]
Options[NGramMarkovChainText] = {
   "StartNGram" -> Automatic, 
   WordSeparators -> {Whitespace, "\n", " ", ".", ",", "!", "?", ";", ":", "-", "\"", "\'"}
};
NGramMarkovChainText[text_String, numberOfPreviousWords_Integer, numberOfWords_Integer, opts : OptionsPattern[]] :=
  Module[{textWords, randomTextWords, wordSeparators},
    wordSeparators = OptionValue[NGramMarkovChainText, WordSeparators];
    textWords = StringSplit[text, wordSeparators];
    randomTextWords = 
     NGramMarkovChain[textWords, numberOfPreviousWords, numberOfWords, DeleteCases[{opts}, WordSeparators -> _]];
    StringJoin @@ Riffle[randomTextWords, " "]
  ] /; 2 <= numberOfPreviousWords <= 5;


End[]

EndPackage[]