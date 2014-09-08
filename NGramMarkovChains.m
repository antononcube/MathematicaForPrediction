(* ::Package:: *)

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
(* TODO
 1. It is a good idea to add working precision option to NGramMarkovChainModel.
 2. I need to profile and experiment with other ways of making the model arrays column stochastic. Both methods I have programmed so far are slow for larger number of words because of the unpacking of the sparse array of the n-gram model. That is for by default "ColumnStochastic"->False.
*)

BeginPackage["NGramMarkovChain`"]

NGramMarkovChainModel::usage = "NGramMarkovChainModel[ws_List, ngram_Integer] finds Markov chain probabilities from ws for each ngram number of words."

NGramMarkovChainGenerate::usage = "NGramMarkovChainGenerate[m_NGramModel, s_List, n_Integer] generates a list of n words using the n-gram model m starting with the sequence s."

NGramMarkovChainText::usage = "NGramMarkovChainText[textArg_String, ngram_Integer, startWords:{_String..}, nwords_Integer] splits the string textArg according to the value given to the option WordSeparators, finds the ngram Markov chain probabilities, and generates text of nwords."

NGramModel::usage = "Symbol for holding the n-gram model data."

MakeColumnStochastic::usage = "MakeColumnStochastic[m_?ArrayQ] makes the array m column stochastic."

Begin["`Private`"]

(* direct, easy to understand, in place *)
Clear[MakeColumnStochastic]
MakeColumnStochastic[mat_?ArrayQ] :=
  Block[{t, inds, IVar, iseq, csmat},
   inds = Array[IVar, Length[Dimensions[mat]] - 1];
   csmat = mat;
   Do[
    iseq = Sequence @@ Append[inds, All];
    t = Total[csmat[[iseq]]];
    If[t > 0,
     csmat[[iseq]] = csmat[[iseq]]/t
    ],
    Evaluate[Sequence @@ Transpose[{inds, Most@Dimensions[mat]}]]
   ];
   If[Head[mat] === SparseArray,
    SparseArray[csmat],
    csmat
   ]
  ];

NGramMarkovChainModel::farg = "A list is expected as a first argument.";
NGramMarkovChainModel::sarg = "A positive integer is expected as a second argument.";

Clear[NGramMarkovChainModel]
Options[NGramMarkovChainModel] = {"ColumnStochastic" -> False};
NGramMarkovChainModel[textWords_, numberOfPreviousWords_, opts : OptionsPattern[]] :=
  Module[{words, PickWord, markovMat, wordToIndexRules, indexToWordRules, ntuples, inds, randomTextWords, columnStochasticOpt},

    If[ ! ListQ[ textWords ],
      Message[NGramMarkovChainModel::farg];
      Return[{}]
    ];
    If[ !(IntegerQ[numberOfPreviousWords] && numberOfPreviousWords > 0 ),
      Message[NGramMarkovChainModel::sarg];
      Return[{}]
    ];

    columnStochasticOpt = OptionValue[NGramMarkovChainModel, "ColumnStochastic"];

    words = Union[textWords];
    wordToIndexRules = Dispatch[Thread[words -> Range[Length[words]]]];
    indexToWordRules = Dispatch[Thread[Range[Length[words]] -> words]];
    ntuples = Partition[textWords, numberOfPreviousWords + 1, 1];

    markovMat = SparseArray[{}, Table[Length[words], {numberOfPreviousWords + 1}]];
    Do[
     inds = Apply[Sequence, t /. wordToIndexRules];
     markovMat[[inds]] = markovMat[[inds]] + 1,
     {t, ntuples}];

    If[TrueQ[columnStochasticOpt],
     markovMat = MakeColumnStochastic[N[markovMat]];
    ];

    NGramModel[markovMat, wordToIndexRules, indexToWordRules]
  ];


Clear[NGramMarkovChainGenerate]
NGramMarkovChainGenerate[
   NGramModel[markovMat_SparseArray, 
    wordToIndexRules : (_Dispatch | {_Rule ..}), 
    indexToWordRules : (_Dispatch | {_Rule ..})], startWords_List, 
   numberOfWords_Integer] :=
  Module[{words, numberOfPreviousWords, randomTextWords, PickWord, startNGram, b = True, w},

    words = If[TrueQ[Head[wordToIndexRules] === Dispatch], Normal[wordToIndexRules][[All, 1]], wordToIndexRules[[All, 1]]];
    PickWord[inds_] :=
     Block[{ps = Total[markovMat[[Sequence @@ inds]]]},
      If[ps > 0,
       {True, 
        RandomSample[Normal[markovMat[[Sequence @@ inds]]] -> words, 
          1][[1]]},
       {False, None}
       ]
      ];
    (*PickWord[inds_]:=RandomSample[Normal[markovMat[[Sequence@@inds]]]->words,1][[1]];*)
    
    PickWord[ss : {_String ..}] := PickWord[ss /. wordToIndexRules];
    numberOfPreviousWords = Length[Dimensions[markovMat]] - 1;
    startNGram = startWords;
    If[Length[startNGram] < numberOfPreviousWords,
     startNGram = 
      Join @@ Table[
        startNGram, {Ceiling[
          numberOfPreviousWords/Length[startNGram]]}]
     ];
    startNGram = Take[startNGram, -numberOfPreviousWords];

    (*randomTextWords=Nest[Append[#,PickWord[Take[#,-numberOfPreviousWords]]]&,startNGram,numberOfWords];*)
   
    randomTextWords = 
     NestWhile[({b, w} = PickWord[Take[#, -numberOfPreviousWords]]; If[b, Append[#, w ], #]) &, startNGram, b &, 1, numberOfWords];

    randomTextWords
    ] /; Equal @@ Join[
      Dimensions[markovMat],
      {If[TrueQ[Head[wordToIndexRules] === Dispatch], 
        Length[Normal[wordToIndexRules]], Length[wordToIndexRules]],
       If[TrueQ[Head[indexToWordRules] === Dispatch], 
        Length[Normal[indexToWordRules]], Length[indexToWordRules]]}] && 
    numberOfWords > 0;

Clear[NGramMarkovChainText]
Options[NGramMarkovChainText] = {WordSeparators -> {Whitespace, "\n", " ", ".", ",", "!", "?", ";", ":", "-", "\"", "\'"}};
NGramMarkovChainText[text_String, numberOfPreviousWords_Integer, startWords : {_String ..}, numberOfWords_Integer, opts : OptionsPattern[]] :=
  Module[{textWords, randomTextWords, wordSeparators, ngMod},
   wordSeparators = OptionValue[NGramMarkovChainText, WordSeparators];
   textWords = StringSplit[text, wordSeparators];
   ngMod = NGramMarkovChainModel[textWords, numberOfPreviousWords, "ColumnStochastic" -> False]; 
   randomTextWords = NGramMarkovChainGenerate[ngMod, startWords, numberOfWords];
   StringJoin @@ Riffle[randomTextWords, " "]
  ] /; 1 <= numberOfPreviousWords <= 5;

End[]

EndPackage[]



