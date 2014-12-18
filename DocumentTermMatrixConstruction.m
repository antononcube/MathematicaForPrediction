(*
    Implementation of document-term matrix construction and re-weighting functions in Mathematica
    Copyright (C) 2013  Anton Antonov

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

(* Version 0.95 *)
(* This package contains definitions for turning a collection of documents into a collection of bags of words and representing them in a linear vector space in which the (stemmed) words of the documents correspond to the axes. In this way the collection turns into a sparse matrix, each row corresponds to a document, each column corresponds to a (stemmed) word. *)
(* TODO: (1) better description of the functions, (2) remove or change the profiling statements with PRINT and AbsoluteTiming. *)

BeginPackage["DocumentTermMatrixConstruction`"]

ToBagOfWords::usage = "ToBagOfWrods[docs:{_String..},{stemmingRules_List, stopWords_List}] converts a list of documents docs into bags words using stemming rules and removing stop words. As options can be given string splitting characters and a post string splitting predicate."

DocumentTermMatrix::usage = "DocumentTermMatrix[docs:{_String..},{stemmingRules_List, stopWords_List}] converts a list of documents docs into sparse matrix using stemming rules and removing stop words. As options can be given string splitting characters and a post string splitting predicate. DocumentTermMatrix uses ToBagOfWords."

WeightTerms::usage = "WeightTerms[docTermMat_?MatrixQ, globalWeightFunc_, localWeightFunc_, normalizerFunc_] changes the entries of docTermMat according to the functions for global weight, local weight, and normalization."

GlobalTermWeight::usage = "GlobalTermWeight implements the global weight over a vector."
 
Begin["`Private`"]

Clear[ToBagOfWords]
Options[ToBagOfWords] = {"SplittingCharacters" -> {Whitespace, "\n", 
     " ", ".", ",", "!", "?", ";", ":", "-", "\"", "'", "(", ")", 
     "\[OpenCurlyDoubleQuote]", "`"}, 
   "PostSplittingPredicate" -> (StringLength[#] > 2 &)};

ToBagOfWords[doc_String, {stemmingRules:(_List|_Dispatch), stopWords_List}, opts : OptionsPattern[]] := 
  ToBagOfWords[{doc}, {stemmingRules, stopWords}, opts][[1]];

ToBagOfWords[docs : {_String ..}, {stemmingRules:(_List|_Dispatch), stopWords_List}, opts : OptionsPattern[]] :=  
  Block[{docTerms, splittingCharaters, pSPred},
    splittingCharaters = OptionValue[ToBagOfWords, "SplittingCharacters"];
    pSPred = OptionValue[ToBagOfWords, "PostSplittingPredicate"];
    If[TrueQ[pSPred === None],
      docTerms = Flatten[StringSplit[#, splittingCharaters]] & /@ docs,
      docTerms = Select[Flatten[StringSplit[#, splittingCharaters]], pSPred] & /@ docs
    ];
    docTerms = Flatten[Fold[If[MemberQ[stopWords, #2], #1, {#1, #2}] &, {}, #]] & /@ docTerms;
    docTerms = docTerms /. stemmingRules;
    docTerms
  ];

Clear[DocumentTermMatrix]
DocumentTermMatrix[docs : {_String ...}, {stemmingRules:(_List|_Dispatch), stopWords_}, opts : OptionsPattern[]] :=
  Block[{terms, mat, docTerms, nDocuments, splittingCharaters, 
    n = Length[docs], globalWeights, termToIndexRules},
   
    (* for the construction of the doc*term matrix*)   
    docTerms = ToBagOfWords[docs, {stemmingRules, stopWords}, opts];
   
    (* find all unique terms *)
    terms = Union[Flatten[docTerms]];
   
    (* matrix of term occurances *)   
    termToIndexRules = Dispatch[Thread[terms -> Range[Length[terms]]]];
    mat = Map[SparseArray[Rule @@@ (Tally[#] /. termToIndexRules), Length[terms]] &, docTerms];
    mat = SparseArray[mat];
    {mat, terms}
  ];

DocumentTermMatrix[docs : {_String ...}, {stemmingRules_, stopWords_}, {globalWeightFunc_, localWeightFunc_, normalizerFunc_}, opts : OptionsPattern[]] :=
  Block[{terms, mat},
    {mat, terms} = DocumentTermMatrix[docs, {stemmingRules, stopWords}, opts];
    {WeightTerms[mat, globalWeightFunc, localWeightFunc, normalizerFunc], terms}
  ];

Clear[WeightTerms]
WeightTerms[docTermMat_?MatrixQ] :=
  WeightTerms[docTermMat, GlobalTermWeight["IDF", #1, #2] &, # &, If[Max[#] == 0, #, #/Norm[#]] &];

WeightTerms[docTermMat_?MatrixQ, globalWeightFunc_, localWeightFunc_, normalizerFunc_] := 
  Block[{mat, nDocuments, n, m, globalWeights, diagMat},
    {n, m} = Dimensions[docTermMat];
    mat = N[SparseArray[docTermMat]];
    
    (* number of documents per term *)
    nDocuments = Map[Total, mat];
    
    mat = Transpose[mat]; (* term * document matrix *)
     
    globalWeights = Map[globalWeightFunc[#, nDocuments] &, mat];
    mat = Transpose[SparseArray[mat]]; (* document * term matrix *)

    (* PRINT[Length[Select[globalWeights, ! NumberQ[#] &]]]; *)
    (* PRINT["WeightTerms::globalWeights : ", Through[{Min, Max, Mean, Median}[globalWeights]]]; *)
   
    If[TrueQ[localWeightFunc === Identity || localWeightFunc == Function[#]], 
      diagMat = SparseArray[MapThread[{#1, #2} -> #3 &, {Range[1, m], Range[1, m], globalWeights}], {m, m}];
      (* PRINT[diagMat, " ", MatrixQ[diagMat, NumberQ]]; *)
      mat = mat.diagMat,
      (* ELSE *)
      mat = Map[SparseArray[Map[localWeightFunc, #]*globalWeights] &, mat]
    ];

    If[TrueQ[normalizerFunc === Identity || normalizerFunc == Function[#]],
      mat = SparseArray[mat],
      mat = SparseArray[Map[normalizerFunc[#] &, mat]];
    ];
    mat
  ];

Clear[GlobalTermWeight]
(*SetAttributes[GlobalTermWeight,HoldAll]*)
ZEROFREQUENCYWEIGHT = 10^4;
GlobalTermWeight["None", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), d_] := 1;

GlobalTermWeight["None", termVec_SparseArray, nDocuments_List] := 1; 

GlobalTermWeight["Entropy", termVec_SparseArray, nDocuments_List] :=
  Block[{ps, nfs, n = Dimensions[termVec][[1]]}, 
		nfs = Total[termVec];
		If[ nfs > 0,
			ps = termVec / nfs; 
			1.0 + Total[Map[If[# == 0, 0, #*Log[#]] &, ps]] / Log[n],
			ZEROFREQUENCYWEIGHT
		]
  ];

GlobalTermWeight["Entropy", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
	GlobalTermWeight["Entropy", termDoc[[termIndex]], nDocuments ]

GlobalTermWeight["IDF", termVec_SparseArray, nDocuments_List] :=  
  Block[{nfs},
    nfs = Total[Clip[termVec]];
    If[nfs > 0, Log[Dimensions[termVec][[1]]/nfs],
      ZEROFREQUENCYWEIGHT
    ]
  ];

GlobalTermWeight["IDF", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
   Block[{nfs, n = Dimensions[termDoc][[2]]},
     nfs = Total[Clip[termDoc[[termIndex]]]];
     If[nfs > 0, Log[n/nfs],
       ZEROFREQUENCYWEIGHT
     ]
   ];

GlobalTermWeight["GFIDF", termVec_SparseArray, nDocuments_List] :=  
  Block[{nfs, fs, n = Dimensions[termVec][[1]]},
	fs = Total[termVec];
    nfs = Total[Clip[termVec]];
    If[nfs > 0, fs/nfs,
      ZEROFREQUENCYWEIGHT
    ]
  ];

GlobalTermWeight["GFIDF", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] := 
   Block[{nfs, fs, n = Dimensions[termDoc][[2]]},
     fs = termDoc[[termIndex]];
     nfs = Total[Clip[fs]];
     fs = Total[fs];
     If[nfs > 0, fs/nfs,
       ZEROFREQUENCYWEIGHT
     ]
   ];

GlobalTermWeight["Normal", termVec_SparseArray, nDocuments_List] :=  
  Block[{nfs},
    nfs = Norm[termVec];
    If[nfs > 0, 1/nfs, ZEROFREQUENCYWEIGHT]
  ];

GlobalTermWeight["Normal", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
   Block[{nfs},
     nfs = Norm[termDoc[[termIndex]]];
     If[nfs > 0, 1/nfs, ZEROFREQUENCYWEIGHT]
   ];

GlobalTermWeight["ProbabilisticInverse", termVec : _SparseArray, nDocuments_List] :=
  Block[{nfs, n = Dimensions[termVec][[1]]},
    nfs = Total[Clip[termVec]];
    If[nfs > 0 && n - nfs > 0, Log[(n - nfs)/nfs],
      ZEROFREQUENCYWEIGHT
    ]
  ];

GlobalTermWeight["ProbabilisticInverse", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
   Block[{nfs, n = Dimensions[termDoc][[2]]},
     nfs = Total[Clip[termDoc[[termIndex]]]];
     If[nfs > 0 && n - nfs > 0, Log[(n - nfs)/nfs],
       ZEROFREQUENCYWEIGHT
     ]
   ];

End[]

EndPackage[]