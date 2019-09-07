(*
    Implementation of document-term matrix construction and re-weighting functions in Mathematica
    Copyright (C) 2013-2016  Anton Antonov

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
	  Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2016 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 0.95 *)
(* This package contains definitions for turning a collection of documents into a collection of bags of words and
   representing them in a linear vector space in which the (stemmed) words of the documents correspond to the axes.
   In this way the collection turns into a sparse matrix, each row corresponds to a document,
   each column corresponds to a (stemmed) word. *)
(*
TODO:
   (1) [ ] better description of the functions,
   (2) [X] remove or change the profiling statements with PRINT and AbsoluteTiming.
*)

BeginPackage["DocumentTermMatrixConstruction`"];

ToBagOfWords::usage = "ToBagOfWords[ docs : {_String..}, { stemmingRules : (_List | _Dispatch | _Association | Automatic), stopWords_List } ] \
converts a list of documents docs into bags words using stemming rules and removing stop words. \
As options can be given string splitting characters and a post string splitting predicate.";

DocumentTermMatrix::usage = "DocumentTermMatrix[docs:{_String..},{stemmingRules_List, stopWords_List}] converts \
a list of documents docs into sparse matrix using stemming rules and removing stop words. As options can be given \
string splitting characters and a post string splitting predicate. DocumentTermMatrix uses ToBagOfWords.";

WeightTerms::usage = "WeightTerms[docTermMat_?MatrixQ, globalWeightFunc_, localWeightFunc_, normalizerFunc_] changes \
the entries of docTermMat according to the functions for global weight, local weight, and normalization.";

GlobalTermWeight::usage = "GlobalTermWeight implements the global weight over a vector.";

GlobalTermFunctionWeights::usage = "Gives the global weights for a given matrix and function name.";

ApplyGlobalTermFunction::usage = "ApplyGlobalTermFunction[mat_?MatrixQ, fname_String] applies the global term weight \
function fname to the elements of mat.";

ApplyLocalTermFunction::usage = "ApplyGlobalTermFunction[mat_?MatrixQ, fname_String] applies the local term weight \
function fname to the elements of mat.";

ApplyNormalizationFunction::usage = "ApplyGlobalTermFunction[mat_?MatrixQ, fname_String] applies term normalization \
function fname to the elements of mat.";

Begin["`Private`"];

Clear[ToBagOfWords];
Options[ToBagOfWords] = {"SplittingCharacters" -> {Whitespace, "\n",
  " ", ".", ",", "!", "?", ";", ":", "-", "\"", "'", "(", ")", "\[OpenCurlyDoubleQuote]", "`"},
  "PostSplittingPredicate" -> (StringLength[#] > 2 &)};

ToBagOfWords[doc_String, {stemmingRules : (_List | _Dispatch | _Association | Automatic), stopWords_List}, opts : OptionsPattern[]] :=
    ToBagOfWords[{doc}, {stemmingRules, stopWords}, opts][[1]];

ToBagOfWords[docs : ( {_String ..} | {{_String...}..} ), {stemmingRules : (_List | _Dispatch | Automatic), stopWords_List}, opts : OptionsPattern[]] :=
    Block[{docTerms, splittingCharacters, pSPred, stopWordsRules, aDerivedStemmingRules},

      splittingCharacters = OptionValue[ToBagOfWords, "SplittingCharacters"];
      pSPred = OptionValue[ToBagOfWords, "PostSplittingPredicate"];

      If[ MatchQ[ docs, {_String..} ],
        docTerms = Flatten[StringSplit[#, splittingCharacters]] & /@ docs,
        docTerms = docs
      ];

      If[TrueQ[pSPred =!= None],
        docTerms = Map[ Pick[ #, pSPred /@ # ] &, docTerms ]
      ];

      If[ MatchQ[ stopWords, {_String..} ],
        stopWordsRules = Dispatch[ Append[ Thread[ stopWords -> True ], _String -> False ] ];
        (* docTerms = Flatten[Fold[If[MemberQ[stopWords, #2], #1, {#1, #2}] &, {}, #]] & /@ docTerms; *)
        docTerms = Pick[ #, Not /@ (# /. stopWordsRules) ] & /@ docTerms;
      ];

      Which[

        DispatchQ[stemmingRules] || Length[stemmingRules] > 0 && MatchQ[stemmingRules, {_Rule...}],
        docTerms = docTerms /. stemmingRules,

        AssociationQ[stemmingRules],
        docTerms = stemmingRules /@ docTerms,

        TrueQ[ stemmingRules === Automatic ] && MatchQ[ docs, {_String..} ],
        aDerivedStemmingRules = Association[ # -> WordData[#, "PorterStem"] & /@ Union[Flatten[docTerms]] ];
        docTerms = Map[ aDerivedStemmingRules, docTerms, {2}],

        TrueQ[ stemmingRules === Automatic ],
        aDerivedStemmingRules = Association[ # -> WordData[#, "PorterStem"] & /@ Union[Flatten[docTerms]] ];
        docTerms = Map[ aDerivedStemmingRules, docTerms]

      ];

      docTerms
    ];

Clear[DocumentTermMatrix];
DocumentTermMatrix[docs : ( {_String ...} | {{_String...}...} ), {stemmingRules : (_List | _Dispatch | _Association | Automatic), stopWords_}, opts : OptionsPattern[]] :=
    Block[{terms, mat, docTerms,
      n = Length[docs], termToIndexRules},

      (* for the construction of the doc*term matrix*)
      docTerms = ToBagOfWords[docs, {stemmingRules, stopWords}, opts];

      (* find all unique terms *)
      terms = Union[Flatten[docTerms]];

      (* matrix of term occurrences *)
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


(***********************************************************)
(* Application of named term weight functions              *)
(***********************************************************)

(* We can use monadic implementation in this way:
*
*  LSIMonUnit[mat] ==>
*   LSIMonApplyLocalTermFunction["Frequency"] ==>
*   LSIMonApplyGlobalTermFunction["IDF"] ==>
*   LSIMonApplyNormalizationFunction["Cosine"]
*
*  This done at some point for the SMRMon monad.
*  It is beneficial though to have a non-monadic implementations in this older, basic (fundamental) package.
*  They can be used in the monad packages. (Like LSIMon and QRMon.)
*
* *)

Clear[WeightTerms];

WeightTerms::wargs = "Wrong arguments `1`";

WeightTerms[docTermMat_?MatrixQ] := WeightTerms[docTermMat, "IDF", "None", "Cosine" ];

WeightTerms[docTermMat_?MatrixQ, globalWeightFunc_String, localWeightFunc_String, normalizerFunc_String ] :=
    Block[{mat, globalWeights},

      mat = ApplyLocalTermFunction[docTermMat, localWeightFunc];
      globalWeights = GlobalTermFunctionWeights[docTermMat, globalWeightFunc];
      mat = mat . DiagonalMatrix[SparseArray[globalWeights]];
      mat = ApplyNormalizationFunction[mat, normalizerFunc];

      mat
    ];

WeightTerms[docTermMat_?MatrixQ, globalWeights_, localWeightFunc_String, normalizerFunc_String ] :=
    Block[{mat},
      mat = ApplyLocalTermFunction[docTermMat, localWeightFunc];
      mat = mat . DiagonalMatrix[SparseArray[globalWeights]];
      mat = ApplyNormalizationFunction[mat, normalizerFunc];

      mat
    ] /; VectorQ[globalWeights, NumberQ] && Length[globalWeights] == Dimensions[docTermMat][[2]] ;

WeightTerms[args___] :=
    Block[{},
      Message[WeightTerms::wargs, {args}];
      $Failed
    ];


Clear[ApplyLocalTermFunction];

ApplyLocalTermFunction::unfunc = "Unknown local weight function specification. Returning the matrix unmodified. \
The known local weight function names are \
\"Binary\", \"Log\", \"Logarithmic\" (same as \"Log\"), \"None\", \"TermFrequency\" (same as \"None\").";

ApplyLocalTermFunction[ docTermMat_?MatrixQ, funcName_String] :=
    Block[{arules},

      Which[
        funcName == "TermFrequency" || funcName == "None",
        docTermMat,

        funcName == "Binary",
        (*This assumes that the non-zero elements of docTermMat are greater than zero.*)
        (*mat = Clip[docTermMat,{0,1}]*)
        arules = Most[ArrayRules[SparseArray[docTermMat]]];
        arules[[All, 2]] = 1;
        SparseArray[arules, Dimensions[docTermMat]],

        funcName == "Log" || funcName == "Logarithmic",
        arules = Most[ArrayRules[SparseArray[docTermMat]]];
        arules[[All, 2]] = Log[ arules[[All, 2]] + 1 ];
        SparseArray[arules, Dimensions[docTermMat]],

        True,
        Message[ApplyLocalTermFunction::unfunc];
        docTermMat
      ]

    ];


Clear[GlobalTermFunctionWeights];

GlobalTermFunctionWeights::unfunc = "Unknown global weight function specification. \
Returning a constant vector of 1's. \
The known global weight function names are \"IDF\", \"GFIDF\", \"None\", \"Binary\", \"ColumnStochastic\".";

GlobalTermFunctionWeights[ docTermMat_?MatrixQ, funcName_String] :=
    Block[{mat, globalWeights, freqSums},

      Which[
        funcName == "IDF",
        mat = SparseArray[docTermMat];
        mat = Clip[mat, {0, 1}];
        globalWeights = Total[mat, {1}];
        globalWeights = Log[ Dimensions[mat][[1]] / (1.0 + globalWeights)];
        globalWeights = Clip[ globalWeights, {0, Max[globalWeights]} ],

        funcName == "GFIDF",
        mat = SparseArray[docTermMat];
        freqSums = Total[mat, {1}];
        mat = Clip[mat, {0, 1}];
        globalWeights = Total[mat, {1}];
        globalWeights = globalWeights /. { 0. -> 1. };
        globalWeights = N[freqSums / globalWeights],

        funcName == "None",
        globalWeights = ConstantArray[ 1, Dimensions[docTermMat][[2]] ],

        funcName == "Binary",
        globalWeights = ConstantArray[ 1, Dimensions[docTermMat][[2]] ],

        funcName == "ColumnStochastic" || funcName == "ColumnSum" || funcName == "Sum",
        mat = SparseArray[docTermMat];
        globalWeights = N[Total[mat, {1}]];
        globalWeights = globalWeights /. { 0. -> 1. };
        globalWeights = 1. / globalWeights,

        True,
        Message[ApplyGlobalTermFunction::unfunc];
        globalWeights = ConstantArray[1, Dimensions[docTermMat][[1]] ]
      ];

      globalWeights
    ];


Clear[ApplyGlobalTermFunction];

ApplyGlobalTermFunction::unfunc = GlobalTermFunctionWeights::unfunc;

ApplyGlobalTermFunction[ docTermMat_?MatrixQ, funcName_String] :=
    Block[{globalWeights},

      globalWeights = GlobalTermFunctionWeights[docTermMat, funcName];

      SparseArray[docTermMat] . DiagonalMatrix[SparseArray[globalWeights]]
    ];

Clear[ApplyNormalizationFunction];

ApplyNormalizationFunction::unfunc = "Unknown normalization function specification. \
Returning the argument unchanged. \
The known normalization function names are \"AbsMax\", \"Cosine\", \"Max\", \"None\", \"RowStochastic\".";

ApplyNormalizationFunction[docTermMat_?MatrixQ, funcName_String] :=
    Block[{dtSMat, mat, normWeights},

      dtSMat = SparseArray[docTermMat];

      Which[
        funcName == "None",
        Return[docTermMat],

        dtSMat["Density"] == 0,
        Return[docTermMat],

        funcName == "Cosine",
        normWeights = N[Sqrt[Total[dtSMat * dtSMat, {2}]]];
        normWeights = normWeights /. { 0. -> 1. };
        normWeights = 1. / normWeights,

        funcName == "RowStochastic" || funcName == "RowSum",
        normWeights = N[Total[dtSMat, {2}]];
        normWeights = normWeights /. { 0. -> 1. };
        normWeights = 1. / normWeights,

        funcName == "Max",
        normWeights = N[Map[Max, dtSMat]];
        normWeights = normWeights /. { 0. -> 1. };
        normWeights = 1. / normWeights,

        funcName == "AbsMax",
        normWeights = N[Map[Max, Abs[dtSMat]]];
        normWeights = normWeights /. { 0. -> 1. };
        normWeights = 1. / normWeights,

        True,
        Message[ApplyNormalizationFunction::unfunc];
        Return[docTermMat]
      ];

      mat = DiagonalMatrix[SparseArray[normWeights]];
      mat = mat . dtSMat;

      mat
    ];


(***********************************************************)
(* Older, slower code for weight term function application *)
(***********************************************************)

WeightTerms[docTermMat_?MatrixQ, globalWeightFunc_, localWeightFunc_, normalizerFunc_] :=
    Block[{mat, nDocuments, n, m, globalWeights, diagMat},
      {n, m} = Dimensions[docTermMat];
      mat = N[SparseArray[docTermMat]];

      If[ TrueQ[ globalWeightFunc === Identity || globalWeightFunc === None || globalWeightFunc == Function[#] ],

        globalWeights = ConstantArray[1.0, m],
        (*ELSE*)
        (* number of documents per term *)
        nDocuments = Map[Total, mat];

        mat = Transpose[mat]; (* term * document matrix *)

        globalWeights = Map[globalWeightFunc[#, nDocuments] &, mat];

        mat = Transpose[ SparseArray[mat] ]; (* document * term matrix *)
      ];

      (*PRINT[Length[Select[globalWeights, ! NumberQ[#] &]]];*)
      (*PRINT["WeightTerms::globalWeights : ", Through[{Min, Max, Mean, Median}[globalWeights]]];*)

      If[TrueQ[ localWeightFunc === Identity || localWeightFunc === None || localWeightFunc == Function[#] ],
        diagMat = SparseArray[MapThread[{#1, #2} -> #3 &, {Range[1, m], Range[1, m], globalWeights}], {m, m}];
        (* PRINT[diagMat, " ", MatrixQ[diagMat, NumberQ]]; *)
        mat = mat.diagMat,
        (* ELSE *)
        mat = Map[ SparseArray[Map[localWeightFunc, #] * globalWeights] &, mat ]
      ];

      If[TrueQ[ normalizerFunc === Identity || normalizerFunc === None || normalizerFunc == Function[#] ],
        mat = SparseArray[ mat ],
        mat = SparseArray[ Map[normalizerFunc[#] &, mat] ];
      ];
      mat
    ];

Clear[GlobalTermWeight];
(*SetAttributes[GlobalTermWeight,HoldAll]*)
ZEROFREQUENCYWEIGHT = 10^4;
GlobalTermWeight["None", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), d_] := 1;

GlobalTermWeight["None", termVec_SparseArray, nDocuments_List] := 1;

GlobalTermWeight["Entropy", termVec_SparseArray, nDocuments_List] :=
    Block[{ps, nfs, n = Dimensions[termVec][[1]]},
      nfs = Total[termVec];
      If[ nfs > 0,
        ps = termVec / nfs;
        1.0 + Total[Map[If[# == 0, 0, # * Log[#]] &, ps]] / Log[n],
        ZEROFREQUENCYWEIGHT
      ]
    ];

GlobalTermWeight["Entropy", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
    GlobalTermWeight["Entropy", termDoc[[termIndex]], nDocuments ]

GlobalTermWeight["IDF", termVec_SparseArray, nDocuments_List] :=
    Block[{nfs},
      nfs = Total[Clip[termVec]];
      If[nfs > 0, Log[Dimensions[termVec][[1]] / nfs],
        ZEROFREQUENCYWEIGHT
      ]
    ];

GlobalTermWeight["IDF", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
    Block[{nfs, n = Dimensions[termDoc][[2]]},
      nfs = Total[Clip[termDoc[[termIndex]]]];
      If[nfs > 0, Log[n / nfs],
        ZEROFREQUENCYWEIGHT
      ]
    ];

GlobalTermWeight["GFIDF", termVec_SparseArray, nDocuments_List] :=
    Block[{nfs, fs, n = Dimensions[termVec][[1]]},
      fs = Total[termVec];
      nfs = Total[Clip[termVec]];
      If[nfs > 0, fs / nfs,
        ZEROFREQUENCYWEIGHT
      ]
    ];

GlobalTermWeight["GFIDF", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
    Block[{nfs, fs, n = Dimensions[termDoc][[2]]},
      fs = termDoc[[termIndex]];
      nfs = Total[Clip[fs]];
      fs = Total[fs];
      If[nfs > 0, fs / nfs,
        ZEROFREQUENCYWEIGHT
      ]
    ];

GlobalTermWeight["Normal", termVec_SparseArray, nDocuments_List] :=
    Block[{nfs},
      nfs = Norm[termVec];
      If[nfs > 0, 1 / nfs, ZEROFREQUENCYWEIGHT]
    ];

GlobalTermWeight["Normal", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
    Block[{nfs},
      nfs = Norm[termDoc[[termIndex]]];
      If[nfs > 0, 1 / nfs, ZEROFREQUENCYWEIGHT]
    ];

GlobalTermWeight["ProbabilisticInverse", termVec : _SparseArray, nDocuments_List] :=
    Block[{nfs, n = Dimensions[termVec][[1]]},
      nfs = Total[Clip[termVec]];
      If[nfs > 0 && n - nfs > 0, Log[(n - nfs) / nfs],
        ZEROFREQUENCYWEIGHT
      ]
    ];

GlobalTermWeight["ProbabilisticInverse", termIndex_Integer, termDoc : ({_SparseArray ..} | _SparseArray), nDocuments_List] :=
    Block[{nfs, n = Dimensions[termDoc][[2]]},
      nfs = Total[Clip[termDoc[[termIndex]]]];
      If[nfs > 0 && n - nfs > 0, Log[(n - nfs) / nfs],
        ZEROFREQUENCYWEIGHT
      ]
    ];

End[];

EndPackage[]