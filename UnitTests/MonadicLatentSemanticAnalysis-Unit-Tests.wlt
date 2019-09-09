(*
    Monadic Latent Semantic Analysis Mathematica unit tests
    Copyright (C) 2019  Anton Antonov

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
    antononcube @ gmai l . c om,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicLatentSemanticAnalysis-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2019-08-02 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: monad, monadic, latent semantic analysis, workflow, State monad, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the package

     https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m

*)
(* Created with the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/. *)

BeginTestSection["MonadicLatentSemanticAnalysis-Unit-Tests.wlt"];


VerificationTest[(* 1 *)
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m"];
  Length[SubValues[MonadicLatentSemanticAnalysis`LSAMonExtractTopics]] > 0
  ,
  True
  ,
  TestID->"LoadPackage"
];


VerificationTest[(* 2 *)
  dsStateOfUnionSpeeches = ResourceData["State of the Union Addresses"];

  dsStateOfUnionSpeeches =
      dsStateOfUnionSpeeches[All,
        Join[<|"ID" -> StringReplace[StringJoin[{TextString[#["President"]], ".", DateString[#["Date"], "ISOOrdinalDate"]}], " " -> "."]|>, #] &];

  aStateOfUnionSpeeches =
      AssociationThread[Normal[dsStateOfUnionSpeeches[All, "ID"]], Normal[dsStateOfUnionSpeeches[All, "Text"]]];

  SeedRandom[123];
  aStateOfUnionSpeeches = RandomSample[aStateOfUnionSpeeches, 22];

  Length[aStateOfUnionSpeeches] > 20 && Min[Values[StringLength /@ aStateOfUnionSpeeches]] > 5000
  ,
  True
  ,
  TestID -> "USASpeechesData"
];


VerificationTest[(* 3 *)
  textHamplet = ToString /@ Flatten[Import["https://raw.githubusercontent.com/antononcube/MathematicaVsR/master/Data/MathematicaVsR-Data-Hamlet.csv"]];

  aTextHamlet = ToAutomaticKeysAssociation[textHamplet];

  Length[aTextHamlet] > 200 && Median[Values[StringLength /@ aTextHamlet]] > 80
  ,
  True
  ,
  TestID -> "HamletData"
];


VerificationTest[ (* 4 *)
  stopWords = Complement[DictionaryLookup["*"], DeleteStopwords[DictionaryLookup["*"]]];
  Length[stopWords] > 300
  ,
  True
  ,
  TestID -> "StopWords"
];

(*************************************************************)
(* Basic pipeline                                            *)
(*************************************************************)

VerificationTest[ (* 5 *)
  docTermMat =
      Fold[LSAMonBind,
        LSAMonUnit[aTextHamlet],
        { LSAMonMakeDocumentTermMatrix[{}, stopWords], LSAMonTakeDocumentTermMatrix }];

  SSparseMatrixQ[docTermMat] &&
      RowNames[docTermMat] == Keys[aTextHamlet] &&
      Dimensions[docTermMat][[1]] == Length[aTextHamlet] &&
      Dimensions[docTermMat][[2]] > 2000
  ,
  True
  ,
  TestID -> "Make-document-term-matrix-1"
];


VerificationTest[ (* 6 *)
  lsaObj =
      Fold[
        LSAMonBind,
        LSAMonUnit[Values[aTextHamlet]],
        { LSAMonMakeDocumentTermMatrix[{}, stopWords] }
      ];

  Keys[LSAMonBind[ lsaObj, LSAMonTakeContext] ]
  ,
  {"documents", "documentTermMatrix", "terms", "stopWords", "stemmingRules"}
  ,
  TestID -> "Make-document-term-matrix-2"
];


VerificationTest[ (* 7 *)
  lsaContext =
      Fold[
        LSAMonBind,
        lsaObj,
        {
          LSAMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
          LSAMonTakeContext
        }
      ];

  Keys[ lsaContext ] ==
      {"documents", "documentTermMatrix", "terms", "stopWords", "stemmingRules", "weightedDocumentTermMatrix",
        "globalWeights", "globalWeightFunction", "localWeightFunction", "normalizerFunction"} &&
      SSparseMatrixQ[lsaContext["weightedDocumentTermMatrix"]] &&
      Dimensions[lsaContext["weightedDocumentTermMatrix"]] == Dimensions[lsaContext["documentTermMatrix"]]
  ,
  True
  ,
  TestID -> "Apply-term-weights-1"
];


VerificationTest[ (* 8 *)
  lsaContext =
      Fold[
        LSAMonBind,
        lsaObj,
        {
          LSAMonApplyTermWeightFunctions[ "GlobalWeightFunction" -> "IDF", "LocalWeightFunction" -> "None", "NormalizerFunction" -> "Cosine" ],
          LSAMonTakeContext
        }
      ];

  Keys[ lsaContext ] ==
      {"documents", "documentTermMatrix", "terms", "stopWords", "stemmingRules", "weightedDocumentTermMatrix",
        "globalWeights", "globalWeightFunction", "localWeightFunction", "normalizerFunction"} &&
      SSparseMatrixQ[lsaContext["weightedDocumentTermMatrix"]] &&
      Dimensions[lsaContext["weightedDocumentTermMatrix"]] == Dimensions[lsaContext["documentTermMatrix"]]
  ,
  True
  ,
  TestID -> "Apply-term-weights-2"
];


VerificationTest[ (* 9 *)
  lsaObj2 =
      Fold[
        LSAMonBind,
        lsaObj,
        {
          LSAMonExtractTopics[12, "MinNumberOfDocumentsPerTerm" -> 10, "NumberOfInitializingDocuments" -> 12, "MaxSteps" -> 12, "PrintProfilingInfo" -> False],
          LSAMonMakeTopicsTable
        }
      ];

  Keys[LSAMonBind[ lsaObj2, LSAMonTakeContext] ]
  ,
  {"documents", "documentTermMatrix", "terms", "stopWords",
    "stemmingRules", "weightedDocumentTermMatrix", "globalWeights",
    "globalWeightFunction", "localWeightFunction", "normalizerFunction",
    "W", "H", "topicColumnPositions", "automaticTopicNames", "method", "topicsTable"}
  ,
  TestID -> "Topic-extraction-1"
];


VerificationTest[ (* 10 *)
  (*  Instead of:  lsaObj ⟹ LSAMonEchoTopicsTable[Dividers -> All];  *)
  MatchQ[LSAMonBind[ lsaObj2, LSAMonTakeValue], {_TableForm ..}]
  ,
  True
  ,
  TestID -> "Topic-extraction-2"
];


VerificationTest[ (* 11 *)
  lsaObj3 =
      Fold[
        LSAMonBind,
        LSAMonUnit[aTextHamlet],
        {
          LSAMonMakeDocumentTermMatrix[{}, Automatic],
          LSAMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
          LSAMonExtractTopics[12, "MinNumberOfDocumentsPerTerm" -> 10, "NumberOfInitializingDocuments" -> 12, "MaxSteps" -> 12, "PrintProfilingInfo" -> False],
          LSAMonMakeTopicsTable
        }
      ];

  Keys[LSAMonBind[ lsaObj3, LSAMonTakeContext] ]
  ,
  {"documents", "documentTermMatrix", "terms", "stopWords", "stemmingRules", "weightedDocumentTermMatrix", "globalWeights",
   "globalWeightFunction", "localWeightFunction", "normalizerFunction",
   "W", "H", "topicColumnPositions", "automaticTopicNames", "method", "topicsTable"}
  ,
  TestID -> "Topic-extraction-3"
];


VerificationTest[ (* 12 *)
  (*  Instead of:  lsaObj ⟹ LSAMonEchoTopicsTable[Dividers -> All];  *)
  MatchQ[LSAMonBind[ lsaObj3, LSAMonTakeValue], {_TableForm ..}]
  ,
  True
  ,
  TestID -> "Topic-extraction-4"
];


VerificationTest[ (* 13 *)
  (*  Instead of:  lsaObj ⟹ LSAMonEchoTopicsTable[Dividers -> All];  *)
  MatchQ[
    Fold[
      LSAMonBind,
      lsaObj3,
      {
        LSAMonExtractStatisticalThesaurus[{"ghost", "king", "lord"}, 12],
        LSAMonTakeValue
      }
    ],
    { { _String, { _String .. } } .. }
  ]
  ,
  True
  ,
  TestID -> "Statistical-thesaurus-1"
];


(*************************************************************)
(* Topics representation                                     *)
(*************************************************************)

VerificationTest[ (* 14 *)
  tMat =
      Fold[ LSAMonBind,
        lsaObj3,
        {
          LSAMonRepresentDocumentTagsByTopics[ Automatic ],
          LSAMonTakeValue
        }];

  SSparseMatrixQ[tMat] &&
(*      RowsCount[tMat] == Length[aTextHamlet] &&*)
      Sort[ColumnNames[tMat]] == Sort[Intersection[ColumnNames[tMat], LSAMonBind[ lsaObj3, LSAMonTakeAutomaticTopicNames ] ]]
  ,
  True
  ,
  TestID -> "Topics-representation-1"
];


(*************************************************************)
(* Data members and accessors                                *)
(*************************************************************)

VerificationTest[ (* 15 *)
  SSparseMatrixQ[LSAMonBind[lsaObj2, LSAMonTakeMatrix]]
  ,
  True
  ,
  TestID -> "Take-document-term-matrix-1"
];

VerificationTest[ (* 16 *)
  SSparseMatrixQ[ LSAMonBind[lsaObj2, LSAMonTakeWeightedMatrix] ]
  ,
  True
  ,
  TestID -> "Take-weighted-document-term-matrix-1"
];

VerificationTest[ (* 17 *)
  SSparseMatrixQ[ LSAMonBind[lsaObj3, LSAMonTakeMatrix] ]
  ,
  True
  ,
  TestID -> "Take-document-term-matrix-2"
];

VerificationTest[ (* 18 *)
  SSparseMatrixQ[ LSAMonBind[lsaObj3, LSAMonTakeWeightedMatrix] ]
  ,
  True
  ,
  TestID -> "Take-weighted-document-term-matrix-2"
];

VerificationTest[ (* 19 *)
  ColumnNames[ LSAMonBind[lsaObj2, LSAMonTakeWeightedMatrix] ] == LSAMonBind[lsaObj2, LSAMonTakeTerms]
  ,
  True
  ,
  TestID -> "Take-terms-1"
];

VerificationTest[ (* 20 *)
  W = LSAMonBind[lsaObj3, LSAMonTakeW];
  H = LSAMonBind[lsaObj3, LSAMonTakeH];
  SSparseMatrixQ[ W ] && SSparseMatrixQ[ H ] && ColumnNames[ W ] == RowNames[ H ]
  ,
  True
  ,
  TestID -> "Take-Factors-1"
];

VerificationTest[ (* 21 *)
  RowNames[ LSAMonBind[lsaObj3, LSAMonTakeW] ] == RowNames[ LSAMonBind[lsaObj3, LSAMonTakeDocumentTermMatrix] ]
  ,
  True
  ,
  TestID -> "Take-Factors-2"
];

VerificationTest[ (* 22 *)
  pos = LSAMonBind[ lsaObj3, LSAMonTakeTopicColumnPositions ];
  ColumnNames[ LSAMonBind[lsaObj3, LSAMonTakeH] ] == ColumnNames[ LSAMonBind[lsaObj3, LSAMonTakeDocumentTermMatrix]] [[ pos ]]
  ,
  True
  ,
  TestID -> "Take-Factors-3"
];

VerificationTest[ (* 23 *)
  W = LSAMonBind[lsaObj3, LSAMonTakeW];
  docTermMat = LSAMonBind[lsaObj3, LSAMonTakeDocumentTermMatrix];
  RowNames[W] == Keys[aTextHamlet] &&
      RowNames[docTermMat] == Keys[aTextHamlet]
  ,
  True
  ,
  TestID -> "Take-Factors-4"
];

VerificationTest[ (* 24 *)
  stopWords = Complement[ DictionaryLookup["*"], DeleteStopwords[DictionaryLookup["*"]]];
  stopWords2 = Fold[ LSAMonBind, LSAMonUnit[aTextHamlet], {LSAMonMakeDocumentTermMatrix[Automatic, Automatic], LSAMonTakeStopWords}];
  VectorQ[stopWords2, StringQ] && stopWords2 == stopWords
  ,
  True
  ,
  TestID -> "Take-StopWords-1"
];

VerificationTest[ (* 25 *)
  srules = Fold[ LSAMonBind, LSAMonUnit[aTextHamlet], {LSAMonMakeDocumentTermMatrix[Automatic, Automatic], LSAMonTakeStemmingRules}];
  TrueQ[srules === Automatic] || AssociationQ[srules]
  ,
  True
  ,
  TestID -> "Take-StemmingRules-1"
];


EndTestSection[]
