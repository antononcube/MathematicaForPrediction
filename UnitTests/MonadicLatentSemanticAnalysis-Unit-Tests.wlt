(*
    Monadic latent semantic analysis Mathematica unit tests
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
  Length[SubValues[MonadicLatentSemanticAnalysis`LSAMonTopicExtraction]] > 0
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


VerificationTest[ (* 3 *)
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

VerificationTest[ (* 4 *)
  docTermMat =
      Fold[LSAMonBind,
        LSAMonUnit[aStateOfUnionSpeeches],
        { LSAMonMakeDocumentTermMatrix[{}, stopWords], LSAMonTakeDocumentTermMatrix }];

  SSparseMatrixQ[docTermMat] &&
      RowNames[docTermMat] == Keys[aStateOfUnionSpeeches] &&
      Dimensions[docTermMat][[1]] == Length[aStateOfUnionSpeeches] &&
      Dimensions[docTermMat][[2]] > 2000
  ,
  True
  ,
  TestID -> "Make-document-term-matrix-1"
];


VerificationTest[ (* 5 *)
  lsaObj =
      Fold[
        LSAMonBind,
        LSAMonUnit[Values[aStateOfUnionSpeeches]],
        { LSAMonMakeDocumentTermMatrix[{}, stopWords] }
      ];

  Keys[LSAMonBind[ lsaObj, LSAMonTakeContext] ]
  ,
  {"documents", "documentTermMatrix", "terms"}
  ,
  TestID -> "Make-document-term-matrix-2"
];


VerificationTest[ (* 6 *)
  lsaContext =
      Fold[
        LSAMonBind,
        lsaObj,
        {
          LSAMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
          LSAMonTakeContext
        }
      ];
  Keys[ lsaContext ] == {"documents", "documentTermMatrix", "terms", "weightedDocumentTermMatrix"} &&
      SSparseMatrixQ[lsaContext["weightedDocumentTermMatrix"]] &&
      Dimensions[lsaContext["weightedDocumentTermMatrix"]] == Dimensions[lsaContext["documentTermMatrix"]]
  ,
  True
  ,
  TestID -> "Apply-term-weights-1"
];


VerificationTest[ (* 7 *)
  lsaObj2 =
      Fold[
        LSAMonBind,
        lsaObj,
        {
          LSAMonTopicExtraction[12, "MinDocumentsPerTerm" -> 10, "NumberOfInitializingDocuments" -> 12, "MaxSteps" -> 12, "PrintProfilingInfo" -> False],
          LSAMonTopicsTable
        }
      ];

  Keys[LSAMonBind[ lsaObj2, LSAMonTakeContext] ]
  ,
  {"documents", "documentTermMatrix", "terms", "weightedDocumentTermMatrix", "W", "H", "topicColumnPositions", "automaticTopicNames", "topicsTable"}
  ,
  TestID -> "Topic-extraction-1"
];


VerificationTest[ (* 8 *)
  (*  Instead of:  lsaObj ⟹ LSAMonEchoTopicsTable[Dividers -> All];  *)
  MatchQ[LSAMonBind[ lsaObj2, LSAMonTakeValue], {_TableForm ..}]
  ,
  True
  ,
  TestID -> "Topic-extraction-2"
];


VerificationTest[ (* 9 *)
  lsaObj3 =
      Fold[
        LSAMonBind,
        LSAMonUnit[aStateOfUnionSpeeches],
        {
          LSAMonMakeDocumentTermMatrix[{}, Automatic],
          LSAMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
          LSAMonTopicExtraction[12, "MinDocumentsPerTerm" -> 10, "NumberOfInitializingDocuments" -> 12, "MaxSteps" -> 12, "PrintProfilingInfo" -> False],
          LSAMonTopicsTable
        }
      ];

  Keys[LSAMonBind[ lsaObj3, LSAMonTakeContext] ]
  ,
  {"documents", "documentTermMatrix", "terms", "weightedDocumentTermMatrix", "W", "H", "topicColumnPositions", "automaticTopicNames", "topicsTable"}
  ,
  TestID -> "Topic-extraction-3"
];


VerificationTest[ (* 10 *)
  (*  Instead of:  lsaObj ⟹ LSAMonEchoTopicsTable[Dividers -> All];  *)
  MatchQ[LSAMonBind[ lsaObj3, LSAMonTakeValue], {_TableForm ..}]
  ,
  True
  ,
  TestID -> "Topic-extraction-4"
];


VerificationTest[ (* 11 *)
  (*  Instead of:  lsaObj ⟹ LSAMonEchoTopicsTable[Dividers -> All];  *)
  MatchQ[
    Fold[
      LSAMonBind,
      lsaObj3,
      {
        LSAMonStatisticalThesaurus[{"indians", "ships", "bank"}, 12],
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

VerificationTest[ (* 12 *)
  tMat =
      Fold[ LSAMonBind,
        lsaObj3,
        {
          LSAMonTopicsRepresentation[ Automatic ],
          LSAMonTakeValue
        }];

  SSparseMatrixQ[tMat] &&
      RowsCount[tMat] == Length[aStateOfUnionSpeeches] &&
      Sort[ColumnNames[tMat]] == Sort[Intersection[ColumnNames[tMat], LSAMonBind[ lsaObj3, LSAMonTakeAutomaticTopicNames ] ]]
  ,
  True
  ,
  TestID -> "Topics-representation-1"
];


(*************************************************************)
(* Data members and accessors                                *)
(*************************************************************)

VerificationTest[ (* 13 *)
  SSparseMatrixQ[LSAMonBind[lsaObj2, LSAMonTakeMatrix]]
  ,
  True
  ,
  TestID -> "Take-document-term-matrix-1"
];

VerificationTest[ (* 14 *)
  SSparseMatrixQ[LSAMonBind[lsaObj2, LSAMonTakeWeightedMatrix]]
  ,
  True
  ,
  TestID -> "Take-weighted-document-term-matrix-1"
];

VerificationTest[ (* 15 *)
  SSparseMatrixQ[ LSAMonBind[lsaObj3, LSAMonTakeMatrix] ]
  ,
  True
  ,
  TestID -> "Take-document-term-matrix-2"
];

VerificationTest[ (* 16 *)
  SSparseMatrixQ[ LSAMonBind[lsaObj3, LSAMonTakeWeightedMatrix] ]
  ,
  True
  ,
  TestID -> "Take-weighted-document-term-matrix-2"
];

VerificationTest[ (* 17 *)
  ColumnNames[ LSAMonBind[lsaObj2, LSAMonTakeWeightedMatrix] ] == LSAMonBind[lsaObj2, LSAMonTakeTerms]
  ,
  True
  ,
  TestID -> "Take-terms-1"
];

VerificationTest[ (* 18 *)
  W = LSAMonBind[lsaObj3, LSAMonTakeW];
  H = LSAMonBind[lsaObj3, LSAMonTakeH];
  SSparseMatrixQ[ W ] && SSparseMatrixQ[ H ] && ColumnNames[ W ] == RowNames[ H ]
  ,
  True
  ,
  TestID -> "Take-Factors-1"
];

VerificationTest[ (* 19 *)
  RowNames[ LSAMonBind[lsaObj3, LSAMonTakeW] ] == RowNames[ LSAMonBind[lsaObj3, LSAMonTakeDocumentTermMatrix] ]
  ,
  True
  ,
  TestID -> "Take-Factors-2"
];

VerificationTest[ (* 20 *)
  pos = LSAMonBind[ lsaObj3, LSAMonTakeTopicColumnPositions ];
  ColumnNames[ LSAMonBind[lsaObj3, LSAMonTakeH] ] == ColumnNames[ LSAMonBind[lsaObj3, LSAMonTakeDocumentTermMatrix]] [[ pos ]]
  ,
  True
  ,
  TestID -> "Take-Factors-3"
];

VerificationTest[ (* 21 *)
  W = LSAMonBind[lsaObj3, LSAMonTakeW];
  docTermMat = LSAMonBind[lsaObj3, LSAMonTakeDocumentTermMatrix];
  RowNames[W] == Keys[aStateOfUnionSpeeches] &&
      RowNames[docTermMat] == Keys[aStateOfUnionSpeeches]
  ,
  True
  ,
  TestID -> "Take-Factors-4"
];

EndTestSection[]
