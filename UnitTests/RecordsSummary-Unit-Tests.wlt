(*
    RecordsSummary Mathematica unit tests
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

(* :Title: RecordsSummary-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2019-09-27 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 12 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: summary, summarization, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the function RecordsSummary in the package

     https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m

*)

BeginTestSection["RecordsSummary-Unit-Tests"];

VerificationTest[(* 1 *)
(*  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"];*)
  Get["/Volumes/Macintosh HD/Users/antonov/MathematicaForPrediction/MathematicaForPredictionUtilities.m"];
  Length[DownValues[MathematicaForPredictionUtilities`RecordsSummary]] > 0
  ,
  True
  ,
  TestID -> "LoadPackage"
];


VerificationTest[(* 2 *)
  n = 200;
  matMixedData = Flatten /@
      Transpose[{
        RandomReal[{-10, 10}, {n, 2}],
        MapAt[ToLowerCase, RandomChoice[CharacterRange["A", "Z"], {n, 2}], {All, 2}],
        RandomChoice[{E, I, \[CapitalGamma]}, n],
        DatePlus[Now, #] & /@ Table[Quantity[RandomInteger[1000], "Days"], n]
      }];
  dsMixedData = Dataset[matMixedData][All, AssociationThread[{"num1", "num2", "char1", "char2", "symb", "dobj"}, #] &];
  MatrixQ[matMixedData] && ( Head[dsMixedData] === Dataset )
  ,
  True
  ,
  TestID -> "GenerateData-1"
];

VerificationTest[(* 3 *)
  dsMixedDataWithMissing = dsMixedData[All, {"num2" -> (If[# > 2, Missing[], #] &), "char1" -> (If[ToCharacterCode[#][[1]] > 76, Missing[], #] &)}];
  dsMixedDataWithNamedRows = Dataset[ AssociationThread[ Range[Dimensions[dsMixedData][[1]]] -> Normal[dsMixedData] ] ];
  Head[dsMixedDataWithMissing] === Dataset && Head[dsMixedDataWithNamedRows] === Dataset
  ,
  True
  ,
  TestID -> "GenerateData-2"
];

VerificationTest[(* 4 *)
  n = 40;
  aImages = AssociationThread[ Range[n] -> RandomChoice[Table[RandomImage[4, {40, 12}], 5], n]];
  aMixed = Association @ Map[ # -> RandomChoice[{Identity, ToString}][#]&, Range[n]];
  AssociationQ[aImages] && AssociationQ[aMixed]
  ,
  True
  ,
  TestID -> "GenerateData-3"
];

VerificationTest[(* 5 *)
  dsNonFullArray = Dataset[{
    <|"a" -> 1, "b" -> "x", "c" -> {1}|>,
    <|"a" -> 2, "b" -> "y", "c" -> {2, 3}|>,
    <|"a" -> 3, "b" -> "z", "c" -> {3}|>,
    <|"a" -> 4, "b" -> "x", "c" -> {4, 5}|>,
    <|"a" -> 5, "b" -> "y", "c" -> {5, 6, 7}|>,
    <|"a" -> 6, "b" -> "z", "c" -> {}|>}];
  Head[dsNonFullArray] === Dataset
  ,
  True
  ,
  TestID -> "GenerateData-4"
];

VerificationTest[(* 6 *)
  MatchQ[RecordsSummary[matMixedData[[All,1]]], {_Column}],
  True,
  TestID -> "Vector-Numbers-1"
];

VerificationTest[(* 7 *)
  MatchQ[RecordsSummary[matMixedData[[All,3]]], {_Column}],
  True,
  TestID -> "Vector-Strings-1"
];

VerificationTest[(* 8 *)
  MatchQ[RecordsSummary[matMixedData[[All,-1]]], {_Column}],
  True,
  TestID -> "Vector-DateObjects-1"
];

VerificationTest[(* 9 *)
  MatchQ[RecordsSummary[dsMixedData], {_Column ..}],
  True,
  TestID -> "Dataset-MixedData-1"
];

VerificationTest[(* 10 *)
  MatchQ[RecordsSummary[dsMixedData, "MaxTallies" -> 12], {_Column ..}],
  True,
  TestID -> "Dataset-MixedData-2"
];

VerificationTest[(* 11 *)
  MatchQ[RecordsSummary[dsMixedData, Range[4]], {_Column..} ]
  ,
  True
  ,
  RecordsSummary::igncols
  ,
  TestID -> "Dataset-MixedData-3"
];

VerificationTest[(* 12 *)
  res = RecordsSummary[dsMixedDataWithMissing];
  MatchQ[res, {_Column ..}] &&
  Count[dsMixedDataWithMissing, _Missing, Infinity ] ==
      Total[Cases[res, {Missing[___] | "Missing[___]", x_Integer} :> x, Infinity]],
  True,
  TestID -> "Dataset-MixedDataWithMissing-1"
];

VerificationTest[(* 13 *)
  MatchQ[RecordsSummary[aImages], {_Column}],
  True,
  TestID -> "Association-Images-1"
];

VerificationTest[(* 14 *)
  MatchQ[RecordsSummary[aImages, Thread->True], Rule[{_Column}, {_Column}] ],
  True,
  TestID -> "Association-Images-2"
];


VerificationTest[(* 15 *)
  MatchQ[RecordsSummary[aMixed, Thread->False], {_Column} ],
  True,
  TestID -> "Association-Mixed-1"
];

VerificationTest[(* 16 *)
  MatchQ[RecordsSummary[aMixed, Thread->True], Rule[{_Column}, {_Column}] ],
  True,
  TestID -> "Association-Mixed-2"
];

VerificationTest[(* 17 *)
  MatchQ[RecordsSummary[dsNonFullArray], $Failed ]
  ,
  True
  ,
  RecordsSummary::args
  ,
  TestID -> "Dataset-NonFullArray-1"
];

VerificationTest[(* 18 *)
  MatchQ[RecordsSummary[dsNonFullArray[All, {"c"->HoldForm}]], {_Column..} ]
  ,
  True
  ,
  TestID -> "Dataset-NonFullArray-2"
];

VerificationTest[(* 19 *)
  MatchQ[RecordsSummary[dsMixedDataWithNamedRows], {_Column..} ]
  ,
  True
  ,
  TestID -> "Dataset-WithNamedRows-1"
];

VerificationTest[(* 20 *)
  MatchQ[RecordsSummary[matMixedData[[All,2]], "2nd" ], {_Column} ]
  ,
  True
  ,
  TestID -> "Vector-specified-column-name-1"
];

VerificationTest[(* 21 *)
  MatchQ[RecordsSummary[matMixedData, RandomWord["KnownWords", Dimensions[matMixedData][[2]] ] ], {_Column..} ]
  ,
  True
  ,
  TestID -> "Array-specified-column-names-1"
];

VerificationTest[(* 21 *)
  MatchQ[RecordsSummary[dsMixedData, RandomWord["KnownWords", Dimensions[dsMixedData][[2]] ] ], {_Column..} ]
  ,
  True
  ,
  RecordsSummary::igncols
  ,
  TestID -> "Dataset-specified-column-names-1"
];

EndTestSection[]
