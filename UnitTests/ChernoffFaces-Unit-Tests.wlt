(*
    ChernoffFaces Mathematica unit tests
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
(* :Date: 2019-10-20 *)

(* :Package Version: 0.7 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: Chernoff face, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the function ChernoffFace and related functions in the package:

     https://github.com/antononcube/MathematicaForPrediction/blob/master/ChernoffFaces.m

*)

BeginTestSection["ChernoffFaces-Unit-Tests.wlt"];

VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/ChernoffFaces.m"],
    Greater[Length[DownValues[ChernoffFaces`ChernoffFace]], 0]
  ]
  ,
  True
  ,
  TestID -> "LoadPackage"
];


VerificationTest[(* 2 *)
  SeedRandom[1425];
  {dists, data} =
      Transpose[
        Table[(
          rdist =
              RandomChoice[{NormalDistribution[RandomReal[10],
                RandomReal[10]], PoissonDistribution[RandomReal[4]],
                BetaDistribution[RandomReal[2], 1.5]}];
          {rdist, RandomVariate[rdist, 12]}), {12}]
      ];
  MatrixQ[data, NumericQ]
  ,
  True
  ,
  TestID -> "Generated-data-1"
];


VerificationTest[(* 3 *)
  Head /@ Table[ChernoffFace[], 36]
  ,
  Table[Graphics, 36]
  ,
  TestID -> "NoArguments-1"
];


VerificationTest[(* 4 *)
  Head /@ Table[ChernoffFace[ColorFunction -> ColorData["BrightBands"], ImageSize -> Tiny], 36]
  ,
  Table[Graphics, 36]
  ,
  TestID -> "ColorFunction-1"
];


VerificationTest[(* 5 *)
  Head /@ Table[ChernoffFace[ColorFunction -> Automatic, ImageSize -> Tiny], 36]
  ,
  Table[Graphics, 36]
  ,
  TestID -> "ColorFunction-2"
];


VerificationTest[(* 6 *)
  Head /@ Table[ChernoffFace[ColorFunction -> None, ImageSize -> Tiny], 36]
  ,
  Table[Graphics, 36]
  ,
  TestID -> "ColorFunction-3"
];


VerificationTest[(* 7 *)
  Head /@ Map[ ChernoffFace, data]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-1"
];


VerificationTest[(* 8 *)
  Head /@ Map[ ChernoffFace[#, ColorFunction -> Automatic, ImageSize -> Tiny]&, data]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-2"
];


VerificationTest[(* 9 *)
  Head /@ Map[ ChernoffFace[#, ColorFunction -> ColorData["BrightBands"]] &, VariablesRescale[data]]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-rowwise-ColorFunction-1"
];


VerificationTest[(* 10 *)
  k = Length[data[[1]]];
  Head /@
      Map[
        ChernoffFace[
          Append[
            AssociationThread[Take[Keys @ ChernoffFace["FacePartsProperties"], k] -> Take[#, k], "MakeSymmetric" -> False],
            ColorFunction -> ColorData["BrightBands"]
          ]
        ]&,
        VariablesRescale[data]
      ]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-rowwise-ColorFunction-2"
];


VerificationTest[(* 11 *)
  KeySort @ ChernoffFace["Properties"]
  ,
  <|"EyeBallColor" -> Automatic, "EyeSize" -> 0.5, "EyeSlant" -> 0.5,
    "EyesVerticalPosition" -> 0.5, "FaceColor" -> Automatic,
    "FaceLength" -> 0.5, "ForeheadShape" -> 0.5,
    "IrisColor" -> Automatic, "LeftEyebrowRaising" -> 0.5,
    "LeftEyebrowSlant" -> 0.5, "LeftEyebrowTrim" -> 0.5,
    "LeftIris" -> 0.5, "MakeSymmetric" -> True,
    "MouthColor" -> Automatic, "MouthSmile" -> 0.5, "MouthTwist" -> 0.5,
    "MouthWidth" -> 0.5, "NoseColor" -> Automatic, "NoseLength" -> 0.5,
    "RightEyebrowRaising" -> 0.5, "RightEyebrowSlant" -> 0.5,
    "RightEyebrowTrim" -> 0.5, "RightIris" -> 0.5|>
  ,
  TestID -> "Properties-1"
];


VerificationTest[(* 12 *)
  KeySort@ChernoffFace["FacePartsProperties"]
  ,
  <|"EyeSize" -> 0.5, "EyeSlant" -> 0.5, "EyesVerticalPosition" -> 0.5,
    "FaceLength" -> 0.5, "ForeheadShape" -> 0.5,
    "LeftEyebrowRaising" -> 0.5, "LeftEyebrowSlant" -> 0.5,
    "LeftEyebrowTrim" -> 0.5, "LeftIris" -> 0.5, "MouthSmile" -> 0.5,
    "MouthTwist" -> 0.5, "MouthWidth" -> 0.5, "NoseLength" -> 0.5,
    "RightEyebrowRaising" -> 0.5, "RightEyebrowSlant" -> 0.5,
    "RightEyebrowTrim" -> 0.5, "RightIris" -> 0.5|>
  ,
  TestID -> "Properties-2"
];


VerificationTest[(* 13 *)
  MatrixQ[ VariablesRescale[data], NumberQ ]
  ,
  True
  ,
  TestID -> "VariablesRescale-1"
];


VerificationTest[(* 14 *)
  Apply[ And, Flatten @ Map[ 0<= # <= 1&, VariablesRescale[ RandomReal[{-100,100}, {100, 17}] ], {-1} ] ]
  ,
  True
  ,
  TestID -> "VariablesRescale-2"
];


VerificationTest[(* 15 *)
  Head /@ Table[ChernoffFace["MakeSymmetric" -> True], 36]
  ,
  Table[Graphics, 36]
  ,
  TestID -> "MakeSymmetric-1"
];


VerificationTest[(* 16 *)
  Head /@ Table[ChernoffFace["MakeSymmetric" -> False], 36]
  ,
  Table[Graphics, 36]
  ,
  TestID -> "MakeSymmetric-2"
];


VerificationTest[(* 17 *)
  SeedRandom[23];
  rmat = RandomVariate[SkewNormalDistribution[0, 3, 0.1], {25, 10}];
  res = ChernoffFace[ rmat, "Summary" ];
  MatchQ[ res, Association[ (_String -> { _Graphics ..}) ..] ]
  ,
  True
  ,
  TestID -> "Summary-1"
];


VerificationTest[(* 18 *)
  res = ChernoffFace[ rmat[[1]], "Summary" ];
  res === $Failed
  ,
  True
  ,
  {ChernoffFace::sarg}
  ,
  TestID -> "Summary-2"
];


VerificationTest[(* 19 *)
  ascs = {<|"ForeheadShape" -> 0.219738, "EyeSize" -> 3.3445,
    "LeftEyebrowSlant" -> 0.740124,
    "NoseLength" -> 3.57202|>, <|"ForeheadShape" -> 0.277967,
    "EyeSize" -> 2.99418, "LeftEyebrowSlant" -> 1.32757,
    "NoseLength" -> 5.06625|>, <|"ForeheadShape" -> -1.27702,
    "EyeSize" -> 3.4797, "LeftEyebrowSlant" -> 4.36544,
    "NoseLength" -> -0.251473|>, <|"ForeheadShape" -> 2.83725,
    "EyeSize" -> 0.725232, "LeftEyebrowSlant" -> -0.284936,
    "NoseLength" -> -1.72739|>, <|"ForeheadShape" -> 0.796431,
    "EyeSize" -> 0.0763072, "LeftEyebrowSlant" -> 2.5892,
    "NoseLength" -> 7.22182|>};
  Head /@ ChernoffFace[ascs]
  ,
  Table[Graphics, Length[ascs]]
  ,
  TestID -> "List-of-associations-1"
];


VerificationTest[(* 20 *)
  Head /@ ChernoffFace[data, ColorFunction -> ColorData["BrightBands"]]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-ColorFunction-1"
];


VerificationTest[(* 21 *)
  gr1 = Map[ ChernoffFace[#, ColorFunction -> ColorData["BrightBands"]] &, VariablesRescale[data]];
  gr2 = ChernoffFace[data, ColorFunction -> ColorData["BrightBands"]];
(*  imgDists = MapThread[ImageDistance, {gr1, gr2}];*)
(*  Max[imgDists] <= 10^(-12)*)
  gr1 == gr2
  ,
  True
  ,
  TestID -> "NumericMatrix-ColorFunction-2"
];


EndTestSection[]
