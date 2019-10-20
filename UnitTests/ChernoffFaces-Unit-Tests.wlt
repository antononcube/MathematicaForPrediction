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
  Head /@ Map[ ChernoffFace, data]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-1"
];


VerificationTest[(* 6 *)
  Head /@ Map[ ChernoffFace[#, ColorFunction -> Automatic, ImageSize -> Tiny]&, data]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-2"
];


VerificationTest[(* 7 *)
  Head /@ Map[ ChernoffFace[#, ColorFunction -> ColorData["BrightBands"]] &, VariablesRescale[data]]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-ColorFunction-1"
];


VerificationTest[(* 8 *)
  k = Length[data[[1]]];
  Head /@
      Map[
        ChernoffFace[
          Append[
            AssociationThread[Take[Keys@ChernoffFace["FacePartsProperties"], k] -> Take[#, k], "MakeSymmetric" -> False],
            ColorFunction -> ColorData["BrightBands"]
          ]
        ]&,
        VariablesRescale[data]
      ]
  ,
  Table[Graphics, Length[data]]
  ,
  TestID -> "NumericMatrix-ColorFunction-2"
];


VerificationTest[(* 9 *)
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


VerificationTest[(* 10 *)
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


EndTestSection[]
