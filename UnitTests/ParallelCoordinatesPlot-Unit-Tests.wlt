(*
    ParallelCoordinatesPlot Mathematica unit tests
    Copyright (C) 2020 Anton Antonov

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

(* :Title: ParallelCoordinatesPlot-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2020-08-25 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Parallel Coordinates, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the function ParallelCoordinatesPlot and related functions in the package:

     https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/ParallelCoordinatesPlot.m

*)

BeginTestSection["ParallelCoordinatesPlot-Unit-Tests.wlt"];

VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/ParallelCoordinatesPlot.m"],
    Greater[Length[DownValues[ParallelCoordinatesPlot`ParallelCoordinatesPlot]], 0]
  ]
  ,
  True
  ,
  TestID -> "LoadPackage"
];


VerificationTest[
  SeedRandom[232];
  n = 70;
  aRData = GroupBy[MapThread[Prepend, {Sort@RandomReal[{-4, 10}, {n, 5}], Sort@RandomChoice[RandomWord[3], n]}], First, #[[All, 2 ;; -1]] &];
  Apply[And, MatrixQ /@ Values[aRData] ]
  ,
  True
  ,
  TestID -> "Generated-data-1"
];

VerificationTest[
  SeedRandom[1425];
  {dists, matData} =
      Transpose[
        Table[(
          rdist =
              RandomChoice[{NormalDistribution[RandomReal[10],
                RandomReal[10]], PoissonDistribution[RandomReal[4]],
                BetaDistribution[RandomReal[2], 1.5]}];
          {rdist, RandomVariate[rdist, 12]}), {12}]
      ];
  MatrixQ[matData, NumericQ]
  ,
  True
  ,
  TestID -> "Generated-data-2"
];


(************************************************************)
(* Matrix                                                   *)
(************************************************************)

VerificationTest[
  Head @ ParallelCoordinatesPlot[matData]
  ,
  Graphics
  ,
  TestID -> "Matrix-1"
];

VerificationTest[
  Head @ ParallelCoordinatesPlot[matData]
  ,
  Graphics
  ,
  TestID -> "Matrix-2"
];

VerificationTest[
  Head @ ParallelCoordinatesPlot[matData, Automatic, "Colors" -> ColorData["BrightBands"], ImageSize -> Small]
  ,
  Graphics
  ,
  TestID -> "Matrix-3"
];

VerificationTest[
  Head @ ParallelCoordinatesPlot[matData, Automatic, Automatic, ImageSize -> Small]
  ,
  Graphics
  ,
  TestID -> "Matrix-4"
];

VerificationTest[
  Head @ ParallelCoordinatesPlot[matData, Range @ Length @ matData[[1]], Automatic, ImageSize -> Small]
  ,
  Graphics
  ,
  TestID -> "Matrix-5"
];

VerificationTest[
  Head @ ParallelCoordinatesPlot[matData, Automatic, MinMax /@ Transpose[matData], "Direction" -> "Vertical", ImageSize -> Small]
  ,
  Graphics
  ,
  TestID -> "Matrix-6"
];


(************************************************************)
(* Association of matrices                                  *)
(************************************************************)

VerificationTest[
  MatchQ[ #, Legended[ _Graphics, _ ] ]& @ ParallelCoordinatesPlot[aRData]
  ,
  True
  ,
  TestID -> "Association-1"
];

VerificationTest[
  MatchQ[ #, Legended[ _Graphics, _ ] ]& @ ParallelCoordinatesPlot[aRData, Automatic]
  ,
  True
  ,
  TestID -> "Association-2"
];


VerificationTest[
  MatchQ[ #, Legended[ _Graphics, _ ] ]& @ ParallelCoordinatesPlot[aRData, Automatic, Automatic]
  ,
  True
  ,
  TestID -> "Association-3"
];


VerificationTest[
  MatchQ[ #, Legended[ _Graphics, _ ] ]& @ ParallelCoordinatesPlot[aRData, Range @ Length @aRData[[1, 1]] ]
  ,
  True
  ,
  TestID -> "Association-4"
];


VerificationTest[
  MatchQ[ #, Legended[ _Graphics, _ ] ]& @ ParallelCoordinatesPlot[aRData, Range @ Length @aRData[[1, 1]], Automatic]
  ,
  True
  ,
  TestID -> "Association-5"
];


(************************************************************)
(* Messages                                                 *)
(************************************************************)

VerificationTest[
  ParallelCoordinatesPlot[dfd, Automatic]
  ,
  $Failed
  ,
  {ParallelCoordinatesPlot::args}
  ,
  TestID -> "WrongArgs-1"
];

VerificationTest[
  ParallelCoordinatesPlot[matData, {"var1", "var2"} ]
  ,
  $Failed
  ,
  {ParallelCoordinatesPlot::ncoln}
  ,
  TestID -> "WrongArgs-2"
];

VerificationTest[
  ParallelCoordinatesPlot[matData, Automatic, "blah" ]
  ,
  $Failed
  ,
  {ParallelCoordinatesPlot::args}
  ,
  TestID -> "WrongArgs-3"
];


EndTestSection[]
