(*
    Monadic Sparse Matrix Recommender Mathematica unit tests
    Copyright (C) 2018  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicSparseMatrixRecommender-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2018-09-15 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: monad, monadic, sparse matrix, recommender, workflow, State monad, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the package

     https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m

*)
BeginTestSection["MonadicSparseMatrixRecommender-Unit-Tests.mt"]


VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicSparseMatrixRecommender.m"],
    Greater[Length[SubValues[MonadicSparseMatrixRecommender`SMRMonCreate]], 0]
  ]
  ,
  True
  ,
  TestID->"LoadPackage"
]

VerificationTest[(* 2 *)
  SeedRandom[342]
  data = RandomInteger[{0, 20}, {100, 5}];
  ds = Dataset[MapThread[Prepend, {data, Range[Length[data]]}]];
  ds = ds[All,
    AssociationThread[
      Prepend[ToString /@ Range[Length[data[[1]]]], "id"], #] &];
  MatchQ[ds, _Dataset] && Count[ds, 0, Infinity] > 10
  ,
  True
  ,
  TestID->"GenerateData"
]

VerificationTest[(* 2 *)
  dsMiss = ds /. {0 -> Missing[]};
  MatchQ[dsMiss, _Dataset] && Count[dsMiss, _Missing, Infinity] > 10
  ,
  True
  ,
  TestID->"MakeDataWithMissing"
]


EndTestSection[]
