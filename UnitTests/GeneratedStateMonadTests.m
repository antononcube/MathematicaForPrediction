(*
    Generated State Monad Mathematica unit tests
    Copyright (C) 2017  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: GeneratedStateMonadTests *)
(* :Context: GeneratedStateMonadTests` *)
(* :Author: antonov *)
(* :Date: 2017-06-18 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

   Before running the tests examine the test with TestID->"LoadPackage".

*)

(*

 TODO:
   1. [ ] Basic value manipulation tests.
   2. [ ] Using pure functions in the pipeline.
   3. [ ] Failing with messages.
   4. [ ] Basic context manipulation tests.
   5. [ ] Basic control flow functions tests.
   6. [X] Iteration functions tests.
   6.1. [X] Fold and FoldList tests.
   6.2. [X] NestList, NestWhileList, and FixedPointList tests.
   6.3. [X] Nest, NestWhile, and FixedPoint tests.

*)
BeginTestSection["GeneratedStateMonadTests"]

VerificationTest[(* 1 *)
  CompoundExpression[Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"], GenerateStateMonadCode["StMon", Rule["StringContextNames", True], Rule["FailureSymbol", None], Rule["EchoFailingFunction", True]], Greater[Length[DownValues[StMonAddToContext]], 0]]
  ,
  True
  ,
  TestID->"LoadPackage"
]

VerificationTest[(* 2 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[Nest, Function[StMonUnit[Plus[Slot[1], 1], Slot[2]]], 4]]
  ,
  StMon[17, Association[]]
  ,
  TestID->"Iterate-Nest-1"
]

VerificationTest[(* 3 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[NestList, Function[StMonUnit[Plus[Slot[1], 1], Slot[2]]], 4, None]]
  ,
  StMon[List[13, 14, 15, 16, 17], Association[]]
  ,
  TestID->"Iterate-NestList-1"
]

VerificationTest[(* 4 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[NestList, Function[StMonUnit[Plus[Slot[1], 1], Slot[2]]], 4, "nlData"]]
  ,
  StMon[List[13, 14, 15, 16, 17], Association[Rule["nlData", List[StMon[13, Association[]], StMon[14, Association[]], StMon[15, Association[]], StMon[16, Association[]], StMon[17, Association[]]]]]]
  ,
  TestID->"Iterate-NestList-1-var"
]

VerificationTest[(* 5 *)
  DoubleLongRightArrow[StMonUnit[1.`], StMonIterate[FixedPoint, Function[StMonUnit[Times[Plus[Slot[1], Times[2, Power[Slot[1], -1]]], Power[2, -1]], Slot[2]]]]]
  ,
  StMon[FixedPoint[Function[Times[Plus[Slot[1], Times[2, Power[Slot[1], -1]]], Power[2, -1]]], 1.`], Association[]]
  ,
  TestID->"Iterate-FixedPoint-Sqrt[2]"
]

VerificationTest[(* 6 *)
  DoubleLongRightArrow[StMonUnit[1.`], StMonIterate[FixedPointList, Function[StMonUnit[Times[Plus[Slot[1], Times[2, Power[Slot[1], -1]]], Power[2, -1]], Slot[2]]], None]]
  ,
  StMon[FixedPointList[Function[Times[Plus[Slot[1], Times[2, Power[Slot[1], -1]]], Power[2, -1]]], 1.`], Association[]]
  ,
  TestID->"Iterate-FixedPointList-Sqrt[2]"
]

VerificationTest[(* 7 *)
  CompoundExpression[Set[res1, DoubleLongRightArrow[StMonUnit[1.`], StMonIterate[FixedPointList, Function[StMonUnit[Times[Plus[Slot[1], Times[2, Power[Slot[1], -1]]], Power[2, -1]], Slot[2]]], "fplData"], StMonRetrieveFromContext["fplData"], Function[List[Tally[Map[Head, Slot[1]]], Map[First, Slot[1]]]]]], Set[res2, Function[List[List[List[StMon, Length[Slot[1]]]], Slot[1]]][FixedPointList[Function[Times[Plus[Slot[1], Times[2, Power[Slot[1], -1]]], Power[2, -1]]], 1.`]]], Equal[res1, res2]]
  ,
  True
  ,
  TestID->"Iterate-FixedPointList-Sqrt[2]-var"
]

VerificationTest[(* 8 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[NestWhile, Function[StMonUnit[Plus[Slot[1], 1], Slot[2]]], Function[Less[Part[Slot[1], 1], 20]]]]
  ,
  StMon[20, Association[]]
  ,
  TestID->"Iterate-NestWhile-1"
]

VerificationTest[(* 9 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[NestWhile, Function[StMonUnit[Plus[Slot[1], 1], Slot[2]]], Function[Less[Part[Slot[1], 1], 20]], 1, 4]]
  ,
  StMon[17, Association[]]
  ,
  TestID->"Iterate-NestWhile-2"
]

VerificationTest[(* 10 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[NestWhileList, Function[Echo[StMonUnit[Plus[Slot[1], 2], Slot[2]]]], Function[Less[Part[Slot[1], 1], 20]], 1]]
  ,
  StMon[List[13, 15, 17, 19, 21], Association[]]
  ,
  TestID->"Iterate-NestWhileList-1"
]

VerificationTest[(* 11 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[NestWhileList, Function[Echo[StMonUnit[Plus[Slot[1], 2], Slot[2]]]], Function[Less[Part[Slot[1], 1], 20]], 1, "nwlData"]]
  ,
  StMon[List[13, 15, 17, 19, 21], Association[Rule["nwlData", List[StMon[13, Association[]], StMon[15, Association[]], StMon[17, Association[]], StMon[19, Association[]], StMon[21, Association[]]]]]]
  ,
  TestID->"Iterate-NestWhileList-1-var"
]

VerificationTest[(* 12 *)
  DoubleLongRightArrow[StMonUnit[13], StMonIterate[Fold, Function[StMonUnit[Plus[Part[Slot[1], 1], Slot[2]], Part[Slot[1], 2]]], Array[a, 4]]]
  ,
  StMon[Plus[13, a[1], a[2], a[3], a[4]], Association[]]
  ,
  TestID->"Iterate-Fold-1"
]

VerificationTest[(* 13 *)
  CompoundExpression[Set[res1, DoubleLongRightArrow[StMonUnit[13], StMonIterate[Composition[StMonUnit, FoldList], Function[StMonUnit[Plus[Part[Slot[1], 1], Slot[2]], Part[Slot[1], 2]]], Array[a, 4]]]], Set[res2, DoubleLongRightArrow[StMonUnit[13], StMonIterate[Composition[StMonUnit, FoldList], Function[StMonUnit[Plus[Part[Slot[1], 1], Slot[2]], Part[Slot[1], 2]]], Array[a, 4]], StMonEchoValue]], Equal[res1, res2]]
  ,
  True
  ,
  TestID->"Iterate-FoldListComposition-1"
]

VerificationTest[(* 14 *)
  DoubleLongRightArrow[StMonUnit[13, Association[Rule["foldData", List[]]]], StMonIterate[Fold, Function[StMonUnit[Plus[Part[Slot[1], 1], Slot[2]], Join[Part[Slot[1], 2], Association[Rule["foldData", Append[Part[Slot[1], 2]["foldData"], List[Slot[2], Plus[Part[Slot[1], 1], Slot[2]]]]]]]]], List[1, 100, 1000]], StMonEchoValue, StMonEchoContext]
  ,
  StMon[1114, Association[Rule["foldData", List[List[1, 14], List[100, 114], List[1000, 1114]]]]]
  ,
  TestID->"Iterate-Fold-with-data-1"
]

EndTestSection[]
