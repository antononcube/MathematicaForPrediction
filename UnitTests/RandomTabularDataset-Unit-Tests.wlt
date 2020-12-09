(*
    RandomTabularDataset Mathematica unit tests
    Copyright (C) 2020  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: RandomTabularDataset-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2020-12-08 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: random, tabular, dataset, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the function RandomTabularDataset in the package

     https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RandomTabularDataset.m

*)

BeginTestSection["RandomTabularDataset-Unit-Tests.wlt.mt"];

VerificationTest[(* 1 *)
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/RandomTabularDataset.m"];
  Length[DownValues[RandomTabularDataset`RandomTabularDataset]] > 0
  ,
  True
  ,
  TestID -> "LoadPackage"
];


(***********************************************************)
(* Basic signatures                                        *)
(***********************************************************)

VerificationTest[
  SeedRandom[32];
  ds1 = RandomTabularDataset[];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] > 0 &&
      Dimensions[ds1][[2]] > 0
  ,
  True
  ,
  TestID -> "Basic-no-arguments-1"
];


VerificationTest[
  ds1 = RandomTabularDataset[30];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] == 30 &&
      Dimensions[ds1][[2]] > 0
  ,
  True
  ,
  TestID -> "Basic-just-nRows-spec-1"
];


VerificationTest[
  ds1 = RandomTabularDataset[{20, Automatic}];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] == 20 &&
      Dimensions[ds1][[2]] > 0
  ,
  True
  ,
  TestID -> "Basic-automatic-nColumns-1"
];


VerificationTest[
  ds1 = RandomTabularDataset[{Automatic, 12}];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] > 0 &&
      Dimensions[ds1][[2]] == 12
  ,
  True
  ,
  TestID -> "Basic-automatic-nRows-1"
];


VerificationTest[
  lsColNames = {"f", "a", "c"};
  ds1 = RandomTabularDataset[{Automatic, lsColNames}];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] > 0 &&
      Dimensions[ds1][[2]] > 0 &&
      Normal[Keys[ds1[1]]] == {"f", "a", "c"}
  ,
  True
  ,
  TestID -> "Basic-explicit-column-names-1"
];


VerificationTest[
  lsColNames = {"a", "a", "b", "b", "c"};
  ds1 = RandomTabularDataset[{Automatic, lsColNames}];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] > 0 &&
      Dimensions[ds1][[2]] > 0 &&
      Normal[Keys[ds1[1]]] == {"a", "b", "c"}
  ,
  True
  ,
  TestID -> "Basic-explicit-column-names-2"
];


(***********************************************************)
(* Column names generator specs                            *)
(***********************************************************)

VerificationTest[
  ds1 = RandomTabularDataset[{23, 12}, "ColumnNameGenerator" -> (RandomWord[#]&) ];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == 12
  ,
  True
  ,
  TestID -> "ColumnNamesGenerator-1"
];


VerificationTest[
  ds1 = RandomTabularDataset[{23, 12}, "ColumnNameGenerator" -> (RandomWord[]&), "PointwiseGeneration" -> True ];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == 12
  ,
  True
  ,
  TestID -> "ColumnNamesGenerator-2"
];


VerificationTest[
  ds1 = RandomTabularDataset[{23, 12}, "ColumnNameGenerator" -> RandomWord ];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == 12
  ,
  True
  ,
  TestID -> "ColumnNamesGenerator-3"
];


VerificationTest[
  ds1 = RandomTabularDataset[{23, 12}, "ColumnNameGenerator" -> RandomWord, "PointwiseGeneration" -> True ];
  TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == 12
  ,
  True
  ,
  TestID -> "ColumnNamesGenerator-4"
];


VerificationTest[
  ClearAll[F];
  nCols = 4;
  ds1 = RandomTabularDataset[{23, nCols}, "ColumnNameGenerator" -> (Table[ToString[F[i]], {i, #}] &)];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == nCols &&
      Normal[Keys[ds1[1]]] == Table[ToString[F[i]], {i, nCols}]
  ,
  True
  ,
  TestID -> "ColumnNamesGenerator-5"
];


VerificationTest[
  nCols = 6;
  ds1 = RandomTabularDataset[{23, nCols}, "ColumnNameGenerator" -> ("A"&), "PointwiseGeneration" -> True];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == nCols &&
      Normal[Keys[ds1[1]]] == Join[ {"A"}, Table[ToString[i], {i, 2, nCols}]]
  ,
  True
  ,
  {RandomTabularDataset::ngcols}
  ,
  TestID -> "ColumnNamesGenerator-6"
];


(***********************************************************)
(* Column values generator specs - list extrapolation      *)
(***********************************************************)

VerificationTest[
  ds1 = RandomTabularDataset[{23, 12}, "ColumnValueGenerators" -> {RandomWord[#] &}];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == 12 &&
      VectorQ[Flatten[Normal[ds1[Values]]], StringQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-list-extrapolation-1"
];


VerificationTest[
  ds1 = RandomTabularDataset[{23, 12}, "ColumnValueGenerators" -> {RandomReal[100, #] &}];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == 12 &&
      VectorQ[Flatten[Normal[ds1[Values]]], NumberQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-list-extrapolation-2"
];


VerificationTest[
  {m, n} = {20, 10};
  ds1 = RandomTabularDataset[{m, n}, "ColumnValueGenerators" -> {RandomReal[100, #]&, RandomWord[#]&}];
  vec1 = Flatten[Normal[ds1[Values]]];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[vec1, NumberQ[#] || StringQ[#] &] &&
      Count[ StringQ /@ vec1, True ] == m * n / 2
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-list-extrapolation-3"
];


VerificationTest[
  {m, n} = {20, 10};
  ds1 = RandomTabularDataset[{m, n}, "ColumnValueGenerators" -> {RandomReal[100, #] &, Table[F[i], {i, #}] &, RandomWord[#] &}];
  vec1 = Flatten[Normal[ds1[Values]]];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[vec1, NumberQ[#] || StringQ[#] || Head[#] === F &] &&
      Count[vec1, F[_]] == m * Floor[n / 3]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-list-extrapolation-4"
];


(***********************************************************)
(* Column values generator specs - shorthand of Random*    *)
(***********************************************************)

VerificationTest[
  {m, n} = {12, 5};
  SeedRandom[3223];
  ds1 = RandomTabularDataset[{m, n},
    "ColumnValueGenerators" -> <|1 -> RandomInteger, 2 -> RandomWord, 3 -> RandomColor, 4 -> RandomReal, 5 -> RandomImage|>];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], IntegerQ] &&
      VectorQ[Normal@ds1[All, 2], StringQ] &&
      VectorQ[Normal@ds1[All, 3], MatchQ[#, _RGBColor] &] &&
      VectorQ[Normal@ds1[All, 4], NumberQ] &&
      VectorQ[Normal@ds1[All, 5], ImageQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Random-1"
];


VerificationTest[
  {m, n} = {12, 5};
  SeedRandom[3223];
  ds1 = RandomTabularDataset[{m, n},
    "ColumnValueGenerators" -> <|1 -> RandomInteger, 2 -> RandomWord, 3 -> RandomColor, 4 -> RandomReal, 5 -> RandomImage|>,
    "PointwiseGeneration" -> True];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], IntegerQ] &&
      VectorQ[Normal@ds1[All, 2], StringQ] &&
      VectorQ[Normal@ds1[All, 3], MatchQ[#, _RGBColor] &] &&
      VectorQ[Normal@ds1[All, 4], NumberQ] &&
      VectorQ[Normal@ds1[All, 5], ImageQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Random-2"
];


VerificationTest[
  {m, n} = {12, 5};
  SeedRandom[3223];
  ds1 = RandomTabularDataset[{m, n},
    "ColumnValueGenerators" -> {RandomInteger, RandomWord, RandomColor}];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], IntegerQ] &&
      VectorQ[Normal@ds1[All, 2], StringQ] &&
      VectorQ[Normal@ds1[All, 3], MatchQ[#, _RGBColor] &] &&
      VectorQ[Normal@ds1[All, 4], IntegerQ] &&
      VectorQ[Normal@ds1[All, 5], StringQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Random-3"
];


VerificationTest[
  {m, n} = {12, 5};
  SeedRandom[3223];
  ds1 = RandomTabularDataset[{m, n},
    "ColumnValueGenerators" -> {RandomInteger, RandomWord, RandomColor},
    "PointwiseGeneration" -> True];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], IntegerQ] &&
      VectorQ[Normal@ds1[All, 2], StringQ] &&
      VectorQ[Normal@ds1[All, 3], MatchQ[#, _RGBColor] &] &&
      VectorQ[Normal@ds1[All, 4], IntegerQ] &&
      VectorQ[Normal@ds1[All, 5], StringQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Random-4"
];


(***********************************************************)
(* Column values generator by association specs            *)
(***********************************************************)

VerificationTest[
  {m, n} = {12, 5};
  ds1 = RandomTabularDataset[{m, n},
    "ColumnValueGenerators" -> <|1 -> (RandomReal[100, #]&), 3 -> (RandomWord["CommonWords", #]&) |>];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], NumberQ] &&
      VectorQ[Normal@ds1[All, 3], StringQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-association-spec-1"
];


VerificationTest[
  {m, n} = {12, 3};
  ds1 = RandomTabularDataset[{m, {"b", "a", "c"}},
    "ColumnValueGenerators" -> <|1 -> (RandomReal[100, #]&), "b" -> RandomColor |>];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], MatchQ[#, _RGBColor]&]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-association-spec-1"
];


(***********************************************************)
(* Column values generators distribution shorthand specs   *)
(***********************************************************)

VerificationTest[
  {m, n} = {12, 5};
  ds1 = RandomTabularDataset[{m, n}, "ColumnValueGenerators" -> {NormalDistribution[12, 1], PoissonDistribution[23]}, "PointwiseGeneration" -> False];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], NumberQ] &&
      VectorQ[Normal@ds1[All, 2], IntegerQ] &&
      VectorQ[Normal@ds1[All, 3], NumberQ] &&
      VectorQ[Normal@ds1[All, 4], IntegerQ] &&
      VectorQ[Normal@ds1[All, 5], NumberQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Distributions-1"
];


VerificationTest[
  {m, n} = {12, 5};
  ds1 = RandomTabularDataset[{m, n}, "ColumnValueGenerators" -> {NormalDistribution[12, 1], PoissonDistribution[23]}, "PointwiseGeneration" -> True];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], NumberQ] &&
      VectorQ[Normal@ds1[All, 2], IntegerQ] &&
      VectorQ[Normal@ds1[All, 3], NumberQ] &&
      VectorQ[Normal@ds1[All, 4], IntegerQ] &&
      VectorQ[Normal@ds1[All, 5], NumberQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Distributions-2"
];


VerificationTest[
  {m, n} = {12, 5};
  ds1 = RandomTabularDataset[{m, n}, "ColumnValueGenerators" -> <| 3 -> SkewNormalDistribution[12, 1, 0.4], 1 -> PoissonDistribution[23] |>, "PointwiseGeneration" -> False];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], IntegerQ] &&
      VectorQ[Normal@ds1[All, 3], NumberQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Distributions-3"
];


VerificationTest[
  {m, n} = {12, 5};
  ds1 = RandomTabularDataset[{m, n}, "ColumnValueGenerators" -> <| 3 -> SkewNormalDistribution[12, 1, 0.4], 1 -> PoissonDistribution[23] |>, "PointwiseGeneration" -> True];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], IntegerQ] &&
      VectorQ[Normal@ds1[All, 3], NumberQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Distributions-4"
];


VerificationTest[
  {m, n} = {12, 5};
  ds1 =
      RandomTabularDataset[{m, n},
        "ColumnValueGenerators" ->
            <|
              3 -> MixtureDistribution[ {0.5, 06}, {SkewNormalDistribution[12, 1, 0.4], LogisticDistribution[32, 2]}],
              1 -> PoissonDistribution[23]
            |>
      ];
  TrueQ[Head[ds1] === Dataset] &&
      Dimensions[ds1][[1]] == m &&
      Dimensions[ds1][[2]] == n &&
      VectorQ[Normal@ds1[All, 1], IntegerQ] &&
      VectorQ[Normal@ds1[All, 3], NumberQ]
  ,
  True
  ,
  TestID -> "ColumnValueGenerators-shorthand-Distributions-5"
];


(***********************************************************)
(* MaxNumberOfValues                                       *)
(***********************************************************)



(***********************************************************)
(* MinNumberOfValues                                       *)
(***********************************************************)



(***********************************************************)
(* PointwiseGeneration                                     *)
(***********************************************************)

VerificationTest[
  SeedRandom[42];
  ds1 = RandomTabularDataset[{23, 12}, "ColumnNameGenerator" -> (RandomWord[#]&) ];
  SeedRandom[42];
  ds2 = RandomTabularDataset[{23, 12}, "ColumnNameGenerator" -> (RandomWord[#]&), "PointwiseGeneration" -> False ];
  ds1 == ds2 &&
      TrueQ[ Head[ds1] === Dataset ] &&
      Dimensions[ds1][[1]] == 23 &&
      Dimensions[ds1][[2]] == 12
  ,
  True
  ,
  TestID -> "PointwiseGeneration-Default-1"
];


EndTestSection[]
