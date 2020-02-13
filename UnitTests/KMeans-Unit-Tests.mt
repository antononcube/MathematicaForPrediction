(*
    KMeans Mathematica unit tests
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

(* :Title: KMeans-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2020-02-13 *)

(* :Package Version: 0.3 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: K-means, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

*)
BeginTestSection["KMeans-Unit-Tests.mt"];

VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/KMeans.m"],
    Greater[Length[DownValues[ChernoffFaces`KMeans]], 0]
  ]
  ,
  True
  ,
  TestID -> "LoadPackage"
];


(***********************************************************)
(* Generate data                                           *)
(***********************************************************)

VerificationTest[(* 2 *)
  SeedRandom[1295];

  pointsPerCluster = 100;
  points2D = <||>;

  points2D["3clusters"] =
      Flatten[#, 1] &@
          MapThread[
            Transpose[{RandomReal[NormalDistribution[#1, #3],
              pointsPerCluster],
              RandomReal[NormalDistribution[#2, #3], pointsPerCluster]}] &,
            Transpose[{{10, 20, 4}, {20, 60, 6}, {40, 10, 6}}]];
  points2D["3clusters"] = RandomSample[points2D["3clusters"]];

  points2D["5clusters"] =
      Flatten[#, 1] &@
          MapThread[
            Transpose[{RandomReal[NormalDistribution[#1, #3],
              pointsPerCluster],
              RandomReal[NormalDistribution[#2, #3], pointsPerCluster]}] &,
            Transpose[{{10, 20, 4}, {20, 60, 6}, {40, 10, 6}, {0, 0, 4}, {100, 100, 8}}]];

  points2D["5clusters"] = RandomSample[points2D["5clusters"]];

  Apply[ And, MatrixQ[#, NumericQ] /@ Values[points2D] ]
  ,
  True
  ,
  TestID -> "Generated-2D-data-1"
];


VerificationTest[(* 3 *)
  SeedRandom[2311];

  pointsPerCluster = 100;
  points3D = <||>;

  pnts = MapThread[
    Transpose[{RandomReal[NormalDistribution[#1, #2],
      pointsPerCluster],
      RandomReal[NormalDistribution[#3, #4], pointsPerCluster],
      RandomReal[NormalDistribution[#3, #4], pointsPerCluster]}] &,
    Transpose[{{10, 2, 10, 3, 10, 1}, {20, 2, 30, 5, 20, 3}, {40, 3, 10, 1, 20, 2}, {40, 3, 40, 3, 40, 1}, {10, 5, 40, 2, 40, 4}}]];
  rmat = {RotationMatrix[-Pi / 3, {0, 0, 1}],
    RotationMatrix[-Pi / 6, {0, 1, 1}],
    RotationMatrix[Pi / 4, {0, 1, 1}],
    RotationMatrix[Pi / 3, {0, 0, 1}],
    RotationMatrix[-Pi / 4, {0, 1, 1}]};
  pnts[[3]] = ReplacePart[#, 3 -> 0] & /@ pnts[[3]];
  pnts = MapThread[
    Function[{mat, p}, m = Mean[p]; (mat.(# - m) + m) & /@ p], {rmat, pnts}, 1];
  pnts[[2]] = ReplacePart[#, 3 -> 40] & /@ pnts[[2]];

  points3D["4clusters"] = RandomSample[Flatten[pnts, 1]];

  Apply[ And, MatrixQ[#, NumericQ] /@ Values[points3D] ]
  ,
  True
  ,
  TestID -> "Generated-3D-data-1"
];


VerificationTest[(* 4 *)
  SeedRandom[342];

  formulaPoints2D = <||>;

  rdata = Rasterize[x^2 + Sqrt[y] == 1, "Data", ImageSize -> 200];

  rdata = Map[Mean, rdata, {-2}];
  rdataVecs =
      Flatten[Table[{i, j, rdata[[i, j]]}, {i, 1, Length[rdata]}, {j, 1,
        Length[rdata[[1]]]}], 1];
  blackVecs = Select[rdataVecs, #[[3]] < 250 &];

  blackVecs2D = Drop[#, {3}] & /@ blackVecs;
  blackVecs2D = {1, -1} * # & /@ (Reverse /@ blackVecs2D);

  formulaPoints2D["x^2+Sqrt[y]==1"] = blackVecs2D;

  Apply[ And, MatrixQ[#, NumericQ] /@ Values[formulaPoints2D] ]
  ,
  True
  ,
  TestID -> "Generated-formula-data-1"
];


(***********************************************************)
(* Clusters                                                *)
(***********************************************************)

VerificationTest[
  clsRes = KMeans[points2D["3clusters"], 3];

  AssociationQ[clRes] &&
      Sort[Keys[clsRes]] == Sort[{"MeanPoints", "Clusters", "ClusterLabels", "IndexClusters"}] &&
      MatrixQ[ clsRes["MeanPoints"], NumberQ] &&
      Length[clRes["Clusters"]] == 3 &&
      Apply[ And, MatrixQ[#, NumberQ]& /@ clRes["Clusters"] ] &&
      VectorQ[ clRes["ClusterLabels"], IntegerQ] &&
      AssociationQ[ clsRes["IndexClusters"] ] && Apply[And, VectorQ[#, IntegerQ]& /@ Values[clRes["IndexClusters"]]]
  ,
  True
  ,
  TestID -> "Standard-call-2Ddata-1"
];


VerificationTest[
  clsRes = KMeans[points2D["3clusters"], 5, "LearningParameter" -> 0.1, MaxSteps -> 10, "MinReassignmentsFraction" -> 0.2];

  AssociationQ[clRes] &&
      Sort[Keys[clsRes]] == Sort[{"MeanPoints", "Clusters", "ClusterLabels", "IndexClusters"}] &&
      MatrixQ[ clsRes["MeanPoints"], NumberQ] &&
      Length[clRes["Clusters"]] == 5 &&
      Apply[ And, MatrixQ[#, NumberQ]& /@ clRes["Clusters"] ] &&
      VectorQ[ clRes["ClusterLabels"], IntegerQ] &&
      AssociationQ[ clsRes["IndexClusters"] ] && Apply[And, VectorQ[#, IntegerQ]& /@ Values[clRes["IndexClusters"]]]
  ,
  True
  ,
  TestID -> "Standard-call-2Ddata-2"
];


(***********************************************************)
(* Messages for wrong input                                *)
(***********************************************************)

VerificationTest[
  $Failed === KMeans[points2D["3clusters"]];
  ,
  True
  ,
  KMeans::nargs
  ,
  TestID -> "Wrong-signature-call-1"
];


VerificationTest[
  $Failed === KMeans[RandomReal[1,12], 4];
  ,
  True
  ,
  KMeans::nargs
  ,
  TestID -> "Wrong-signature-call-2"
];


VerificationTest[
  $Failed === KMeans[RandomReal[1,{120,3}], -1];
  ,
  True
  ,
  KMeans::nargs
  ,
  TestID -> "Wrong-signature-call-3"
];


VerificationTest[
  $Failed === KMeans[points2D["3clusters"], 3, "LearningParameter" -> -0.1, "MinReassignmentsFraction" -> 0.2]
  ,
  True
  ,
  KMeans::nfrac
  ,
  TestID -> "Wrong-option-value-call-1"
];


VerificationTest[
  $Failed === KMeans[points2D["3clusters"], 3, "LearningParameter" -> 0.1, "MinReassignmentsFraction" -> 2]
  ,
  True
  ,
  KMeans::nfrac
  ,
  TestID -> "Wrong-option-value-call-2"
];


VerificationTest[
  $Failed === KMeans[points2D["3clusters"], 3, "LearningParameter" -> 0.1, "MinReassignmentsFraction" -> 0.2, MaxSteps -> -12 ]
  ,
  True
  ,
  KMeans::npi
  ,
  TestID -> "Wrong-option-value-call-3"
];


EndTestSection[]
