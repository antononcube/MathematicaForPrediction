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
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicSparseMatrixRecommender-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2018-09-15 *)

(* :Package Version: 0.4 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: monad, monadic, sparse matrix, recommender, workflow, State monad, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the package

     https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicSparseMatrixRecommender.m

*)
BeginTestSection["MonadicSparseMatrixRecommender-Unit-Tests.mt"];


VerificationTest[(* 1 *)
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicSparseMatrixRecommender.m"];
  Greater[Length[SubValues[MonadicSparseMatrixRecommender`SMRMonCreate]], 0]
  ,
  True
  ,
  TestID -> "LoadPackage"
];


VerificationTest[(* 2 *)
  SeedRandom[342];
  data = RandomInteger[{0, 20}, {100, 5}];
  ds = Dataset[MapThread[Prepend, {data, Range[Length[data]]}]];
  ds = ds[All, AssociationThread[Prepend[ToString /@ Range[Length[data[[1]]]], "id"], #] &];
  MatchQ[ds, _Dataset] && Count[ds, 0, Infinity] > 10
  ,
  True
  ,
  TestID -> "GenerateData"
];


VerificationTest[(* 2 *)
  dsMiss = ds /. {0 -> Missing[]};
  MatchQ[dsMiss, _Dataset] && Count[dsMiss, _Missing, Infinity] > 10
  ,
  True
  ,
  TestID -> "MakeDataWithMissing"
];


VerificationTest[(* 4 *)
  lsTitanic = Import["https://raw.githubusercontent.com/antononcube/MathematicaVsR/master/Data/MathematicaVsR-Data-Titanic.csv"];
  dsTitanic = Dataset[Dataset[Rest[lsTitanic]][All, AssociationThread[First[lsTitanic], #] &]];
  Head[dsTitanic] === Dataset && Normal[Keys[dsTitanic[[1]]]] == {"id", "passengerClass", "passengerAge", "passengerSex", "passengerSurvival"}
  ,
  True
  ,
  TestID -> "Load-Titanic-data"
];


VerificationTest[(* 5 *)
  lsMushroom = Import["https://raw.githubusercontent.com/antononcube/MathematicaVsR/master/Data/MathematicaVsR-Data-Mushroom.csv"];
  dsMushroom = Dataset[Dataset[Rest[lsMushroom]][All, AssociationThread[First[lsMushroom], #] &]];
  expectedColumnNames = Sort @ {"id", "cap-Shape", "cap-Surface", "cap-Color", "bruises?", "odor",
    "gill-Attachment", "gill-Spacing", "gill-Size", "gill-Color",
    "stalk-Shape", "stalk-Root", "stalk-Surface-Above-Ring",
    "stalk-Surface-Below-Ring", "stalk-Color-Above-Ring",
    "stalk-Color-Below-Ring", "veil-Type", "veil-Color", "ring-Number",
    "ring-Type", "spore-Print-Color", "population", "habitat",
    "edibility"};
  Head[dsMushroom] === Dataset && Sort[Normal[Keys[dsMushroom[[1]]]]] == expectedColumnNames
  ,
  True
  ,
  TestID -> "Load-Mushroom-data"
];


VerificationTest[(* 5 *)
  lsMint = Import["https://raw.githubusercontent.com/antononcube/R-packages/master/DataObfuscation/inst/extdata/dfMintBubblesTransactions.csv"];
  dfMintBubblesTransactions = Dataset[Dataset[Rest[lsMint]][All, AssociationThread[First[lsMint], #] &]];
  Block[{k = 1},
    dfMintBubblesTransactions = dfMintBubblesTransactions[All, Prepend[#, "ID" -> "id." <> ToString[k++]] &];
  ];
  expectedColumnNames = Sort @ {"ID", "Date", "Description", "Amount", "Transaction.Type", "Category", "Account.Name"};
  Head[dfMintBubblesTransactions] === Dataset && Sort[Normal[Keys[dfMintBubblesTransactions[[1]]]]] == expectedColumnNames
  ,
  True
  ,
  TestID -> "Load-MintBubblesTransactions"
];


(*---------------------------------------------------------*)
(* Creations                                               *)
(*---------------------------------------------------------*)

VerificationTest[(* 6 *)
  smrTitanic = SMRMonBind[ SMRMonUnit[], SMRMonCreate[dsTitanic, "id"] ];

  TrueQ[Head[smrTitanic] === SMRMon] &&
      AssociationQ[smrTitanic[[2]]] &&
      Keys[smrTitanic[[2]]] == {"data", "itemNames", "tags", "tagTypeWeights", "matrices", "M01", "M"}
  ,
  True
  ,
  TestID -> "SMR-creation-with-Titanic-data"
];


VerificationTest[(* 7 *)
  colNames = Normal[Keys[dsTitanic[[1]]]];

  smats = Association @
      Map[# -> ToSSparseMatrix[CrossTabulate[dsTitanic[All, {First[colNames], #}]]] &, Rest[colNames] ];

  smrTitanic2 = SMRMonBind[ SMRMonUnit[], SMRMonCreate[smats] ];

  TrueQ[Head[smrTitanic2] === SMRMon] &&
      AssociationQ[smrTitanic2[[2]]] &&
      Keys[smrTitanic2[[2]]] == {"itemNames", "tags", "tagTypeWeights", "matrices", "M01", "M"}
  ,
  True
  ,
  TestID -> "SMR-creation-with-Titanic-matrices"
];


VerificationTest[(* 8 *)
  smrMushroom = SMRMonBind[ SMRMonUnit[], SMRMonCreate[dsMushroom, "id", "AddTagTypesToColumnNames" -> True, "TagValueSeparator" -> ":" ]];

  TrueQ[Head[smrMushroom] === SMRMon] &&
      AssociationQ[smrMushroom[[2]]] &&
      Keys[smrMushroom[[2]]] == {"data", "itemNames", "tags", "tagTypeWeights", "matrices", "M01", "M"}
  ,
  True
  ,
  TestID -> "SMR-creation-with-Mushroom-data"
];


VerificationTest[(* 9 *)
  colNames = Normal[Keys[dsMushroom[[1]]]];

  smats = Association @
      Map[# -> ToSSparseMatrix[CrossTabulate[dsMushroom[All, {First[colNames], #}]]] &, Rest[colNames]];

  smrMushroom2 = SMRMonBind[ SMRMonUnit[], SMRMonCreate[smats] ];

  TrueQ[Head[smrMushroom2] === SMRMon] &&
      AssociationQ[smrMushroom2[[2]]] &&
      Keys[smrMushroom2[[2]]] == {"itemNames", "tags", "tagTypeWeights", "matrices", "M01", "M"}
  ,
  True
  ,
  TestID -> "SMR-creation-with-Mushroom-matrices"
];


(*---------------------------------------------------------*)
(* Recommendations by history                              *)
(*---------------------------------------------------------*)

VerificationTest[(* 13 *)
  recs2 =
      Fold[
        SMRMonBind,
        smrTitanic2,
        {
          SMRMonRecommend[<|"10" -> 1, "120" -> 0.5|>, 12],
          SMRMonTakeValue
        }
      ];
  VectorQ[Keys[recs2], StringQ] && VectorQ[Values[recs2], NumberQ]
  ,
  True
  ,
  TestID -> "smrTitanic2-recommendations-1"
];


VerificationTest[(* 14 *)
  recs3 =
      Fold[
        SMRMonBind,
        smrTitanic2,
        {
          SMRMonRecommend[{"10", "120"}, 12],
          SMRMonTakeValue
        }
      ];
  VectorQ[Keys[recs3], StringQ] && VectorQ[Values[recs3], NumberQ]
  ,
  True
  ,
  TestID -> "smrTitanic2-recommendations-2"
];


VerificationTest[(* 15 *)
  recs4 =
      Fold[
        SMRMonBind,
        smrMushroom2,
        {
          SMRMonRecommend[<|"1" -> 1, "12" -> 0.5|>, 12],
          SMRMonTakeValue
        }
      ];
  VectorQ[Keys[recs4], StringQ] && VectorQ[Values[recs4], NumberQ]
  ,
  True
  ,
  TestID -> "smrMushroom2-recommendations-1"
];


(*---------------------------------------------------------*)
(* Recommendations by profile                              *)
(*---------------------------------------------------------*)

VerificationTest[(* 17 *)
  precs1 =
      Fold[
        SMRMonBind,
        smrTitanic,
        {
          SMRMonRecommendByProfile[<|"male" -> 1, "died" -> 1|>, 12],
          SMRMonTakeValue
        }
      ];

  Union[Flatten[Normal[dsTitanic[Select[MemberQ[Keys[precs1], ToString@#["id"]] &], {"passengerSex"}][Values]]]] == {"male"} &&
      Union[Flatten[Normal[dsTitanic[Select[MemberQ[Keys[precs1], ToString@#["id"]] &], {"passengerSurvival"}][Values]]]] == {"died"}
  ,
  True
  ,
  TestID -> "smrTitanic-recommendations-by-profile-1"
];


VerificationTest[(* 18 *)
  precs2 = Fold[ SMRMonBind, smrMushroom, {SMRMonRecommendByProfile[<|"odor:pungent" -> 1, "edibility:poisonous" -> 1|>, 12], SMRMonTakeValue} ];

  Union[Flatten[Normal[dsMushroom[Select[MemberQ[Keys[precs2], ToString@#["id"]] &], {"odor"}][Values]]]] == {"pungent"} &&
      Union[Flatten[Normal[dsMushroom[Select[MemberQ[Keys[precs2], ToString@#["id"]] &], {"edibility"}][Values]]]] == {"poisonous"}
  ,
  True
  ,
  TestID -> "smrMushroom-recommendations-by-profile-1"
];


VerificationTest[(* 19 *)
  precs2 =
      Fold[
        SMRMonBind,
        smrMushroom,
        {
          SMRMonRecommendByProfile[ {"odor:pungent", "edibility:poisonous"}, 12],
          SMRMonTakeValue
        }
      ];

  Union[Flatten[Normal[dsMushroom[Select[MemberQ[Keys[precs2], ToString@#["id"]] &], {"odor"}][Values]]]] == {"pungent"} &&
      Union[Flatten[Normal[dsMushroom[Select[MemberQ[Keys[precs2], ToString@#["id"]] &], {"edibility"}][Values]]]] == {"poisonous"}
  ,
  True
  ,
  TestID -> "smrMushroom-recommendations-by-profile-2"
];


(*---------------------------------------------------------*)
(* Application of term weights                             *)
(*---------------------------------------------------------*)

VerificationTest[(* 20 *)
  recs2 = Fold[
    SMRMonBind,
    smrTitanic2,
    {
      SMRMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
      SMRMonRecommend[<|"10" -> 1, "120" -> 0.5|>, 12],
      SMRMonTakeValue
    }
  ];
  VectorQ[Keys[recs2], StringQ] && VectorQ[Values[recs2], NumberQ]
  ,
  True
  ,
  TestID -> "smrTitanic2-apply-term-weights-1"
];


VerificationTest[(* 20 *)
  recs2 = Fold[
    SMRMonBind,
    smrTitanic2,
    {
      SMRMonApplyTermWeightFunctions[ "GlobalWeightFunction" -> "IDF", "LocalWeightFunction" -> "None", "NormalizerFunction" -> "Cosine"],
      SMRMonRecommend[<|"10" -> 1, "120" -> 0.5|>, 12],
      SMRMonTakeValue
    }
  ];
  VectorQ[Keys[recs2], StringQ] && VectorQ[Values[recs2], NumberQ]
  ,
  True
  ,
  TestID -> "smrTitanic2-apply-term-weights-2"
];


VerificationTest[(* 21 *)
  recs2 = Fold[
    SMRMonBind,
    smrTitanic2,
    {
      SMRMonApplyTermWeightFunctions,
      SMRMonRecommend[<|"10" -> 1, "120" -> 0.5|>, 12],
      SMRMonTakeValue
    }
  ];
  VectorQ[Keys[recs2], StringQ] && VectorQ[Values[recs2], NumberQ]
  ,
  True
  ,
  TestID -> "smrTitanic2-apply-term-weights-3"
];


(*---------------------------------------------------------*)
(* Proofs                                                  *)
(*---------------------------------------------------------*)

VerificationTest[(* 22 *)
  proofs1 = Fold[
    SMRMonBind,
    smrTitanic2,
    {
      SMRMonProveByMetadata[ <|"male" -> 1, "female" -> 1, "died" -> 1 |>, "10" ],
      SMRMonTakeValue
    }
  ];
  AssociationQ[proofs1] && Length[proofs1] == 1
  ,
  True
  ,
  TestID -> "smrTitanic2-metadata-proofs-1"
];


VerificationTest[(* 23 *)
  itemNames = { "10", "120", "320" };
  proofs2 = Fold[
    SMRMonBind,
    smrTitanic2,
    {
      SMRMonProveByMetadata[ <|"male" -> 1, "female" -> 1, "died" -> 1 |>, itemNames, "Normalize" -> True ],
      SMRMonTakeValue
    }
  ];
  AssociationQ[proofs2] && Length[proofs2] == Length[itemNames]
  ,
  True
  ,
  TestID -> "smrTitanic2-metadata-proofs-2"
];


VerificationTest[(* 24 *)
  proofs3 = Fold[
    SMRMonBind,
    smrTitanic2,
    {
      SMRMonProveByHistory[ <|"10" -> 1, "12" -> 1, "13" -> 1 |>, "120" ],
      SMRMonTakeValue
    }
  ];
  AssociationQ[proofs3] && Length[proofs3] == 1
  ,
  True
  ,
  TestID -> "smrTitanic2-history-proofs-1"
];


VerificationTest[(* 25 *)
  itemNames = { "120", "220", "320" };
  proofs4 = Fold[
    SMRMonBind,
    smrTitanic2,
    {
      SMRMonProveByHistory[ <|"10" -> 1, "12" -> 1, "13" -> 1 |>, itemNames, "Normalize" -> True ],
      SMRMonTakeValue
    }
  ];
  AssociationQ[proofs4] && Length[proofs4] == Length[itemNames]
  ,
  True
  ,
  TestID -> "smrTitanic2-history-proofs-2"
];


(*---------------------------------------------------------*)
(* Get top recommendations                                 *)
(*---------------------------------------------------------*)

VerificationTest[(* 25 *)
  recs1 =
      Fold[
        SMRMonBind,
        smrMushroom2,
        {
          SMRMonGetTopRecommendations[<|"1" -> 1, "12" -> 0.5|>, 12],
          SMRMonTakeValue
        }
      ];

  recs2 =
      Fold[
        SMRMonBind,
        smrMushroom2,
        {
          SMRMonRecommend[<|"1" -> 1, "12" -> 0.5|>, 12],
          SMRMonTakeValue
        }
      ];

  recs1 == recs2
  ,
  True
  ,
  TestID -> "smrMushroom-get-top-recommendations-1"
];


VerificationTest[(* 26 *)
  precs1 =
      Fold[
        SMRMonBind,
        smrMushroom,
        {
          SMRMonGetTopRecommendations[ {"odor:pungent", "edibility:poisonous"}, 12],
          SMRMonTakeValue
        }
      ];

  precs2 =
      Fold[
        SMRMonBind,
        smrMushroom,
        {
          SMRMonRecommendByProfile[ {"odor:pungent", "edibility:poisonous"}, 12],
          SMRMonTakeValue
        }
      ];

  precs1 == precs2
  ,
  True
  ,
  TestID -> "smrMushroom-get-top-recommendations-2"
];


VerificationTest[(* 27 *)
  Fold[
    SMRMonBind,
    smrMushroom2,
    {
      SMRMonGetTopRecommendations[<|"blah1" -> 1, "blah12" -> 0.5|>, 12],
      SMRMonTakeValue
    }
  ]
  ,
  $SMRMonFailure
  ,
  TestID -> "smrMushroom-get-top-recommendations-3"
];


VerificationTest[(* 28 *)
  res1 =
      Keys @
          Fold[
            SMRMonBind,
            smrTitanic,
            {
              SMRMonFilterByProfile[{"male", "1st"}, "Type" -> "Intersection"],
              SMRMonTakeValue
            }
          ];
  res2 = Normal @ dsTitanic[ Select[#passengerSex == "male" && #passengerClass == "1st" &] ][All, "id"];
  Sort[res1] == Sort[ToString /@ res2]
  ,
  True
  ,
  TestID -> "smrTitanic-filter-by-profile-1"
];


VerificationTest[(* 29 *)
  res1 =
      Keys @
          Fold[
            SMRMonBind,
            smrTitanic,
            {
              SMRMonFilterByProfile[{"male", "1st"}, "Type" -> "Union"],
              SMRMonTakeValue
            }
          ];
  res2 = Normal @ dsTitanic[ Select[#passengerSex == "male" || #passengerClass == "1st" &] ][All, "id"];
  Sort[res1] == Sort[ToString /@ res2]
  ,
  True
  ,
  TestID -> "smrTitanic-filter-by-profile-2"
];


VerificationTest[(* 29 *)
  smrObj = SMRMonBind[ smrTitanic, SMRMonFilterMatrix[{"male", "1st"}, "Type" -> "Intersection"] ];
  res2 = Normal @ dsTitanic[ Select[#passengerSex == "male" && #passengerClass == "1st" &] ][All, "id"];
  res2 = Sort[ToString /@ res2];
  Sort[ RowNames @ SMRMonBind[ smrObj, SMRMonTakeM] ] == res2 &&
      Apply[And, Map[ Sort[RowNames[#]] == res2 &, SMRMonBind[ smrObj, SMRMonTakeMatrices ] ] ]
  ,
  True
  ,
  TestID -> "smrTitanic-filter-matrix-1"
];


(*---------------------------------------------------------*)
(* Recommender algebra                                     *)
(*---------------------------------------------------------*)

VerificationTest[
  smrObj1 =
      Fold[ SMRMonBind,
        SMRMonUnit[],
        {
          SMRMonCreate[dfMintBubblesTransactions[200 ;; -1, {1, 2, 3, 4}]]
        }
      ];

  smrObj2 =
      Fold[ SMRMonBind,
        SMRMonUnit[],
        {
          SMRMonCreate[dfMintBubblesTransactions[1 ;; 300 , {1, 5, 6, 7}]]
        }
      ];

  Head[smrObj1] === SMRMon &&
      AssociationQ[ smrObj1[[2]]["matrices"] ] &&
      Length[smrObj1[[2]]["matrices"]] == 3 &&
      Head[smrObj2] === SMRMon &&
      AssociationQ[ smrObj2[[2]]["matrices"] ] &&
      Length[smrObj2[[2]]["matrices"]] == 3
  ,
  True
  ,
  TestID -> "Recommender-algebra-recommenders-1"
];


VerificationTest[
  smrObj3 =
      Fold[SMRMonBind,
        smrObj1,
        {
          SMRMonJoin[smrObj2, "outer"]}
      ];


  Head[smrObj3] === SMRMon &&
      AssociationQ[ smrObj3[[2]]["matrices"] ] &&
      Length[smrObj3[[2]]["matrices"]] == 6 &&
      Sort @ Keys @ SMRMonBind[smrObj3, SMRMonTakeMatrices] == Sort @ Complement[ Normal @ Keys @ dfMintBubblesTransactions[1], {"ID"}] &&
      Apply[Equal, Prepend[ RowsCount /@ Values[SMRMonBind[smrObj3, SMRMonTakeMatrices]], Length @ Union @ Normal @ dfMintBubblesTransactions[All, "ID"] ] ] &&
      Apply[Equal, Prepend[ RowNames /@ Values[SMRMonBind[smrObj3, SMRMonTakeMatrices]], Union @ Normal @ dfMintBubblesTransactions[All, "ID"] ] ]
  ,
  True
  ,
  TestID -> "Recommender-algebra-join-outer-1"
];


VerificationTest[
  smrObj3 =
      Fold[SMRMonBind,
        smrObj1,
        {
          SMRMonJoin[smrObj2, "left"]}
      ];


  Head[smrObj3] === SMRMon &&
      AssociationQ[ smrObj3[[2]]["matrices"] ] &&
      Length[smrObj3[[2]]["matrices"]] == 6 &&
      Sort @ Keys @ SMRMonBind[smrObj3, SMRMonTakeMatrices] == Sort @ Complement[ Normal @ Keys @ dfMintBubblesTransactions[1], {"ID"}] &&
      Apply[Equal, Join[ RowsCount /@ Values[SMRMonBind[smrObj3, SMRMonTakeMatrices]], RowsCount /@ Values[SMRMonBind[smrObj1, SMRMonTakeMatrices]] ] ] &&
      Apply[Equal, Join[ RowNames /@ Values[SMRMonBind[smrObj3, SMRMonTakeMatrices]], RowNames /@ Values[SMRMonBind[smrObj1, SMRMonTakeMatrices]] ] ]
  ,
  True
  ,
  TestID -> "Recommender-algebra-join-left-1"
];


VerificationTest[
  smrObj3 = SMRMonBind[smrObj1, SMRMonJoin[smrObj2, "inner"]];

  Head[smrObj3] === SMRMon &&
      AssociationQ[ smrObj3[[2]]["matrices"] ] &&
      Length[smrObj3[[2]]["matrices"]] == 6 &&
      Sort @ Keys @ SMRMonBind[smrObj3, SMRMonTakeMatrices] == Sort @ Complement[ Normal @ Keys @ dfMintBubblesTransactions[1], {"ID"}] &&
      Apply[Equal,
        Prepend[
          RowNames /@ Values[SMRMonBind[smrObj3, SMRMonTakeMatrices]],
          Sort @
              Intersection[
                RowNames @ First @ SMRMonBind[smrObj1, SMRMonTakeMatrices],
                RowNames @ First @ SMRMonBind[smrObj2, SMRMonTakeMatrices]
              ]
        ]
      ]
  ,
  True
  ,
  TestID -> "Recommender-algebra-join-inner-1"
];


VerificationTest[
  smrObj4 =
      SMRMonBind[
        smrObj1,
        SMRMonAnnexSubMatrices[<| "Account.Name" -> SMRMonBind[smrObj2, SMRMonTakeContext]["matrices"][[1]] |>]
      ];


  Head[smrObj4] === SMRMon &&
      AssociationQ[ smrObj4[[2]]["matrices"] ] &&
      Length[smrObj4[[2]]["matrices"]] == 4 &&
      Keys[SMRMonBind[smrObj4, SMRMonTakeMatrices]] == Append[ Keys @ SMRMonBind[smrObj1, SMRMonTakeMatrices], "Account.Name"]
  ,
  True
  ,
  TestID -> "Recommender-algebra-annex-sub-matrix-1"
];


VerificationTest[
  smrObj5 = SMRMonBind[smrObj1, SMRMonAnnexSubMatrices[SMRMonBind[smrObj2, SMRMonTakeContext]["matrices"][[1]], "Account.Name"]];

  Head[smrObj5] === SMRMon &&
      AssociationQ[ smrObj5[[2]]["matrices"] ] &&
      Length[smrObj5[[2]]["matrices"]] == 4 &&
      Keys[SMRMonBind[smrObj5, SMRMonTakeMatrices]] == Append[ Keys @ SMRMonBind[smrObj1, SMRMonTakeMatrices], "Account.Name"]
  ,
  True
  ,
  TestID -> "Recommender-algebra-annex-sub-matrix-2"
];


VerificationTest[
  SMRMonBind[smrObj4, SMRMonTakeMatrices] == SMRMonBind[smrObj5, SMRMonTakeMatrices]
  ,
  True
  ,
  TestID -> "Recommender-algebra-annex-sub-matrix-3"
];


EndTestSection[]
