(*
    DataReshape Mathematica unit tests
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

(* :Title: DataReshape-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2020-08-22 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Long format, Long form, Narrow format, Narrow form, Wide format, Wide form, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has unit tests of the functions ToLongForm and ToWideForm implemented in the file:

     https://github.com/antononcube/MathematicaForPrediction/blob/master/DataReshape.m

*)

BeginTestSection["DataReshape-Unit-Tests.wlt"];

(***********************************************************)
(* Load package                                            *)
(***********************************************************)

VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/DataReshape.m"],
    Greater[Length[DownValues[DataReshape`ToLongForm]], 0]
  ]
  ,
  True
  ,
  TestID -> "LoadPackage"
];


(***********************************************************)
(* Generate data                                           *)
(***********************************************************)

VerificationTest[
  SeedRandom[1295];

  dsSmall = Dataset @ {
    <|"a" -> "x", "b" -> 5|>,
    <|"a" -> "y", "b" -> 6|>,
    <|"a" -> "x", "b" -> 10|>,
    <|"a" -> "y", "b" -> 100|>,
    <|"a" -> "z", "b" -> Missing[]|>};

  k = 1;
  dsSmallWithIDs = dsSmall[All, Prepend[#, "ID" -> k++] &];

  SeedRandom[4];
  n = 200;
  dsLarge = Dataset @ Transpose[{RandomInteger[5, n], RandomChoice[RandomWord[5], n], RandomChoice[RandomWord[20], n], RandomReal[{-100, 100}, n]}];

  k = 1;
  dsLargeWithIDs = dsLarge[All, Prepend[#, k++] &];

  aSmall = AssociationThread[Map[#ID &, Normal@dsSmallWithIDs], Map[KeyDrop[#, "ID"] &, Normal@dsSmallWithIDs]];

  aLarge = AssociationThread[Map[First, Normal@dsLargeWithIDs], Map[Rest, Normal@dsLargeWithIDs]];

  Apply[ And, Map[ MatchQ[#, _Dataset]&, {dsSmall, dsSmallWithIDs, dsLarge, dsLargeWithIDs} ] ] &&
      AssociationQ[aSmall] &&
      AssociationQ[aLarge]
  ,
  True
  ,
  TestID -> "Generated-datasets"
];


(***********************************************************)
(* Get built in data                                       *)
(***********************************************************)

VerificationTest[
  dsTitanic = ExampleData[{"Dataset", "Titanic"}];
  MatchQ[dsTitanic, _Dataset] && Dimensions[dsTitanic] == {1309, 4}
  ,
  True
  ,
  TestID -> "Titanic-dataset"
];


(***********************************************************)
(* To long form                                            *)
(***********************************************************)

VerificationTest[
  dsTemp = ToLongForm[dsSmall];
  MatchQ[ dsTemp, _Dataset] &&
      Dimensions[dsTemp] == { 2 * Dimensions[dsSmall][[1]], 3 } &&
      Normal[ Keys[dsTemp[[1]]] ] == { "AutomaticKey", "Variable", "Value" }
  ,
  True
  ,
  TestID -> "ToLongForm-1"
];


VerificationTest[
  dsTemp = ToLongForm[dsSmall, Automatic, {"a", "b"} ];
  dsTemp2 = ToLongForm[dsSmall, "IdentifierColumns" -> Automatic, "VariableColumns" -> {"a", "b"} ];
  MatchQ[ dsTemp2, _Dataset] &&
      Dimensions[dsTemp2] == { 2 * Dimensions[dsSmall][[1]], 3 } &&
      Normal[ Keys[dsTemp2[[1]]] ] == { "AutomaticKey", "Variable", "Value" } &&
      dsTemp == dsTemp2
  ,
  True
  ,
  TestID -> "ToLongForm-2"
];


VerificationTest[
  dsTemp = ToLongForm[dsSmallWithIDs, "ID", {"a", "b"}];
  MatchQ[ dsTemp, _Dataset] &&
      Dimensions[dsTemp] == { 2 * Dimensions[dsSmallWithIDs][[1]], 3 } &&
      Normal[ Keys[dsTemp[[1]]] ] == { "ID", "Variable", "Value" }
  ,
  True
  ,
  TestID -> "ToLongForm-3"
];


VerificationTest[
  dsTemp = ToLongForm[dsSmallWithIDs, "ID", {"a", "b"}];
  dsTemp2 = ToLongForm[dsSmallWithIDs, "IdentifierColumns" -> "ID", "VariableColumns" -> {"a", "b"} ];
  MatchQ[ dsTemp, _Dataset] &&
      Dimensions[dsTemp] == { 2 * Dimensions[dsSmallWithIDs][[1]], 3 } &&
      Normal[ Keys[dsTemp[[1]]] ] == { "ID", "Variable", "Value" } &&
      dsTemp == dsTemp2
  ,
  True
  ,
  TestID -> "ToLongForm-4"
];


VerificationTest[
  dsTemp1 = ToLongForm[dsSmall];
  dsTemp2 = ToLongForm[dsSmall, Automatic, Automatic];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "ToLongForm-Automatic-Equivalence-1"
];


VerificationTest[
  vInds = Range[ 2, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = ToLongForm[ dsSmallWithIDs, 1, vInds ];
  dsTemp2 = ToLongForm[ dsSmallWithIDs, Automatic, vInds];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "ToLongForm-Automatic-Equivalence-2"
];


VerificationTest[
  vInds = Range[ 1, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = ToLongForm[ dsSmallWithIDs, 0, vInds ];
  dsTemp2 = ToLongForm[ dsSmallWithIDs, Automatic, vInds];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "ToLongForm-Automatic-Equivalence-3"
];


VerificationTest[
  vInds = Range[ 2, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = ToLongForm[ dsSmallWithIDs, 1, vInds ];
  dsTemp2 = ToLongForm[ dsSmallWithIDs, 1, Automatic ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "ToLongForm-Automatic-Equivalence-4"
];


VerificationTest[
  vInds = Range[ 1, Dimensions[dsSmallWithIDs][[2]] ];
  dsTemp1 = ToLongForm[ dsSmallWithIDs, 0, vInds ];
  dsTemp2 = ToLongForm[ dsSmallWithIDs, 0, Automatic ];
  dsTemp1 == dsTemp2
  ,
  True
  ,
  TestID -> "ToLongForm-Automatic-Equivalence-5"
];


(***********************************************************)
(* To long form failure                                    *)
(***********************************************************)

VerificationTest[
  ToLongForm[Normal @ dsSmallWithIDs, "ID", {"a", "b"}]
  ,
  $Failed
  ,
  {ToLongForm::args}
  ,
  TestID -> "ToLongForm-fail-1"
];


VerificationTest[
  ToLongForm[ dsSmallWithIDs, "blah", {"a", "b"}]
  ,
  $Failed
  ,
  {ToLongForm::colkeys}
  ,
  TestID -> "ToLongForm-fail-2"
];


VerificationTest[
  ToLongForm[ dsSmallWithIDs, "ID", {"blah", "b"}]
  ,
  $Failed
  ,
  {ToLongForm::colkeys}
  ,
  TestID -> "ToLongForm-fail-3"
];


VerificationTest[
  ToLongForm[ dsLargeWithIDs, "ID", {"a", "b"}]
  ,
  $Failed
  ,
  {ToLongForm::nocolkeys}
  ,
  TestID -> "ToLongForm-fail-4"
];


(***********************************************************)
(* To wide form                                            *)
(***********************************************************)

VerificationTest[
  MatchQ[ ToWideForm[ToLongForm[dsSmall], "AutomaticKey", "Variable", "Value"], _Dataset ]
  ,
  True
  ,
  TestID -> "ToWideForm-1"
];


EndTestSection[]

