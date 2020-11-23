(*
    Random Tabular Dataset Mathematica Package
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
    antononcube@posteo.com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: RandomTabularDataset *)
(* :Context: RandomTabularDataset` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-11-23 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: dataset, random, tabular, long form, wide form *)
(* :Discussion:

  # Usage
  RandomTabularDataset[]
    generates a random tabular dataset.

  RandomTabularDataset[m]
    generates a random tabular dataset with m rows.

  RandomTabularDataset[{m, n}]
    generates a random tabular dataset with m rows and n columns.

  # Details & options

  RandomTabularDataset primary purpose a dataset that has random number of rows, number of columns, and values.

  RandomTabularDataset takes the following options:

    "ColumnNameGenerator"	Automatic	generator of column names
    "ColumnValueGenerators"	Automatic	generators for the values in each column
    "Form"	"Wide"	the form of the generated dataset (long or wide)
    "NumberOfValues"	Automatic	number of non-missing values
    "RowKeys"	False	should the rows have keys or not


   # Basic Examples

   Generate a random tabular dataset:

   SeedRandom[2];
   RandomTabularDataset[]


   Generate a random tabular dataset with specified number of rows:

   SeedRandom[2];
   RandomTabularDataset[4]

   Generate a random tabular dataset with specified random value generators for certain columns:

   SeedRandom[4];
   RandomTabularDataset[{5, Automatic}, "ColumnValueGenerators" -> <|1 -> (RandomVariate[NormalDistribution[100, 3]] &), 3 -> (RandomColor[] &)|>]

   # Scope

   The generated dataset can be produced in long form or wide form and can have row keys. Here is wide form dataset with row keys:

   SeedRandom[32];
   RandomTabularDataset[{4, 3}, "Form" -> "Wide", "RowKeys" -> True]

   Here is the corresponding long form with row keys:

   SeedRandom[32];
   RandomTabularDataset[{4, 3}, "Form" -> "Long", "RowKeys" -> True]

   Using Identity or symbols without down values to specify the column name generation or column value generation gives insight of how the random generator functions are called. Here is example with Identity utilization:

   RandomTabularDataset[{4, 5}, "ColumnNameGenerator" -> ToString@*Identity, "ColumnValueGenerators" -> Identity, "RowKeys" -> True]

   Here is an example with symbols without down values:

   Clear[F, G];
   RandomTabularDataset[{4, 5}, "ColumnNameGenerator" -> ToString@*F@*Identity, "ColumnValueGenerators" -> Table[G, {5}], "RowKeys" -> True]

*)

BeginPackage["RandomTabularDataset`"];

RandomTabularDataset::usage = "Generates a random tabular dataset";

Begin["`Private`"];

Clear[RandomTabularDataset];

SyntaxInformation[
  RandomTabularDataset] = {"ArgumentsPattern" -> {_., OptionsPattern[]}};

Options[RandomTabularDataset] := {"ColumnNameGenerator" -> Automatic,
  "ColumnValueGenerators" -> Automatic, "Form" -> "Wide",
  "NumberOfValues" -> Automatic, "RowKeys" -> False};

RandomTabularDataset::args =
    "One argument is expected that specifies the number of rows and number of columns.";

RandomTabularDataset::nform =
    "The value of the option \"Form\" is expected to be one of \"Long\", \"Wide\", or Automatic.";

RandomTabularDataset::nnov =
    "The value of the option \"NumberOfValues\" is expected to be a positive integer or Automatic.";

RandomTabularDataset[opts : OptionsPattern[]] :=
    RandomTabularDataset[{Automatic, Automatic}, opts];

RandomTabularDataset[nrows_ : (_?IntegerQ | Automatic), opts : OptionsPattern[]] :=
    RandomTabularDataset[{nrows, Automatic}, opts];

RandomTabularDataset[{Automatic, ncols_}, opts : OptionsPattern[]] :=
    RandomTabularDataset[{Automatic, Automatic}, opts];

RandomTabularDataset[{Automatic, ncols_}, opts : OptionsPattern[]] :=
    Block[{nrows = RandomVariate[PoissonDistribution[20]]},
      If[nrows < 1, nrows = 1];
      RandomTabularDataset[{nrows, ncols}, opts]
    ];

RandomTabularDataset[{nrows_, Automatic}, opts : OptionsPattern[]] :=
    Block[{ncols = RandomVariate[PoissonDistribution[7]]},
      If[ncols < 1, ncols = 1];
      RandomTabularDataset[{nrows, ncols}, opts]
    ];

RandomTabularDataset[{nrows_Integer, ncols_Integer}, opts : OptionsPattern[]] :=
    Block[{colNameGen, aColValGens, aAutomaticColValGens, rowKeysQ,
      numberOfValues, form, lsColNames, lsPairs, tbl, res, aMissing},

      colNameGen = OptionValue[RandomTabularDataset, "ColumnNameGenerator"];
      If[TrueQ[colNameGen === Automatic] || TrueQ[colNameGen === RandomWord],
        colNameGen = First@RandomWord["CommonWords", 1] &
      ];
      If[TrueQ[colNameGen === None], colNameGen = Identity];

      aColValGens = OptionValue[RandomTabularDataset, "ColumnValueGenerators"];
      aAutomaticColValGens =
          AssociationThread[
            Range[ncols],
            RandomChoice[{RandomReal[{-10, 10}] &, RandomInteger[{-100, 100}] &, First@RandomWord[1] &}, ncols]
          ];

      If[TrueQ[aColValGens === Automatic] || TrueQ[aColValGens === RandomChoice], aColValGens = <||>];
      If[TrueQ[aColValGens === Identity] || TrueQ[aColValGens === None],
        aColValGens = AssociationThread[Range[ncols] -> Table[Identity, ncols]]
      ];
      If[ListQ[aColValGens],
        aColValGens = Join[ aColValGens, Flatten @ Table[ aColValGens, Ceiling[ncols / Length[aColValGens]] ] ];
        aColValGens = Take[ aColValGens, ncols];
        aColValGens = AssociationThread[Range@ncols, aColValGens]
      ];
      aColValGens = Join[aAutomaticColValGens, aColValGens];

      form = OptionValue[RandomTabularDataset, "Form"];
      If[TrueQ[form === Automatic], form = "Wide"];
      If[! MemberQ[ToLowerCase@{"Long", "Wide"}, ToLowerCase[form]],
        Message[RandomTabularDataset::nform];
        form = "Wide";
      ];

      numberOfValues = OptionValue[RandomTabularDataset, "NumberOfValues"];
      If[TrueQ[numberOfValues === Automatic] || TrueQ[numberOfValues === All],
        numberOfValues = nrows * ncols
      ];
      If[! (IntegerQ[numberOfValues] && numberOfValues > 0),
        Message[RandomTabularDataset::nnov];
        Return[$Failed];
      ];

      rowKeysQ = OptionValue[RandomTabularDataset, "RowKeys"];
      If[TrueQ[rowKeysQ === Automatic], rowKeysQ = False];
      If[TrueQ[rowKeysQ === RandomChoice], rowKeysQ = RandomChoice[{False, True}]];
      rowKeysQ = TrueQ[rowKeysQ];

      lsColNames = Table[colNameGen[i], {i, ncols}];
      lsPairs = Flatten[Table[{i, j}, {i, nrows}, {j, ncols}], 1];
      If[numberOfValues < Length[lsPairs],
        lsPairs = Sort@RandomSample[lsPairs, numberOfValues]
      ];

      tbl = MapThread[{#1, lsColNames[[#2]], aColValGens[#2][{#1, #2}]} &, Transpose[lsPairs]];

      Which[
        ! rowKeysQ && ToLowerCase[form] == "long",
        res =
            Dataset[tbl][All, AssociationThread[{"ID", "Variable", "Value"}, #] &],

        rowKeysQ && ToLowerCase[form] == "long",
        res =
            Dataset@Association@MapIndexed[#2[[1]] -> AssociationThread[{"ID", "Variable", "Value"}, #1] &, tbl],

        ToLowerCase[form] == "wide",
        aMissing = AssociationThread[lsColNames, Missing[]];
        res =
            GroupBy[tbl,
              First,
              Join[aMissing, AssociationThread[#[[All, 2]], #[[All, 3]]]] &];
        res = If[rowKeysQ, Dataset[res], Dataset[Values@res]]

      ];

      res
    ] /; nrows > 0 && ncols > 0;

RandomTabularDataset[___] :=
    Block[{},
      Message[RandomTabularDataset::args];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]