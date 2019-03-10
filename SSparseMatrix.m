(*
    SSparseMatrix Mathematica package
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
    antononcube @ gmail.com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: SSparseMatrix *)
(* :Author: Anton Antonov *)
(* :Date: 2018-03-30 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 11.2 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: S, R, sparse array, sparse matrix, named rows, named columns *)
(* :Discussion:

# In brief

This package has the function implementations for manipulating objects with head SSparseMatrix that behave like
SparseArray objects but have the added functionalities to use row names and column names in a manner similar to
that of the sparse matrix objects from the base library Matrix [2] for the programming language R [1].
(Similar to regular matrices in S and R.)

The idea is fairly simple: we can use associations or replacement rules to map row names and column names into integers.
Similarly to how it is done in S and R, SSparseMatrix handles only strings as row names and column names.

Here are the overloaded core WL functions:

    ArrayRules, Dimensions, Dot, MatrixForm, MatrixPlot, SparseArray, Plus, Times, Total

Note that assignment (with Set[__]) is not implemented.

See the commented out delegation to SparseArray implementation at the end of the file.


# The previous version, RSparseMatrix.m

The first version of this package was made in 2015 with the name RSparseMatrix.m, [4].

The reason for renaming RSparseMatrix into SSparseMatrix is becaus of the naming convention of the
RLink functions. (E.g. RList, REvaluate, etc.)

Since the language S precedes R and "S" stands for "Statistics" and S has matrices with named rows and columns,
the "SSparseMatrix" was chosen.

"SSparseMatrix" should mean "statistical sparse matrix" or "S inspired sparse matrix".


# Unit tests

In order to facilitate further package development (and demonstrate what the package functions do)
the unit test file SSparseMatrix-tests.wlt was made, [3].


# Usage examples

      rmat = MakeSSparseMatrix[
             {{1, 1} -> 1, {2, 2} -> 2, {4, 3} -> 3, {1, 4} -> 4, {3, 5} -> 2},
             "ColumnNames" -> {"a", "b", "c", "d", "e"},
             "RowNames" -> {"A", "B", "C", "D"},
             "DimensionNames" -> {"U", "V"}]

      rmat // MatrixForm

      rmat // MatrixPlot

      RowNames[rmat]
      (* {"A", "B", "C", "D"} *)

      ColumnNames[rmat]
      (* {"a", "b", "c", "d", "e"} *)

      DimensionNames[rmat]
      (* {"U", "V"} *)

      rmat.Transpose[rmat[[{1}, All]]]

      rmat[[{"C", "D", "A", "B"}, {"c", "d", "e", "a", "b"}]]

      rmat2 = ToSSparseMatrix[rmat, "RowNames" -> Map["s." <> # &, RowNames[rmat]]];

      RowBind[rmat, rmat] // MatrixForm

      RowBind[rmat, rmat2] // MatrixForm


# References:

[1] The R Core Team, R Language Definition, (2015).
     URL: https://cran.r-project.org/doc/manuals/r-release/R-lang.pdf

[2] D. Bates, M. Maechler, Sparse and Dense Matrix Classes and Methods, Package 'Matrix', (2015).
     URL: https://cran.r-project.org/web/packages/Matrix/Matrix.pdf.

[3] Anton Antonov, SSparseMatrix Mathematica unit tests, (2018), MathematicaForPrediction at GitHub.
     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/SSparseMatrix-tests.wlt

[4] Anton Antonov, RSparseMatrix Mathematica package, (2015), MathematicaForPrediction at GitHub.
     URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RSparseMatrix.m


This file was created using Mathematica Plugin for IntelliJ IDEA.

Anton Antonov
Windermere, FL, USA
2015-09-27
2018-04-02 (reviewed)

*)

BeginPackage["SSparseMatrix`"]

SSparseMatrix::usage = "Head of a sparse matrix with named rows and columns."

SSparseMatrixQ::usage = "Predicate is the argument a SSparseMatrix object."

MakeSSparseMatrix::usage = "Makes a sparse matrix with named rows and columns."

ToSSparseMatrix::usage = "Converts several types of objects into SSparseMatrix objects. (SparseArray, Dataset, CrossTable results.)"

RowNames::usage = "Gives the row names of a SSparseMatrix object."

ColumnNames::usage = "Gives the column names of a SSparseMatrix object."

DimensionNames::usage = "Gives the dimension names of a SSparseMatrix object."

RowNamesAssociation::usage = "Gives the row names association of a SSparseMatrix object."

ColumnNamesAssociation::usage = "Gives the column names association of a SSparseMatrix object."

DimensionNamesAssociation::usage = "Gives the dimension names association of a SSparseMatrix object."

SetColumnNames::usage = "Sets column names of a SSparseMatrix object."

SetRowNames::usage = "Sets row names of a SSparseMatrix object."

SetDimensionNames::usage = "Sets dimension names of a SSparseMatrix object."

ColumnsCount::usage = "Gives the number of columns of a SSparseMatrix object."

RowsCount::usage = "Gives the number of rows of a SSparseMatrix object."

ColumnSums::usage = "Gives the sums of the columns of a SSparseMatrix object."

ColumnSumsAssociation::usage = "Gives an Association of the sums of the columns of a SSparseMatrix object."

RowSums::usage = "Gives the sums of the rows of a SSparseMatrix object."

RowSumsAssociation::usage = "Gives an Association the sums of the rows of a SSparseMatrix object."

ColumnBind::usage = "Binds SSparseMatrix objects column-wise."

RowBind::usage = "Binds SSparseMatrix objects row-wise."

SSparseMatrixToTriplets::usage = "Gives the long form of a SSparseMatrix."

ImposeColumnNames::usage = "ImposeColumnNames[smat,cn] imposes the column names cn into the SSparseMatrix smat. \
In effect makes an union of cn and ColumnNames[smat]."

ImposeRowNames::usage = "ImposeRowNames[smat,rn] imposes the row names rn into the SSparseMatrix smat. \
In effect makes an union of rn and RowNames[smat]."

Begin["`Private`"]

Clear[SSparseMatrix, MakeSSparseMatrix, ToSSparseMatrix,
  RowNames, ColumnNames, DimensionNames, SetRowNames, SetColumnNames, SetDimensionNames, RowsCount, ColumnsCount,
  RowBind, ColumnBind,
  ImposeRowNames, ImposeColumnNames]

(* Predicate(s) *)

SSparseMatrixQ[x_] := Head[x] === SSparseMatrix;

(*MakeSSparseMatrix[obj_]:=SSparseMatrix[<|"SparseMatrix"->SparseArray[args],"ColumnNames"\[Rule]None,"RowNames"\[Rule]None,"DimensionNames"\[Rule]None|>];*)

(*Creation and conversion*)

SSparseMatrix::rnset =
    "The row names `1` are expected to be a list of strings with length that equals the number of rows (`2`) of the SSparseMatrix object.";

SSparseMatrix::cnset =
    "The column names `1` are expected to be a list of strings with length that equals the number of columns (`2`) of the SSparseMatrix object.";

SSparseMatrix::dnset =
    "The dimension names `1` are expected to be a list of two strings or None.";

SSparseMatrix::dnsame =
    "The dimension names `1` are the same; using {\"1\", \"2\"} instead.";

ToSSparseMatrix::arg1 =
    "The first argument is expected to be a sparse array, a dataset with two dimensions, or a SSparseMatrix object";

Options[MakeSSparseMatrix] = {"RowNames" -> None, "ColumnNames" -> None, "DimensionNames" -> None};

MakeSSparseMatrix[rules_, opts : OptionsPattern[]] :=
    MakeSSparseMatrix[rules, Automatic, 0, opts];

MakeSSparseMatrix[rules_, dims_, val_, opts : OptionsPattern[]] :=
    Block[{sarr},
      sarr = SparseArray[rules, dims, val];
      ToSSparseMatrix[sarr, opts]
    ];

MakeSSparseMatrix[triplets:_?MatrixQ, opts : OptionsPattern[]] :=
    MakeSSparseMatrix[triplets, Automatic, 0, opts]/; Dimensions[triplets][[2]] == 3;

MakeSSparseMatrix[triplets:_?MatrixQ, dims_, val_, opts : OptionsPattern[]] :=
    Block[{sarr, rowNames, colNames, rules},

      rowNames = Union[ triplets[[All,1]] ];
      rowNames = AssociationThread[ rowNames, Range[Length[rowNames]]];

      colNames = Union[ triplets[[All,2]] ];
      colNames = AssociationThread[ colNames, Range[Length[colNames]]];

      rules = triplets;
      rules[[All,1]] = rowNames /@ rules[[All,1]];
      rules[[All,2]] = colNames /@ rules[[All,2]];

      sarr = SparseArray[Most[#]->Last[#]& /@ rules];
      ToSSparseMatrix[sarr, "RowNames"-> Map[ToString,Keys[rowNames]], "ColumnNames"-> Map[ToString,Keys[colNames]], opts]
    ]/; Dimensions[triplets][[2]] == 3;

Options[ToSSparseMatrix] = Options[MakeSSparseMatrix];

ToSSparseMatrix[rmat_SSparseMatrix, opts : OptionsPattern[]] :=
    ToSSparseMatrix[First[rmat]["SparseMatrix"], opts,
      "RowNames" -> RowNames[rmat], "ColumnNames" -> ColumnNames[rmat],
      "DimensionNames" -> DimensionNames[rmat]];

ToSSparseMatrix[sarr_SparseArray, opts : OptionsPattern[]] :=
    Block[{rnames, cnames, dnames},

      rnames = OptionValue[ToSSparseMatrix, "RowNames"];
      cnames = OptionValue[ToSSparseMatrix, "ColumnNames"];
      dnames = OptionValue[ToSSparseMatrix, "DimensionNames"];

      If[! ( rnames === None || (VectorQ[rnames, StringQ] && Length[rnames] == Dimensions[sarr][[1]]) ),
        Message[SSparseMatrix::rnset, rnames, Dimensions[sarr][[1]]];
        Return[$Failed]
      ];

      If[! ( cnames === None || (VectorQ[cnames, StringQ] && Length[cnames] == Dimensions[sarr][[2]]) ),
        Message[SSparseMatrix::cnset, cnames, Dimensions[sarr][[2]]];
        Return[$Failed]
      ];

      If[dnames === {None, None}, dnames = None];

      If[ MatchQ[dnames, {_String, None}], dnames = {dnames[[1]], "2"} ];

      If[ MatchQ[dnames, {None, _String}], dnames = {"1", dnames[[2]]} ];

      If[! (dnames === None || (MatchQ[dnames, {_String ..}] && Length[dnames] == 2)),
        Message[SSparseMatrix::dnset, dnames]; Return[$Failed]
      ];

      If[ Length[dnames] == 2 && dnames[[1]] == dnames[[2]],
        Message[SSparseMatrix::dnsame, dnames];
        dnames = {"1", "2"}
      ];

      SSparseMatrix[<|"SparseMatrix" -> sarr,
          "RowNames" ->
              If[rnames === None, None,
                AssociationThread[rnames, Range[Dimensions[sarr][[1]]]]],
          "ColumnNames" ->
              If[cnames === None, None,
                AssociationThread[cnames, Range[Dimensions[sarr][[2]]]]],
          "DimensionNames" ->
              If[dnames === None,
                AssociationThread[{"1", "2"}, {1, 2}],
                AssociationThread[dnames, {1, 2}]
              ]
      |>]
    ];

ToSSparseMatrix[ds_Dataset, opts : OptionsPattern[]] :=
    Block[{rows, dsRownames, dsColnames, vals, res},
      rows = Normal[ds];
      If[AssociationQ[rows],
        dsRownames = Keys[rows];
        rows = rows /@ dsRownames,
      (*ELSE*)
        dsRownames = None;
      ];
      If[AssociationQ[rows[[1]]],
        dsColnames = Keys[rows[[1]]];
        vals = Map[Values, rows],
      (*ELSE*)
        dsColnames = None;
        vals = rows;
      ];

      res = ToSSparseMatrix[SparseArray[vals], "RowNames" -> dsRownames, "ColumnNames" -> dsColnames];

      If[ Length[{opts}] == 0, res,
        ToSSparseMatrix[ res, opts ]
      ]
    ] /; Length[Dimensions[ds]] == 2;


ToSSparseMatrix[xtabs_Association, opts : OptionsPattern[] ] :=
    Block[{},
      ToSSparseMatrix[ xtabs["SparseMatrix"],
        "RowNames" -> Map[ToString, xtabs["RowNames"]],
        "ColumnNames" -> Map[ToString, xtabs["ColumnNames"]],
        opts
      ]
    ]/; KeyExistsQ[xtabs, "SparseMatrix"] && KeyExistsQ[xtabs, "RowNames"] && KeyExistsQ[xtabs, "ColumnNames"];

ToSSparseMatrix[___] := Message[ToSSparseMatrix::arg1];

SparseArray[rmat_SSparseMatrix] ^:= First[rmat]["SparseMatrix"];


(* Setters *)

(*SetAttributes[SetRowNames, HoldFirst]*)
SetRowNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToSSparseMatrix[rmat,"RowNames"->names,"ColumnNames"->ColumnNames[rmat],"DimensionNames"->DimensionNames[rmat]];
      If[ Head[res] === SSparseMatrix,
        res,
        $Failed
      ]
    ];

(*SetAttributes[SetColumnNames, HoldFirst]*)
SetColumnNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToSSparseMatrix[rmat,"RowNames"->RowNames[rmat],"ColumnNames"->names,"DimensionNames"->DimensionNames[rmat]];
      If[ TrueQ[Head[res] === SSparseMatrix],
         res,
        $Failed
      ]
    ];


(*SetAttributes[SetDimensionNames, HoldFirst]*)
SetDimensionNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToSSparseMatrix[rmat,"RowNames"->RowNames[rmat],"ColumnNames"->ColumnNames[rmat],"DimensionNames"->names];
      If[ TrueQ[Head[res] === SSparseMatrix],
        res,
        $Failed
      ]
    ];


(*Query methods*)

RowNames[SSparseMatrix[obj_]] :=
    If[obj["RowNames"] === None, None, Keys[obj["RowNames"]]];

ColumnNames[SSparseMatrix[obj_]] :=
    If[obj["ColumnNames"] === None, None, Keys[obj["ColumnNames"]]];

DimensionNames[SSparseMatrix[obj_]] :=
    If[obj["DimensionNames"] === None, {None, None}, Keys[obj["DimensionNames"]]];

RowNamesAssociation[SSparseMatrix[obj_]] :=
    If[obj["RowNames"] === None, None, obj["RowNames"]];

ColumnNamesAssociation[SSparseMatrix[obj_]] :=
    If[obj["ColumnNames"] === None, None, obj["ColumnNames"]];

DimensionNamesAssociation[SSparseMatrix[obj_]] :=
    If[obj["DimensionNames"] === None, {None, None}, obj["DimensionNames"]];


ArrayRules[SSparseMatrix[obj_]] ^:=
    ArrayRules[obj["SparseMatrix"]];

Dimensions[SSparseMatrix[obj_]] ^:=
    Dimensions[obj["SparseMatrix"]];

RowsCount[r_SSparseMatrix] := Dimensions[r][[1]];
ColumnsCount[r_SSparseMatrix] := Dimensions[r][[2]];

(*Transpose*)

Transpose[SSparseMatrix[obj_]] ^:=
    Block[{assoc = obj},
      assoc["SparseMatrix"] = Transpose[obj["SparseMatrix"]];
      assoc["ColumnNames"] = obj["RowNames"];
      assoc["RowNames"] = obj["ColumnNames"];
      assoc["DimensionNames"] = If[obj["DimensionNames"] === None, None, Reverse[obj["DimensionNames"]]];
      SSparseMatrix[assoc]
    ];

(*Showing the matrix*)

MatrixForm[SSparseMatrix[obj_], args___] ^:=
    MatrixForm[SSparseMatrix[obj][[1]]["SparseMatrix"], args,
      TableHeadings -> {RowNames[SSparseMatrix[obj]],
        ColumnNames[SSparseMatrix[obj]]}];

MatrixPlot[SSparseMatrix[obj_], args___] ^:=
    MatrixPlot[obj["SparseMatrix"], args];

(*Sums*)

RowSums[SSparseMatrix[obj_]] := Total[obj["SparseMatrix"], {2}];

RowSumsAssociation[smat_SSparseMatrix] := AssociationThread[RowNames[smat], RowSums[smat]];

ColumnSums[SSparseMatrix[obj_]] := Total[obj["SparseMatrix"]];

ColumnSumsAssociation[smat_SSparseMatrix] := AssociationThread[ColumnNames[smat], ColumnSums[smat]];

Clip[SSparseMatrix[objArg_], args___] ^:=
    Block[{obj=objArg},
      obj["SparseMatrix"] = Clip[ obj["SparseMatrix"], args];
      SSparseMatrix[obj]
    ];

Total[SSparseMatrix[obj_], args___] ^:= Total[obj["SparseMatrix"], args];

(*Dot product*)

(*Note that here we do not have to define the behavior for Dot[r1,r2,r3,r4,\[Ellipsis]] .*)

Dot[SSparseMatrix[obj1_], SSparseMatrix[obj2_]] ^:=
    Block[{res},
      res = Dot[SSparseMatrix[obj1][[1]]["SparseMatrix"], SSparseMatrix[obj2][[1]]["SparseMatrix"]];
      ToSSparseMatrix[res, "RowNames" -> RowNames[SSparseMatrix[obj1]],
        "ColumnNames" -> ColumnNames[SSparseMatrix[obj2]],
        "DimensionNames" -> {DimensionNames[SSparseMatrix[obj1]][[1]],
          DimensionNames[SSparseMatrix[obj2]][[2]]}]
    ];

Dot[SSparseMatrix[obj_], x_] ^:=
    Block[{res},
      res = Dot[SSparseMatrix[obj][[1]]["SparseMatrix"], x];
      ToSSparseMatrix[res, "RowNames" -> RowNames[SSparseMatrix[obj]],
        "DimensionNames" -> {DimensionNames[SSparseMatrix[obj]][[1]], "2"}]
    ];

Dot[x_, SSparseMatrix[obj_]] ^:=
    Block[{res},
      res = Dot[x, SSparseMatrix[obj][[1]]["SparseMatrix"]];
      ToSSparseMatrix[res, "ColumnNames" -> ColumnNames[SSparseMatrix[obj]],
        "DimensionNames" -> {"1", DimensionNames[SSparseMatrix[obj]][[2]]}]
    ];

(* Arithmetic operators *)

(*Here we need to have an option to respect or to ignore the row names and column names.*)

Times[rmat1_SSparseMatrix, rmat2_SSparseMatrix] ^:=
    Block[{},
      If[ TrueQ[ RowNames[rmat1] == RowNames[rmat2] && ColumnNames[rmat1] == ColumnNames[rmat2] ],
        ToSSparseMatrix[Times[SparseArray[rmat1], SparseArray[rmat2]],
          "RowNames" -> RowNames[rmat1], "ColumnNames" -> ColumnNames[rmat1],
          "DimensionNames" -> DimensionNames[rmat1]],
      (*ELSE*)
        ToSSparseMatrix[Times[SparseArray[rmat1], SparseArray[rmat2]]]
      ]
    ];

Times[rmat1_SSparseMatrix, x_] ^:=
    ToSSparseMatrix[Times[SparseArray[rmat1], x], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

Times[x_, rmat1_SSparseMatrix] ^:=
    ToSSparseMatrix[Times[x, SparseArray[rmat1]], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

(* Same as above for Plus. *)

Plus[rmat1_SSparseMatrix, rmat2_SSparseMatrix] ^:=
    Block[{},
      If[TrueQ[ RowNames[rmat1] == RowNames[rmat2] && ColumnNames[rmat1] == ColumnNames[rmat2] ],
        ToSSparseMatrix[Plus[SparseArray[rmat1], SparseArray[rmat2]],
          "RowNames" -> RowNames[rmat1], "ColumnNames" -> ColumnNames[rmat1],
          "DimensionNames" -> DimensionNames[rmat1]],
      (*ELSE*)

        ToSSparseMatrix[Plus[SparseArray[rmat1], SparseArray[rmat2]]]
      ]
    ];

Plus[rmat1_SSparseMatrix, x_] ^:=
    ToSSparseMatrix[Plus[SparseArray[rmat1], x], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

Plus[x_, rmat1_SSparseMatrix] ^:=
    ToSSparseMatrix[Plus[x, SparseArray[rmat1]], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

(* Part *)

(*Part[SSparseMatrix[obj_], s1:(_Integer | {_Integer..} | _Span ) ] ^:= Part[obj["SparseMatrix"], s1, All];*)

Part[SSparseMatrix[obj_], s1 : (_String | {_String ..})] ^:=
    Block[{ i1 },
      i1 = If[ ListQ[s1], obj["RowNames"] /@ s1, obj["RowNames"] @ s1 ];
      Part[ SSparseMatrix[obj], i1, All ]
    ];
Part[SSparseMatrix[obj_], s1 : (_String | {_String ..}), s2 : (_String | {_String ..})] ^:=
    Block[{ i1, i2 },
      i1 = If[ ListQ[s1], obj["RowNames"] /@ s1, obj["RowNames"] @ s1 ];
      i2 = If[ ListQ[s2], obj["ColumnNames"] /@ s2, obj["ColumnNames"] @ s2 ];
      Part[ SSparseMatrix[obj], i1, i2 ]
    ];
Part[SSparseMatrix[obj_], s1 : (_String | {_String ..}), s2_] ^:=
    Block[{ i1 },
      i1 = If[ ListQ[s1], obj["RowNames"] /@ s1, obj["RowNames"] @ s1 ];
      Part[ SSparseMatrix[obj], i1, s2 ]
    ];
Part[SSparseMatrix[obj_], s1_, s2 : (_String | {_String ..})] ^:=
    Block[{ i2 },
      i2 = If[ ListQ[s2], obj["ColumnNames"] /@ s2, obj["ColumnNames"] @ s2 ];
      Part[ SSparseMatrix[obj], s1, i2 ]
    ];
Part[SSparseMatrix[obj_], s1_, s2_] ^:=
    Block[{smat},
      smat = Part[ obj["SparseMatrix"], s1, s2 ];
      If[Head[smat] === Part,
        smat,
        If[ MatrixQ[smat],
          ToSSparseMatrix[smat,
            "RowNames" -> If[ RowNames[SSparseMatrix[obj]]===None, None, RowNames[SSparseMatrix[obj]][[s1]] ],
            "ColumnNames" -> If[ ColumnNames[SSparseMatrix[obj]]===None, None, ColumnNames[SSparseMatrix[obj]][[s2]] ],
            "DimensionNames" -> DimensionNames[SSparseMatrix[obj]]],
        (* ELSE *)
          smat
        ]
      ]
    ];

(* RowBind, ColumnBind *)

(* Here we need to have an option to respect or to ignore the row names and column names for RowBind and ColumnBind respectively.

    RowBind[r1_SSparseMatrix, r2_SSparseMatrix, opts : OptionsPattern[]]
ColumnBind[r1_SSparseMatrix, r2_SSparseMatrix, opts : OptionsPattern[]]

There are three solutions (1) using array rules, (2) using matrix padding, ArrayPad, ArrayReshape, PadLeft and PadRight, and (3) using Join.

    Here are the steps of the first algorithm for RowBind:
    1. Get array rules of both sparse arrays.
        2. Increment the row indices of the second one with the number of rows of the first one.
        3. Join the rules and make a new SparseArray object.
        4. Make a new SSparseMatrix object with its row names being the joined row names of the arguments.

        Here are the steps of the second algorithm for RowBind:
    1. Pad from below the sparse array of the first argument to the number of result rows.
        2. Pad from above the sparse array of the second argument to the number of result rows.
        3. Sum the padded sparse arrays.
        4. Make the result SSparseMatrix object with the row names being the joined row names of the arguments.

        Using Join is of course straightforward.

        Since Association removes duplication of keys special care has to be taken when joining the row and column names.
*)

(*Options[RowBind] = {"IgnoreColumnNames" -> False};*)

RowBind::ncols = "The column names of the two SSparseMatrix objects are expected to be the same.";

RowBind[r1_SSparseMatrix, r2_SSparseMatrix, rm__] := RowBind[ RowBind[r1, r2], rm];

RowBind[rm:{_SSparseMatrix..}] := Fold[RowBind, First[rm], Rest[rm]];

RowBind[r1_SSparseMatrix, r2_SSparseMatrix ] :=
    Block[{sarr, joinedRowAssoc, resRowNames},

      If[Sort[ColumnNames[r1]] != Sort[ColumnNames[r2]],
        Message[RowBind::ncols];
        Return[$Failed];
      ];

      sarr = Join[ SparseArray[r1], SparseArray[r2] ];
      (* Special handling of duplication of row names in the result. *)

      joinedRowAssoc = Join[First[r1]["RowNames"], First[r2]["RowNames"]];
      If[Length[joinedRowAssoc] == Dimensions[sarr][[1]],
        resRowNames = Join[RowNames[r1], RowNames[r2]],
        resRowNames =
            Join[# <> ".1" & /@ RowNames[r1], # <> ".2" & /@ RowNames[r2]]
      ];
      ToSSparseMatrix[sarr, "RowNames" -> resRowNames,
        "ColumnNames" -> ColumnNames[r1], "DimensionNames" -> DimensionNames[r1]]
    ];

(*Options[ColumnBind] = {"IgnoreRowNames" -> False};*)

ColumnBind[r1_SSparseMatrix, r2_SSparseMatrix, rm__] := ColumnBind[ ColumnBind[r1, r2], rm];

ColumnBind[rm:{_SSparseMatrix..}] := Fold[ColumnBind, First[rm], Rest[rm]];

ColumnBind[r1_SSparseMatrix, r2_SSparseMatrix ] :=
    Block[{sarr, joinedRowAssoc, resColumnNames},
      sarr = Transpose@
          Join[Transpose@SparseArray[r1], Transpose@SparseArray[r2]];
      (* Special handling of duplication of column names in the result. *)

      joinedRowAssoc = Join[ColumnNamesAssociation[r1], ColumnNamesAssociation[r2]];
      If[Length[joinedRowAssoc] == Dimensions[sarr][[2]],
        resColumnNames = Join[ColumnNames[r1], ColumnNames[r2]],
        (*ELSE*)
        resColumnNames =
            Join[# <> ".1" & /@ ColumnNames[r1], # <> ".2" & /@ ColumnNames[r2]]
      ];
      ToSSparseMatrix[sarr, "RowNames" -> RowNames[r1],
        "ColumnNames" -> resColumnNames, "DimensionNames" -> DimensionNames[r1]]
    ];


Clear[ImposeRowNames, ImposeColumnNames]

ImposeRowNames[rmat_SSparseMatrix, rowNames : {_String ..}] :=
    ImposeRowNames[rmat, AssociationThread[rowNames -> Range[Length[rowNames]]]];

ImposeRowNames[rmat_SSparseMatrix, rowNames : Association[(_String -> _Integer) ..]] :=

    Block[{arules, rmatRowNames, aInds, resMat},

      arules = ArrayRules[SparseArray[rmat]];

      rmatRowNames = RowNamesAssociation[rmat];

      aInds =
          AssociationThread[Values[rmatRowNames],
            Lookup[rowNames, #, None] & /@ Keys[rmatRowNames]];

      arules =
          Append[Map[{aInds[#[[1, 1]]], #[[1, 2]]} -> #[[2]] &,
            Most[arules]], Last[arules]];
      arules = DeleteCases[arules, {None, _Integer} -> _];

      ToSSparseMatrix[
        SparseArray[arules, {Length[rowNames], ColumnsCount[rmat]}],
        "RowNames" -> Keys[rowNames], "ColumnNames" -> ColumnNames[rmat]]
    ];

ImposeColumnNames[rmat_SSparseMatrix, colNames : {_String ..} | Association[(_String->_Integer)..]] :=
    Transpose[ImposeRowNames[Transpose[rmat], colNames]];


Clear[SSparseMatrixToTriplets]
SSparseMatrixToTriplets[ rsmat_SSparseMatrix ] :=
    Block[{t},
      t = Most[ArrayRules[rsmat]];
      t[[All, 1, 1]] = t[[All, 1, 1]] /. Dispatch[Thread[Range[RowsCount[rsmat]] -> RowNames[rsmat]]];
      t[[All, 1, 2]] = t[[All, 1, 2]] /. Dispatch[Thread[Range[ColumnsCount[rsmat]] -> ColumnNames[rsmat]]];
      Flatten/@ (List @@@ t)
    ];

(* Delegation to SparseArray functions *)

(* This is similar to the OOP design pattern Decorator.
The implementation is still experimental.
New functions for SSparseMatrix objects have to be added into the do-not-decorate list.
Note that this decoration is very aggressive and it might have un-forseen effects.
*)

(* Format *)

Format[SSparseMatrix[obj_]] := obj["SparseMatrix"];

(*F_[rmat_SSparseMatrix, args___] ^:=*)
    (*Block[{res = F[SparseArray[rmat], args]},*)
      (*Print["SSparseMatrix decoration::F=",F];*)
      (*Print["SSparseMatrix decoration::res=",res];*)
      (*If[MatrixQ[res],*)
        (*SSparseMatrix[*)
          (*Join[<|"SparseMatrix" -> SparseArray[res]|>, Rest[First[rmat]]]],*)
        (*res*)
      (*]*)
    (*] /;*)
        (*! MemberQ[*)
          (*Join[{"SparseMatrix", "ToSSparseMatrix",*)
            (*"RowNames", "ColumnNames", "DimensionNames",*)
            (*"SetRowNames", "SetColumnNames", "SetDimensionNames",*)
            (*"MatrixForm", "MatrixPlot",*)
            (*"Dimensions", "ArrayRules",*)
            (*"Total", "RowSums", "ColumnSums", "RowsCount", "ColumnsCount",*)
            (*"Dot", "Plus", "Times", "Part",*)
            (*"RowBind", "ColumnBind",*)
            (*"Head", "Format", "Print"},*)
            (*Names["System`*Hold*"],*)
            (*Names["System`Inactiv*"],*)
            (*Names["System`Activ*"]*)
          (*], SymbolName[F] ];*)

End[]

EndPackage[]
