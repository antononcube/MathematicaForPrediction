(*
    RSparseMatrix Mathematica package
    Copyright (C) 2015  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2015 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: RSparseMatrix *)
(* :Author: Anton Antonov *)
(* :Date: 2015-09-27 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 10.2 *)
(* :Copyright: (c) 2015 Anton Antonov *)
(* :Keywords: R, sparse array, sparse matrix, named rows, named columns *)
(* :Discussion:

This notebook has the function implementations for manipulating objects with head RSparseMatrix that behave like
SparseArray objects but have the added functionalities to use row names and column names in a manner similar to
that of the sparse arrays objects from the base library Matrix [2] for the programming language R [1].

The idea is fairly simple: we can use associations or replacement rules to map row names and column names into integers.
Similarly to how it is done in R, RSparseMatrix handles only strings as row names and column names.

Note that assignment (with Set[__]) is not implemented.

Since the package is under development it is not a real Mathematica package.

References:

[1] The R Core Team, R Language Definition, (2015).
     URL: https://cran.r-project.org/doc/manuals/r-release/R-lang.pdf

[2] D. Bates, M. Maechler, Sparse and Dense Matrix Classes and Methods, Package 'Matrix', (2015).
     URL: https://cran.r-project.org/web/packages/Matrix/Matrix.pdf.

This file was created using Mathematica Plugin for IntelliJ IDEA.

Anton Antonov
2015-09-27

*)


ClearAll[RSparseMatrix, MakeRSparseMatrix, ToRSparseMatrix, RowNames,
  ColumnNames, DimensionNames, RowsCount, ColumnsCount, SetRowNames, SetColumnNames, SetDimensionNames ]

Predicate(s)

RSparseMatrixQ[x_] := Head[x] === RSparseMatrix;

(*MakeRSparseMatrix[obj_]:=RSparseMatrix[<|"sparseArray"->SparseArray[args],"colnames"\[Rule]None,"rownames"\[Rule]None,"dimnames"\[Rule]None|>];*)

(*Creation and conversion*)

RSparseMatrix::rnset =
    "The row names `1` are expected to be a list of strings with length that equals the number of rows (`2`) of the RSparseMatrix object.";

RSparseMatrix::cnset =
    "The column names `1` are expected to be a list of strings with length that equals the number of columns (`2`) of the RSparseMatrix object.";

RSparseMatrix::dnset =
    "The dimension names `1` are expected to be a list of two strings.";

ToRSparseMatrix::arg1 =
    "The first argument is expected to a sparse array or a RSparseMatrix object.";

Options[MakeRSparseMatrix] = {"RowNames" -> None, "ColumnNames" -> None, "DimensionNames" -> None};

MakeRSparseMatrix[rules_, opts : OptionsPattern[]] :=
    MakeRSparseMatrix[rules, Automatic, 0, opts];

MakeRSparseMatrix[rules_, dims_, val_, opts : OptionsPattern[]] :=
    Block[{sarr},
      sarr = SparseArray[rules, dims, val];
      ToRSparseMatrix[sarr, opts]
    ];

Options[ToRSparseMatrix] = Options[MakeRSparseMatrix];

ToRSparseMatrix[rmat_RSparseMatrix, opts : OptionsPattern[]] :=
    ToRSparseMatrix[rmat[[1]]["sparseArray"], opts,
      "RowNames" -> RowNames[rmat], "ColumnNames" -> ColumnNames[rmat],
      "DimensionNames" -> DimensionNames[rmat]];

ToRSparseMatrix[sarr_SparseArray, opts : OptionsPattern[]] :=
    Block[{rnames, cnames, dnames},
      rnames = OptionValue[ToRSparseMatrix, "RowNames"];
      cnames = OptionValue[ToRSparseMatrix, "ColumnNames"];
      dnames = OptionValue[ToRSparseMatrix, "DimensionNames"];
      If[! (rnames === None || (MatchQ[rnames, {_String ..}] && Length[rnames] == Dimensions[sarr][[1]])),
        Message[RSparseMatrix::rnset, rnames, Dimensions[sarr][[1]]];
        Return[$Failed]
      ];
      If[! (cnames === None || (MatchQ[cnames, {_String ..}] && Length[cnames] == Dimensions[sarr][[2]])),
        Message[RSparseMatrix::cnset, cnames, Dimensions[sarr][[2]]];
        Return[$Failed]
      ];
      If[dnames === {None, None}, dnames = None];
      If[ MatchQ[dnames, {_String, None}] || MatchQ[dnames, {None, _String}], dnames = dnames /. None->"" ];
      If[! (dnames === None || (MatchQ[dnames, {_String ..}] && Length[dnames] == 2)),
        Message[RSparseMatrix::dnset, dnames]; Return[$Failed]
      ];
      RSparseMatrix[<|"sparseArray" -> sarr,
          "rownames" ->
              If[rnames === None, None,
                AssociationThread[rnames, Range[Dimensions[sarr][[1]]]]],
          "colnames" ->
              If[cnames === None, None,
                AssociationThread[cnames, Range[Dimensions[sarr][[2]]]]],
          "dimnames" ->
              If[dnames === None, None, AssociationThread[dnames, {1, 2}]]|>]
    ];

ToRSparseMatrix[___] := Message[ToRSparseMatrix::arg1];

SparseArray[rmat_RSparseMatrix] ^:= rmat[[1]]["sparseArray"];

(* Setters *)

SetAttributes[SetRowNames, HoldFirst]
SetRowNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToRSparseMatrix[rmat,"RowNames"->names,"ColumnNames"->ColumnNames[rmat],"DimensionNames"->DimensionNames[rmat]];
      If[ Head[res] === RSparseMatrix,
        rmat = res; rmat,
        $Failed
      ]
    ];

SetAttributes[SetColumnNames, HoldFirst]
SetColumnNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToRSparseMatrix[rmat,"RowNames"->RowNames[rmat],"ColumnNames"->names,"DimensionNames"->DimensionNames[rmat]];
      If[ Head[res] === RSparseMatrix,
        rmat = res; rmat,
        $Failed
      ]
    ];


SetAttributes[SetDimensionNames, HoldFirst]
SetDimensionNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToRSparseMatrix[rmat,"RowNames"->RowNames[rmat],"ColumnNames"->ColumnNames[rmat],"DimensionNames"->names];
      If[ Head[res] === RSparseMatrix,
        rmat = res; rmat,
        $Failed
      ]
    ];


(*Query methods*)

RowNames[sarr_RSparseMatrix] :=
    If[sarr[[1]]["rownames"] === None, None, Keys[sarr[[1]]["rownames"]]];

ColumnNames[sarr_RSparseMatrix] :=
    If[sarr[[1]]["colnames"] === None, None, Keys[sarr[[1]]["colnames"]]];

DimensionNames[sarr_RSparseMatrix] :=
    If[sarr[[1]]["dimnames"] === None, {None, None}, Keys[sarr[[1]]["dimnames"]]];

ArrayRules[RSparseMatrix[obj_]] ^:=
    ArrayRules[RSparseMatrix[obj][[1]]["sparseArray"]];

Dimensions[RSparseMatrix[obj_]] ^:=
    Dimensions[RSparseMatrix[obj][[1]]["sparseArray"]];

RowsCount[r_RSparseMatrix] := Dimensions[r][[1]];
ColumnsCount[r_RSparseMatrix] := Dimensions[r][[2]];

(*Transpose*)

Transpose[RSparseMatrix[obj_]] ^:=
    Block[{assoc = obj},
      assoc["sparseArray"] = Transpose[obj["sparseArray"]];
      assoc["colnames"] = obj["rownames"];
      assoc["rownames"] = obj["colnames"];
      assoc["dimnames"] = If[obj["dimnames"] === None, None, Reverse[obj["dimnames"]]];
      RSparseMatrix[assoc]
    ];

(*Showing the matrix*)

MatrixForm[RSparseMatrix[obj_], args___] ^:=
    MatrixForm[RSparseMatrix[obj][[1]]["sparseArray"], args,
      TableHeadings -> {RowNames[RSparseMatrix[obj]],
        ColumnNames[RSparseMatrix[obj]]}];

MatrixPlot[RSparseMatrix[obj_], args___] ^:=
    MatrixPlot[RSparseMatrix[obj][[1]]["sparseArray"], args];

(*Sums*)

RowSums[rmat_RSparseMatrix] := Total[rmat[[1]]["sparseArray"]];
ColumnSums[rmat_RSparseMatrix] := Total[rmat[[1]]["sparseArray"], {2}];

Total[rmat_RSparseMatrix, args___] ^:= Total[rmat[[1]]["sparseArray"], args];

(*Dot product*)

(*Note that here we do not have to define the behavior for Dot[r1,r2,r3,r4,\[Ellipsis]] .*)

Dot[RSparseMatrix[obj1_], RSparseMatrix[obj2_]] ^:=
    Block[{res},
      res = Dot[RSparseMatrix[obj1][[1]]["sparseArray"],
        RSparseMatrix[obj2][[1]]["sparseArray"]];
      ToRSparseMatrix[res, "RowNames" -> RowNames[RSparseMatrix[obj1]],
        "ColumnNames" -> ColumnNames[RSparseMatrix[obj2]],
        "DimensionNames" -> {DimensionNames[RSparseMatrix[obj1]][[1]],
          DimensionNames[RSparseMatrix[obj2]][[2]]}]
    ];

Dot[RSparseMatrix[obj_], x_] ^:=
    Block[{res},
      res = Dot[RSparseMatrix[obj][[1]]["sparseArray"], x];
      ToRSparseMatrix[res, "RowNames" -> RowNames[RSparseMatrix[obj]],
        "DimensionNames" -> {DimensionNames[RSparseMatrix[obj]][[1]], ""}]
    ];

Dot[x_, RSparseMatrix[obj_]] ^:=
    Block[{res},
      res = Dot[x, RSparseMatrix[obj][[1]]["sparseArray"]];
      ToRSparseMatrix[res, "ColumnNames" -> ColumnNames[RSparseMatrix[obj]],
        "DimensionNames" -> {"", DimensionNames[RSparseMatrix[obj]][[2]]}]
    ];

(* Arithmetic operators *)

(*Here we need to have an option to respect or to ignore the row names and column names.*)

Times[rmat1_RSparseMatrix, rmat2_RSparseMatrix] ^:=
    Block[{},
      If[ TrueQ[ RowNames[rmat1] == RowNames[rmat2] && ColumnNames[rmat1] == ColumnNames[rmat2] ],
        ToRSparseMatrix[Times[SparseArray[rmat1], SparseArray[rmat2]],
          "RowNames" -> RowNames[rmat1], "ColumnNames" -> ColumnNames[rmat1],
          "DimensionNames" -> DimensionNames[rmat1]],
      (*ELSE*)
        ToRSparseMatrix[Times[SparseArray[rmat1], SparseArray[rmat2]]]
      ]
    ];

Times[rmat1_RSparseMatrix, x_] ^:=
    ToRSparseMatrix[Times[SparseArray[rmat1], x], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

Times[x_, rmat1_RSparseMatrix] ^:=
    ToRSparseMatrix[Times[x, SparseArray[rmat1]], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

(* Same as above for Plus. *)

Plus[rmat1_RSparseMatrix, rmat2_RSparseMatrix] ^:=
    Block[{},
      If[TrueQ[ RowNames[rmat1] == RowNames[rmat2] && ColumnNames[rmat1] == ColumnNames[rmat2] ],
        ToRSparseMatrix[Plus[SparseArray[rmat1], SparseArray[rmat2]],
          "RowNames" -> RowNames[rmat1], "ColumnNames" -> ColumnNames[rmat1],
          "DimensionNames" -> DimensionNames[rmat1]],
      (*ELSE*)

        ToRSparseMatrix[Plus[SparseArray[rmat1], SparseArray[rmat2]]]
      ]
    ];

Plus[rmat1_RSparseMatrix, x_] ^:=
    ToRSparseMatrix[Plus[SparseArray[rmat1], x], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

Plus[x_, rmat1_RSparseMatrix] ^:=
    ToRSparseMatrix[Plus[x, SparseArray[rmat1]], "RowNames" -> RowNames[rmat1],
      "ColumnNames" -> ColumnNames[rmat1],
      "DimensionNames" -> DimensionNames[rmat1]];

(* Part *)

Part[RSparseMatrix[obj_], s1Arg : (_String | {_String ..})] ^:=
    Block[{ s1 = s1Arg },
      s1 = If[ListQ[s1],s1,{s1}];
      Part[RSparseMatrix[obj], obj["rownames"] /@ s1, All]
    ];
Part[RSparseMatrix[obj_], s1Arg : (_String | {_String ..}), s2Arg : (_String | {_String ..})] ^:=
    Block[{s1 = s1Arg, s2 = s2Arg },
      s1 = If[ListQ[s1Arg],s1,{s1}]; s2 = If[ListQ[s2Arg],s2,{s2}];
      Part[RSparseMatrix[obj], obj["rownames"] /@ s1, obj["colnames"] /@ s2]
    ];
Part[RSparseMatrix[obj_], s1Arg : (_String | {_String ..}), s2_] ^:=
    Block[{ s1 = s1Arg },
      s1 = If[ListQ[s1Arg],s1,{s1}];
      Part[RSparseMatrix[obj], obj["rownames"] /@ s1, s2]
    ];
Part[RSparseMatrix[obj_], s1_, s2Arg : (_String | {_String ..})] ^:=
    Block[{ s2 = s2Arg },
      s2 = If[ListQ[s2Arg],s2,{s2}];
      Part[RSparseMatrix[obj], s1, obj["colnames"] /@ s2]
    ];
Part[RSparseMatrix[obj_], s1_, s2_] ^:=
    Block[{smat},
      smat = Part[obj["sparseArray"], s1, s2];
      If[Head[smat] === Part,
        smat,
        ToRSparseMatrix[smat, "RowNames" -> RowNames[RSparseMatrix[obj]][[s1]],
          "ColumnNames" -> ColumnNames[RSparseMatrix[obj]][[s2]],
          "DimensionNames" -> DimensionNames[RSparseMatrix[obj]]]
      ]
    ];


(* RowBind, ColumnBind *)

(* Here we need to have an option to respect or to ignore the row names and column names for RowBind and ColumnBind respectively.

    RowBind[r1_RSparseMatrix, r2_RSparseMatrix, opts : OptionsPattern[]]
ColumnBind[r1_RSparseMatrix, r2_RSparseMatrix, opts : OptionsPattern[]]

There are three solutions (1) using array rules, (2) using matrix padding, ArrayPad, ArrayReshape, PadLeft and PadRight, and (3) using Join.

    Here are the steps of the first algorithm for RowBind:
    1. Get array rules of both sparse arrays.
        2. Increment the row indices of the second one with the number of rows of the first one.
        3. Join the rules and make a new SparseArray object.
        4. Make a new RSparseMatrix object with its row names being the joined row names of the arguments.

        Here are the steps of the second algorithm for RowBind:
    1. Pad from below the sparse array of the first argument to the number of result rows.
        2. Pad from above the sparse array of the second argument to the number of result rows.
        3. Sum the padded sparse arrays.
        4. Make the result RSparseMatrix object with the row names being the joined row names of the arguments.

        Using Join is of course straightforward.

        Since Association removes duplication of keys special care has to be taken when joining the row and column names.
*)

Options[RowBind] = {"IgnoreColumnNames" -> False};
RowBind[r1_RSparseMatrix, r2_RSparseMatrix, opts : OptionsPattern[]] :=
    Block[{sarr, joinedRowAssoc, resRowNames},
      sarr = Join[r1[[1]]["sparseArray"], r2[[1]]["sparseArray"]];
      (* Special handling of duplication of row names in the result. *)

      joinedRowAssoc = Join[r1[[1]]["rownames"], r2[[1]]["rownames"]];
      If[Length[joinedRowAssoc] == Dimensions[sarr][[1]],
        resRowNames = Join[RowNames[r1], RowNames[r2]],
        resRowNames =
            Join[# <> ".1" & /@ RowNames[r1], # <> ".2" & /@ RowNames[r2]]
      ];
      ToRSparseMatrix[sarr, "RowNames" -> resRowNames,
        "ColumnNames" -> ColumnNames[r1], "DimensionNames" -> DimensionNames[r1]]
    ];

Options[ColumnBind] = {"IgnoreRowNames" -> False};
ColumnBind[r1_RSparseMatrix, r2_RSparseMatrix, opts : OptionsPattern[]] :=
    Block[{sarr, joinedRowAssoc, resColumnNames},
      sarr = Transpose@
          Join[Transpose@r1[[1]]["sparseArray"],
            Transpose@r2[[1]]["sparseArray"]];
      (* Special handling of duplication of column names in the result. *)

      joinedRowAssoc = Join[r1[[1]]["colnames"], r2[[1]]["colnames"]];
      If[Length[joinedRowAssoc] == Dimensions[sarr][[2]],
        resColumnNames = Join[ColumnNames[r1], ColumnNames[r2]],
        resColumnNames =
            Join[# <> ".1" & /@ ColumnNames[r1], # <> ".2" & /@ ColumnNames[r2]]
      ];
      ToRSparseMatrix[sarr, "RowNames" -> RowNames[r1],
        "ColumnNames" -> resColumnNames, "DimensionNames" -> DimensionNames[r1]]
    ];