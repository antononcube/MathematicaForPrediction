(*
    SSparseMatrix Mathematica package
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


(* :Title: SSparseMatrix *)
(* :Author: Anton Antonov *)
(* :Date: 2015-09-27 *)

(* :Package Version: 0.6 *)
(* :Mathematica Version: 10.2 *)
(* :Copyright: (c) 2015 Anton Antonov *)
(* :Keywords: S, R, sparse array, sparse matrix, named rows, named columns *)
(* :Discussion:

This package has the function implementations for manipulating objects with head SSparseMatrix that behave like
SparseArray objects but have the added functionalities to use row names and column names in a manner similar to
that of the sparse arrays objects from the base library Matrix [2] for the programming languages S and R [1].

The idea is fairly simple: we can use associations or replacement rules to map row names and column names into integers.
Similarly to how it is done in S and R, SSparseMatrix handles only strings as row names and column names.

Note that assignment (with Set[__]) is not implemented.

See the commented out delegation to SparseArray implementation at the of the file.

Since the package is under development it is not a real Mathematica package.

Here are the overloaded core WL functions:

    ArrayRules, Dimensions, Dot, MatrixForm, MatrixPlot, SparseArray, Plus, Times, Total

References:

[1] The R Core Team, R Language Definition, (2015).
     URL: https://cran.r-project.org/doc/manuals/r-release/R-lang.pdf

[2] D. Bates, M. Maechler, Sparse and Dense Matrix Classes and Methods, Package 'Matrix', (2015).
     URL: https://cran.r-project.org/web/packages/Matrix/Matrix.pdf.

This file was created using Mathematica Plugin for IntelliJ IDEA.

Anton Antonov
Windermere, FL, USA
2015-09-27

*)

BeginPackage["SSparseMatrix`"]

SSparseMatrix::usage = "Head of a sparse matrix with named rows and columns."

MakeSSparseMatrix::usage = "Makes a sparse matrix with named rows and columns."

ToSSparseMatrix::usage = "Converts several types of objects into SSparseMatrix objects. (SparseArray, Dataset, CrossTable results.)"

ColumnNames::usage = "Gives the column names of a SSparseMatrix object."

RowNames::usage = "Gives the row names of a SSparseMatrix object."

DimensionNames::usage = "Gives the dimension names of a SSparseMatrix object."

SetColumnNames::usage = "Sets column names of a SSparseMatrix object."

SetRowNames::usage = "Sets row names of a SSparseMatrix object."

SetDimensionNames::usage = "Sets dimension names of a SSparseMatrix object."

ColumnsCount::usage = "Gives the number of columns of a SSparseMatrix object."

RowsCount::usage = "Gives the number of rows of a SSparseMatrix object."

ColumnSums::usage = "Gives the sums of the columns of a SSparseMatrix object."

RowSums::usage = "Gives the sums of the rows of a SSparseMatrix object."

ColumnBind::usage = "Binds SSparseMatrix objects column-wise."

RowBind::usage = "Binds SSparseMatrix objects row-wise."

SSparseMatrixToTriplets::usage = "Gives the long form of a SSparseMatrix."

ImposeColumnNames::usage = "ImposeColumnNames[smat,cn] imposes the column names cn into the SSparseMatrix smat. \
In effect makes an union of cn and ColumnNames[smat]."

ImposeRowNames::usage = "ImposeRowNames[smat,rn] imposes the row names rn into the SSparseMatrix smat. \
In effect makes an union of rn and RowNames[smat]."

Begin["`Private`"]

ClearAll[SSparseMatrix, MakeSSparseMatrix, ToSSparseMatrix,
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
    "The dimension names `1` are expected to be a list of two strings.";

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
      If[! (rnames === None || (MatchQ[rnames, {_String ..}] && Length[rnames] == Dimensions[sarr][[1]])),
        Message[SSparseMatrix::rnset, rnames, Dimensions[sarr][[1]]];
        Return[$Failed]
      ];
      If[! (cnames === None || (MatchQ[cnames, {_String ..}] && Length[cnames] == Dimensions[sarr][[2]])),
        Message[SSparseMatrix::cnset, cnames, Dimensions[sarr][[2]]];
        Return[$Failed]
      ];
      If[dnames === {None, None}, dnames = None];
      If[ MatchQ[dnames, {_String, None}] || MatchQ[dnames, {None, _String}], dnames = dnames /. None->"" ];
      If[! (dnames === None || (MatchQ[dnames, {_String ..}] && Length[dnames] == 2)),
        Message[SSparseMatrix::dnset, dnames]; Return[$Failed]
      ];
      SSparseMatrix[<|"SparseMatrix" -> sarr,
          "RowNames" ->
              If[rnames === None, None,
                AssociationThread[rnames, Range[Dimensions[sarr][[1]]]]],
          "ColumnNames" ->
              If[cnames === None, None,
                AssociationThread[cnames, Range[Dimensions[sarr][[2]]]]],
          "DimensionNames" ->
              If[dnames === None, None, AssociationThread[dnames, {1, 2}]]|>]
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

SetAttributes[SetRowNames, HoldFirst]
SetRowNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToSSparseMatrix[rmat,"RowNames"->names,"ColumnNames"->ColumnNames[rmat],"DimensionNames"->DimensionNames[rmat]];
      If[ Head[res] === SSparseMatrix,
        rmat = res; rmat,
        $Failed
      ]
    ];

SetAttributes[SetColumnNames, HoldFirst]
SetColumnNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToSSparseMatrix[rmat,"RowNames"->RowNames[rmat],"ColumnNames"->names,"DimensionNames"->DimensionNames[rmat]];
      If[ TrueQ[Head[res] === SSparseMatrix],
        rmat = res; rmat,
        $Failed
      ]
    ];


SetAttributes[SetDimensionNames, HoldFirst]
SetDimensionNames[ rmat_, names_:{_String..} ] :=
    Block[{res},
      res = ToSSparseMatrix[rmat,"RowNames"->RowNames[rmat],"ColumnNames"->ColumnNames[rmat],"DimensionNames"->names];
      If[ TrueQ[Head[res] === SSparseMatrix],
        rmat = res; rmat,
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
ColumnSums[SSparseMatrix[obj_]] := Total[obj["SparseMatrix"]];

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
        "DimensionNames" -> {DimensionNames[SSparseMatrix[obj]][[1]], ""}]
    ];

Dot[x_, SSparseMatrix[obj_]] ^:=
    Block[{res},
      res = Dot[x, SSparseMatrix[obj][[1]]["SparseMatrix"]];
      ToSSparseMatrix[res, "ColumnNames" -> ColumnNames[SSparseMatrix[obj]],
        "DimensionNames" -> {"", DimensionNames[SSparseMatrix[obj]][[2]]}]
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

RowBind[r1_SSparseMatrix, r2_SSparseMatrix, rm__] := RowBind[ RowBind[r1, r2], rm];

RowBind[rm:{_SSparseMatrix..}] := Fold[RowBind, First[rm], Rest[rm]];

RowBind[r1_SSparseMatrix, r2_SSparseMatrix ] :=
    Block[{sarr, joinedRowAssoc, resRowNames},
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

      joinedRowAssoc = Join[r1[[1]]["ColumnNames"], r2[[1]]["ColumnNames"]];
      If[Length[joinedRowAssoc] == Dimensions[sarr][[2]],
        resColumnNames = Join[ColumnNames[r1], ColumnNames[r2]],
        resColumnNames =
            Join[# <> ".1" & /@ ColumnNames[r1], # <> ".2" & /@ ColumnNames[r2]]
      ];
      ToSSparseMatrix[sarr, "RowNames" -> RowNames[r1],
        "ColumnNames" -> resColumnNames, "DimensionNames" -> DimensionNames[r1]]
    ];

Clear[ImposeRowNames, ImposeColumnNames]
ImposeRowNames[rmat_SSparseMatrix, rowNames : {_String ..}] :=
    Block[{rmRowNames = RowNames[rmat], resMat, pos},
      resMat =
          Table[SparseArray[{0}, ColumnsCount[rmat]], {Length[rowNames]}];
      resMat =
          Fold[Function[{m, rn},
            pos = Position[rowNames, RowNames[rmat][[rn]]];
            If[Length[pos] == 0, m,
              pos = pos[[1, 1]];
              ReplacePart[m, pos -> rmat[[rn, All]]]
            ]
          ], resMat, Range[RowsCount[rmat]]];
      ToSSparseMatrix[SparseArray[resMat], "RowNames" -> rowNames, "ColumnNames" -> ColumnNames[rmat]]
    ];
ImposeColumnNames[rmat_SSparseMatrix, colNames : {_String ..}] :=
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
