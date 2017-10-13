
(*
    MathematicaForPrediction utilities
    Copyright (C) 2014-2016  Anton Antonov

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
    antononcube @ gmail . com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2016 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: CrossTabulate *)
(* :Context: CrossTabulate` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-10-13 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: cross tabulation, xtabs *)
(* :Discussion:

    # Motivation

    # Usage examples



*)

BeginPackage["CrossTabulate`"]

CrossTensorate::usage = "Finds the contingency co-occurrence values for multiple columns of a matrix \
using a formula specification. The first argument is the formula with the form \
Count == cn1 + cn2 + ... or cn0 == cn1 + cn2 + ..."

CrossTensorateSplit::usage = "Splits the result of CrossTensorate along a variable. The result can be \
shown with MatrixPlot."

CrossTabulate::usage = "Finds the contingency co-occurrence values in a matrix (2D array). The result can be \
shown with MatrixPlot."

xtabsViaRLink::usage = "Calling R's function xtabs {stats} via RLink`."

FromRXTabsForm::usage = "Transforms RObject result of xtabsViaRLink into an association."

Begin["`Private`"]


Clear[CrossTensorate]

SetAttributes[CrossTensorate, HoldFirst];

CrossTensorate::wcnames = "The third argument for the data column names is expected to be Automatic, \
an Association, or a list with length equal to the number of columns in the data." ;

CrossTensorate::wargs = "Wrong arguments.";

CrossTensorate::mcnames = "Not all formula column names are found in the column names specified by \
the third argument.";

CrossTensorate[formula_Equal, data_Dataset, columnNames_: Automatic ] :=
    Block[{colKeys},
      colKeys = Normal[ data[[1]] ];
      Which[
        MatchQ[colKeys, _Association] && TrueQ[columnNames===Automatic],
        CrossTensorate[ formula, Normal[data[All, Values]], Keys[colKeys] ],
        MatchQ[colKeys, _Association],
        CrossTensorate[ formula, Normal[data[All, Values]], columnNames ],
        True,
        CrossTensorate[ formula, Normal[data], columnNames ]
      ]
    ]/; Length[Dimensions[data]]==2;

CrossTensorate[formula_Equal, data_?MatrixQ, columnNames_: Automatic] :=
    Block[{aColumnNames, idRules, formulaLHS, formulaRHS, t},

      Which[
        TrueQ[columnNames === Automatic],
        aColumnNames =
            AssociationThread[Range[Dimensions[data][[2]]] -> Range[Dimensions[data][[2]]]],
        ListQ[columnNames] && Length[columnNames] == Dimensions[data][[2]],
        aColumnNames = AssociationThread[columnNames -> Range[Dimensions[data][[2]]]],
        AssociationQ[columnNames],
        aColumnNames = columnNames,
        True,
        Message[CrossTensorate::wcnames]; Return[{}]
      ];

      aColumnNames =
          Join[ aColumnNames, AssociationThread[Range[Dimensions[data][[2]]]->Range[Dimensions[data][[2]]]] ];

      formulaLHS = formula[[1]];

      If[! TrueQ[formulaLHS === Count], formulaLHS = aColumnNames[formulaLHS]];

      formulaRHS = ReleaseHold[Hold[formula] /. Plus -> List][[2]];

      If[Length[Intersection[Keys[aColumnNames], formulaRHS]] < Length[formulaRHS],
        Message[CrossTensorate::mcnames]; Return[{}]
      ];

      formulaRHS = aColumnNames /@ formulaRHS;
      idRules = Table[(t = Union[data[[All, i]]];Dispatch@Thread[t -> Range[Length[t]]]), {i, formulaRHS}];
      Which[
        TrueQ[formulaLHS === Count],
        t = SparseArray[
          Map[MapThread[Replace, {#[[1]], idRules}] -> #[[2]] &,
            Tally[data[[All, formulaRHS]]]]],
        IntegerQ[formulaLHS],
        t = SparseArray[
          Map[MapThread[Replace, {#[[1]], idRules}] -> #[[2]] &,
            Map[{#[[1, 1 ;; -2]], Total[#[[All, -1]]]} &,
              GatherBy[data[[All, Append[formulaRHS, formulaLHS]]], Most]]]],
        True,
        Message[CrossTensorate::wargs]; Return[{}]
      ];
      Join[<|"XTABTensor" -> t|>, AssociationThread[ Keys[aColumnNames][[formulaRHS]] -> Map[Normal[#][[All, 1]] &, idRules]]]
    ] /; (AssociationQ[columnNames] || ListQ[columnNames] || TrueQ[columnNames === Automatic]);


ClearAll[CrossTensorateSplit]
CrossTensorateSplit::nvar = "The second argument is expected to be a key in the first.";
CrossTensorateSplit[varName_] := CrossTensorateSplit[#, varName] &;
CrossTensorateSplit[xtens_Association, varName_] :=
    Block[{aVars = KeyDrop[xtens, "XTABTensor"], varInd, perm},
      If[! (MemberQ[Keys[xtens], varName] && (varName != "XTABTensor")),
        Message[CrossTensorateSplit::nvar]; Return[{}]
      ];
      varInd = Position[Keys[xtens], varName][[1, 1]] - 1;
      perm = Range[2, Length[aVars]];
      perm = Join[perm[[1 ;; varInd - 1]], {1}, perm[[varInd ;; -1]]];
      Association@
          MapThread[
            Rule[#1, Join[<|"XTABTensor" -> #2|>, KeyDrop[aVars, varName]]] &,
            {xtens[varName], # & /@ Transpose[xtens["XTABTensor"], perm]}]
    ];


Clear[CrossTabulate]

CrossTabulate::narr = "The first argument is expected to be an array with two or three columns.
If present the third column is expected to be numerical."

CrossTabulate[ data_Dataset ] :=
    Block[{colKeys},
      colKeys = Normal[ data[[1]] ];
      If[ MatchQ[colKeys, _Association],
        CrossTabulate[ Normal[data[All, Values]] ],
        CrossTabulate[ Normal[data] ]
      ]
    ]/; Length[Dimensions[data]]==2;

CrossTabulate[ arr_?MatrixQ ] :=
    Block[{idRules,t},
      idRules = Table[(t=Union[arr[[All,i]]];Dispatch@Thread[t->Range[Length[t]]]), {i,Min[2,Dimensions[arr][[2]]]}];
      Which[
        Dimensions[arr][[2]]==2,
        t = { SparseArray[ Map[MapThread[Replace,{#[[1]],idRules}] -> #[[2]] &, Tally[arr]]],
          Normal[#][[All,1]]&/@idRules},
        Dimensions[arr][[2]]==3 && VectorQ[arr[[All,3]],NumericQ],
        t = { SparseArray[
          Map[MapThread[Replace, {#[[1]], idRules}] -> #[[2]] &,
            Map[{#[[1, 1 ;; 2]], Total[#[[All, 3]]]} &, GatherBy[arr, Most]]]],
          Normal[#][[All,1]]&/@idRules},
        True,
        Message[CrossTabulate::narr];
        Return[{}]
      ];
      <| "XTABMatrix" -> t[[1]], "RowNames" -> t[[2,1]], "ColumnNames" -> t[[2,2]] |>
    ];

Clear[xtabsViaRLink];
xtabsViaRLink::norlink = "R is not installed.";
xtabsViaRLink[data_?ArrayQ, columnNames : {_String ..}, formula_String, sparse:(False|True):False] :=
    Block[{},
      If[Length[DownValues[RLink`REvaluate]] == 0,
        Message[xtabsViaRLink::norlink];
        Return[$Failed]
      ];
      RLink`RSet["data", Transpose[data]];
      If[ RLink`REvaluate["class(data)"][[1]] == "matrix",
        RLink`REvaluate["dataDF <- as.data.frame( t(data), stringsAsFactors=F )"],
      (*RLink`REvaluate["dataDF <- do.call( rbind.data.frame, data )"]*)
      (*RLink`REvaluate["dataDF <- data.frame( matrix( unlist(data), nrow = " <> ToString[Length[data]] <> ", byrow = T), stringsAsFactors=FALSE)"]*)
        RLink`REvaluate["dataDF <- as.data.frame( data, srtingsAsFactors=F )"]
      ]
      RLink`RSet["columnNames", columnNames];
      RLink`REvaluate["names(dataDF)<-columnNames"];
      RLink`REvaluate["xtabs(" <> formula <> ", dataDF, sparse = " <> If[sparse,"T","F"] <> ")"]
    ];

Clear[FromRXTabsForm];
FromRXTabsForm[rres_RLink`RObject]:=
    Block[{},
      <|"XTABMatrix" -> rres[[1]],
        "RowNames" -> ("dimnames" /. rres[[2, 3]])[[1, 1]],
        "ColumnNames" -> ("dimnames" /. rres[[2, 3]])[[1, 2]]|>
    ] /; (! FreeQ[rres, {"xtabs", "table"}, Infinity]);

Unprotect[Association];

MatrixForm[x_Association /; (KeyExistsQ[x, "XTABMatrix"] || KeyExistsQ[x, "XTABTensor"])] ^:=
    (MatrixForm[#1, TableHeadings -> Rest[{##}]] & @@ x);

MatrixPlot[
  x_Association /; (KeyExistsQ[x, "XTABMatrix"] || KeyExistsQ[x, "XTABTensor"]), opts___] ^:=
    (MatrixPlot[#1,
      Append[{opts}, FrameLabel -> {{Keys[x][[2]], None}, {Keys[x][[3]], None}}]] & @@ x);


Transpose[x_Association /; (KeyExistsQ[x, "XTABMatrix"] || KeyExistsQ[x, "XTABTensor"]),args___] ^:=
    Block[{assoc = x},
      If[ KeyExistsQ[x, "XTABMatrix"],
        assoc["XTABMatrix"] = Transpose[x["XTABMatrix"],args],
        assoc["XTABTensor"] = Transpose[x["XTABTensor"],args]
      ];
      assoc["ColumnNames"] = x["RowNames"];
      assoc["RowNames"] = x["ColumnNames"];
      assoc
    ];

End[] (* `Private` *)

EndPackage[]