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

If[Length[DownValues[MosaicPlot`MosaicPlot]] == 0,
  Echo["MosaicPlot.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MosaicPlot.m"]
];

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Echo["CrossTabulate.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];


BeginPackage["MathematicaForPredictionUtilities`"];

ClassificationSuccessTableForm::usage = "Turns classification success rate rules into and array and applys TableForm to it.";

ClassificationSuccessGrid::usage = "Turns classification success rate rules into and array and applys Grid to it.";

NumericVectorSummary::usage = "Summary of a numerical vector.";

CategoricalVectorSummary::usage = "Summary of a categorical vector.";

DataColumnsSummary::usage = "Summary of a list of data columns.";

RecordsSummary::usage = "Summary of a list of records that form a full two dimensional array.";

GridTableForm::usage = "GridTableForm[listOfList, TableHeadings->headings] mimics TableForm by using Grid \
(and producing fancier outlook).";

ParetoLawPlot::usage = "ParetoLawPlot[data,opts] makes a list plot for the manifestation of the Pareto law. \
It has the same signature and options as ListPlot. \
The argument data is expected to be a numerical a vector or a list of numerical vectors.";

IntervalMappingFunction::usage = "IntervalMappingFunction[boundaries] makes a piece-wise function for mapping of \
a real value to the enumerated intervals Partition[Join[{-Infinity}, boundaries, {Infinity}], 2, 1].";

ToCategoricalColumns::usage = "ToCategoricalColumns[data_?ArrayQ, qs_: Range[0, 1, 0.2]] \
converts the numerical columns of an array to categorical. (Using IntervalMappingFunction.)";

VariableDependenceGrid::usage = "VariableDependenceGrid[data_?MatrixQ,columnNames,opts] makes a grid with \
variable dependence plots.";

ExcessKurtosis::usage = "ExcessKurtosis[d] computes the excess kurtosis for d (which is Kurtosis[d]-3).";

KurtosisUpperBound::usage = "KurtosisUpperBound[vec_?VectorQ] computes the upper bound of the kurtosis of vec. \
KurtosisUpperBound[d_,n_Integer] computes the upper bound of the kurtosis of a sample of size n from \
the distribution d.";

GridOfCodeAndComments::usage = "GridOfCodeAndComments[code_String, opts___] tabulates code and comments. \
The tabulation function is specified with the option \"GridFunction\".";

DataRulesForClassifyQ::usage = "Checks is the argument is a list of item->label rules that can be used by Classify.";

DataArrayRulesForClassifyQ::usage = "Checks is the argument is a list of record->label rules that can be used by Classify. \
All records should form an array.";

ImportCSVToDataset::usage = "Imports a CSV file and attempts to convert into a Dataset object.";

DatasetColumnNumericQ::usage = "DatasetColumnNumericQ[ds] Returns Vector[#,NumericQ]& over the columns of ds \
after removing missing and NA values.";

ToAutomaticKeysAssociation::usage = "ToAutomaticKeysAssociation[ls_List, prefix_String] makes an association with \
automatically derived keys.";

Begin["`Private`"];

Needs["MosaicPlot`"];
Needs["CrossTabulate`"];

Clear[KurtosisUpperBound, ExcessKurtosis]

ExcessKurtosis[d_] := Kurtosis[d] - 3;

KurtosisUpperBound[vec_?VectorQ] :=
    Block[{n = Length[vec]},
      1 / 2 (n - 3) / (n - 2) (CentralMoment[vec, 3] / StandardDeviation[vec]^3)^2 + n / 2];

KurtosisUpperBound[d_, n_Integer] :=
    Block[{}, 1 / 2 (n - 3) / (n - 2) (CentralMoment[d, 3] / StandardDeviation[d]^3)^2 + n / 2];

Clear[ClassificationSuccessTableForm]
ClassificationSuccessTableForm[ctRules_] :=
    Block[{labels = Union[ctRules[[All, 1, 1]]]},
      TableForm[
        Normal[SparseArray[
          ctRules /.
              Join[Thread[labels -> Range[Length[labels]]], {True -> 1, False -> 2, All -> Length[labels] + 1}]]],
        TableHeadings -> {labels, {True, False}}]
    ];

Clear[ClassificationSuccessGrid]
ClassificationSuccessGrid[ctRules_] :=
    Block[{labels = Union[ctRules[[All, 1, 1]]], gridData},
      gridData =
          Normal[SparseArray[
            ctRules /.
                Join[Thread[labels -> Range[Length[labels]]], {True -> 1, False -> 2, All -> Length[labels] + 1}]]];
      gridData = Prepend[MapThread[Prepend, {gridData, labels}], {"", True, False}];
      Grid[gridData, Alignment -> Left,
        Dividers -> {{2 -> GrayLevel[0.5]}, {2 -> GrayLevel[0.5]}},
        Spacings -> {2, Automatic}]
    ];

Clear[DataRulesForClassifyQ]
DataRulesForClassifyQ[data_] := MatchQ[data, {Rule[_?AtomQ, _] ..}] || DataArrayRulesForClassifyQ[data];

Clear[DataArrayRulesForClassifyQ]
DataArrayRulesForClassifyQ[data_] := MatchQ[data, {Rule[_List, _] ..}] && ArrayQ[data[[All, 1]]];


Clear[NumericVectorSummary, CategoricalVectorSummary]
NumericVectorSummary[dvec_] :=
    Block[{r, cm},
      r = Flatten[Through[{Min, Max, Mean, Quartiles}[DeleteMissing[dvec]]]] /. x_Rational :> N[x];
      r = SortBy[Transpose[{{"Min", "Max", "Mean", "1st Qu", "Median", "3rd Qu"}, DeleteMissing[r]}], #[[2]] &];
      cm = Count[dvec, Missing[___]];
      If[ TrueQ[cm > 0], Append[r, { "Missing[___]", cm}], r ]
    ] /; VectorQ[DeleteMissing[dvec], NumberQ];
CategoricalVectorSummary[dvec_, maxTallies_Integer : 7] :=
    Block[{r},
      r = SortBy[Tally[dvec], -#[[2]] &];
      If[Length[r] <= maxTallies, r,
        Join[r[[1 ;; maxTallies - 1]], {{"(Other)", Total[r[[maxTallies ;; -1, 2]]]}}]
      ]
    ] /; VectorQ[dvec];

Clear[DataColumnsSummary]
(* The option Thread->False is just for compatibility with RecordsSummary. *)
Options[DataColumnsSummary] = {"MaxTallies" -> 7, "NumberedColumns" -> True, Thread -> False};
DataColumnsSummary[dataColumns_, opts : OptionsPattern[]] :=
    DataColumnsSummary[dataColumns, Table["column " <> ToString[i], {i, 1, Length[dataColumns]}], opts];
DataColumnsSummary[dataColumns_, columnNamesArg_, opts : OptionsPattern[]] :=
    Block[{columnTypes, columnNames = columnNamesArg,
      maxTallies = OptionValue[DataColumnsSummary, "MaxTallies"],
      numberedColumnsQ = TrueQ[OptionValue[DataColumnsSummary, "NumberedColumns"]]},
      If[numberedColumnsQ,
        columnNames = MapIndexed[ToString[#2[[1]]] <> " " <> #1 &, columnNames]
      ];
      columnTypes = Map[If[VectorQ[DeleteMissing[#], NumberQ], Number, Symbol] &, dataColumns];
      MapThread[
        Column[{
          Style[#1, Blue, FontFamily -> "Times"],
          If[TrueQ[#2 === Number],
            Grid[NumericVectorSummary[#3], Alignment -> Left],
            Grid[CategoricalVectorSummary[#3, maxTallies],
              Alignment -> Left]
          ]}] &, {columnNames, columnTypes, dataColumns}, 1]
    ] /; Length[dataColumns] == Length[columnNamesArg];

RecordsSummary::arrdepth = "The first argument is expected to be a full array of depth 1 or 2."

Clear[RecordsSummary];

RecordsSummary[{}, ___] := {};

RecordsSummary[dataRecords_Dataset, opts : OptionsPattern[] ] :=
    Block[{row1, colKeys, records},

      row1 = Normal[dataRecords[1]];
      If[ MatchQ[row1, _Association],
        colKeys = Keys[row1];
        ,
        colKeys = Table["", Length[row1]]
      ];

      Which[
        MatchQ[row1, _Association],
        records = Normal[dataRecords[All, Values]];
        If[ MatchQ[records, _Association], records = Values[records] ];
        RecordsSummary[ Normal[records], colKeys, opts ],

        True,
        records = Normal[dataRecords];
        If[ MatchQ[records, _Association], records = Values[records] ];
        RecordsSummary[ records, colKeys, opts ]
      ]
    ];

RecordsSummary[dataRecords_, opts : OptionsPattern[]] :=
    DataColumnsSummary[Transpose[dataRecords], opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );

RecordsSummary[dataRecords_, columnNames_, opts : OptionsPattern[]] :=
    DataColumnsSummary[Transpose[dataRecords], columnNames, opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );

RecordsSummary[dataRecords_?DataRulesForClassifyQ, varNames_Rule, opts : OptionsPattern[]] :=
    Block[{newArgs = {opts}},
      newArgs = DeleteCases[newArgs, Rule[Thread, __] ];
      Rule @@
          MapThread[
            RecordsSummary[#1, #2, newArgs] &,
            {Transpose[List @@@ dataRecords], Map[Flatten@*List, List @@ varNames]}
          ]
    ] /; DataRulesForClassifyQ[List[varNames]] && MemberQ[{opts}, Thread -> True ];

RecordsSummary[dataRecords_?DataRulesForClassifyQ, args___] :=
    Block[{newArgs = {args}},
      newArgs = DeleteCases[newArgs, Rule[Thread, __] ];
      Rule @@ Map[RecordsSummary[#, newArgs]&, Transpose[List @@@ dataRecords]] /; MemberQ[{args}, Thread -> True ]
    ];

RecordsSummary[dataRecords_, args___ ] :=
    RecordsSummary[ List /@ dataRecords, args ] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 1 && Length[dataRecords] > 0);

RecordsSummary[a_Association, args___] := Map[ RecordsSummary[#, args]&, a];

RecordsSummary[___] := (Message[RecordsSummary::arrdepth];$Failed);


Clear[GridTableForm]
Options[GridTableForm] = Join[ {TableHeadings -> None}, Options[Grid] ];
GridTableForm[data_, opts : OptionsPattern[]] :=
    Block[{gridData, gridHeadings, dataVecQ = False},
      gridHeadings = OptionValue[GridTableForm, TableHeadings];
      gridData = data;
      If[VectorQ[data], dataVecQ = True; gridData = List@data ];
      gridData = Map[Join[#, Table["", {Max[Length /@ gridData] - Length[#]}]] &, gridData];
      gridData = MapIndexed[Prepend[#1, #2[[1]]] &, gridData];
      If[gridHeadings === None || ! ListQ[gridHeadings],
        gridHeadings = Join[{"#"}, Range[1, Length[gridData[[1]]] - 1]],
        (*ELSE*)
        gridHeadings = Join[{"#"}, gridHeadings];
      ];
      gridHeadings = Map[Style[#, Blue, FontFamily -> "Times"] &, gridHeadings];
      If[Length[gridHeadings] < Length[gridData[[1]]],
        gridHeadings = Append[gridHeadings, SpanFromLeft];
      ];
      gridData = Prepend[gridData, gridHeadings];
      (*If[dataVecQ, gridData = Transpose[gridData] ];*)
      Grid[gridData,
        DeleteCases[ {opts}, (TableHeadings -> _)],
        Alignment -> Left,
        Dividers -> {Join[{1 -> Black, 2 -> Black},
          Thread[Range[3, Length[gridData[[2]]] + 1] -> GrayLevel[0.8]], {Length[gridData[[2]]] + 1 -> Black}], {True, True, {False}, True}},
        Background -> {Automatic, Flatten[Table[{White, GrayLevel[0.96]}, {Length[gridData] / 2}]]}]
    ];


Clear[ParetoLawPlot]
Options[ParetoLawPlot] = Options[ListPlot];
ParetoLawPlot[dataVec : {_?NumberQ ..}, opts : OptionsPattern[]] := ParetoLawPlot[{Tooltip[dataVec, 1]}, opts];
ParetoLawPlot[dataVecs : {{_?NumberQ ..} ..}, opts : OptionsPattern[]] :=
    ParetoLawPlot[MapThread[Tooltip, {dataVecs, Range[Length[dataVecs]]}], opts];
ParetoLawPlot[dataVecs : {Tooltip[{_?NumberQ ..}, _] ..}, opts : OptionsPattern[]] :=
    Block[{t, mc = 0.5},
      t = Map[ Tooltip[(Accumulate[#] / Total[#] &)[SortBy[#[[1]], -# &]], #[[2]]] &, dataVecs];
      ListPlot[t, opts, PlotRange -> All,
        GridLines -> {Length[t[[1, 1]]] Range[0.1, mc, 0.1], {0.8}},
        Frame -> True,
        FrameTicks -> {{Automatic, Automatic}, {Automatic, Table[{Length[t[[1, 1]]] c, ToString[Round[100 c]] <> "%"}, {c, Range[0.1, mc, 0.1]}]}}]
    ];

Clear[IntervalMappingFunction]
IntervalMappingFunction[qBoundaries : {_?NumberQ ...}] :=
    Block[{XXX, t = Partition[Join[{-\[Infinity]}, qBoundaries, {\[Infinity]}], 2, 1]},
      Function[
        Evaluate[Piecewise[
          MapThread[{#2, #1[[1]] < XXX <= #1[[2]]} &, {t, Range[1, Length[t]]}]] /. {XXX -> #}]]
    ];

(***********************************************************)
(* ToCategoricalColumns                                  *)
(***********************************************************)

Clear[ToCategoricalColumns];

ToCategoricalColumns::mslen = "The second argument is expected to be a numerical vector or \
a list of numerical vectors. When a lists of numerical vectors then the length of that list \
is expected to be equal to the number of numerical columns of the first argument.";

Options[ToCategoricalColumns] = { "QuantileBreaks" -> False };

ToCategoricalColumns[data_?ArrayQ, breaks_List : Range[0, 1, 0.1], opts:OptionsPattern[] ] :=
    Block[{inds, imFuncs, res, quantileBreaksQ},

      quantileBreaksQ = TrueQ[ OptionValue[ ToCategoricalColumns, "QuantileBreaks" ] ];

      inds =
          Pick[Range[Dimensions[data][[2]]],
            VectorQ[#, NumericQ] & /@ Transpose[Take[data, UpTo[12]]]];

      Which[
        quantileBreaksQ && VectorQ[ breaks, NumericQ ],
        imFuncs =
            IntervalMappingFunction /@ (Quantile[DeleteMissing[#], breaks] & /@ Transpose[data[[All, inds]]]),

        quantileBreaksQ && Length[breaks] == Length[inds] && Apply[ And, VectorQ[ #, NumericQ ]& /@ breaks],
        imFuncs = MapIndexed[ IntervalMappingFunction[ Quantile[  DeleteMissing[ data[[All, #1]] ], breaks[[ #2[[1]] ]] ] ] &, inds ],

        VectorQ[ breaks, NumericQ],
        imFuncs = Table[ IntervalMappingFunction[breaks], Length[inds] ],

        Length[breaks] == Length[inds] && Apply[ And, VectorQ[ #, NumericQ ]& /@ breaks ],
        imFuncs = Map[ IntervalMappingFunction, breaks ],

        True,
        Message[ToCategoricalColumns::mslen];
        Return[$Failed]
      ];

      res = data;
      Do[res[[All, inds[[i]]]] = res[[All, inds[[i]]]] /. x_?NumericQ :> imFuncs[[i]][x], {i, Length[inds]}];

      res
    ] /; Length[Dimensions[data]] == 2;

ToCategoricalColumns[ds_Dataset, breaks_List : Range[0, 1, 0.1], opts:OptionsPattern[] ] :=
    Block[{aNumColsQ, numCols, imFuncs, quantileBreaksQ},

      quantileBreaksQ = TrueQ[ OptionValue[ ToCategoricalColumns, "QuantileBreaks" ] ];

      aNumColsQ =
          Normal @ ds[Transpose /* Query[All, VectorQ[DeleteMissing[#], NumericQ] &]];

      numCols = Keys[Pick[aNumColsQ, Values[aNumColsQ]]];

      Which[
        quantileBreaksQ && VectorQ[ breaks, NumericQ ],
        imFuncs = IntervalMappingFunction /@ (Quantile[DeleteMissing[#], breaks] & /@ Transpose[ds[All, numCols]]);
        imFuncs = Normal @ imFuncs,

        quantileBreaksQ && Length[breaks] == Length[numCols] && Apply[ And, VectorQ[ #, NumericQ ]& /@ breaks],
        imFuncs = Association @ MapIndexed[ #1 -> IntervalMappingFunction[ Quantile[ Normal @ DeleteMissing[ ds[All, #1] ], breaks[[ #2[[1]] ]] ] ] &, numCols ],

        VectorQ[ breaks, NumericQ],
        imFuncs = AssociationThread[ numCols -> Table[ IntervalMappingFunction[breaks], Length[numCols]] ],

        Length[breaks] == Length[numCols] && Apply[ And, VectorQ[ #, NumericQ ]& /@ breaks ],
        imFuncs = AssociationThread[ numCols -> Map[ IntervalMappingFunction, breaks ] ],

        True,
        Message[ToCategoricalColumns::mslen];
        Return[$Failed]
      ];

      Fold[
        Function[{d, k}, d[All, <|#, k -> (Slot[k] /. y_?NumericQ :> (imFuncs[k])[y])|> &]],
        ds,
        Keys[imFuncs]
      ]
    ];


(***********************************************************)
(* VariableDependenceGrid                                  *)
(***********************************************************)

Clear[VariableDependenceGrid];
Options[VariableDependenceGrid] = {"IgnoreCategoricalVariables" -> False};
VariableDependenceGrid[data_Dataset, args___] :=
    Block[{colKeys},
      colKeys = Normal[ data[[1]] ];
      If[ MatchQ[colKeys, _Association],
        VariableDependenceGrid[ Normal[data[All, Values]], Keys[colKeys], args ],
        VariableDependenceGrid[ Normal[data], args ]
      ]
    ];
VariableDependenceGrid[data_?MatrixQ, opts : OptionsPattern[]] :=
    VariableDependenceGrid[ data, Range[Dimensions[data][[2]]], opts];
VariableDependenceGrid[data_?MatrixQ, columnNamesArg_, opts : OptionsPattern[]] :=
    Block[{varTypes, grs, ninds, ddata, columnNames = columnNamesArg },
      varTypes = Map[VectorQ[DeleteMissing[#], NumericQ] &, Transpose[data]];

      If[ Length[columnNames] < Dimensions[data][[2]],
        AppendTo[ columnNames, Length[columnNames] + Range[Dimensions[data][[2]] - Length[columnNames]]]
      ];

      ninds = Range[Dimensions[data][[2]]];
      If[TrueQ[OptionValue["IgnoreCategoricalVariables"]],
        ninds = Pick[Range[Dimensions[data][[2]]], varTypes];
      ];

      grs =
          Which[
            (SameQ @@ #) && (! varTypes[[#[[1]]]]), columnNames[[#[[1]]]],

            (SameQ @@ #) && (varTypes[[#[[1]]]]),
            Histogram[data[[All, #[[1]]]], Automatic, "Probability",
              PlotTheme -> "Detailed",
              PlotLabel -> Style[columnNames[[#[[1]]]], "FontSize" -> 14]],

            TrueQ[varTypes[[#[[1]]]] && varTypes[[#[[2]]]]],
            ListPlot[{data[[All, #]]}, PlotStyle -> {PointSize[0.01]},
              PlotRange -> All, AspectRatio -> 1, Frame -> True],

            TrueQ[! varTypes[[#[[1]]]] && ! varTypes[[#[[2]]]]],
            MosaicPlot[data[[All, #]], "LabelRotation" -> {{1, 4}, {4, 1}}, ColorRules -> {1 -> ColorData[7, "ColorList"]}],

            TrueQ[varTypes[[#[[1]]]] && ! varTypes[[#[[2]]]]],
            ddata = Map[Prepend[#[[All, 1]], #[[1, -1]]] &, GatherBy[data[[All, #]], Last]];
            DistributionChart[ddata[[All, 2 ;; -1]],
              ChartLabels -> {ddata[[All, 1]]},
              ChartElementFunction -> "DensityQuantile",
              BarOrigin -> Bottom, AspectRatio -> 1],

            TrueQ[! varTypes[[#[[1]]]] && varTypes[[#[[2]]]]],
            ddata = Map[Prepend[#[[All, 1]], #[[1, -1]]] &, GatherBy[data[[All, Reverse@#]], Last]];
            DistributionChart[ddata[[All, 2 ;; -1]],
              ChartLabels -> ddata[[All, 1]], ChartStyle -> 54, BarOrigin -> Left],

            True, ""
          ] & /@ Flatten[Outer[List, ninds, ninds], 1];

      Grid[ArrayReshape[grs, {Length[ninds], Length[ninds]}], Dividers -> All]
    ];


(***********************************************************)
(* GridOfCodeAndComments                                   *)
(***********************************************************)

ClearAll[GridOfCodeAndComments];
Options[GridOfCodeAndComments] = {"GridFunction" -> (Grid[#, Alignment -> Left] &)};
GridOfCodeAndComments[code_String, opts : OptionsPattern[]] :=
    Block[{grData, codeLines, commentLines, comPat, gridFunc},
      gridFunc = OptionValue["GridFunction"];
      If[TrueQ[gridFunc === Automatic],
        gridFunc = (Grid[#, Alignment -> Left] &)];

      (* Split the code into lines *)
      codeLines = StringSplit[code, "\n"];

      (* Split each line into a {code, comment} pair *)
      comPat = ("(*" ~~ ___ ~~ "*)");
      grData =
          Map[
            If[StringFreeQ[#, "(*"], {#, ""},
              StringCases[#, (x__ ~~ y : (comPat) ~~ z___) :> {x <> z, y}][[1]]
            ] &, codeLines];

      (* Style the code and comments *)
      grData[[All, 1]] = Map[Style[#, "Input"] &, grData[[All, 1]]];
      grData[[All, 2]] =
          Map[Style[#,
            "CommentStyle" /. Options[$FrontEnd, AutoStyleOptions][[1, 2]]] &,
            grData[[All, 2]]];

      (* Show result *)
      gridFunc[grData]
    ];


(***********************************************************)
(* Import CSV files into Dataset objects                   *)
(***********************************************************)

Clear[ImportCSVToDataset];
Options[ImportCSVToDataset] = {"RowNames" -> False, "ColumnNames" -> True};
ImportCSVToDataset[fname_String, opts : OptionsPattern[]] :=
    Block[{data},
      data = Import[fname];
      If[OptionValue["ColumnNames"],
        data = Dataset[Dataset[Rest[data]][All, AssociationThread[First[data], #] &]],
        (*ELSE*)
        data = Dataset[data]
      ];
      If[OptionValue["RowNames"],
        data = Dataset[AssociationThread[Normal[data[All, First]], Normal[data[All, Rest]]]]
      ];
      data
    ];

(* The pattern handling is not general enough: it is only for strings. *)
Clear[DatasetColumnNumericQ];
Options[DatasetColumnNumericQ] = { "NotAvailablePattern" -> ( "" | "NA" | "Null" | "None"), IgnoreCase -> True};
DatasetColumnNumericQ[data_Dataset, opts : OptionsPattern[]] :=
    Block[{naPattern = OptionValue["NotAvailablePattern"], ignoreCase = OptionValue[IgnoreCase]},
      Transpose[data][All, VectorQ[DeleteCases[DeleteMissing[#], (x_String /; StringMatchQ[x, naPattern, IgnoreCase -> ignoreCase])], NumericQ] &]
    ];


(***********************************************************)
(* Automatic keys Associations                             *)
(***********************************************************)

(* We use nd-1 because that includes the decimal point. *)
Clear[ToIDString];
ToIDString[i_Integer, nd_Integer] := ToString[NumberForm[i, {nd - 1, 0}, NumberPadding -> {"0", ""}]];

Clear[ToAutomaticKeysAssociation];
ToAutomaticKeysAssociation[ ls_List, prefix_String : "id." ] :=
    AssociationThread[ Map[ prefix <> ToIDString[#, Ceiling[Log10[Length[ls]]] + 1] &, Range[Length[ls]]], ls ];

End[];

EndPackage[]