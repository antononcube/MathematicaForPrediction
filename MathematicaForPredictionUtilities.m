
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

BeginPackage["MathematicaForPredictionUtilities`"]

ClassificationSuccessTableForm::usage = "Turns classification success rate rules into and array and applys TableForm to it."

ClassificationSuccessGrid::usage = "Turns classification success rate rules into and array and applys Grid to it."

NumericVectorSummary::usage = "Summary of a numerical vector."

CategoricalVectorSummary::usage = "Summary of a categorical vector."

DataColumnsSummary::usage = "Summary of a list of data columns."

RecordsSummary::usage = "Summary of a list of records that form a full two dimensional array."

GridTableForm::usage = "GridTableForm[listOfList, TableHeadings->headings] mimics TableForm by using Grid \
(and producing fancier outlook)."

ParetoLawPlot::usage = "ParetoLawPlot[data,opts] makes a list plot for the manifestation of the Pareto law. \
It has the same signature and options as ListPlot."

IntervalMappingFunction::usage = "IntervalMappingFunction[boundaries] makes a piece-wise function for mapping of \
a real value to the enumerated intervals Partition[Join[{-Infinity}, boundaries, {Infinity}], 2, 1]."

VariableDependenceGrid::usage = "VariableDependenceGrid[data_?MatrixQ,columnNames,opts] makes a grid with \
variable dependence plots."

ExcessKurtosis::usage = "ExcessKurtosis[d] computes the excess kurtosis for d (which is Kurtosis[d]-3)."

KurtosisUpperBound::usage = "KurtosisUpperBound[vec_?VectorQ] computes the upper bound of the kurtosis of vec. \
KurtosisUpperBound[d_,n_Integer] computes the upper bound of the kurtosis of a sample of size n from \
the distribution d."

CrossTabulate::usage = "Finds the contigency co-occurance values in a full array."

xtabsViaRLink::usage = "Calling R's function xtabs {stats} via RLink`."

Begin["`Private`"]

Needs["MosaicPlot`"]

Clear[KurtosisUpperBound, ExcessKurtosis]

ExcessKurtosis[d_] := Kurtosis[d] - 3;

KurtosisUpperBound[vec_?VectorQ] :=
    Block[{n = Length[vec]},
      1/2 (n - 3)/(n - 2) (CentralMoment[vec, 3]/StandardDeviation[vec]^3)^2 + n/2];

KurtosisUpperBound[d_,n_Integer] :=
    Block[{}, 1/2 (n - 3)/(n - 2) (CentralMoment[d, 3]/StandardDeviation[d]^3)^2 + n/2];

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

Clear[NumericVectorSummary, CategoricalVectorSummary]
NumericVectorSummary[dvec_] :=
  Block[{r},
    r = Flatten[Through[{Min, Max, Mean, Quartiles}[DeleteMissing[dvec]]]];
    SortBy[Transpose[{{"Min", "Max", "Mean", "1st Qu", "Median", "3rd Qu"}, r}], #[[2]] &]
  ] /; VectorQ[DeleteMissing[dvec], NumberQ];
CategoricalVectorSummary[dvec_, maxTallies_Integer: 7] :=
  Block[{r},
    r = SortBy[Tally[dvec], -#[[2]] &];
    If[Length[r] <= maxTallies, r,
     Join[r[[1 ;; maxTallies - 1]], {{"(Other)", Total[r[[maxTallies ;; -1, 2]]]}}]
    ]
  ] /; VectorQ[dvec];

Clear[DataColumnsSummary]
Options[DataColumnsSummary] = {"MaxTallies" -> 7, "NumberedColumns" -> True};
DataColumnsSummary[dataColumns_, opts : OptionsPattern[]] := 
  DataColumnsSummary[dataColumns, Table["column " <> ToString[i], {i, 1, Length[dataColumns]}], opts];
DataColumnsSummary[dataColumns_, columnNamesArg_, opts : OptionsPattern[]] :=
  Block[{columnTypes, columnNames = columnNamesArg, 
     maxTallies = OptionValue[DataColumnsSummary, "MaxTallies"], 
     numberedColumnsQ = TrueQ[OptionValue[DataColumnsSummary, "NumberedColumns"]]},
    If[numberedColumnsQ,
     columnNames = MapIndexed[ToString[#2[[1]]] <> " " <> #1 &, columnNames]
    ];
    columnTypes = Map[If[NumberQ[#], Number, Symbol] &, dataColumns[[All, 1]]];
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
RecordsSummary[dataRecords_, opts : OptionsPattern[]] := 
  DataColumnsSummary[Transpose[dataRecords], opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );
RecordsSummary[dataRecords_, columnNames_, opts : OptionsPattern[]] :=
  DataColumnsSummary[Transpose[dataRecords], columnNames, opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );
RecordsSummary[dataRecords_, args___ ] :=
  RecordsSummary[ List /@ dataRecords, args ] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 1 );
RecordsSummary[___] := (Message[RecordsSummary::arrdepth];$Failed);

Clear[GridTableForm]
Options[GridTableForm] = {TableHeadings -> None};
GridTableForm[data_, opts : OptionsPattern[]] :=
  Block[{gridData, gridHeadings, dataVecQ=False},
   gridHeadings = OptionValue[GridTableForm, TableHeadings];
   gridData = data;
   If[VectorQ[data], dataVecQ = True; gridData=List@data ];
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
   Grid[gridData, Alignment -> Left, 
    Dividers -> {Join[{1 -> Black, 2 -> Black},
       Thread[Range[3, Length[gridData[[2]]] + 1] -> GrayLevel[0.8]], {Length[gridData[[2]]] + 1 -> Black}], {True, True, {False}, True}}, 
    Background -> {Automatic, Flatten[Table[{White, GrayLevel[0.96]}, {Length[gridData]/2}]]}]
  ];


Clear[ParetoLawPlot]
Options[ParetoLawPlot] = Options[ListPlot];
ParetoLawPlot[dataVec : {_?NumberQ ..}, opts : OptionsPattern[]] := ParetoLawPlot[{Tooltip[dataVec, 1]}, opts];
ParetoLawPlot[dataVecs : {{_?NumberQ ..} ..}, opts : OptionsPattern[]] :=
    ParetoLawPlot[MapThread[Tooltip, {dataVecs, Range[Length[dataVecs]]}], opts];
ParetoLawPlot[dataVecs : {Tooltip[{_?NumberQ ..}, _] ..}, opts : OptionsPattern[]] :=
    Block[{t, mc = 0.5},
      t = Map[
        Tooltip[(Accumulate[#]/Total[#] &)[SortBy[#[[1]], -# &]], #[[2]]] &,
        dataVecs];
      ListPlot[t, opts, PlotRange -> All,
        GridLines -> {Length[t[[1, 1]]] Range[0.1, mc, 0.1], {0.8}},
        Frame -> True,
        FrameTicks -> {{Automatic, Automatic}, {Automatic,
          Table[{Length[t[[1, 1]]] c, ToString[Round[100 c]] <> "%"}, {c,
            Range[0.1, mc, 0.1]}]}}]
    ];

Clear[IntervalMappingFunction]
IntervalMappingFunction[qBoundaries : {_?NumberQ ...}] :=
    Block[{XXX, t = Partition[Join[{-\[Infinity]}, qBoundaries, {\[Infinity]}], 2, 1]},
      Function[
        Evaluate[Piecewise[
          MapThread[{#2, #1[[1]] < XXX <= #1[[2]]} &, {t,
            Range[1, Length[t]]}]] /. {XXX -> #}]]
    ];

Clear[VariableDependenceGrid]
Options[VariableDependenceGrid] = {"IgnoreCategoricalVariables" -> False};
VariableDependenceGrid[data_?MatrixQ, opts : OptionsPattern[]] :=
    VariableDependenceGrid[ data, Range[Dimensions[data][[2]]], opts];
VariableDependenceGrid[data_?MatrixQ, columnNamesArg_, opts : OptionsPattern[]] :=
    Block[{varTypes, grs, ninds, ddata, columnNames = columnNamesArg },
      varTypes = Map[VectorQ[DeleteMissing[#], NumericQ] &, Transpose[data]];

      If[ Length[columnNames] < Dimensions[data][[2]],
        AppendTo[ columnNames, Length[columnNames] + Range[Dimensions[data][[2]]-Length[columnNames]]]
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
            MosaicPlot[data[[All, #]], "LabelRotation"->{{1,4},{4,1}}, ColorRules -> {1 -> ColorData[7, "ColorList"]}],

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

Clear[CrossTabulate]

CrossTabulate::narr = "The first argument is expected to be an array with two or three columns.
If present the third column is expected to be numerical."

CrossTabulate[ arr_?ArrayQ ] :=
    Block[{idRules,t},
      idRules = Table[(t=Union[arr[[All,i]]];Dispatch@Thread[t->Range[Length[t]]]), {i,Min[2,Dimensions[arr][[2]]]}];
      Which[
        Dimensions[arr][[2]]==2,
        { SparseArray[ Map[MapThread[Replace,{#[[1]],idRules}] -> #[[2]] &, Tally[arr]]],
          Normal[#][[All,1]]&/@idRules},
        Dimensions[arr][[2]]==3 && VectorQ[arr[[All,3]],NumericQ],
        { SparseArray[
          Map[MapThread[Replace, {#[[1]], idRules}] -> #[[2]] &,
            Map[{#[[1, 1 ;; 2]], Total[#[[All, 3]]]} &, GatherBy[arr, Most]]]],
          Normal[#][[All,1]]&/@idRules},
        True,
        Message[CrossTabulate::narr];
        {}
      ]
    ];

Clear[xtabsViaRLink]
xtabsViaRLink::norlink = "R is not installed.";
xtabsViaRLink[data_?ArrayQ, columnNames : {_String ..}, formula_String] :=
    Block[{rres},
      If[Length[DownValues[RLink`REvaluate]] == 0,
        Message[xtabsViaRLink::norlink];
        Return[$Failed]
      ];
      RLink`RSet["dataColumns", Transpose[data]];
      RLink`REvaluate[
        "dataDF<-as.data.frame(dataColumns,stringsAsFactors=FALSE)"];
      RLink`RSet["columnNames", columnNames];
      RLink`REvaluate["names(dataDF)<-columnNames"];
      rres = RLink`REvaluate["tdf <- xtabs(" <> formula <> ", dataDF)"];
      <|"matrix" -> rres[[1]],
        "rownames" -> ("dimnames" /. rres[[2, 3]])[[1, 1]],
        "colnames" -> ("dimnames" /. rres[[2, 3]])[[1, 2]]|>
    ];

End[]

EndPackage[]