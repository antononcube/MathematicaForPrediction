
(*
    MathematicaForPrediction utilities
    Copyright (C) 2014  Anton Antonov

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
	antononcube@gmail.com, 
	7320 Colbury Ave, 
	Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2014 Wolfram Research, Inc.

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

GridTableForm::usage = "GridTableForm[listOfList, TableHeadings->headings] mimics TableForm by using Grid (and producing fancier outlook)."

Begin["`Private`"]

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
    r = Flatten[Through[{Min, Max, Mean, Quartiles}[dvec]]];
    SortBy[Transpose[{{"Min", "Max", "Mean", "1st Qu", "Median", "3rd Qu"}, r}], #[[2]] &]
  ] /; VectorQ[dvec, NumberQ];
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

RecordsSummary::arrdepth = "The first argument is expected to be a full array of depth 2."
	
Clear[RecordsSummary];
RecordsSummary[dataRecords_, opts : OptionsPattern[]] := 
  DataColumnsSummary[Transpose[dataRecords], opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );
RecordsSummary[dataRecords_, columnNames_, opts : OptionsPattern[]] :=
  DataColumnsSummary[Transpose[dataRecords], columnNames, opts] /; ( ArrayQ[dataRecords] && ArrayDepth[dataRecords] == 2 );
RecordsSummary[___] := (Message[RecordsSummary::arrdepth];$Failed);

Clear[GridTableForm]
Options[GridTableForm] = {TableHeadings -> None};
GridTableForm[data_, opts : OptionsPattern[]] :=
  Block[{gridData, gridHeadings},
   gridHeadings = OptionValue[GridTableForm, TableHeadings];
   gridData = data;
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
   Grid[gridData, Alignment -> Left, 
    Dividers -> {Join[{1 -> Black, 2 -> Black}, 
       Thread[Range[3, Length[gridData[[2]]] + 1] -> GrayLevel[0.8]], {Length[gridData[[2]]] + 1 -> Black}], {True, True, {False}, True}}, 
    Background -> {Automatic, Flatten[Table[{White, GrayLevel[0.96]}, {Length[gridData]/2}]]}]
  ];

End[]

EndPackage[]