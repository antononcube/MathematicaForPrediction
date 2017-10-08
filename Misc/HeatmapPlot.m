(*
    Heatmap plot Mathematica package
    Copyright (C) 2017  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: HeatmapPlot *)
(* :Context: HeatmapPlot` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-10-06 *)

(* :Package Version: 0.5 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

   This package provides a heatmap plot with hierarchical clustering reordering of the rows and columns.
   
   The rows and columns can be clustered with different distance and linkage functions.

   The resulting heatmap has tooltips over the matrix elements: click on the plot and press "." .

   The dendrograms of the obtained clusters are not drawn by default because I don't like how the plot looks.

   # Usage examples

      HeatmapPlot[Transpose@ExampleData[{"Statistics", "EmployeeAttitude"}]]

      HeatmapPlot[Transpose@ExampleData[{"Statistics", "EmployeeAttitude"}],
         DistanceFunction -> {EuclideanDistance, CosineDistance}]

   Anton Antonov
   2017-10-06

*)

BeginPackage["HeatmapPlot`"]

MatrixPlotWithTooltips::usage = "MatrixPlotWithTooltips[data, rowNames, columnNames] makes a MatrixPlot with tooltips."

HeatmapPlot::usage = "HeatmapPlot[data_?MatrixQ, rowNames_List, columnNames_List] makes a heat-map plot based on MatrixPlot \
and HierarchicalClustering`Agglomerate ."

Begin["`Private`"]

Needs["HierarchicalClustering`"]


MatrixPlotWithTooltips[mat_, rowNames_, columnNames_, opts : OptionsPattern[]] :=
    With[{dims = Dimensions[mat],

      indFunc = {Clip[Floor[#1[[1]] - #2[[2]]] + 1, {1, #1[[1]]}], Clip[Floor[#2[[1]]] + 1, {1, #1[[2]]}]} &},

      With[{
        copiedvalues = "CopiedValueFunction" ->
          Function[pt,
            {indFunc[dims, pt], Extract[mat, indFunc[dims, pt]]}],
        coordtooltips = "DisplayFunction" ->
            Function[pt,
              Column[{
                rowNames[[indFunc[dims, pt][[1]]]],
                columnNames[[indFunc[dims, pt][[2]]]],
                Extract[mat, indFunc[dims, pt]]},
                Background -> White, ImageSize -> {Automatic, 30},
                ImageMargins -> {{5, 5}, {10, 10}}, Alignment -> Center]] },
        MatrixPlot[mat, opts, CoordinatesToolOptions -> {coordtooltips, copiedvalues}]
      ]

    ];


Clear[HeatmapPlot]

Options[HeatmapPlot] =
    Join[
      { DistanceFunction -> {Automatic,Automatic}, Linkage -> {Automatic, Automatic}, Dendrogram -> False },
      Options[MatrixPlot] ];


(*
HeatmapPlot[data:_[Association[__]], opts:OptionsPattern[]] :=
    Block[{},
      HeatmapPlot[ SparseArray[data], RSparseMatrix`RowNames[data], RSparseMatrix`ColumnNames[data], opts ]
    ] /; SymbolName[Head[data]] == "RSparseMatrix";
*)

HeatmapPlot[xtabs_Association, opts:OptionsPattern[]] :=
    Block[{},
      HeatmapPlot[ xtabs["XTABMatrix"], xtabs["RowNames"], xtabs["ColumnNames"], opts ]
    ] /; KeyExistsQ[xtabs, "XTABMatrix"];

HeatmapPlot[data_?MatrixQ, opts:OptionsPattern[]] :=
    HeatmapPlot[ data, Range[Dimensions[data][[1]]], Range[Dimensions[data][[2]]], opts];

HeatmapPlot[data_?MatrixQ, {}|Automatic, {}|Automatic, opts:OptionsPattern[]] :=
    HeatmapPlot[ data, Range[Dimensions[data][[1]]], Range[Dimensions[data][[2]]], opts ];

HeatmapPlot[data_?MatrixQ, rowNames_List, columnNames_List, opts:OptionsPattern[]] :=
    Block[{distFunction, linkFunction, dendrogramQs,
      rowClustering, columnClustering, rowReordering, columnReordering,
      dendrogramRight = {}, dendrogramTop = {},
      rowNameTicks, columnNameTicks, mat, tOpts},

      mat = If[ TrueQ[Head[data] === SparseArray], Normal[data], data];

      distFunction = OptionValue[HeatmapPlot, DistanceFunction];
      If[Length[distFunction] < 2, distFunction = Flatten@{distFunction, distFunction}];

      linkFunction = OptionValue[HeatmapPlot, Linkage];
      If[Length[linkFunction] < 2, linkFunction = Flatten@{linkFunction, linkFunction}];

      dendrogramQs = OptionValue[HeatmapPlot, Dendrogram];
      If[Length[dendrogramQs] < 2, dendrogramQs = Flatten@{dendrogramQs, dendrogramQs}];
      dendrogramQs = dendrogramQs /. None->False;
      dendrogramQs = TrueQ /@ dendrogramQs;

      If[ distFunction[[1]] === Automatic, distFunction[[1]] = EuclideanDistance ];
      If[ distFunction[[2]] === Automatic, distFunction[[2]] = EuclideanDistance ];

      Which[
        TrueQ[distFunction[[1]] === None],
        rowReordering = Range[Dimensions[mat][[1]]],

        TrueQ[distFunction[[1]] === Sort],
        rowReordering = Ordering[mat],

        True,
        rowClustering =
            HierarchicalClustering`Agglomerate[mat -> Range[Length[mat]],
              DistanceFunction -> distFunction[[1]],
              Linkage -> linkFunction[[1]]];
        rowReordering = HierarchicalClustering`ClusterFlatten[rowClustering];

        If[ dendrogramQs[[1]],
          dendrogramRight = HierarchicalClustering`DendrogramPlot[rowClustering, Orientation -> Right]
        ];
      ];

      Which[
        TrueQ[distFunction[[2]] === None],
        columnReordering = Range[Dimensions[mat][[2]]],

        TrueQ[distFunction[[2]] === Sort],
        columnReordering = Ordering[Transpose[mat]],

        True,
        columnClustering =
            HierarchicalClustering`Agglomerate[
              Transpose@mat -> Range[Dimensions[mat][[2]]],
              DistanceFunction -> distFunction[[2]],
              Linkage -> linkFunction[[2]]];
        columnReordering = HierarchicalClustering`ClusterFlatten[columnClustering];

        If[ dendrogramQs[[2]],
          dendrogramTop = HierarchicalClustering`DendrogramPlot[columnClustering, Orientation -> Top]
        ];
      ];


      rowNameTicks =
          Table[{i, rowNames[[rowReordering[[i]]]], {0, 0}}, {i, 1, Length[rowReordering]}];
      columnNameTicks =
          Table[{i, Rotate[#, Pi/2] & @ columnNames[[columnReordering[[i]]]], {0, 0}}, {i, 1, Length[columnReordering]}];

      mat = mat[[rowReordering,columnReordering]];

      tOpts = DeleteCases[ {opts}, (DistanceFunction|Linkage|Dendrogram) -> ___ ];

      Show[{
        MatrixPlotWithTooltips[mat, rowNames[[rowReordering]], columnNames[[columnReordering]],
          tOpts,
          FrameStyle -> AbsoluteThickness[0],
          FrameTicks -> {{rowNameTicks, None}, {columnNameTicks, None}},
          BaseStyle -> {FontSize -> 10},
          ColorFunction -> Automatic,
          ColorFunctionScaling -> True
        ],
        If[ TrueQ[dendrogramRight === {}], {},
          Graphics[{GrayLevel[0.6],
            Translate[
              Translate[dendrogramRight[[1]], {Dimensions[mat][[2]], 0}], {0, -1 / 2}]}]
        ],
        If[ TrueQ[dendrogramTop === {}], {},
          Graphics[{GrayLevel[0.6],
            Translate[Translate[dendrogramTop[[1]], {0, Dimensions[mat][[1]]}], {-1 / 2, 0}]}]
        ]
      }]
    ] /; Dimensions[data] == { Length[rowNames], Length[columnNames] };

End[] (* `Private` *)

EndPackage[]
