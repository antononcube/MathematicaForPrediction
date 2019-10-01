(* Mathematica Package *)
(* Created with the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ . *)

(* :Title: ParallelCoordinatesPlot *)
(* :Context: ParallelCoordinatesPlot` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-09-30 *)

(* :Package Version: 0.8 *)
(* :Mathematica Version: 12 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: parallel coordinates, data analysis, plot *)
(* :Discussion:

   # In brief

     This package provides the function ParallelCoordinatesPlot that does Parallel coordinates plots. (See [1]).

     From [1]:

       Parallel coordinates are a common way of visualizing high-dimensional geometry and analyzing
       multivariate data.

       To show a set of points in an n-dimensional space, a backdrop is drawn consisting of n parallel
       lines, typically vertical and equally spaced. A point in n-dimensional space is represented
       as a polyline with vertices on the parallel axes; the position of the vertex on the i-th axis
       corresponds to the i-th coordinate of the point.

   # Usage

      data = ExampleData[{"Statistics", "FisherIris"}];
      colNames = ExampleData[{"Statistics", "FisherIris"}, "ColumnDescriptions"]

      aData = GroupBy[data, #[[-1]] &, #[[All, 1 ;; -2]] &];
      
      ParallelCoordinatesPlot[aData, Most[colNames]]

   # References

   [1] Wikipedia entry, Parallel coordinates,
       URL: https://en.wikipedia.org/wiki/Parallel_coordinates .

   [2] Anton Antonov, "How to plot Parallel Coordinates?" answer, (2019), MathematicaStackExchange.
       URL: https://mathematica.stackexchange.com/a/207059/34008 .
*)

BeginPackage["ParallelCoordinatesPlot`"];

ParallelCoordinatesPlot::usage = "ParallelCoordinatesPlot[ data : ( _?MatrixQ | <| ( _ -> _?MatrixQ ).. |>), colNames_List, opts___ ] \
makes a parallel plot for a numerical matrix or an association of numerical matrices.";

Begin["`Private`"];

Clear[ParallelCoordinatesPlot];

ParallelCoordinatesPlot::args = "The expected arguments are association, column names, and minmax pairs.";

ParallelCoordinatesPlot::copt = "The value of the option \"Colors\" is expected to be an \
association with keys that correspond to the keys of the first argument.";

Options[ParallelCoordinatesPlot] =
    Join[
      {"Colors" -> Automatic, "AxesOrder" -> Automatic, Direction-> "Horizontal", PlotStyle -> Automatic},
      Options[Graphics]
    ];

ParallelCoordinatesPlot[data_?MatrixQ, opts : OptionsPattern[]] :=
    ParallelCoordinatesPlot[data, Range[Length[data[[1]]]],
      MinMax /@ Transpose[data], opts];

ParallelCoordinatesPlot[data_?MatrixQ, colNames_List, opts : OptionsPattern[]] :=
    ParallelCoordinatesPlot[data, colNames, MinMax /@ Transpose[data], opts];

ParallelCoordinatesPlot[data_?MatrixQ, colNames_List, minMaxes_?MatrixQ, opts : OptionsPattern[]] :=
    Block[{divisions, data2, grBase, grid, xs, n = 5, c = 0.05, pstyle},

      pstyle = OptionValue[ParallelCoordinatesPlot, PlotStyle];
      If[ TrueQ[pstyle === Automatic], pstyle = Nothing ];

      divisions = FindDivisions[#, n] & /@ minMaxes;
      data2 = Transpose[MapThread[Rescale[#1, #2, {0, 1}] &, {Transpose[data], MinMax /@ divisions}]];
      xs = Range[Length[data[[1]]]];
(*      grBase = ListLinePlot[data2, opts, Axes -> False];*)
      grBase = Graphics[ { Sequence @@ Flatten[{pstyle}], Line[Transpose[{Range[Length[data2[[1]]]], #1}]]& /@ data2}, FilterRules[{opts}, Options[Graphics]], Axes -> False ];
      grid =
          Graphics[{
            Line[{{#, 0}, {#, 1}}] & /@ xs,
            MapThread[
              Function[{x, ds},
                MapThread[{Line[{{x - c, #2}, {x + c, #2}}],
                  Text[#1, {x - c, #2}, {2, 0}]} &, {N@ds, Rescale[ds]}]
              ],
              {xs, divisions}],
            MapThread[Text[#2, {#1, 0}, {0, 3}] &, {xs, colNames}]
          }];
      Show[grBase, grid]
    ] /; MatrixQ[data, NumberQ] && MatrixQ[minMaxes, NumberQ] && Dimensions[minMaxes] == {Dimensions[data][[2]], 2};

ParallelCoordinatesPlot[aData_Association, colNames_List, opts : OptionsPattern[]] :=
    Block[{minMaxes, cols, axesOrder, grs},

      cols = OptionValue[ParallelCoordinatesPlot, "Colors"];
      axesOrder = OptionValue[ParallelCoordinatesPlot, "AxesOrder"];

      Which[

        TrueQ[cols === Automatic],
        cols = AssociationThread[Keys[aData], ColorData["Pastel", "ColorFunction"] /@ Rescale[Range[Length[aData]]]],

        TrueQ[cols === Random],
        cols = AssociationThread[Keys[aData], RandomSample[ColorData[11, "ColorList"], Length[aData]]]
      ];

      If[! (AssociationQ[cols] && Length[Intersection[Keys[cols], Keys[aData]]] == Length[aData]),
        Message[ParallelCoordinatesPlot::copt];
        Return[$Failed]
      ];
      cols = cols /@ Keys[aData];

      Which[
        TrueQ[axesOrder === Automatic],
        axesOrder = Range[Dimensions[aData[[1]]][[2]]],

        TrueQ[axesOrder === Random],
        axesOrder = RandomSample[Range[Dimensions[aData[[1]]][[2]]]]
      ];

      minMaxes = MinMax /@ Transpose[Join @@ Values[aData]];
      grs =
          MapThread[
            ParallelCoordinatesPlot[
              #1[[All, axesOrder]],
              colNames[[axesOrder]],
              minMaxes[[axesOrder]],
              PlotStyle -> #2, FilterRules[{opts}, Options[ListLinePlot]]] &,
            {Values@aData, cols}
          ];
      Legended[Show[grs], LineLegend[cols, Keys[aData]]]
    ] /; MatrixQ[Join @@ Values[aData], NumberQ];

ParallelCoordinatesPlot[___] :=
    Block[{},
      Message[ParallelCoordinatesPlot::args];
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]