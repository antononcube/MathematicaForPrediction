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

SyntaxInformation[ParallelCoordinatesPlot] = { "ArgumentsPattern" -> {_, _., _., OptionsPattern[]} };

ParallelCoordinatesPlot::args = "The expected arguments are \
(1) a matrix or an association of matrices, \
(2) column names, and \
(3) minmax pairs. \
The number of the column names should agree with the number of columns in the first argument.";

ParallelCoordinatesPlot::optao = "The value of the option \"AxesOrder\" is expected to be \
a list of indexes with the same length as the number of columns in the first argument, or Automatic, or Random.";

ParallelCoordinatesPlot::optc = "The value of the option \"Colors\" is expected to be \
an association with keys that correspond to the keys of the first argument, or Automatic, or Random.";

ParallelCoordinatesPlot::optlo = "The value of the option \"LabelsOffset\" is expected to be a number.";

Options[ParallelCoordinatesPlot] =
    Join[
      {"Colors" -> Automatic, "AxesOrder" -> Automatic, Direction-> "Horizontal", "LabelsOffset" -> Automatic, PlotStyle -> Automatic},
      Options[Graphics]
    ];

ParallelCoordinatesPlot[data_?MatrixQ, opts : OptionsPattern[]] :=
    ParallelCoordinatesPlot[data, Automatic, Automatic, opts];

ParallelCoordinatesPlot[data_?MatrixQ, colNames_, opts : OptionsPattern[]] :=
    ParallelCoordinatesPlot[data, colNames, Automatic, opts];

ParallelCoordinatesPlot[data_?MatrixQ, Automatic, minMaxes_, opts : OptionsPattern[]] :=
    ParallelCoordinatesPlot[data, Range[Length[data[[1]]]], minMaxes, opts];

ParallelCoordinatesPlot[data_?MatrixQ, colNames_, Automatic, opts : OptionsPattern[]] :=
    ParallelCoordinatesPlot[data, colNames, MinMax /@ Transpose[data], opts];

ParallelCoordinatesPlot[data_?MatrixQ, colNames_List, minMaxes_?MatrixQ, opts : OptionsPattern[]] :=
    Block[{pstyle, horizontalQ, lblOff, divisions, data2, grBase, grid, xs, n = 5, c = 0.05, dirFunc = Identity },

      horizontalQ = ! TrueQ[ MemberQ[ {"Vertical", "FromAbove"}, OptionValue[ParallelCoordinatesPlot, Direction] ] ];

      lblOff = OptionValue[ParallelCoordinatesPlot, "LabelsOffset"];
      Which[
        TrueQ[lblOff === Automatic] && horizontalQ, lblOff = 3,
        TrueQ[lblOff === Automatic] && !horizontalQ, lblOff = -0.1,
        !NumberQ[lblOff],
        Message[ParallelCoordinatesPlot::optlo];
        Return[$Failed]
      ];

      pstyle = OptionValue[ParallelCoordinatesPlot, PlotStyle];
      If[ TrueQ[pstyle === Automatic], pstyle = Nothing ];

      divisions = FindDivisions[#, n] & /@ minMaxes;
      data2 = Transpose[MapThread[Rescale[#1, #2, {0, 1}] &, {Transpose[data], MinMax /@ divisions}]];
      xs = Range[Length[data[[1]]]];
(*      grBase = ListLinePlot[data2, opts, Axes -> False];*)

      dirFunc = If[ horizontalQ, Identity, Reverse];

      grBase = Graphics[ { Sequence @@ Flatten[{pstyle}], Line[ dirFunc /@ Transpose[{Range[Length[data2[[1]]]], #1}]]& /@ data2}, FilterRules[{opts}, Options[Graphics]], Axes -> False ];

      grid =
          Graphics[{
            Line[ dirFunc /@ {{#, 0}, {#, 1}}] & /@ xs,
            MapThread[
              Function[{x, ds},
                MapThread[{Line[dirFunc /@ {{x - c, #2}, {x + c, #2}}],
                  Text[#1, dirFunc @ {x - c, #2}, dirFunc @ {2, 0}]} &, {N@ds, Rescale[ds]}]
              ],
              {xs, divisions}],
            If[ horizontalQ,
              MapThread[Text[#2, {#1, 0}, {Center, lblOff}] &, {xs, colNames}],
              MapThread[Text[#2, {lblOff, #1}, {Right, Center}] &, {xs, colNames}]
            ]
          }];
      Show[grBase, grid]
    ] /; MatrixQ[data, NumberQ] && MatrixQ[minMaxes, NumberQ] && Dimensions[minMaxes] == {Dimensions[data][[2]], 2};

(*-----------------------------------------------------------*)

ParallelCoordinatesPlot[aData : Association[ (_ -> _?MatrixQ) .. ], opts : OptionsPattern[] ] :=
    ParallelCoordinatesPlot[aData, Automatic, Automatic, opts ];

ParallelCoordinatesPlot[aData : Association[ (_ -> _?MatrixQ) .. ], Automatic, opts : OptionsPattern[] ] :=
    ParallelCoordinatesPlot[aData, Automatic, Automatic, opts ];

ParallelCoordinatesPlot[aData : Association[ (_ -> _?MatrixQ) .. ], colNames_List, opts : OptionsPattern[] ] :=
    ParallelCoordinatesPlot[aData, colNames, Automatic, opts ];

ParallelCoordinatesPlot[aData_Association, colNamesArg : ( Automatic | _List ), Automatic, opts : OptionsPattern[]] :=
    Block[{colNames = colNamesArg, minMaxes, cols, axesOrder, pstyle, grs},

      If[ TrueQ[colNames === Automatic],
        colNames = Range@Length@Values[aData][[1, 1]]
      ];

      cols = OptionValue[ParallelCoordinatesPlot, "Colors"];
      axesOrder = OptionValue[ParallelCoordinatesPlot, "AxesOrder"];

      pstyle = OptionValue[ParallelCoordinatesPlot, PlotStyle];
      If[ TrueQ[pstyle === Automatic], pstyle = {} ];
      pstyle = Flatten[{pstyle}];

      Which[

        TrueQ[cols === Automatic],
        cols = AssociationThread[Keys[aData], ColorData["Pastel", "ColorFunction"] /@ Rescale[Range[Length[aData]]]],

        TrueQ[cols === Random],
        cols = AssociationThread[Keys[aData], RandomSample[ColorData[11, "ColorList"], Length[aData]]]
      ];

      If[! (AssociationQ[cols] && Length[Intersection[Keys[cols], Keys[aData]]] == Length[aData]),
        Message[ParallelCoordinatesPlot::optc];
        Return[$Failed]
      ];
      cols = cols /@ Keys[aData];

      Which[
        TrueQ[axesOrder === Automatic],
        axesOrder = Range[Dimensions[aData[[1]]][[2]]],

        TrueQ[axesOrder === Random],
        axesOrder = RandomSample[Range[Dimensions[aData[[1]]][[2]]]],

        !( VectorQ[axesOrder, IntegerQ] && Length[axesOrder] == Dimensions[aData[[1]]][[2]] && Range[Length[axesOrder]] == Sort[axesOrder] ),
        Message[ParallelCoordinatesPlot::optao];
        Return[$Failed];
      ];

      minMaxes = MinMax /@ Transpose[Join @@ Values[aData]];
      grs =
          MapThread[
            ParallelCoordinatesPlot[
              #1[[All, axesOrder]],
              colNames[[axesOrder]],
              minMaxes[[axesOrder]],
              PlotStyle -> Prepend[pstyle, #2], opts] &,
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