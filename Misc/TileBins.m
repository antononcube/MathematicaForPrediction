(*
    Tile bins Mathematica package
    Copyright (C) 2020  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: TileBins *)
(* :Context: TileBins` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-04-18 *)

(* :Package Version: 0.6 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Tile, Rectangle, Binning, Histogram, Polygon, Mathematica, Wolfram Language, WL *)
(* :Discussion:

   # In brief

   This package provides a few functions for rectangular tile binning of 2D data.

   The package functions can have a specified aggregation function applied to binned values.

   # Usage examples

     SeedRandom[2129];
     data = RandomVariate[MultinormalDistribution[{10, 10}, 7 IdentityMatrix[2]], 100];

     TileBins[data, 2, "PolygonKeys" -> False]

     data2 = Map[# -> RandomInteger[{1, 10}] &, data];

     TileBins[data2, 2, "PolygonKeys" -> False]

     TileBins[data2, 6]

     Show[{TileHistogram[data, 2, "AggregationFunction" -> Mean, ColorFunction -> (Opacity[#, Blue] &), PlotRange -> All],
           Graphics[{Red, PointSize[0.01], Point[data], PointSize[0.02], Green,Point[Keys@TileBins[data, 2, "PolygonKeys" -> False]]}]}]

     Show[{TileHistogram[data, 3, "HistogramType" -> 1, ColorFunction -> ColorData["TemperatureMap"], PlotRange -> All],
           Graphics[{Red, PointSize[0.01], Point[data], PointSize[0.02], Green, Point[Keys@TileBins[data, 3, "PolygonKeys" -> False]]}]}]


   # References

   This package reflects the organization and implementation of the package "HextileBins.m":

     https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/HextileBins.m

*)

BeginPackage["TileBins`"];
(* Exported symbols added here with SymbolName::usage *)

TileBins::usage = "TileBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}] bins data into rectangular tiles. \
Returns an association with keys that are polygon objects.
If the option \"PolygonKeys\" is set to False then the keys are rectangle centers.";

TileHistogram::usage = "TileHistogram[data, binSize, {{xmin, xmax}, {ymin, ymax}}] makes a tile histogram.";

TileBinsPlot::usage = "TileBinsPlot[ tileBins_Association ] plots the polygon keys of tileBins \
and colors them according to the values.";

Begin["`Private`"];

(*********************************************************)
(* Support functions                                     *)
(*********************************************************)

(*Clear[ReferenceRectangle];*)
(*ReferenceRectangle[] := {{-1/2, -1/2}, {1/2, -1/2}, {1/2, 1/2}, {-1/2, 1/2}};*)

Clear[ReferenceRectangle];
ReferenceRectangle[] := {{0, 0}, {1, 0}, {1, 1}, {0, 1}};

Clear[TileContaining];
TileContaining[{x_, y_}] := {Floor[x], Floor[y]};

Clear[NearestRectangle];
NearestRectangle[point : {_?NumericQ, _?NumericQ}] := TileContaining[point];

Clear[TransformByVector];
TransformByVector[v_, tr_] := Polygon[TranslationTransform[tr][v]];

Clear[RectangleVertexDistance];
RectangleVertexDistance[binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), factor_?NumericQ ] :=
    Map[ binSize * #&, factor * ReferenceRectangle[] ];

Clear[TileBinDataRulesQ];
TileBinDataRulesQ[d_] :=
    MatchQ[d, (List | Association)[({_?NumericQ, _?NumericQ} -> _?NumericQ) ..]] ||
        MatchQ[d, (List | Association)[({_?NumericQ, _?NumericQ} -> _) ..]];

Clear[TileBinDataQ];
TileBinDataQ[d_] := (MatrixQ[d] && Dimensions[d][[2]] == 2) || TileBinDataRulesQ[d];


(*********************************************************)
(* TileOriginBins                                        *)
(*********************************************************)

Clear[TileOriginBins];

SyntaxInformation[TileOriginBins] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[TileOriginBins] = { "AggregationFunction" -> Total };

TileOriginBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), opts : OptionsPattern[] ] :=
    TileOriginBins[ data, binSize, Automatic, opts ];

TileOriginBins[data_?MatrixQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), Automatic, opts : OptionsPattern[] ] :=
    TileOriginBins[data, binSize, MinMax /@ Transpose[data], opts] /; Dimensions[data][[2]] == 2;

TileOriginBins[data_?MatrixQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{res},
      res = Map[ binSize * NearestRectangle[ # / binSize ]&, data ];
      Association[Rule @@@ Tally[res]]
    ] /; Dimensions[data][[2]] == 2;

TileOriginBins[data_?TileBinDataRulesQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), Automatic, opts : OptionsPattern[] ] :=
    TileOriginBins[ data, binSize, MinMax /@ Transpose[Keys[data]], opts ];

TileOriginBins[data_?TileBinDataRulesQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{aggrFunc},

      aggrFunc = OptionValue[TileOriginBins, "AggregationFunction"];

      GroupBy[Map[(binSize * NearestRectangle[#[[1]] / binSize]) -> #[[2]] &, Normal[data]], #[[1]] &, aggrFunc[#[[All, 2]]] &]
    ];


(*********************************************************)
(* TileCenterBins                                     *)
(*********************************************************)

Clear[TileCenterBins];

SyntaxInformation[TileCenterBins] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[TileCenterBins] = Join[ {"OverlapFactor" -> 1}, Options[TileOriginBins] ];

TileCenterBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), opts : OptionsPattern[] ] :=
    TileCenterBins[data, binSize, Automatic, opts ];

TileCenterBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), Automatic, opts : OptionsPattern[] ] :=
    TilePolygonBins[
      data,
      binSize,
      If[MatrixQ[data], MinMax /@ Transpose[data], MinMax /@ Transpose[Keys[data]]],
      opts
    ];

TileCenterBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{overlapFactor, vh},

      overlapFactor = OptionValue[TilePolygonBins, "OverlapFactor"];

      vh = RectangleVertexDistance[binSize, overlapFactor];

      (* Making a polygon object and then taking its center (mean of coordinates) seems inefficient. *)
      (* But avoids separate parametrization with arbitrary, non-square rectangular tiles. *)
      KeyMap[
        Mean @ PolygonCoordinates @ TransformByVector[vh, #] &,
        TileOriginBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}, FilterRules[{opts}, Options[TileOriginBins]]]
      ]
    ];


(*********************************************************)
(* TilePolygonBins                                    *)
(*********************************************************)

Clear[TilePolygonBins];

SyntaxInformation[TilePolygonBins] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[TilePolygonBins] = Join[ {"OverlapFactor" -> 1}, Options[TileOriginBins] ];

TilePolygonBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), opts : OptionsPattern[] ] :=
    TilePolygonBins[data, binSize, Automatic, opts ];

TilePolygonBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), Automatic, opts : OptionsPattern[] ] :=
    TilePolygonBins[
      data,
      binSize,
      If[MatrixQ[data], MinMax /@ Transpose[data], MinMax /@ Transpose[Keys[data]]],
      opts
    ];

TilePolygonBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{overlapFactor, vh},

      overlapFactor = OptionValue[TilePolygonBins, "OverlapFactor"];

      vh = RectangleVertexDistance[binSize, overlapFactor];

      KeyMap[ TransformByVector[vh, #] &, TileOriginBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}, FilterRules[{opts}, Options[TileOriginBins]]] ]
    ];


(*********************************************************)
(* TileBins                                           *)
(*********************************************************)

Clear[TileBins];

SyntaxInformation[TileBins] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

TileBins::"nargs" = "The first argument is expected to be a numerical matrix or \
an association of 2D coordinates to numeric values. \
The second argument is expected to be a positive number or a pair of positive numbers. \
The third argument is expected to be a range specification, two pairs of numbers, or Automatic.";

TileBins::"nof" = "The value of the option \"OverlapFactor\" is expected to be a positive number.";

Options[TileBins] = { "AggregationFunction" -> Total, "PolygonKeys" -> True, "OverlapFactor" -> 1 };

TileBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), opts : OptionsPattern[] ] :=
    TileBins[data, binSize, Automatic, opts ];

TileBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), Automatic, opts : OptionsPattern[] ] :=
    TileBins[
      data,
      binSize,
      If[MatrixQ[data], MinMax /@ Transpose[data], MinMax /@ Transpose[Keys[data]]],
      opts
    ];

TileBins[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{overlapFactor, polygonKeys},

      overlapFactor = OptionValue[TileBins, "OverlapFactor"];
      If[ ! ( NumberQ[overlapFactor] && overlapFactor > 0 ),
        Message[TileBins::"nof"];
        Return[$Failed]
      ];

      polygonKeys = OptionValue[TileBins, "PolygonKeys"];

      If[ BooleanQ[polygonKeys] && !polygonKeys,
        TileCenterBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}, FilterRules[{opts}, Options[TileOriginBins]]],
        (*ELSE*)
        TilePolygonBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}, FilterRules[{opts}, Options[TilePolygonBins]]]
      ]
    ] /; Apply[ And, Map[ # > 0 &, Flatten[{binSize}] ] ];

TileBins[___] :=
    Block[{},
      Message[TileBins::"nargs"];
      $Failed
    ];


(*********************************************************)
(* TileHistogram                                      *)
(*********************************************************)

Clear[TileHistogram];

SyntaxInformation[TileHistogram] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

TileHistogram::"nargs" = "The first argument is expected to be a numerical matrix or \
an association of 2D coordinates to numeric values. \
The second argument is expected to be a positive number or a pair of positive numbers. \
The third argument is expected to be a range specification, two pairs of numbers, or Automatic.";

TileHistogram::"nof" = "The value of the option \"OverlapFactor\" is expected to be a positive number.";

Options[TileHistogram] =
    Join[
      {
        "AggregationFunction" -> Total,
        "HistogramType" -> "ColoredPolygons",
        "OverlapFactor" -> 1,
        ColorFunction -> (Blend[{Lighter[Blue, 0.99], Darker[Blue, 0.6]}, Sqrt[#]] &)
      },
      Options[Graphics]
    ];

TileHistogram[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), opts : OptionsPattern[]] :=
    TileHistogram[data, binSize, Automatic, opts];

TileHistogram[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), Automatic, opts : OptionsPattern[]] :=
    TileHistogram[data, binSize, If[MatrixQ[data], MinMax /@ Transpose[data], MinMax /@ Transpose[Keys[data]]], opts];

TileHistogram[data_?TileBinDataQ, binSize : ( _?NumericQ | { _?NumericQ, _?NumericQ } ), {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[]] :=

    Block[{cFunc, ptype, overlapFactor, tally, vh},

      cFunc = OptionValue[TileHistogram, ColorFunction];
      If[StringQ[cFunc], cFunc = ColorData[cFunc]];
      If[ TrueQ[cFunc === Automatic], cFunc = (Blend[{Lighter[Blue, 0.99], Darker[Blue, 0.6]}, Sqrt[#]] &) ];

      ptype = OptionValue[TileHistogram, "HistogramType"];

      overlapFactor = OptionValue[TileHistogram, "OverlapFactor"];
      If[ ! ( NumberQ[overlapFactor] && overlapFactor > 0 ),
        Message[TileHistogram::"nof"];
        Return[$Failed]
      ];

      vh = RectangleVertexDistance[binSize, overlapFactor];

      tally = TileOriginBins[ data, binSize, {{xmin, xmax}, {ymin, ymax}}, FilterRules[{opts}, Options[TileOriginBins]] ];
      tally = List @@@ Normal[tally];

      With[{maxTally = Max[Last /@ tally]},
        Graphics[
          Table[
            Which[
              ptype == 1 || ptype == "ColoredPolygons",
              Tooltip[
                {cFunc[Last@tally[[n]] / maxTally], TransformByVector[vh, First@tally[[n]]]},
                Last@tally[[n]]
              ],

              ptype == 2 || ptype == "ProportionalSideSize",
              Tooltip[
                TransformByVector[Last@tally[[n]] / maxTally * vh, First@tally[[n]]],
                Last@tally[[n]]
              ],

              ptype == 3 || ptype == "ProportionalArea",
              Tooltip[
                TransformByVector[Sqrt[Last@tally[[n]] / maxTally] * vh, First@tally[[n]]],
                Last@tally[[n]]
              ]

            ],
            {n, Length@tally}
          ],
          FilterRules[{opts}, Options[Graphics]],
          Frame -> True, PlotRange -> {{xmin, xmax}, {ymin, ymax}}, PlotRangeClipping -> True]
      ]
    ];

TileHistogram[___] :=
    Block[{},
      Message[TileHistogram::"nargs"];
      $Failed
    ];


(*********************************************************)
(* TileHistogramPlot                                     *)
(*********************************************************)

Clear[TileBinsPlot];

SyntaxInformation[TileBinsPlot] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

TileBinsPlot::"nargs" = "The first argument is expected to be an association with keys that are polygons.";

Options[TileBinsPlot] =
    Join[
      {
        ColorFunction -> (Blend[{Lighter[Blue, 0.99], Darker[Blue, 0.6]}, Sqrt[#]] &)
      },
      Options[Graphics]
    ];

TileBinsPlot[ bins : Association[ (_Polygon -> _?NumericQ) ..], opts : OptionsPattern[] ] :=
    Block[{cFunc, tally, maxTally},

      cFunc = OptionValue[TileBinsPlot, ColorFunction];
      If[StringQ[cFunc], cFunc = ColorData[cFunc]];
      If[ TrueQ[cFunc === Automatic], cFunc = (Blend[{Lighter[Blue, 0.99], Darker[Blue, 0.6]}, Sqrt[#]] &) ];

      tally = List @@@ Normal[bins];

      maxTally = Max[Last /@ tally];

      Graphics[
        Map[
          Tooltip[
            {cFunc[Last[#] / maxTally], First[#]},
            Last[#]
          ] &,
          tally
        ],
        FilterRules[{opts}, Options[Graphics]],
        PlotRange -> All, Frame -> True, PlotRangeClipping -> True]
    ];

TileBinsPlot[___] :=
    Block[{},
      Message[TileBinsPlot::"nargs"];
      $Failed
    ];


End[]; (* `Private` *)

EndPackage[]