(*
    Hextile bins Mathematica package
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
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: HextileBins *)
(* :Context: HextileBins` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-29 *)

(* :Package Version: 0.4 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Hextile, Hexagon, Binning, Histogram, Polygon, Mathematica, Wolfram Language, WL *)
(* :Discussion:

   # In brief

   This package provides a few functions for hex-tile binning of 2D data.

   The package functions can have a specified aggregation function applied to binned values.

   # Usage examples

     SeedRandom[2129];
     data = RandomVariate[MultinormalDistribution[{10, 10}, 7 IdentityMatrix[2]], 100];

     HextileCenterBins[data, 2]

     data2 = Map[# -> RandomInteger[{1, 10}] &, data];

     HextileCenterBins[data2, 2]

     HextileBins[data2, 6]

     Show[{HextileHistogram[data, 2, "AggregationFunction" -> Mean, ColorFunction -> (Opacity[#, Blue] &), PlotRange -> All],
           Graphics[{Red, PointSize[0.01], Point[data], PointSize[0.02], Green,Point[Keys@HextileCenterBins[data, 2]]}]}]

     Show[{HextileHistogram[data, 3, "HistogramType" -> 3, ColorFunction -> ColorData["TemperatureMap"], PlotRange -> All],
           Graphics[{Red, PointSize[0.01], Point[data], PointSize[0.02], Green, Point[Keys@HextileCenterBins[data, 3]]}]}]


   # References

     Initial ideas / versions of the code in this package can be found in Mathematica Stack Exchange:

       https://mathematica.stackexchange.com/q/28149


*)

BeginPackage["HextileBins`"];
(* Exported symbols added here with SymbolName::usage *)

HextileCenterBins::usage = "HextileCenterBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}] returns and association with \
keys that are hexagon centers.";

HextileBins::usage = "HextileBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}] returns and association with \
keys that are polygon objects (hexagons.)";

HextileHistogram::usage = "HextileHistogram[data, binSize, {{xmin, xmax}, {ymin, ymax}}] makes a hextile histogram";


Begin["`Private`"];

(*********************************************************)
(* Support functions                                     *)
(*********************************************************)

Clear[ReferenceHexagon];
ReferenceHexagon[] := {{1 / 2, Sqrt[3] / 2}, {-(1 / 2), Sqrt[3] / 2}, {-1, 0}, {-(1 / 2), -(Sqrt[3] / 2)}, {1 / 2, -(Sqrt[3] / 2)}, {1, 0}};

Clear[NearestWithinTile];
NearestWithinTile = Nearest[{{0, 0}, {1, 0}, {1 / 2, Sqrt[3] / 2}, {0, Sqrt[3]}, {1, Sqrt[3]}}];

Clear[TileContaining];
TileContaining[{x_, y_}] := {Floor[x], Sqrt[3] Floor[y / Sqrt[3]]};

Clear[NearestHexagon];
NearestHexagon[point : {_?NumberQ, _?NumberQ}] :=
    Module[{tile, relative},
      tile = TileContaining[point];
      relative = point - tile;
      tile + First@NearestWithinTile[relative]
    ];

Clear[TransformByVector];
TransformByVector[v_, tr_] := Polygon[TranslationTransform[tr][RotationTransform[Pi / 2][v]]];

Clear[HexagonVertexDistance];
HexagonVertexDistance[binSize_?NumberQ] :=
    binSize * 1.05 * ReferenceHexagon[] / Sqrt[3];

Clear[HextileBinDataRulesQ];
HextileBinDataRulesQ[d_] := MatchQ[d, (List | Association)[({_?NumberQ, _?NumberQ} -> _?NumberQ) ..]];

Clear[HextileBinDataQ];
HextileBinDataQ[d_] := (MatrixQ[d] && Dimensions[d][[2]] == 2) || HextileBinDataRulesQ[d];


(*********************************************************)
(* HextileCenterBins                                     *)
(*********************************************************)

Clear[HextileCenterBins];

SyntaxInformation[HextileCenterBins] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[HextileCenterBins] = { "AggregationFunction" -> Total };

HextileCenterBins[data_?HextileBinDataQ, binSize_, opts : OptionsPattern[] ] :=
    HextileCenterBins[ data, binSize, Automatic, opts ];

HextileCenterBins[data_?MatrixQ, binSize_, Automatic, opts : OptionsPattern[] ] :=
    HextileCenterBins[data, binSize, MinMax /@ Transpose[data], opts] /; Dimensions[data][[2]] == 2;

HextileCenterBins[data_?MatrixQ, binSize_, {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{},
      Association[Rule @@@ Tally[binSize * (NearestHexagon /@ (data / binSize))]]
    ] /; Dimensions[data][[2]] == 2;

HextileCenterBins[data_?HextileBinDataRulesQ, binSize_, Automatic, opts : OptionsPattern[] ] :=
    HextileCenterBins[ data, binSize, MinMax /@ Transpose[Keys[data]], opts ];

HextileCenterBins[data_?HextileBinDataRulesQ, binSize_, {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{aggrFunc},

      aggrFunc = OptionValue[HextileCenterBins, "AggregationFunction"];

      GroupBy[Map[(binSize * NearestHexagon[#[[1]] / binSize]) -> #[[2]] &, Normal[data]], #[[1]] &, aggrFunc[#[[All, 2]]] &]
    ];


(*********************************************************)
(* HextileBin                                            *)
(*********************************************************)

Clear[HextileBins];

SyntaxInformation[HextileBins] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[HextileBins] = Options[HextileCenterBins];

HextileBins[data_?HextileBinDataQ, binSize_, opts : OptionsPattern[] ] :=
    HextileBins[data, binSize, Automatic, opts ];

HextileBins[data_?HextileBinDataQ, binSize_, Automatic, opts : OptionsPattern[] ] :=
    HextileBins[
      data,
      binSize,
      If[MatrixQ[data], MinMax /@ Transpose[data], MinMax /@ Transpose[Keys[data]]],
      opts
    ];

HextileBins[data_?HextileBinDataQ, binSize_, {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[] ] :=
    Block[{vh = HexagonVertexDistance[binSize]},
      KeyMap[ TransformByVector[vh, #] &, HextileCenterBins[data, binSize, {{xmin, xmax}, {ymin, ymax}}, opts] ]
    ];


(*********************************************************)
(* HextileHistogram                                      *)
(*********************************************************)

Clear[HextileHistogram];

SyntaxInformation[HextileHistogram] = { "ArgumentsPattern" -> { _, _, _., OptionsPattern[] } };

Options[HextileHistogram] =
    Join[
      {
        "AggregationFunction" -> Total,
        "HistogramType" -> "ColoredPolygons",
        ColorFunction -> (Blend[{Lighter[Blue, 0.99], Darker[Blue, 0.6]}, #] &)
      },
      Options[Graphics]
    ];

HextileHistogram[data_?HextileBinDataQ, binSize_?NumericQ, opts : OptionsPattern[]] :=
    HextileHistogram[data, binSize, Automatic, opts];

HextileHistogram[data_?HextileBinDataQ, binSize_?NumericQ, Automatic, opts : OptionsPattern[]] :=
    HextileHistogram[data, binSize, If[MatrixQ[data], MinMax /@ Transpose[data], MinMax /@ Transpose[Keys[data]]], opts];

HextileHistogram[data_?HextileBinDataQ, binSize_?NumericQ, {{xmin_, xmax_}, {ymin_, ymax_}}, opts : OptionsPattern[]] :=

    Block[{cFunc, ptype, tally, vh = HexagonVertexDistance[binSize]},

      cFunc = OptionValue[HextileHistogram, ColorFunction];
      If[StringQ[cFunc], cFunc = ColorData[cFunc]];

      ptype = OptionValue[HextileHistogram, "HistogramType"];

      tally = HextileCenterBins[ data, binSize, {{xmin, xmax}, {ymin, ymax}}, FilterRules[{opts}, Options[HextileCenterBins]] ];
      tally = List @@@ Normal[tally];

      With[{maxTally = Max[Last /@ tally]},
        Graphics[
          Table[
            Which[
              ptype == 1 || ptype == "ColoredPolygons",
              Tooltip[
                {cFunc[Sqrt[Last@tally[[n]] / maxTally]], TransformByVector[vh, First@tally[[n]]]},
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


End[]; (* `Private` *)

EndPackage[]