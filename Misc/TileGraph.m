(*
    Tile graph Mathematica package
    Copyright (C) 2022  Anton Antonov

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
  	ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
  	Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2022 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: TileGraph *)
(* :Context: TileGraph` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-05-01 *)

(* :Package Version: 0.8 *)
(* :Mathematica Version: 13.0 *)
(* :Copyright: (c) 2022 Anton Antonov *)
(* :Keywords: Tile, Rectangle, Binning, Histogram, Graph, Mathematica, Wolfram Language, WL *)
(* :Discussion:

   # In brief

   This package has a function that makes graphs that correspond to rectangular tile binning.

   It is a modified version of the package HextileGraph.m :
    https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/HextileGraph.m
*)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[TileGraph`TileBins]] == 0,
  Echo["TileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/TileBins.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["TileGraph`"];

TileGraph::usage = "TileGraph[ aLonLatValue, cellRadius, opts] \
makes a rectangular tiling graph for specified data.
(This is a \"legacy\" function; half of it relies on a undocumented features of GeoHistogram.)";

Begin["`Private`"];

Needs["TileBins`"];

(***********************************************************)
(* TileGraph                                               *)
(***********************************************************)

Clear[TileGraph];

TileGraph::"nbm" = "The value of the option \"BinMethod\" is expected to be one of \"TileGraph\" or \"GeoHistogram\".";

TileGraph::"mr" = "If the value of the option \"BinMethod\" is `1` then the second argument is expected to be `2`.";

Options[TileGraph] :=
    Join[
      {"BinMethod" -> "TileGraph", "RemoveLoneCells" -> False},
      Options[TileBins],
      Options[GeoHistogram],
      Options[NearestNeighborGraph]
    ];

TileGraph[
  aLonLatValue : Association[({_?NumberQ, _?NumberQ} -> _?NumberQ) ..],
  cellRadius : (_?NumberQ | _Quantity),
  opts : OptionsPattern[] ] :=

    Block[{binMethod, removeLoneCellsQ, aPolygonValues, lsCells, aCells,
      nc, lsDistances, pos, grSquareCellsNetwork, grSquareCells},

      binMethod = OptionValue[TileGraph, "BinMethod"];
      removeLoneCellsQ = TrueQ[OptionValue[TileGraph, "RemoveLoneCells"]];

      Which[
        ToLowerCase["TileGraph"] == ToLowerCase[binMethod] && NumberQ[cellRadius],
        aPolygonValues = TileBins[aLonLatValue, cellRadius, FilterRules[{opts}, Options[TileBins]]],

        ToLowerCase["TileGraph"] == ToLowerCase[binMethod] && !NumberQ[cellRadius],
        Message[TileGraph::"mr", "\"TileGraph\"", "a number"];
        Return[$Failed],

        True,
        Message[TileGraph::"nbm"];
        Return[$Failed]
      ];

      (* Make cell objects *)

      lsCells = KeyValueMap[<|"Value" -> #2, "Cell" -> #1, "Center" -> Mean[PolygonCoordinates[#1]]|> &, aPolygonValues];
      lsCells = SortBy[lsCells, #["Center"] &];
      aCells = AssociationThread[Range[Length[lsCells]], lsCells];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Create a function to find the nearest cell to a given position *)
      nc = Nearest[Values[aCells] -> Keys[aCells], DistanceFunction -> (EuclideanDistance[#1["Center"], #2["Center"]] &)];

      lsDistances = Select[Flatten@DistanceMatrix[Values[#["Center"] & /@ aCells]], # > 0 &];

      (* Identify outlier(s) and drop them *)
      If[removeLoneCellsQ,
        pos = Select[nc[#, {8, 1.1 * Min[lsDistances] * Sqrt[2]}] & /@ aCells, Length[#] == 1 &];
        aCells = KeyDrop[aCells, Keys[pos]];
      ];

      (* Reassign cell ID's *)
      aCells = AssociationThread[Range[Length[aCells]], Values[aCells]];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Make neighbors graph *)
      grSquareCellsNetwork =
          NearestNeighborGraph[
            Keys[aCells], {9, Min[lsDistances] * Sqrt[2]},
            DistanceFunction -> (EuclideanDistance[aCells[#1]["Center"], aCells[#2]["Center"]] &),
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[NearestNeighborGraph]]
          ];

      (* Make final graph *)
      grSquareCells =
          Graph[
            DirectedEdge @@@
                Join[
                  EdgeList[grSquareCellsNetwork],
                  Reverse /@ EdgeList[grSquareCellsNetwork]
                ],
            DirectedEdges -> True,
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[Graph]]
          ]
    ];


End[]; (* `Private` *)

EndPackage[]