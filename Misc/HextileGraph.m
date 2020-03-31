(*
    Hextiling graph Mathematica package
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

(* :Title: HextileGraph *)
(* :Context: HextileGraph` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-03-30 *)

(* :Package Version: 0.8 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[HextileBins`HextileBins]] == 0,
  Echo["HextileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/HextileBins.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["HextileGraph`"];

HextileGraph::usage = "HextileGraph[ aLonLatValue, cellRadius, opts] \
makes a hexagonal tiling graph for specified data.";

Begin["`Private`"];

Needs["HextileBins`"];

(***********************************************************)
(* MakeHexGraph                                            *)
(***********************************************************)

Clear[HextileGraph];

HextileGraph::"nbm" = "The value of the option \"BinMethod\" is expected to be one of \"HextileBins\" or \"GeoHistogram\".";

HextileGraph::"mr" = "If the value of the option \"BinMethod\" is `1` then the second argument is expected to be `2`.";

Options[HextileGraph] :=
    Join[
      {"BinMethod" -> "HextileBins", "RemoveLoneCells" -> False},
      Options[HextileBins],
      Options[GeoHistogram],
      Options[NearestNeighborGraph]
    ];

HextileGraph[
  aLonLatValue : Association[({_?NumberQ, _?NumberQ} -> _?NumberQ) ..],
  cellRadius : (_?NumberQ | _Quantity),
  opts : OptionsPattern[] ] :=

    Block[{binMethod, removeLoneCellsQ, grHist, aPolygonValues, lsCells, aCells,
      nc, lsDistances, pos, grHexagonCellsNetwork, grHexagonCells},

      binMethod = OptionValue[HextileGraph, "BinMethod"];
      removeLoneCellsQ = TrueQ[OptionValue[HextileGraph, "RemoveLoneCells"]];

      Which[
        ToLowerCase["HextileBins"] == ToLowerCase[binMethod] && NumberQ[cellRadius],
        aPolygonValues = HextileBins[aLonLatValue, cellRadius, FilterRules[{opts}, Options[HextileBins]]],

        ToLowerCase["HextileBins"] == ToLowerCase[binMethod] && !NumberQ[cellRadius],
        Message[HextileGraph::"mr", "\"HextileBins\"", "a number"];
        Return[$Failed],

        ToLowerCase["GeoHistogram"] == ToLowerCase[binMethod] && QuantityQ[cellRadius],
        grHist = GeoHistogram[KeyMap[Reverse, aLonLatValue], cellRadius, Automatic, FilterRules[{opts}, Options[GeoHistogram]]];
        aPolygonValues = Association@Cases[grHist[[1]], Tooltip[h_Polygon /; MatrixQ[h[[1]]], pop_ /; NumberQ[pop] && pop > 3] :> h -> pop, \[Infinity]],

        ToLowerCase["GeoHistogram"] == ToLowerCase[binMethod] && !QuantityQ[cellRadius],
        Message[HextileGraph::"mr", "\"GeoHistogram\"", "Quantity"];
        Return[$Failed],

        True,
        Message[HextileGraph::"nbm"];
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
        pos = Select[nc[#, {6, 1.1 * Min[lsDistances] / Cos[\[Pi] / 6.]}] & /@ aCells, Length[#] == 1 &];
        aCells = KeyDrop[aCells, Keys[pos]];
      ];

      (* Reassign cell ID's *)
      aCells = AssociationThread[Range[Length[aCells]], Values[aCells]];
      aCells = Association@KeyValueMap[#1 -> Prepend[#2, "ID" -> #1] &, aCells];

      (* Make neighbors graph *)
      grHexagonCellsNetwork =
          NearestNeighborGraph[
            Keys[aCells], {7, Min[lsDistances] / Cos[\[Pi] / 6.]},
            DistanceFunction -> (EuclideanDistance[aCells[#1]["Center"], aCells[#2]["Center"]] &),
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[NearestNeighborGraph]]
          ];

      (* Make final graph *)
      grHexagonCells =
          Graph[
            DirectedEdge @@@
                Join[
                  EdgeList[grHexagonCellsNetwork],
                  Reverse /@ EdgeList[grHexagonCellsNetwork]
                ],
            DirectedEdges -> True,
            VertexCoordinates -> KeyValueMap[#1 -> #2["Center"] &, aCells],
            FilterRules[{opts}, Options[Graph]]
          ]
    ];


End[]; (* `Private` *)

EndPackage[]