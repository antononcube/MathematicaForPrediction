(*
    Tiling utilization functions Mathematica package
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

(* :Title: TilingUtilizationFunctions *)
(* :Context: TilingUtilizationFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-05-04 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Hextile, Hexagon, Tile, Rectangle, Binning, Histogram, Polygon, Mathematica, Wolfram Language, WL *)
(* :Discussion: *)

(*********************************************************)
(* Load needed packages                                  *)
(*********************************************************)

If[Length[DownValues[HextileBins`HextileBins]] == 0,
  Echo["HextileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/HextileBins.m"]
];

If[Length[DownValues[TileBins`TileBins]] == 0,
  Echo["TileBins.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/TileBins.m"]
];


(*********************************************************)
(* Package definition                                    *)
(*********************************************************)

BeginPackage["TilingUtilizationFunctions`"];
(* Exported symbols added here with SymbolName::usage *)

TileTagging::usage = "TileTagging[ aPoints : Association[ ( id_ -> { _?NumberQ, _?NumberQ} ).. ], cellSize_?NumericQ, ___] \
produces a tile tagging system for specified 2D points with ID's.";

Begin["`Private`"];

Needs["HextileBins`"];
Needs["TileBins`"];

(*********************************************************)
(* TileTagging                                           *)
(*********************************************************)

Clear[TileTagging];
Options[TileTagging] = { "TilingFunction" -> HextileBins };
TileTagging[ aPoints : Association[ ( id_ -> { _?NumberQ, _?NumberQ} ).. ], cellSize_?NumericQ, opts : OptionsPattern[] ] :=
    Block[{tilingFunc, aTileBins, aTileBinCenters, nf, aCellIDs},

      tilingFunc = OptionValue[TileTagging, "TillingFunction"];

      aTileBins = KeySortBy[tilingFunc[Values@aPoints, cellSize], Mean@*PolygonCoordinates];

      aTileBinCenters = KeyMap[Mean@*PolygonCoordinates, aTileBins];

      nf = Nearest[Keys[aTileBinCenters] -> "Index"];

      aCellIDs = Map[First[nf[#]] &, aPoints];

      <| "TileTagging" -> aCellIDs, "TileBins" -> aTileBins, "NearestTileCenterFunction" -> nf |>

    ] /; NumberQ[cellSize] && cellSize > 0;

TileTagging[___] :=
    Block[{},
      $Failed
    ];

End[]; (* `Private` *)

EndPackage[]