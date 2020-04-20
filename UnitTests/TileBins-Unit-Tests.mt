(*
    Tile Bins Mathematica unit tests
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
    antononcube @ gmai l . c om,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: TileBins-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2020-04-20 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Tile, Polygon, Binning, Histogram, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has unit tests of the functions TileBins and TileHistogram implemented in the file:

     https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/TileBins.m

*)
BeginTestSection["TileBins-Unit-Tests.mt"];

VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/TileBins.m"],
    Greater[Length[DownValues[TileBins`TileBins]], 0]
  ]
  ,
  True
  ,
  TestID -> "LoadPackage"
];


(***********************************************************)
(* Generate data                                           *)
(***********************************************************)

VerificationTest[(* 2 *)
  SeedRandom[1295];

  data = RandomVariate[MultinormalDistribution[{10, 10}, 7 IdentityMatrix[2]], 100];

  data2 = Map[# -> RandomInteger[{1, 10}] &, data];

  MatrixQ[data, NumericQ] && MatrixQ[ data2[[All,1]], NumericQ ]
  ,
  True
  ,
  TestID -> "Generated-2D-data-1"
];


(***********************************************************)
(* TileBins standard calls                              *)
(***********************************************************)

VerificationTest[

  res = TileBins[data, 2, "PolygonKeys" -> True ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-matrix-1"
];


VerificationTest[

  res = TileBins[data, 2, MinMax /@ Transpose[data], "PolygonKeys" -> True ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-matrix-2"
];


VerificationTest[

  res = TileBins[data, 2, "PolygonKeys" -> False ];

  MatchQ[ res, Association[ ( {_?NumericQ, _?NumericQ} -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-matrix-3"
];


VerificationTest[

  res = TileBins[data, 2, MinMax /@ Transpose[data], "PolygonKeys" -> False ];

  MatchQ[ res, Association[ ( {_?NumericQ, _?NumericQ} -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-matrix-4"
];


VerificationTest[

  res = TileBins[data, {2, 3.2}, MinMax /@ Transpose[data], "PolygonKeys" -> False ];

  MatchQ[ res, Association[ ( {_?NumericQ, _?NumericQ} -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-matrix-5"
];


VerificationTest[

  res = TileBins[data2, 2 ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-rules-1"
];


VerificationTest[

  res = TileBins[data2, 2, MinMax /@ Transpose[ data2[[All,1]] ] ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-rules-2"
];


VerificationTest[

  res = TileBins[data2, {2, 3}, MinMax /@ Transpose[ data2[[All,1]] ] ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "TileBins-rules-3"
];


VerificationTest[

  TileBins[ data2, 2 ] == TileBins[ Association @ data2, 2 ]
  ,
  True
  ,
  TestID -> "TileBins-signature-1"
];


VerificationTest[

  TileBins[ data2, 2, "AggregationFunction" -> Total ] == TileBins[ data2, 2 ]
  ,
  True
  ,
  TestID -> "TileBins-signature-2"
];


VerificationTest[

  TileBins[ data, 2, "AggregationFunction" -> Mean ] == TileBins[ data, 2 ]
  ,
  True
  ,
  TestID -> "TileBins-signature-3"
];


VerificationTest[

  TileBins[ data, 2, Automatic ] == TileBins[ data, 2 ] &&
      TileBins[ data, 2, Automatic ] == TileBins[ data, 2, MinMax /@ Transpose[ data ] ]
  ,
  True
  ,
  TestID -> "TileBins-signature-4"
];


VerificationTest[

  TileBins[ data2, 2, Automatic ] == TileBins[ data2, 2 ] &&
      TileBins[ data2, 2, Automatic ] == TileBins[ data2, 2, MinMax /@ Transpose[ data2[[All, 1]] ] ]
  ,
  True
  ,
  TestID -> "TileBins-signature-5"
];


VerificationTest[

  TileBins[ data2, 2, Automatic, "AggregationFunction" -> Mean ] == TileBins[ data2, 2, "AggregationFunction" -> Mean ]
  ,
  True
  ,
  TestID -> "TileBins-signature-6"
];

VerificationTest[

  TileBins[ data2, {2, 3}, Automatic, "AggregationFunction" -> Mean ] == TileBins[ data2, {2, 3}, "AggregationFunction" -> Mean ]
  ,
  True
  ,
  TestID -> "TileBins-signature-7"
];

(***********************************************************)
(* Messages                                                *)
(***********************************************************)

VerificationTest[

  TileBins[ data2, 2, "OverlapFactor" -> -1 ]
  ,
  $Failed
  ,
  {TileBins::"nof"}
  ,
  TestID -> "TileBins-OverlapFactor-wrong-1"
];


EndTestSection[]
