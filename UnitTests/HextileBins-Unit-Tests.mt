(*
    HextileBins Mathematica unit tests
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

(* :Title: HextileBins-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2020-04-01 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Hextile, Hexagon, Binning, Histogram, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has unit tests of the functions HextileBins and HextileHistogram implemented in the file:

     https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/HextileBins.m

*)
BeginTestSection["HextileBins-Unit-Tests.mt"];

VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/HextileBins.m"],
    Greater[Length[DownValues[HextileBins`HextileBins]], 0]
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
(* HextileBins standard calls                              *)
(***********************************************************)

VerificationTest[

  res = HextileBins[data, 2, "PolygonKeys" -> True ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "HextileBins-matrix-1"
];


VerificationTest[

  res = HextileBins[data, 2, MinMax /@ Transpose[data], "PolygonKeys" -> True ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "HextileBins-matrix-2"
];


VerificationTest[

  res = HextileBins[data, 2, "PolygonKeys" -> False ];

  MatchQ[ res, Association[ ( {_?NumericQ, _?NumericQ} -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "HextileBins-matrix-3"
];


VerificationTest[

  res = HextileBins[data, 2, MinMax /@ Transpose[data], "PolygonKeys" -> False ];

  MatchQ[ res, Association[ ( {_?NumericQ, _?NumericQ} -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "HextileBins-matrix-4"
];


VerificationTest[

  res = HextileBins[data2, 2 ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "HextileBins-rules-1"
];


VerificationTest[

  res = HextileBins[data2, 2, MinMax /@ Transpose[ data2[[All,1]] ] ];

  MatchQ[ res, Association[ ( _Polygon -> _?NumericQ ).. ] ]
  ,
  True
  ,
  TestID -> "HextileBins-rules-2"
];


VerificationTest[

  HextileBins[ data2, 2 ] == HextileBins[ Association @ data2, 2 ]
  ,
  True
  ,
  TestID -> "HextileBins-signature-1"
];


VerificationTest[

  HextileBins[ data2, 2, "AggregationFunction" -> Total ] == HextileBins[ data2, 2 ]
  ,
  True
  ,
  TestID -> "HextileBins-signature-2"
];


VerificationTest[

  HextileBins[ data, 2, "AggregationFunction" -> Mean ] == HextileBins[ data, 2 ]
  ,
  True
  ,
  TestID -> "HextileBins-signature-3"
];


VerificationTest[

  HextileBins[ data, 2, Automatic ] == HextileBins[ data, 2 ] &&
      HextileBins[ data, 2, Automatic ] == HextileBins[ data, 2, MinMax /@ Transpose[ data ] ]
  ,
  True
  ,
  TestID -> "HextileBins-signature-4"
];


VerificationTest[

  HextileBins[ data2, 2, Automatic ] == HextileBins[ data2, 2 ] &&
      HextileBins[ data2, 2, Automatic ] == HextileBins[ data2, 2, MinMax /@ Transpose[ data2[[All, 1]] ] ]
  ,
  True
  ,
  TestID -> "HextileBins-signature-5"
];


VerificationTest[

  HextileBins[ data2, 2, Automatic, "AggregationFunction" -> Mean ] == HextileBins[ data2, 2, "AggregationFunction" -> Mean ]
  ,
  True
  ,
  TestID -> "HextileBins-signature-6"
];


(***********************************************************)
(* Messages                                                *)
(***********************************************************)

VerificationTest[

  HextileBins[ data2, 2, "OverlapFactor" -> -1 ]
  ,
  $Failed
  ,
  {HextileBins::"nof"}
  ,
  TestID -> "HextileBins-OverlapFactor-wrong-1"
];


EndTestSection[]
