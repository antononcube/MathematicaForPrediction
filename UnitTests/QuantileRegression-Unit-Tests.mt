(*
    QuantileRegression Mathematica unit tests
    Copyright (C) 2019  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: RecordsSummary-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2019-10-17 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: quantile regression, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the functions QuantileRegression and QuantileRegressionFit in the package:

     https://github.com/antononcube/MathematicaForPrediction/blob/master/QuantileRegression.m

*)
BeginTestSection["QuantileRegression-Unit-Tests.mt"];


VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/QuantileRegression.m"],
    Greater[Length[DownValues[QuantileRegression`QuantileRegression]], 0]
  ]
  ,
  True
  ,
  TestID -> "LoadPackage"
];


VerificationTest[(* 2 *)
  distData = Table[{x, Exp[-x^2] + RandomVariate[NormalDistribution[0, .15]]}, {x, -3, 3, .2}];
  distData2 = Table[{x, Exp[-x^2] + RandomVariate[NormalDistribution[0, .15]]}, {x, -3, 3, .02}];
  MatrixQ[distData, NumberQ] && MatrixQ[distData2, NumberQ]
  ,
  True
  ,
  TestID -> "GenerateData"
];


VerificationTest[(* 3 *)
  findData =
      TemporalData[TimeSeries,
        List[List[
          StructuredArray[QuantityArray, List[22],
            StructuredArray`StructuredData[QuantityArray,
              List[16.170000076293945`, 14.109999656677246`,
                13.479999542236328`, 14.069999694824219`, 14.079999923706055`,
                13.609999656677246`, 13.630000114440918`, 12.9399995803833`,
                11.289999961853027`, 10.100000381469727`, 7.5`,
                7.570000171661377`, 10.15999984741211`, 10.390000343322754`,
                9.989999771118164`, 10.170000076293945`, 9.4399995803833`,
                10.5`, 10.449999809265137`, 8.25`, 8.9399995803833`,
                8.890000343322754`], "USDollars", List[List[1]]]]],
          List[List[
            List[3726345600, 3728764800, 3731270400, 3734035200, 3736713600,
              3739219200, 3741984000, 3744662400, 3747081600, 3749932800,
              3752524800, 3755203200, 3757881600, 3760300800, 3762806400,
              3765571200, 3768249600, 3770668800, 3773520000, 3776112000,
              3778790400, 3780086400]]], 1, List["Discrete", 1],
          List["Discrete", 1], 1,
          List[Rule[ValueDimensions, 1], Rule[DateFunction, Automatic],
            Rule[MetaInformation, List[Rule["FinancialProperty", "Close"]]],
            Rule[ResamplingMethod,
              List["Interpolation", Rule["InterpolationOrder", 0]]],
            Rule[TemporalRegularity, Automatic]]], True, 12.`];
  TrueQ[ Head[finData] === TemporalData ]
  ,
  True
  ,
  TestID -> "FinancialData"
];


VerificationTest[(* 4 *)
  probs = Range[0.2, 0.8, 0.2];
  qFuncs = QuantileRegression[ distData, 12, probs ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-1"
];


VerificationTest[(* 5 *)
  VectorQ[ Through[qFuncs[0]], NumberQ ]
  ,
  True
  ,
  TestID -> "QuantileRegression-2"
];


VerificationTest[(* 6 *)
  qFuncs = QuantileRegression[ distData, {-3, -2, 1, 0, 1, 1.5, 2.5, 3}, probs ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-3"
];


VerificationTest[(* 7 *)
  qFuncs = QuantileRegression[ distData, {-3, -2, 1, 0, 1, 1.5, 2.5, 3}, 0.5 ];
  ListQ[qFuncs] && Length[qFuncs] == 1 && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-4"
];


VerificationTest[(* 8 *)
  qFuncs = QuantileRegression[ distData[[All, 2]], 12, probs ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-5"
];


VerificationTest[(* 9 *)
  qFuncs = QuantileRegression[ finData, 12, probs ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-6"
];


VerificationTest[(* 10 *)
  qFuncs = QuantileRegressionFit[distData, {1, x, Exp[-x^2]}, x, probs];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && VectorQ[qFuncs /. x -> 12, NumberQ]
  ,
  True
  ,
  TestID -> "QuantileRegressionFit-1"
];


VerificationTest[(* 11 *)
  qFuncs = QuantileRegressionFit[distData[[All, 2]], {1, x, x^2}, x, probs];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && VectorQ[qFuncs /. x -> 12, NumberQ]
  ,
  True
  ,
  TestID -> "QuantileRegressionFit-2"
];


VerificationTest[(* 12 *)
  qFuncs = QuantileRegressionFit[finData, {1, x, x^2}, x, probs];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && VectorQ[qFuncs /. x -> 12, NumberQ]
  ,
  True
  ,
  TestID -> "QuantileRegressionFit-3"
];


VerificationTest[(* 13 *)
  qFuncs = QuantileRegression[ distData, 12, probs, Method -> LinearProgramming ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-Method-1"
];


VerificationTest[(* 14 *)
  qFuncs = QuantileRegression[ distData, 6, probs, Method -> { LinearProgramming } ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-Method-2"
];


VerificationTest[(* 15 *)
  qFuncs = QuantileRegression[ distData, 6, probs, Method -> { LinearProgramming, Method -> "CLP", Tolerance -> 10^(-4) } ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-Method-3"
];


VerificationTest[(* 16 *)
  qFuncs = QuantileRegression[ distData, 6, probs, Method -> NMinimize ];
  ListQ[qFuncs] && Length[qFuncs] == Length[probs] && Apply[ And, TrueQ[ Head[#] === Function ] & /@ qFuncs ]
  ,
  True
  ,
  TestID -> "QuantileRegression-Method-4"
];


VerificationTest[(* 17 *)
  qFuncs = QuantileRegression[distData2, 6, probs];
  sepPointsFractions =
      Map[Function[{f}, Length[Select[distData2, #[[2]] < f[#[[1]]] &]] / Length[distData2] // N], qFuncs];
  Norm[sepPointsFractions - probs, Infinity] <= 0.03
  ,
  True
  ,
  TestID -> "QuantileRegression-Separation-1"
];


VerificationTest[(* 17 *)
  qFuncs = QuantileRegressionFit[distData2, Table[Cos[x i], {i, 0, 16}], x, probs];
  sepPointsFractions =
      Map[Function[{f}, Length[Select[distData2, #[[2]] < (f /. x -> #[[1]]) &]] / Length[distData2] // N], qFuncs];
  Norm[sepPointsFractions - probs, Infinity] <= 0.03
  ,
  True
  ,
  TestID -> "QuantileRegressionFit-Separation-1"
];


EndTestSection[]
