(*
    Monadic latent semantic analysis Mathematica package
    Copyright (C) 2017  Anton Antonov

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
    antononcube @ gmail . com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicTimeSeriesAnalysis *)
(* :Context: MonadicTimeSeriesAnalysis` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-06-03 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MathematicaForPredictionUtilities`RecordsSummary]] == 0,
  Echo["MathematicaForPredictionUtilities.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"]
];

If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];

If[Length[DownValues[QuantileRegression`QuantileRegression]] == 0,
  Echo["QuantileRegression.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/QuantileRegression.m"]
];

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];

If[Length[DownValues[OutlierIdentifiers`OutlierPosition]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicTimeSeriesAnalysis`"]

$TSAMonFailure::usage = "Failure symbol for the monad TSAMon."

TSAMonGetData::usage = "Get time series path data."

TSAMonQuantileRegression::usage = "Quantile regression for the data in the pipeline or the context."

TSAMonQuantileRegressionFit::usage = "Quantile regression fit for the data in the pipeline or the context \
using specified functions to fit."

TSAMonPlot::usage = "Plots the data points or the data points together with the found regression curves."

TSAMonRescale::usage = "Rescales the data."


Begin["`Private`"]

Needs["MathematicaForPredictionUtilities`"]
Needs["StateMonadCodeGenerator`"]
Needs["QuantileRegression`"]
Needs["CrossTabulate`"]
(*Needs["SSparseMatrix`"]*)
Needs["OutlierIdentifiers`"]


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of LSAMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicTimeSeriesAnalysis`TSAMon", "FailureSymbol" -> $TSAMonFailure, "StringContextNames" -> False ]

(**************************************************************)
(* GetData                                                    *)
(**************************************************************)
ClearAll[TSAMonGetData];

TSAMonGetData[$TSAMonFailure] := $TSAMonFailure;

TSAMonGetData[][xs_, context_] := TSAMonGetData[xs, context]

TSAMonGetData[xs_, context_] :=
    Block[{},

      Which[
        MatrixQ[xs, NumericQ] && Dimensions[xs][[2]] == 2,
        TSAMonUnit[xs, context],

        VectorQ[xs, NumericQ],
        TSMonUnit[ Transpose[{ Range[Length[xs]], xs }], context],

        KeyExistsQ[context, "data"] && MatrixQ[context["data"], NumericQ] && Dimensions[context["data"]][[2]] == 2,
        TSAMonUnit[ context["data"], context],

        True,
        Echo["Cannot find data.", "GetData:"];
        $TSAMonFailure
      ]

    ];

TSAMonGetData[___][xs_, context_Association] := $TSAMonFailure;


(**************************************************************)
(* Quantile regression                                        *)
(**************************************************************)

ClearAll[TSAMonQuantileRegression];

Options[TSAMonQuantileRegression] = Options[QuantileRegression];

TSAMonQuantileRegression[knots_Integer, opts:OptionsPattern[]][xs_, context_] :=
    TSAMonQuantileRegression[knots, {0.25, 0.5, 0.75}, opts][xs, context];

TSAMonQuantileRegression[knots_Integer, qs:{_?NumberQ..}, opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, qFuncs},

      data = TSAMonBind[ TSAMonGetData[xs, context], TSAMonTakeValue ];

      qFuncs = QuantileRegression[data, knots, qs, opts];

      If[ ListQ[qFuncs] && Length[qFuncs] == Length[qs],
        qFuncs = AssociationThread[qs, qFuncs];
        TSAMonUnit[qFuncs, Join[context, <|"data"->data, "qFuncs" -> qFuncs|>] ],
        (* ELSE *)
        $TSAMonFailure
      ]
    ];


(**************************************************************)
(* Plot                                                       *)
(**************************************************************)

ClearAll[TSAMonPlot];

TSAMonPlot[$TSAMonFailure] := $TSAMonFailure;

TSAMonPlot[opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, res},

      data = TSAMonBind[ TSAMonGetData[xs, context], TSAMonTakeValue];

      If[data===$TSAMonFailure, Return[$TSAMonFailure]];

      res=
          Which[
            KeyExistsQ[context, "qFuncs"],
            Show[{
              ListPlot[data, opts, PlotStyle -> Lighter[Red], ImageSize->Medium],
              Plot[Through[context["qFuncs"][x]], {x, Min[data[[All, 1]]], Max[data[[All, 1]]]}, PerformanceGoal -> "Speed"]}
            ],

            True,
            ListPlot[data, opts, ImageSize->Medium]
          ];

      Echo[res, "Plot:"];
      TSAMonUnit[res, context]
    ];


End[] (* `Private` *)

EndPackage[]