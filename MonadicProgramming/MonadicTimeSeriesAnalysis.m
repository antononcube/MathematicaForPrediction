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

TSAMonTakeData::usage = "Takes \"data\" from the monad."

TSAMonQuantileRegression::usage = "Quantile regression for the data in the pipeline or the context."

TSAMonQuantileRegressionFit::usage = "Quantile regression fit for the data in the pipeline or the context \
using specified functions to fit."

TSAMonPlot::usage = "Plots the data points or the data points together with the found regression curves."

TSAMonRescale::usage = "Rescales the data."

TSAMonConditionalCDF::usage = "Finds conditional CDF approximations for specified points."

TSAMonConditionalCDFPlot::usage = "Plots approximations of conditional CDF."

TSAMonOutliers::usage = "Find the outliers in the data."

TSAMonOutliersPlot::usage = "Plot the outliers in the data. Finds them first if not already in the context."

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
(* Setters / getters                                          *)
(**************************************************************)

ClearAll[TSAMonTakeData]
TSAMonRescale[$TSAMonFailure] := $TSAMonFailure;
TSAMonRescale[][$TSAMonFailure] := $TSAMonFailure;
TSAMonTakeData[xs_, context_] := TSAMonTakeData[][xs, context];
TSAMonTakeData[][xs_, context_] := TSAMonBind[ TSAMonGetData[xs, context], TSAMonTakeValue ];
TSAMonTakeData[__][___] := $TSAMonFailure;


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
(* Rescale                                                    *)
(**************************************************************)

ClearAll[TSAMonRescale];

Options[TSAMonRescale] = {Axes->{"x","y"}};

TSAMonRescale[$TSAMonFailure] := $TSAMonFailure;

TSAMonRescale[opts:OptionsPattern[]][xs_, context_] :=
    Block[{data},

      data = TSAMonBind[ TSAMonGetData[xs, context], TSAMonTakeValue];

      If[ data === $TSAMonFailure,
        $TSAMonFailure,
        (*ELSE*)
        data = Transpose[Rescales /@ Transpose[data]];
        TSAMonUnit[ data, Join[ context, <|"data"->data|>] ]
      ]
    ];


(**************************************************************)
(* LeastSquaresFit                                            *)
(**************************************************************)



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
              Plot[Evaluate[Through[Values[context["qFuncs"]][x]]], {x, Min[data[[All, 1]]], Max[data[[All, 1]]]},
                PerformanceGoal -> "Speed",
                PlotLegends->Keys[context["qFuncs"]]
              ]
            }],

            True,
            ListPlot[data, opts, ImageSize->Medium]
          ];

      Echo[res, "Plot:"];
      TSAMonUnit[res, Join[ context, <|"data"->data|>] ]
    ];


(**************************************************************)
(* Conditional distribution                                   *)
(**************************************************************)

Clear[CDFEstimate]
CDFEstimate[qFuncs_Association, t0_, intOrder:_Integer:1] :=
    Interpolation[Transpose[{Through[Values[qFuncs][t0]], Keys[qFuncs]}], InterpolationOrder -> intOrder];

Clear[CDFPDFPlot]
CDFPDFPlot[t0_?NumberQ, qCDFInt_InterpolatingFunction, qs:{_?NumericQ..}, opts : OptionsPattern[]] :=
    Block[{},
      Plot[
        {qCDFInt[x], qCDFInt'[x]},
        {x, qCDFInt["Domain"][[1, 1]], qCDFInt["Domain"][[1, 2]]},
        opts, GridLines->{None, qs}, PlotRange -> {0, 1}, Axes -> False, Frame -> True]
    ];

ClearAll[TSAMonConditionalCDF]

TSAMonConditionalCDF[$TSAMonFailure] := $TSAMonFailure;

TSAMonConditionalCDF[__][$TSAMonFailure] := $TSAMonFailure;

TSAMonConditionalCDF[t0_?NumberQ][xs_, context_] := TSAMonConditionalCDF[{t0}][xs, context];

TSAMonConditionalCDF[ts:{_?NumberQ..}][xs_, context_] :=
    Block[{},
      Which[
        KeyExistsQ[context, "qFuncs"],
        TSAMonUnit[ Association[ Map[ #->CDFEstimate[ context["qFuncs"], # ] &, ts] ], context ],

        True,
        Echo["Cannot find regression quantiles.", "TSAMonCDFApproximation:"];
        $TSAMonFailure
      ]
    ];

TSAMonConditionalCDF[___][___] := $TSAMonFailure;


(**************************************************************)
(* Conditional distributions plot                             *)
(**************************************************************)

ClearAll[TSAMonConditionalCDFPlot]

Options[TSAMonConditionalCDFPlot] := Prepend[ Options[Plot], "Echo"->True ];

TSAMonConditionalCDFPlot[$TSAMonFailure] := $TSAMonFailure;

TSAMonConditionalCDFPlot[__][$TSAMonFailure] := $TSAMonFailure;

TSAMonConditionalCDFPlot[xs_, context_Association] := TSAMonConditionalCDFPlot[][xs, context];

TSAMonConditionalCDFPlot[opts:OptionsPattern[]][xs_, context_]:=
    Block[{funcs, res, plotOpts},

      Which[

        MatchQ[xs, Association[(_?NumericQ -> _InterpolatingFunction) ..]],
        funcs = xs,

        True,
        funcs = Fold[ TSAMonBind, TSAMonUnit[xs, context], {TSAMonConditionalCDF, TSAMonTakeValue}]
      ];

      If[ TrueQ[funcs === $TSAMonFailure], Return[$TSAMonFailure] ];

      plotOpts = Sequence @@ DeleteCases[{opts}, HoldPattern["Echo"->_] ];

      res =
          Association@
              KeyValueMap[#1 ->
                  Plot[#2[x], Prepend[First[#2["Domain"]], x],
                    Evaluate[plotOpts],
                    PlotRange -> {All, All}, PlotLegends -> False,
                    PlotTheme -> "Detailed",
                    PlotLabel -> Row[{"CDF at x-value:", #1}],
                    FrameLabel -> {"y-value", "Probability"},
                    ImageSize -> Small
                  ] &, funcs];

      If[ TrueQ[OptionValue[TSAMonConditionalCDFPlot, "Echo"]],
        Echo[ res, If[Length[res]==1, "conditional CDF:", "conditional CDF's:"] ]
      ];

      TSAMonUnit[res, context]
    ];

TSAMonConditionalCDFPlot[__][__] := $TSAMonFailure;


(**************************************************************)
(* Outlier finding                                            *)
(**************************************************************)

ClearAll[TSAMonOutliers]

Options[TSAMonOutliers] := { "Knots" -> 12, "TopOutliersQuantile" -> 0.98, "BottomOutliersQuantile" -> 0.02 };

TSAMonOutliers[$TSAMonFailure] := $TSAMonFailure;

TSAMonOutliers[__][$TSAMonFailure] := $TSAMonFailure;

TSAMonOutliers[xs_, context_Association] := TSAMonOutliers[][xs, context];

TSAMonOutliers[opts:OptionsPattern[]][xs_, context_] :=
    Block[{knots, tq, bq, tfunc, bfunc, outliers, data},

      knots = OptionValue[ "Knots" ];
      Which[
        TrueQ[knots === Automatic],
        knots = 12,

        ! ( IntegerQ[knots] || VectorQ[knots, NumericQ]),
        Echo["The value of the options \"Knots\" is expected to be an integer or a list of numbers.", "TSAMonOutliers:"];
        Return[$TSAMonFailure]
      ];

      tq = OptionValue[ "TopOutliersQuantile" ];
      bq = OptionValue[ "BottomOutliersQuantile" ];
      Which[
        ! ( NumberQ[tq] && 0 < tq < 1 && NumberQ[bq] && 0 < bq < 1  ),
        Echo["The values of the options \"TopOutliersQuantile\" and \"BottomOutliersQuantile\" are expected to be numbers between 0 and 1.", "TSAMonOutliers:"];
        Return[$TSAMonFailure]
      ];

      data = TSAMonBind[ TSAMonGetData[xs, context], TSAMonTakeValue];

      If[ TrueQ[data === $TSAMonFailure],
        Echo["Cannot find data.", "TSAMonOutliers:"];
        Return[$TSAMonFailure]
      ];

      {bfunc, tfunc} = QuantileRegression[ data, knots, {bq, tq} ];

      outliers =
          <| "topOutliers" -> Select[data, tfunc[#[[1]]] <= #[[2]]&],
             "bottomOutliers" -> Select[data, bfunc[#[[1]]] >= #[[2]]&] |>;

      TSAMonUnit[
        outliers,
        Join[context, <| "outliers"->outliers, "outlierRegressionQuantiles" -> <| tq->tfunc, bq->bfunc |> |> ]
      ]
    ];


(**************************************************************)
(* Outliers plot                                              *)
(**************************************************************)

ClearAll[TSAMonOutliersPlot]

Options[TSAMonOutliersPlot] := { "Echo"->True, ListPlot -> {}, Plot -> {} };

TSAMonOutliersPlot[$TSAMonFailure] := $TSAMonFailure;

TSAMonOutliersPlot[__][$TSAMonFailure] := $TSAMonFailure;

TSAMonOutliersPlot[xs_, context_Association] := TSAMonOutliersPlot[][xs, context];

TSAMonOutliersPlot[opts:OptionsPattern[]][xs_, context_] :=
    Block[{unit, res},

      unit =
          If[ KeyExistsQ[context, "outliers"] && KeyExistsQ[context, "outlierRegressionQuantiles"],
            TSAMonUnit[ xs, context ],
          (*ELSE*)
            TSAMonBind[ TSAMonUnit[ xs, context ], TSAMonOutliers ]
          ];

      (* This can be improved: right now the regression quantiles are plotted over the outlier points. *)
      (* Also, there should be an option for not plotting the regression quantiles. *)

      res =
          Show[{

            ListPlot[Join[{#data}, Values[#outliers]],
              Evaluate[OptionValue[TSAMonOutliersPlot, ListPlot]],
              PlotStyle -> {Gray, {PointSize[0.01], Lighter[Red]}, {PointSize[0.01], Lighter[Red]}},
              ImageSize -> Large, PlotTheme -> "Detailed"
            ],

            Plot[Evaluate@KeyValueMap[ Tooltip[#2[x], #1]&, #outlierRegressionQuantiles], Prepend[MinMax[#data[[All, 1]]], x],
              Evaluate[OptionValue[TSAMonOutliersPlot, Plot]],
              PlotStyle -> {Opacity[0.1], GrayLevel[0.9]},
              PerformanceGoal -> "Speed"
              (*PlotRange -> {MinMax[#data[[All,1]]], MinMax[#data[[All,2]]]}*)
            ]

          }] & [ TSAMonBind[ unit, TSAMonTakeContext ] ];

      If[ TrueQ[OptionValue[TSAMonOutliersPlot, "Echo"]],
        Echo[res, "Outliers plot:"]
      ];

      TSAMonUnit[ res, TSAMonBind[ unit, TSAMonTakeContext ] ]
    ];

End[] (* `Private` *)

EndPackage[]