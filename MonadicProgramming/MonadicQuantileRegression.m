(*
    Monadic Quantile Regression Mathematica package
    Copyright (C) 2018  Anton Antonov

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

(* :Title: MonadicQuantileRegression *)
(* :Context: MonadicQuantileRegression` *)
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

BeginPackage["MonadicQuantileRegression`"]

$QRegMonFailure::usage = "Failure symbol for the monad QRegMon."

QRegMonGetData::usage = "Get time series path data."

QRegMonSetData::usage = "Assigns the argument to the key \"data\" in the monad context. \
(The rest of the monad context is unchanged.)"

QRegMonTakeData::usage = "Gives the value of the key \"data\" from the monad context."

QRegMonSetRegressionFunctions::usage = "Assigns the argument to the key \"regressionFunctions\" in the monad context. \
(The rest of the monad context is unchanged.)"

QRegMonTakeRegressionFunctions::usage = "Gives the value of the key \"regressionFunctions\" from the monad context."

QRegMonLeastSquaresFit::usage = "Linear regression fit for the data in the pipeline or the context \
using specified functions to fit."

QRegMonFit::usage = "Same as QRegMonLinearRegressionFit."

QRegMonQuantileRegression::usage = "Quantile regression for the data in the pipeline or the context."

QRegMonQuantileRegressionFit::usage = "Quantile regression fit for the data in the pipeline or the context \
using specified functions to fit."

QRegMonEvaluate::usage = "Evaluates the regression functions over a number or a list of numbers."

QRegMonPlot::usage = "Plots the data points or the data points together with the found regression curves."

QRegMonErrorPlots::usage = "Plots relative approximation errors for each regression quantile."

QRegMonRescale::usage = "Rescales the data."

QRegMonConditionalCDF::usage = "Finds conditional CDF approximations for specified points."

QRegMonConditionalCDFPlot::usage = "Plots approximations of conditional CDF."

QRegMonOutliers::usage = "Find the outliers in the data."

QRegMonOutliersPlot::usage = "Plot the outliers in the data. Finds them first if not already in the context."

QRegMonBandsSequence::usage = "Maps the time series values into a sequence of band indices derived from the regression quantiles."

QRegMonGridSequence::usage = "Maps the time series values into a sequence of indices derived from a values grid."

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

GenerateStateMonadCode[ "MonadicQuantileRegression`QRegMon", "FailureSymbol" -> $QRegMonFailure, "StringContextNames" -> False ]

(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)

ClearAll[QRegMonSetData]
QRegMonSetData[$QRegMonFailure] := $QRegMonFailure;
QRegMonSetData[][$QRegMonFailure] := $QRegMonFailure;
QRegMonSetData[xs_, context_] := $QRegMonFailure;
QRegMonSetData[data_List][xs_, context_] := QRegMonUnit[ xs, Join[ context, <|"data"->data|> ] ];
QRegMonSetData[__][___] := $QRegMonFailure;


ClearAll[QRegMonTakeData]
QRegMonTakeData[$QRegMonFailure] := $QRegMonFailure;
QRegMonTakeData[][$QRegMonFailure] := $QRegMonFailure;
QRegMonTakeData[xs_, context_] := QRegMonTakeData[][xs, context];
QRegMonTakeData[][xs_, context_] := QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue ];
QRegMonTakeData[__][___] := $QRegMonFailure;


ClearAll[QRegMonSetRegressionFunctions]
QRegMonSetRegressionFunctions[$QRegMonFailure] := $QRegMonFailure;
QRegMonSetRegressionFunctions[][$QRegMonFailure] := $QRegMonFailure;
QRegMonSetRegressionFunctions[xs_, context_] := $QRegMonFailure;
QRegMonSetRegressionFunctions[funcs_Association][xs_, context_] := QRegMonUnit[ xs, Join[ context, <|"regressionFunctions"->funcs|> ] ];
QRegMonSetRegressionFunctions[__][___] := $QRegMonFailure;


ClearAll[QRegMonTakeRegressionFunctions]
QRegMonTakeRegressionFunctions[$QRegMonFailure] := $QRegMonFailure;
QRegMonTakeRegressionFunctions[][$QRegMonFailure] := $QRegMonFailure;
QRegMonTakeRegressionFunctions[xs_, context_] := context["regressionFunctions"];
QRegMonTakeRegressionFunctions[][xs_, context_] := context["regressionFunctions"];
QRegMonTakeRegressionFunctions[__][___] := $QRegMonFailure;

(**************************************************************)
(* GetData                                                    *)
(**************************************************************)

Clear[DataToNormalForm]

DataToNormalForm[data_] :=
    Which[
      MatrixQ[data, NumericQ] && Dimensions[data][[2]] == 2,
      data,

      VectorQ[data, NumericQ],
      Transpose[{ Range[Length[data]], data }]
    ];

ClearAll[QRegMonGetData];

QRegMonGetData[$QRegMonFailure] := $QRegMonFailure;

QRegMonGetData[][xs_, context_] := QRegMonGetData[xs, context]

QRegMonGetData[xs_, context_] :=
    Block[{data},

      Which[

        KeyExistsQ[context, "data"] && MatrixQ[context["data"], NumericQ] && Dimensions[context["data"]][[2]] == 2,
        QRegMonUnit[ context["data"], context],

        KeyExistsQ[context, "data"] && VectorQ[xs, NumericQ],
        data = DataToNormalForm[context["data"]];
        QRegMonUnit[ data, Join[context, <| "data"->data |>] ],

        MatrixQ[xs, NumericQ] && Dimensions[xs][[2]] == 2,
        QRegMonUnit[xs, context],

        VectorQ[xs, NumericQ],
        QRegMonUnit[ Transpose[{ Range[Length[xs]], xs }], context],

        True,
        Echo["Cannot find data.", "GetData:"];
        $QRegMonFailure
      ]

    ];

QRegMonGetData[___][xs_, context_Association] := $QRegMonFailure;


(**************************************************************)
(* Rescale                                                    *)
(**************************************************************)

ClearAll[QRegMonRescale];

Options[QRegMonRescale] = {Axes->{"x","y"}};

QRegMonRescale[$QRegMonFailure] := $QRegMonFailure;

QRegMonRescale[opts:OptionsPattern[]][xs_, context_] :=
    Block[{data},

      data = QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue];

      If[ data === $QRegMonFailure,
        $QRegMonFailure,
        (*ELSE*)
        data = Transpose[Rescales /@ Transpose[data]];
        QRegMonUnit[ data, Join[ context, <|"data"->data|>] ]
      ]
    ];


(**************************************************************)
(* LeastSquaresFit                                            *)
(**************************************************************)

ClearAll[QRegMonLeastSquaresFit];

QRegMonLeastSquaresFit[$QRegMonFailure] := $QRegMonFailure;

QRegMonLeastSquaresFit[xs_, context_Association] := $QRegMonFailure;

QRegMonLeastSquaresFit[funcs_List, opts:OptionsPattern[]][xs_, context_] :=
    Block[{var},

      var =
          With[{globalQ = Context@# === "Global`" &},
            DeleteDuplicates@Cases[funcs, _Symbol?globalQ, Infinity]
          ];

      If[ Length[var] == 0,
        $QRegMonFailure,
      (*ELSE*)
        QRegMonLeastSquaresFit[funcs, First[var], opts][xs, context]
      ]
    ];

QRegMonLeastSquaresFit[funcs_List, var_Symbol, opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, qFunc},

      data = QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue ];

      qFunc = Fit[data, funcs, var];

      qFunc = Function[ Evaluate[ qFunc /. var -> Slot[1] ] ];

      QRegMonUnit[qFunc,
        Join[context,
          <|"data"->data,
            "regressionFunctions" -> Join[ Lookup[context, "regressionFunctions", <||>], <| "mean" -> qFunc |> ] |>
        ]
      ]
    ];

QRegMonLeastSquaresFit[___][__] := $QRegMonFailure;


ClearAll[QRegMonFit];

QRegMonFit = QRegMonLeastSquaresFit;

(**************************************************************)
(* Quantile regression                                        *)
(**************************************************************)

ClearAll[QRegMonQuantileRegression];

Options[QRegMonQuantileRegression] = Options[QuantileRegression];

QRegMonQuantileRegression[$QRegMonFailure] := $QRegMonFailure;

QRegMonQuantileRegression[xs_, context_Association] := $QRegMonFailure;

QRegMonQuantileRegression[knots_Integer, opts:OptionsPattern[]][xs_, context_] :=
    QRegMonQuantileRegression[knots, {0.25, 0.5, 0.75}, opts][xs, context];

QRegMonQuantileRegression[knots_Integer, qs:{_?NumberQ..}, opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, qFuncs},

      data = QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue ];

      qFuncs = QuantileRegression[data, knots, qs, opts];

      If[ ListQ[qFuncs] && Length[qFuncs] == Length[qs],
        qFuncs = AssociationThread[qs, qFuncs];
        QRegMonUnit[qFuncs, Join[context, <|"data"->data, "regressionFunctions" -> Join[ Lookup[context, "regressionFunctions", <||>], qFuncs] |> ] ],
        (* ELSE *)
        $QRegMonFailure
      ]
    ];

QRegMonQuantileRegression[___][__] :=
    Block[{},
      Echo[
        StringJoin[
          "The first argument is expected to be knots specification, (_Integer | {_?NumberQ..}). ",
          "The second option argument is expected to quantiles specification, {_?NumberQ..}."
        ]
        "QRegMonQuantileRegression:"
      ];

      $QRegMonFailure
    ];


(**************************************************************)
(* Quantile regression fit                                    *)
(**************************************************************)

ClearAll[QRegMonQuantileRegressionFit];

Options[QRegMonQuantileRegressionFit] = Options[QuantileRegressionFit];

QRegMonQuantileRegressionFit[$QRegMonFailure] := $QRegMonFailure;

QRegMonQuantileRegressionFit[xs_, context_Association] := $QRegMonFailure;

QRegMonQuantileRegressionFit[funcs_List, opts:OptionsPattern[]][xs_, context_] :=
    QRegMonQuantileRegressionFit[funcs, {0.25, 0.5, 0.75}, opts][xs, context];

QRegMonQuantileRegressionFit[funcs_List, qs:{_?NumberQ..}, opts:OptionsPattern[]][xs_, context_] :=
    Block[{var},

      var =
          With[{globalQ = Context@# === "Global`" &},
            DeleteDuplicates@Cases[funcs, _Symbol?globalQ, Infinity]
          ];

      If[ Length[var] == 0,
        $QRegMonFailure,
        (*ELSE*)
        QRegMonQuantileRegressionFit[funcs, First[var], qs, opts][xs, context]
      ]
    ];

QRegMonQuantileRegressionFit[funcs_List, var_Symbol, opts:OptionsPattern[]][xs_, context_] :=
    QRegMonQuantileRegressionFit[funcs, var, {0.25, 0.5, 0.75}, opts][xs, context];

QRegMonQuantileRegressionFit[funcs_List, var_Symbol, qs:{_?NumberQ..}, opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, qFuncs},

      data = QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue ];

      qFuncs = QuantileRegressionFit[data, funcs, var, qs, opts];

      If[ ListQ[qFuncs] && Length[qFuncs] == Length[qs],
        qFuncs = Map[Function[{expr}, Function[Evaluate[expr /. var -> Slot[1]]]], qFuncs];
        qFuncs = AssociationThread[qs, qFuncs];
        QRegMonUnit[qFuncs, Join[context, <|"data"->data, "regressionFunctions" -> Join[ Lookup[context, "regressionFunctions", <||>], qFuncs] |> ] ],
      (* ELSE *)
        $QRegMonFailure
      ]
    ];

QRegMonQuantileRegressionFit[___][__] := $QRegMonFailure;


(**************************************************************)
(* Evaluate                                                   *)
(**************************************************************)
(* See InterpolationFunction[___]["Evaluate"[{0.2, 0.3, 1}]] *)
(* Probably can be optimized. *)

ClearAll[QRegMonEvaluate]

QRegMonEvaluate[$QRegMonFailure] := $QRegMonFailure;

QRegMonEvaluate[xs_, context_Association] := $QRegMonFailure;

QRegMonEvaluate[___][$QRegMonFailure] := $QRegMonFailure;

QRegMonEvaluate[n_?AtomQ][x_, context_] :=
    If[KeyExistsQ[context, "regressionFunctions"],
      QRegMonUnit[ Map[#[n]&, context["regressionFunctions"] ], context ],
      (*ELSE*)
      $QRegMonFailure
    ];

QRegMonEvaluate[arr_List][x_, context_] :=
    If[KeyExistsQ[context, "regressionFunctions"],
      QRegMonUnit[ Map[Function[{qf}, Map[qf, arr, {-1}]], context["regressionFunctions"] ], context ],
    (*ELSE*)
      $QRegMonFailure
    ];

QRegMonEvaluate[___][__] := $QRegMonFailure;

(**************************************************************)
(* Plot                                                       *)
(**************************************************************)

ClearAll[QRegMonPlot];

Options[QRegMonPlot] = Prepend[Options[ListPlot], "Echo"->True];

QRegMonPlot[QRegMonPlot] := $QRegMonFailure;

QRegMonPlot[x_, context_Association] := QRegMonPlot[][x, context];

QRegMonPlot[opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, res},

      data = QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue];

      If[data===$QRegMonFailure, Return[$QRegMonFailure]];

      res=
          Which[
            KeyExistsQ[context, "regressionFunctions"],
            Show[{
              ListPlot[data, DeleteCases[{opts}, HoldPattern["Echo"->_]], PlotStyle -> Lighter[Red], ImageSize->Medium],
              Plot[Evaluate[Through[Values[context["regressionFunctions"]][x]]], {x, Min[data[[All, 1]]], Max[data[[All, 1]]]},
                Evaluate[DeleteCases[{opts}, HoldPattern["Echo"->_]]],
                PerformanceGoal -> "Speed",
                PlotLegends->Keys[context["regressionFunctions"]]
              ]
            }],

            True,
            ListPlot[data, opts, ImageSize->Medium]
          ];


      If[ TrueQ[OptionValue[QRegMonPlot, "Echo"]],
        Echo[res, "Plot:"];
      ];

      QRegMonUnit[res, Join[ context, <|"data"->data|>] ]
    ];

QRegMonPlot[__][__] := $QRegMonFailure;


(**************************************************************)
(* Error plots                                                *)
(**************************************************************)

ClearAll[QRegMonErrorPlots]

Options[QRegMonErrorPlots] = Prepend[Options[ListPlot], "Echo"->True];

QRegMonErrorPlots[$QRegMonFailure] := $QRegMonFailure;

QRegMonErrorPlots[x_, context_Association] := QRegMonErrorPlots[][x, context];

QRegMonErrorPlots[opts:OptionsPattern[]][xs_, context_] :=
    Block[{res},
      res =
          KeyValueMap[
            Function[{k, f},
              k ->
                  ListPlot[
                    Map[Function[{p}, {p[[1]], (f[p[[1]]] - p[[2]])/p[[2]]}], context["data"] ],
                    DeleteCases[{opts}, HoldPattern["Echo"->_]],
                    PlotRange -> All, Filling -> Axis, Frame -> True, ImageSize -> Medium]
            ],
            context["regressionFunctions"]
          ];

      If[ TrueQ[OptionValue[QRegMonErrorPlots, "Echo"]],
        Echo[res, "Error plots:"];
      ];

      QRegMonUnit[res, context]
    ];

QRegMonErrorPlots[__][__] := $QRegMonFailure;


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

ClearAll[QRegMonConditionalCDF]

QRegMonConditionalCDF[$QRegMonFailure] := $QRegMonFailure;

QRegMonConditionalCDF[__][$QRegMonFailure] := $QRegMonFailure;

QRegMonConditionalCDF[t0_?NumberQ][xs_, context_] := QRegMonConditionalCDF[{t0}][xs, context];

QRegMonConditionalCDF[ts:{_?NumberQ..}][xs_, context_] :=
    Block[{},
      Which[
        KeyExistsQ[context, "regressionFunctions"],
        QRegMonUnit[ Association[ Map[ #->CDFEstimate[ context["regressionFunctions"], # ] &, ts] ], context ],

        True,
        Echo["Cannot find regression quantiles.", "QRegMonCDFApproximation:"];
        $QRegMonFailure
      ]
    ];

QRegMonConditionalCDF[___][___] :=
    Block[{},
      Echo["The first argument is expected to be a number or a list of numbers.", "QRegMonConditionalCDF:"];
      $QRegMonFailure
    ];


(**************************************************************)
(* Conditional distributions plot                             *)
(**************************************************************)

ClearAll[QRegMonConditionalCDFPlot]

Options[QRegMonConditionalCDFPlot] := Prepend[ Options[Plot], "Echo"->True ];

QRegMonConditionalCDFPlot[$QRegMonFailure] := $QRegMonFailure;

QRegMonConditionalCDFPlot[__][$QRegMonFailure] := $QRegMonFailure;

QRegMonConditionalCDFPlot[xs_, context_Association] := QRegMonConditionalCDFPlot[][xs, context];

QRegMonConditionalCDFPlot[opts:OptionsPattern[]][xs_, context_]:=
    Block[{funcs, res, plotOpts},

      Which[

        MatchQ[xs, Association[(_?NumericQ -> _InterpolatingFunction) ..]],
        funcs = xs,

        True,
        funcs = Fold[ QRegMonBind, QRegMonUnit[xs, context], {QRegMonConditionalCDF, QRegMonTakeValue}]
      ];

      If[ TrueQ[funcs === $QRegMonFailure], Return[$QRegMonFailure] ];

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

      If[ TrueQ[OptionValue[QRegMonConditionalCDFPlot, "Echo"]],
        Echo[ res, If[Length[res]==1, "conditional CDF:", "conditional CDF's:"] ]
      ];

      QRegMonUnit[res, context]
    ];

QRegMonConditionalCDFPlot[__][__] :=
    Block[{},
      Echo["Options are expected as arguments. (Plot options or \"Echo\"->(True|False).)", "QRegMonConditionalCDFPlot:"];
      $QRegMonFailure
    ];


(**************************************************************)
(* Outlier finding                                            *)
(**************************************************************)

ClearAll[QRegMonOutliers]

Options[QRegMonOutliers] := { "Knots" -> 12, "TopOutliersQuantile" -> 0.98, "BottomOutliersQuantile" -> 0.02 };

QRegMonOutliers[$QRegMonFailure] := $QRegMonFailure;

QRegMonOutliers[__][$QRegMonFailure] := $QRegMonFailure;

QRegMonOutliers[xs_, context_Association] := QRegMonOutliers[][xs, context];

QRegMonOutliers[opts:OptionsPattern[]][xs_, context_] :=
    Block[{knots, tq, bq, tfunc, bfunc, outliers, data},

      knots = OptionValue[ "Knots" ];
      Which[
        TrueQ[knots === Automatic],
        knots = 12,

        ! ( IntegerQ[knots] || VectorQ[knots, NumericQ]),
        Echo["The value of the options \"Knots\" is expected to be an integer or a list of numbers.", "QRegMonOutliers:"];
        Return[$QRegMonFailure]
      ];

      tq = OptionValue[ "TopOutliersQuantile" ];
      bq = OptionValue[ "BottomOutliersQuantile" ];
      Which[
        ! ( NumberQ[tq] && 0 < tq < 1 && NumberQ[bq] && 0 < bq < 1  ),
        Echo["The values of the options \"TopOutliersQuantile\" and \"BottomOutliersQuantile\" are expected to be numbers between 0 and 1.", "QRegMonOutliers:"];
        Return[$QRegMonFailure]
      ];

      data = QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue];

      If[ TrueQ[data === $QRegMonFailure],
        Echo["Cannot find data.", "QRegMonOutliers:"];
        Return[$QRegMonFailure]
      ];

      {bfunc, tfunc} = QuantileRegression[ data, knots, {bq, tq} ];

      outliers =
          <| "topOutliers" -> Select[data, tfunc[#[[1]]] <= #[[2]]&],
             "bottomOutliers" -> Select[data, bfunc[#[[1]]] >= #[[2]]&] |>;

      QRegMonUnit[
        outliers,
        Join[context, <| "outliers"->outliers, "outlierRegressionQuantiles" -> <| tq->tfunc, bq->bfunc |> |> ]
      ]
    ];

QRegMonOutliers[__][__] :=
    Block[{},
      Echo[StringJoin[ "Options are expected as arguments:", ToString[Options[QRegMonOutliers]], "."], "QRegMonOutliers:"];
      $QRegMonFailure
    ];


(**************************************************************)
(* Outliers plot                                              *)
(**************************************************************)

ClearAll[QRegMonOutliersPlot]

Options[QRegMonOutliersPlot] := { "Echo"->True, ListPlot -> {}, Plot -> {} };

QRegMonOutliersPlot[$QRegMonFailure] := $QRegMonFailure;

QRegMonOutliersPlot[__][$QRegMonFailure] := $QRegMonFailure;

QRegMonOutliersPlot[xs_, context_Association] := QRegMonOutliersPlot[][xs, context];

QRegMonOutliersPlot[opts:OptionsPattern[]][xs_, context_] :=
    Block[{unit, res},

      unit =
          If[ KeyExistsQ[context, "outliers"] && KeyExistsQ[context, "outlierRegressionQuantiles"],
            QRegMonUnit[ xs, context ],
          (*ELSE*)
            QRegMonBind[ QRegMonUnit[ xs, context ], QRegMonOutliers ]
          ];

      (* This can be improved: right now the regression quantiles are plotted over the outlier points. *)
      (* Also, there should be an option for not plotting the regression quantiles. *)

      res =
          Show[{

            ListPlot[Join[{#data}, Values[#outliers]],
              Evaluate[OptionValue[QRegMonOutliersPlot, ListPlot]],
              PlotStyle -> {Gray, {PointSize[0.01], Lighter[Red]}, {PointSize[0.01], Lighter[Red]}},
              ImageSize -> Large, PlotTheme -> "Detailed"
            ],

            Plot[Evaluate@KeyValueMap[ Tooltip[#2[x], #1]&, #outlierRegressionQuantiles], Prepend[MinMax[#data[[All, 1]]], x],
              Evaluate[OptionValue[QRegMonOutliersPlot, Plot]],
              PlotStyle -> {Opacity[0.1], GrayLevel[0.9]},
              PerformanceGoal -> "Speed"
              (*PlotRange -> {MinMax[#data[[All,1]]], MinMax[#data[[All,2]]]}*)
            ]

          }] & [ QRegMonBind[ unit, QRegMonTakeContext ] ];

      If[ TrueQ[OptionValue[QRegMonOutliersPlot, "Echo"]],
        Echo[res, "Outliers plot:"]
      ];

      QRegMonUnit[ res, QRegMonBind[ unit, QRegMonTakeContext ] ]
    ];


(**************************************************************)
(* Bands sequential representation                            *)
(**************************************************************)

Clear[FindIntervalFunc]
FindIntervalFunc[qBoundaries : {_?NumericQ ...}] :=
    Block[{XXX, t = Partition[Join[{- Infinity}, qBoundaries, {Infinity}], 2, 1]},
      Function[
        Evaluate[
          Piecewise[
            MapThread[{#2, #1[[1]] < XXX <= #1[[2]]} &, {t, Range[1, Length[t]]}]
          ] /. {XXX -> #}
        ]
      ]
    ];

Clear[FindQRRange]
FindQRRange[{x_, y_}, funcs_List] :=
    Block[{qfs, pfunc},
      qfs = Through[funcs[x]];
      pfunc = FindIntervalFunc[qfs];
      pfunc[y]
    ];

ClearAll[QRegMonBandsSequence]

QRegMonBandsSequence[$QRegMonFailure] := $QRegMonFailure;

QRegMonBandsSequence[___][$QRegMonFailure] := $QRegMonFailure;

QRegMonBandsSequence[xs_, context_Association ] := QRegMonBandsSequence[][xs, context];

QRegMonBandsSequence[][xs_, context_] :=
    Block[{qstates},

      Which[

        !KeyExistsQ[context, "data"],
        Echo["Cannot find data.", "QRegMonBandsSequence:"];
        $QRegMonFailure,

        !KeyExistsQ[context, "regressionFunctions"],
        Echo["Compute the regression quantiles first.", "QRegMonBandsSequence:"];
        $QRegMonFailure,

        True,
        qstates = FindQRRange[ #, Values[context["regressionFunctions"]] ]& /@ QRegMonBind[ QRegMonGetData[xs, context], QRegMonTakeValue ];
        QRegMonUnit[ qstates, context ]
      ]

    ];

QRegMonBandsSequence[__][__] :=
    Block[{},
      Echo[ "No arguments are expected.", "QRegMonBandsSequence:"];
      $QRegMonFailure
    ];


(**************************************************************)
(* Grid sequential representation                             *)
(**************************************************************)

ClearAll[QRegMonGridSequence]

QRegMonGridSequence[$QRegMonFailure] := $QRegMonFailure;

QRegMonGridSequence[___][$QRegMonFailure] := $QRegMonFailure;

QRegMonGridSequence[xs_, context_Association ] := QRegMonGridSequence[][xs, context];

QRegMonGridSequence[][xs_, context_] := QRegMonGridSequence[Automatic][xs, context];

QRegMonGridSequence[Automatic][xs_, context_] :=
    Block[{data, qvals},
      Which[

        !KeyExistsQ[context, "data"],
        Echo["Cannot find data.", "QRegMonGridSequence:"];
        $QRegMonFailure,

        KeyExistsQ[context, "regressionFunctions"],
        data = DataToNormalForm[context["data"]];
        qvals = Quantile[ data[[All, 2]], Keys[context["regressionFunctions"]] ];
        QRegMonGridSequence[ qvals ][xs, Join[context, <|"data"->data|> ]],

        True,
        Echo["Cannot find quantiles for the values mapping with Automatic grid argument.", "QRegMonGridSequence"];
        $QRegMonFailure
      ]
    ];

QRegMonGridSequence[gridNCells_Integer][xs_, context_] :=
    Block[{data, rvals},
      Which[

        gridNCells < 1,
        Echo["An integer first argument is expected to be positive.", "QRegMonGridSequence"];
        $QRegMonFailure,

        KeyExistsQ[context, "data"],
        data = DataToNormalForm[context["data"]];
        rvals = Rescale[Range[1, gridNCells], {1, gridNCells}, MinMax[data[[All, 2]]]];
        QRegMonGridSequence[ rvals ][xs, Join[context, <|"data"->data|> ]],

        True,
        Echo["Cannot find data.", "QRegMonGridSequence"];
        $QRegMonFailure
      ]
    ];

QRegMonGridSequence[grid:{_?NumericQ..}][xs_, context_] :=
    Block[{stFunc, states},

      Which[

        !KeyExistsQ[context, "data"],
        Echo["Cannot find data.", "QRegMonGridSequence:"];
        $QRegMonFailure,

        True,
        stFunc = FindIntervalFunc[grid];
        states = stFunc /@ DataToNormalForm[context["data"]][[All,2]];
        QRegMonUnit[ states, context ]
      ]

    ];

QRegMonGridSequence[___][__] :=
    Block[{},
      Echo[ "The first argument is expected to be Automatic, an integer, or a list of numbers.", "QRegMonGridSequence:"];
      $QRegMonFailure
    ];


End[] (* `Private` *)

EndPackage[]