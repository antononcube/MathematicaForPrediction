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

$QRMonFailure::usage = "Failure symbol for the monad QRMon."

QRMonGetData::usage = "Get time series path data."

QRMonSetData::usage = "Assigns the argument to the key \"data\" in the monad context. \
(The rest of the monad context is unchanged.)"

QRMonTakeData::usage = "Gives the value of the key \"data\" from the monad context."

QRMonSetRegressionFunctions::usage = "Assigns the argument to the key \"regressionFunctions\" in the monad context. \
(The rest of the monad context is unchanged.)"

QRMonTakeRegressionFunctions::usage = "Gives the value of the key \"regressionFunctions\" from the monad context."

QRMonTakeOutliers::usage = "Gives the value of the key \"outliers\" from the monad context."

QRMonEchoDataSummary::usage = "Echoes a summary of the data."

QRMonDeleteMissing::usage = "Deletes records with missing data."

QRMonRescale::usage = "Rescales the data."

QRMonLeastSquaresFit::usage = "Linear regression fit for the data in the pipeline or the context \
using specified functions to fit."

QRMonFit::usage = "Same as QRMonLinearRegressionFit."

QRMonQuantileRegression::usage = "Quantile regression for the data in the pipeline or the context."

QRMonRegression::usage = "Quantile regression for the data in the pipeline or the context. \
(Same as QRMonQuantileRegression.)"

QRMonQuantileRegressionFit::usage = "Quantile regression fit for the data in the pipeline or the context \
using specified functions to fit."

QRMonRegressionFit::usage = "Quantile regression fit for the data in the pipeline or the context \
using specified functions to fit. (Same as QRMonQuantileRegressionFit.)"

QRMonEvaluate::usage = "Evaluates the regression functions over a number or a list of numbers."

QRMonPlot::usage = "Plots the data points or the data points together with the found regression curves."

QRMonDateListPlot::usage = "Plots the data points or the data points together with the found regression curves."

QRMonErrorPlots::usage = "Plots relative approximation errors for each regression quantile."

QRMonConditionalCDF::usage = "Finds conditional CDF approximations for specified points."

QRMonConditionalCDFPlot::usage = "Plots approximations of conditional CDF."

QRMonOutliers::usage = "Find the outliers in the data."

QRMonOutliersPlot::usage = "Plot the outliers in the data. Finds them first if not already in the context."

QRMonBandsSequence::usage = "Maps the time series values into a sequence of band indices derived from the regression quantiles."

QRMonGridSequence::usage = "Maps the time series values into a sequence of indices derived from a values grid."

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

GenerateStateMonadCode[ "MonadicQuantileRegression`QRMon", "FailureSymbol" -> $QRMonFailure, "StringContextNames" -> False ]

(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)

ClearAll[QRMonSetData]
QRMonSetData[$QRMonFailure] := $QRMonFailure;
QRMonSetData[][$QRMonFailure] := $QRMonFailure;
QRMonSetData[xs_, context_] := $QRMonFailure;
QRMonSetData[data_List][xs_, context_] := QRMonUnit[ xs, Join[ context, <|"data"->data|> ] ];
QRMonSetData[__][___] := $QRMonFailure;


ClearAll[QRMonTakeData]
QRMonTakeData[$QRMonFailure] := $QRMonFailure;
QRMonTakeData[][$QRMonFailure] := $QRMonFailure;
QRMonTakeData[xs_, context_] := QRMonTakeData[][xs, context];
QRMonTakeData[][xs_, context_] := QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue ];
QRMonTakeData[__][___] := $QRMonFailure;


ClearAll[QRMonSetRegressionFunctions]
QRMonSetRegressionFunctions[$QRMonFailure] := $QRMonFailure;
QRMonSetRegressionFunctions[][$QRMonFailure] := $QRMonFailure;
QRMonSetRegressionFunctions[xs_, context_] := $QRMonFailure;
QRMonSetRegressionFunctions[funcs_Association][xs_, context_] := QRMonUnit[ xs, Join[ context, <|"regressionFunctions"->funcs|> ] ];
QRMonSetRegressionFunctions[__][___] := $QRMonFailure;


ClearAll[QRMonTakeRegressionFunctions]
QRMonTakeRegressionFunctions[$QRMonFailure] := $QRMonFailure;
QRMonTakeRegressionFunctions[][$QRMonFailure] := $QRMonFailure;
QRMonTakeRegressionFunctions[xs_, context_] := context["regressionFunctions"];
QRMonTakeRegressionFunctions[][xs_, context_] := context["regressionFunctions"];
QRMonTakeRegressionFunctions[__][___] := $QRMonFailure;


ClearAll[QRMonTakeOutliers]
QRMonTakeOutliers[$QRMonFailure] := $QRMonFailure;
QRMonTakeOutliers[][$QRMonFailure] := $QRMonFailure;
QRMonTakeOutliers[xs_, context_] := context["outliers"];
QRMonTakeOutliers[][xs_, context_] := context["outliers"];
QRMonTakeOutliers[__][___] := $QRMonFailure;


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

ClearAll[QRMonGetData];

QRMonGetData[$QRMonFailure] := $QRMonFailure;

QRMonGetData[][xs_, context_] := QRMonGetData[xs, context]

QRMonGetData[xs_, context_] :=
    Block[{data},

      Which[

        KeyExistsQ[context, "data"] && MatrixQ[context["data"], NumericQ] && Dimensions[context["data"]][[2]] == 2,
        QRMonUnit[ context["data"], context],

        KeyExistsQ[context, "data"] && VectorQ[context["data"], NumericQ],
        data = DataToNormalForm[context["data"]];
        QRMonUnit[ data, Join[context, <| "data"->data |>] ],

        KeyExistsQ[context, "data"] && MatchQ[context["data"], (_TimeSeries | _TemporalData)],
        QRMonUnit[ context["data"]["Path"] /. Quantity[x_, u_] :> x, context],

        MatrixQ[xs, NumericQ] && Dimensions[xs][[2]] == 2,
        QRMonUnit[xs, context],

        VectorQ[xs, NumericQ],
        QRMonUnit[ Transpose[{ Range[Length[xs]], xs }], context],

        MatchQ[xs, (_TimeSeries | _TemporalData)],
        QRMonUnit[ xs["Path"] /. Quantity[x_, u_] :> x, context],

        True,
        Echo["Cannot find data.", "GetData:"];
        $QRMonFailure
      ]

    ];

QRMonGetData[___][xs_, context_Association] := $QRMonFailure;


(**************************************************************)
(* DeleteMissing                                             *)
(**************************************************************)

ClearAll[QRMonEchoDataSummary];

QRMonEchoDataSummary[$QRMonFailure] := $QRMonFailure;

QRMonEchoDataSummary[xs_, context_] := QRMonEchoDataSummary[][xs, context];

QRMonEchoDataSummary[][xs_, context_] :=
    Fold[ QRMonBind, QRMonUnit[xs, context], { QRMonGetData, QRMonEchoFunctionValue["Data summary:", RecordsSummary] } ]

QRMonEchoDataSummary[___][__] := $QRMonFailure;


(**************************************************************)
(* DeleteMissing                                             *)
(**************************************************************)

ClearAll[QRMonDeleteMissing];

QRMonDeleteMissing[$QRMonFailure] := $QRMonFailure;

QRMonDeleteMissing[xs_, context_] := QRMonDeleteMissing[][xs, context];

QRMonDeleteMissing[][xs_, context_] :=
    Block[{data},

      data = QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue];

      If[ data === $QRMonFailure,
        $QRMonFailure,
      (*ELSE*)
        data = DeleteMissing[data, 1, 2];
        QRMonUnit[ data, Join[ context, <|"data"->data|>] ]
      ]
    ];

QRMonDeleteMissing[___][__] := $QRMonFailure;


(**************************************************************)
(* Rescale                                                    *)
(**************************************************************)

ClearAll[QRMonRescale];

Options[QRMonRescale] = {Axes->{"x","y"}};

QRMonRescale[$QRMonFailure] := $QRMonFailure;

QRMonRescale[xs_, context_] := QRMonRescale[][xs, context];

QRMonRescale[opts:OptionsPattern[]][xs_, context_] :=
    Block[{data},

      data = QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue];

      If[ data === $QRMonFailure,
        $QRMonFailure,
        (*ELSE*)
        data = Transpose[Rescales /@ Transpose[data]];
        QRMonUnit[ data, Join[ context, <|"data"->data|>] ]
      ]
    ];

QRMonRescale[___][__] := $QRMonFailure;


(**************************************************************)
(* LeastSquaresFit                                            *)
(**************************************************************)

ClearAll[QRMonLeastSquaresFit];

QRMonLeastSquaresFit[$QRMonFailure] := $QRMonFailure;

QRMonLeastSquaresFit[xs_, context_Association] := $QRMonFailure;

QRMonLeastSquaresFit[funcs_List, opts:OptionsPattern[]][xs_, context_] :=
    Block[{var},

      var =
          With[{globalQ = Context@# === "Global`" &},
            DeleteDuplicates@Cases[funcs, _Symbol?globalQ, Infinity]
          ];

      If[ Length[var] == 0,
        $QRMonFailure,
      (*ELSE*)
        QRMonLeastSquaresFit[funcs, First[var], opts][xs, context]
      ]
    ];

QRMonLeastSquaresFit[funcs_List, var_Symbol, opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, qFunc},

      data = QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue ];

      qFunc = Fit[data, funcs, var];

      qFunc = Function[ Evaluate[ qFunc /. var -> Slot[1] ] ];

      QRMonUnit[qFunc,
        Join[context,
          <|"data"->data,
            "regressionFunctions" -> Join[ Lookup[context, "regressionFunctions", <||>], <| "mean" -> qFunc |> ] |>
        ]
      ]
    ];

QRMonLeastSquaresFit[___][__] := $QRMonFailure;


ClearAll[QRMonFit];

QRMonFit = QRMonLeastSquaresFit;

(**************************************************************)
(* Quantile regression                                        *)
(**************************************************************)

ClearAll[QRMonQuantileRegression];

Options[QRMonQuantileRegression] = Options[QuantileRegression];

QRMonQuantileRegression[$QRMonFailure] := $QRMonFailure;

QRMonQuantileRegression[xs_, context_Association] := $QRMonFailure;

QRMonQuantileRegression[knots_Integer, opts:OptionsPattern[]][xs_, context_] :=
    QRMonQuantileRegression[knots, {0.25, 0.5, 0.75}, opts][xs, context];

QRMonQuantileRegression[knots_Integer, qs:{_?NumberQ..}, opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, qFuncs},

      data = QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue ];

      qFuncs = QuantileRegression[data, knots, qs, opts];

      If[ ListQ[qFuncs] && Length[qFuncs] == Length[qs],
        qFuncs = AssociationThread[qs, qFuncs];
        QRMonUnit[qFuncs, Join[context, <|"data"->data, "regressionFunctions" -> Join[ Lookup[context, "regressionFunctions", <||>], qFuncs] |> ] ],
        (* ELSE *)
        $QRMonFailure
      ]
    ];

QRMonQuantileRegression[___][__] :=
    Block[{},
      Echo[
        StringJoin[
          "The first argument is expected to be knots specification, (_Integer | {_?NumberQ..}). ",
          "The second option argument is expected to quantiles specification, {_?NumberQ..}."
        ]
        "QRMonQuantileRegression:"
      ];

      $QRMonFailure
    ];


ClearAll[QRMonRegression]
QRMonRegression = QRMonQuantileRegression;


(**************************************************************)
(* Quantile regression fit                                    *)
(**************************************************************)

ClearAll[QRMonQuantileRegressionFit];

Options[QRMonQuantileRegressionFit] = Options[QuantileRegressionFit];

QRMonQuantileRegressionFit[$QRMonFailure] := $QRMonFailure;

QRMonQuantileRegressionFit[xs_, context_Association] := $QRMonFailure;

QRMonQuantileRegressionFit[funcs_List, opts:OptionsPattern[]][xs_, context_] :=
    QRMonQuantileRegressionFit[funcs, {0.25, 0.5, 0.75}, opts][xs, context];

QRMonQuantileRegressionFit[funcs_List, qs:{_?NumberQ..}, opts:OptionsPattern[]][xs_, context_] :=
    Block[{var},

      var =
          With[{globalQ = Context@# === "Global`" &},
            DeleteDuplicates@Cases[funcs, _Symbol?globalQ, Infinity]
          ];

      If[ Length[var] == 0,
        $QRMonFailure,
        (*ELSE*)
        QRMonQuantileRegressionFit[funcs, First[var], qs, opts][xs, context]
      ]
    ];

QRMonQuantileRegressionFit[funcs_List, var_Symbol, opts:OptionsPattern[]][xs_, context_] :=
    QRMonQuantileRegressionFit[funcs, var, {0.25, 0.5, 0.75}, opts][xs, context];

QRMonQuantileRegressionFit[funcs_List, var_Symbol, qs:{_?NumberQ..}, opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, qFuncs},

      data = QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue ];

      qFuncs = QuantileRegressionFit[data, funcs, var, qs, opts];

      If[ ListQ[qFuncs] && Length[qFuncs] == Length[qs],
        qFuncs = Map[Function[{expr}, Function[Evaluate[expr /. var -> Slot[1]]]], qFuncs];
        qFuncs = AssociationThread[qs, qFuncs];
        QRMonUnit[qFuncs, Join[context, <|"data"->data, "regressionFunctions" -> Join[ Lookup[context, "regressionFunctions", <||>], qFuncs] |> ] ],
      (* ELSE *)
        $QRMonFailure
      ]
    ];

QRMonQuantileRegressionFit[___][__] := $QRMonFailure;


ClearAll[QRMonRegressionFit]
QRMonRegressionFit = QRMonQuantileRegressionFit;


(**************************************************************)
(* Evaluate                                                   *)
(**************************************************************)
(* See InterpolationFunction[___]["Evaluate"[{0.2, 0.3, 1}]] *)
(* Probably can be optimized. *)

ClearAll[QRMonEvaluate]

QRMonEvaluate[$QRMonFailure] := $QRMonFailure;

QRMonEvaluate[xs_, context_Association] := $QRMonFailure;

QRMonEvaluate[___][$QRMonFailure] := $QRMonFailure;

QRMonEvaluate[n_?AtomQ][x_, context_] :=
    If[KeyExistsQ[context, "regressionFunctions"],
      QRMonUnit[ Map[#[n]&, context["regressionFunctions"] ], context ],
      (*ELSE*)
      $QRMonFailure
    ];

QRMonEvaluate[arr_List][x_, context_] :=
    If[KeyExistsQ[context, "regressionFunctions"],
      QRMonUnit[ Map[Function[{qf}, Map[qf, arr, {-1}]], context["regressionFunctions"] ], context ],
    (*ELSE*)
      $QRMonFailure
    ];

QRMonEvaluate[___][__] := $QRMonFailure;


(**************************************************************)
(* Plot                                                       *)
(**************************************************************)

ClearAll[QRMonPlot];

Options[QRMonPlot] = Join[ {"Echo"->True, "DateListPlot"->False}, Options[ListPlot] ];

QRMonPlot[QRMonPlot] := $QRMonFailure;

QRMonPlot[x_, context_Association] := QRMonPlot[][x, context];

QRMonPlot[opts:OptionsPattern[]][xs_, context_] :=
    Block[{data, res, listPlotFunc = ListPlot, listPlotOpts, plotOpts},

      data = QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue];

      If[data===$QRMonFailure, Return[$QRMonFailure]];

      If[ TrueQ[OptionValue[QRMonPlot, "DateListPlot"]], listPlotFunc = DateListPlot ];

      listPlotOpts = Normal @ KeyTake[ {opts}, First /@ Options[listPlotFunc]];
      plotOpts = Normal @ KeyTake[ {opts}, First /@ Options[Plot]];


      res=
          Which[
            KeyExistsQ[context, "regressionFunctions"],
            Show[{
              listPlotFunc[data, listPlotOpts, PlotStyle -> Gray, ImageSize->Medium, PlotTheme -> "Scientific"],
              Plot[Evaluate[Through[Values[context["regressionFunctions"]][x]]], {x, Min[data[[All, 1]]], Max[data[[All, 1]]]},
                Evaluate[plotOpts],
                PerformanceGoal -> "Speed",
                PlotLegends->Keys[context["regressionFunctions"]]
              ]
            }],

            True,
            listPlotFunc[data, listPlotOpts, ImageSize->Medium, PlotTheme -> "Scientific"]
          ];


      If[ TrueQ[OptionValue[QRMonPlot, "Echo"]],
        Echo[res, "Plot:"];
      ];

      QRMonUnit[res, Join[ context, <|"data"->data|>] ]
    ];

QRMonPlot[__][__] := $QRMonFailure;


(**************************************************************)
(* DateListPlot                                               *)
(**************************************************************)

ClearAll[QRMonDateListPlot];

Options[QRMonDateListPlot] =  Join[ {"Echo"->True}, Options[ListPlot] ];;

QRMonPlot[QRMonDateListPlot] := $QRMonFailure;

QRMonDateListPlot[x_, context_Association] := QRMonPlot["DateListPlot"->True][x, context];

QRMonDateListPlot[opts:OptionsPattern[]][xs_, context_] := QRMonPlot["DateListPlot"->True, opts][x, context];

QRMonDateListPlot[__][__] := $QRMonFailure;


(**************************************************************)
(* Error plots                                                *)
(**************************************************************)

ClearAll[QRMonErrorPlots]

Options[QRMonErrorPlots] = Options[QRMonPlot] = Join[ {"Echo"->True, "DateListPlot"->False}, Options[ListPlot] ];

QRMonErrorPlots[$QRMonFailure] := $QRMonFailure;

QRMonErrorPlots[x_, context_Association] := QRMonErrorPlots[][x, context];

QRMonErrorPlots[opts:OptionsPattern[]][xs_, context_] :=
    Block[{res, listPlotFunc = ListPlot, listPlotOpts},

      listPlotFunc = If[ TrueQ[OptionValue[QRMonErrorPlots, "DateListPlot"]], DateListPlot, ListPlot ];

      listPlotOpts = Normal @ KeyTake[ {opts}, First /@ Options[listPlotFunc]];

      res =
          KeyValueMap[
            Function[{k, f},
              k ->
                  listPlotFunc[
                    Map[Function[{p}, {p[[1]], (f[p[[1]]] - p[[2]])/p[[2]]}], context["data"] ],
                    listPlotOpts,
                    PlotRange -> All, Filling -> Axis, Frame -> True, ImageSize -> Medium, PlotTheme -> "Scientific"]
            ],
            context["regressionFunctions"]
          ];

      If[ TrueQ[OptionValue[QRMonErrorPlots, "Echo"]],
        Echo[res, "Error plots:"];
      ];

      QRMonUnit[res, context]
    ];

QRMonErrorPlots[__][__] := $QRMonFailure;


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

ClearAll[QRMonConditionalCDF]

QRMonConditionalCDF[$QRMonFailure] := $QRMonFailure;

QRMonConditionalCDF[__][$QRMonFailure] := $QRMonFailure;

QRMonConditionalCDF[t0_?NumberQ][xs_, context_] := QRMonConditionalCDF[{t0}][xs, context];

QRMonConditionalCDF[ts:{_?NumberQ..}][xs_, context_] :=
    Block[{},
      Which[
        KeyExistsQ[context, "regressionFunctions"],
        QRMonUnit[ Association[ Map[ #->CDFEstimate[ context["regressionFunctions"], # ] &, ts] ], context ],

        True,
        Echo["Cannot find regression quantiles.", "QRMonCDFApproximation:"];
        $QRMonFailure
      ]
    ];

QRMonConditionalCDF[___][___] :=
    Block[{},
      Echo["The first argument is expected to be a number or a list of numbers.", "QRMonConditionalCDF:"];
      $QRMonFailure
    ];


(**************************************************************)
(* Conditional distributions plot                             *)
(**************************************************************)

ClearAll[QRMonConditionalCDFPlot]

Options[QRMonConditionalCDFPlot] := Prepend[ Options[Plot], "Echo"->True ];

QRMonConditionalCDFPlot[$QRMonFailure] := $QRMonFailure;

QRMonConditionalCDFPlot[__][$QRMonFailure] := $QRMonFailure;

QRMonConditionalCDFPlot[xs_, context_Association] := QRMonConditionalCDFPlot[][xs, context];

QRMonConditionalCDFPlot[opts:OptionsPattern[]][xs_, context_]:=
    Block[{funcs, res, plotOpts},

      Which[

        MatchQ[xs, Association[(_?NumericQ -> _InterpolatingFunction) ..]],
        funcs = xs,

        True,
        funcs = Fold[ QRMonBind, QRMonUnit[xs, context], {QRMonConditionalCDF, QRMonTakeValue}]
      ];

      If[ TrueQ[funcs === $QRMonFailure], Return[$QRMonFailure] ];

      plotOpts = Sequence @@ DeleteCases[{opts}, HoldPattern["Echo"->_] ];

      res =
          Association@
              KeyValueMap[#1 ->
                  Plot[#2[x], Prepend[First[#2["Domain"]], x],
                    Evaluate[plotOpts],
                    PlotRange -> {All, All}, PlotLegends -> False,
                    PlotTheme -> "Scientific",
                    PlotLabel -> Row[{"CDF at x-value:", #1}],
                    FrameLabel -> {"y-value", "Probability"},
                    ImageSize -> Small
                  ] &, funcs];

      If[ TrueQ[OptionValue[QRMonConditionalCDFPlot, "Echo"]],
        Echo[ res, If[Length[res]==1, "conditional CDF:", "conditional CDF's:"] ]
      ];

      QRMonUnit[res, context]
    ];

QRMonConditionalCDFPlot[__][__] :=
    Block[{},
      Echo["Options are expected as arguments. (Plot options or \"Echo\"->(True|False).)", "QRMonConditionalCDFPlot:"];
      $QRMonFailure
    ];


(**************************************************************)
(* Outlier finding                                            *)
(**************************************************************)

ClearAll[QRMonOutliers]

Options[QRMonOutliers] := { "Knots" -> 12, "TopOutliersQuantile" -> 0.98, "BottomOutliersQuantile" -> 0.02 };

QRMonOutliers[$QRMonFailure] := $QRMonFailure;

QRMonOutliers[__][$QRMonFailure] := $QRMonFailure;

QRMonOutliers[xs_, context_Association] := QRMonOutliers[][xs, context];

QRMonOutliers[opts:OptionsPattern[]][xs_, context_] :=
    Block[{knots, tq, bq, tfunc, bfunc, outliers, data},

      knots = OptionValue[ "Knots" ];
      Which[
        TrueQ[knots === Automatic],
        knots = 12,

        ! ( IntegerQ[knots] || VectorQ[knots, NumericQ]),
        Echo["The value of the options \"Knots\" is expected to be an integer or a list of numbers.", "QRMonOutliers:"];
        Return[$QRMonFailure]
      ];

      tq = OptionValue[ "TopOutliersQuantile" ];
      bq = OptionValue[ "BottomOutliersQuantile" ];
      Which[
        ! ( NumberQ[tq] && 0 < tq < 1 && NumberQ[bq] && 0 < bq < 1  ),
        Echo["The values of the options \"TopOutliersQuantile\" and \"BottomOutliersQuantile\" are expected to be numbers between 0 and 1.", "QRMonOutliers:"];
        Return[$QRMonFailure]
      ];

      data = QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue];

      If[ TrueQ[data === $QRMonFailure],
        Echo["Cannot find data.", "QRMonOutliers:"];
        Return[$QRMonFailure]
      ];

      {bfunc, tfunc} = QuantileRegression[ data, knots, {bq, tq} ];

      outliers =
          <| "topOutliers" -> Select[data, tfunc[#[[1]]] <= #[[2]]&],
             "bottomOutliers" -> Select[data, bfunc[#[[1]]] >= #[[2]]&] |>;

      QRMonUnit[
        outliers,
        Join[context, <| "data"-> data, "outliers"->outliers, "outlierRegressionFunctions" -> <| tq->tfunc, bq->bfunc |> |> ]
      ]
    ];

QRMonOutliers[__][__] :=
    Block[{},
      Echo[StringJoin[ "Options are expected as arguments:", ToString[Options[QRMonOutliers]], "."], "QRMonOutliers:"];
      $QRMonFailure
    ];


(**************************************************************)
(* Outliers plot                                              *)
(**************************************************************)

ClearAll[QRMonOutliersPlot]

Options[QRMonOutliersPlot] := { "Echo" -> True, "DateListPlot" -> False, ListPlot -> {Joined->False}, Plot -> {} };

QRMonOutliersPlot[$QRMonFailure] := $QRMonFailure;

QRMonOutliersPlot[__][$QRMonFailure] := $QRMonFailure;

QRMonOutliersPlot[xs_, context_Association] := QRMonOutliersPlot[][xs, context];

QRMonOutliersPlot[opts:OptionsPattern[]][xs_, context_] :=
    Block[{unit, listPlotFunc = ListPlot, listPlotOpts, res},

      unit =
          If[ KeyExistsQ[context, "outliers"] && KeyExistsQ[context, "outlierRegressionFunctions"],
            QRMonUnit[ xs, context ],
          (*ELSE*)
            QRMonBind[ QRMonUnit[ xs, context ], QRMonOutliers ]
          ];

      listPlotFunc = If[ TrueQ[OptionValue[QRMonOutliersPlot, "DateListPlot"]], DateListPlot, ListPlot ];

      listPlotOpts = Normal @ KeyTake[ OptionValue[QRMonOutliersPlot, ListPlot], First /@ Options[listPlotFunc]];


      (* This can be improved: right now the regression quantiles are plotted over the outlier points. *)
      (* Also, there should be an option for not plotting the regression quantiles. *)

      res =
          Show[{

            listPlotFunc[Join[{#data}, Values[#outliers]],
              listPlotOpts,
              PlotStyle -> {Gray, {PointSize[0.01], Lighter[Red]}, {PointSize[0.01], Lighter[Red]}},
              ImageSize -> Medium, PlotTheme -> "Scientific"
            ],

            Plot[Evaluate@KeyValueMap[ Tooltip[#2[x], #1]&, #outlierRegressionFunctions], Prepend[MinMax[#data[[All, 1]]], x],
              Evaluate[OptionValue[QRMonOutliersPlot, Plot]],
              PlotStyle -> {Opacity[0.1], GrayLevel[0.9]},
              PerformanceGoal -> "Speed"
              (*PlotRange -> {MinMax[#data[[All,1]]], MinMax[#data[[All,2]]]}*)
            ]

          }] & [ QRMonBind[ unit, QRMonTakeContext ] ];

      If[ TrueQ[OptionValue[QRMonOutliersPlot, "Echo"]],
        Echo[res, "Outliers plot:"]
      ];

      QRMonUnit[ res, QRMonBind[ unit, QRMonTakeContext ] ]
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

ClearAll[QRMonBandsSequence]

QRMonBandsSequence[$QRMonFailure] := $QRMonFailure;

QRMonBandsSequence[___][$QRMonFailure] := $QRMonFailure;

QRMonBandsSequence[xs_, context_Association ] := QRMonBandsSequence[][xs, context];

QRMonBandsSequence[][xs_, context_] :=
    Block[{qstates},

      Which[

        !KeyExistsQ[context, "data"],
        Echo["Cannot find data.", "QRMonBandsSequence:"];
        $QRMonFailure,

        !KeyExistsQ[context, "regressionFunctions"],
        Echo["Compute the regression quantiles first.", "QRMonBandsSequence:"];
        $QRMonFailure,

        True,
        qstates = FindQRRange[ #, Values[context["regressionFunctions"]] ]& /@ QRMonBind[ QRMonGetData[xs, context], QRMonTakeValue ];
        QRMonUnit[ qstates, context ]
      ]

    ];

QRMonBandsSequence[__][__] :=
    Block[{},
      Echo[ "No arguments are expected.", "QRMonBandsSequence:"];
      $QRMonFailure
    ];


(**************************************************************)
(* Grid sequential representation                             *)
(**************************************************************)

ClearAll[QRMonGridSequence]

QRMonGridSequence[$QRMonFailure] := $QRMonFailure;

QRMonGridSequence[___][$QRMonFailure] := $QRMonFailure;

QRMonGridSequence[xs_, context_Association ] := QRMonGridSequence[][xs, context];

QRMonGridSequence[][xs_, context_] := QRMonGridSequence[Automatic][xs, context];

QRMonGridSequence[Automatic][xs_, context_] :=
    Block[{data, qvals},
      Which[

        !KeyExistsQ[context, "data"],
        Echo["Cannot find data.", "QRMonGridSequence:"];
        $QRMonFailure,

        KeyExistsQ[context, "regressionFunctions"],
        data = DataToNormalForm[context["data"]];
        qvals = Quantile[ data[[All, 2]], Keys[context["regressionFunctions"]] ];
        QRMonGridSequence[ qvals ][xs, Join[context, <|"data"->data|> ]],

        True,
        Echo["Cannot find quantiles for the values mapping with Automatic grid argument.", "QRMonGridSequence"];
        $QRMonFailure
      ]
    ];

QRMonGridSequence[gridNCells_Integer][xs_, context_] :=
    Block[{data, rvals},
      Which[

        gridNCells < 1,
        Echo["An integer first argument is expected to be positive.", "QRMonGridSequence"];
        $QRMonFailure,

        KeyExistsQ[context, "data"],
        data = DataToNormalForm[context["data"]];
        rvals = Rescale[Range[1, gridNCells], {1, gridNCells}, MinMax[data[[All, 2]]]];
        QRMonGridSequence[ rvals ][xs, Join[context, <|"data"->data|> ]],

        True,
        Echo["Cannot find data.", "QRMonGridSequence"];
        $QRMonFailure
      ]
    ];

QRMonGridSequence[grid:{_?NumericQ..}][xs_, context_] :=
    Block[{stFunc, states},

      Which[

        !KeyExistsQ[context, "data"],
        Echo["Cannot find data.", "QRMonGridSequence:"];
        $QRMonFailure,

        True,
        stFunc = FindIntervalFunc[grid];
        states = stFunc /@ DataToNormalForm[context["data"]][[All,2]];
        QRMonUnit[ states, context ]
      ]

    ];

QRMonGridSequence[___][__] :=
    Block[{},
      Echo[ "The first argument is expected to be Automatic, an integer, or a list of numbers.", "QRMonGridSequence:"];
      $QRMonFailure
    ];


End[] (* `Private` *)

EndPackage[]