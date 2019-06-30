(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)

(* :Title: MonadicStructuralBreaksFinder *)
(* :Context: MonadicStructuralBreaksFinder` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-06-29 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2019 antonov *)
(* :Keywords: *)
(* :Discussion: *)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MonadicQuantileRegression`QRMonUnit]] == 0,
  Echo["MonadicQuantileRegression.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)


BeginPackage["MonadicStructuralBreaksFinder`"];

QRMonFindChowTestLocalMaxima::usage = "Finds structural breaks in the data using the Chow Test. \
It takes as options the options of QRMonFindLocalExtrema.";

Begin["`Private`"];

Needs["MonadicQuantileRegression`"];

(**************************************************************)
(* Find Chow Test local maxima                                *)
(**************************************************************)


ClearAll[QRMonFindChowTestLocalMaxima];

Options[QRMonFindChowTestLocalMaxima] = { "Knots" -> 20, "NearestWithOutliers" -> False, "NumberOfProximityPoints" -> 15, "EchoPlots" -> False };

QRMonFindChowTestLocalMaxima[$QRMonFailure] := $QRMonFailure;

QRMonFindChowTestLocalMaxima[xs_, context_Association] := QRMonFindChowTestLocalMaxima[][xs, context];

QRMonFindChowTestLocalMaxima[][xs_, context_Association] := QRMonFindChowTestLocalMaxima[ Automatic, {1, x} ][xs, context];

QRMonFindChowTestLocalMaxima[ points : ( { _?NumberQ.. } | Automatic ), fitFuncs_List, opts:OptionsPattern[] ][xs_, context_Association] :=
    Block[{knots, nearestWithOutliersQ, nProxPoints, echoPlotsQ, ctStats, res},

      knots = OptionValue["Knots"];
(*      Better leave the check to QRMonQuantileRegression. *)

      nearestWithOutliersQ = TrueQ[OptionValue["NearestWithOutliers"]];

      nProxPoints = OptionValue["NumberOfProximityPoints"];

      echoPlotsQ = TrueQ[OptionValue["EchoPlots"]];

      ctStats =
          Fold[
            QRMonBind,
            QRMonUnit[ xs, context ],
            { QRMonChowTestStatistic[ points, fitFuncs ],
              QRMonTakeValue
            }];

      res =
          Fold[
            QRMonBind,
            QRMonUnit[ ctStats ],
            { QRMonQuantileRegression[ knots, 0.5 ],
              QRMonSetRegressionFunctionsPlotOptions[{PlotStyle -> Red}],
              QRMonIfElse[ echoPlotsQ &,
                Function[{x,c}, QRMonPlot[][x,c]],
                Function[{x,c}, QRMonUnit[x,c]]
              ],
              QRMonFindLocalExtrema[ "NearestWithOutliers" -> nearestWithOutliersQ, "NumberOfProximityPoints" -> nProxPoints ],
              QRMonAddToContext["localExtrema"],
              QRMonIfElse[ echoPlotsQ &,
                Function[{x,c},
                  QRMonEchoFunctionContext[
                    ListPlot[#["data"],
                      GridLines -> {#["localExtrema", "localMaxima"][[All, 1]], None},
                      GridLinesStyle -> Pink, Joined -> False, Filling -> Axis,
                      PlotTheme -> "Detailed", ImageSize -> Large] &][x, c]
                ],
                Function[{x,c}, QRMonUnit[x,c]]
              ],
              QRMonTakeValue
            }];

      QRMonUnit[ res["localMaxima"], context ]
    ];


(**************************************************************)
(* Find structural breaks                                     *)
(**************************************************************)

(*Clear[QRMonFindStructuralBreaks];*)

(*QRMonFindStructuralBreaks[$QRMonFailure] := $QRMonFailure;*)

(*QRMonFindStructuralBreaks[xs_, context_] := QRMonFindStructuralBreaks[][xs, context];*)

(*QRMonFindStructuralBreaks[][xs_, context_] := QRMonFindStructuralBreaks[ Automatic, {1, x} ][xs, context];*)

(*QRMonFindStructuralBreaks[ points : ({_?NumericQ..} | Automatic), fitFuncs_List ] :=*)
(*    Block[{},*)


(*    ];*)

End[]; (* `Private` *)

EndPackage[]