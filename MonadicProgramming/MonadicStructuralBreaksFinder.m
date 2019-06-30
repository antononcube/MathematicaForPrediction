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
It takes as options the options of QRMonQuantileRegression, QRMonFindLocalExtrema, and QRMonPlot.";

Begin["`Private`"];

Needs["MonadicQuantileRegression`"];

(**************************************************************)
(* Find Chow Test local maxima                                *)
(**************************************************************)


ClearAll[QRMonFindChowTestLocalMaxima];

Options[QRMonFindChowTestLocalMaxima] = { "Knots" -> 12, "NearestWithOutliers" -> False, "NumberOfProximityPoints" -> 15, "EchoPlots" -> False };

Options[QRMonFindChowTestLocalMaxima] =
    Join[
      { "Knots" -> 20, "EchoPlots" -> False },
      Options[QRMonQuantileRegression],
      Options[QRMonFindLocalExtrema],
      Options[QRMonPlot]
    ];


QRMonFindChowTestLocalMaxima[$QRMonFailure] := $QRMonFailure;

QRMonFindChowTestLocalMaxima[xs_, context_Association] := QRMonFindChowTestLocalMaxima[][xs, context];

QRMonFindChowTestLocalMaxima[][xs_, context_Association] := QRMonFindChowTestLocalMaxima[ Automatic, {1, x} ][xs, context];

QRMonFindChowTestLocalMaxima[ points : ( { _?NumberQ.. } | Automatic ), fitFuncs_List, opts:OptionsPattern[] ][xs_, context_Association] :=
    Block[{knots, echoPlotsQ, localMaximaPlotFunc, ctStats, res},

      knots = OptionValue["Knots"];

      echoPlotsQ = TrueQ[OptionValue["EchoPlots"]];

      ctStats =
          Fold[
            QRMonBind,
            QRMonUnit[ xs, context ],
            { QRMonChowTestStatistic[ points, fitFuncs ],
              QRMonTakeValue
            }];

      localMaximaPlotFunc = ListPlot;
      If[ TrueQ[ "DateListPlot" /. {opts} /. { "DateListPlot"->False} ],
        localMaximaPlotFunc = DateListPlot
      ];

      res =
          Fold[
            QRMonBind,
            QRMonUnit[ ctStats ],
            { QRMonQuantileRegression[ knots, 0.5, FilterRules[ {opts}, Options[QRMonQuantileRegression] ] ],
              QRMonIfElse[ echoPlotsQ &,
                Function[{x,c}, QRMonPlot[ FilterRules[ {opts}, Options[QRMonPlot] ] ][x,c]],
                Function[{x,c}, QRMonUnit[x,c]]
              ],
              QRMonFindLocalExtrema[ FilterRules[ {opts}, Options[QRMonFindLocalExtrema] ] ],
              QRMonAddToContext["localExtrema"],
              QRMonIfElse[ echoPlotsQ &,
                Function[{x,c},
                  QRMonEchoFunctionContext[
                    localMaximaPlotFunc[#["data"],
                      FilterRules[ {opts}, Options[localMaximaPlotFunc] ],
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