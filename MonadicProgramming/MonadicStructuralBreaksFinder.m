(*
    Monadic Structural Breaks Finder Mathematica package
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
    antononcube @ gmail . com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicStructuralBreaksFinder *)
(* :Context: MonadicStructuralBreaksFinder` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-06-30 *)

(* :Package Version: 0.5 *)
(* :Mathematica Version: 12 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: Chow Test, Quantile Regression, Structural Breaks *)
(* :Discussion:

   # In brief

   This package provides functions to for the Quantile Regression Monad package (QRMon):

     https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m .

   Here is an example usage:

      ts = FinancialData[Entity["Financial", "^SPX"], {{2015, 1, 1}, Date[]}];

      QRMonUnit[ts]⟹
        QRMonFindChowTestLocalMaxima[ts["Times"][[2 ;; -2 ;; 10]], {1, x, x^2}, "EchoPlots" -> True]⟹
        QRMonEchoValue;

   The function QRMonFindChowTestLocalMaxima takes the options of QRMon's functions
   QRMonQuantileRegression, QRMonFindLocalExtrema, and QRMonPlot.

   For example:

      QRMonUnit[ts]
        QRMonFindChowTestLocalMaxima[
             "Knots" -> 20, InterpolationOrder -> 2,
             "NearestWithOutliers" -> False, "NumberOfProximityPoints" -> 5,
             "DateListPlot" -> True,
             "EchoPlots" -> True]⟹
       QRMonEchoValue;

   # Future plans

   It is probably a good idea to provide a function for plotting the fits over partitions based on
   the structural breaks.



   Anton Antonov
   Windermere, Florida, USA
   2019-06-30

*)

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

QRMonFindChowTestLocalMaxima[][xs_, context_Association] := QRMonFindChowTestLocalMaxima[ Automatic, Automatic ][xs, context];

QRMonFindChowTestLocalMaxima[ opts:OptionsPattern[] ][xs_, context_Association] := QRMonFindChowTestLocalMaxima[ Automatic, Automatic, opts ][xs, context];

QRMonFindChowTestLocalMaxima[ points : ( { _?NumberQ.. } | Automatic ), fitFuncs_, opts:OptionsPattern[] ][xs_, context_Association] :=
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