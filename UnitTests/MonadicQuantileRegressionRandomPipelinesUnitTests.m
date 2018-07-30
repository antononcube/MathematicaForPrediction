(*
    Monadic Quantile Regression random pipelines Mathematica unit tests
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
    antononcube @ gmai l . c om,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicQuantileRegressionRandomPipelinesUnitTests *)
(* :Context: MonadicQuantileRegressionRandomPipelinesUnitTests` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-07-29 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: unit test, randomly generated, quantile regression, least squares regression *)
(* :Discussion:

    Generation and execution of QRMon random test pipelines.

    Here is how to generate pipelines:

        SeedRandom[234];
        pipelines = MakeQRMonRandomPipelines[50];
        Length[pipelines]


    Here is how to run generated pipelines as verification tests:

        AbsoluteTiming[
          res = TestRunQRMonPipelines[pipelines, "Echo" -> True]
        ]


    Here is how to make test report and examine it:

        rpTRObj = TestReport[res]
        Column /@ (Normal /@ rpTRObj["TestsFailed"]) // TabView


    Note the the verification tests are made to match an expected combined/complicated pattern.
    That pattern catches output that correspond to:

    1. $QRMonFailure,
    2. outliers association (with keys "bottomOutliers" and "topOutliers"),
    3. regression functions association (with keys that are quantile numbers or "mean"),
    4. data arrays or time series (obtained with MovingAverage, etc.).

    That list can be derived by observing the stages-before-the-last in MakeQRMonRandomPipelines.


    Anton Antonov,
    Windermere, Florida, USA,
    2018-07-29

*)

If[Length[SubValues[MonadicQuantileRegression`QRMonQuantileRegression]] == 0,
  Echo["MonadicQuantileRegression.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]
];

BeginPackage["MonadicQuantileRegressionRandomPipelinesUnitTests`"];
(* Exported symbols added here with SymbolName::usage *)

MakeDistributionData::usage = "Make random test data."

MakeQRMonRandomPipelines::usage = "Make QRMon random pipelines."

TestRunQRMonPipelines::usage = "Run QRMon pipelines using VerificationTest."

Begin["`Private`"];

Needs["MonadicQuantileRegression`"];

ClearAll[MakeDistributionData, MakeQRMonRandomPipelines, TestRunQRMonPipelines];

MakeDistributionData[{rmin_Integer, rmax_Integer}, step_?NumberQ] :=
    Block[{ distData },

      distData =
          Table[{x, Exp[-x^2] + RandomVariate[NormalDistribution[0, .15]]}, {x, rmin, rmax, step}];

      {distData, TimeSeries[distData]}
    ];

MakeQRMonRandomPipelines[n_Integer] :=
    Block[{n1, n2},
      n1 = Floor[3/4 * n];
      n2 = (n - n1);
      MakeQRMonRandomPipelines[{n1, n2}]
    ];

MakeQRMonRandomPipelines[{n_Integer, n2_Integer}] :=
    MakeQRMonRandomPipelines[MakeDistributionData[{-3,3}, 0.1], {n, n2}];

MakeQRMonRandomPipelines[{distData_?ArrayQ, tsData:(_TimeSeries|_TemporalData)}, {n_Integer, n2_Integer}] :=
    Module[{stage1, stage2, stage3, stage4, stage5, stage3a, stage4a,
      allStages, allStages2, pipelines},

      stage1 = {QRMonUnit[], QRMonUnit[distData], QRMonUnit[tsData]};

      stage2 = {
        QRMonEchoDataSummary,
        QRMonDeleteMissing,
        QRMonRescale[Axes -> True],
        QRMonRescale[Axes -> {True,False}]
      };

      stage3 = {
        QRMonQuantileRegression,
        QRMonQuantileRegression[],
        QRMonQuantileRegression[6,RandomReal[{0,1},RandomInteger[{1,6}]]],
        QRMonQuantileRegressionFit[6],
        QRMonQuantileRegressionFit[{1, Global`x}],
        QRMonQuantileRegressionFit[{1, Global`x}, Global`x],
        QRMonLeastSquaresFit[6],
        QRMonLeastSquaresFit[{1, Global`x}],
        QRMonLeastSquaresFit[{1, Global`x}, Global`x],
        QRMonQuantileRegression[6, Method -> {LinearProgramming, Method -> Automatic}],
        QRMonQunalileRegeression[6, InterpolationOrder->1]
      };

      stage4 = {
        QRMonErrors[],
        QRMonErrors,
        QRMonBandsSequence,
        QRMonGridSequence,
        QRMonSimulate,
        QRMonOutliers
      };

      stage5 = {QRMonTakeValue, QRMonTakeRegressionFunctions, QRMonTakeOutliers};

      stage3a = {
        QRMonMovingAverage[RandomInteger[{0,10}]],
        QRMonMovingMedian[RandomInteger[{0,10}]],
        QRMonMovingMap[Mean, RandomReal[{0,0.2}]]
      };

      stage4a = {QRMonTakeValue};

      allStages = {{1, 1} -> stage1, {0, 5} -> stage2, {1, 1} -> stage3, {1, 1} -> stage4, {1, 1} -> stage5};
      allStages2 = {{1, 1} -> stage1, {1, 5} -> stage2, {1, 2} -> stage3a, {1, 1} -> stage4a};

      pipelines =
          Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages], {n}];
      pipelines =
          Join[pipelines,
            Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages2], {n2}]];

      pipelines
    ];

Options[TestRunQRMonPipelines] = {"Echo"->True};

TestRunQRMonPipelines[ pipelines_, opts:OptionsPattern[] ] :=
    Block[{echoQ, testRes, testPatt},

      echoQ = TrueQ[OptionValue[TestRunQRMonPipelines, "Echo"]];

      MapIndexed[(
        If[echoQ, Echo[ #2[[1]], Style["pipeline:", Bold, Blue]] ];

        testPatt =
            Which[
              MatchQ[ #1[[1]], QRMon[None, <||>]] && MatchQ[ #1[[-1]], _QRMonTakeRegressionFunctions],
              $QRMonFailure,

              MatchQ[ #1[[-1]], QRMonTakeOutliers ],
              _Missing | Association[(_String -> {{_?NumberQ,_?NumberQ} ..}) .. ],

              MatchQ[ #1[[-1]], QRMonTakeRegressionFunctions],
              Association[( (_String|_?NumberQ) -> _) .. ],

              MatchQ[ #1[[-1]], QRMonTakeValue ],
              None | _?ArrayQ | _TimeSeries | _TemporalData | _Association,

              True,
              $QRMonFailure

            ];

        VerificationTest[
          testRes = Fold[QRMonBind, First[#], Rest[#]];
          MatchQ[testRes, testPatt | $QRMonFailure],

          True,

          TestID -> #2[[1]]])&,
        pipelines]

    ];


End[]; (* `Private` *)

EndPackage[]