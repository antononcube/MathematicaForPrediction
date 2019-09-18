(*
    Monadic Sparse Matrix Recommender random pipelines Mathematica unit tests
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

(* :Title: MonadicQuantileRegressionRandomPipelinesUnitTests *)
(* :Context: MonadicQuantileRegressionRandomPipelinesUnitTests` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-09-18 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: unit test, randomly generated, recommendations, linear algebra *)
(* :Discussion:

    Generation and execution of SMRMon random test pipelines.

    Here is how to generate pipelines:

        SeedRandom[234];
        pipelines = MakeSMRMonRandomPipelines[50];
        Length[pipelines]


    Here is how to run generated pipelines as verification tests:

        AbsoluteTiming[
          res = TestRunSMRMonPipelines[pipelines, "Echo" -> True]
        ]


    Here is how to make test report and examine it:

        rpTRObj = TestReport[res]
        Column /@ (Normal /@ rpTRObj["TestsFailed"]) // TabView


    Note the the verification tests are made to match an expected combined/complicated pattern.
    That pattern catches output that correspond to:

    1. $SMRMonFailure,
    2. recommendations dataset,
    3. profile dataset,
    4. association of recommendations.

    That list can be derived by observing the stages-before-the-last in MakeSMRMonRandomPipelines.

    Anton Antonov,
    Windermere, Florida, USA,
    2019-09-18

*)

If[Length[SubValues[MonadicSparseMatrixRecommender`SMRMonRecommend]] == 0,
  Echo["MonadicSparseMatrixRecommender.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicSparseMatrixRecommender.m"]
];

BeginPackage["MonadicQuantileRegressionRandomPipelinesUnitTests`"];
(* Exported symbols added here with SymbolName::usage *)

MakeSMRMonTestData::usage = "Make test data.";

MakeSMRMonRandomPipelines::usage = "Make random SMRMon pipelines.";

TestRunSMRMonPipelines::usage = "Run SMRMon pipelines using VerificationTest.";

Begin["`Private`"];

Needs["MonadicSparseMatrixRecommender`"];

ClearAll[MakeSMRMonTestData, MakeSMRMonRandomPipelines, TestRunSMRMonPipelines];

MakeSMRMonTestData[] :=
    Block[{ dfTitanic, dfMushroom },

      dfTitanic = Import["https://raw.githubusercontent.com/antononcube/MathematicaVsR/master/Data/MathematicaVsR-Data-Titanic.csv"];
      dfTitanic = Dataset[Rest[dfTitanic]][All, AssociationThread[First[dfTitanic] -> #] &];

      dfMushroom = Import["https://raw.githubusercontent.com/antononcube/MathematicaVsR/master/Data/MathematicaVsR-Data-Mushroom.csv"];
      dfMushroom = Dataset[Rest[dfMushroom]][All, AssociationThread[ First[dfMushroom] -> # ]& ];

      { dfTitanic, RandomSample[dfMushroom, 2000] }
    ];

MakeSMRMonRandomPipelines[ n_Integer ] :=
    MakeSMRMonRandomPipelines[ MakeSMRMonTestData[], n ];

MakeSMRMonRandomPipelines[ {dfTitanic_Dataset, dfMushroom_Dataset}, n_Integer ] :=
    Module[{stage1, stage2, stage3, stage4, stage5, stage6, allStages, pipelines},

      stage1 = {SMRMonUnit[], SMRMonUnit[dfTitanic], SMRMonUnit[dfMushroom]};

      stage2 = {
        SMRMonCreate,
        SMRMonCreate[dfTitanic],
        SMRMonCreate[dfTitanic, Automatic],
        SMRMonCreate[dfTitanic, "id"],
        SMRMonCreate[dfMushroom, "id"]
      };

      stage3 = {
        SMRMonEchoDataSummary,
        SMRMonEchoFunctionContext[ Dimensions /@ #matrices& ],
        SMRMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
        SMRMonApplyTermWeightFunctions
      };

      stage4 = {
        SMRMonRecommend,
        SMRMonRecommend[ Normal @ RandomSample[ dfTitanic[All, "id"], 12] ],
        SMRMonRecommend[ Normal @ RandomSample[ dfMushroom[All, "id"], 12] ],
        SMRMonRecommend[ AssociationThread[ Normal @ RandomSample[ dfMushroom[All, "id"], 7], RandomReal[{0,1}, 7] ] ],
        SMRMonRecommend[ AssociationThread[ Normal @ RandomSample[ dfMushroom[All, "id"], 6], RandomReal[{0,1}, 6] ] ],
        SMRMonRecommendByProfile[ {"male", "died"} ],
        SMRMonRecommendByProfile[ AssociationThread[ {"male", "died"}, { 1, 0.1 } ] ],
        SMRMonRecommendByProfile[ {"brown", "foul"} ],
        SMRMonRecommendByProfile[ AssociationThread[ {"brown", "foul"}, { 1, 0.1 } ] ]
      };

      stage5 = {
        SMRMonEchoValue,
        SMRMonJoinAcross[ dfTitanic, "id" ],
        SMRMonJoinAcross[ dfMushroom, "id" ],
        SMRMonJoinAcross[ dfTitanic, "id", "AsDataset" ->False ],
        SMRMonJoinAcross[ dfMushroom, "id", "AsDataset" ->False ]
      };

      stage6 = {
        SMRMonTakeValue
      };

      allStages = Map[ {1, 1} -> # &, {stage1, stage2, stage3, stage4, stage5, stage6} ];

      pipelines =
          Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages], {n}];

      pipelines
    ];

Options[TestRunSMRMonPipelines] = {"Echo"->True};

TestRunSMRMonPipelines[ pipelines_, opts:OptionsPattern[] ] :=
    Block[{echoQ, testRes, testPatt},

      echoQ = TrueQ[OptionValue[TestRunSMRMonPipelines, "Echo"]];

      MapIndexed[(
        If[echoQ, Echo[ #2[[1]], Style["pipeline:", Bold, Blue]] ];

        testPatt =
            Which[

              MatchQ[ #1[[-1]], SMRMonTakeValue ],
              None | _Association | _Dataset,

              True,
              $SMRMonFailure

            ];

        VerificationTest[
          testRes = Fold[SMRMonBind, First[#], Rest[#]];
          MatchQ[testRes, testPatt | $SMRMonFailure],

          True,

          TestID -> #2[[1]]])&,
        pipelines]

    ];


End[]; (* `Private` *)

EndPackage[]