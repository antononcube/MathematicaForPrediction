(*
    Monadic contextual classification random pipelines Mathematica unit tests
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

(* :Title: MonadicContextualClassification-Random-Pipelines-Unit-Tests *)
(* :Context: MonadicContextualClassification-Random-Pipelines-Unit-Tests` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-05-11 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

    Generation and execution of ClCon random test pipelines.

    Here is how to generate pipelines:

        SeedRandom[234];
        pipelines = MakeClConRandomPipelines[50];
        Length[pipelines]


    Here is how to run generated pipelines as verification tests:

        AbsoluteTiming[
          res = TestRunClConPipelines[pipelines, "Echo" -> True]
        ]


    Here is how to make test report and examine it:

        rpTRObj = TestReport[res]
        Column /@ (Normal /@ rpTRObj["TestsFailed"]) // TabView


    Note the the verification tests are made to match an expected combined/complicated pattern.
    That pattern catches output that correspond to:

    1. $ClConFailure,
    2. measurements results obtained with ClConClassifierMeasurements or ClConClassifierMeasurementsByThreshold,
    3. ROC data obtained by ClConROCData,
    4. summaries obtained ClConSummarizeData or ClConSummarizeDataLongForm.

    That list can be derived by observing the stages-before-the-last in MakeClConRandomPipelines.


    Anton Antonov,
    Windermere, Florida, USA,
    2018-05-11

*)

If[Length[SubValues[MonadicContextualClassification`ClConSplitData]] == 0,
  Echo["MonadicContextualClassification.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicContextualClassification.m"]
];

BeginPackage["MonadicContextualClassificationRandomPipelinesUnitTests`"]

MakeOddQData::usage = "Make random test data."

MakeClConRandomPipelines::usage = "Make ClCon random pipelines."

TestRunClConPipelines::usage = "Run ClCon pipelines using VerificationTest."

Begin["`Private`"]

Needs["MonadicContextualClassification`"];

ClearAll[MakeOddQData, MakeClConRandomPipelines, TestRunClConPipelines];

MakeOddQData[{rmin_Integer, rmax_Integer}, n_Integer] :=
    Block[{ ds, mlrData },

      ds = RandomInteger[{rmin, rmax}, {n}];

      ds = Dataset[ Transpose[{ds, Mod[ds, 3], Last@*IntegerDigits /@ ds, OddQ /@ ds}]];

      ds = Dataset[ds[All, AssociationThread[{"number", "feature1", "feature2", "label"}, #] &]];

      mlrData = ClConToNormalClassifierData[ds];

      {ds, mlrData}
    ];

MakeClConRandomPipelines[n_Integer] :=
    Block[{n1, n2, n3},
      n1 = Floor[3/4 * n];
      n2 = Floor[1/2 * (n - n1)];
      n3 = n - n1 - n2;
      MakeClConRandomPipelines[{n1, n2, n3}]
    ];

MakeClConRandomPipelines[{n_Integer, n2_Integer, n3_Integer}] :=
    MakeClConRandomPipelines[MakeOddQData[ {0,1000}, 400], {n, n2, n3}];

MakeClConRandomPipelines[{ds_Dataset, mlrData:{_Rule..}}, {n_Integer, n2_Integer, n3_Integer} ] :=
    Module[{stage1, stage2, stage3, stage4, stage5, stage3a, stage4a, stage3b, stage4b,
      allStages, allStages2, allStages3, pipelines},

      stage1 = {ClConUnit[], ClConUnit[ds], ClConUnit[mlrData]};
      (*stage1={ClConUnit[ds],ClConUnit[mlrData]};*)

      stage2 = {
        ClConSplitData[0.7],
        ClConSplitData[0.7, 0.2],
        ClConSplitData[1], ClConSplitData[1, 0.1],
        ClConSetTrainingData[RandomSample[ds, Floor[0.8*Length[ds]]]],
        ClConSetTrainingData[ RandomSample[mlrData, Floor[0.8*Length[ds]]]],
        ClConSetTestData[RandomSample[ds, Floor[0.2*Length[ds]]]],
        ClConSetTestData[RandomSample[mlrData, Floor[0.2*Length[ds]]]],
        ClConSetValidationData[RandomSample[ds, Floor[0.1*Length[ds]]]],
        ClConSetValidationData[RandomSample[mlrData, Floor[0.1*Length[ds]]]]
      };

      stage3 = {ClConMakeClassifier, ClConMakeClassifier[],
        ClConMakeClassifier[Automatic],
        ClConMakeClassifier["LogisticRegression"],
        ClConMakeClassifier["NearestNeighbors"],
        ClConMakeClassifier[Method -> {"RandomForest", "TreeNumber" -> 200}],
        ClConMakeClassifier[{"NearestNeighbors", 0.8, 5, RandomChoice}]};

      stage4 = {
        ClConClassifierMeasurements[{"Accuracy", "Precision","Recall"}],
        ClConROCData,
        ClConClassifierMeasurementsByThreshold[{"Accuracy", "Precision",  "Recall"}, "a" -> 0.4]};

      stage5 = {ClConTakeValue};

      stage3a = {
        ClConSummarizeData,
        ClConSummarizeData[Thread -> True],
        ClConSummarizeDataLongForm};

      stage4a = {ClConTakeValue};

      stage3b = {
        ClConAssignVariableNames,
        ClConAssignVariableNames[ToString /@ Range[Length[ds[1]]]]};

      stage4b = {ClConTakeValue};

      allStages = {{1, 1} -> stage1, {1, 5} -> stage2, {1, 1} -> stage3, {1, 1} -> stage4, {1, 1} -> stage5};
      allStages2 = {{1, 1} -> stage1, {1, 5} -> stage2, {1, 2} -> stage3a, {1, 1} -> stage4a};
      allStages3 = {{1, 1} -> stage1, {1, 5} -> stage2, {1, 2} -> stage3b, {1, 1} -> stage4b};

      pipelines =
          Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages], {30}];
      pipelines =
          Join[pipelines,
            Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages2], {10}]];
      pipelines =
          Join[pipelines,
            Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages3], {10}]];

      pipelines
    ];

Options[TestRunClConPipelines] = {"Echo"->True};

TestRunClConPipelines[ pipelines_, opts:OptionsPattern[] ] :=
    Block[{echoQ, testRes},

      echoQ = TrueQ[OptionValue[TestRunClConPipelines, "Echo"]];

      MapIndexed[(
        If[echoQ, Echo[ #2[[1]], Style["pipeline:", Bold, Blue]] ];

        VerificationTest[
            testRes = Fold[ClConBind, First[#], Rest[#]];
            MatchQ[testRes,
              Association[(_ -> {_?ROCAssociationQ ..}) ..] |
                  (x_Association /; (Length[Intersection[Keys[x], {"Accuracy", "Precision", "Recall"}]] == 3)) |
                  {(_ -> ({_Column ..} | {})) ..} |
                  $ClConFailure],

          True,

          TestID -> #2[[1]]])&,
        pipelines]

    ];

End[] (* `Private` *)

EndPackage[]