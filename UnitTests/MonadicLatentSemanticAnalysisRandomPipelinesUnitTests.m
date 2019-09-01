(*
    Monadic Latent Semantic Analysis random pipelines Mathematica unit tests
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

(* :Title: MonadicLatentSemanticAnalysisRandomPipelinesUnitTests *)
(* :Context: MonadicLatentSemanticAnalysisRandomPipelinesUnitTests` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-09-01 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: unit test, randomly generated, latent semantic analysis, LSA, NLP *)
(* :Discussion:

    Generation and execution of LSAMon random test pipelines.

    Here is how to generate pipelines:

        SeedRandom[234];
        pipelines = MakeLSAMonRandomPipelines[50];
        Length[pipelines]


    Here is how to run generated pipelines as verification tests:

        AbsoluteTiming[
          res = TestRunLSAMonPipelines[pipelines, "Echo" -> True]
        ]


    Here is how to make test report and examine it:

        rpTRObj = TestReport[res]
        Column /@ (Normal /@ rpTRObj["TestsFailed"]) // TabView


    Note the the verification tests are made to match an expected combined/complicated pattern.
    That pattern catches output that correspond to:

    1. $LSAMonFailure,
    2. outliers association (with keys "bottomOutliers" and "topOutliers"),
    3. regression functions association (with keys that are quantile numbers or "mean"),
    4. data arrays or time series (obtained with MovingAverage, etc.).

    That list can be derived by observing the stages-before-the-last in MakeLSAMonRandomPipelines.


    Anton Antonov,
    Windermere, Florida, USA,
    2019-09-01

*)

If[Length[SubValues[MonadicLatentSemanticAnalysis`LSAMonExtractTopics]] == 0,
  Echo["MonadicLatentSemanticAnalysis.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m"]
];

BeginPackage["MonadicLatentSemanticAnalysisRandomPipelinesUnitTests`"];
(* Exported symbols added here with SymbolName::usage *)

MakeLSAMonTestData::usage = "Make random test data.";

MakeLSAMonRandomPipelines::usage = "Make LSAMon random pipelines.";

TestRunLSAMonPipelines::usage = "Run LSAMon pipelines using VerificationTest.";

Begin["`Private`"];

Needs["MathematicaForPredictionUtilities`"];
Needs["MonadicLatentSemanticAnalysis`"];

ClearAll[MakeDistributionData, MakeLSAMonRandomPipelines, TestRunLSAMonPipelines];

MakeLSAMonTestData[] :=
    Block[{ textHamlet, aHamlet, aStateOfUnionSpeeches },
      textHamlet = ToString /@ Flatten[Import["https://raw.githubusercontent.com/antononcube/MathematicaVsR/master/Data/MathematicaVsR-Data-Hamlet.csv"]];
      aHamlet = ToAutomaticKeysAssociation[textHamlet];

      aStateOfUnionSpeeches = Association[Import["/Volumes/Macintosh HD/Users/antonov/MathematicaVsR/Data/MathematicaVsR-Data-StateOfUnionSpeeches.JSON"]];

      { aHamlet, RandomSample[ aStateOfUnionSpeeches, 20 ] }
    ];

MakeLSAMonRandomPipelines[n_Integer] :=
    Block[{n1, n2},
      n1 = Floor[3/4 * n];
      n2 = (n - n1);
      MakeLSAMonRandomPipelines[{n1, n2}]
    ];

MakeLSAMonRandomPipelines[{n_Integer, n2_Integer}] :=
    MakeLSAMonRandomPipelines[ MakeLSAMonTestData[], {n, n2} ];

MakeLSAMonRandomPipelines[ {aHamlet_Association, aStateOfUnionSpeeches_Association}, {n_Integer, n2_Integer} ] :=
    Module[{stopWords, stage1, stage2, stage3, stage4, stage5, stage6, stage7, stage8,
      allStages, allStages2, pipelines},

      stopWords = Complement[ DictionaryLookup["*"], DeleteStopwords[DictionaryLookup["*"]] ];

      stage1 = {LSAMonUnit[], LSAMonUnit[aHamlet], LSAMonUnit[aStateOfUnionSpeeches]};

      stage2 = {
        LSAMonEchoDataSummary,
        LSAMonSetDocuments[aHamlet],
        LSAMonSetDocuments[aStateOfUnionSpeeches]
      };

      stage3 = {
        LSAMonMakeDocumentTermMatrix,
        LSAMonMakeDocumentTermMatrix[],
        LSAMonMakeDocumentTermMatrix[Automatic, Automatic],
        LSAMonMakeDocumentTermMatrix[{}, stopWords]
      };

      stage4 = {
        LSAMonEchoDocumentsStatistics,
        LSAMonEchoDataSummary
      };

      stage5 = {
        LSAMonApplyTermWeightFunctions,
        LSAMonApplyTermWeightFunctions[],
        LSAMonApplyTermWeightFunctions["IDF"],
        LSAMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
        LSAMonApplyTermWeightFunctions["NormalizerFunction"->"Cosine"],
        LSAMonApplyTermWeightFunctions["GlobalWeightFunction"->"IDF"],
        LSAMonApplyTermWeightFunctions["LocalWeightFunction"->"Binary"],
        LSAMonApplyTermWeightFunctions["LocalWeightFunction"->"BlahBlah"]
      };

      stage6 = {
        LSAMonExtractTopics[12],
        LSAMonExtractTopics["Topics" -> 16, "Method" -> "SVD" ],
        LSAMonExtractTopics["Topics" -> 16, "Method" -> "NNMF" ],
        LSAMonExtractTopics["Topics" -> -1, "Method" -> "NNMF", "MaxSteps" -> 12 ],
        LSAMonExtractTopics["Topics" -> 2, "Method" -> "NNMF", "MaxSteps" -> Automatic ],
        LSAMonExtractTopics["Topics" -> 2000, "Method" -> "SVD", "MaxSteps" -> 12 ],
        LSAMonExtractTopics["Topics" -> 16, "Method" -> "SVD", "MinNumberOfDocumentsPerTerm" -> 12 ],
        LSAMonExtractTopics["Topics" -> 16, "Method" -> "SVD", "MinNumberOfDocumentsPerTerm" -> 1200 ],
        LSAMonExtractTopics["Topics" -> 16, "Method" -> "SVD", "MinNumberOfDocumentsPerTerm" -> -12 ]
      };

      stage7 = {
        LSAMonExtractStatisticalThesaurus,
        LSAMonMakeTopicsTable,
        LSAMonFindMostImportantDocuments
      };

      stage8 = {
        LSAMonEchoTopicsTable,
        LSAMonEchoStatisticalThesaurus,
        LSAMonTakeValue
      };

      allStages = Map[ {1, Length[#]} -> # &, {stage1, stage2, stage3, stage4, stage5, stage6, stage7, stage8} ];

      allStages2 = {{1, 1} -> stage1, {2, Length[stage2]} -> stage2, {0, Length[stage3]} -> stage3, {1, Length[stage5]} -> stage5, {1, Length[stage6]} -> stage6 };

      pipelines =
          Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages], {n}];

      pipelines =
          Join[pipelines,
            Table[Join @@ Map[RandomChoice[#[[2]], RandomInteger[#[[1]]]] &, allStages2], {n2}]];

      pipelines
    ];

Options[TestRunLSAMonPipelines] = {"Echo"->True};

TestRunLSAMonPipelines[ pipelines_, opts:OptionsPattern[] ] :=
    Block[{echoQ, testRes, testPatt},

      echoQ = TrueQ[OptionValue[TestRunLSAMonPipelines, "Echo"]];

      MapIndexed[(
        If[echoQ, Echo[ #2[[1]], Style["pipeline:", Bold, Blue]] ];

        testPatt =
            Which[

              MatchQ[ #1[[-1]], LSAMonTakeValue ],
              None | _Association,

              True,
              $LSAMonFailure

            ];

        VerificationTest[
          testRes = Fold[LSAMonBind, First[#], Rest[#]];
          MatchQ[testRes, testPatt | $LSAMonFailure],

          True,

          TestID -> #2[[1]]])&,
        pipelines]

    ];


End[]; (* `Private` *)

EndPackage[]