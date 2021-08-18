(*
    Computational Spec Completion Mathematica package
    Copyright (C) 2021  Anton Antonov

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
    antononcube @ posteo . net,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2021 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: ComputationalSpecCompletion *)
(* :Context: ComputationalSpecCompletion` *)
(* :Author: Anton Antonov *)
(* :Date: 2021-07-19 *)

(* :Package Version: 0.2 *)
(* :Mathematica Version: 12.3 *)
(* :Copyright: (c) 2021 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

## Problem formulation

We want to have a system that:

- Generates relevant, correct, executable programming code based natural language specifications of computational workflows

- Can automatically recognize the workflow types

- Can generate code for different programming languages and related software packages

The points above are given in order of importance; the most important are placed first.

## Examples

Consider the following -- intentionally short and non-specific -- computational workflow specifications:

```mathematica
lsCommands = {
   "Create a random dataset.",
   "Do quantile regression over findData.",
   "Make a classifier over dsTitanic."};
```

Here we generate the code -- note the only the list of commands is given to the function ComputationalSpecCompletion, [AAp1]:

```mathematica
aRes = ComputationalSpecCompletion[lsCommands];
```

Here we tabulate the code generation (templates fill-in) results:

```mathematica
ResourceFunction["GridTableForm"][List @@@ Normal[aRes], TableHeadings -> {"Spec", "Code"}]
```

## References

[AAp1] Anton Antonov, Computational workflow type classifier Mathematica package

*)


(***********************************************************)
(* Load packages                                           *)
(***********************************************************)

If[ Length[DownValues[MonadicContextualClassification`ClConUnit]] == 0,
  Echo["MonadicContextualClassification.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicContextualClassification.m"];
];

If[ Length[DownValues[MonadicQuantileRegression`QRMonUnit]] == 0,
  Echo["MonadicQuantileRegression.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"];
];

If[ Length[DownValues[MonadicStructuralBreaksFinder`QRMonFindChowTestLocalMaxima]] == 0,
  Echo["MonadicStructuralBreaksFinder.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicStructuralBreaksFinder.m"];
];

If[ Length[DownValues[MonadicLatentSemanticAnalysis`LSAMonUnit]] == 0,
  Echo["MonadicLatentSemanticAnalysis.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m"];
];

If[ Length[DownValues[MonadicSparseMatrixRecommender`SMRMonUnit]] == 0,
  Echo["MonadicSparseMatrixRecommender.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicSparseMatrixRecommender.m"];
];

(*If[ Length[DownValues[MonadicEpidemiologyCompartmentalModeling`ECMMonUnit]] == 0,*)
(*  Echo["MonadicEpidemiologyCompartmentalModeling.m", "Importing from GitHub:"];*)
(*  Import["https://raw.githubusercontent.com/antononcube/SystemModeling/master/Projects/Coronavirus-propagation-dynamics/WL/MonadicEpidemiologyCompartmentalModeling.m"];*)
(*];*)


(***********************************************************)
(* Package definition                                      *)
(***********************************************************)

BeginPackage["ComputationalSpecCompletion`"];
(* Exported symbols added here with SymbolName::usage *)

GetRawAnswers::usage = "GetRawAnswers[wfSpec, spec, opts] \
finds probability-scored answers of parameters questions for the computational workflow template wfSpec \
over the computational specification spec.";

GetAnswers::usage = "GetAnswers[wfSpec, spec, opts] \
finds precise answers of parameters questions for the computational workflow template wfSpec \
over the computational specification spec.";

ComputationalSpecCompletion::usage = "ComputationalSpecCompletion[wfSpec, spec, opts] \
finds parameters for the computational workflow template wfSpec based on the computational specification spec \
and creates corresponding executable expression.";

Begin["`Private`"];

(***********************************************************)
(* Shortcuts                                               *)
(***********************************************************)

aShortcuts = <|
  "ProgrammingEnvironment" -> "ProgrammingEnvironment",
  "Programming" -> "ProgrammingEnvironment",
  "System" -> "ProgrammingEnvironment",

  "QuantileRegression" -> "QuantileRegression",
  "QR" -> "QuantileRegression",

  "QRMon" -> "QRMon",

  "LatentSemanticAnalysis" -> "LatentSemanticAnalysis",
  "LSAMon" -> "LatentSemanticAnalysis",
  "LSA" -> "LatentSemanticAnalysis",

  "Classification" -> "Classification",
  "Classify" -> "Classification",

  "ClCon" -> "ClCon",
  "CLMon" -> "ClCon",

  "RandomTabularDataset" -> "RandomTabularDataset",
  "RandomDataset" -> "RandomTabularDataset",
  "RandomDataGeneration" -> "RandomTabularDataset",
  "RandomDatasetGeneration" -> "RandomTabularDataset",

  "Recommendations" -> "Recommendations",
  "SMRMon" -> "Recommendations"
|>;


(***********************************************************)
(* WL templates                                            *)
(***********************************************************)

aWLTemplates = <|
  "QuantileRegression" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "Module[{qrData,aQRFuncs,aQRPlotData},
            qrData = `dataset`;
            qrData = N@Which[ Head[qrData] === TemporalData, QuantityMagnitude[qrData[\"Path\"]], VectorQ[qrData], Transpose[{Range@Length@qrData, qrData}], True, qrData];
            Echo[ResourceFunction[\"RecordsSummary\"][qrData],\"data summary:\"];
            aQRFuncs = AssociationThread[ `probs`, ResourceFunction[\"QuantileRegression\"][qrData, `knots`, `probs`, InterpolationOrder->`intOrder`]];
            aQRPlotData = Prepend[(Transpose[{qrData[[All, 1]], #1 /@ qrData[[All, 1]]}] &) /@ aQRFuncs, \"data\" -> qrData];
            Echo[ListPlot[Values[aQRPlotData], Joined -> Prepend[Table[True, Length[aQRPlotData]-1], False], PlotLegends -> Keys[aQRPlotData], PlotTheme -> \"Detailed\", FrameLabel -> {\"Regressor\", \"Value\"}, ImageSize -> Medium],\"regression quantiles:\"];
            Echo[Map[Function[{qFunc},
             DateListPlot[
              Map[{#[[1]], (qFunc[#[[1]]] - #[[2]])/#[[2]]} &, qrData], Joined -> False, PlotRange -> All, Filling -> Axis, PlotTheme -> \"Detailed\", ImageSize -> Medium]], aQRFuncs],\"errors:\"];
           ]",

  "QRMon" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "qrObj=
        QRMonUnit[`dataset`]\[DoubleLongRightArrow]
        QRMonEchoDataSummary[]\[DoubleLongRightArrow]
        QRMonQuantileRegression[`knots`, `probs`, InterpolationOrder->`intOrder`]\[DoubleLongRightArrow]
        QRMonPlot[\"DateListPlot\"->`dateListPlotQ`,PlotTheme->\"Detailed\"]\[DoubleLongRightArrow]
        QRMonErrorPlots[\"RelativeErrors\"->`relativeErrorsQ`,\"DateListPlot\"->`dateListPlotQ`,PlotTheme->\"Detailed\"];",

  "LatentSemanticAnalysis" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "lsaObj=
      LSAMonUnit[`textData`] \[DoubleLongRightArrow]
      LSAMonMakeDocumentTermMatrix[ \"StemmingRules\" -> `stemmingRules`, \"StopWords\" -> `stopWords`] \[DoubleLongRightArrow]
      LSAMonEchoDocumentTermMatrixStatistics[\"LogBase\" -> 10] \[DoubleLongRightArrow]
      LSAMonApplyTermWeightFunctions[\"GlobalWeightFunction\" -> \"`globalWeightFunction`\", \"LocalWeightFunction\" -> \"`localWeightFunction`\", \"NormalizerFunction\" -> \"`normalizerFunction`\"] \[DoubleLongRightArrow]
      LSAMonExtractTopics[\"NumberOfTopics\" -> `numberOfTopics`, Method -> \"`method`\", \"MaxSteps\" -> `maxSteps`, \"MinNumberOfDocumentsPerTerm\" -> `minNumberOfDocumentsPerTerm`] \[DoubleLongRightArrow]
      LSAMonEchoTopicsTable[\"NumberOfTerms\" -> `topicsTableNumberOfTerms`] \[DoubleLongRightArrow]
      LSAMonEchoStatisticalThesaurus[ \"Words\" -> `statThesaurusWords`];",

  "Classification" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "Module[{clData,clDataTraining,clDataTesting,clObj,clCMObj,clMeasurements},
            clData = ClConToNormalClassifierData[`data`];
            {clDataTraining, clDataTesting} = TakeDrop[clData, Floor[`splitRatio` * Length[clData]]];
            clObj = Classify[clDataTraining, Method -> \"`method`\"];
            clCMObj = ClassifierMeasurements[clObj, clDataTesting];
            Echo[ clCMObj[{\"Accuracy\", \"Precision\", \"Recall\"}], \"measurements:\"];
            clMeasurements = Intersection[clCMObj[\"Properties\"], `measurementFuncs`];
            If[ Length[clMeasurements] > 0, Echo[ clCMObj[clMeasurements], ToString[clMeasurements] <> \":\"]];
            Echo[ clCMObj[\"ConfusionMatrixPlot\"], \"confusion matrix:\"];
           ]",

  "ClCon" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "clObj=
        ClConUnit[`data`]\[DoubleLongRightArrow]
        ClConSplitData[`splitRatio`]\[DoubleLongRightArrow]
        ClConEchoDataSummary\[DoubleLongRightArrow]
        ClConMakeClassifier[\"`method`\"]\[DoubleLongRightArrow]
        ClConClassifierMeasurements[`measurementFuncs`]\[DoubleLongRightArrow]
        ClConEchoValue\[DoubleLongRightArrow]
        ClConROCPlot[`rocPlotFuncs`];",

  "RandomTabularDataset" ->
      StringTemplate[
        "ResourceFunction[\"RandomTabularDataset\"][" <>
            "{`nrow`, `ncol`}, " <>
            "\"ColumnNamesGenerator\" -> `columnNamesGenerator`, " <>
            "\"Form\" -> \"`form`\", " <>
            "\"MaxNumberOfValues\" -> `maxNumberOfValues`, " <>
            "\"MinNumberOfValues\" -> `minNumberOfValues`, " <>
            "\"RowKeys\" -> `rowKeys`" <>
            "]"],

  "Recommendations" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "smrObj=
        SMRMonUnit[]\[DoubleLongRightArrow]
        SMRMonCreate[`dataset`]\[DoubleLongRightArrow]
        SMRMonRecommendByProfile[`prof`, `nrecs`]\[DoubleLongRightArrow]
        SMRMonJoinAcross[`dataset`]\[DoubleLongRightArrow]
        SMRMonEchoValue[];"
|>;


(***********************************************************)
(* R templates                                            *)
(***********************************************************)

aRTemplates = <|
  "QuantileRegression" ->
      StringTemplate["{library(quanreg)}"],

  "QRMon" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "qrObj <-
          QRMonUnit(`dataset`) %>%
          QRMonEchoDataSummary() %>%
          QRMonQuantileRegression(df = `knots`, probabilities = `probs`, degree = `intOrder`) %>%
          QRMonPlot(datePlotQ = `dateListPlotQ` ) %>%
          QRMonErrorsPlot(relativeErrors = `relativeErrorsQ`, datePlotQ = `dateListPlotQ`)",

  "LatentSemanticAnalysis" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "lsaObj <-
          LSAMonUnit(`textData`) %>%
          LSAMonMakeDocumentTermMatrix(stemWordsQ = `stemmingRules`, stopWords = `stopWords`) %>%
          LSAMonEchoDocumentTermMatrixStatistics(logBase = 10) %>%
          LSAMonApplyTermWeightFunctions(globalWeightFunction = \"`globalWeightFunction`\", localWeightFunction = \"`localWeightFunction`\", normalizerFunction = \"`normalizerFunction`\") %>%
          LSAMonExtractTopics(numberOfTopics = `numberOfTopics`, method = \"`method`\", maxSteps = `maxSteps`, minNumberOfDocumentsPerTerm = `minNumberOfDocumentsPerTerm`) %>%
          LSAMonEchoTopicsTable(numberOfTerms = `topicsTableNumberOfTerms`, wideFormQ = TRUE) %>%
          LSAMonEchoStatisticalThesaurus(words = `statThesaurusWords`)",

  "Classification" -> StringTemplate["Not implemented"],

  "ClCon" -> StringTemplate["Not implemented"],

  "RandomTabularDataset" ->
      StringTemplate[
        "RandomDataFrame(" <>
            "nrow = `nrow`, ncol = `ncol`, " <>
            "columnNamesGenerator = `columnNamesGenerator`, " <>
            "form =  \"`form`\", " <>
            "maxNumberOfValues = `maxNumberOfValues`, " <>
            "minNumberOfValues = `minNumberOfValues`, " <>
            "rowNamesQ = `rowKeys`" <>
            ")"],

  "Recommendations" ->
      (StringTemplate @ StringReplace[#, "\n" ~~ (WhitespaceCharacter..) -> "\n"]&) @
          "smrObj <-
          SMRMonUnit() %>%
          SMRMonCreate( data = `dataset`) %>%
          SMRMonRecommendByProfile( profile = `prof`, nrecs = `nrecs`) %>%
          SMRMonJoinAcross( data = `dataset`) %>%
          SMRMonEchoValue()"
|>;

(***********************************************************)
(* All templates                                           *)
(***********************************************************)

aTemplates = <| "R" -> aRTemplates, "WL" -> aWLTemplates |>;


(***********************************************************)
(* Questions                                               *)
(***********************************************************)

aQuestions = <|

  "ProgrammingEnvironment" ->
      (KeySort @
          Join[ #,
            KeyMap[ StringReplace[#, {"package" ~~ WordBoundary -> "library", "packages" -> "libraries"}]&, #],
            KeyMap[ StringReplace[#, {"use" -> "load"}]&, #],
            KeyMap[ StringReplace[#, {"language" -> "language to use"}]&, #],
            KeyMap[ StringReplace[#, {"Which" -> "What"}]&, #]
          ] & @
          <|
            "Which language" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "lang", "ContextWordsToRemove" -> {"code", "language", "programming"} |>,
            "Which programming language" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "lang", "ContextWordsToRemove" -> {"code", "language", "programming"} |>,
            "Which computer language" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "lang", "ContextWordsToRemove" -> {"code", "language", "programming"} |>,

            "Which packages" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "packages", "ContextWordsToRemove" -> {"library", "libraries", "package", "packages"} |>,
            "Which packages to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.56, "Parameter" -> "packages", "ContextWordsToRemove" -> {"library", "libraries", "package", "packages"} |>
          |>),

  "QuantileRegression" ->
      <|
        "How many knots" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.6, "Parameter" -> "knots",
          "ContextWordsToRemove" -> {"knots"} |>,

        "What is the interpolation order" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.75, "Parameter" -> "intOrder"|>,

        "Data summary" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "dataSummaryQ"|>,

        "Which axes to rescale" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.75, "Parameter" -> {"rescaleTimeAxisQ", "rescaleValueAxisQ"}|>,

        "What kind of plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "dateListPlotQ",
          "TrueValues" -> {"true", "yes", "datelist", "date list", "datelist plot", "use date list plot"}|>,
        "Date list plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75,
          "Parameter" -> "dateListPlotQ",
          "TrueValues" -> {"true", "yes", "datelist", "date list", "datelist plot", "use date list plot"}|>,

        "Relative errors plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "relativeErrorsQ"|>,
        "Absolute errors plot" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "absoluteErrorsQ"|>,

        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.40, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Which data to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.40, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"data", "dataset"}|>,
        "Which time series to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.4, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"time series"}|>,
        "Over which" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Over what" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset",
          "ContextWordsToRemove" -> {"dataset", "data"}|>,

        "Which probabilities" -> <|"TypePattern" -> {_?NumericQ ..}, "Threshold" -> 0.7, "Parameter" -> "probs",
          "ContextWordsToRemove" -> {"probabilities"}|>,
        "Which regression quantiles" -> <|"TypePattern" -> {_?NumericQ ..}, "Threshold" -> 0.6, "Parameter" -> "probs",
          "ContextWordsToRemove" -> {"regression quantiles"}|>
      |>,

  "LatentSemanticAnalysis" ->
      <|
        "Apply stemming" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "stemmingRules"|>,

        "How many topics" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.75, "Parameter" -> "numberOfTopics",
          "ContextWordsToRemove" -> {"topics"}|>,

        "Which method" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "method",
          "ContextWordsToRemove" -> {"method", "algorithm"}|>,
        "Which dimension reduction method" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "method",
          "ContextWordsToRemove" -> {"method", "algorithm"}|>,
        "Which topic extraction method" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "method",
          "ContextWordsToRemove" -> {"method", "algorithm"}|>,

        "Show topics table" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "showTopicsTableQ"|>,
        "Number of terms in the topics table" -> <|"TypePattern" -> _?BooleanQ, "Threshold" -> 0.75, "Parameter" -> "topicsTableNumberOfTerms"|>,

        "Which words to use for statistical thesaurus" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.15, "Parameter" -> "statThesaurusWords"|>,
        "Thesaurus words" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.15, "Parameter" -> "statThesaurusWords"|>,
        "Thesaurus words to show" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.15, "Parameter" -> "statThesaurusWords"|>,
        "Statistical thesaurus words" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.75, "Parameter" -> "statThesaurusWords"|>,

        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "textData"|>,
        "Which text corpus" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "textData"|>,
        "Which collection of texts" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "textData"|>
      |>,

  "Classification" ->
      <|
        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Which data to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "For which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "For which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Using which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,
        "Over which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "data", "ContextWordsToRemove" -> {"dataset", "data"}|>,

        "What is the split ratio" -> <|"TypePattern" -> _?NumericQ, "Threshold" -> 0.75, "Parameter" -> "splitRatio"|>,
        "Which split ratio to use" -> <|"TypePattern" -> _?NumericQ, "Threshold" -> 0.75, "Parameter" -> "splitRatio"|>,
        "Training vs testing data  ratio" -> <|"TypePattern" -> _?NumericQ, "Threshold" -> 0.75, "Parameter" -> "splitRatio"|>,

        "Which classifier method" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,
        "What kind of classifier" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,
        "How to classify" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,
        "Which classifier algorithm" -> <|"TypePattern" -> _String, "Threshold" -> 0.66, "Parameter" -> "method"|>,

        "Classifier measurements" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "measurementFuncs",
          "ContextWordsToRemove" -> {"measurements", "ROC functions", "classifier"}|>,
        "Which evaluation metrics" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "measurementFuncs",
          "ContextWordsToRemove" -> {"measurements", "ROC functions", "classifier"}|>,
        "Which measurements" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "measurementFuncs",
          "ContextWordsToRemove" -> {"measurements", "ROC functions", "classifier"}|>,

        "Which ROC functions" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "rocPlotFuncs"|>,
        "Which ROC plot functions" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "rocPlotFuncs"|>
      |>,

  "RandomTabularDataset" ->
      <|
        "How many rows" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "nrow"|>,
        "What number of rows" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "nrow"|>,
        "Rows count" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "nrow"|>,

        "How many columns" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "ncol"|>,
        "What number of columns" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "ncol"|>,
        "Columns count" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "ncol"|>,

        "Max number values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,
        "Maximum number of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,
        "Number of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,
        "At most how many of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "maxNumberOfValues"|>,

        "Min number values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "minNumberOfValues"|>,
        "Minimum number of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "minNumberOfValues"|>,
        "At least how many of values" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "minNumberOfValues"|>,

        "Which form" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,
        "Which format" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,
        "What kind of form" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,
        "What kind of format" -> <|"TypePattern" -> _String, "Threshold" -> 0.5, "Parameter" -> "form"|>,

        "What is the column name generator" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "columnNamesGenerator"|>,
        "Which is the column name generator" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "columnNamesGenerator"|>,
        "How to generate the column names" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.5, "Parameter" -> "columnNamesGenerator"|>,

        "What are the value generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>,
        "What are the generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>,
        "Which value generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>,
        "Which generators" -> <|"TypePattern" -> {_String..}, "Threshold" -> 0.5, "Parameter" -> "generators"|>
      |>,

  "Recommendations" ->
      <|
        "Which dataset to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Which data to use" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Over which data" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,
        "Over which dataset" -> <|"TypePattern" -> _String, "Threshold" -> 0.35, "Parameter" -> "dataset"|>,

        "Which profile to use" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "prof"|>,
        "Which profile" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "prof"|>,
        "What is the profile" -> <|"TypePattern" -> {_String ..}, "Threshold" -> 0.35, "Parameter" -> "data"|>,

        "How many recommendations" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>,
        "What number of top recommendations" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>,
        "How many results" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>,
        "What number of top results" -> <|"TypePattern" -> _Integer, "Threshold" -> 0.35, "Parameter" -> "nrecs"|>
      |>
|>;

aQuestions = Join[aQuestions, <|"ClCon" -> aQuestions["Classification"], "QRMon" -> aQuestions["QuantileRegression"]|>];


(***********************************************************)
(* Defaults                                                *)
(***********************************************************)

aDefaults = <|
  "ProgrammingEnvironment" -> <|
    "Language" -> "WL",
    "Packages" -> None
  |>,

  "QuantileRegression" -> <|
    "knots" -> 12,
    "probs" -> {0.25, 0.5, 0.75},
    "intOrder" -> 3,
    "rescaleTimeAxisQ" -> False,
    "rescaleValueAxisQ" -> False,
    "dateListPlotQ" -> False,
    "dataset" -> None,
    "relativeErrorsQ" -> False
  |>,

  "LatentSemanticAnalysis" -> <|
    "globalWeightFunction" -> "IDF",
    "localWeightFunction" -> "None",
    "maxSteps" -> 16,
    "method" -> "SVD",
    "minNumberOfDocumentsPerTerm" -> 20,
    "normalizerFunction" -> "Cosine",
    "numberOfTopics" -> 40,
    "removeStopWordsQ" -> True,
    "showTopicsTableQ" -> True,
    "statThesaurusWords" -> None,
    "stemmingRules" -> Automatic,
    "stopWords" -> Automatic,
    "textData" -> None,
    "thesaurusWords" -> None,
    "topicsTableNumberOfTerms" -> 10|>,

  "Classification" -> <|
    "data" -> None,
    "splitRatio" -> 0.75,
    "method" -> "LogisticRegression",
    "measurementFuncs" -> "{\"Accuracy\", \"Precision\", \"Recall\"}",
    "rocPlotFuncs" -> "{\"FPR\", \"TPR\"}"
  |>,

  "RandomTabularDataset" -> <|
    "nrow" -> Automatic,
    "ncol" -> Automatic,
    "columnNamesGenerator" -> Automatic,
    "generators" -> Automatic,
    "form" -> "Wide",
    "maxNumberOfValues" -> Automatic,
    "minNumberOfValues" -> Automatic,
    "pointwiseGeneration" -> False,
    "rowKeys" -> False
  |>,

  "Recommendations" -> <|
    "dataset" -> None,
    "prof" -> None,
    "nrecs" -> 12
  |>

|>;

aDefaults = Join[aDefaults, <|"ClCon" -> aDefaults["Classification"], "QRMon" -> aDefaults["QuantileRegression"]|>];


(***********************************************************)
(* GetRawAnswers                                           *)
(***********************************************************)

Clear[GetRawAnswers];

GetRawAnswers::nwft =
    "The first argument is an unknown workflow type. The first argument is expected to be one of `1`.";

Options[GetRawAnswers] = Options[FindTextualAnswer];

GetRawAnswers[workflowTypeArg_String, command_String, nAnswers_Integer : 4, opts : OptionsPattern[]] :=
    Block[{workflowType = workflowTypeArg, aRes},

      workflowType = workflowType /. aShortcuts;

      If[! MemberQ[Values[aShortcuts], workflowType],
        Message[GetRawAnswers::nwft, Union[Keys[aShortcuts], Values[aShortcuts]]];
        Return[$Failed]
      ];

      aRes =
          Association@
              Map[# ->
                  FindTextualAnswer[command, #, nAnswers, {"String", "Probability"},
                    opts] &, Keys@aQuestions[workflowType]];

      Map[Association[Rule @@@ #] &, aRes]
    ];


(***********************************************************)
(* GetAnswers                                              *)
(***********************************************************)

ClearAll[TakeLargestKey];
TakeLargestKey[args__] := StringTrim@First@Keys@TakeLargest[args];

ClearAll[RemoveContextWords];
RemoveContextWords[s : (_String | {_String ..}), {} ] := s;
RemoveContextWords[s : (_String | {_String ..}), words : {_String..} ] :=
    StringTrim[StringReplace[ s, Thread[words -> ""] ]];

Clear[GetAnswers];
Options[GetAnswers] = Join[ Options[GetRawAnswers], {"RemoveByThreshold" -> True}];
GetAnswers[workflowTypeArg_String, command_String, nAnswers_Integer : 4, opts : OptionsPattern[]] :=
    Block[{workflowType = workflowTypeArg, aRes, aParameterQuestions, parVal},

      (*
       We have multiple questions for each parameter in order to capture relevant answers
       from different types of phrasings.
       We hope that the (1) multiplicity of questions and (2) passing threshold and redundant words per question
       would suffice to get correct answers (most of the time.)
      *)

      (*"Normalize" the specified workflow type with replacement rules.*)
      workflowType = workflowType /. aShortcuts;

      (*Get raw answers.*)
      (*For each question we get an association of scored answers. The answers are keys. *)
      aRes = GetRawAnswers[workflowType, command, nAnswers, FilterRules[{opts}, Options[GetRawAnswers]]];
      If[TrueQ[aRes === $Failed],
        Return[$Failed]
      ];

      (*Filter out candidates with too low probabilities.*)
      (*For each question apply the threshold filtering.*)
      If[ TrueQ[OptionValue[GetAnswers, "RemoveByThreshold"]],
        aRes =
            Association@
                KeyValueMap[
                  Function[{k, v},
                    k -> Select[v, # >= aQuestions[workflowType, k, "Threshold"] &]], aRes];
        aRes = Select[aRes, Length[#] > 0 &];
      ];

      (*Remove specified "redundant" words.*)
      (*For each question and each key/answer replace redundant words with empty string.*)
      aRes =
          Association@
              KeyValueMap[
                Function[{k, v},
                  k ->
                      KeySelect[
                        KeyMap[RemoveContextWords[#, Lookup[aQuestions[workflowType, k], "ContextWordsToRemove", {}]]&, v],
                        StringLength[#] > 0&
                      ]
                ], aRes];
      aRes = Select[aRes, Length[#] > 0 &];

      (*Group the questions per parameter.*)
      aParameterQuestions = GroupBy[aQuestions[workflowType], #["Parameter"] &, Keys];
      (*aParameterCandidateValues=Map[Merge[Values@KeyTake[aRes,#],Max]&, aParameterQuestions];*)

      (*For each parameter and each question of that parameter extract the top candidate parameter value and associated probability.*)
      aRes =
          Map[
            KeyValueMap[
              Function[{k, v},
                parVal =
                    Switch[
                      ToString[aQuestions[workflowType, k, "TypePattern"]],

                      "{_String..}",

                      Map["\"" <> # <> "\"" &,
                        Select[StringTrim[StringSplit[TakeLargestKey[v, 1], {",", "and"}]], StringLength[#] > 0 &]],

                      "{_?NumericQ..}" | "{_Integer..}",

                      ToExpression /@ Select[StringTrim[StringSplit[TakeLargestKey[v, 1], {",", "and"}]], StringLength[#] > 0&],

                      "_?NumericQ" | "_Integer",
                      ToExpression[TakeLargestKey[v, 1]],

                      "_?BooleanQ" | "(True|False)" | "(False|True)",
                      MemberQ[
                        If[KeyExistsQ[aQuestions[workflowType, k], "TrueValues"], aQuestions[workflowType, k, "TrueValues"], {"true", "false"}],
                        ToLowerCase[TakeLargestKey[v, 1]]
                      ],

                      _,
                      TakeLargestKey[v, 1]
                    ];
                parVal -> TakeLargest[v, 1][[1]]
              ],
              KeyTake[aRes, Flatten[{#}]]
            ] &,
            aParameterQuestions
          ];

      (*For each parameter take the parameter value candidate with the largest probability *)

      aRes = TakeLargestBy[#, #[[2]] &, 1][[1]] & /@ Select[aRes, Length[#] > 0 &];

      (*Drop the associated probabilities*)
      Keys /@ aRes
    ];


(***********************************************************)
(* ComputationalSpecCompletion                             *)
(***********************************************************)

ClearAll[ComputationalSpecCompletion];

Options[ComputationalSpecCompletion] =
    Join[
      Options[GetAnswers],
      {"ProgrammingLanguage" -> "WL", "AvoidMonads" -> False, "AssociationResult" -> False, "UserID" -> None}
    ];

ComputationalSpecCompletion::plang = "The value of the option \"ProgrammingLanguage\" is expected to be one of `1`.";
ComputationalSpecCompletion::aulang = "The automatic programming language detection failed. Continuing by using \"WL\".";
ComputationalSpecCompletion::nargs = "If one argument is given then the first argument is expected to be a string or a list of strings. \
If two arguments are given then the first argument is expected to be a classifier function or Automatic, \
and the second argument is expected to be a string or a list of strings.";

ComputationalSpecCompletion["Data"] :=
    <|
      "Templates" -> aTemplates,
      "Questions" -> aQuestions,
      "Defaults" -> aDefaults,
      "Shortcuts" -> aShortcuts
    |>;

ComputationalSpecCompletion["Templates"] := ComputationalSpecCompletion["Data"]["Templates"];

ComputationalSpecCompletion["Questions"] := ComputationalSpecCompletion["Data"]["Questions"];

ComputationalSpecCompletion["Defaults"] := ComputationalSpecCompletion["Data"]["Defaults"];

ComputationalSpecCompletion["Shortcuts"] := ComputationalSpecCompletion["Data"]["Shortcuts"];

ComputationalSpecCompletion[ commands : ( _String | {_String..} ), opts : OptionsPattern[]] :=
    ComputationalSpecCompletion[Automatic, commands, opts];

ComputationalSpecCompletion[ sf : (Automatic | _ClassifierFunction | _String), commands : {_String..}, opts : OptionsPattern[]] :=
    Association @ Map[ # -> ComputationalSpecCompletion[sf, #, opts]&, commands];

ComputationalSpecCompletion[Automatic, command_String, opts : OptionsPattern[]] :=
    Block[{cf},

      If[Length[DownValues[ComputationalWorkflowTypeClassifier`GetComputationalWorkflowTypeClassifier]] == 0,
        Echo["ComputationalWorkflowTypeClassifier.m", "Importing from GitHub:"];
        Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/ComputationalWorkflowTypeClassifier.m"];
      ];

      cf = ComputationalWorkflowTypeClassifier`GetComputationalWorkflowTypeClassifier[];

      If[ TrueQ[OptionValue[ComputationalSpecCompletion, "AvoidMonads"]],
        ComputationalSpecCompletion[ cf[command], command, opts],
        (*ELSE*)
        ComputationalSpecCompletion[ cf[command] /. {"Classification" -> "ClCon", "QuantileRegression" -> "QRMon"}, command, opts]
      ]
    ];

ComputationalSpecCompletion[cf_ClassifierFunction, command_String, opts : OptionsPattern[]] :=
    ComputationalSpecCompletion[ cf[command], command, opts];

ComputationalSpecCompletion[workflowTypeArg_String, command_String, opts : OptionsPattern[]] :=
    Block[{workflowType = workflowTypeArg, userID, lang, aRes, code, codeExpr},

      userID = OptionValue[ComputationalSpecCompletion, "UserID"];
      If[ MemberQ[{None, Automatic}, userID], userID = ""];
      userID = ToString[userID];

      lang = OptionValue[ComputationalSpecCompletion, "ProgrammingLanguage"];
      If[ TrueQ[lang === Automatic],
        aRes = Join[aDefaults["ProgrammingEnvironment"], GetAnswers["ProgrammingEnvironment", command]];
        lang =
            Which[
              Length[StringCases[ aRes["Language"], WordBoundary ~~ ("WL" | "Mathematica") ~~ WordBoundary]] > 0, "WL",
              Length[StringCases[ aRes["Language"], WordBoundary ~~ ("R") ~~ WordBoundary]] > 0, "R",
              True,
              Message[ComputationalSpecCompletion::aulang];
              "WL"
            ]
      ];
      If[ TrueQ[StringQ[lang] && ToLowerCase[lang] == "mathematica"], lang = "WL"];
      If[ !StringQ[lang] || !MemberQ[ {"R", "WL"}, ToUpperCase[lang] ],
        Message[ComputationalSpecCompletion::plang, {"R", "WL", Automatic}];
        lang = "WL"
      ];

      workflowType = workflowType /. aShortcuts;

      aRes = GetAnswers[workflowType, command, FilterRules[{opts}, Options[GetAnswers]]];

      If[TrueQ[aRes === $Failed],
        Return[$Failed]
      ];

      code = aTemplates[lang][workflowType][Join[aDefaults[workflowType], aRes]];

      If[ lang == "WL",
        codeExpr = ToExpression["Hold[" <> code <> "]"],
        (*ELSE*)
        code =
            StringReplace[
              code,
              {
                WordBoundary ~~ "Automatic" ~~ WordBoundary -> "NULL",
                WordBoundary ~~ "True" ~~ WordBoundary -> "TRUE",
                WordBoundary ~~ "False" ~~ WordBoundary -> "FALSE",
                "{" ~~ x : (Except[Characters["{}"]]..) ~~ "}" :> "c(" <> x <> ")"
              }];
        codeExpr = "parse( text = '" <> code <> "')"
      ];

      If[ TrueQ[OptionValue[ComputationalSpecCompletion, "AssociationResult"]],
        <|
          "CODE" -> code,
          "USERID" -> userID,
          "DSLTARGET" -> lang <> "::" <> workflowType,
          "DSL" -> workflowType,
          "DSLFUNCTION" -> With[{wf = workflowType, l = lang, am = TrueQ[OptionValue[ComputationalSpecCompletion, "AvoidMonads"]]},
            ToString[ComputationalSpecCompletion[wf, #,
              "ProgrammingLanguage" -> l,
              "AvoidMonads" -> am,
              "AssociationResult" -> True]&]
          ]
        |>,
        (*ELSE*)
        codeExpr
      ]
    ];

ComputationalSpecCompletion[___] :=
    Block[{},
      Message[ComputationalSpecCompletion::nargs];
      $Failed
    ];

End[];

EndPackage[];