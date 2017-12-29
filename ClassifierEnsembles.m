(*
    Classifier ensembles functions Mathematica package

    Copyright (C) 2016  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2016 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Mathematica Package *)

(* :Title: ClassifierEnsembles *)
(* :Context: ClassifierEnsembles` *)
(* :Author: Anton Antonov *)
(* :Date: 2016-10-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

    This package provides functions for creation and classification with ensembles of classifiers.
    An ensemble of classifiers is simply an Association that maps classifier IDs to classifier functions.

    Given a classifier ensemble we have the obvious option to classify a record by classifier voting.
    Each classifier returns a label, we tally the returned labels, the returned label of the ensemble is
    the label with the largest tally number.

    Since ClassifierFunction has the method "Probabilities" for a classifier ensemble we can also average
    the probabilities for each label, and return the label with the highest average probability.
    If a threshold is specified for a label, then we can pick that label as the classification result
    if its average probability is above the threshold.

    The functions in this package are especially useful when used together with functions of
    the package ROCFunctions.m. See:

      https://github.com/antononcube/MathematicaForPrediction/blob/master/ROCFunctions.m .

    An attempt to import the package ROCFunctions.m is made if definitions of its functions are not found.


    Usage example
    =============

    ## Getting data

      data = ExampleData[{"MachineLearning", "Titanic"}, "TrainingData"];
      data = ((Flatten@*List) @@@ data)[[All, {1, 2, 3, -1}]];
      trainingData = DeleteCases[data, {___, _Missing, ___}];

      data = ExampleData[{"MachineLearning", "Titanic"}, "TestData"];
      data = ((Flatten@*List) @@@ data)[[All, {1, 2, 3, -1}]];
      testData = DeleteCases[data, {___, _Missing, ___}];

    ## Create a classifier ensemble

      aCLs = EnsembleClassify[Automatic, trainingData[[All, 1 ;; -2]] -> trainingData[[All, -1]]]


    ## Classify a record

       EnsembleClassify[aCLs, testData[[1, 1 ;; -2]]]
       (* "survived" *)

       EnsembleClassifyByThreshold[aCLs, testData[[1, 1 ;; -2]], "survived" -> 2, "Votes"]
       (* "survived" *)

       EnsembleClassifyByThreshold[aCLs, testData[[1, 1 ;; -2]], "survived" -> 0.2, "ProbabilitiesMean"]
       (* "survived" *)


    ## Classify a list of records using a thershold

    ### Return "survived" if it gets at least two votes

       EnsembleClassifyByThreshold[aCLs, testData[[1 ;; 12, 1 ;; -2]], "survived" -> 2, "Votes"]

       (* {"survived", "died", "survived", "survived", "died", "survived", \
           "survived", "survived", "died", "survived", "died", "survived"} *)


    ### Return "survived" if its average probability is at least 0.7

       EnsembleClassifyByThreshold[aCLs, testData[[1 ;; 12, 1 ;; -2]], "survived" -> 0.7, "ProbabilitiesMean"]

       (* {"survived", "died", "survived", "died", "died", \
           "survived", "died", "survived", "died", "survived", "died", \
           "survived"} *)


    ## Threshold classification with ROC

    rocRange = Range[0, 1, 0.1];
    aROCs =
      Table[(cres = EnsembleClassifyByThreshold[aCLs, testData[[All, 1 ;; -2]], "survived" -> i];
             ToROCAssociation[{"survived", "died"}, testData[[All, -1]], cres]),
            {i, rocRange}];

    ROCPlot[rocRange, aROCs]


    This file was created by Mathematica Plugin for IntelliJ IDEA.

    Anton Antonov
    2016-10-12
    Winderemere, FL, 2016

*)
(*
    TODO
      1. Better error messages.
      2. Add error message for EnsembleClassifierROCData and EnsembleClassifierROCPlots.
*)

BeginPackage["ClassifierEnsembles`"]

EnsembleClassifier::usage = "Create an ensemble of classifiers over the same data. \
Returns an Association of IDs mapped to classifier functions."

EnsembleClassifierVotes::usage = "Find votes by a classifier ensemble for a record ora a list of records."

EnsembleClassifierProbabilities::usage = "Give the averaged probabilities of a classifier ensemble \
a record or a list of records."

EnsembleClassify::usage = "Classify by a classifier ensemble for a record or a list of records. \
The third argument is one of \"Votes\" or \"ProbabilitiesMean\"."

EnsembleClassifyByThreshold::usage = "Classify by a classifier ensemble for a record or a list of records. \
The third argument is a rule label->threshold. The fourth argument is one of \"Votes\" or \"ProbabilitiesMean\". \
The specified label is returned if its votes or average probability are higher or equal than \
the specified threshold."

ClassifyByThreshold::usage = "A shortcut to calling EnsembleClassifyByThreshold using a classifier function \
instead of a classifier ensemble."

EnsembleClassifierMeasurements::usage = "EnsembleClassifierMeasurements[ensCF, testData, props] \
gives measurements corresponding to props when the ensemble of classifiers ensCF is evaluated over testData. \
(Emulates ClassifierMeasurements for ensembles of classifiers.)"

ResamplingEnsembleClassifier::usage = "ResamplingEnsembleClassifier[{(_String | {_String, _?NumberQ} | {_String, _?NumberQ, _Integer}) ..}, data] \
builds ensemble classifier based on a specification."

EnsembleClassifierROCData::usage = "EnsembleClassifierROCData[ensCF, testData, thRange, targetClasses] \
returns an association of classifier ensemble ROC data."

EnsembleClassifierROCPlots::usage = "EnsembleClassifierROCPlots[ensCF, testData, thRange, targetClasses, opts___] \
returns an association of classifier ensemble ROC plots."

Begin["`Private`"]

If[Length[DownValues[ROCFunctions`ToROCAssociation]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/ROCFunctions.m"]
];

Needs["ROCFunctions`"]

Clear[EnsembleClassifier]
EnsembleClassifier::nargs =
    "The first argument is expected to match (_String|{_String..}|Automatic). \
The rest of the arguments are given to Classify.";

EnsembleClassifier[Automatic, args___] :=
    EnsembleClassifier[{"NearestNeighbors", "NeuralNetwork", "LogisticRegression",
      "RandomForest", "SupportVectorMachine", "NaiveBayes"}, args];

EnsembleClassifier[clID_String, args___] := EnsembleClassifier[{clID}, args];

EnsembleClassifier[clIDs : {_String ..}, args___] :=
    Association @ Table[cl -> Classify[args, Method -> cl], {cl, clIDs}];

EnsembleClassifier[___] := (Message[EnsembleClassify::nargs]; $Failed);


(**************************************************************)
(* Resampling classifier making                               *)
(**************************************************************)

Clear[ClassifierDataQ]
ClassifierDataQ[data_] := MatchQ[data, {Rule[_List, _] ..}] && ArrayQ[data[[All, 1]]];

Clear[ResamplingEnsembleClassifier]
ResamplingEnsembleClassifier[specs : {(_String | {_String, _?NumberQ} | {_String, _?NumberQ, _Integer} | {_String, _?NumberQ, _Integer, RandomSample|RandomChoice}) ..},
  data_?ClassifierDataQ] :=
    Block[{fullSpecs},
      fullSpecs =
          specs /. {
            m_String :> <| "method"-> m |>,
            { m_String, f_?NumberQ} :>  <| "method"->m, "samplingFraction"->f|>,
            { m_String, f_?NumberQ, n_Integer } :>  <| "method"->m, "samplingFraction"->f, "nClassifiers"->n|>,
            { m_String, f_?NumberQ, n_Integer, sf:(RandomSample|RandomChoice) } :>  <| "method"->m, "samplingFraction"->f, "nClassifiers"->n, "samplingFunction"->sf|>
          };

      ResamplingEnsembleClassifier[ fullSpecs, data ]
    ];

ResamplingEnsembleClassifier[specs:{_Association..}, data_?ClassifierDataQ ] :=
    Block[{fullSpecs, res},
      fullSpecs = Map[ Join[ <| "method"->"LogisticRegression", "sampleFraction"->0.9, "nClassifiers"->1, "samplingFunction"->RandomChoice |>, # ]&, specs];
      res =
          Map[
            Table[#["method"] <> "[" <> ToString[i] <> "," <> ToString[#["sampleFraction"]] <> "]" ->
                Classify[#["samplingFunction"][data, Floor[#["sampleFraction"]*Length[data]]], Method -> #["method"]], {i, #["nClassifiers"]}] &,
            fullSpecs];

      Association@Flatten[res, 1]
    ];

(**************************************************************)
(* Ensemble classification functions                          *)
(**************************************************************)

Clear[EnsembleClassifierVotes]
EnsembleClassifierVotes::nargs =
    "The first argument is expected to be an Association of classifier IDs to \
classifier functions. The second argument is expected to be a vector or a \
matrix.";

EnsembleClassifierVotes[cls_Association, record_?VectorQ] :=
    Association[Rule @@@ Sort[Tally[Through[Values[cls][record]]], -#[[-1]] &]];

EnsembleClassifierVotes[cls_Association, records_?MatrixQ] :=
    Map[Association[Rule @@@ Sort[Tally[#], -#[[-1]] &]] &, Transpose[Through[Values[cls][records]]]];

EnsembleClassifierVotes[___] := (Message[EnsembleClassifierVotes::nargs]; $Failed);

Clear[EnsembleClassifierProbabilities]
EnsembleClassifierProbabilities::nargs =
    "The first argument is expected to be an Association of classifier IDs to \
classifier functions. The second argument is expected to be a vector or a \
matrix.";

EnsembleClassifierProbabilities[cls_Association, record_?VectorQ] :=
    Mean[Through[Values[cls][record, "Probabilities"]]];

EnsembleClassifierProbabilities[cls_Association, records_?MatrixQ] :=
    Mean /@ Transpose[Through[Values[cls][records, "Probabilities"]]];

EnsembleClassifierProbabilities[___] := (Message[EnsembleClassifierProbabilities::nargs]; $Failed);


Clear[EnsembleClassify]
EnsembleClassify::nargs =
    "The first argument is expected to be an Association of classifier IDs to \
classifier functions. The second argument is expected to be a vector or a \
matrix. The third argument is expected to be one of \"Votes\" or \
\"ProbabilitiesMean\".";

EnsembleClassify[cls_Association, record_] := EnsembleClassify[cls, record, "Votes"];

EnsembleClassify[cls_Association, record_?VectorQ, "Votes"] :=
    First@Keys@TakeLargest[EnsembleClassifierVotes[cls, record], 1];

EnsembleClassify[cls_Association, records_?MatrixQ, "Votes"] :=
    Map[First@Keys@TakeLargest[#, 1] &, EnsembleClassifierVotes[cls, records]];

EnsembleClassify[cls_Association, record_?VectorQ, "ProbabilitiesMean"] :=
    First@Keys@
        TakeLargest[Mean[Through[Values[cls][record, "Probabilities"]]], 1];

EnsembleClassify[cls_Association, records_?MatrixQ, "ProbabilitiesMean"] :=
    Map[First@Keys@TakeLargest[#, 1] &,
      EnsembleClassifierProbabilities[cls, records]];

EnsembleClassify[___] := (Message[EnsembleClassify::nargs]; $Failed);



(**************************************************************)
(* EnsembleClassifyByThreshold                                *)
(**************************************************************)

Clear[EnsembleClassifyByThreshold]
EnsembleClassifyByThreshold::nargs =
    "The first argument is expected to be an Association of classifier IDs to \
classifier functions. The second argument is expected to be a vector or a \
matrix. The third argument is expected to be a rule, label->threshold, where \
threshold is numerical. The fourth argument is expected to be one of \
\"Votes\" or \"ProbabilitiesMean\".";

EnsembleClassifyByThreshold[cls_Association, record_?VectorQ,
  label_ -> threshold_?NumericQ,
  method_String: "ProbabilitiesMean"] :=
    Block[{pmeans},
      If[TrueQ[method == "ProbabilitiesMean"],
        pmeans = EnsembleClassifierProbabilities[cls, record],
        pmeans = Join[<|label -> 0|>, EnsembleClassifierVotes[cls, record]]
      ];
      If[pmeans[label] >= threshold, label, First@Keys@TakeLargest[pmeans, 1]]
    ];

EnsembleClassifyByThreshold[cls_Association, records_?MatrixQ,
  label_ -> threshold_?NumericQ,
  method_String: "ProbabilitiesMean"] :=
    Block[{pmeans},
      If[TrueQ[method == "ProbabilitiesMean"],
        pmeans = EnsembleClassifierProbabilities[cls, records],
        pmeans =
            Map[Join[<|label -> 0|>, #] &, EnsembleClassifierVotes[cls, records]]
      ];
      Map[If[#[label] >= threshold, label, First@Keys@TakeLargest[#, 1]] &, pmeans]
    ];

EnsembleClassifyByThreshold[___] := (Message[EnsembleClassifyByThreshold::nargs]; $Failed);

ClassifyByThreshold[ cf_ClassifierFunction, data:(_?VectorQ|_?MatrixQ), label_ -> threshold_?NumericQ ] :=
    EnsembleClassifyByThreshold[ <| "cf"->cf |>, data, label->threshold, "ProbabilitiesMean" ];


(**************************************************************)
(* Calculating classifier ensemble measurements               *)
(**************************************************************)

ClearAll[EnsembleClassifierMeasurements]

EnsembleClassifierMeasurements::nargs =
    "The first argument, the classifier ensemble, is expected to be an Association of classifier IDs to \
classifier functions. \
The second argument, the test data, is expected to be a list of record-to-label rules. \
The third argument is expected to be a list of measures; see ROCFunctions`ROCFunctions[\"FunctionNames\"]. \
Use the option \"Classes\" to specify target classes. \
Use the option Method to specify which method the classifier ensemble should classify with.";

Options[EnsembleClassifierMeasurements] = {"Classes"->Automatic, Method -> Automatic};

EnsembleClassifierMeasurements[cls_Association,
  testData_?ClassifierDataQ, measure_String,
  opts : OptionsPattern[]] :=
    First @ EnsembleClassifierMeasurements[cls, testData, {measure}, opts];

EnsembleClassifierMeasurements[cls_Association, testData_, args___] :=
    EnsembleClassifierMeasurements[cls, Thread[testData], args] /; MatchQ[testData, Rule[_?ArrayQ, _]];

EnsembleClassifierMeasurements[cls_Association, testData_?ClassifierDataQ, measures : {_String ..}, opts : OptionsPattern[]] :=
    Block[{targetClasses, cfMethod, testLabels, clRes, clVals, clClasses, aROCs, knownMeasures,
      ccNotLabel, ccTestLabels, ccModelVals},

      targetClasses = OptionValue[EnsembleClassifierMeasurements, "Classes"];

      cfMethod = OptionValue[EnsembleClassifierMeasurements, Method];
      If[ cfMethod === Automatic,
        cfMethod = (EnsembleClassify[#1, #2, "ProbabilitiesMean"] &)
      ];

      testLabels = testData[[All, 2]];

      clVals = cfMethod[cls, testData[[All, 1]]];

      clClasses = ClassifierInformation[cls[[1]], "Classes"];

      If[ targetClasses === Automatic,
        targetClasses = clClasses;
      ];

      knownMeasures = measures /. {"Precision" -> "PPV", "Recall" -> "TPR", "Accuracy" -> "ACC"};
      clRes =
          Table[
            If[ MemberQ[ targetClasses, clClasses[[i]] ],
              ccNotLabel = "Not-"<>ToString[clClasses[[i]]];
              ccTestLabels = Map[ If[# == clClasses[[i]], #, ccNotLabel]&, testLabels ];
              ccModelVals = Map[ If[# == clClasses[[i]], #, ccNotLabel]&, clVals ];
              aROCs =
                  ToROCAssociation[{ clClasses[[i]], ccNotLabel }, ccTestLabels, ccModelVals];
              clClasses[[i]] ->
                  AssociationThread[measures -> Through[N[ROCFunctions[knownMeasures][aROCs]]]],
              (*ELSE*)
              Nothing
            ],
            {i, Length[clClasses]}
          ];

      clRes = Dataset[Association@clRes];
      clRes = Map[Normal[clRes[All, #]] &, measures];

      MapThread[If[MemberQ[{"Accuracy", "ACC"}, #1], First@Values[#2], #2] &, {measures, clRes}]
    ];

(**************************************************************)
(* Calculating classifier ensemble ROC data and plots         *)
(**************************************************************)

ClearAll[EnsembleClassifierROCData]

EnsembleClassifierROCData::nargs =
    "The first argument, the classifier ensemble, is expected to be an Association of classifier IDs to \
classifier functions. \
The second argument, the test data, is expected to be a list of record-to-label rules. \
The optional third argument, the threshold range, is expected to be a list of numbers between 0 and 1. \
The optional fourth argument, the target classes, is expected to be list of class labels or All."

EnsembleClassifierROCData[aCL_Association,
  testData_?ClassifierDataQ,
  thRange : {_?NumericQ ..}, targetClasses : (_List | All) : All] :=
    Block[{clClasses, clRes, testLabels, ccLabel, ccNotLabel, ccTestLabels, rocs},

      If[TrueQ[targetClasses === All || targetClasses === Automatic],
        clClasses = ClassifierInformation[aCL[[1]], "Classes"],
        clClasses = targetClasses
      ];

      clRes = EnsembleClassifierProbabilities[aCL, testData[[All, 1]]];

      testLabels = testData[[All, 2]];

      Table[
        ccNotLabel = "Not-" <> ToString[ccLabel];
        ccTestLabels =
            Map[If[# == ccLabel, #, ccNotLabel] &, testLabels];
        rocs =
            Table[
              ToROCAssociation[{ccLabel, ccNotLabel}, ccTestLabels,
                Map[If[# >= th, ccLabel, ccNotLabel] &, Through[clRes[ccLabel]]]],
              {th, thRange}];
        ccLabel -> rocs,
        {ccLabel, clClasses}]
    ];

EnsembleClassifierROCData[___] := (Message[EnsembleClassifierROCData::nargs]; $Failed);


ClearAll[EnsembleClassifierROCPlots];

EnsembleClassifierROCPlots::nargs =
    "The first argument, the classifier ensemble, is expected to be an Association of classifier IDs to \
classifier functions. \
The second argument, the test data, is expected to be a list of record-to-label rules. \
The optional third argument, the threshold range, is expected to be a list of numbers between 0 and 1. \
The optional fourth argument, the target classes, is expected to be list of class labels or All. \
As options the options of ROCFunctions`ROCPlot and Graphics can be given."

Options[EnsembleClassifierROCPlots] = Options[ROCPlot];

EnsembleClassifierROCPlots[aCL_Association,
  testData_?ClassifierDataQ,
  thRange : {_?NumericQ ..}, targetClasses : (_List | All) : All,
  opts : OptionsPattern[]] :=
    Block[{rocRes},
      rocRes = Association@EnsembleClassifierROCData[aCL, testData, thRange, targetClasses];
      AssociationMap[ROCPlot[thRange, rocRes[#], opts] &, Keys[rocRes]]
    ];

EnsembleClassifierROCPlots[___] := (Message[EnsembleClassifierROCPlots::nargs]; $Failed);


End[] (* `Private` *)

EndPackage[]