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
      1. Better messages
*)

BeginPackage["ClassifierEnsembles`"]

EnsembleClassifier::usage = "Create an ensemble of classifiers over the same data. \
Returns an Association of IDs mapped to classifier funcitons."

EnsembleClassifierVotes::usage = "Find votes by a classifier ensemble for a record ora a list of records."

EnsembleClassifierProbabilities::usage = "Give the averaged probabilities of a classifier ensemble \
a record or a list of records."

EnsembleClassify::usage = "Classify by a classifier ensemble for a record or a list of records. \
The third argument is one of \"Votes\" or \"ProbabilitiesMean\"."

EnsembleClassifyByThreshold::usage = "Classify by a classifier ensemble for a record or a list of records. \
The third argument is a rule label->threshold. The fourth argument is one of \"Votes\" or \"ProbabilitiesMean\". \
The specified label is returned if its votes or average probability are higher or equal than \
the specified threshold."


Begin["`Private`"]


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


Clear[EnsembleClassifierVotes]
EnsembleClassifierVotes::nargs =
    "The first argument is expected to be an Association of classfier IDs to \
classifer functions. The second argument is expected to be a vector or a \
matrix.";

EnsembleClassifierVotes[cls_Association, record_?VectorQ] :=
    Association[Rule @@@ Sort[Tally[Through[Values[cls][record]]], -#[[-1]] &]];

EnsembleClassifierVotes[cls_Association, records_?MatrixQ] :=
    Map[Association[Rule @@@ Sort[Tally[#], -#[[-1]] &]] &, Transpose[Through[Values[cls][records]]]];

EnsembleClassifierVotes[___] := (Message[EnsembleClassifierVotes::nargs]; $Failed);

Clear[EnsembleClassifierProbabilities]
EnsembleClassifierProbabilities::nargs =
    "The first argument is expected to be an Association of classfier IDs to \
classifer functions. The second argument is expected to be a vector or a \
matrix.";

EnsembleClassifierProbabilities[cls_Association, record_?VectorQ] :=
    Mean[Through[Values[cls][record, "Probabilities"]]];

EnsembleClassifierProbabilities[cls_Association, records_?MatrixQ] :=
    Mean /@ Transpose[Through[Values[cls][records, "Probabilities"]]];

EnsembleClassifierProbabilities[___] := (Message[EnsembleClassifierProbabilities::nargs]; $Failed);


Clear[EnsembleClassify]
EnsembleClassify::nargs =
    "The first argument is expected to be an Association of classfier IDs to \
classifer functions. The second argument is expected to be a vector or a \
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


Clear[EnsembleClassifyByThreshold]
EnsembleClassifyByThreshold::nargs =
    "The first argument is expected to be an Association of classfier IDs to \
classifer functions. The second argument is expected to be a vector or a \
matrix. The third argument is expected to be a rule, label->threshold, where \
threshold is numerical. The fourth argument is expected to be one of \
\"Votes\" or \"ProbabilitiesMean\".";

EnsembleClassifyByThreshold[cls_Association, record_?VectorQ,
  label_ -> threshold_?NumericQ, method_String: "ProbabilitiesMean"] :=
    Block[{pmeans},
      If[TrueQ[method == "ProbabilitiesMean"],
        pmeans = EnsembleClassifierProbabilities[cls, record],
        pmeans = Join[<|label -> 0|>, EnsembleClassifierVotes[cls, record]]
      ];
      If[pmeans[label] >= threshold, label, First@Keys@TakeLargest[pmeans, 1]]
    ];

EnsembleClassifyByThreshold[cls_Association, records_?MatrixQ,
  label_ -> threshold_?NumericQ, method_String: "ProbabilitiesMean"] :=

    Block[{pmeans},
      If[TrueQ[method == "ProbabilitiesMean"],
        pmeans = EnsembleClassifierProbabilities[cls, records],
        pmeans =
            Map[Join[<|label -> 0|>, #] &, EnsembleClassifierVotes[cls, records]]
      ];
      Map[If[#[label] >= threshold, label, First@Keys@TakeLargest[#, 1]] &,
        pmeans]
    ];

EnsembleClassifyByThreshold[___] := (Message[EnsembleClassifyByThreshold::nargs]; $Failed);

End[] (* `Private` *)

EndPackage[]