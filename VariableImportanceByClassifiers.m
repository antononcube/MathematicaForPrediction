(*
    Variable importance determination by classifiers implementation in Mathematica
    Copyright (C) 2013-2015  Anton Antonov

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
	  antononcube@gmail.com,
	  Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2015 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: VariableImportanceByClassifiers *)
(* :Author: Anton Antonov *)
(* :Date: 2015-12-27 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 10.3.1 *)
(* :Copyright: (c) 2015 Anton Antonov *)
(* :Keywords: Classify, variable importance, Titanic *)
(* :Discussion:

  This package has a function that can be used to find the importance of variables in a data set.

  -------------------
  Procedure outline
  -------------------

    1. Build a classifier with the training set.

    2. Verify using the test set that good classification results are obtained.

    3. If the number of variables (attributes) is k for each i, 1≤i≤k :

    3.1. Shuffle the values of the i-th column of the test data and find the classification success rates.

    4. Compare the obtained k classification success rates between each other and
       with the success rates obtained by the un-shuffled test data.

  The variables for which the classification success rates are the worst are the most decisive.

  Instead of overall classification accuracy the package can work with F-Scores of a given set of labels.

  -------------------
  Examples
  -------------------

  1. Load some data. (Using the Titanic dataset.)

    testSetName = "Titanic"; (* "Mushroom" *)
    trainingSet = ExampleData[{"MachineLearning", testSetName}, "TrainingData"];
    testSet = ExampleData[{"MachineLearning", testSetName}, "TestData"];


  2. Variable names and unique class labels.

    varNames = Flatten[List @@ ExampleData[{"MachineLearning", testSetName}, "VariableDescriptions"]]
    (* {"passenger class", "passenger age", "passenger sex", "passenger survival"} *)

    classLabels = Union[ExampleData[{"MachineLearning", testSetName}, "Data"][[All, -1]]]
    (* {"died", "survived"} *)


  3. Make the classifier.

    clFunc = Classify[trainingSet, Method -> "RandomForest"]


  4. Obtain accuracies after shuffling.

    accs = AccuracyByVariableShuffling[clFunc, testSet, varNames]

    (* <|None -> 0.778626, "passenger class" -> 0.743003, "passenger age" -> 0.768448, "passenger sex" -> 0.580153|> *)


  5. Tabulate the results.

    Grid[
      Prepend[
        List @@@ Normal[accs/First[accs]],
        Style[#, Bold, Blue, FontFamily -> "Times"] & /@ {"shuffled variable", "accuracy ratio"}],
      Alignment -> Left, Dividers -> All]


  6. Further confirmation of the found variable importance can be done using the mosaic plots.
     (The package for MosaicPlot is provided by this repository. See the references. )

     t = (Flatten /@ (List @@@ trainingSet));
     MosaicPlot[t[[All, {1, 3, 4}]], ColorRules -> {3 -> ColorData[7, "ColorList"]} ]


  4a. In order to use F-scores instead of overall accuracy the desired class labels are specified with
      the option "FScoreLabels".

    accs = AccuracyByVariableShuffling[clFunc, testSet, varNames, "FScoreLabels" -> classLabels]

    (* <|None -> {0.836158, 0.658824},
         "passenger class" -> {0.796992, 0.574803},
         "passenger age" -> {0.824197, 0.638132},
         "passenger sex" -> {0.704797, 0.344262}|> *)


  4b. Here is another example that uses the class label with the smallest F-score.
      (Probably the most important since it is most mis-classified).

    accs = AccuracyByVariableShuffling[clFunc, testSet, varNames,
                                       "FScoreLabels" -> Position[#, Min[#]][[1, 1, 1]] &@
                                                                  ClassifierMeasurements[clFunc, testSet, "FScore"]]

    (* <|None -> {0.658824},
         "passenger class" -> {0.54321},
         "passenger age" -> {0.666667},
         "passenger sex" -> {0.347107}|> *)


  -------------------
  References
  -------------------

    I read the description of this procedure in the book:

    [1] Breiman, L. et al., Classification and regression trees, Chapman & Hall, 1984.


    For further references, examples, and discussions with census data see the blog post:

    [2] "Classification and association rules for census income data"
        Posted on March 30, 2014 by Anton Antonov Antonov on MathematicaForPrediction at WordPress.com .
        URL: https://mathematicaforprediction.wordpress.com/2014/03/30/classification-and-association-rules-for-census-income-data/


    The mosaic plots suggested above can be made using this package:

    [3] "Mosaic plot for data visualization implementation in Mathematica" (2014)
        https://github.com/antononcube/MathematicaForPrediction/blob/master/MosaicPlot.m


  This file was created using Mathematica Plugin for IntelliJ IDEA.

  Anton Antonov
  2015-12-28

*)

BeginPackage["VariableImportanceByClassifiers`"]
(* Exported symbols added here with SymbolName::usage *)

AccuracyByVariableShuffling::usage = "AccuracyByVariableShuffling[clFunc, testData, variableNames, opts] finds the variable \
importance using a ClassiferFunction object clFunc and a test data set testData. The names of the variables \
can be specified with variableNames. With the option \"FScoreLabels\" the accuracies can be computed over a specific list \
of class labels."

Begin["`Private`"]

Clear[IsClassifierDataQ, AccuracyByVariableShuffling]

IsClassifierDataQ[data_] := MatchQ[ data, { Rule[_List, _] .. } ] && ArrayQ[ data[[ All, 1 ]] ];

AccuracyByVariableShuffling::varnames = "The third argument (variableNames) is expected to be Automatic or a list of strings."

Options[AccuracyByVariableShuffling] = { "FScoreLabels" -> None };

AccuracyByVariableShuffling[ clFunc_ClassifierFunction, testData_?IsClassifierDataQ, variableNames_:Automatic, opts:OptionsPattern[] ] :=
    Block[{ baseAccuracy, tmat, shuffledTestSets, accuraciesOfShuffledTestSets, varNames, fscoreLabels },

      fscoreLabels = OptionValue["FScoreLabels"];
      If[ TrueQ[ fscoreLabels =!= None ] && AtomQ[fscoreLabels], fscoreLabels = {fscoreLabels} ];

      (* Matrix/array of attributes *)
      tmat = testData[[All, 1]];

      (* Variable names *)
      varNames =
          Which[
            TrueQ[ variableNames === Automatic ],
            Range[ Dimensions[tmat][[2]] ],
            MatchQ[ variableNames, {_String..} ],
            Which[
              Length[variableNames] == Dimensions[tmat][[2]], variableNames,
              Length[variableNames] < Dimensions[tmat][[2]], Join[ variableNames, Range[ Length[variableNames]+1, Dimensions[tmat][[2]] ] ],
              Length[variableNames] > Dimensions[tmat][[2]], Take[ variableNames, Dimensions[tmat][[2]] ]
            ],
            True,
            Message[AccuracyByVariableShuffling::varnames];
            Return[$Failed]
          ];

      (* Find the baseline accuracy. *)
      If[ fscoreLabels === None,
        baseAccuracy = ClassifierMeasurements[ clFunc, testData, "Accuracy"],
        (* ELSE *)
        baseAccuracy = ClassifierMeasurements[ clFunc, testData, "FScore"];
        baseAccuracy = baseAccuracy /@ fscoreLabels;
      ];

      (* Shuffle each column of the test set. *)
      tmat = Transpose[tmat];
      shuffledTestSets =
          Map[Function[{i},
            Thread[
              Transpose[ReplacePart[tmat, i -> RandomSample[tmat[[i]]]]] -> testData[[All, 2]]]
          ], Range[Dimensions[tmat][[1]]]];

      (* Calculate the classifier accuracy for each of the datasets *)
      If[ fscoreLabels === None,
        accuraciesOfShuffledTestSets =
            ClassifierMeasurements[clFunc, #, "Accuracy"] & /@ shuffledTestSets,
        (* ELSE *)
        accuraciesOfShuffledTestSets =
            ClassifierMeasurements[clFunc, #, "FScore"] & /@ shuffledTestSets;
        accuraciesOfShuffledTestSets = Map[ # /@ fscoreLabels&, accuraciesOfShuffledTestSets ];
      ];

      (* Return result *)
      PrependTo[ varNames, None ];
      PrependTo[ accuraciesOfShuffledTestSets, baseAccuracy ];
      AssociationThread[ varNames -> accuraciesOfShuffledTestSets ]
    ];



End[] (* `Private` *)

EndPackage[]