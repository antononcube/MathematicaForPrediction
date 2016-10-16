# **ROCFunctions example usage**

Anton Antonov  
[MathematicaForPrediction project at GitHub](https://github.com/antononcube/MathematicaForPrediction/)  
[MathematicaVsR project at GitHub](https://github.com/antononcube/MathematicaVsR/)  
October 2016 

## Introduction

The package \[[2](https://github.com/antononcube/MathematicaForPrediction/blob/master/ROCFunctions.m)\] provides Mathematica implementations of [Receiver Operating Characteristic](https://en.wikipedia.org/wiki/Receiver_operating_characteristic) (ROC) functions calculation and plotting. The ROC framework is used for analysis and tuning of binary classifiers, \[[3](https://en.wikipedia.org/wiki/Receiver_operating_characteristic)\]. (The classifiers are assumed to classify into a positive/true label or a negative/false label. )

The function `ROCFuntions` gives access to the individual ROC functions through string arguments. Those ROC functions are applied to special objects, called ROC Association objects.

Each ROC Association object is an `Association` that has the following four keys: "TruePositive", "FalsePositive", "TrueNegative", and "FalseNegative" .

Given two lists of actual and predicted labels a ROC Association object can be made with the function `ToROCAssociation` .

For more definitions and example of ROC terminology and functions see \[[3](https://en.wikipedia.org/wiki/Receiver_operating_characteristic)\].

## Minimal example

Note that here although we use both of the provided Titanic training and test data, the code is doing only training. The test data is used to find the best tuning parameter (threshold) through ROC analysis.

### Get packages

These commands load the packages \[[1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m),[2](https://github.com/antononcube/MathematicaForPrediction/blob/master/ROCFunctions.m)\]:

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"]
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/ROCFunctions.m"]

### Using Titanic data

Here is the summary of the Titanic data used below:

    titanicData = (Flatten@*List) @@@ExampleData[{"MachineLearning", "Titanic"}, "Data"];
    columnNames = (Flatten@*List) @@ExampleData[{"MachineLearning", "Titanic"}, "VariableDescriptions"];
    RecordsSummary[titanicData, columnNames]

[![Titanic1][1]][1]

This variable dependence grid shows the relationships between the variables.

    Magnify[#, 0.7] &@VariableDependenceGrid[titanicData, columnNames]

[![VariableDependencies][7]][7]

### Get training and testing data

    data = ExampleData[{"MachineLearning", "Titanic"}, "TrainingData"];
    data = ((Flatten@*List) @@@ data)[[All, {1, 2, 3, -1}]];
    trainingData = DeleteCases[data, {___, _Missing, ___}];
    Dimensions[trainingData]

(* {732, 4} *)

    data = ExampleData[{"MachineLearning", "Titanic"}, "TestData"];
    data = ((Flatten@*List) @@@ data)[[All, {1, 2, 3, -1}]];
    testData = DeleteCases[data, {___, _Missing, ___}];
    Dimensions[testData]

(* {314, 4} *)

### Replace categorical with numerical values

    trainingData = trainingData /. {"survived" -> 1, "died" -> 0, "1st" -> 0, "2nd" -> 1, "3rd" -> 2, "male" -> 0, "female" -> 1};

    testData = testData /. {"survived" -> 1, "died" -> 0, "1st" -> 1, "2nd" -> 2, "3rd" -> 3, "male" -> 0, "female" -> 1};

### Do linear regression

    lfm = LinearModelFit[{trainingData[[All, 1 ;; -2]], trainingData[[All, -1]]}]

[![Regression1][2]][2]

### Get the predicted values

    modelValues = lfm @@@ testData[[All, 1 ;; -2]];

    Histogram[modelValues, 20]

[![Prediction1][3]][3]

RecordsSummary[modelValues]

[![Prediction2][4]][4]

### Obtain ROC associations over a set of parameter values

    testLabels = testData[[All, -1]];

    thRange = Range[0.1, 0.9, 0.025];
    aROCs = Table[ToROCAssociation[{0, 1}, testLabels, Map[If[# > \[Theta], 1, 0] &, modelValues]], {\[Theta], thRange}];

### Evaluate ROC functions for given ROC association

    Through[ROCFunctions[{"PPV", "NPV", "TPR", "ACC", "SPC"}][aROCs[[3]]]]

(* {34/43, 19/37, 17/32, 197/314, 95/122} *)

### Standard ROC plot

    ROCPlot[thRange, aROCs, "PlotJoined" -> Automatic, "ROCPointCallouts" -> True, "ROCPointTooltips" -> True, GridLines -> Automatic]

[![ROCPlot1][5]][5]

### Plot ROC functions wrt to parameter values

    ListLinePlot[Map[Transpose[{thRange, #}] &, Transpose[Map[Through[ROCFunctions[{"PPV", "NPV", "TPR", "ACC", "SPC"}][#]] &, aROCs]]],
     Frame -> True, FrameLabel -> Map[Style[#, Larger] &, {"threshold, \[Theta]", "rate"}], PlotLegends -> Map[# <> ", " <> (ROCFunctions["FunctionInterpretations"][#]) &, {"PPV", "NPV", "TPR", "ACC", "SPC"}], GridLines -> Automatic]

[![ROCPlot2][6]][6]

### Finding the intersection point of PPV and TPR

We want to find a point that provides balanced positive and negative labels success rates. One way to do this is to find the intersection point of the ROC functions PPV (positive predictive value) and TPR (true positive rate).

Examining the plot above we can come up with the initial condition for $x$.

    ppvFunc = Interpolation[Transpose@{thRange, ROCFunctions["PPV"] /@ aROCs}];
    tprFunc = Interpolation[Transpose@{thRange, ROCFunctions["TPR"] /@ aROCs}];
    FindRoot[ppvFunc[x] - tprFunc[x] == 0, {x, 0.2}]

(* {x -> 0.3} *)

### Area under the ROC curve

The Area Under the ROC curve (AUROC) tells for a given range of the controlling parameter "what is the probability of the classifier to rank a randomly chosen positive instance higher than a randomly chosen negative instance, (assuming 'positive' ranks higher than 'negative')", [3,4]

Calculating AUROC is easy using the Trapezoidal quadrature formula:

     N@Total[Partition[Sort@Transpose[{ROCFunctions["FPR"] /@ aROCs, ROCFunctions["TPR"] /@ aROCs}], 2, 1] 
       /. {{x1_, y1_}, {x2_, y2_}} :> (x2 - x1) (y1 + (y2 - y1)/2)]

     (* 0.698685 *)

It is also implemented in \[[2](https://github.com/antononcube/MathematicaForPrediction/blob/master/ROCFunctions.m)\]:

    N@ROCFunctions["AUROC"][aROCs]

    (* 0.698685 *)

## References

\[1\] Anton Antonov, [MathematicaForPrediction utilities](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m), (2014), source code [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), package [MathematicaForPredictionUtilities.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m).

\[2\] Anton Antonov, [Receiver operating characteristic functions Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/ROCFunctions.m), (2016), source code [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), package [ROCFunctions.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/ROCFunctions.m) .

\[3\] Wikipedia entry, Receiver operating characteristic. URL: http://en.wikipedia.org/wiki/Receiver_operating_characteristic .

\[4\] Tom Fawcett, [An introduction to ROC analysis](https://ccrma.stanford.edu/workshops/mir2009/references/ROCintro.pdf), (2006), Pattern Recognition Letters, 27, 861-874.

<!---
[1]:Titanic1.png
[2]:Regression1.png
[3]:Prediction1.png
[4]:Prediction2.png
[5]:ROCPlot1.png
[6]:ROCPlot2.png
[7]:VariableDependencies.png
-->

[1]:http://i.imgur.com/VTVyV9P.png
[2]:http://i.imgur.com/d663I98.png
[3]:http://i.imgur.com/bBXsDp2.png
[4]:http://i.imgur.com/mzWjhZc.png
[5]:http://i.imgur.com/EeaALMz.png
[6]:http://i.imgur.com/mQvPDmA.png
[7]:http://i.imgur.com/DSkPQOH.png
