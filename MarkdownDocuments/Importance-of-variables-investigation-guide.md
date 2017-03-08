# Importance of variables investigation guide

## Importance of variables investigation using Classifiers, Mosaic plots, Decision trees, Association rules, and Dimension reduction

Anton Antonov  
[MathematicaForPrediction blog at WordPress](https://mathematicaforprediction.wordpress.com)  
[MathematicaForPrediction blog at GitHub](https://github.com/antononcube/MathematicaForPrediction)  
December 2015  
January 2016  

Version 1.0

## Introduction

The document is meant to serve as a guide for variable importance finding and it is organized to have the flow and corresponding explanations in parallel for the two datasets, "Titanic" and "Mushroom".

One of the questions that would follow the formulation of a classification problem for a given data set is which variables are most important for the correct classification to the class labels. 

For example, assume we have mushroom edibility data exemplified with this sample:

[![Variable-Intro1][1]][1]

We can ask the following questions:

1. Which variables (cap shape, odor, etc.) are most decisive for determining the edibility of a mushroom?

2. Which values of which variables form conditions that would imply a poisonous mushroom?

This guide is mostly about answering the first question. The second question is answered in order to validate the answers for the first.

In the blog post ["Classification and association rules for census income data"](https://mathematicaforprediction.wordpress.com/2014/03/30/classification-and-association-rules-for-census-income-data/), \[[2](https://mathematicaforprediction.wordpress.com/2014/03/30/classification-and-association-rules-for-census-income-data/)\], I have described a procedure for determining variable importance that answers similar questions. The procedure builds a classifier with the training data, damages the test data in a systematic way for each variable, and compares the "damage effects", i.e. how much worse the classifier performs over the damaged test data. The variables that produce worst results are considered most important.

The procedure is taken from the book "Classification and regression trees", \[1\], largely written by [Leo Breiman](https://en.wikipedia.org/wiki/Leo_Breiman), the great advocate and inventor of [random forests](https://en.wikipedia.org/wiki/Random_forest). The blog post \[[2](https://mathematicaforprediction.wordpress.com/2014/03/30/classification-and-association-rules-for-census-income-data/)\] has examples and a discussion of determining variable importance using two classifiers, Decision Trees, \[[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m)\], and Naive Bayesian Classifier (NBC), \[[4](https://github.com/antononcube/MathematicaForPrediction/blob/master/NaiveBayesianClassifier.m)\]. 

The importance of variables determination has to be considered in the larger of context of the data. The outcomes of the procedures used have to be evaluated do they make sense and do they explain observations from the data. For that in this document we are going to (i) see the application of Mosaic Plots \[[5](https://github.com/antononcube/MathematicaForPrediction/blob/master/MosaicPlot.m), [6](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/)\], (ii) examine Decision trees \[[10](http://onlinelibrary.wiley.com/book/10.1002/9780470175668)\], (iii) Association rules \[[11](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/MovieLens%20genre%20associations.pdf)\], and (iv) topics from dimension reduction, \[[13](https://mathematicaforprediction.wordpress.com/2013/10/15/statistical-thesaurus-from-npr-podcasts/),[14](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf)\].

In this document two datasets are used -- "Titanic" and "Mushroom" -- to make examples that illustrate the algorithms and procedures. Both datasets are available in *Mathematica* versions 10 and later. "Titanic" consists of metadata and survival records of passengers in the [wreck of the ship Titanic](https://en.wikipedia.org/wiki/Wreck_of_the_RMS_Titanic). "Mushroom" has records for mushroom edibility (with much more records and variables than in the table above). The datasets are of mostly categorical data. For purely numerical data techniques like Principal Components Analysis (PCA) are applicable. (Not covered in this guide.) Nevertheless, numerical variables can be converted into categorical with suitable piecewise constant functions. (See the example below for "passenger age" of "Titanic".)

*Mathematica*'s versions 10.0 and later have the function [`Classify`](http://reference.wolfram.com/language/ref/Classify.html) that generates classification functions over a variety of data sets. The classifiers in the blog post \[[2](https://mathematicaforprediction.wordpress.com/2014/03/30/classification-and-association-rules-for-census-income-data/)\] were developed before the introduction of `Classify` in *Mathematica*. This document discusses code, \[[8](https://github.com/antononcube/MathematicaForPrediction/blob/master/VariableImportanceByClassifiers.m)\], and applications based on `Classify` and related functions for data partitioning and classifier function querying or invocation.

**Remark:** It should be pointed out that the considered datasets ("Titanic" and "Mushroom") are relatively small and give nice and clean results. Real applications of the described procedures might require preliminary (extensive) data cleaning and normalization before obtaining good classification and variable importance results.

This document has a version number associated with it since it is available at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction) and I plan to update and extend it. Another plan I have is to make a version of this document that includes commands in R, or it is entirely written with R and RStudio tools.

## Datasets

In this section we introduce the datasets with samples and data summaries.

### Titanic

The following command loads the "Titanic" dataset. The dataset is given as a list of rules, so in order to tabulate it we are going to flatten it first.

	titanicDataset =  Map[Flatten, List @@@ ExampleData[{"MachineLearning", "Titanic"}, "Data"]];
	Dimensions[titanicDataset]

    {1309, 4}

Here are the variable names (the last one is the one with the class labels):

    titanicVarNames =  Flatten[List @@   ExampleData[{"MachineLearning", "Titanic"}, "VariableDescriptions"]]

    {"passenger class", "passenger age", "passenger sex", "passenger survival"}

The following command tabulates a sample of the Titanic dataset rows. Each row represents a passenger.

    Magnify[##, 0.8] &@TableForm[RandomSample[titanicDataset, 12],
	  TableHeadings -> {None, titanicVarNames}]

[![Titanic-Datasets1][2]][2]

Here is a summary of different columns. (See \[[7](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)\] for the function `RecordsSummary`.)

    Magnify[##, 0.8] &@ Grid[List@RecordsSummary[titanicDataset /. _Missing -> 0, titanicVarNames], Dividers -> All, Alignment -> {Left, Top}]

[![Titanic-Datasets1][3]][3]

We can see that for 263 (or 20%) of the records the passenger age values are missing:

    Count[##, _Missing] & /@  AssociationThread[titanicVarNames, Transpose[titanicDataset]]
    %/Dimensions[titanicDataset][[1]] // N


    (* <|"passenger class" -> 0, "passenger age" -> 263, "passenger sex" -> 0, "passenger survival" -> 0|>

    <|"passenger class" -> 0., "passenger age" -> 0.200917, "passenger sex" -> 0., "passenger survival" -> 0.|> *)

###Mushroom

The following command loads the "Mushroom" dataset. The dataset is given as a list of rules, so in order to tabulate it we are going to flatten it first.

    mushroomDataset =  Map[Flatten, List @@@ ExampleData[{"MachineLearning", "Mushroom"}, "Data"]];
    Dimensions[mushroomDataset]

    {8124, 23}

Here are the variable names (the last one is the one with the class labels):

    mushroomVarNames = Flatten[List @@ ExampleData[{"MachineLearning", "Mushroom"}, "VariableDescriptions"]];
    mushroomVarNames[[-1]] = StringReplace[mushroomVarNames[[-1]], " (" ~~ x___ ~~ ")" :> ""];
    mushroomVarNames
    
    (* {"cap-shape", "cap-surface", "cap-color", "bruises?", "odor", "gill-attachment", "gill-spacing", "gill-size", "gill-color", "stalk-shape", "stalk-root", "stalk-surface-above-ring", "stalk-surface-below-ring", "stalk-color-above-ring", "stalk-color-below-ring", "veil-type", "veil-color" "ring-number", "ring-type", "spore-print-color", "population", "habitat", "edibility of mushroom"} *)

The following command tabulates a sample of the "Mushroom" dataset rows (in the PDF not all columns are seen):

    Magnify[##, 0.8] &@TableForm[RandomSample[mushroomDataset, 12],
      TableHeadings -> {None, mushroomVarNames}]

[![Mushroom-Dataset1][4]][4]

The following command gives summaries of the different columns. (See \[[7](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)\] for the function `RecordsSummary`.)

    Magnify[##, 0.8] &@Grid[ArrayReshape[RecordsSummary[mushroomDataset /. _Missing -> 0, mushroomVarNames], {6, 4}, ""], Dividers -> All, Alignment -> {Left, Top}]

[![Mushroom-Dataset2][5]][5]

We can see that for 2480 (or $\approx$ 31%) of the records the values of the column "stalk-root" are missing:

    t = Count[##, _Missing] & /@ AssociationThread[mushroomVarNames, Transpose[mushroomDataset]];
    Select[t, ## > 0 &]
    %/Dimensions[mushroomDataset][[1]] // N


    (* <|"stalk-root" -> 2480|>

    <|"stalk-root" -> 0.305268|> *)
___
## Procedure and implementation

This section has descriptions of the fundamental (main) procedure used in this guide for variable importance investigation.

### Procedure outline

1. Split the data into training and testing datasets.

2. Build a classifier with the training set.

3. Verify using the test set that good classification results are obtained. Find the baseline accuracy.

4. If the number of variables (attributes) is $k$ for each $i, 1 \leq i \leq k$:

 4.1. Shuffle the values of the $i$-th column of the test data and find the classification success rates.

5. Compare the obtained $k$ classification success rates between each other and with the success rates obtained by the un-shuffled test data.

6. The variables for which the classification success rates are the worst are the most decisive.

Note that instead of using the overall baseline accuracy we can make the comparison over the accuracies for selected, more important class labels. (See the examples below.)

The procedure is classifier agnostic. With certain classifiers, Naive Bayesian classifiers and Decision trees, the importance of variables can be directly concluded from their structure obtained after training.

The procedure can be enhanced by using dimension reduction before building the classifiers. (See the last section for an outline.)

### Implementation

The implementation of the procedure is straightforward in *Mathematica* -- see the package "VariableImportanceByClassifiers", \[[8](https://github.com/antononcube/MathematicaForPrediction/blob/master/VariableImportanceByClassifiers.m)\].

The package can be imported with the command:

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/VariableImportanceByClassifiers.m"]

At this point the package has only one function, `AccuracyByVariableShuffling`, that takes as arguments a `ClassifierFunction` object, a dataset, optional variable names, and the option "FScoreLabels" that allows the use of accuracies over a custom list of class labels instead of overall baseline accuracy. 

Here is the function signature:
___
    AccuracyByVariableShuffling[ clFunc_ClassifierFunction, testData_, variableNames_:Automatic, opts:OptionsPattern[] ]
___

The returned result is an `Association` structure that contains the baseline accuracy and the accuracies corresponding to the shuffled versions of the dataset. I.e. steps 3 and 4 of the procedure are performed by `AccuracyByVariableShuffling`. Returning the result in the form `Association[___]` means we can treat the result as a list with named elements similar to the list structures in Lua and R.

Examples are given in the next section.

## Concrete applications

In this section we are going to split the datasets and find the accuracy decrease using the obtained with training data classifiers and the damaged versions of the test data.

### Titanic

First we partition the "Titanic" dataset into a training and testing sets.

    titanicTrainingData = ExampleData[{"MachineLearning", "Titanic"}, "TrainingData"];
    Dimensions[titanicTrainingData[[All, 1]]]

    (* {916, 3} *)


    titanicTestData = ExampleData[{"MachineLearning", "Titanic"}, "TestData"];
    Dimensions[titanicTestData[[All, 1]]]

    (* {393, 3} *)

**Remark:** We relied on `ExampleData` to produce dataset parts that are representative of the whole dataset. If the split is derived by other means it should be verified that the training and testing datasets have similar distributions of the observations. (For example, we can use `RecordsSummary`, \[[7](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)\], on each of them and compare the obtained values.)

We make a classifier with training data.

    titanicCFunc = Classify[titanicTrainingData, Method -> "SupportVectorMachine"] 

[![Titanic-Class-Function1][6]][6]

Next we find the accuracies of the classifier function over the shuffled versions of the testing dataset.

    AccuracyByVariableShuffling[titanicCFunc, titanicTestData, titanicVarNames]

    (* <|None -> 0.783715, "passenger class" -> 0.709924, "passenger age" -> 0.783715, "passenger sex" -> 0.62341|> *)

The association value for the key None corresponds to the baseline accuracy. Since the accuracy corresponding to "passenger sex" is the worst we can conjecture that "passenger sex" is the most important variable. Confirmations of this conjecture are given in the next sections. The variables "passenger class" and "passenger age" *seem to be* on par.

**Remark:** With each run we would get (slightly) different results because of the random shuffling.

The default value of the argument `variableNames` is `Automatic`. If no variable names are specified then (currently) column indices are used in the result.

    AccuracyByVariableShuffling[titanicCFunc, titanicTestData]

    (* <|None -> 0.783715, 1 -> 0.70229, 2 -> 0.778626, 3 -> 0.603053|> *)

If we look at so called F-Scores produced by the classifier function we can see the class label "survived" is mis-classified more often "died", and generally speaking a success rate of 0.7 (obtained with "SupportVectorMachine") is not that great.

    ClassifierMeasurements[titanicCFunc, titanicTestData, "FScore"]

    (* <|"died" -> 0.839925, "survived" -> 0.666667|> *)

Since also the records summary table shows that records with the label "survived" are 38% (i.e. smaller than 50%) we might want to judge the importance of variables over the ability of the classifier to guess correctly the label "survived". For this we use the option "FScoreLabels" of `AccuracyByVariableShuffling`.

    accRes = AccuracyByVariableShuffling[titanicCFunc, titanicTestData, titanicVarNames, "FScoreLabels" -> "survived"]

    (* <|None -> {0.666667}, "passenger class" -> {0.558704}, "passenger age" -> {0.648438}, "passenger sex" -> {0.409639}|> *)

We can see that "passenger sex" is still the most important, but now we also see that "passenger class" is significantly more important than "passenger age". This can be made more obvious by looking into the relative damage effects:

    (accRes[None] - accRes)/accRes[None]

    (* <|None -> {0.}, "passenger class" -> {0.161943}, "passenger age" -> {0.0273437}, "passenger sex" -> {0.385542}|> *)

Further investigation using mosaic plots and decision trees (see below) shows that female and first class passengers were much more likely to survive.

### Mushroom

Let us repeat the steps in the previous sub-section for the dataset "Mushroom".

First let partition the "Mushroom" dataset into a training and testing sets.

    mushroomTrainingData = ExampleData[{"MachineLearning", "Mushroom"}, "TrainingData"];
    Dimensions[mushroomTrainingData[[All, 1]]]

    (* {5686, 22} *)


    mushroomTestData = ExampleData[{"MachineLearning", "Mushroom"}, "TestData"];
    Dimensions[mushroomTestData[[All, 1]]]

    (* {2438, 22} *)

We make a classifier with training data.

    mushroomCFunc = Classify[mushroomTrainingData, Method -> "SupportVectorMachine"] 

[![Mushroom-Class-Function1][7]][7]

Next we find and sort the accuracies of the classifier function over the shuffled versions of the testing dataset.

    accRes = AccuracyByVariableShuffling[mushroomCFunc, mushroomTestData, mushroomVarNames];
    Sort[accRes]

    (* <|"odor" -> 0.703856, "spore-print-color" -> 0.986874, "gill-size" -> 0.99918, "bruises?" -> 1., "cap-color" -> 1., "cap-shape" -> 1., "cap-surface" -> 1., "gill-attachment" -> 1., "gill-color" -> 1., "gill-spacing" -> 1., "habitat" -> 1., "population" -> 1., "ring-number" -> 1., "ring-type" ->1., "stalk-color-above-ring" -> 1., "stalk-color-below-ring" -> 1., "stalk-root" -> 1., "stalk-shape" -> 1., "stalk-surface-above-ring" -> 1., "stalk-surface-below-ring" -> 1., "veil-color" -> 1., "veil-type" -> 1., None -> 1.|> *)

We can see that "odor" seems most decisive and this makes sense. It is further clarified with mosaic plot below.

If we look at so called F-Scores produced by the classifier function we can see that for both class labels "edible" and "poisonous" we get perfect classification scores:

    ClassifierMeasurements[mushroomCFunc, mushroomTestData, "FScore"]

    (* <|"edible" -> 1., "poisonous" -> 1.|> *)

Hence there is no need to compute more specialized accuracies as it was done for "Titanic".
___

## Confirmations and explanations with Mosaic plots

Mosaic plots \[[5](https://github.com/antononcube/MathematicaForPrediction/blob/master/MosaicPlot.m),[6](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/)\] give visual representation of conditional probabilities in categorical data. We can say that Mosaic plots correspond to NBC's, so it does make a lot of sense to use them for variable importance confirmation.

### Titanic

Here is a mosaic plot for the dataset "Titanic" without the column "passenger age" (which is not categorical):

    MosaicPlot[titanicDataset[[All, {4, 3, 1}]] , ColorRules -> {2 -> ColorData[7, "ColorList"]}]

[![Titanic-Mosaic1][8]][8]

From the previous plot it is immediately obvious why "passenger sex" is so decisive. The plot is made from the conditional probabilities direction starting from the class labels. For example, the plot answers questions like "given that a person survived, what is the probability of (i) being female, and (ii) being female and 1st class". The `Tooltip` tables show the answers are 0.678 and 0.278 respectively.

Let us make another mosaic plot that is structured and colored in such a way so we can follow which conditions led to death or survival.

    MosaicPlot[titanicDataset[[All, {1, 3, 4}]] , ColorRules -> {3 -> ColorData[7, "ColorList"]}]

[![Titanic-Mosaic2][9]][9]

We can see that the blue rectangles that correspond to survival are much larger for females and especially for females from 1st and 2nd class.

Here is the distribution of the passenger ages:

    Histogram[titanicDataset[[All, 2]], Automatic, AxesLabel -> {"passenger age", "number of passengers"}]

[![Titanic-AgePlot1][10]][10]

One way to visualize the survival dependence of age is the following.

    ListPlot[{
      Tooltip[Tally[Pick[titanicDataset[[All, 2]], ## == "survived" & /@titanicDataset[[All, 4]]]], "survied"], 
      Tooltip[Tally[Pick[titanicDataset[[All, 2]], ## == "died" & /@titanicDataset[[All, 4]]]], "died"]}, Filling -> Axis, PlotRange -> All, PlotLegends -> {"survived", "died"}, 
     AxesLabel -> {"passenger age", "number of passengers"}]

[![Titanic-AgePlot2][11]][11]

From the plot we can see that age-wise more or less survival and death followed the distribution of the ages of all passengers.

At this point it is better to turn age into a categorical variable and visualize with a mosaic plot. One way to do this is to use quantiles of ages; another is to use predefined age ranges. We are going to use the latter approach.

    titanicDatasetCatAge = titanicDataset;

    ageQF = Piecewise[{{1, -\[Infinity] < ##1 <= 5}, {2, 5 < ##1 <= 14}, {3, 14 < ##1 <= 21}, {4, 21 < ##1 <= 28}, {5, 28 < ##1 <= 35}, {6, 35 < ##1 <= 50}, {7, 50 < ##1 <= \[Infinity]}}, 0] &;

    titanicDatasetCatAge[[All, 2]] = Map[If[MissingQ[##], 0, ageQF[##]] &, titanicDatasetCatAge[[All, 2]]] /.
         {1 -> "1(under 6)", 2 -> "2(6\[Ellipsis]14)", 3 -> "3(15\[Ellipsis]21)", 4 -> "4(22\[Ellipsis]28)", 5 -> "5(29\[Ellipsis]35)",    6 -> "6(36\[Ellipsis]50)", 7 -> "7(50+)", 0 -> "0(missing)"};
    MosaicPlot[titanicDatasetCatAge[[All, {1, 2, 4, 3}]],
      ColorRules -> {3 -> ColorData[7, "ColorList"]},
      "LabelRotation" -> {{0.5, 0.5}, {0, 1}},
      "ColumnNames" -> Map[Style[##, Larger, Bold, Purple] &, titanicVarNames[[{1, 2, 4, 3}]]]]

[![Titanic-Mosaic3][12]][12]

From the plot we can see that the very young were much more likely to survive in the 1st and 2nd class. Note the large amount of missing ages. The missing data might be one of the reasons "passenger age" is not that decisive.

The conversion of "passenger age" into a categorical variable is also useful for the application of Association rules and topic extraction. (See below.)

### Mushroom

Mosaic plots for the "Mushroom" dataset over the columns "edibility of mushroom" and "odor" immediately explain why "odor" is so decisive for the mushroom classification. For example, we can see that a large fraction, $\approx$ 81%, of the edible mushrooms have no odor, and only $\approx$ 3% of the poisonous mushrooms have no odor. (These fractions can be seen on the `Tooltip` tables when the mouse pointer is over the rectangles in the first plot below.)

    MosaicPlot[mushroomDataset[[All, {-1, 5}]], "LabelRotation" -> {{1, 0.5}, {0, 1}}, "ColumnNames" -> Map[Style[##, Larger, Bold, Purple] &, mushroomVarNames[[{-1, 5}]]]]

[![Mushroom-Mosaic1][13]][13]

As with "Titanic" we can make a mosaic plot taking a different perspective of the conditional probabilities, showing which conditions lead to edible or poisonous. (Note the variable order and the specified coloring for the second axis in the value given to "ColorRules".) 

    MosaicPlot[mushroomDataset[[All, {5, -1}]], "FirstAxis" -> "Top", "LabelRotation" -> {{1, 0.3}, {0, 1}}, "ColumnNames" -> Map[Style[##, Larger, Bold, Purple] &, mushroomVarNames[[{5, -1}]]], 
      ColorRules -> {2 -> ColorData[7, "ColorList"]}]

[![Mushroom-Mosaic2][14]][14]


In the second plot the blue rectangles correspond to "poisonous". We can see that there is almost no overlap between the odors of edible with the odors of poisonous.
___
## Confirmations and explanations with Decision trees

Let us look into some decision trees built over the whole datasets in order to verify the found variable importance using the impurity functions of the decision tree building algorithm  (based on Information entropy and Gini coefficient). For more details see \[[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m),[9](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Waveform%20recognition%20with%20decision%20trees.pdf)\].

The decision trees we make a deliberately short. It is expected that the most important variables are going to be used first for splitting.

This command loads the package \[[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m)\]:

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/AVCDecisionTreeForest.m"]

### Titanic

The following command builds a decision tree over the whole "Titanic" dataset. The leaf count shows that the tree is small (short).

    dtree = BuildDecisionTree[titanicDataset /. {_Missing -> 0}, {12, 0.02}, "ImpurityFunction" -> "Gini", "PreStratify" -> True];
    LeafCount[dtree]

    (* 38 *)

The package \[[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m)\] provides functions to assess the classification rates of decision trees and forests. Since we used the whole dataset the invocation of these functions is just for sanity checks.

    DecisionTreeClassificationSuccess[dtree, Map[Flatten, List @@@ titanicTestData]]

    (* {{"died", True} -> 0.979424, {"died", False} -> 0.0205761, {"survived", True} -> 0.453333, {"survived", False} -> 0.546667, {All, True} -> 0.778626, {All, False} -> 0.221374} *)

Because of the rules `{survived,True}->0.453333` and `{survived,False}->0.546667` we see that the tree does not classify that well for the label "survived". The reasons are obvious by looking at the tree plot below.
<!-- should the { be written as \{ or does the ` enclosure prevent it from acting?
-->

The following command converts the obtained decision tree into a list of rules suitable for graph visualization and plots the corresponding graph.

    LayeredGraphPlot[DecisionTreeToRules[dtree] /. {{imp_?NumberQ, x_, i_Integer, b__} :> {imp, x, Style[titanicVarNames[[i]], Red, Bold], b}}, VertexLabeling -> True]

[![Titanic-DTree1][15]][15]

In the plot:

**1.** Each non-leaf node of the tree has the format:

    {impurity, splitting value, splitting variable, variable type, number of rows, node label}.

Where "variable type" is either Number or Symbol, and "number of rows" refers to the size of the part of the data that was observed (scanned) at that point of splitting. 

**2.** The leaf nodes are numbered. The second and third rows of a leaf show the number of records and labels corresponding to the predicate used to obtain the leaf. For example, for the predicate 

$$\text{"passenger sex"} = \text{"female"} \wedge \text{"passenger class"} = \text{"3 rd"}$$

the obtained subset of data has 110 records with "died" and 106 records with "survived".

We can see from the tree that (i) for males the probability to survive was $\approx$ 19% and that (ii) the females from 1st and 2nd class had much higher probability to survive. This confirms our findings with the classifier based procedure applied above.

### Mushroom

The following command builds a decision tree over the whole "Mushroom" dataset. The leaf count shows that the tree is small (short).

    dtree = BuildDecisionTree[mushroomDataset /. {_Missing -> None}, {3000, 0.03}, "ImpurityFunction" -> "Entropy"];
    LeafCount[dtree]

    (* 74 *)

 Again, as in the previous sub-section, let us do a sanity check using a classification success rates computation function.

    DecisionTreeClassificationSuccess[dtree, Map[Flatten, List @@@ mushroomDataset]]

    (* {{"edible", True} -> 1., {"edible", False} -> 0., {"poisonous", True} -> 0.850868, {"poisonous", False} -> 0.149132, {All, True} -> 0.928114, {All, False} -> 0.0718858} *)

We see that the tree classifies very well for both labels. (We have high values for all rules of the form `{_,True}->_` .)

The following command converts the obtained decision tree into a list of rules suitable for graph visualization and plots the corresponding graph.

    LayeredGraphPlot[DecisionTreeToRules[dtree] /. {{imp_?NumberQ, x_, i_Integer, b__} :> {imp, x, Style[mushroomVarNames[[i]], Red, Bold], b}}, VertexLabeling -> True]

[![Mushroom-DTree1][16]][16]

The plot of the obtained decision tree shows "odor" as the first, most significant variable having a large corresponding impurity value. (This should be expected having seen the "Mushroom" mosaic plot.) 

We would get a different tree if we use `"ImpurityFunction"->"Gini"`. The variable "odor" is still the most decisive one.

    dtree = BuildDecisionTree[mushroomDataset /. {_Missing -> 0}, {3000, 0.03}, "ImpurityFunction" -> "Gini"];
    LayeredGraphPlot[DecisionTreeToRules[dtree] /. {{imp_?NumberQ, x_, i_Integer, b__} :> {imp, x, Style[mushroomVarNames[[i]], Red, Bold], b}}, VertexLabeling -> True]

[![Mushroom-DTree2][17]][17]
___

## Association rules

We can use [Association rules learning](https://en.wikipedia.org/wiki/Association_rule_learning), \[[10](http://onlinelibrary.wiley.com/book/10.1002/9780470175668),[11](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/MovieLens%20genre%20associations.pdf)\], to find out which values of which variables imply the different class labels with highest confidence. This is demonstrated also in \[[2](https://mathematicaforprediction.wordpress.com/2014/03/30/classification-and-association-rules-for-census-income-data/)\]. This approach produces frequent sets and rules for concrete values of the variables. In order to derive (or imply) variable importance we do the following.

1. For each class label $L_{i}$ select a subset of association rules with high confidence that have $L_{i}$ as a consequent.

2. Compute statistics for the variables of the concrete values in the antecedents of the selected rules.

3. Variables that appear most often in the antecedents of the rules implying $L_{i}$ are most decisive for that class label.

Instead of subsets of association rules we can use frequent sets and perform statistics on them. The frequent sets are the required step to find association rules, and computing the association rules might be time consuming. A similar procedure using frequent sets follows.

1. For each class label $L_{i}$ select a subset of frequent sets that have $L_{i}$ are that have large enough frequencies (support).

2. Compute statistics for the variables of the concrete values in the frequent sets.

3. Variables that appear most often in the frequent sets containing $L_{i}$ are most decisive for that class label.

This command loads the [package AprioriAlgorithm.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/AprioriAlgorithm.m), \[[12](https://github.com/antononcube/MathematicaForPrediction/blob/master/AprioriAlgorithm.m)\]:

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/AprioriAlgorithm.m"]

**Remark:** Frequent sets and association rules learning algorithms do not require array shaped data; they can work on general sets of metadata tags. (Lists of lists of strings.) That is why below the concrete values of the variables of the considered datasets ("Titanic" and "Mushroom") are called "tags".

Extended examples and explanations for the use of Association rules are given in \[[11](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/MovieLens%20genre%20associations.pdf)\].

### Titanic

The following computation finds frequent sets of tags that appear in at least 2% of the records. After the computation a table shows the tally of the lengths of the found frequent sets. 

    \[Mu] = 0.02;
    Print["Number of records corresponding to \[Mu]=", \[Mu], ": ", Length[titanicDatasetCatAge]*\[Mu]]
    Print["Computation time:", AbsoluteTiming[
        {aprioriRes, itemToIDRules, idToItemRules} = AprioriApplication[titanicDatasetCatAge, \[Mu]];
     ][[1]]];
    Grid[Prepend[Tally[Map[Length, Join @@ aprioriRes]], {"frequent set\nlength", "number of\nfrequent sets"}], Dividers -> {None, {True, True}}]

   (* Number of records corresponding to mu=0.02: 26.18 *)

   (* Computation time:0.033345 *)

[![Titanic-TagTable1][18]][18]

Note that the command above uses the version of "Titanic" in which the numerical variable "passenger age" was replaced with a categorical representation.

Next we find association rules from the frequent sets that contain "survived". We are asking for association rules that have confidence at least 70% and have support in at least 2% of the records. 

    items = {"survived"};
    itemRules = ItemRules[titanicDatasetCatAge, aprioriRes, itemToIDRules, idToItemRules, items, 0.7, 0.02];

The following command tabulated those of the rules that have "survived" as a consequent. The rules are sorted according to their confidence.

    Magnify[##, 0.7] &@Grid[
      Prepend[
        SortBy[Select[Join @@ itemRules, MemberQ[items, ##[[-1, 1]]] && ##[[2]] > 0.7 &&2 <= Length[##[[-2]]] <= 10 &], -##[[2]] &],
        Map[Style[##, Blue, FontFamily -> "Times"] &, {"Support", "Confidence", "Lift", "Leverage", "Conviction", "Antecedent", "Consequent"}]
      ], Alignment -> Left]

[![Titanic-TagTable2][19]][19]

We can see that the highest confidence of survival is associated with the tag "female". The variable "passenger sex" that has values with that tag should be very decisive. We can confirm this by finding the high confidence association rules that contain "death". We can see that almost all antecedents have the tag "male".

    items = {"died"};
    itemRules = ItemRules[titanicDatasetCatAge, aprioriRes, itemToIDRules, idToItemRules, items, 0.7, 0.02];
    Magnify[##, 0.7] &@Grid[
      Prepend[
        SortBy[Select[Join @@ itemRules, MemberQ[items, ##[[-1, 1]]] && ##[[2]] > 0.7 &&2 <= Length[##[[-2]]] <= 10 &], -##[[2]] &],
	    Map[Style[##, Blue, FontFamily -> "Times"] &, {"Support", "Confidence", "Lift", "Leverage", "Conviction", "Antecedent", "Consequent"}]
      ], Alignment -> Left]

[![Titanic-TagTable3][20]][20]

### Mushroom

Before applying the Apriori algorithm to the "Mushroom" dataset let us modify its values to include the variable (column) names.

    mushroomDatasetWithVarNames = Map[ToString, mushroomDataset /. _Missing -> "NA", {-1}];
    mushroomDatasetWithVarNames = Transpose[MapThread[Function[{col, vn}, Map[vn <> ":" <> ## &, col]], {Transpose[mushroomDatasetWithVarNames], mushroomVarNames}]];
    mushroomDatasetWithVarNames = mushroomDatasetWithVarNames /. {"edibility of mushroom:edible" -> "edible", "edibility of mushroom:poisonous" -> "poisonous"};

A row of this data looks like this:

    mushroomDatasetWithVarNames[[12]]

    (* {"cap-shape:convex", "cap-surface:scaly", "cap-color:yellow", "bruises?:True", "odor:almond", "gill-attachment:free", "gill-spacing:close", "gill-size:broad", "gill-color:brown", "stalk-shape:enlarging", "stalk-root:club", "stalk-surface-above-ring:smooth", "stalk-surface-below-ring:smooth", "stalk-color-above-ring:white", "stalk-color-below-ring:white", "veil-type:partial", "veil-color:white", "ring-number:one", "ring-type:pendant", "spore-print-color:black", "population:scattered", "habitat:meadows", "edible"} *)

The following computation finds frequent sets of 3 or less tags that appear in at least 5% of the records. After the computation a table shows the tally of the lengths of the found frequent sets. 

    \[Mu] = 0.05; mt = 3;
    Print["Number of records corresponding to \[Mu]=", \[Mu], ": ", Length[mushroomDataset]*\[Mu]]
    Print["Computation time:", AbsoluteTiming[
        {aprioriRes, itemToIDRules, idToItemRules} = AprioriApplication[mushroomDatasetWithVarNames, \[Mu], "MaxNumberOfItems" -> mt];
       ][[1]]];
    Grid[Prepend[Tally[Map[Length, Join @@ aprioriRes]], {"frequent set\nlength", "number of\nfrequent sets"}], Dividers -> {None, {True, True}}]

    (* Number of records corresponding to mu=0.05: 406.2 *)

    (* Computation time:9.24365 *)

[![Mushroom-TagTable1][21]][21]

Next we find association rules from the frequent sets that contain "edible". We are asking for association rules that have confidence at least 70% and have support in at least 20% of the records. 

    AbsoluteTiming[
     items = {"edible"};
     itemRules = ItemRules[mushroomDatasetWithVarNames, aprioriRes, itemToIDRules, idToItemRules, items, 0.7, 0.2];
    ]

    (* {1.28314, Null} *)

Next we pick the associations rules with antecedent length 1.

    Magnify[##, 0.7] &@Grid[
      Prepend[
        SortBy[Select[Join @@ itemRules, MemberQ[items, ##[[-1, 1]]] && ##[[2]] > 0.7 && 1 <= Length[##[[-2]]] <= 1 &&Length[##[[-1]]] == 1 &], -##[[2]] &],
        Map[Style[##, Blue, FontFamily -> "Times"] &, {"Support", "Confidence", "Lift", "Leverage", "Conviction", "Antecedent", "Consequent"}]
	  ], Alignment -> Left]

[![Mushroom-TagTable2][22]][22]

What is in the table above is similar to what we observed with mosaic plots for "Mushroom" for the value "none" of the variable "odor". Using the code above we can examine rules with larger number of tags in their antecedents or consequents.

Let us find association rules for the class label "poisonous".

    AbsoluteTiming[
     items = {"poisonous"};
     itemRules = ItemRules[mushroomDatasetWithVarNames, aprioriRes, itemToIDRules, idToItemRules, items, 0.7, 0.20];
    ]

    (* {1.22516, Null} *)

    Magnify[##, 0.7] &@Grid[
      Prepend[
       SortBy[Select[Join @@ itemRules, MemberQ[items, ##[[-1, 1]]] && ##[[2]] > 0.8 && 1 <= Length[##[[-2]]] <= 1 &&Length[##[[-1]]] == 1 &], -##[[2]] &],
       Map[Style[##, Blue, FontFamily -> "Times"] &, {"Support", "Confidence", "Lift", "Leverage", "Conviction", "Antecedent", "Consequent"}]
    ], Alignment -> Left]

[![Mushroom-TagTable3][23]][23]

Association rules that have confidence 1 are logical rules -- see the summary table in the next sub-section. For example, from the table above we can see that mushrooms with foul odor are 100% poisonous. Again this is something we observed with the mosaic plots for "Mushroom". 

### Associations rules measures summary

The following tables are also given in \[[11](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/MovieLens%20genre%20associations.pdf)\] with more detailed corresponding explanations.

[![AssociationRules-Measures1][24]][24]
___

## Confirmations and explanations with Dimension reduction

Somewhat similar -- but orthogonal in character -- to the application of Association rules learning we can apply matrix factorization algorithms in a manner used in Principal Components Analysis (PCA) and Latent Semantic Analysis (LSA) for natural language texts. In LSA with matrix factorizations we obtain topics of words and statistical thesaurus entries; see \[[13](https://mathematicaforprediction.wordpress.com/2013/10/15/statistical-thesaurus-from-npr-podcasts/),[14](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf)\] for matrix factorization applied to podcast transcripts.

See the last section for using dimension reduction before the building the classifiers.

### Topic extraction of linear vector space representations

Applying Non-Negative Matrix Factorization (NNMF) to the "Titanic" dataset produces easy to interpret topics (change of basis vectors). With larger datasets the interpretation of the results from the variable importance perspective is not that straightforward. 

The main difference between NNMF application shown here and PCA is that PCA uses orthogonal transformations of the linear vector space representation of the data, like Singular Value Decomposition (SVD). PCA is good for numerical data, but not that good for categorical data (like the datasets considered in this document). We can apply PCA using the built-in functions `SingularValueDecomposition` or `DimensionReduction`. (See the next sub-section.)

First, as we did for the application of Association rules learning, we concatenate the variable names with the dataset values.

    data = Map[ToString, titanicDatasetCatAge /. _Missing -> "NA", {-1}];
    data = Transpose[MapThread[Function[{col, vn}, Map[vn <> ":" <> ## &, col]], {Transpose[data], titanicVarNames}]];

We can see each row of the dataset as a "bag of words". Next we represent the dataset with a matrix that can be seen as a linear operator that maps passengers to a space of tags (in this case passenger class, sex, age, and survival).

This commands loads a package useful for LSA vector space models \[[15](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m)\].  

    Import["~/MathFiles/MathematicaForPrediction/DocumentTermMatrixConstruction.m"]

The package \[[15](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m)\] has a function, `DocumentTermMatrix`, that can be used to convert a list of lists of tags ("bags of words") into a matrix in which every column corresponds to an unique tag ("word").

    {TM, words} = DocumentTermMatrix[data, {{}, {}}];

Here is the matrix summary:

    TM

    SparseArray[< 5236 >, {1309, 15}]

[![NNMF-MatrixSummary1][29]][29]


Here is table that shows the matrix columns tag interpretation and their corresponding number of non-zero elements:

    Magnify[##, 0.8] &@GridTableForm[Transpose[{words, Total[TM]}], TableHeadings -> {"word", "count"}]

[![Titanic-NNMF1][25]][25]

The matrix $TM \in R^{1309 \times 15}$ that is the vector space representation of the dataset "Titanic".  The $i$-th row of the matrix $TM$ corresponds to the $i$-th Titanic passenger and the values of the row (which are either 0 or 1) are the coefficients for the basis vectors corresponding to the variable values shown in the table above.

The following command loads the package \[[15](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m)\] for performing NNMF:

    Import["~/MathFiles/MathematicaForPrediction/NonNegativeMatrixFactorization.m"]

With the following command we apply an NNMF algorithm to reduce the dimension of the original matrix $TM$ from having 15 columns to having 2.

    {W, H} = GDCLS[TM, 2];

At this point the matrix $TM \in R^{1309 \times 15}$ is approximated with the matrix-matrix product $W H$, $W \in R^{1309 \times 2}$, $H \in R^{2 \times 15}$. The entries of the matrices $W$ and $H$ are non-negative. The applied algorithm finds such $W$ and $H$ that (roughly speaking) a local minimum of $||TM-W H||_{2}$ is obtained. The rows of the matrix $H$ represent the two topics to which the basis of 15 "words" of the dataset was reduced.

In order to do the topic interpretation we have to normalize the product $W H$ in such a way that the rows of $H$ have norms 1. Then each row of the matrix $W$ (i.e. each passenger) has the coefficients for the new basis of two topics.

    {W, H} = RightNormalizeMatrixProduct[W, H];
    Norm /@ H

    (* {1., 1.} *)

Finally, we tabulate the extracted two topics:

    Magnify[##, 0.8] & /@Map[GridTableForm[BasisVectorInterpretation[##, 15, words], TableHeadings -> {"score", "tag"}] &, Normal[H]]

[![Titanic-NNMF2][26]][26]

We can see that top "words" in the extracted two topics confirm that the passenger survival correlates with the passengers being first class and female. We also see that the passenger death correlates with the passengers being male and third class.

### Principal components analysis

PCA is a standard statistical procedure. Because of its utilization of an orthogonal linear space transformation it has limited use when working with categorical data, but here those related PCA properties can be useful to verify the correlations between the values of variables and the class labels.

By applying SVD to the matrix $TM \in R^{1309 \times 15}$ we are going to obtain an approximation of $TM$ with the matrix product $W S H$, $W \in R^{1309 \times 2}$, $S \in R^{2 \times 2}$, $H \in R^{2 \times 15}$.

    {W, S, H} = SingularValueDecomposition[N@TM, 2];

The following command gives the interpretation of the basis vectors:

    Magnify[##, 0.8] & /@Map[GridTableForm[BasisVectorInterpretation[##, 15, words], TableHeadings -> {"score", "tag"}] &, Transpose[H]]

[![Titanic-SVD1][27]][27]

We can see that because of the orthogonality of the new basis vectors they have coordinates with mixed signs. If the data matrix given to SVD is not normalized and centralized, the first vector, with the largest singular value, points to the center of the data. The second vector is oriented in such a way that the data is most diverse (spread out) in its direction. Now let us note that in the obtained second SVD vector (i) "died" and "male" have positive coordinates with close absolute values and (ii) "female" and "survived" have negative coordinates with close absolute values. We can use this observation to confirm that "passenger sex" is the important variable in "Titanic".

## Further investigations

### Repeating the results with several classifiers

It is good idea to verify that we get the same results using different classifiers. Below is given code that computes the shuffled accuracies and returns the relative damage scores for a set of methods of `Classify`.

    mres = Association@Map[
       Function[{clMethod},
        cf = Classify[titanicTrainingData, Method -> clMethod];
        accRes = AccuracyByVariableShuffling[cf, titanicTestData, titanicVarNames, "FScoreLabels" -> "survived"];
        clMethod -> (accRes[None] - Rest[accRes])/accRes[None]
       ], {"LogisticRegression", "NearestNeighbors", "NeuralNetwork", "RandomForest", "SupportVectorMachine"}] ;

    Magnify[##, 0.8] &@Dataset[mres]

[![Titanic-VerifiedClass1][28]][28]

### Pearson coefficient 

Pearson coefficient can be used in the coloring of the mosaic plots. This functionality is not implemented in the package \[[6](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/)\], but it is provided by R's mosaic plots functions and packages. The interpretation is analogous to the impurity function values in the decision trees.

### Parallel implementation

A natural extension for the function `AccuracyByVariableShuffling` is to speed it up by parallel execution. Its parallel implementation is almost trivial using `ParallelMap` or `ParallelTable`.

### Dimension reduction before building the classifiers

We can apply the topic extraction described above first and build a classifier based on the matrix factor $W$. Using dimension reduction is one possible way to improve classifiers performance. Dimension reduction already allows direct variable importance interpretation by examining the norms of the vectors in the new vector bases. Using the classifier based data columns shuffling procedure is for further explanations and verification.

Note that in general with dimension reduction some experiments would be required to determine which variants of LSA, PCA, or NNMF to apply for best results and what is the best number of topics to which to reduce the dimension. Also, because of the mapping of categorical data into a linear vector space, certain suitable numerical thresholds for the coordinates in the new reduced dimension basis have to be determined before building a classifier. 

### DimensionReduction

One of the examples of the (experimental) built-in function `DimensionReduction` is making a recommender. We can turn such a recommender into a classifier and apply the main procedure described in this document.
___

## References

\[1\] Leo Breiman et al., Classification and regression trees, Chapman & Hall, 1984, ISBN-13: 978-0412048418.

\[2\] Anton Antonov, "Classification and association rules for census income data", (2014), [MathematicaForPrediction at WordPress.com](https://mathematicaforprediction.wordpress.com/), URL: https://mathematicaforprediction.wordpress.com/2014/03/30/classification-and-association-rules-for-census-income-data/ .

\[3\] Anton Antonov, [Decision tree and random forest implementations in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m), (2013), source code [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction),  https://github.com/antononcube/MathematicaForPrediction, package [AVCDecisionTreeForest.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m).

\[4\] Anton Antonov, [Implementation of naive Bayesian classifier generation in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/NaiveBayesianClassifier.m), (2013), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), https://github.com/antononcube/MathematicaForPrediction, package [NaiveBayesianClassifier.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/NaiveBayesianClassifier.m).

\[5\] Anton Antonov, [Mosaic plot for data visualization implementation in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/MosaicPlot.m), (2014), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), package [MosaicPlot.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MosaicPlot.m). 

\[6\] Anton Antonov, ["Mosaic plots for data visualization"](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/), (2014), [MathematicaForPrediction at WordPress.com](https://mathematicaforprediction.wordpress.com/). URL: https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/ .

\[7\] Anton Antonov, ["MathematicaForPrediction utilities"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m), (2014), source code [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction),  https://github.com/antononcube/MathematicaForPrediction, package [MathematicaForPredictionUtilities.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m).

\[8\] Anton Antonov, [Variable importance determination by classifiers implementation in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/VariableImportanceByClassifiers.m), (2015), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction),  https://github.com/antononcube/MathematicaForPrediction, package [VariableImportanceByClassifiers.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/VariableImportanceByClassifiers.m).

\[9\] Anton Antonov, ["Waveform recoginition with decision trees"](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Waveform%20recognition%20with%20decision%20trees.pdf), (2013),  [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), https://github.com/antononcube/MathematicaForPrediction, folder [Documentation](https://github.com/antononcube/MathematicaForPrediction/tree/master/Documentation).

\[10\] Amiya Nayak (Editor), Ivan Stojmenovic (Editor), [Handbook of Applied Algorithms: Solving Scientific, Engineering, and Practical Problems](http://onlinelibrary.wiley.com/book/10.1002/9780470175668), Wiley-IEEE Press, 2008, ISBN: 978-0-470-04492-6.

\[11\] Anton Antonov, ["MovieLens genre associations" (2013)](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/MovieLens%20genre%20associations.pdf),  [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), https://github.com/antononcube/MathematicaForPrediction, folder [Documentation](https://github.com/antononcube/MathematicaForPrediction/tree/master/Documentation).

\[12\] Anton Antonov, [Implementation of the Apriori algorithm in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/AprioriAlgorithm.m), (2014), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), package AprioriAlgorithm.m.

\[13\] Anton Antonov, ["Statistical thesaurus from NPR podcasts"](https://mathematicaforprediction.wordpress.com/2013/10/15/statistical-thesaurus-from-npr-podcasts/), (2013), [MathematicaForPrediction at WordPress.com](https://mathematicaforprediction.wordpress.com/). URL: https://mathematicaforprediction.wordpress.com/2013/10/15/statistical-thesaurus-from-npr-podcasts/ .

\[14\] Anton Antonov, ["Topic and thesaurus extraction from a document collection"](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf) (2013),  [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), https://github.com/antononcube/MathematicaForPrediction, folder [Documentation](https://github.com/antononcube/MathematicaForPrediction/tree/master/Documentation).

\[15\] Anton Antonov, [Implementation of the Non-Negative Matrix Factorization algorithm in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m), (2013), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), package [NonNegativeMatrixFactorization.m](https://github.com/antononcube/MathematicaForPrediction).

<!--
[1]:Variable-Intro1.png
[2]:Titanic-Dataset1.png
[3]:Titanic-Dataset2.png
[4]:Mushroom-Dataset1.png
[5]:Mushroom-Dataset2.png
[6]:Titanic-Class-Function1.png
[7]:Mushroom-Class-Function1.png
[8]:Titanic-Mosaic1.png
[9]:Titanic-Mosaic2.png
[10]:Titanic-AgePlot1.png
[11]:Titanic-AgePlot2.png
[12]:Titanic-Mosaic3.png
[13]:Mushroom-Mosaic1.png
[14]:Mushroom-Mosaic2.png
[15]:Titanic-DTree1.png
[16]:Mushroom-DTree1.png
[17]:Mushroom-DTree2.png
[18]:Titanic-TagTable1.png
[19]:Titanic-TagTable2.png
[20]:Titanic-TagTable3.png
[21]:Mushroom-TagTable1.png
[22]:Mushroom-TagTable2.png
[23]:Mushroom-TagTable3.png
[24]:AssociationRules-Measures1.png
[25]:Titanic-NNMF1.png
[26]:Titanic-NNMF2.png
[27]:Titanic-SVD1.png
[28]:Titanic-VerifiedClass1.png
[29]:NNMF-MatrixSummary1.png
-->

[1]:http://i.imgur.com/7bsDyoR.png
[2]:http://i.imgur.com/I9B8nw3.png
[3]:http://i.imgur.com/nYpgybc.png
[4]:http://i.imgur.com/SeA2u1z.png
[5]:http://i.imgur.com/jjhjmC1.png
[6]:http://i.imgur.com/iSdHPEb.png
[7]:http://i.imgur.com/76zOWZF.png
[8]:http://i.imgur.com/X2neSja.png
[9]:http://i.imgur.com/damPzeC.png
[10]:http://i.imgur.com/uCC4GNZ.png
[11]:http://i.imgur.com/UFFXbVu.png
[12]:http://i.imgur.com/uuduXr3.png
[13]:http://i.imgur.com/xbYA8id.png
[14]:http://i.imgur.com/uLHJI2R.png
[15]:http://i.imgur.com/SWwC5fL.png
[16]:http://i.imgur.com/MinOQDL.png
[17]:http://i.imgur.com/9R2N99J.png
[18]:http://i.imgur.com/Oxc0dVo.png
[19]:http://i.imgur.com/7ZtNAmc.png
[20]:http://i.imgur.com/4SEyj5W.png
[21]:http://i.imgur.com/NzbTgpC.png
[22]:http://i.imgur.com/aONm7wp.png
[23]:http://i.imgur.com/GtHRlaT.png
[24]:http://i.imgur.com/3Tdogag.png
[25]:http://i.imgur.com/gNfS3hI.png
[26]:http://i.imgur.com/6pbILxg.png
[27]:http://i.imgur.com/23mhx74.png
[28]:http://i.imgur.com/4mtnrpo.png
[29]:http://i.imgur.com/cyuThdX.png
