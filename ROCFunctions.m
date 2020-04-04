(*
    Receiver operating characteristic functions Mathematica package

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

(* :Title: Receiver operating characteristic functions *)
(* :Context: ROCFunctions` *)
(* :Author: Anton Antonov *)
(* :Date: 2016-10-09 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 Anton Antonov *)
(* :Keywords: ROC, Reciever operating characteristic *)
(* :Discussion:


    This package provides Receiver Operating Characteristic (ROC) functions calculation
    and plotting. The ROC framework provides analysis and tuning of binary classifiers.
    (The classifiers are assumed to classify into a positive/true label or a negative/false label. )

    The function ROCFuntions gives access to the individual ROC functions
    through string arguments. Those ROC functions are applied to special objects,
    called ROC Association objects.

    Each ROC Association object is an Association that has the following four keys:
    "TruePositive", "FalsePositive", "TrueNegative", and "FalseNegative" .

    Given two lists of actual and predicted labels a ROC Association object can be made
    with the function ToROCAssociation .

    For more definitions and example of ROC terminology and functions see:
      https://en.wikipedia.org/wiki/Receiver_operating_characteristic .


    Complete usage example with Linear regression
    =============================================

    Note that below although we use both of the provided Titanic training and test data,
    the code is doing only training.  The test data is used to find the the best tuning
    parameter (threshold) through ROC analysis.


    #### Using Titanic data

        ExampleData[{"MachineLearning", "Titanic"}, "TrainingData"][[1 ;; 5]]

    #### Get training and testing data

        data = ExampleData[{"MachineLearning", "Titanic"}, "TrainingData"];
        data = ((Flatten@*List) @@@ data)[[All, {1, 2, 3, -1}]];
        trainingData = DeleteCases[data, {___, _Missing, ___}];

        data = ExampleData[{"MachineLearning", "Titanic"}, "TestData"];
        data = ((Flatten@*List) @@@ data)[[All, {1, 2, 3, -1}]];
        testData = DeleteCases[data, {___, _Missing, ___}];

    #### Replace categorical with numerical values

        trainingData = trainingData /. {"survived" -> 1, "died" -> 0,
            "1st" -> 0, "2nd" -> 1, "3rd" -> 2,
            "male" -> 0, "female" -> 1};

        testData = testData /. {"survived" -> 1, "died" -> 0,
            "1st" -> 1, "2nd" -> 2, "3rd" -> 3,
            "male" -> 0, "female" -> 1};

    #### Do linear regression

        lfm = LinearModelFit[{trainingData[[All, 1 ;; -2]], trainingData[[All, -1]]}]

    #### Get the predicted values

        modelValues = lfm @@@ testData[[All, 1 ;; -2]];

        (*Histogram[modelValues,20]*)
        TableForm[{Range[0, 1, 0.2], Quantile[modelValues, Range[0, 1, 0.2]]}]

    #### Obtain ROC associations over a set of parameter values

        testLabels = testData[[All, -1]];

        thRange = Range[0.1, 0.9, 0.025];
        aROCs = Table[ToROCAssociation[{0, 1}, testLabels, Map[If[# > th, 1, 0] &, modelValues]], {th, thRange}];

    #### Evaluate ROC functions for given ROC association

        Through[ROCFunctions[{"PPV", "NPV", "TPR", "ACC", "SPC"}][aROCs[[3]]]]

    #### Standard ROC plot

        ROCPlot[thRange, aROCs, "PlotJoined" -> Automatic, "ROCPointCallouts" -> True, "ROCPointTooltips" -> True,
         GridLines -> Automatic]

    #### Plot ROC functions wrt to parameter values

        ListLinePlot[
         Map[Transpose[{thRange, #}] &,
          Transpose[Map[Through[ROCFunctions[{"PPV", "NPV", "TPR", "ACC", "SPC"}][#]] &, aROCs]]],
         Frame -> True,
         FrameLabel -> Map[Style[#, Larger] &, {"threshold, \[Theta]", "rate"}],
         PlotLegends ->
          Map[# <> ", " <> (ROCFunctions["FunctionInterpretations"][#]) &, {"PPV", "NPV", "TPR", "ACC", "SPC"}],
         GridLines -> Automatic]


    #### Finding the intersection point of PPV and TPR

    We want to find a point that provides balanced positive and negative labels success rates.
    One way to do this is to find the intersection point of the ROC functions
    PPV (positive predictive value) and TPR (true positive rate).

    Examining the plot above we can come up with the initial condition for x.

         ppvFunc = Interpolation[Transpose@{thRange, ROCFunctions["PPV"] /@ aROCs}];
         tprFunc = Interpolation[Transpose@{thRange, ROCFunctions["TPR"] /@ aROCs}];
         FindRoot[Abs[ppvFunc[x] - tprFunc[x]] == 0, {x, 0.2}]

         (* {x -> 0.3} *)


    ## Comments

    Remark:
     The requirements for atomic labels probably can be removed, but I decided to be conservative and impose
     that restriction.


    Anton Antonov
    2016-10-09
    Windermere, FL, USA
*)

(*

    # Update March-April 2018

    I added the function ROCValues that corresponds in spirit to the function ROCValues in the file:

      https://github.com/antononcube/MathematicaForPrediction/blob/master/R/VariableImportanceByClassifiers.R .

    In R because we can get from most classifiers matrices with named columns that correspond to the class labels
    and entries that correspond to the probabilities for those class labels. (The rows correspond to the test records.)

    In Mathematica the build-in classifiers can return lists of Association objects (using the "Probabilities" property.)
    These lists can easily be turned into Dataset objects that have named columns.

    Hence ROCValues below is based on Dataset objects.

    I also added the Association key "ROCParameter" to the ROC Associations objects. This makes the use ROCPlot
    easier in many cases.

*)

(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginPackage["ROCFunctions`"];

ToROCAssociation::usage = "ToROCAssociation[ {trueLabel, falseLabel}, actualLabels, predictedLabels] converts \
two labels lists (actual and predicted) into an Association that can be used as an argument for the ROC functions. \
See ROCFunctions .";

ROCAssociationQ::usage = "Verifies that the argument is a valid ROC Association object. \
A ROC Association object has the keys \
\"TruePositive\", \"FalsePositive\", \"TrueNegative\", and \"FalseNegative\" .";

ROCFunctions::usage = "Gives access to the implement ROC functions.
It can be used as Thread[ROCFunctions[][rocAssoc]] or Thread[ROCFunctions[{\"TPR\",\"SPC\"}][rocAssoc]] .\
See ROCFunctions[\"FunctionInterpretations\"] for available functions and their interpretations.";

ROCPlot::usage = "Makes a standard ROC plot for specified parameter list and corresponding ROC Association objects. \
ROCPlot takes all options of Graphics and additional options for \
ROC points size, color, callouts, tooltips, and joining. \
The allowed signatures are: \
\nROCPlot[ aROCs:{_?ROCAssociationQ..}, opts] \
\nROCPlot[ parVals:({_?NumericQ..}|Automatic), aROCs:{_?ROCAssociationQ..}, opts] \
\nROCPlot[ xFuncName_String, yFuncName_String, aROCs:{_?ROCAssociationQ..}, opts] \
\nROCPlot[ xFuncName_String, yFuncName_String, parVals:({_?NumericQ..}|Automatic), aROCs:{_?ROCAssociationQ..}, opts]";

ROCValues::usage = "ROCValues[predictionProbabilities_Dataset, actualLabels_List, thRange_?VectorQ ] \
computes ROC associations (for ROCPlot).";

ToClassifyROCCurvePlot::usage = "Changes the style of ROCPlot plots. (Experimental.)";

ConfusionMatrixPlot::usage = "ConfusionMatrixPlot[ aROC_?ROCAssociationQ, labelNames: {yesLabel_, noLabel_}
plots a confusion matrix based on a ROC association.";

ConfusionMatrixPlotFrame::usage = "ConfusionMatrixPlotFrame[mat, refMat, rowNames, columnNames, opts]
frames a given confusion matrix.";

Begin["`Private`"];

Clear[ToROCAssociation];

ToROCAssociation::nalbl = "The the first argument is expected to be list of two atomic elements,
or a list of an atomic label and a list of atomic labels.";

ToROCAssociation::nvecs = "The the second and third arguments are expected to be vectors of the same length.";

ToROCAssociation::sgntrs = "The allowed signatures are one of : \
\nToROCAssociation[ {trueLabel_?AtomQ, falseLabel:(_?AtomQ|{_?AtomQ..})}, actualLabels_, predictedLabels_ ] , \
\nToROCAssociation[ {trueLabel_?AtomQ, falseLabel_?AtomQ}, apfAssoc_Association] .";

ToROCAssociation[ {trueLabel_, falseLabel_}, actualLabels_List, predictedLabels_List ] :=
    Block[{ra,localFalseLabel, flRules},
      If[ ! ( AtomQ[trueLabel] && ( AtomQ[falseLabel] || MatchQ[falseLabel,{_?AtomQ..}] ) ),
        Message[ToROCAssociation::nalbl];
        Return[$Failed]
      ];
      If[ ! ( VectorQ[actualLabels] && VectorQ[predictedLabels] && Length[actualLabels] == Length[predictedLabels] ),
        Message[ToROCAssociation::nvecs];
        Return[$Failed]
      ];
      If[ AtomQ[falseLabel],
        localFalseLabel = falseLabel;
        ra = Tally[Transpose[{actualLabels,predictedLabels}]],
        (*ELSE*)
        localFalseLabel = "Not-"<>ToString[trueLabel];
        flRules = Dispatch[ Thread[falseLabel->localFalseLabel]];
        ra = Tally[Transpose[{actualLabels/.flRules,predictedLabels/.flRules}]]
      ];
      ra = Association[ Rule @@@ ra ];
      ra = Join[ Association @ Flatten[Outer[{#1,#2}->0&,{trueLabel,localFalseLabel},{trueLabel,localFalseLabel}]], ra ];
      ToROCAssociation[{trueLabel, localFalseLabel}, ra]
    ];

ToROCAssociation[ {trueLabel_?AtomQ, falseLabel_?AtomQ}, apfAssoc_Association] :=
    Block[{},
      Association[
        { "TruePositive" -> apfAssoc[{trueLabel, trueLabel}],
          "FalsePositive" -> apfAssoc[{falseLabel, trueLabel}],
          "TrueNegative" -> apfAssoc[{falseLabel, falseLabel}],
          "FalseNegative" -> apfAssoc[{trueLabel, falseLabel}]
        }]
    ];

ToROCAssociation[___] := (Message[ToROCAssociation::sgntrs];$Failed);

Clear[ROCAssociationQ]
ROCAssociationQ[ obj_ ] :=
    AssociationQ[obj] &&
        Length[Intersection[Keys[obj],{"TruePositive","FalsePositive","TrueNegative","FalseNegative"}]] == 4;

TPR[rocAssoc_?ROCAssociationQ] := (rocAssoc["TruePositive"])/(rocAssoc["TruePositive"] + rocAssoc["FalseNegative"]);
TPR[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[TPR, rocs];

SPC[rocAssoc_?ROCAssociationQ] := (rocAssoc["TrueNegative"])/(rocAssoc["FalsePositive"] + rocAssoc["TrueNegative"]);
SPC[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[SPC, rocs];

PPV[rocAssoc_?ROCAssociationQ] := (rocAssoc["TruePositive"])/(rocAssoc["TruePositive"] + rocAssoc["FalsePositive"]);
PPV[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[PPV, rocs];

NPV[rocAssoc_?ROCAssociationQ] := (rocAssoc["TrueNegative"])/(rocAssoc["TrueNegative"] + rocAssoc["FalseNegative"]);
NPV[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[NPV, rocs];

FPR[rocAssoc_?ROCAssociationQ] := (rocAssoc["FalsePositive"])/(rocAssoc["FalsePositive"] + rocAssoc["TrueNegative"]);
FPR[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[FPR, rocs];

FDR[rocAssoc_?ROCAssociationQ] := (rocAssoc["FalsePositive"])/(rocAssoc["FalsePositive"] + rocAssoc["TruePositive"]);
FDR[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[FDR, rocs];

FNR[rocAssoc_?ROCAssociationQ] := (rocAssoc["FalseNegative"])/(rocAssoc["FalseNegative"] + rocAssoc["TruePositive"]);
FNR[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[FNR, rocs];

ACC[rocAssoc_?ROCAssociationQ] := (rocAssoc["TruePositive"] + rocAssoc["TrueNegative"]) / Total[Values[rocAssoc]];
ACC[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[ACC, rocs];

FOR[rocAssoc_?ROCAssociationQ] := 1 - NPV[rocAssoc];
FOR[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[FOR, rocs];

F1[rocAssoc_?ROCAssociationQ] := 2 * PPV[rocAssoc] * TPR[rocAssoc] / ( PPV[rocAssoc] + TPR[rocAssoc] );
F1[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[F1, rocs];

(*
Note the addition of the points {0,0} and {1,1}.
If
  rps = Transpose[{ROCFunctions["FPR"] /@ pROCs, ROCFunctions["TPR"] /@ pROCs}] ]
has points {0, p0} and at {1, p1} then after applying Sort and Partition[#,2,1]&
we will get 0-length intervals and correctly ordered pairs, i.e.
  { {{0,0}, {0,p0}}, {{0, p0}, _}, ___, {_, {1,p1}}, {{1,p1}, {1,1}} } .
Hence the trapezoidal formula integration is going to work correctly.
*)
AUROC[pROCs:{_?ROCAssociationQ..}] :=
    Total[Partition[ Sort @ Join[ { {0,0}, {1,1} }, Transpose[{ROCFunctions["FPR"] /@ pROCs, ROCFunctions["TPR"] /@ pROCs}] ], 2, 1]
        /. {{x1_, y1_}, {x2_, y2_}} :> (x2 - x1) (y1 + (y2 - y1)/2)];
AUROC[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[AUROC, rocs];

MCC[rocAssoc_?ROCAssociationQ] :=
    Block[{tp, tn, fp, fn, tpfp, tpfn, tnfp, tnfn},

      {tp, tn, fp, fn} = Through[ { TPR, SPC, FPR, FNR}[rocAssoc] ];
      {tpfp, tpfn, tnfp, tnfn} = Map[ If[#==0, 1,#]&, { tp + fp, tp + fn, tn + fp, tn + fn }];

      (tp*tn - fp*fn) / Sqrt[ tpfp * tpfn * tnfp * tnfn ]
    ];
MCC[rocs : ({_?ROCAssociationQ..} | <|_?ROCAssociationQ..|> )] := Map[MCC, rocs];


aROCAcronyms =
    AssociationThread[
      {"TPR", "TNR", "SPC", "PPV", "NPV", "FPR", "FDR", "FNR", "ACC", "AUROC", "FOR",
        "F1", "MCC", "Recall", "Precision", "Accuracy", "Sensitivity"},
      {"true positive rate", "true negative rate", "specificity", "positive predictive value",
        "negative predictive value", "false positive rate",
        "false discovery rate", "false negative rate", "accuracy", "area under the ROC curve", "false omission rate",
        "F1 score", "Matthews correlation coefficient",
        "same as TPR", "same as PPV", "same as ACC", "same as TPR"}
    ];

aROCFunctions =
    Join[
      AssociationThread[
        {"TPR", "TNR", "SPC", "PPV", "NPV", "FPR", "FDR", "FNR", "ACC", "AUROC", "FOR", "F1", "MCC"},
        {TPR, SPC, SPC, PPV, NPV, FPR, FDR, FNR, ACC, AUROC, FOR, F1, MCC}],
      AssociationThread[
        {"Recall", "Sensitivity", "Precision", "Accuracy", "Specificity",
          "FalsePositiveRate", "TruePositiveRate", "FalseNegativeRate", "TrueNegativeRate", "FalseDiscoveryRate",
          "FalseOmissionRate", "F1Score", "AreaUnderROCCurve", "MatthewsCorrelationCoefficient" },
        {TPR, TPR, PPV, ACC, SPC, FPR, TPR, FNR, SPC, FDR, FOR, F1, AUROC, MCC }
      ]
    ];


Clear[ROCFunctions]
ROCFunctions["Methods"] := {"FunctionInterpretations", "FunctionNames", "Functions", "Methods", "Properties"};
ROCFunctions["Properties"] := ROCFunctions["Methods"];
ROCFunctions["FunctionNames"] := Keys[aROCAcronyms];
ROCFunctions["FunctionInterpretations"] := aROCAcronyms;
ROCFunctions["FunctionsAssociation"] := aROCFunctions;
ROCFunctions["Functions"] := Union[Values[aROCFunctions]];
ROCFunctions[] := Evaluate[ROCFunctions["FunctionsAssociation"]];
ROCFunctions[fnames:{_String..}] := aROCFunctions/@fnames;
ROCFunctions[fname_String] := aROCFunctions[fname];

Clear[ROCPlot];

ROCPlot::apv = "The parameter values are specified as Automatic, but extracting \"ROCParameter\" from the ROC data\
 did not produce a numerical vector.";

Options[ROCPlot] =
    Join[ {"ROCPointSize"-> 0.02, "ROCColor"-> Lighter[Blue], "ROCPointColorFunction" -> Automatic,
           "ROCPointTooltips"->True, "ROCPointCallouts"->True, "ROCCurveColorFunction" -> Automatic,
           "PlotJoined" -> True }, Options[Graphics]];

ROCSpecQ[arg_] :=
    MatchQ[ arg, {_?ROCAssociationQ..} | {{_?ROCAssociationQ..}..} | Association[ (_->{_?ROCAssociationQ..})..] ];

ROCPlot[ aROCs_?ROCSpecQ, opts:OptionsPattern[]] :=
    ROCPlot[ "FPR", "TPR", Automatic, aROCs, opts];

ROCPlot[ parVals_List, aROCs_?ROCSpecQ, opts:OptionsPattern[]] :=
    ROCPlot[ "FPR", "TPR", parVals, aROCs, opts];

ROCPlot[ xFuncName_String, yFuncName_String, aROCs_?ROCSpecQ, opts:OptionsPattern[]] :=
    ROCPlot[ xFuncName, yFuncName, Automatic, aROCs, opts];

ROCPlot[
  xFuncName_String, yFuncName_String,
  parValsArg : (Automatic | {_?NumericQ..} | _List),
  aROCs : {{_?ROCAssociationQ..}..}, opts : OptionsPattern[]] :=
      ROCPlot[ xFuncName, yFuncName, parValsArg, AssociationThread[ Range[Length[aROCs]], aROCs], opts ];

ROCPlot[
  xFuncName_String, yFuncName_String,
  parValsArg : (Automatic | {_?NumericQ..} | _List),
  aROCs : Association[ (_->{_?ROCAssociationQ..}) .. ], opts : OptionsPattern[]] :=
      Block[{rocCurveColorFunc, cls, grs},

        rocCurveColorFunc = OptionValue[ROCPlot, "ROCCurveColorFunction"];
        If[ TrueQ[rocCurveColorFunc === Automatic],
          rocCurveColorFunc = ColorData["DarkBands", "ColorFunction"];
        ];

        cls =  rocCurveColorFunc /@ Rescale[Range[Length[aROCs]]];
        grs = MapThread[ ROCPlot[xFuncName, yFuncName, #2, opts, "PlotJoined" -> True, "ROCColor" -> #3 ] &, {Keys[aROCs], Values[aROCs], cls}];
        Legended[Show[grs], SwatchLegend[cls, Keys[aROCs]]]
      ];

ROCPlot[
  xFuncName_String, yFuncName_String,
  parValsArg : (Automatic | {_?NumericQ..} | _List),
  aROCs : {_?ROCAssociationQ..}, opts : OptionsPattern[]] :=
    Block[{xFunc, yFunc, psize, rocc, pt, pc, pj, rocpcf, points, parVals=parValsArg, pred},

      psize = OptionValue["ROCPointSize"];
      rocc = OptionValue["ROCColor"];
      rocpcf = OptionValue["ROCPointColorFunction"];
      {pt, pc, pj} = TrueQ[OptionValue[#]] & /@ { "ROCPointTooltips", "ROCPointCallouts", "PlotJoined" };
      pj = pj || !pj && TrueQ[OptionValue["PlotJoined"]===Automatic];

      {xFunc, yFunc} = ROCFunctions[{xFuncName, yFuncName}];

      points = Map[Through[{xFunc,yFunc}[#1]] &, aROCs];

      If[TrueQ[parVals===Automatic], parVals = Map[#["ROCParameter"]&,aROCs] ];
      (*If[ !VectorQ[parVals,NumericQ],
        Message[ROCPlot::apv];
        Return[$Failed]
      ];*)

      pred = Map[VectorQ[#,NumericQ]&, points ];
      points = Pick[points, pred];
      parVals = Pick[parVals, pred];

      Graphics[{
        If[pj, {Lighter[rocc],Line[points]},{}],

        PointSize[psize], rocc,

        If[pj, Line[points]],

        If[ TrueQ[rocpcf===Automatic] || pj,
          Which[
            pt,
            MapThread[Tooltip[Point[#1], #2] &, {points, parVals}],

            !pt,
            Point[points],

            True,
            Nothing
          ],
          (*ELSE*)
          Which[
            pt,
            MapThread[{rocpcf[#1,#2,#3],Tooltip[Point[#1], #2]} &, {points, parVals, Range[Length[points]]}],
            True,
            MapThread[{rocpcf[#1,#2,#3],Point[#]}&, {points, parVals, Range[Length[points]]}]
          ]
        ],

        Black,

        If[ pc,
          MapThread[
            Text[#2, #1, {-1, 2}] &, {points, parVals}],
          {}
        ]},

        AspectRatio -> 1, Frame -> True,

        FrameLabel ->
            Map[Style[StringRiffle[{#, Lookup[ROCFunctions["FunctionInterpretations"], #, Nothing]}, ", "], Larger, Bold] &, {xFuncName,yFuncName}],

        DeleteCases[{opts},
          ( "ROCPointSize" | "ROCColor" | "ROCPointColorFunction" |
            "ROCPointTooltips" | "ROCPointCallouts" | "ROCCurveColorFunction" | "PlotJoined") -> _ ]
      ]
    ] /; Length[parValsArg] == Length[aROCs] || TrueQ[parValsArg===Automatic];


Clear[ROCValues];

ROCValues::nrng = "The range argument is expected to be a list of numbers between 0 and 1.";

ROCValues::nlen = "The prediction probabilities Dataset object and the actual labels (the first and second arguments) \
are expected to have equal lengths.";

ROCValues::nlbl = "The value of \"ClassLabel\" is expected to be one of the columns of the first argument.";

ROCValues::args = "The arguments are expected to be a predictions probabilities Dataset, \
a list of actual labels, and threshold range.";

Options[ROCValues] = {"ClassLabel"->Automatic};

ROCValues[clRes_Dataset, testLabels_List, opts:OptionsPattern[]] :=
    ROCValues[clRes, testLabels, Range[0, 1, 0.05], opts];

ROCValues[predictionProbabilities_Dataset, actualLabels_List, thRange_?VectorQ, opts:OptionsPattern[]] :=
    Block[{ focusClassLabel, classLabels, predictedLabels, rocRes, mainLabel, notMainLabel, modifiedActualLabels},

      If[ Length[predictionProbabilities] != Length[actualLabels],
        Message[ROCValues::nlen];
        $Failed
      ];

      If[ ! ( VectorQ[ thRange, NumberQ] && Apply[And, Map[1 >= # >= 0&, thRange] ] ),
        Message[ROCValues::nrng];
        $Failed
      ];

      focusClassLabel = OptionValue[ROCValues, "ClassLabel"];
      If[ TrueQ[focusClassLabel===Automatic],
        focusClassLabel = First @ Normal @ Keys[predictionProbabilities[1]]
      ];

      If[ !MemberQ[Normal @ Keys[predictionProbabilities[1]], focusClassLabel ],
        Message[ROCValues::nlbl];
        $Failed
      ];

      mainLabel = ToString[focusClassLabel];
      notMainLabel = "Not-"<>mainLabel;
      modifiedActualLabels = If[ # == mainLabel, #, notMainLabel]& /@ actualLabels;

      (*This is no longer actual: classLabels = Normal[Keys[predictionProbabilities[1]]];*)
      classLabels = {mainLabel,notMainLabel};

      Table[
        predictedLabels =
            Normal @ predictionProbabilities[All, If[#[[1]] >= th, mainLabel, notMainLabel] &];

        rocRes = ToROCAssociation[classLabels, modifiedActualLabels, predictedLabels];
        If[ AssociationQ[rocRes],
          Join[<|"ROCParameter" -> th|>, rocRes],
          $Failed
        ]
        , {th, thRange}]
    ];

ROCValues[___] :=
    Block[{},
      Message[ROCValues::args];
      $Failed
    ];

Clear[ToClassifyROCCurvePlot];
ToClassifyROCCurvePlot[gr_] :=
    Block[{cols, pFunc},
      pFunc[x_, {cedge_RGBColor, cface_RGBColor}] := {EdgeForm[cedge], FaceForm[{cface, Opacity[0.34]}], Polygon[x]};
      cols = Cases[gr, _RGBColor, Infinity];
      gr /. {Line[x__] -> pFunc[x, {Darker[Blue], LightBlue}], PointSize[x_] -> PointSize[0.001]}
    ];

(*
Modified/productized version of kglr's MSE answer: https://mathematica.stackexchange.com/a/200221/34008 .
*)

Clear[ConfusionMatrixPlot];

Options[ConfusionMatrixPlot] = Join[ { "Normalize" -> False }, Options[MatrixPlot] ];

ConfusionMatrixPlot[ aROC_?ROCAssociationQ, labelNames: {yesLabel_, noLabel_}: {"True", "False"}, opts:OptionsPattern[] ] :=
   Block[{mat, refMat},

     mat = { {aROC["FalseNegative"], aROC["TruePositive"]}, {aROC["TrueNegative"], aROC["FalsePositive"]}};

     refMat = mat;

     If[ TrueQ[OptionValue[ConfusionMatrixPlot, "Normalize"]],
       mat = N[ mat / { aROC["TruePositive"] + aROC["FalseNegative"], aROC["TrueNegative"] + aROC["FalsePositive"]} ];
     ];

     ConfusionMatrixPlotFrame[ mat, refMat, labelNames, labelNames, opts]
   ];

Clear[ConfusionMatrixPlotFrame];

Options[ConfusionMatrixPlotFrame] = Options[MatrixPlot];

ConfusionMatrixPlotFrame[ mat_?MatrixQ, rowNames_List, columnNames_List, opts:OptionsPattern[] ] :=
    ConfusionMatrixPlotFrame[ mat, mat, rowNames, columnNames, opts ];

ConfusionMatrixPlotFrame[ mat_?MatrixQ, refMat_?MatrixQ, rowNames_List, columnNames_List, opts:OptionsPattern[] ] :=
   Block[{},

     MatrixPlot[mat,
       FilterRules[{opts}, Options[MatrixPlot]],
       ColorRules -> {0 -> White},
       Frame -> True,
       FrameLabel -> {"actual", "predicted"},
       FrameTicks ->
           {{MapIndexed[{#2[[1]], #} &, rowNames], MapIndexed[{#2[[1]], #} &, Total@Transpose@refMat]},
             {MapIndexed[{#2[[1]], #} &, Total[refMat]], MapIndexed[{#2[[1]], #} &, columnNames]}},
(*       ColorFunction -> "Rainbow",*)
       Epilog -> MapIndexed[Text[#, #2 - 1/2] &, Transpose@Reverse@mat, {2}]]
   ];

ConfusionMatrixPlotFrame[ smat_?AssociationQ, opts:OptionsPattern[] ] :=
    Block[{},
      ConfusionMatrixPlotFrame[ Sequence @@ Values[smat], opts]
    ] /; Sort[Keys[smat]] == Sort[{"SparseMatrix", "RowNames", "ColumnNames"}];

End[]; (* `Private` *)

EndPackage[]