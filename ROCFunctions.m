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

  TODO:

*)

(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginPackage["ROCFunctions`"]

ToROCAssociation::usage = "ToROCAssociation[ {trueLabel, falseLabel}, actualLabels, predictedLabels] converts \
two labels lists (actual and predicted) into an Association that can be used as an argument for the ROC functions. \
See ROCFunctions ."

ROCAssociationQ::usage = "Verifies that the argument is a valid ROC Assocition object. \
A ROC Association object has the keys \
\"TruePositive\", \"FalsePositive\", \"TrueNegative\", and \"FalseNegative\" ."

ROCFunctions::usage = "Gives access to the implement ROC functions.
It can be used as Thread[ROCFunctions[][rocAssoc]] or Thread[ROCFunctions[{\"TPR\",\"SPC\"}][rocAssoc]] .\
See ROCFunctions[\"FunctionInterpretations\"] for available functions and their interpretations."

ROCPlot::usage = "Makes a standard ROC plot for specified parameter list and corresponding ROC Association objects. \
ROCPlot takes all options of Graphics and additional options for \
ROC points size, color, callouts, tooltips, and joining. \
The allowed signatures are: \
\nROCPlot[ parVals:{_?NumericQ..}, aROCs:{_?ROCAssociationQ..}, opts]
\nROCPlot[ xFuncName_String, yFuncName_String, parVals:{_?NumericQ..}, aROCs:{_?ROCAssociationQ..}, opts]"

Begin["`Private`"]

Clear[ToROCAssociation]

ToROCAssociation::nalbl = "The the first argument is expected to be list of two atomic elements,
or a list of an atomic label and a list of atomic labels."

ToROCAssociation::nvecs = "The the second and third arguments are expected to be vectors of the same length."

ToROCAssociation::sgntrs = "The alllowed signatures are one of : \
\nToROCAssociation[ {trueLabel_?AtomQ, falseLabel:(_?AtomQ|{_?AtomQ..})}, actualLabels_, predictedLabels_ ] , \
\nToROCAssociation[ {trueLabel_?AtomQ, falseLabel_?AtomQ}, apfAssoc_Association] ."

ToROCAssociation[ {trueLabel_, falseLabel_}, actualLabels_List, predictedLabels_List ] :=
    Block[{ra,localFalseLabel, flRules},
      If[ ! ( AtomQ[trueLabel] && ( AtomQ[falseLabel] || MatchQ[falseLabel,{_?AtomQ..}] ) ),
        Message[ToROCAssociation::nalbl]
        Return[$Failed]
      ];
      If[ ! ( VectorQ[actualLabels] && VectorQ[predictedLabels] && Length[actualLabels] == Length[predictedLabels] ),
        Message[ToROCAssociation::nvecs]
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

SPC[rocAssoc_?ROCAssociationQ] := (rocAssoc["TrueNegative"])/(rocAssoc["FalsePositive"] + rocAssoc["TrueNegative"]);

PPV[rocAssoc_?ROCAssociationQ] := (rocAssoc["TruePositive"])/(rocAssoc["TruePositive"] + rocAssoc["FalsePositive"]);

NPV[rocAssoc_?ROCAssociationQ] := (rocAssoc["TrueNegative"])/(rocAssoc["TrueNegative"] + rocAssoc["FalseNegative"]);

FPR[rocAssoc_?ROCAssociationQ] := (rocAssoc["FalsePositive"])/(rocAssoc["FalsePositive"] + rocAssoc["TrueNegative"]);

FDR[rocAssoc_?ROCAssociationQ] := (rocAssoc["FalsePositive"])/(rocAssoc["FalsePositive"] + rocAssoc["TruePositive"]);

FNR[rocAssoc_?ROCAssociationQ] := (rocAssoc["FalseNegative"])/(rocAssoc["FalseNegative"] + rocAssoc["TruePositive"]);

ACC[rocAssoc_?ROCAssociationQ] := (rocAssoc["TruePositive"] + rocAssoc["TrueNegative"]) / Total[Values[rocAssoc]];

AUROC[pROCs:{_?ROCAssociationQ..}] :=
    Total[Partition[ Sort@Transpose[{ROCFunctions["FPR"] /@ pROCs, ROCFunctions["TPR"] /@ pROCs}], 2, 1]
        /. {{x1_, y1_}, {x2_, y2_}} :> (x2 - x1) (y1 + (y2 - y1)/2)];


aROCAcronyms =
    AssociationThread[{"TPR", "SPC", "PPV", "NPV", "FPR", "FDR", "FNR", "ACC", "AUROC"} ->
        {"true positive rate", "specificity", "positive predictive value",
          "negative predictive value", "false positive rate",
          "false discovery rate", "false negative rate", "accuracy", "area under the ROC curve"}];

aROCFunctions =
    AssociationThread[{"TPR", "SPC", "PPV", "NPV", "FPR", "FDR", "FNR", "ACC", "AUROC"} ->
        {TPR,SPC,PPV,NPV,FPR,FDR,FNR,ACC,AUROC}];


Clear[ROCFunctions]
ROCFunctions["Methods"] := {"FunctionInterpretations", "FunctionNames", "Functions", "Methods", "Properties"};
ROCFunctions["Properties"] := ROCFunctions["Methods"];
ROCFunctions["FunctionNames"] := Keys[aROCAcronyms];
ROCFunctions["FunctionInterpretations"] := aROCAcronyms;
ROCFunctions["Functions"] := {TPR,SPC,PPV,NPV,FPR,FDR,FNR,ACC,AUROC};
ROCFunctions[] := Evaluate[ROCFunctions["Functions"]];
ROCFunctions[fnames:{_String..}] := aROCFunctions/@fnames;
ROCFunctions[fname_String] := aROCFunctions[fname];

Clear[ROCPlot]

Options[ROCPlot] =
    Join[ {"ROCPointSize"-> 0.02, "ROCColor"-> Lighter[Blue], "ROCPointColorFunction" -> Automatic,
      "ROCPointTooltips"->True, "ROCPointCallouts"->True, "PlotJoined" -> False }, Options[Graphics]];

ROCPlot[ parVals:{_?NumericQ..}, aROCs:{_?ROCAssociationQ..}, opts:OptionsPattern[]] :=
    ROCPlot[ "FPR", "TPR", parVals, aROCs, opts];

ROCPlot[
  xFuncName_String, yFuncName_String,
  parVals:{_?NumericQ..},
  aROCs:{_?ROCAssociationQ..}, opts:OptionsPattern[]] :=
    Block[{xFunc, yFunc, psize, rocc, pt, pc, pj, pja, rocpcf, points},
      psize = OptionValue["ROCPointSize"];
      rocc = OptionValue["ROCColor"];
      rocpcf = OptionValue["ROCPointColorFunction"];
      {pt, pc, pj} = TrueQ[OptionValue[#]] & /@ { "ROCPointTooltips", "ROCPointCallouts", "PlotJoined" };
      pja = TrueQ[OptionValue["PlotJoined"]===Automatic];
      {xFunc, yFunc} = ROCFunctions[{xFuncName, yFuncName}];
      points = Map[Through[{xFunc,yFunc}[#1]] &, aROCs];
      Graphics[{
        If[pja, {Lighter[rocc],Line[points]},{}],
        PointSize[psize], rocc,
        If[ TrueQ[rocpcf===Automatic] || pj,
          Which[
            pt && !pj,
            MapThread[Tooltip[Point[#1], #2] &, {points, parVals}],
            !pt && !pj,
            Point[points],
            True,
            Line[points]
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
            Text[#2, Through[{xFunc,yFunc}[#1]], {-1, 2}] &, {aROCs, parVals}],
          {}
        ]},
        AspectRatio -> 1, Frame -> True,
        FrameLabel ->
            Map[Style[#<>", "<>ROCFunctions["FunctionInterpretations"][#], Larger, Bold] &, {xFuncName,yFuncName}],
        DeleteCases[{opts},
          ( "ROCPointSize" | "ROCColor" | "ROCPointColorFunction" |
            "ROCPointTooltips" | "ROCPointCallouts" | "PlotJoined") -> _ ]
      ]
    ]/; Length[parVals] == Length[aROCs];

End[] (* `Private` *)

EndPackage[]