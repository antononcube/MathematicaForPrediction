(*
    Java tries with frequencies Mathematica package
    Copyright (C) 2017  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: JavaTriesWithFrequencies *)
(* :Context: JavaTriesWithFrequencies` *)
(* :Author: antonov *)
(* :Date: 2017-01-08 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: prefix tree, trie, Java, Mathematica, Wolfram Language *)
(* :Discussion:


  This package provides Mathematica functions for easier use of the Java implementation of tries with frequencies:

    https://github.com/antononcube/MathematicaForPrediction/tree/master/Java/TriesWithFrequencies

  Although the functions in this package are very close to the ones in the Mathematica package TriesWithFrequencies.m:

    https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m

  the functions in this package can be used independently of TriesWithFrequencies.m -- that is why I made this
  separate package.

  This package can convert the Java tries to JSON, and TriesWithFrequencies.m can convert JSON expressions to tries.
  Therefore, functions of TriesWithFrequencies.m (like TrieForm) can be used over the tries made with this package.

  Before using the package there are two necessary steps:

   1. the building of the jar file TriesWithFrequencies.jar, and

   2. the appropriate Java installation in Mathematica.

  Prescriptions for Step 1 are given in :

    https://github.com/antononcube/MathematicaForPrediction/blob/master/Java/TriesWithFrequencies/README.md

  Here is the Mathematica code for Step 2:

    AppendTo[$Path, "/Users/antonov/MathFiles/MathematicaForPrediction"];
    Needs["JavaTriesWithFrequencies`"]

    $JavaTriesWithFrequenciesPath =
      "/Users/antonov/MathFiles/MathematicaForPrediction/Java/TriesWithFrequencies";

    JavaTrieInstall[$JavaTriesWithFrequenciesPath]

  Several examples of building tries and operations over them follow.

    words = {"barks", "barkers", "barked", "barkeeps", "barkeepers", "barking"};

    jTr = JavaTrieCreateBySplit[words];

    JavaTrieCompleteMatch[jTr, Characters@"bark"]
    (* False *)

    JavaTrieContains[jTr, Characters@"barked"]
    (* True *)

    JavaTrieToJSON@JavaTrieShrink@jTr

    (* {"value" -> 6., "key" -> "",
     "children" -> {{"value" -> 6., "key" -> "ba",
        "children" -> {{"value" -> 7., "key" -> "r",
           "children" -> {{"value" -> 6., "key" -> "k",
              "children" -> {{"value" -> 1., "key" -> "s",
                 "children" -> {}}, {"value" -> 1., "key" -> "ing",
                 "children" -> {}}, {"value" -> 4., "key" -> "e",
                 "children" -> {{"value" -> 1., "key" -> "rs",
                    "children" -> {}}, {"value" -> 1., "key" -> "d",
                    "children" -> {}}, {"value" -> 2., "key" -> "ep",
                    "children" -> {{"value" -> 1., "key" -> "s",
                       "children" -> {}}, {"value" -> 1., "key" -> "ers",
                       "children" -> {}}}}}}}}}}}}}} *)

  For more usage examples see the corresponding unit tests file:

    https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/JavaTriesWithFrequencies-Unit-Tests.wlt

  Anton Antonov
  Windermere, FL
  January 2017
*)

(* This file was created with the Mathematica plug-in for IntelliJ IDEA. *)

BeginPackage["JavaTriesWithFrequencies`"];

JavaTrieClone::usage = "JavaTrieClone[ jTr ] makes a deep clone of a given Java trie.";

JavaTrieHasCompleteMatchQ::usage = "JavaTrieHasCompleteMatchQ[ jTr_, sw:{_String..}] finds does a fraction\
 of the list of strings sw is a complete match in the Java trie jTr.";

JavaTrieContains::usage = "JavaTrieContains[ jTr_, sw:{_String..}] finds is the list of strings\
 sw a complete match in the Java trie jTr.";

JavaTrieCreate::usage = "JavaTrieCreate[ ws:{{_String..}..}] creates a Java trie object from a list of string lists.";

JavaTrieCreateBySplit::usage = "JavaTrieCreateBySplit[ ws:{_String..}, regex_String:\"\"] creates a Java trie object\
 from a list of strings that are split with a given regex.";

JavaTrieEqualQ::usage = "JavaTrieEqualQ[ jTr1, jTr2] compares two Java tries and returns True if\
 the tries have the same shape and all corresponding keys and values are the same.";

JavaTrieGetWords::usage = "JavaTrieGetWords[ jTr_, sw:{_String..}] gives a list words in jTr that start with sw.";

JavaTrieInsert::usage = "JavaTrieInsert[ jTr_, sw:{_String..}] inserts a list of strings into the Java trie jTr.\
 JavaTrieInsert[ jTr_, sws:{{_String..}..}] inserts each of the \"words\" of sws into jTr.";

JavaTrieInstall::usage = "JavaTrieInstall[path_String] installs Java and loads the JavaTrie classes\
 from the jar file in the specified class path.";

JavaTrieKeyQ::usage = "JavaTrieKeyQ[jTr_, sw:{_String..}] finds is the list of strings sw\
a key in the Java trie jTr.";

JavaTrieLeafProbabilities::usage = "Gives the probabilities to reach the leaves of a trie.";

JavaTrieMapOptimizationCall::usage = "Used for optimization calls over lists of \"words\".";

JavaTrieMemberQ::usage = "Same as JavaTrieContains.";

JavaTrieMerge::usage = "Merges two Java tries.";

JavaTrieNodeCounts::usage = "Returns the node counts in trie (total, internal, leaves.)";

JavaTriePosition::usage = "JavaTriePosition[ jTr_, sw:{_String..}] finds a sub-list of the list of strings\
 sw that corresponds to a sub-trie in the Java trie jTr.";

JavaTrieNodeProbabilities::usage = "Gives the corresponding Java trie with node frequencies converted\
 to probabilities.";

JavaTrieRetrieve::usage = "JavaTrieRetrieve[ jTr_, sw:{_String..}] retrieves the sub-trie in jTr\
 that corresponds to sw. Note that only a leading part of sw can be found jTr. \
 The last found element of sw is the root of the returned sub-trie.";

JavaTrieJSONRootToLeafPaths::usage = "Gives lists of key-value pairs corresponding to the root-to-leaf paths\
 in a given trie. (Using a JSON string.)";

JavaTrieRootToLeafPaths::usage = "Gives lists of key-value pairs corresponding to the root-to-leaf paths\
 in a given trie.";

JavaTrieRegexRemove::usage = "Remove nodes that have keys adhering to a regex expression.";

JavaTriePrune::usage = "Prune a trie to a specified maximum level. The root is level 0.";

JavaTrieShrink::usage = "JavaTrieShrink[ jTr_, sep_String:\"\"] concatenates the \"single path\" nodes\
 in the trie jTr using the given separator sep.";

JavaTrieShrinkInternalNodes::usage = "JavaTrieShrinkInternalNodes[ jTr_, sep_String:\"\"] concatenates\
 the \"single path\" internal nodes in the trie jTr using the given separator sep.";

JavaTrieThresholdRemove::usage = "Remove nodes that have values below (or above) a threshold.";

JavaTrieParetoFractionRemove::usage = "Remove nodes that have values below (or above) thresholds derived\
 from a specified Pareto fraction.";

JavaTrieRandomChoice::usage = "Random choice of a root-to-leaf path.";

JavaTrieToJSON::usage = "Converts a Java trie to a corresponding JSON expression.";

JSONTrieToRules::usage = "Converts a JSON trie into rules for GraphPlot.";

JavaTrieForm::usage = "Plots a given Java trie object. Takes options for LayeredGraphPlot";

JavaTrieComparisonGrid::usage = "Makes a grid trie plots for a specified list of Java trie expressions.";

JavaTrieClassify::usage = "JavaTrieClassify[jTr_,record_] classifies a record using a Java trie. \
The signature JavaTrieClassify[jTr_,record_,prop_] can take properties as the ones given to ClassifierFunction. \
JavaTrieClassify[jTr_,record_] is the same as JavaTrieClassify[tr_,record_,\"Decision\"].";


Begin["`Private`"];

Needs["JLink`"];

Clear[JavaTrieClone];
JavaTrieClone[ jTr_?JavaObjectQ ] := jTr@clone[];

Clear[JavaTrieCreateBySplit];
JavaTrieCreateBySplit[words : {_String ..}, regex_String: ""] :=
    Block[{jWords, jSp},
      jWords = MakeJavaObject[words];
      jWords = Arrays`asList[jWords];
      jSp = MakeJavaObject[regex];
      TrieFunctions`createBySplit[jWords, jSp]
    ];

JavaTrieCreateBySplit[ swords : {{_String ..} ..}, regex_String: "®ø®"] :=
    Block[{jWords, jSp},
      jWords = MakeJavaObject[Map[StringJoin @@ Riffle[#, regex] &, swords]];
      jWords = Arrays`asList[jWords];
      jSp = MakeJavaObject[regex];
      TrieFunctions`createBySplit[jWords, jSp]
    ];

Clear[JavaTrieCreate];
JavaTrieCreate[swords : {{_String ..} ..}] :=
    Block[{jWords, jW},
      jWords = JavaNew["java.util.ArrayList"];
      Do[
        jW = JavaNew["java.util.ArrayList"];
        jW@add[MakeJavaObject@#] & /@ wl;
        jWords@add[jW],
        {wl, swords}];
      TrieFunctions`create[jWords]
    ];

Clear[JavaTrieEqualQ];
JavaTrieEqualQ[ jTr1_?JavaObjectQ, jTr2_?JavaObjectQ ] := jTr1@equals[ jTr2 ];

Clear[JavaTrieMerge];
JavaTrieMerge[jTr1_?JavaObjectQ, jTr2_?JavaObjectQ] :=
    TrieFunctions`merge[jTr1, jTr2];

Clear[JavaTrieInsert];
JavaTrieInsert[jTr_?JavaObjectQ, word : {_String ..}] :=
    TrieFunctions`insert[jTr, Arrays`asList[MakeJavaObject[word]]];

JavaTrieInsert[jTr_?JavaObjectQ, word : {_String ..}, val_?NumericQ] :=
    TrieFunctions`insert[jTr, Arrays`asList[JLink`MakeJavaObject[word]], JLink`MakeJavaObject[N[val]], JLink`MakeJavaObject[Null]];

JavaTrieInsert[jTr_?JavaObjectQ, word : {_String ..}, val_?NumericQ, bottomVal_?NumberQ] :=
    TrieFunctions`insert[jTr, Arrays`asList[JLink`MakeJavaObject[word]], JLink`MakeJavaObject[N[val]], JLink`MakeJavaObject[N[bottomVal]]];

JavaTrieInsert[jTr_?JavaObjectQ, words : {{_String ..} ..}] :=
    Block[{jTr2},
      jTr2 = JavaTrieCreate[words];
      TrieFunctions`merge[jTr, jTr2]
    ];

Clear[JavaTrieInstall];
JavaTrieInstall[path_String, opts:OptionsPattern[]] :=
    Block[{},
      Needs["JLink`"];
      JLink`AddToClassPath[path];
      If[ Length[{opts}] > 0,
        JLink`ReinstallJava[opts],
        JLink`ReinstallJava[JLink`JVMArguments -> "-Xmx2g"]
      ];
      JLink`LoadJavaClass["java.util.Collections"];
      JLink`LoadJavaClass["java.util.Arrays"];
      JLink`LoadJavaClass["Trie"];
      JLink`LoadJavaClass["TrieFunctions"];
    ];

Clear[JavaTrieLeafProbabilities, JavaTrieLeafProbabilitiesSimple];

Options[JavaTrieLeafProbabilities] = { "Normalized"->False, "ChopValue"->Automatic };

JavaTrieLeafProbabilities[jTr_?JavaObjectQ, opts:OptionsPattern[]] :=
    Block[{rootVal, chopVal, res},
      res =
          If[ TrueQ[ OptionValue["Normalized"] ],
            rootVal = "value" /. JavaTrieToJSON[jTr, 0];
            JavaTrieLeafProbabilitiesSimple[jTr] /. ("value" -> x_?NumberQ) :> ("value" -> x/rootVal),
          (*ELSE*)
            JavaTrieLeafProbabilitiesSimple[jTr]
          ];
      chopVal = OptionValue["ChopValue"];
      If[ TrueQ[NumericQ[chopVal]],
        Select[res, ("value" /. #) >= chopVal &],
        res
      ]
    ];

(*JavaTrieLeafProbabilitiesSimple[jTr_?JavaObjectQ]:=*)
(*    ImportString[ StringReplace[ TrieFunctions`leafProbabilitiesJSON[jTr], "\"\"\"" -> "\"\\\"\""], "JSON"];*)
JavaTrieLeafProbabilitiesSimple[jTr_?JavaObjectQ]:=
    Block[{str},
      str = StringReplace[ TrieFunctions`leafProbabilitiesJSON[jTr], "\"\"\"" -> "\"\\\"\""];
      ImportString[ FromCharacterCode@ToCharacterCode[str, "UTF-8"], "RawJSON"]
    ];

Clear[JavaTrieNodeCounts];
JavaTrieNodeCounts[jTr_?JavaObjectQ] :=
    AssociationThread[{"total","internal","leaves"}->JLink`JavaObjectToExpression[TrieFunctions`nodeCounts[jTr]]];

Clear[JavaTrieNodeProbabilities];
JavaTrieNodeProbabilities[jTr_?JavaObjectQ] :=
    TrieFunctions`nodeProbabilities[jTr];

Clear[JavaTrieShrink];
JavaTrieShrink[jTr_?JavaObjectQ, sep_String: ""] := TrieFunctions`shrink[jTr, sep];

JavaTrieShrink[jTr_?JavaObjectQ, sep_String, th_?NumberQ ] := TrieFunctions`shrinkByThreshold[jTr, sep, th];

Clear[JavaTrieShrinkInternalNodes];
JavaTrieShrinkInternalNodes[jTr_?JavaObjectQ, sep_String: ""] :=
    TrieFunctions`shrinkInternalNodes[jTr, sep, 1.0 ];

JavaTrieShrinkInternalNodes[jTr_?JavaObjectQ, sep_String, th_?NumberQ ] :=
    TrieFunctions`shrinkInternalNodes[jTr, sep, th];

Clear[JavaTrieToJSON];
(*JavaTrieToJSON[jTr_?JavaObjectQ] := ImportString[jTr@toJSON[], "JSON"];*)
(*JavaTrieToJSON[jTr_?JavaObjectQ, maxLevel_Integer ] := ImportString[jTr@toJSON[maxLevel], "JSON"];*)
JavaTrieToJSON[jTr_?JavaObjectQ] :=
    ImportString[FromCharacterCode@ToCharacterCode[jTr@toJSON[], "UTF-8"], "RawJSON"];

JavaTrieToJSON[jTr_?JavaObjectQ, maxLevel_Integer ] :=
    ImportString[FromCharacterCode@ToCharacterCode[jTr@toJSON[maxLevel], "UTF-8"], "RawJSON"];

Clear[JavaTrieRetrieve];
JavaTrieRetrieve[jTr_?JavaObjectQ, sword : {_String ..}] :=
    TrieFunctions`retrieve[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTrieRetrieve[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaTrieMapOptimizationCall[TrieFunctions`mapRetrieve, jTr, swords];

Clear[JavaTriePosition];
JavaTriePosition[jTr_?JavaObjectQ, sword : {_String ..}] :=
    JavaObjectToExpression @ TrieFunctions`position[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTriePosition[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaObjectToExpression /@
        Map[
          JavaObjectToExpression,
          JavaTrieMapOptimizationCall[TrieFunctions`mapPosition, jTr, swords]
        ];

Clear[JavaTrieKeyQ];
JavaTrieKeyQ[jTr_?JavaObjectQ, sword : {_String ..}] :=
    TrieFunctions`isKey[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTrieKeyQ[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaTrieMapOptimizationCall[TrieFunctions`mapIsKey, jTr, swords];

Clear[JavaTrieHasCompleteMatchQ];
JavaTrieHasCompleteMatchQ[jTr_?JavaObjectQ, sword : {_String ..}] :=
    TrieFunctions`hasCompleteMatch[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTrieHasCompleteMatchQ[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaTrieMapOptimizationCall[TrieFunctions`mapHasCompleteMatch, jTr, swords];

Clear[JavaTrieContains];
JavaTrieContains[jTr_?JavaObjectQ, sword : {_String ..}] :=
    TrieFunctions`contains[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTrieContains[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaTrieMapOptimizationCall[TrieFunctions`mapContains, jTr, swords];

JavaTrieMemberQ = JavaTrieContains;

Clear[JavaTrieMapOptimizationCall];
JavaTrieMapOptimizationCall[func_, jTr_?JavaObjectQ,
  swords : {{_String ..} ..}, splitRegex_String: "®ø®"] :=
    Block[{jWords},
      (*jWords=MakeJavaObject[Map[StringRiffle[#,splitRegex]&,swords]];*)(* 3 times slower*)
      jWords = MakeJavaObject[Map[StringJoin @@ Riffle[#, splitRegex] &, swords]];
      jWords = Arrays`asList[jWords];
      jWords = TrieFunctions`splitWords[jWords, splitRegex];
      func[jTr, jWords]
    ];

Clear[JavaTrieGetWords];
JavaTrieGetWords[jTr_?JavaObjectQ] :=
    Block[{res},
      res = JavaObjectToExpression /@ JavaObjectToExpression[TrieFunctions`getWords[jTr]];
      If[res === Null, {}, res]
    ];
JavaTrieGetWords[jTr_?JavaObjectQ, sword : {_String ..}] :=
    Block[{res},
      res = JavaObjectToExpression /@
          JavaObjectToExpression[
            TrieFunctions`getWords[jTr, Arrays`asList[MakeJavaObject[sword]]]];
      If[res === Null, {}, res]
    ];

Clear[JavaTrieJSONRootToLeafPaths];
JavaTrieJSONRootToLeafPaths[jTr_?JavaObjectQ] :=
    Map[{"key", "value"} /. # &,
      ImportString[TrieFunctions`pathsToJSON[TrieFunctions`rootToLeafPaths[jTr]], "JSON"], {2}];

Clear[JavaTrieRootToLeafPaths];
JavaTrieRootToLeafPaths[jTr_?JavaObjectQ] :=
    Block[{pathsObj, keyPaths, valuePaths},
      pathsObj = JLink`ReturnAsJavaObject[ TrieFunctions`rootToLeafPaths[ jTr ] ];
      keyPaths = JLink`JavaObjectToExpression[TrieFunctions`pathsKeys[ pathsObj ]];
      valuePaths = JLink`JavaObjectToExpression[TrieFunctions`pathsValues[ pathsObj ]];
      MapThread[Transpose[{#1,#2}]&, {JLink`JavaObjectToExpression/@keyPaths, JLink`JavaObjectToExpression/@valuePaths}]
    ];

Clear[JavaTrieRegexRemove];
JavaTrieRegexRemove[jTr_?JavaObjectQ, regex_String ] :=
      TrieFunctions`removeByKeyRegex[jTr, regex];

JavaTrieRegexRemove[jTr_?JavaObjectQ, regex_String, postfix_String ] :=
    TrieFunctions`removeByKeyRegex[jTr, regex, postfix];

Clear[JavaTrieThresholdRemove];
JavaTrieThresholdRemove[jTr_?JavaObjectQ, threshold_?NumericQ, belowThresholdQ:(True|False), postfix_String ] :=
    TrieFunctions`removeByThreshold[jTr, threshold, belowThresholdQ, postfix];

JavaTrieThresholdRemove[jTr_?JavaObjectQ, threshold_?NumericQ, belowThresholdQ:(True|False) ] :=
    TrieFunctions`removeByThreshold[jTr, threshold, belowThresholdQ];

JavaTrieThresholdRemove[jTr_?JavaObjectQ, threshold_?NumericQ] :=
    TrieFunctions`removeByThreshold[jTr, threshold];

JavaTrieThresholdRemove[jTr_?JavaObjectQ, threshold_?NumericQ, postfix_String] :=
    TrieFunctions`removeByThreshold[jTr, threshold, True, postfix];

Clear[JavaTrieParetoFractionRemove];
JavaTrieParetoFractionRemove[jTr_?JavaObjectQ, paretoFraction_?NumericQ, removeBottomElementsQ:(True|False), postfix_String ] :=
    TrieFunctions`removeByParetoFraction[jTr, paretoFraction, removeBottomElementsQ, postfix];

JavaTrieParetoFractionRemove[jTr_?JavaObjectQ, paretoFraction_?NumericQ, removeBottomElementsQ:(True|False) ] :=
    TrieFunctions`removeByParetoFraction[jTr, paretoFraction, removeBottomElementsQ];

JavaTrieParetoFractionRemove[jTr_?JavaObjectQ, paretoFraction_?NumericQ] :=
    TrieFunctions`removeByParetoFraction[jTr, paretoFraction];

JavaTrieParetoFractionRemove[jTr_?JavaObjectQ, paretoFraction_?NumericQ, postfix_String] :=
    TrieFunctions`removeByParetoFraction[jTr, paretoFraction, True, postfix];

Clear[JavaTriePrune];
JavaTriePrune[jTr_?JavaObjectQ, maxLevel_Integer ] :=
    TrieFunctions`prune[jTr, maxLevel];

Clear[JavaTrieRandomChoice];
Options[JavaTrieRandomChoice] = { "Weighted"->True };
JavaTrieRandomChoice[jTr_?JavaObjectQ, opts:OptionsPattern[] ] :=
    JavaObjectToExpression[ TrieFunctions`randomChoice[jTr, TrueQ[OptionValue["Weighted"]] ] ];

JavaTrieRandomChoice[jTr_?JavaObjectQ, n_Integer, opts:OptionsPattern[] ] :=
    JavaObjectToExpression /@ JavaObjectToExpression[ TrieFunctions`randomChoice[jTr, n, TrueQ[OptionValue["Weighted"]] ] ];

Clear[JSONTrieToRules];
JSONTrieToRules[tree_] := Block[{ORDER = 0}, JSONTrieToRules[tree, 0, 0]];
JSONTrieToRules[tree_, level_, order_] :=
    Block[{nodeRules},
      Which[
        tree === {}, {},
        Rest[tree] === {}, {},
        True,
        nodeRules = Map[{{"key","value"}/.tree, {level, order}} -> {{"key","value"}/.#, {level + 1, ORDER++}} &, "children"/.tree, {1}];
        Join[
          nodeRules,
          Flatten[MapThread[JSONTrieToRules[#1, level + 1, #2] &, {"children"/.tree, nodeRules[[All, 2, 2, 2]]}], 1]
        ]
      ]
    ];


Clear[GrFramed];
GrFramed[text_] :=
    Framed[text, {Background -> RGBColor[1, 1, 0.8],
      FrameStyle -> RGBColor[0.94, 0.85, 0.36],
      FrameMargins -> Automatic}];


Clear[JavaTrieForm];
Options[JavaTrieForm] = Options[LayeredGraphPlot];
JavaTrieForm[jTr_?JavaObjectQ, opts:OptionsPattern[]] :=
    LayeredGraphPlot[JSONTrieToRules[JavaTrieToJSON[jTr]],
      opts, VertexShapeFunction -> (Text[GrFramed[#2[[1]]], #1] &), PlotTheme -> "Classic" ];


ClearAll[JavaTrieComparisonGrid];
SetAttributes[JavaTrieComparisonGrid, HoldAll]
Options[JavaTrieComparisonGrid] = Union[Options[Graphics], Options[Grid], {"NumberFormPrecision"->3}];
JavaTrieComparisonGrid[jTrs : {_?JavaObjectQ ..}, opts : OptionsPattern[]] :=
    Block[{graphOpts,gridOpts,nfp},
      graphOpts = Select[{opts}, MemberQ[Options[Graphics][[All, 1]], #[[1]]] &];
      gridOpts = Select[{opts}, MemberQ[Options[Grid][[All, 1]], #[[1]]] &];
      nfp = OptionValue["NumberFormPrecision"];
      Grid[{
        HoldForm /@ Inactivate[jTrs],
        If[ Length[{graphOpts}] == 0,
          Map[JavaTrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v,nfp]} &, jTrs],
          Map[JavaTrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v,nfp]} /. (gr_Graphics) :> Append[gr, graphOpts] &, jTrs],
          Map[JavaTrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v,nfp]} &, jTrs]
        ]
      }, gridOpts, Dividers -> All, FrameStyle -> LightGray]
    ];


Clear[JavaTrieClassify];

Options[JavaTrieClassify] := {"Default" -> None};

JavaTrieClassify[tr_, record_, opts : OptionsPattern[]] :=
    JavaTrieClassify[tr, record, "Decision", opts] /; FreeQ[{opts}, "Probability"|"TopProbabilities"];

JavaTrieClassify[tr_, record_, "Decision", opts : OptionsPattern[]] :=
    First@Keys@JavaTrieClassify[tr, record, "Probabilities", opts];

JavaTrieClassify[tr_, record_, "Probability" -> class_, opts : OptionsPattern[]] :=
    Lookup[JavaTrieClassify[tr, record, "Probabilities", opts], class, 0];

JavaTrieClassify[tr_, record_, "TopProbabilities", opts : OptionsPattern[]] :=
    Select[JavaTrieClassify[tr, record, "Probabilities", opts], # > 0 &];

JavaTrieClassify[tr_, record_, "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Take[JavaTrieClassify[tr, record, "Probabilities", opts], UpTo[n]];

JavaTrieClassify[tr_, record_, "Probabilities", opts : OptionsPattern[]] :=
    Block[{res, dval = OptionValue[JavaTrieClassify, "Default"]},
      res = JavaTrieLeafProbabilities[JavaTrieRetrieve[tr, record], "Normalized"->True];
      If[Length[res] == 0, <|dval -> 0|>,
        res = AssociationThread[res[[All, 1, 2]] -> res[[All, 2, 2]]];
        res = ReverseSort[Association[Rule @@@ res]]
      ]
    ];

JavaTrieClassify[tr_, records:(_Dataset|{_List..}), "Decision", opts : OptionsPattern[]] :=
    First @* Keys @* TakeLargest[1] /@ JavaTrieClassify[tr, records, "Probabilities", opts];

JavaTrieClassify[tr_, records:(_Dataset|{_List..}), "Probability" -> class_, opts : OptionsPattern[]] :=
    Map[ Lookup[#, class, 0]&, JavaTrieClassify[tr, records, "Probabilities", opts] ];

JavaTrieClassify[tr_, records:(_Dataset|{_List..}), "TopProbabilities", opts : OptionsPattern[]] :=
    Map[ Select[#, # > 0 &]&, JavaTrieClassify[tr, records, "Probabilities", opts] ];

JavaTrieClassify[tr_, records:(_Dataset|{_List..}), "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Map[ TakeLargest[#, UpTo[n]]&, JavaTrieClassify[tr, records, "Probabilities", opts] ];

JavaTrieClassify[tr_, records:(_Dataset|{_List..}), "Probabilities", opts:OptionsPattern[] ] :=
    Block[{clRes, classLabels, stencil},

      clRes = Map[ JavaTrieClassify[tr, #, "Probabilities", opts] &, Normal@records ];

      classLabels = Union[Flatten[Normal[Keys /@ clRes]]];

      stencil = AssociationThread[classLabels -> 0];

      KeySort[Join[stencil, #]] & /@ clRes
    ];


End[]; (* `Private` *)

EndPackage[]