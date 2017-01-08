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

  the functions in this package can be used indepenedently of TriesWithFrequencies.m -- that is why I made this
  separate package.

  This package can convert the Java tries to JSON, and TriesWithFrequencies.m can convert JSON experessions to tries.
  Therefore, functions of TriesWithFrequencies.m (like TrieForm) can be used over the tries made with this package.

  Before using the package there are two necessary steps:

   1. the building of the jar file TriesWithFrequencies.jar, and

   2. the appropriate Java installation in Mathematica.

  Prescriptions for Step 1 are given in :

    https://github.com/antononcube/MathematicaForPrediction/blob/master/Java/TriesWithFrequencies/README.md

  Here is the Mathematica code for Step 2:

    $JavaTriesWithFrequenciesPath =
      "/Users/antonov/MathFiles/MathematicaForPrediction/Java/TriesWithFrequencies";

    Needs["JLink`"];
    AddToClassPath[$JavaTriesWithFrequenciesPath];
    ReinstallJava[JVMArguments -> "-Xmx2g"];
    LoadJavaClass["java.util.Collections"];
    LoadJavaClass["java.util.Arrays"];
    LoadJavaClass["Trie"];
    LoadJavaClass["TrieFunctions"];

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

  Anton Antonov
  Windermere, FL
  January 2017
*)

(* This file was created with the Mathematica plug-in for IntelliJ IDEA. *)

BeginPackage["JavaTriesWithFrequencies`"]

JavaTrieCompleteMatch::usage = "JavaTrieCompleteMatch[ jTr_, sw:{_String..}] finds is fraction\
 of the list of strings sw a complete match in the Java trie jTr."

JavaTrieContains::usage = "JavaTrieContains[ jTr_, sw:{_String..}] finds is the list of strings\
 sw a complete match in the Java trie jTr."

JavaTrieCreate::usage = "JavaTrieCreate[ ws:{{_String..}..}] creates a Java trie object from a list of string lists."

JavaTrieCreateBySplit::usage = "JavaTrieCreate[ ws:{_String..}, regex_String:\"\"] creates a Java trie object\
 from a list of strings that are split with a given regex."

JavaTrieGetWords::usage = "JavaTrieGetWords[ jTr_, sw:{_String..}] gives a list words in jTr that start with sw."

JavaTrieInsert::usage = "JavaTrieInsert[ jTr_, sw:{_String..}] inserts a list of strings into the Java trie jTr.\
 JavaTrieInsert[ jTr_, sws:{{_String..}..}] inserts each of the \"words\" of sws into jTr."

JavaTrieMapOptimizationCall::usage = "Used for optimization calls over lists of \"words\"."

JavaTrieMemberQ::usage = "Same as JavaTrieContains."

JavaTrieMerge::usage = "Merges two Java tries."

JavaTrieNodeProbabilities::usage = "Gives the corresponding Java trie with node frequencies converted\
 to probabilities."

JavaTrieRetrieve::usage = "JavaTrieRetrieve[ jTr_, sw:{_String..}] retrieves the sub-trie in jTr\
 that corresponds to sw. Note that only a leading part of sw can be found jTr. \
 The last found element of sw is the root of the returned sub-trie."

JavaTrieRootToLeafPaths::usage = "Gives lists of key-value pairs corresponding to the root-to-leaf paths\
 in a given trie."

JavaTrieShrink::usage = "JavaTrieShrink[ jTr_, sep_String:\"\"] concatenates the \"single path\" nodes\
 in the trie jTr using the given separator sep."

JavaTrieToJSON::usage = "Converts a Java trie to a corresponding JSON expression."

Begin["`Private`"]

Needs["JLink`"]

Clear[JavaTrieCreateBySplit]
JavaTrieCreateBySplit[words : {_String ..}, regex_String: ""] :=
    Block[{jWords, jSp},
      jWords = MakeJavaObject[words];
      jWords = Arrays`asList[jWords];
      jSp = MakeJavaObject[regex];
      TrieFunctions`createBySplit[jWords, jSp]
    ];

Clear[JavaTrieCreate]
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

Clear[JavaTrieMerge]
JavaTrieMerge[jTr1_?JavaObjectQ, jTr2_?JavaObjectQ] :=
    TrieFunctions`merge[jTr1, jTr2];

Clear[JavaTrieInsert]
JavaTrieInsert[jTr_?JavaObjectQ, word : {_String ..}] :=
    TrieFunctions`insert[jTr, Arrays`asList[MakeJavaObject[word]]];

JavaTrieInsert[jTr_?JavaObjectQ, words : {{_String ..} ..}] :=
    Block[{jTr2},
      jTr2 = JavaTrieCreate[words];
      TrieFunctions`merge[jTr, jTr2]
    ];

Clear[JavaTrieNodeProbabilities]
JavaTrieNodeProbabilities[jTr_?JavaObjectQ] :=
    TrieFunctions`nodeProbabilities[jTr];

Clear[JavaTrieShrink]
JavaTrieShrink[jTr_?JavaObjectQ, sep_String: ""] := TrieFunctions`shrink[jTr, sep];

Clear[JavaTrieToJSON]
JavaTrieToJSON[jTr_?JavaObjectQ] := ImportString[jTr@toJSON[], "JSON"];

Clear[JavaTrieRetrieve]
JavaTrieRetrieve[jTr_?JavaObjectQ, sword : {_String ..}] :=
    TrieFunctions`retrieve[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTrieRetrieve[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaTrieMapOptimizationCall[TrieFunctions`mapRetrieve, jTr, swords];

Clear[JavaTrieCompleteMatch]
JavaTrieCompleteMatch[jTr_?JavaObjectQ, sword : {_String ..}] :=
    TrieFunctions`completeMatch[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTrieCompleteMatch[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaTrieMapOptimizationCall[TrieFunctions`mapCompleteMatch, jTr, swords];

Clear[JavaTrieContains]
JavaTrieContains[jTr_?JavaObjectQ, sword : {_String ..}] :=
    TrieFunctions`contains[jTr, Arrays`asList[MakeJavaObject[sword]]];

JavaTrieContains[jTr_?JavaObjectQ, swords : {{_String ..} ..}] :=
    JavaTrieMapOptimizationCall[TrieFunctions`mapContains, jTr, swords];

JavaTrieMemberQ = JavaTrieContains;

Clear[JavaTrieMapOptimizationCall]
JavaTrieMapOptimizationCall[func_, jTr_?JavaObjectQ,
  swords : {{_String ..} ..}, splitRegex_String: "®ø®"] :=
    Block[{jWords},
      (*jWords=MakeJavaObject[Map[StringRiffle[#,splitRegex]&,swords]];*)(* 3 times slower*)
      jWords = MakeJavaObject[Map[StringJoin @@ Riffle[#, splitRegex] &, swords]];
      jWords = Arrays`asList[jWords];
      jWords = TrieFunctions`splitWords[jWords, splitRegex];
      func[jTr, jWords]
    ];

Clear[JavaTrieGetWords]
JavaTrieGetWords[jTr_?JavaObjectQ, sword : {_String ..}] :=
    Block[{res},
      res = JavaObjectToExpression /@
          JavaObjectToExpression[
            TrieFunctions`getWords[jTr, Arrays`asList[MakeJavaObject[sword]]]];
      If[res === Null, {}, res]
    ];

Clear[JavaTrieRootToLeafPaths]
JavaTrieRootToLeafPaths[jTr_?JavaObjectQ] :=
    Map[{"key", "value"} /. # &,
      ImportString[TrieFunctions`pathsToJSON[TrieFunctions`rootToLeafPaths[jTr]], "JSON"], {2}];

End[] (* `Private` *)

EndPackage[]