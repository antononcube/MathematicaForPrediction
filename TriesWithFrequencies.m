(*
    Tries with frequencies Mathematica package
    Copyright (C) 2013  Anton Antonov

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

(* Version 0.8 *)
(* This version contains functions to build, shrink, and retrieve nodes of tries (also known as "prefix trees").
   The implementations are geared toward utilization of data mining algorithms, like frequent sequence occurrences. *)

(* TODO:
  1. Enhance the functionality of TriePosition to work over shrunk tries.
  2. Enhance the signature and functionality of TrieLeafProbabilities to take a second argument of a "word"
     for which the leaf probabilities have to be found.
*)

BeginPackage["TriesWithFrequencies`"]

TriePosition::usage = "TriePosition[t_, w :(_String | _List)] gives the position node corresponding to the last \"character\" of the \"word\" w in the trie t.\
 Strings are converted to lists first."

TrieRetrieve::usage = "TrieRetrieve[t_, w :(_String | _List)] gives the node corresponding to the last \"character\" of the \"word\" w in the trie t.\
 Strings are converted to lists first."

TrieSubTrie::usage = "TrieRetrieve[t_, w :(_String | _List)] gives the sub-trie corresponding to the last \"character\" of the \"word\" w in the trie t.\
 Strings are converted to lists first."

TrieCreate::usage = "TrieCreate[words:{(_String|_List)..}] creates a trie from a list of strings or a list of lists."

TrieInsert::usage = "TrieInsert[t_, w : (_String | _List)] insert a \"word\" to the trie t. TrieInsert[t_, w : (_String | _List), val_] inserts a key and a corresponding value."

TrieMerge::usage = "TrieMerge[t1_, t2_] merges two tries."

TrieShrink::usage = "TrieShrink shrinks the leaves and internal nodes into prefixes."

TrieToRules::usage = "Converts a trie into a list of rules suitable for visualization with GraphPlot and LayeredGraphPlot. To each trie node is added a list of its level and its traversal order."

TrieForm::usage = "Shrinks the trie argument and returns a list of rules for a graph plot of it. In order to eliminate ambiguity each node is with its traversal order."

TrieNodeProbabilities::usage = "Converts the frequencies at the nodes of a trie into probabilities. The value of the option \"ProbabilityModifier\" is a function that is applied to the computed probabilities."

TrieNodeFrequencies::usage = "Converts the probabilities at the nodes of a trie into frequencies. The value of the option \"FrequencyModifier\" is a function that is applied to the computed frequencies."

TrieLeafProbabilities::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie."

TrieLeafProbabilitiesWithPositions::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie. For each leaf its position in the trie is given."

TriePositionParts::usage = "Transforms a list of the form {a[1],a[2],...,a[n],1} into {{1}, {a[1],1}, {a[1],a[2],1}, ..., {a[1],a[2],...,a[n],1}}."

TriePathFromPosition::usage = "TriePathFromPosition[trie,pos] gives a list of nodes from the root of a trie to the node at a specified position."

TrieRootToLeafPaths::usage = "TrieRootToLeafPaths[trie] gives all paths from the root node to the leaf nodes."

TrieRemove::usage = "TrieRemove removes a \"word\" from a trie."

TrieCompleteMatch::usage = "TrieCompleteMatch[ t, pos ] checks is the position list pos of \"word\" is a complete match in the trie t."

TrieMemberQ::usage = "TrieMemberQ[t, w] checks is the \"word\" w in the trie t."

TriePrune::usage = "TriePrune[t,maxLvl] prunes the trie to a maximum node level. (The root is level 0.)"

ToTrieFromJSON::usage = "ToTrieFromJSON[jsonTrie:{_Rule...}] converts a JSON import into a Trie object. \
ToTrieFromJSON[jsonTrie:{_Rule...}, elementNames:{key_String, value_String, children_String}] is going to use \
the specified element names for the conversion."
	
Begin["`Private`"]

Clear[TrieRootQ]
TrieRootQ[t_]:= TrueQ[ ListQ[t[[1]]] && Length[t[[1]]] == 0 ];

Clear[TriePosition, TriePositionRec]
TriePosition[t_, word_String] := TriePosition[t, Characters[word]];
TriePosition[t_, {}] := {};
TriePosition[{}, _] := {};
TriePosition[{_}, _] := {};
TriePosition[t_, chars_] :=
    Block[{res},
      If[ TrieRootQ[ t[[1]] ],
        TriePositionRec[ t, chars ],
        res = TriePositionRec[ { {{}, t[[1, 2]] }, t }, chars ];
        If[Length[res] == 0, {}, Rest[res] ]
      ]
    ];
TriePositionRec[t_, {}] := {};
TriePositionRec[{}, _] := {};
TriePositionRec[{_}, _] := {};
TriePositionRec[t_, chars_] :=
  Block[{i = 2},
   (* This assumes that the first node is a standard root i.e. empty list. *)
   While[i <= Length[t] && t[[i, 1, 1]] =!= chars[[1]], i++];
   If[i > Length[t], {},
    Join[{i}, TriePositionRec[t[[i]], Rest[chars]]]
   ]
  ];

TrieSubTrie[t_, word_String] := TrieSubTrie[t, Characters[word]];
TrieSubTrie[t_, chars_] :=
    Block[{pos},
      pos = TriePosition[t, chars];
      Which[
        Length[pos] == 0, {},
        TrieRootQ[t[[1]]] && Length[pos] == Length[chars], t[[Sequence @@ pos]],
        Length[pos] + 1 == Length[chars], t[[Sequence @@ pos]],
        True, {}
      ]
    ];

TrieRetrieve[t_, word_String] := TrieRetrieve[t, Characters[word]];
TrieRetrieve[t_, chars_] :=
    Block[{res},
      res = TrieSubTrie[t, chars];
      If[ Length[res] == 0, {}, res[[1]] ]
    ];


Clear[MakeTrie]
MakeTrie[word_String] := MakeTrie[word, 1];
MakeTrie[word_String, v_Integer] := With[{chars = Characters[word]}, MakeTrie[chars,v]];
MakeTrie[chars_List] := MakeTrie[chars, 1];
MakeTrie[chars_List, v_Integer] := Fold[{{#2, v}, #1} &, {{Last[chars], v}}, Reverse@Most@chars];

Clear[TrieMerge]
TrieMerge[{}, {}] := {};
TrieMerge[t1_, t2_] :=
  Which[
   TrueQ[ t1[[1, 1]] != t2[[1, 1]] ], {t1, t2},
   TrueQ[ t1[[1, 1]] == t2[[1, 1]] ],
   Prepend[
    Join[
     Select[Rest[t1], ! MemberQ[Rest[t2][[All, 1, 1]], #[[1, 1]]] &],
     Select[Rest[t2], ! MemberQ[Rest[t1][[All, 1, 1]], #[[1, 1]]] &],
     DeleteCases[
      Flatten[
       Outer[If[ TrueQ[ #1[[1, 1]] == #2[[1, 1]] ], TrieMerge[#1, #2], {}] &, Rest[t1], Rest[t2], 1],
       1], {}]
     ], {t1[[1, 1]], t1[[1, 2]] + t2[[1, 2]]}],
   TrueQ[ Rest[t1] === {} ], t2,
   TrueQ[ Rest[t2] === {} ], t1
  ];

Clear[TrieInsert]

TrieInsert[t_, word : (_String | _List)] := TrieMerge[t, {{{}, 1}, MakeTrie[word]}];

TrieInsert[t_, wordKey : (_String | _List), value_] :=
  Block[{mt},
    mt = MakeTrie[wordKey,0];
    mt[[Sequence @@ Join[Table[2, {Depth[mt] - 3}], {1, 2}]]] = value;
    TrieMerge[t, {{{}, 0}, mt}]
  ];

TrieCreate1[ {}] := {{{}, 0}};
TrieCreate1[words : {(_String | _List) ...}] :=
  Fold[TrieInsert, {{{}, 1}, MakeTrie[First[words]]}, Rest@words];

Clear[TrieCreate]
TrieCreate[ {}] := {{{}, 0}};
TrieCreate[words : {(_String | _List) ...}] :=
  Block[{},
   If[
    Length[words] <= 5, TrieCreate1[words],
    (*ELSE*)
    TrieMerge[
     TrieCreate[Take[words, Floor[Length[words]/2]]],
     TrieCreate[Take[words, {Floor[Length[words]/2] + 1, Length[words]}]]
    ]
   ]
  ];

Clear[TrieRemoveFrequencies]
TrieRemoveFrequencies[t_] :=
  Which[
   MatchQ[t, {_, _Integer}], t[[1]],
   MatchQ[t, {{_, _Integer}}], {t[[1, 1]]},
   MatchQ[t, {{_, _Integer}, ___}],
   Prepend[TrieRemoveFrequencies /@ Rest[t], t[[1, 1]]]
  ];

Clear[NodeJoin]
NodeJoin[n_String] := n;
NodeJoin[n1_String, n2_String] := n1 <> n2;
NodeJoin[n1_, n2_String] := n2;
NodeJoin[n_] := TH[n];
NodeJoin[n1_TH, n2_TH] := TH @@ Join[n1, n2];
NodeJoin[n1_, n2_TH] := Join[TH[n1], n2];
NodeJoin[n1_, n2_] := TH[n1, n2];

Clear[TrieShrink, TrieShrinkRec];
TrieShrink[t_] := TrieShrinkRec[t] /. TH -> List;
TrieShrinkRec[{}] := {};
TrieShrinkRec[t_] :=
  Block[{tt, newnode, rootQ = TrieRootQ[t[[1]]] },
   Which[
    ! rootQ && Length[t] == 1, {{NodeJoin[t[[1, 1]]], t[[1, 2]]}},
    ! rootQ && Length[t] == 2 && t[[1, 2]] == t[[2, 1, 2]],
    tt = TrieShrinkRec[t[[2]]];
    newnode = {NodeJoin[t[[1, 1]], tt[[1, 1]]], tt[[1, 2]]};
    If[Length[tt] == 1,
     {newnode},
     Prepend[Rest@tt, newnode]
    ],
    True,
    Prepend[TrieShrinkRec /@ Rest[t],
     If[rootQ, t[[1]], {NodeJoin[t[[1, 1]]], t[[1, 2]]}]]
   ]
  ];

Clear[TrieMapAtNodes];
TrieMapAtNodes[{}] := {};
TrieMapAtNodes[func_, t_] :=
  Which[
   Length[t] == 1, func[t[[1]]],
   True,
   Prepend[TrieMapAtNodes[func, #] & /@ Rest[t], func[t[[1]]]]
  ];

Clear[TrieFold];
TrieFold[{}] := {};
TrieFold[func_, t_] :=
  Which[
   Length[t] == 1, {func[t]},
   True,
   Prepend[TrieFold[func, #] & /@ Rest[t], func[t]]
  ];


Clear[TrieToRules]
TrieToRules[tree_] := Block[{ORDER = 0}, TrieToRules[tree, 0, 0]];
TrieToRules[tree_, level_, order_] :=
  Block[{nodeRules},
   Which[
    tree === {}, {},
    Rest[tree] === {}, {},
    True,
    nodeRules = Map[{tree[[1]], {level, order}} -> {#[[1]], {level + 1, ORDER++}} &, Rest[tree], {1}];
    Join[
     nodeRules,
     Flatten[MapThread[TrieToRules[#1, level + 1, #2] &, {Rest[tree], nodeRules[[All, 2, 2, 2]]}], 1]
    ]
   ]
  ];

Clear[GrFramed]
GrFramed[text_] :=
  Framed[text, {Background -> RGBColor[1, 1, 0.8],
    FrameStyle -> RGBColor[0.94, 0.85, 0.36],
    FrameMargins -> Automatic}];

Clear[TrieForm]
TrieForm[mytrie_, opts:OptionsPattern[]] :=
  LayeredGraphPlot[TrieToRules[mytrie],
   VertexRenderingFunction -> (Text[GrFramed[#2[[1]]], #1] &), opts];

TrieNodeProbabilities::ntnode = "Non trie node was encountered: `1`. A trie node has two elements prefix and frequency."

Clear[TrieNodeProbabilities, TrieNodeProbabilitiesRec]
Options[TrieNodeProbabilities] = {"ProbabilityModifier" -> N};
Options[TrieNodeProbabilitiesRec] = Options[TrieNodeProbabilities];
TrieNodeProbabilities[trie_, opts : OptionsPattern[]] :=
    ReplacePart[TrieNodeProbabilitiesRec[trie, opts], {1, 2} -> 1];
TrieNodeProbabilitiesRec[trie_, opts : OptionsPattern[]] :=
    Block[{sum, res, pm = OptionValue["ProbabilityModifier"]},
      Which[
        Rest[trie] == {}, trie,
        True,
        If[trie[[1, 2]] == 0,
          sum = Total[Rest[trie][[All, 1, 2]]],
          sum = trie[[1, 2]],
          Message[TrieNodeProbabilities::ntnode,trie[[1]]];
          Return[{}]
        ];
        res = Map[TrieNodeProbabilitiesRec[#, opts] &, Rest[trie]];
        res[[All, 1, 2]] = Map[pm, res[[All, 1, 2]]/sum];
        Prepend[res, First[trie]]
      ]
    ];

TrieNodeFrequencies::ntnode = "Non trie node was encountered: `1`. A trie node has two elements prefix and frequency."

Clear[TrieNodeFrequencies, TrieNodeFrequenciesRec]
Options[TrieNodeFrequencies] = {"FrequencyModifier" -> N};
Options[TrieNodeFrequenciesRec] = Options[TrieNodeFrequencies];
TrieNodeFrequencies[trie_, opts : OptionsPattern[]] := TrieNodeFrequencies[ trie, 1, opts ];
TrieNodeFrequencies[trie_, scale_?NumericQ, opts : OptionsPattern[]] :=
    TrieNodeFrequenciesRec[ ReplacePart[trie, {1,2}->scale * trie[[1,2]]], opts];
TrieNodeFrequenciesRec[trie_, opts : OptionsPattern[]] :=
    Block[{sum, res, pm = OptionValue["FrequencyModifier"]},
      Which[
        Rest[trie] == {}, trie,
        True,
        res = Map[TrieNodeFrequenciesRec[#, opts] &, Map[ ReplacePart[#,{1,2}->#[[1,2]]*trie[[1,2]] ]& , Rest[trie] ]];
        res[[All, 1, 2]] = Map[pm, res[[All, 1, 2]]];
        Prepend[res, First[trie]]
      ]
    ];


TrieLeafProbabilities::ntnode = "Non trie node was encountered: `1`. A trie node has two elements prefix and frequency."
TrieLeafProbabilitiesWithPositions::ntnode = "Non trie node was encountered: `1`. A trie node has two elements prefix and frequency."

Clear[TrieLeafProbabilities]
TrieLeafProbabilities[trieArg_] :=
  Block[{TrieLeafProbabilitiesRec},

   TrieLeafProbabilitiesRec[trie_] :=
    Block[{res, sum},
     Which[
      Rest[trie] == {}, trie,
      True,
      sum = Total[Rest[trie][[All, 1, 2]]];
      res = Map[TrieLeafProbabilitiesRec[#] &, Rest[trie]];
      If[sum < 1, res = Append[res, {{trie[[1, 1]], 1 - sum}}]];
      res = Map[{#[[1]], #[[2]]*trie[[1, 2]]} &, Flatten[res, 1]]
     ]
    ];

   If[trieArg[[1, 2]] == 0,
    TrieLeafProbabilitiesRec[trieArg],
    Map[{#[[1]], #[[2]]} &, TrieLeafProbabilitiesRec[trieArg]],
    Message[TrieLeafProbabilities::ntnode,trieArg[[1]]];
    Return[{}]
   ]
  ];

Clear[TrieLeafProbabilitiesWithPositions]
TrieLeafProbabilitiesWithPositions[trieArg_] :=
  Block[{TrieLeafProbabilitiesRec},

   TrieLeafProbabilitiesRec[trie_] :=
    Block[{res, sum},
     Which[
      Rest[trie] === {}, {Append[trie[[1]], {1}]},
      True,
      sum = Total[Rest[trie][[All, 1, 2]]];
      res = Map[TrieLeafProbabilitiesRec[#] &, Rest[trie]];
      res =
       MapThread[
        Function[{r, ind},
         Map[Append[Most[#], Prepend[#[[-1]], ind]] &, r]
         ], {res, Range[2, Length[trie]]}];
      If[sum < 1, res = Append[res, {{trie[[1, 1]], 1 - sum, {1}}}]];
      res = Map[{#[[1]], #[[2]]*trie[[1, 2]], #[[3]]} &, Flatten[res, 1]]
     ]
    ];

   If[trieArg[[1, 2]] == 0,
    TrieLeafProbabilitiesRec[trieArg],
    Map[{#[[1]], #[[2]], #[[3]]} &, TrieLeafProbabilitiesRec[trieArg]],
    Message[TrieLeafProbabilitiesWithPositions::ntnode,trieArg[[1]]];
    Return[{}]
   ]
  ];

Clear[TriePositionParts]
TriePositionParts[pos : {_Integer ..}] :=
  Map[Append[#, 1] &, FoldList[Join[#1, {#2}] &, {}, Most[pos]]];

Clear[TriePathFromPosition]
TriePathFromPosition[trie_, pos : {_Integer ...}] :=
  Block[{ps},
   ps = FoldList[Append[#1, #2] &, {First[pos]}, Rest[pos]];
   Fold[Append[#1, trie[[Sequence @@ Append[#2, 1]]]] &, {}, Most[ps]]
  ];

Clear[TrieRootToLeafPaths]
TrieRootToLeafPaths[trie_] :=
  Block[{rows = {}, TrieRows},
   TrieRows[t_, r_] :=
    Which[
     Length[Rest[t]] == 0, AppendTo[rows, Append[r, t[[1]]]],
     True, Map[TrieRows[#, Append[r, t[[1]]]] &, Rest[t]]
    ];
   TrieRows[trie, {}];
   rows
  ];

TrieRemove::nnval = "The value of the option \"HowManyTimes\" is expected to a number, Automatic, or All.";

TrieRemove::nprob = "If \"FrequencyValues\"->False then \"HowManyTimes\" is taken to be Automatic.";

Clear[TrieRemove, TrieRemoveRecFreq, TrieRemoveRecProb]
Options[TrieRemove] = {"HowManyTimes" -> Automatic, "FrequencyValues" -> Automatic};
TrieRemove[trie_, wordArg : (_List | _String), opts : OptionsPattern[]] :=
  Block[{word = wordArg,
    nVal = OptionValue[TrieRemove, "HowManyTimes"],
    freqVals = OptionValue[TrieRemove, "FrequencyValues"]},
   If[StringQ[word], word = Characters[word]];
   If[ freqVals === Automatic,
     freqVals =
         Which[
           IntegerQ[ trie[[1,2]] ] && trie[[1,2]] > 1, True,
           Length[ Rest[trie] ] > 1 && trie[[1,2]] <= 1,
           If[ Total[ Rest[trie][[All,1,2]] ] <= 1, False, True],
           True, True
         ];
   ];
   If[! TrueQ[freqVals] && ! (TrueQ[nVal === Automatic] || TrueQ[nVal === All]),
     Message[TrieRemove::nprob];
     nVal = Automatic;
   ];
   If[nVal === Automatic || nVal === All,
     nVal = TrieRetrieve[trie, word];
     If[ nVal==={}, Return[trie] ];
     nVal = nVal[[2]];
   ];
   If[! NumberQ[nVal],
     Message[TrieRemove::nnval]; Return[{}];
   ];
   If[TrueQ[freqVals],
     TrieRemoveRecFreq[trie, Prepend[word, {}], nVal]
     ,(*ELSE*)
     TrieRemoveRecProb[trie, Prepend[word, {}], nVal]
   ]
  ];
TrieRemoveRecFreq[trie_, word_List, nVal_] :=
  Block[{},
   Which[
    trie === {}, {},
    word === {}, {},
    trie[[1, 1]] == word[[1]],
    Prepend[
     DeleteCases[
      Map[TrieRemoveRecFreq[#, Rest[word], nVal] &,
       Rest[trie]], {}], {trie[[1, 1]], trie[[1, 2]] - nVal}],
    True, trie
   ]
  ];
TrieRemoveRecProb[trie_, word_List, nVal_] :=
  Block[{res, ts},
   Which[
    trie === {}, {},
    word === {}, {},
    trie[[1, 1]] == word[[1]],
    res =
     Prepend[
      DeleteCases[
       Map[TrieRemoveRecProb[#, Rest[word], nVal] &,
        Rest[trie]], {}],
      If[Length[word] > 1,
       trie[[1]], {trie[[1, 1]], trie[[1, 2]] - nVal}]
      ];
    If[Length[res] > 0,
      ts = Total[Rest[res][[All, 1, 2]]];
      If[ts == 0, {},
        Prepend[
          Map[Prepend[Rest[#], {#[[1, 1]], #[[1, 2]]/ts}] &, Rest[res]],
          {res[[1, 1]], res[[1, 2]]*ts}
        ]
      ],
      {}
    ],
    True, trie
   ]
  ];

Clear[TrieCompleteMatch]
TrieCompleteMatch[trie_, {}] := False;
TrieCompleteMatch[trie_, pos : {_Integer ..}] :=
  Block[{},
    If[ TrieRootQ[ trie[[1]] ],
      If[Length[trie[[Sequence @@ pos]]] == 1 ||
          trie[[Sequence @@ pos, 1, 2]] > Total[trie[[Sequence @@ pos, 2 ;; -1, 1, 2]]],
        True,
        False
      ],
      (* ELSE *)
      TrieCompleteMatch[{ { {}, trie[[1,2]] }, trie }, Prepend[ pos, 2 ] ]
    ]
  ];

Clear[TrieMemberQ]
TrieMemberQ[trie_, {}] := False;
TrieMemberQ[trie_, word_String ] := TrieMemberQ[ trie, Characters[word] ];
TrieMemberQ[trie_, sword_List] :=
    Block[{pos},
      pos = TriePosition[ trie, sword ];
      Which[
        TrieRootQ[ trie[[1]] ] && Length[pos] == Length[sword],
        TrieCompleteMatch[trie, pos],
        Length[pos] + 1 == Length[sword],
        TrieCompleteMatch[trie, pos],
        True,
        False
      ]
    ];

Clear[TriePrune, TriePruneRec]
TriePrune[trie_, maxLevel_Integer] :=
    Block[{},
      TriePruneRec[trie, maxLevel, 0]
    ];
TriePruneRec[tree_, maxLevel_Integer, level_Integer] :=

    Block[{nodeRules},
      Which[
        tree === {}, {},
        Length[tree] == 1, tree,
        maxLevel <= level, Take[tree, 1],
        True,
        Prepend[TriePruneRec[#, maxLevel, level + 1] & /@ Rest[tree],
          tree[[1]]]
      ]
    ];

Clear[ToTrieFromJSON, ToTrieFromJSONRec]

ToTrieFromJSON[jsonTrie : {_Rule ...}] :=
    ToTrieFromJSON[jsonTrie, {"key", "value", "children"}];

ToTrieFromJSON[jsonTrie : {_Rule ...}, elementNames : {key_String, value_String, children_String}] :=
    Block[{},
      ToTrieFromJSONRec[jsonTrie, {key, value, children}, 0]
    ];

ToTrieFromJSONRec[jsonTrie : {_Rule ...}, elementNames : {key_String, value_String, children_String}, n_Integer] :=
    Block[{tr},
      If[Length[jsonTrie] == 0, {},
      (*ELSE*)
        tr = {{key, value} /. jsonTrie};
        If[Length[children /. jsonTrie] > 0,
          tr = Join[tr, Map[ToTrieFromJSONRec[#, {key, value, children}, n + 1] &, children /. jsonTrie]]
        ];
        tr
      ]
    ];


End[]

EndPackage[]