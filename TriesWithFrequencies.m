(*
    Tries with frequencies Mathematica package
    Copyright (C) 2013 - 2018  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)
(* :Title: TriesWithFrequencies *)
(* :Context: TriesWithFrequencies` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-04-16 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:


  Duplicates the functionalities of TriesWithFrequencies.m:

    https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequenciesV9.m

  using Associations.


  # Design

  An Association Trie with Frequencies (ATF) has the form:

    <| key1 -> <| $TrieValue->_?NumberQ, ___ |> |>

  Here is a concrete example:

    <|a1 -> <|$TrieValue -> 3, b1 -> <|$TrieValue -> 2, c1 -> <|$TrieValue -> 1|>, c2 -> <|$TrieValue -> 1|>|>, b2 -> <|$TrieValue -> 1|>|>|>

  In most cases the top-most key is $TrieRoot. For example, examine the result of

    TrieCreate[{{a1, b1, c1}, {a1, b1, c2}, {a1, b2}}]

    (* <|$TrieRoot -> <|$TrieValue -> 3, a1 -> <|b1 -> <|c1 -> <|$TrieValue -> 1|>,
         c2 -> <|$TrieValue -> 1|>, $TrieValue -> 2|>, b2 -> <|$TrieValue -> 1|>, $TrieValue -> 3|>|>|> *)


*)

BeginPackage["TriesWithFrequencies`"];

$TrieRoot::usage = "Symbol marking the root of a trie.";

$TrieValue::usage = "Symbol used as a key for a trie node value.";

TrieQ::usage = "A predicate is an expression a trie.";

TrieBodyQ::usage = "A predicate is an expression a trie body.";

TrieRuleQ::usage = "A predicate is an expression a trie rule.";

TrieRetrieve::usage = "TrieRetrieve[t_, w_List] gives the node corresponding to the last \"character\" of the \"word\" w in the trie t.";

TrieSubTrie::usage = "TrieSubTrie[t_, w_List] gives the sub-trie corresponding to the last \"character\" of the \"word\" w in the trie t.";

TriePosition::usage = "TriePosition[ tr_, ks_List ] finds a sub-list of the list of keys \
ks that corresponds to a sub-trie in the trie tr.";

TrieCreate::usage = "TrieCreate[words:{_List..}] creates a trie from a list of lists.";

TrieCreateBySplit::usage = "TrieCreateBySplit[ ws:{_String..}, patt:\"\"] creates a trie object \
from a list of strings that are split with a given pattern patt.";

TrieInsert::usage = "TrieInsert[t_, w_List] insert a \"word\" to the trie t. \
TrieInsert[t_, w_List, val_] inserts a key and a corresponding value.";

TrieMerge::usage = "TrieMerge[t1_, t2_] merges two tries.";

TrieShrink::usage = "TrieShrink[tr_?TrieQ] shrinks the leaves and internal nodes of the trie tr into prefixes. \
TrieShrink[tr_?TrieQ, sep_String] does the shrinking of string nodes with the string separator sep.";

TrieToRules::usage = "Converts a trie into a list of rules suitable for visualization with GraphPlot and LayeredGraphPlot. \
To each trie node is added a list of its level and its traversal order.";

TrieForm::usage = "Graph plot for a trie.";

TrieValueTotal::usage = "TrieValueTotal[trb_?TrieBodyQ] gives the total sum of the values in a trie body.";

TrieNodeProbabilities::usage = "Converts the frequencies at the nodes of a trie into probabilities. \
The value of the option \"ProbabilityModifier\" is a function that is applied to the computed probabilities.";

TrieNodeFrequencies::usage = "Converts the probabilities at the nodes of a trie into frequencies. \
The value of the option \"FrequencyModifier\" is a function that is applied to the computed frequencies.";

TrieLeafProbabilities::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie.";

TrieLeafProbabilitiesWithPositions::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie. \
For each leaf its position in the trie is given.";

TriePathFromPosition::usage = "TriePathFromPosition[trie,pos] gives a list of nodes from the root of a trie to the node at a specified position.";

TrieRootToLeafPaths::usage = "TrieRootToLeafPaths[trie] gives all paths from the root node to the leaf nodes.";

TrieRootToLeafPathRules::usage = "TrieRootToLeafPathRules[trie] gives rules for all paths from the root node to the leaf node values.";

TrieRootToLeafPathProbabilityRules::usage = "TrieRootToLeafPathProbabilityRules[trie] gives path probability rules \
for all paths from the root node to the leaf nodes.";

TrieGetWords::usage = "TrieGetWords[ tr_, sw_List ] gives a list words in tr that start with sw.";

TrieRemove::usage = "TrieRemove removes a \"word\" from a trie.";

TrieThresholdRemove::usage = "TrieThresholdRemove[tr_, th_, opts] removes nodes that have values below (or above) \
a specified threshold. \
If the value postfixVal of the option \"Postfix\" is different than NULL or None then \
the dropped nodes are replaced with postfixVal -> removedTotal,
where removedTotal is the total of the values of the dropped nodes.";

TrieThresholdRemove::usage = "Synonym of TrieThresholdRemove.";

TrieHasCompleteMatchQ::usage = "TrieHasCompleteMatchQ[ tr_, sw_List ] finds does a fraction \
of the list sw is a complete match in the trie tr.";

TrieContains::usage = "TrieContains[ tr_, sw_List ] finds is the list sw a complete match in the trie tr.";

TrieMemberQ::usage = "Same as TrieContains.";

TrieKeyExistsQ::usage = "TrieKeyExistsQ[tr_, sw_List] finds is the list sw a key in the trie tr.";

TrieKeyQ::usage = "Synonym of TrieKeyExistsQ.";

TriePrune::usage = "TriePrune[t, maxLvl] prunes the trie to a maximum node level. (The root is level 0.)";

TrieMap::usage = "TrieMap[t, preFunc, postFunc] traverses the trie t and applies preFunc and postFunc at each node.";

TrieNodeCounts::usage = "TrieNodeCounts[t] gives and association with the total number of nodes, internal nodes only, and leaves only.";

TrieDepth::usage = "TrieDepth[tr] gives the maximum level of the trie tr.";

TrieToJSON::usage = "TrieToJSON[tr] converts a trie to a corresponding JSON expression.";

TrieToListTrie::usage = "TrieToListTrie[tr] converts an Association based trie to a List based trie. (The \"old\" approach.)";

ToTrieFromJSON::usage = "ToTrieFromJSON[jsonTrie:{_Rule...}] converts a JSON import into a Trie object. \
ToTrieFromJSON[jsonTrie:{_Rule...}, elementNames:{key_String, value_String, children_String}] is going to use \
the specified element names for the conversion.";

TrieComparisonGrid::usage = "Makes a grid trie plots for a specified list of trie expressions.";

TrieClassify::usage = "TrieClassify[tr_,record_] classifies a record using a trie. \
The signature TrieClassify[tr_,record_,prop_] can take properties as the ones given to ClassifierFunction. \
TrieClassify[tr_,record_] is the same as TrieClassify[tr_,record_,\"Decision\"].";


Begin["`Private`"];

(************************************************************)
(* Trie core functions                                      *)
(************************************************************)

Clear[TrieBodyQ];
TrieBodyQ[a_Association] := KeyExistsQ[a, $TrieValue];
TrieBodyQ[___] := False;

Clear[TrieQ];
TrieQ[a_Association] := MatchQ[a, Association[x_ -> b_?TrieBodyQ]];
TrieQ[___] := False;

Clear[TrieRuleQ];
TrieRuleQ[a_Rule] := MatchQ[a, Rule[x_, b_?TrieBodyQ]];
TrieRuleQ[___] := False;

(* This is private context only. *)
Clear[TrieWithTrieRootQ];
TrieWithTrieRootQ[a_Association] := MatchQ[a, Association[$TrieRoot -> b_?TrieBodyQ]];
TrieWithTrieRootQ[___] := False;

Clear[TrieNodeCounts];
TrieNodeCounts[tr_] :=
    Block[{cs},
      cs = {Count[tr, <|___, $TrieValue -> _, ___|>, Infinity], Count[tr, <|$TrieValue -> _|>, Infinity]};
      <|"total" -> cs[[1]], "internal" -> cs[[1]] - cs[[2]], "leaves" -> cs[[2]]|>
    ];

Clear[TrieDepth];
TrieDepth[tr_?TrieQ] := Depth[tr] - 2;

Clear[TrieMerge];
TrieMerge[<||>, <||>] := <||>;
TrieMerge[t1_?TrieQ, t2_?TrieQ] :=
    Block[{ckey},
      Which[
        Keys[t1] == Keys[t2],
        ckey = First@Keys[t1];
        <|ckey ->
            Join[Merge[{t1[ckey], t2[ckey]},
              TrieMerge], <|$TrieValue -> (t1[ckey][$TrieValue] + t2[ckey][$TrieValue])|>]|>,

        True,
        Join[t1, t2]
      ]
    ];
TrieMerge[{<||>, t2_Association}] := t2;
TrieMerge[{t1_Association, <||>}] := t1;
TrieMerge[{t1_Association}] := t1;
TrieMerge[{t1_Association, t2_Association}] :=
    Block[{},
      Join[Merge[{KeyDrop[t1, $TrieValue], KeyDrop[t2, $TrieValue]},
        TrieMerge], <|$TrieValue -> (t1[$TrieValue] + t2[$TrieValue])|>]
    ];


Clear[TrieBlank];
TrieBlank[] := <|$TrieRoot -> <|$TrieValue -> 0|>|>;

Clear[TrieMake, TrieInsert];

TrieMake[chars_List] := TrieMake[chars, 1];
TrieMake[chars_List, v_Integer] := TrieMake[chars, v, v];
TrieMake[chars_List, v_Integer, v0_Integer] :=
    Fold[<|#2 -> <|$TrieValue -> v, #1|>|> &, <|Last[chars] -> <|$TrieValue -> v0|>|>,
      Reverse@Most@chars];

TrieInsert[tr_?TrieQ, word_List] :=
    TrieMerge[tr, <|$TrieRoot -> Join[<|$TrieValue -> 1|>, TrieMake[word, 1]]|>];

TrieInsert[tr_, word_List, value_] :=
    Block[{},
      TrieMerge[
        tr, <|$TrieRoot -> Join[<|$TrieValue -> 1|>, TrieMake[word, 0, value]]|>]
    ];


(************************************************************)
(* Trie creation functions                                  *)
(************************************************************)

Clear[TrieCreate1];
TrieCreate1[{}] := <|$TrieRoot -> <|$TrieValue -> 0|>|>;
TrieCreate1[words : {_List ..}] :=
    Fold[TrieInsert, <|
      $TrieRoot -> Join[<|$TrieValue -> 1|>, TrieMake[First[words], 1]]|>, Rest@words];


Clear[TrieCreate];
TrieCreate[{}] := <|$TrieRoot -> <|$TrieValue -> 0|>|>;
TrieCreate[words : {_List ..}] :=
    Block[{},
      If[Length[words] <= 5, TrieCreate1[words], (*ELSE*)
        TrieMerge[TrieCreate[Take[words, Floor[Length[words] / 2]]],
          TrieCreate[Take[words, {Floor[Length[words] / 2] + 1, Length[words]}]]]
      ]
    ];

Clear[TrieCreateBySplit];
TrieCreateBySplit[words : {_String ..}, patt_ : ""] :=
    TrieCreate[ Map[StringSplit[#, ""]&, words]]


(************************************************************)
(* Trie retrieval functions                                 *)
(************************************************************)

Clear[TrieSubTrie, TrieSubTriePathRec];

TrieSubTrie::wargs = "The first argument is expected to be a trie; the second argument is expected to be a list.";

TrieSubTrie[tr_?TrieQ, wordArg_List ] :=
    Block[{path, word = wordArg},
      If[TrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot, ___}], word = Prepend[word, $TrieRoot] ];
      path = TrieSubTriePathRec[tr, word ];
      If[Length[path] == 0, {},
        <|Last[path] -> tr[ Sequence @@ path ]|>
      ]
    ];

TrieSubTrie[___] :=
    Block[{},
      Message[TrieSubTrie::wargs];
      $Failed
    ];

TrieSubTriePathRec[tr_, {}] := {};
TrieSubTriePathRec[tr_, word_List] :=
    If[KeyExistsQ[tr, First[word]],
      Join[{First[word]}, TrieSubTriePathRec[tr[First[word]], Rest[word]]],
      {}
    ] /; TrieBodyQ[tr] || TrieQ[tr];


Clear[TriePosition];
TriePosition[tr_?TrieQ, word_List] :=
    If[TrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot, ___}],
      TrieSubTriePathRec[tr, Prepend[word, $TrieRoot] ],
      TrieSubTriePathRec[tr, word ]
    ];

Clear[TrieRetrieve];
TrieRetrieve[tr_?TrieQ, wordArg_List] :=
    Block[{p, word = wordArg},
      If[TrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot, ___}], word = Prepend[word, $TrieRoot] ];
      p = tr[ Sequence @@ word ];
      If[ FreeQ[p, _Missing], p,
        (*ELSE*)
        p = TriePosition[tr, wordArg];
        Which[
          Length[p] == 0, {},
          True, tr[ Sequence @@ p ]
        ]
      ]
    ];


(************************************************************)
(* Trie key query functions                                 *)
(************************************************************)

Clear[TrieHasCompleteMatchQ];
TrieHasCompleteMatchQ[tr_?TrieQ, word_List ] :=
    Block[{pos, b},
      pos = TriePosition[tr, word];

      If[ Length[pos] == 0, Return[False] ];

      b = False;
      While[ Length[pos] > 0 && !b,
        b = TrieValueTotal[ tr[ Sequence @@ pos ] ] < tr[ Sequence @@ pos, $TrieValue ];
        pos = Most[pos];
      ];

      b
    ];

Clear[TrieContains];
TrieContains[tr_?TrieQ, wordArg_List ] :=
    Block[{pos, word = wordArg},

      If[ TrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot, ___}], word = Prepend[word, $TrieRoot] ];

      pos = TriePosition[tr, word];

      If[ Length[pos] == Length[word],
        TrieValueTotal[ tr[ Sequence @@ pos ] ] < tr[ Sequence @@ pos, $TrieValue ],
        (* ELSE *)
        False
      ]
    ];

TrieMemberQ = TrieContains;

Clear[TrieKeyExistsQ];
TrieKeyExistsQ[ tr_?TrieQ, wordArg_List ] :=
    Block[{pos, word = wordArg},
      If[ TrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot, ___}], word = Prepend[word, $TrieRoot] ];
      pos = TriePosition[tr, word];
      Length[pos] == Length[word]
    ];

Clear[TrieKeyQ];
TrieKeyQ = TrieKeyExistsQ;


(************************************************************)
(* TrieNodeProbabilities and related functions              *)
(************************************************************)

Clear[TrieNodeProbabilities, TrieNodeProbabilitiesRec];

Options[TrieNodeProbabilities] = {"ProbabilityModifier" -> N};
Options[TrieNodeProbabilitiesRec] = Options[TrieNodeProbabilities];

TrieNodeProbabilities[tr_?TrieQ, opts : OptionsPattern[]] :=
    Block[{},
      <|First[Keys[tr]] -> Join[TrieNodeProbabilitiesRec[First@Values[tr], opts], <|$TrieValue -> 1|>]|>
    ];

TrieNodeProbabilitiesRec[trb_?TrieBodyQ, opts : OptionsPattern[]] :=
    Block[{sum, res, pm = OptionValue["ProbabilityModifier"]},
      Which[
        Length[Keys[trb]] == 1, trb,

        True,
        If[trb[$TrieValue] == 0,
          sum = TrieValueTotal[trb],
          sum = trb[$TrieValue]
        ];
        res = Map[TrieNodeProbabilitiesRec[#] &, KeyDrop[trb, $TrieValue]];
        res = Replace[res, <|a___, $TrieValue -> x_, b___|> :> <|a, $TrieValue -> pm[x / sum], b|>, {1}];
        Join[res, KeyTake[trb, $TrieValue]]
      ]
    ];

Clear[TrieValueTotal];
TrieValueTotal[trb_?TrieBodyQ] := Total[Map[#[$TrieValue] &, KeyDrop[trb, $TrieValue]]];

Clear[TrieLeafProbabilities, TrieLeafProbabilitiesRec];

TrieLeafProbabilities::ntnode = "Non trie node was encountered: `1`. A trie node has two elements prefix and frequency.";

TrieLeafProbabilities[trieArg_?TrieQ] :=
    Block[{res},

      res =
          Which[
            TrueQ[trieArg[First@Keys@trieArg][$TrieValue] == 0],
            TrieLeafProbabilitiesRec[trieArg],

            True,
            TrieLeafProbabilitiesRec[First@Keys@trieArg, First@Values@trieArg]
          ];

      If[Length[res] == 1, res, Merge[res, Total]]
    ];

TrieLeafProbabilities[args__] :=
    Block[{},
      Message[TrieLeafProbabilities::ntnode, {args}];
      Return[<||>]
    ];

TrieLeafProbabilitiesRec[k_, trb_?TrieBodyQ] :=
    Block[{res, sum},

      Which[

        Length[Keys[trb]] == 1,
        {k -> trb[$TrieValue]},

        True,
        sum = TrieValueTotal[trb];
        res = KeyValueMap[TrieLeafProbabilitiesRec, KeyDrop[trb, $TrieValue]];
        If[sum < 1,
          res = Append[res, k -> (1 - sum)]
        ];
        res = Map[#[[1]] -> #[[2]] * trb[$TrieValue] &, Flatten[res, 1]]

      ]
    ];


(************************************************************)
(* TrieShrink and related functions                         *)
(************************************************************)

Clear[NodeJoin];
NodeJoin[n_String] := n;
NodeJoin[n1_String, n2_String] := n1 <> n2;
NodeJoin[n1_List, n2_String] := List[n1, n2];
NodeJoin[n_] := List[n];
NodeJoin[n1_List, n2_List] := Join[n1, n2];
NodeJoin[n1_, n2_List] := Prepend[n2, n1];
NodeJoin[n1_List, n2_] := Append[n1, n2];
NodeJoin[n1_, n2_] := List[n1, n2];

Clear[TrieShrink, TrieShrinkRec];

TrieShrink::wargs = "The first argument is expected to be a trie; the second, optional argument is expected to a string.";

TrieShrink[tr_?TrieQ] := TrieShrinkRec[tr];

TrieShrink[tr_?TrieQ, sep_String] :=
    Block[{res},
      NodeJoin[n1_String, n2_String] := n1 <> sep <> n2;
      res = TrieShrinkRec[tr];
      NodeJoin[n1_String, n2_String] := n1 <> n2;
      res
    ];

TrieShrink[___] := Message[TrieShrink::wargs];

TrieShrinkRec[{}] := {};

TrieShrinkRec[tr_?TrieQ] := TrieShrinkRec[First[Normal[tr]]];

TrieShrinkRec[tr_?TrieRuleQ] :=
    Block[{vals = tr[[2]], key = tr[[1]], valKeys },
      valKeys = Keys @ KeyDrop[vals, $TrieValue];

      Which[
        Length[ vals ] == 1,
        tr,

        key =!= $TrieRoot && Length[vals] == 2 && vals[$TrieValue] == vals[First @ valKeys][$TrieValue],
        TrieShrinkRec[ <| NodeJoin[ key, First[valKeys] ] -> vals[First @ valKeys] |> ],

        True,
        <| key -> Join[ <| $TrieValue -> vals[$TrieValue] |>, Association @ Map[ TrieShrinkRec, Normal @ KeyDrop[vals, $TrieValue] ] ] |>
      ]
    ];


(************************************************************)
(* Trie root to leaf functions                              *)
(************************************************************)

(* I am not particularly happy with using FixedPoint. This has to be profiled. *)
Clear[TrieRootToLeafPaths];
TrieRootToLeafPaths[tr_?TrieQ] :=
    Map[List @@@ Most[#[[1]]] &,
      FixedPoint[
        Flatten[Normal[#] /.
            Rule[n_, m_?TrieBodyQ] :>

                Which[
                  Length[m] == 1,
                  KeyMap[Append[n, #] &, m],

                  m[$TrieValue] > TrieValueTotal[m] || TrieValueTotal[m] < 1,
                  Join[ KeyMap[Append[n, # -> m[#][$TrieValue]] &, KeyDrop[m, $TrieValue]], KeyMap[Append[n, #] &, KeyTake[m, $TrieValue]] ],

                  True,
                  KeyMap[Append[n, # -> m[#][$TrieValue]] &, KeyDrop[m, $TrieValue]]], 1] &,
        KeyMap[{# -> First[Values[tr]][$TrieValue]} &, tr]]
    ];

(* This is implemented because it looks neat, and it can be used for tensor creation. *)
Clear[TrieRootToLeafPathRules];
TrieRootToLeafPathRules[tr_?TrieQ] :=
    Association[
      Map[ Most[#[[1]]] -> #[[2]] &,
        FixedPoint[
          Flatten[Normal[#] /.
              Rule[n_, m_?TrieBodyQ] :>
                  If[Length[m] == 1 || m[$TrieValue] > TrieValueTotal[m] || TrieValueTotal[m] < 1,
                    KeyMap[Append[n, #] &, m],
                    KeyMap[Append[n, #] &, KeyDrop[m, $TrieValue]]], 1] &,
          KeyMap[{#} &, tr]
        ]
      ]
    ];

Clear[TrieRootToLeafPathProbabilityRules];
TrieRootToLeafPathProbabilityRules[tr_?TrieQ] :=
    ReverseSort @ Association @ Map[ #[[All, 1]] -> Apply[Times, #[[All, 2]]] &, TrieRootToLeafPaths[tr] ];

Clear[TrieGetWords];
TrieGetWords[ tr_?TrieQ, word_List ] :=
    Which[
      Length[word] == 0,
      {},

      TrieKeyExistsQ[tr, word],
      Map[ Join[Most[word], #]&, Keys @ TrieRootToLeafPathRules[TrieSubTrie[tr, word]] ],

      True,
      {}
    ];


(************************************************************)
(* TriePrune                                                *)
(************************************************************)

Clear[TriePrune, TriePruneRec];
TriePrune[trie_?TrieQ, maxLevel_Integer] :=
    Block[{},
      Association[TriePruneRec[First @ Normal @ trie, maxLevel, 0]]
    ];

TriePruneRec[tr_?TrieRuleQ, maxLevel_Integer, level_Integer] :=
    Block[{key = tr[[1]]},
      Which[
        Length[tr] == 0, {},
        Length[tr[[2]]] == 1, tr,
        maxLevel <= level, key -> KeyTake[tr[[2]], $TrieValue],
        True,
        key ->
            Join[
              Association @ KeyValueMap[ TriePruneRec[#1 -> #2, maxLevel, level + 1] &, KeyDrop[tr[[2]], $TrieValue] ],
              KeyTake[tr[[2]], $TrieValue]
            ]
      ]
    ];


(************************************************************)
(* Trie Map / traversal                                     *)
(************************************************************)

Clear[TrieMap, TrieMapRec];

TrieMap::funcs = "At least one of the second and third argument has to be a function.";

TrieMap[ tr_?TrieQ, preFunc_, postFunc_ ] :=
    Block[{},

      If[ ( preFunc === Null || preFunc === None ) && ( postFunc === Null || postFunc === None ),
        Message[TrieMap::funcs];
        Return[tr]
      ];

      Association[ TrieMapRec[ First @ Normal @ tr, preFunc, postFunc, 0 ] ]
    ];

TrieMapRec[tr_?TrieRuleQ, preFunc_, postFunc_, level_Integer ] :=
    Block[{res, resChildren},

      If[ Length[tr] == 0, Return[{}] ];

      If[ ! ( TrueQ[preFunc === Null] || TrueQ[preFunc === None] ),
        res = preFunc[tr],
        (* ELSE *)
        res = tr
      ];
      (*      Print @ Normal @ KeyDrop[ res[[2]], $TrieValue ] ;*)
      If[ Length[res[[2]]] == 1,
        resChildren = <||>,
        (* ELSE *)
        resChildren = Association @ Map[ TrieMapRec[#, preFunc, postFunc, level + 1]&, Normal @ KeyDrop[ res[[2]], $TrieValue ] ]
      ];

      res = res[[1]] -> Join[ KeyTake[res[[2]], $TrieValue], resChildren ];

      If[ !(TrueQ[postFunc === Null] || TrueQ[postFunc === None]),
        res = postFunc[res]
      ];

      res
    ];


(************************************************************)
(* Trie threshold removal                                   *)
(************************************************************)

Clear[TrieThresholdRemove];

SyntaxInformation[TrieThresholdRemove] = { "ArgumentsPattern" -> {_, _, OptionsPattern[]} };

Options[TrieThresholdRemove] = {"Postfix" -> Anonymous, "BelowThreshold" -> True};

TrieThresholdRemove[ tr_?TrieQ, threshold_?NumberQ, opts:OptionsPattern[] ]:=
    Block[{postfix, belowThresholdQ},

      postfix = OptionValue[TrieThresholdRemove, "Postfix"];
      belowThresholdQ = TrueQ[OptionValue[TrieThresholdRemove, "BelowThreshold"]];

      TrieMap[ tr, ThresholdRemove[#, threshold, postfix, belowThresholdQ] &, None]
    ];


Clear[ThresholdRemove];

ThresholdRemove[tr_?TrieRuleQ, threshold_?NumberQ, postfix_, belowThresholdQ : (True | False) ] :=
    Block[{resChildren, removeSum},

      If[Length[KeyDrop[tr, $TrieValue]] == 0, Return[tr]];

      If[ belowThresholdQ,
        resChildren = Select[KeyDrop[tr[[2]], $TrieValue], #[$TrieValue] >= threshold &],
        (* ELSE *)
        resChildren = Select[KeyDrop[tr[[2]], $TrieValue], #[$TrieValue] <= threshold &]
      ];

      If[! (TrueQ[postfix === None] || TrueQ[postfix === Null]) && Length[resChildren] < Length[tr[[2]]] - 1,

        If[ belowThresholdQ,
          removeSum = Total[Map[#[$TrieValue] &, Select[KeyDrop[tr[[2]], $TrieValue], #[$TrieValue] < threshold &]]],
          (* ELSE *)
          removeSum = Total[Map[#[$TrieValue] &, Select[KeyDrop[tr[[2]], $TrieValue], #[$TrieValue] > threshold &]]]
        ];

        resChildren = Append[resChildren, postfix -> <|$TrieValue -> removeSum|>]
      ];

      tr[[1]] -> Join[KeyTake[tr[[2]], $TrieValue], resChildren]
    ];



(************************************************************)
(* Trie Pareto fraction removal                             *)
(************************************************************)

Clear[TrieParetoFractionRemove];

Options[TrieParetoFractionRemove] = { "RemoveBottomElements" -> True, "Postfix" -> Anonymous };

TrieParetoFractionRemove[ trie_?TrieQ, paretoFraction_?NumberQ : 0.8, opts : OptionsPattern[] ] :=
    Block[{ removeBottomElementQ },

      removeBottomElementQ = TrueQ[OptionValue[TrieParetoFractionRemove, "RemoveBottomElements"]];

    ];

(*TrieParetoFractionRemoveRec[ trie]*)

(************************************************************)
(* Trie conversion related functions                        *)
(************************************************************)

Clear[TrieToRules];
TrieToRules[tree_?TrieQ] := Block[{ORDER = 0}, TrieToRules[tree, 0, 0]];
TrieToRules[tree_, level_, order_] :=
    Block[{nodeRules, k, v},
      Which[
        tree === <||>, {},
        Keys[tree] === {$TrieValue}, {},
        True,
        k = First[Keys[tree]]; v = tree[k, $TrieValue];
        nodeRules =
            KeyValueMap[{{k, v}, {level, order}} -> {{#1, #2[$TrieValue]}, {level + 1, ORDER++}} &, KeyDrop[tree[k], $TrieValue]];
        Join[nodeRules,
          Flatten[
            MapThread[
              TrieToRules[<|#1|>, level + 1, #2] &, {Normal@
                KeyDrop[tree[k], $TrieValue], nodeRules[[All, 2, 2, 2]]}], 1]]
      ]
    ] /; TrieQ[tree];


Clear[GrFramed];
GrFramed[text_] :=
    Framed[text, {Background -> RGBColor[1, 1, 0.8],
      FrameStyle -> RGBColor[0.94, 0.85, 0.36], FrameMargins -> Automatic}];

Clear[TrieForm];
Options[TrieForm] = Options[LayeredGraphPlot];
TrieForm[mytrie_?TrieQ, opts : OptionsPattern[]] :=
    LayeredGraphPlot[
      TrieToRules[mytrie],
      opts,
      VertexShapeFunction -> (Text[GrFramed[#2[[1]]], #1] &), PlotTheme -> "Classic" ];

Clear[TrieToJSON];
TrieToJSON[tr_?TrieQ] := TrieToJSON[First@Normal@tr];
TrieToJSON[tr_?TrieRuleQ] :=
    Block[{k = First@tr},
      {"key" -> k, "value" -> tr[[2]][$TrieValue],
        "children" -> Map[TrieToJSON, Normal[KeyDrop[tr[[2]], $TrieValue]]]}
    ];

Clear[TrieToListTrie];
TrieToListTrie[tr_?TrieQ] := TrieToListTrie[First@Normal@tr] /. $TrieRoot -> {};
TrieToListTrie[tr_?TrieRuleQ] :=
    Block[{k = First@tr},
      Join[ {{k, tr[[2]][$TrieValue]}}, Normal @ Map[TrieToListTrie, Normal[KeyDrop[tr[[2]], $TrieValue]]] ]
    ];


(************************************************************)
(* TrieComparisonGrid                                       *)
(************************************************************)

ClearAll[TrieComparisonGrid];
SetAttributes[TrieComparisonGrid, HoldAll]
Options[TrieComparisonGrid] = Union[Options[Graphics], Options[Grid], {"NumberFormPrecision" -> 3}];
TrieComparisonGrid[trs_List, opts : OptionsPattern[]] :=
    Block[{graphOpts, gridOpts, nfp},
      graphOpts = Select[{opts}, MemberQ[Options[Graphics][[All, 1]], #[[1]]] &];
      gridOpts = Select[{opts}, MemberQ[Options[Grid][[All, 1]], #[[1]]] &];
      nfp = OptionValue["NumberFormPrecision"];
      Grid[{
        First @ Map[HoldForm, Inactivate[Hold[trs]], {2}],
        If[ Length[{graphOpts}] == 0,
          Map[TrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v, nfp]} &, trs],
          Map[TrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v, nfp]} /. (gr_Graphics) :> Append[gr, graphOpts] &, trs],
          Map[TrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v, nfp]} &, trs]
        ]
      }, gridOpts, Dividers -> All, FrameStyle -> LightGray]
    ];


(************************************************************)
(* Trie classify                                            *)
(************************************************************)

Clear[TrieClassify];

TrieClassify::notkey = "The second argument is not key in the trie: `1`.";

Options[TrieClassify] := {"Default" -> None};

TrieClassify[tr_?TrieQ, record_, opts : OptionsPattern[]] :=
    TrieClassify[tr, record, "Decision", opts] /; FreeQ[{opts}, "Probability" | "TopProbabilities"];

TrieClassify[tr_?TrieQ, record_, "Decision", opts : OptionsPattern[]] :=
    First@Keys@TrieClassify[tr, record, "Probabilities", opts];

TrieClassify[tr_?TrieQ, record_, "Probability" -> class_, opts : OptionsPattern[]] :=
    Lookup[TrieClassify[tr, record, "Probabilities", opts], class, 0];

TrieClassify[tr_?TrieQ, record_, "TopProbabilities", opts : OptionsPattern[]] :=
    Select[TrieClassify[tr, record, "Probabilities", opts], # > 0 &];

TrieClassify[tr_?TrieQ, record_, "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Take[TrieClassify[tr, record, "Probabilities", opts], UpTo[n]];

TrieClassify[tr_?TrieQ, record_, "Probabilities", opts : OptionsPattern[]] :=
    Block[{res, dval = OptionValue[TrieClassify, "Default"]},

      If[ !TrieKeyExistsQ[ tr, record ],
        Message[TrieClassify::notkey, record];
        Return[$Failed]
      ];

      res = TrieSubTrie[tr, record];
      If[ TrueQ[res === $Failed], Return[$Failed] ];
      If[ Length[res] == 0,
        <|dval -> 0|>,
        (* ELSE *)
        res = ReverseSort[ Association[ Rule @@@ TrieLeafProbabilities[res] ] ];
        res / Total[res]
      ]
    ];

TrieClassify[tr_?TrieQ, records : (_Dataset | {_List..}), "Decision", opts : OptionsPattern[]] :=
    First @* Keys @* TakeLargest[1] /@ TrieClassify[tr, records, "Probabilities", opts];

TrieClassify[tr_?TrieQ, records : (_Dataset | {_List..}), "Probability" -> class_, opts : OptionsPattern[]] :=
    Map[Lookup[#, class, 0]&, TrieClassify[tr, records, "Probabilities", opts] ];

TrieClassify[tr_?TrieQ, records : (_Dataset | {_List..}), "TopProbabilities", opts : OptionsPattern[]] :=
    Map[ Select[#, # > 0 &]&, TrieClassify[tr, records, "Probabilities", opts] ];

TrieClassify[tr_?TrieQ, records : (_Dataset | {_List..}), "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Map[TakeLargest[#, UpTo[n]]&, TrieClassify[tr, records, "Probabilities", opts] ];

TrieClassify[tr_?TrieQ, records : (_Dataset | {_List..}), "Probabilities", opts : OptionsPattern[] ] :=
    Block[{clRes, classLabels, stencil},

      clRes = Map[ TrieClassify[tr, #, "Probabilities", opts] &, Normal@records ];

      classLabels = Union[Flatten[Normal[Keys /@ clRes]]];

      stencil = AssociationThread[classLabels -> 0];

      KeySort[Join[stencil, #]] & /@ clRes
    ];

TrieClassify[___] := $Failed;

End[]; (* `Private` *)

EndPackage[]