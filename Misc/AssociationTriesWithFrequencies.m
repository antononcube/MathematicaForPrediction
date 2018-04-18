(*
    Tries with frequencies Mathematica through Associations package
    Copyright (C) 2018  Anton Antonov

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

(* :Title: AssociationTriesWithFrequencies *)
(* :Context: AssociationTriesWithFrequencies` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-04-16 *)

(* :Package Version: 0.9 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

  Duplicates the functionalities of TriesWithFrequencies.m:

    https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m

  using Associations.


  # Design

  An Association Trie with Frequencies (ATF) has the form:

    <| key1 -> <| $TrieValue->_?NumberQ, ___ |> |>

  Here is a concrete example:

    <|a1 -> <|$TrieValue -> 3, b1 -> <|$TrieValue -> 2, c1 -> <|$TrieValue -> 1|>, c2 -> <|$TrieValue -> 1|>|>, b2 -> <|$TrieValue -> 1|>|>|>

  In most cases the top-most key is $TrieRoot. For example, examine the result of

    ATrieCreate[{{a1, b1, c1}, {a1, b1, c2}, {a1, b2}}]

    (* <|$TrieRoot -> <|$TrieValue -> 3, a1 -> <|b1 -> <|c1 -> <|$TrieValue -> 1|>,
         c2 -> <|$TrieValue -> 1|>, $TrieValue -> 2|>, b2 -> <|$TrieValue -> 1|>, $TrieValue -> 3|>|>|> *)


*)

BeginPackage["AssociationTriesWithFrequencies`"]

$TrieRoot::usage = "Symbol marking the root of a trie."

$TrieValue::usage = "Symbol used as a key for a trie node value."

ATrieQ::usage = "A predicate is an expression a trie."

ATrieBodyQ::usage = "A predicate is an expression a trie body."

ATrieRuleQ::usage = "A predicate is an expression a trie rule."

ATrieRetrieve::usage = "TrieRetrieve[t_, w_List] gives the node corresponding to the last \"character\" of the \"word\" w in the trie t."

ATrieSubTrie::usage = "TrieSubTrie[t_, w_List] gives the sub-trie corresponding to the last \"character\" of the \"word\" w in the trie t."

ATriePosition::usage = "TriePosition[ tr_, ks_List ] finds a sub-list of the list of keys\
 ks that corresponds to a sub-trie in the trie tr."

ATrieCreate::usage = "TrieCreate[words:{_List..}] creates a trie from a list of lists."

ATrieInsert::usage = "TrieInsert[t_, w_List] insert a \"word\" to the trie t. TrieInsert[t_, w_List, val_] inserts a key and a corresponding value."

ATrieMerge::usage = "TrieMerge[t1_, t2_] merges two tries."

ATrieShrink::usage = "TrieShrink shrinks the leaves and internal nodes into prefixes."

ATrieToRules::usage = "Converts a trie into a list of rules suitable for visualization with GraphPlot and LayeredGraphPlot.\
 To each trie node is added a list of its level and its traversal order."

ATrieForm::usage = "Graph plot for a trie."

ATrieNodeProbabilities::usage = "Converts the frequencies at the nodes of a trie into probabilities.\
 The value of the option \"ProbabilityModifier\" is a function that is applied to the computed probabilities."

ATrieNodeFrequencies::usage = "Converts the probabilities at the nodes of a trie into frequencies.\
 The value of the option \"FrequencyModifier\" is a function that is applied to the computed frequencies."

ATrieLeafProbabilities::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie."

ATrieLeafProbabilitiesWithPositions::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie.\
For each leaf its position in the trie is given."

ATriePathFromPosition::usage = "TriePathFromPosition[trie,pos] gives a list of nodes from the root of a trie to the node at a specified position."

ATrieRootToLeafPaths::usage = "TrieRootToLeafPaths[trie] gives all paths from the root node to the leaf nodes."

ATrieRootToLeafPathRules::usage = "ATrieRootToLeafPathRules[trie] gives rules for all paths from the root node to the leaf node values."

ATrieGetWords::usage = "TrieGetWords[ tr_, sw_List ] gives a list words in tr that start with sw."

ATrieRemove::usage = "TrieRemove removes a \"word\" from a trie."

ATrieHasCompleteMatchQ::usage = "TrieHasCompleteMatchQ[ tr_, sw_List ] finds does a fraction\
 of the list sw is a complete match in the trie tr."

ATrieContains::usage = "TrieContains[ tr_, sw_List ] finds is the list sw a complete match in the trie tr."

ATrieMemberQ::usage = "Same as TrieContains."

ATriePrune::usage = "TriePrune[t, maxLvl] prunes the trie to a maximum node level. (The root is level 0.)"

ATrieNodeCounts::usage = "TrieNodeCounts[t] gives and association with the total number of nodes, internal nodes only, and leaves only."

ATrieDepth::usage = "TrieDepth[tr] gives the maximum level of the trie tr."

ATrieToJSON::usage = "TrieToJSON[tr] converts a trie to a corresponding JSON expression."

ToATrieFromJSON::usage = "ToTrieFromJSON[jsonTrie:{_Rule...}] converts a JSON import into a Trie object. \
ToTrieFromJSON[jsonTrie:{_Rule...}, elementNames:{key_String, value_String, children_String}] is going to use \
the specified element names for the conversion."

ATrieComparisonGrid::usage = "Makes a grid trie plots for a specified list of trie expressions."

ATrieClassify::usage = "TrieClassify[tr_,record_] classifies a record using a trie. \
The signature TrieClassify[tr_,record_,prop_] can take properties as the ones given to ClassifierFunction. \
TrieClassify[tr_,record_] is the same as TrieClassify[tr_,record_,\"Decision\"]."


Begin["`Private`"]

Clear[ATrieBodyQ]
ATrieBodyQ[a_Association] := KeyExistsQ[a, $TrieValue];
ATrieBodyQ[___] := False;

Clear[ATrieQ]
ATrieQ[a_Association] := MatchQ[a, Association[x_ -> b_?ATrieBodyQ]];
ATrieQ[___] := False;

Clear[ATrieRuleQ]
ATrieRuleQ[a_Rule] := MatchQ[a, Rule[x_, b_?ATrieBodyQ]];
ATrieRuleQ[___] := False;

(* This is private context only. *)
Clear[ATrieWithTrieRootQ]
ATrieWithTrieRootQ[a_Association] := MatchQ[a, Association[$TrieRoot -> b_?ATrieBodyQ]];
ATrieWithTrieRootQ[___] := False;

Clear[ATrieNodeCounts]
ATrieNodeCounts[tr_] :=
    Block[{cs},
      cs = {Count[tr, <|___, $TrieValue -> _, ___|>, Infinity], Count[tr, <|$TrieValue -> _|>, Infinity]};
      <|"total" -> cs[[1]], "internal" -> cs[[1]] - cs[[2]], "leaves" -> cs[[2]]|>
    ];

Clear[ATrieDepth]
ATrieDepth[tr_?ATrieQ] := Depth[tr] - 2;

Clear[ATrieMerge]
ATrieMerge[<||>, <||>] := <||>;
ATrieMerge[t1_?ATrieQ, t2_?ATrieQ] :=
    Block[{ckey},
      Which[
        Keys[t1] == Keys[t2],
        ckey = First@Keys[t1];
        <|ckey ->
            Join[Merge[{t1[ckey], t2[ckey]},
              ATrieMerge], <|$TrieValue -> (t1[ckey][$TrieValue] + t2[ckey][$TrieValue])|>]|>,

        True,
        Join[t1, t2]
      ]
    ];
ATrieMerge[{<||>, t2_Association}] := t2;
ATrieMerge[{t1_Association, <||>}] := t1;
ATrieMerge[{t1_Association}] := t1;
ATrieMerge[{t1_Association, t2_Association}] :=
    Block[{},
      Join[Merge[{KeyDrop[t1, $TrieValue], KeyDrop[t2, $TrieValue]},
        ATrieMerge], <|$TrieValue -> (t1[$TrieValue] + t2[$TrieValue])|>]
    ];


Clear[ATrieBlank]
ATrieBlank[] := <|$TrieRoot -> <|$TrieValue -> 0|>|>;

Clear[ATrieMake, ATrieInsert]

ATrieMake[chars_List] := ATrieMake[chars, 1];
ATrieMake[chars_List, v_Integer] := ATrieMake[chars, v, v];
ATrieMake[chars_List, v_Integer, v0_Integer] :=
    Fold[<|#2 -> <|$TrieValue -> v, #1|>|> &, <|Last[chars] -> <|$TrieValue -> v0|>|>,
      Reverse@Most@chars];

ATrieInsert[tr_?ATrieQ, word_List] :=
    ATrieMerge[tr, <|$TrieRoot -> Join[<|$TrieValue -> 1|>, ATrieMake[word, 1]]|>];

ATrieInsert[tr_, word_List, value_] :=
    Block[{},
      ATrieMerge[
        tr, <|$TrieRoot -> Join[<|$TrieValue -> 1|>, ATrieMake[word, 0, value]]|>]
    ];


Clear[ATrieCreate1]
ATrieCreate1[{}] := <|$TrieRoot -> <|$TrieValue -> 0|>|>;
ATrieCreate1[words : {_List ..}] :=
    Fold[ATrieInsert, <|
      $TrieRoot -> Join[<|$TrieValue -> 1|>, ATrieMake[First[words], 1]]|>, Rest@words];


Clear[ATrieCreate]
ATrieCreate[{}] := <|$TrieRoot -> <|$TrieValue -> 0|>|>;
ATrieCreate[words : {_List ..}] :=
    Block[{},
      If[Length[words] <= 5, ATrieCreate1[words],(*ELSE*)
        ATrieMerge[ATrieCreate[Take[words, Floor[Length[words]/2]]],
          ATrieCreate[Take[words, {Floor[Length[words]/2] + 1, Length[words]}]]]
      ]
    ];

Clear[ATrieSubTrie, ATrieSubTriePathRec]

ATrieSubTrie[tr_?ATrieQ, wordArg_List ] :=
    Block[{path, word=wordArg},
      If[ATrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot,___}], word = Prepend[word, $TrieRoot] ];
      path = ATrieSubTriePathRec[tr, word ];
      If[Length[path]==0,{},
        <|Last[path] -> tr[ Sequence @@ path ]|>
      ]
    ];

ATrieSubTriePathRec[tr_, {}] := {};
ATrieSubTriePathRec[tr_, word_List] :=
    If[KeyExistsQ[tr, First[word]],
      Join[{First[word]}, ATrieSubTriePathRec[tr[First[word]], Rest[word]]],
      {}
    ] /; ATrieBodyQ[tr] || ATrieQ[tr];


Clear[ATriePosition]
ATriePosition[tr_?ATrieQ, word_List] :=
    If[ATrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot,___}],
      ATrieSubTriePathRec[tr, Prepend[word, $TrieRoot] ],
      ATrieSubTriePathRec[tr, word ]
    ];

Clear[ATrieRetrieve]
ATrieRetrieve[tr_?ATrieQ, wordArg_List] :=
    Block[{p, word=wordArg},
      If[ATrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot,___}], word = Prepend[word, $TrieRoot] ];
      p = tr[ Sequence @@ word ];
      If[ FreeQ[p,_Missing], p,
      (*ELSE*)
        p = TriePosition[tr, wordArg];
        Which[
          Length[p] == 0, {},
          True, tr[ Sequence @@ p ]
        ]
      ]
    ];


Clear[ATrieHasCompleteMatchQ]
ATrieHasCompleteMatchQ[tr_?ATrieQ, word_List ] :=
    Block[{pos, b},
      pos = ATriePosition[tr, word];

      If[ Length[pos] == 0, Return[False] ];

      b = False;
      While[ Length[pos] > 0 && !b,
        b = ATrieValueSum[ tr[ Sequence @@ pos ] ] < tr[ Sequence @@ pos, $TrieValue ];
        pos = Most[pos];
      ];

      b
    ];

Clear[ATrieContains]
ATrieContains[tr_?ATrieQ, wordArg_List ] :=
    Block[{pos, word = wordArg},

      If[ ATrieWithTrieRootQ[tr] && !MatchQ[word, {$TrieRoot,___}], word = Prepend[word, $TrieRoot] ];

      pos = ATriePosition[tr, word];

      If[ Length[pos] == Length[word],
        ATrieValueSum[ tr[ Sequence @@ pos ] ] < tr[ Sequence @@ pos, $TrieValue ],
        (* ELSE *)
        False
      ]
    ];

ATrieMemberQ = ATrieContains;

Clear[ATrieNodeProbabilities, ATrieNodeProbabilitiesRec]

Options[ATrieNodeProbabilities] = {"ProbabilityModifier" -> N};
Options[ATrieNodeProbabilitiesRec] = Options[ATrieNodeProbabilities];

ATrieNodeProbabilities[tr_?ATrieQ, opts : OptionsPattern[]] :=
    Block[{},
      <|First[Keys[tr]] -> Join[ATrieNodeProbabilitiesRec[First@Values[tr], opts], <|$TrieValue -> 1|>]|>
    ];

ATrieNodeProbabilitiesRec[trb_?ATrieBodyQ, opts : OptionsPattern[]] :=
    Block[{sum, res, pm = OptionValue["ProbabilityModifier"]},
      Which[
        Length[Keys[trb]] == 1, trb,

        True,
        If[trb[$TrieValue] == 0,
          sum = ATrieValueSum[trb],
          sum = trb[$TrieValue]
        ];
        res = Map[ATrieNodeProbabilitiesRec[#] &, KeyDrop[trb, $TrieValue]];
        res = Replace[res, <|a___, $TrieValue -> x_, b___|> :> <|a, $TrieValue -> pm[x/sum], b|>, {1}];
        Join[res, KeyTake[trb, $TrieValue]]
      ]
    ];

Clear[ATrieValueSum]
ATrieValueSum[trb_?ATrieBodyQ] :=  Total[Map[#[$TrieValue] &, KeyDrop[trb, $TrieValue]]];

Clear[ATrieLeafProbabilities, ATrieLeafProbabilitiesRec]

ATrieLeafProbabilities::ntnode = "Non trie node was encountered: `1`. A trie node has two elements prefix and frequency.";

ATrieLeafProbabilities[trieArg_?ATrieQ] :=
    Block[{res},
      res =
          Which[
            TrueQ[trieArg[First@Keys@trieArg][$TrieValue] == 0],
            ATrieLeafProbabilitiesRec[trieArg],

            True,
            ATrieLeafProbabilitiesRec[First@Keys@trieArg, First@Values@trieArg]
          ];
      Merge[res, Total]
    ];

ATrieLeafProbabilities[args__] :=
    Block[{},
      Message[ATrieLeafProbabilities::ntnode, {args}];
      Return[<||>]
    ];

ATrieLeafProbabilitiesRec[k_, trb_?ATrieBodyQ] :=
    Block[{res, sum},
      Which[
        Length[Keys[trb]] == 1, k -> trb[$TrieValue],
        True,
        sum = ATrieValueSum[trb];
        res = KeyValueMap[ATrieLeafProbabilitiesRec, KeyDrop[trb, $TrieValue]];
        If[sum < 1,
          res = Append[res, k -> (1 - sum)]
        ];
        res = Map[#[[1]] -> #[[2]]*trb[$TrieValue] &, Flatten[res, 1]]]
    ];


Clear[NodeJoin]
NodeJoin[n_String] := n;
NodeJoin[n1_String, n2_String] := n1 <> n2;
NodeJoin[n1_List, n2_String] := List[n1, n2];
NodeJoin[n_] := List[n];
NodeJoin[n1_List, n2_List] := Join[n1, n2];
NodeJoin[n1_, n2_List] := Prepend[n2, n1];
NodeJoin[n1_List, n2_] := Append[n1, n2];
NodeJoin[n1_, n2_] := List[n1, n2];

Clear[ATrieShrink, ATrieShrinkRec]
ATrieShrink[tr_?ATrieQ] := ATrieShrinkRec[tr];
ATrieShrinkRec[{}] := {};
ATrieShrinkRec[tr_?ATrieQ] := ATrieShrinkRec[First[Normal[tr]]];
ATrieShrinkRec[tr_?ATrieRuleQ] :=
    Block[{vals = tr[[2]], key = tr[[1]], valKeys },
      valKeys = Keys @ KeyDrop[vals, $TrieValue];

      Which[
        Length[ vals ] == 1,
        tr,

        key =!= $TrieRoot && Length[vals] == 2 && vals[$TrieValue] == vals[First @ valKeys][$TrieValue],
        ATrieShrinkRec[ <| NodeJoin[ key, First[valKeys] ] -> vals[First @ valKeys] |> ],

        True,
        <| key -> Join[ <| $TrieValue -> vals[$TrieValue] |>, Association @ Map[ ATrieShrinkRec, Normal @ KeyDrop[vals, $TrieValue] ] ] |>
      ]
    ];

(* I am not particularly happy with using FixedPoint. This has to be profiled. *)
Clear[ATrieRootToLeafPaths]
ATrieRootToLeafPaths[tr_] :=
    Map[List @@@ Most[#[[1]]] &,
      FixedPoint[
        Flatten[Normal[#] /.
            Rule[n_, m_?ATrieBodyQ] :>
                If[Length[m] == 1 || m[$TrieValue] > ATrieValueSum[m],
                  KeyMap[Append[n, #] &, m],
                  KeyMap[Append[n, # -> m[#][$TrieValue]] &, KeyDrop[m, $TrieValue]]], 1] &,
        KeyMap[{# -> First[Values[tr]][$TrieValue]} &, tr]]
    ];

(* This is implemented because it looks neat, and it can be used for tensor creation. *)
Clear[ATrieRootToLeafPathRules]
ATrieRootToLeafPathRules[tr_?ATrieQ] :=
    Map[ Most[#[[1]]]->#[[2]] &,
      FixedPoint[
        Flatten[Normal[#] /.
            Rule[n_, m_?ATrieBodyQ] :>
                If[Length[m] == 1 || m[$TrieValue] > ATrieValueSum[m],
                  KeyMap[Append[n, #] &, m],
                  KeyMap[Append[n, #] &, KeyDrop[m, $TrieValue]]], 1] &,
        KeyMap[{#} &, tr]
      ]
    ];


Clear[ATrieToRules]
ATrieToRules[tree_?ATrieQ] := Block[{ORDER = 0}, ATrieToRules[tree, 0, 0]];
ATrieToRules[tree_, level_, order_] :=
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
              ATrieToRules[<|#1|>, level + 1, #2] &, {Normal@
                KeyDrop[tree[k], $TrieValue], nodeRules[[All, 2, 2, 2]]}], 1]]
      ]
    ] /; ATrieQ[tree];


Clear[GrFramed]
GrFramed[text_] :=
    Framed[text, {Background -> RGBColor[1, 1, 0.8],
      FrameStyle -> RGBColor[0.94, 0.85, 0.36], FrameMargins -> Automatic}];

Clear[ATrieForm]
ATrieForm[mytrie_?ATrieQ, opts : OptionsPattern[]] :=
    LayeredGraphPlot[ATrieToRules[mytrie],
      VertexRenderingFunction -> (Text[GrFramed[#2[[1]]], #1] &), opts];

ClearAll[ATrieComparisonGrid]
SetAttributes[ATrieComparisonGrid, HoldAll]
Options[ATrieComparisonGrid] = Union[Options[Graphics], Options[Grid], {"NumberFormPrecision"->3}];
ATrieComparisonGrid[trs : {_?ATrieQ ..}, opts : OptionsPattern[]] :=
    Block[{graphOpts,gridOpts,nfp},
      graphOpts = Select[{opts}, MemberQ[Options[Graphics][[All, 1]], #[[1]]] &];
      gridOpts = Select[{opts}, MemberQ[Options[Grid][[All, 1]], #[[1]]] &];
      nfp = OptionValue["NumberFormPrecision"];
      Grid[{
        HoldForm /@ Inactivate[trs],
        If[ Length[{graphOpts}] == 0,
          Map[ATrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v,nfp]} &, trs],
          Map[ATrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v,nfp]} /. (gr_Graphics) :> Append[gr, graphOpts] &, trs],
          Map[ATrieForm[#] /. {k_String, v_?NumericQ} :> {k, NumberForm[v,nfp]} &, trs]
        ]
      }, gridOpts, Dividers -> All, FrameStyle -> LightGray]
    ];

Clear[ATrieClassify]

Options[ATrieClassify] := {"Default" -> None};

ATrieClassify[tr_?ATrieQ, record_, opts : OptionsPattern[]] :=
    ATrieClassify[tr, record, "Decision", opts] /; FreeQ[{opts}, "Probability"|"TopProbabilities"];

ATrieClassify[tr_?ATrieQ, record_, "Decision", opts : OptionsPattern[]] :=
    First@Keys@ATrieClassify[tr, record, "Probabilities", opts];

ATrieClassify[tr_?ATrieQ, record_, "Probability" -> class_, opts : OptionsPattern[]] :=
    Lookup[ATrieClassify[tr, record, "Probabilities", opts], class, 0];

ATrieClassify[tr_?ATrieQ, record_, "TopProbabilities", opts : OptionsPattern[]] :=
    Select[ATrieClassify[tr, record, "Probabilities", opts], # > 0 &];

ATrieClassify[tr_?ATrieQ, record_, "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Take[ATrieClassify[tr, record, "Probabilities", opts], UpTo[n]];

ATrieClassify[tr_?ATrieQ, record_, "Probabilities", opts : OptionsPattern[]] :=
    Block[{res, dval = OptionValue[ATrieClassify, "Default"]},
      res = ATrieSubTrie[tr, record];
      If[Length[res] == 0, <|dval -> 0|>,
        res = ReverseSort[Association[Rule @@@ ATrieLeafProbabilities[res]]];
        res / Total[res]
      ]
    ];

ATrieClassify[tr_?ATrieQ, records:(_Dataset|{_List..}), "Decision", opts : OptionsPattern[]] :=
    First @* Keys @* TakeLargest[1] /@ ATrieClassify[tr, records, "Probabilities", opts];

ATrieClassify[tr_?ATrieQ, records:(_Dataset|{_List..}), "Probability" -> class_, opts : OptionsPattern[]] :=
    Map[Lookup[#, class, 0]&, ATrieClassify[tr, records, "Probabilities", opts] ];

ATrieClassify[tr_?ATrieQ, records:(_Dataset|{_List..}), "TopProbabilities", opts : OptionsPattern[]] :=
    Map[ Select[#, # > 0 &]&, ATrieClassify[tr, records, "Probabilities", opts] ];

ATrieClassify[tr_?ATrieQ, records:(_Dataset|{_List..}), "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Map[TakeLargest[#, UpTo[n]]&, ATrieClassify[tr, records, "Probabilities", opts] ];

ATrieClassify[tr_?ATrieQ, records:(_Dataset|{_List..}), "Probabilities", opts:OptionsPattern[] ] :=
    Block[{clRes, classLabels, stencil},

      clRes = Map[ ATrieClassify[tr, #, "Probabilities", opts] &, Normal@records ];

      classLabels = Union[Flatten[Normal[Keys /@ clRes]]];

      stencil = AssociationThread[classLabels -> 0];

      KeySort[Join[stencil, #]] & /@ clRes
    ];

End[] (* `Private` *)

EndPackage[]