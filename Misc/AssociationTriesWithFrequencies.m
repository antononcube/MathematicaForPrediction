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

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

  Duplicates the functionalities of TriesWithFrequencies.m:

    https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m

  using Associations.

*)

BeginPackage["AssociationTriesWithFrequencies`"]

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

ATrieToRules::usage = "Converts a trie into a list of rules suitable for visualization with GraphPlot and LayeredGraphPlot. To each trie node is added a list of its level and its traversal order."

ATrieForm::usage = "Graph plot for a trie."

ATrieNodeProbabilities::usage = "Converts the frequencies at the nodes of a trie into probabilities. The value of the option \"ProbabilityModifier\" is a function that is applied to the computed probabilities."

ATrieNodeFrequencies::usage = "Converts the probabilities at the nodes of a trie into frequencies. The value of the option \"FrequencyModifier\" is a function that is applied to the computed frequencies."

ATrieLeafProbabilities::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie."

ATrieLeafProbabilitiesWithPositions::usage = "Gives the probabilities to end up at each of the leaves by paths from the root of the trie. For each leaf its position in the trie is given."

ATriePathFromPosition::usage = "TriePathFromPosition[trie,pos] gives a list of nodes from the root of a trie to the node at a specified position."

ATrieRootToLeafPaths::usage = "TrieRootToLeafPaths[trie] gives all paths from the root node to the leaf nodes."

ATrieRemove::usage = "TrieRemove removes a \"word\" from a trie."

ATrieCompleteMatch::usage = "TrieCompleteMatch[ t, pos ] checks is the position list pos of \"word\" is a complete match in the trie t."

ATrieMemberQ::usage = "TrieMemberQ[t, w] checks is the \"word\" w in the trie t."

ATriePrune::usage = "TriePrune[t,maxLvl] prunes the trie to a maximum node level. (The root is level 0.)"

ATrieNodeCounts::usage = "TrieNodeCounts[t] gives and association with the total number of nodes, internal nodes only, and leaves only."

ToATrieFromJSON::usage = "ToTrieFromJSON[jsonTrie:{_Rule...}] converts a JSON import into a Trie object. \
ToTrieFromJSON[jsonTrie:{_Rule...}, elementNames:{key_String, value_String, children_String}] is going to use \
the specified element names for the conversion."

ATrieClassify::usage = "TrieClassify[tr_,record_] classifies a record using a trie. \
The signature TrieClassify[tr_,record_,prop_] can take properties as the ones given to ClassifierFunction. \
TrieClassify[tr_,record_] is the same as TrieClassify[tr_,record_,\"Decision\"]."


Begin["`Private`"]

Clear[ATrieBodyQ]
ATrieBodyQ[a_Association] := KeyExistsQ[a, "$Value"];
ATrieBodyQ[___] := False;

Clear[ATrieQ]
ATrieQ[a_Association] := MatchQ[a, Association[x_ -> b_?ATrieBodyQ]];
ATrieQ[___] := False;

Clear[ATrieRuleQ]
ATrieRuleQ[a_Rule] := MatchQ[a, Rule[x_, b_?ATrieBodyQ]];
ATrieRuleQ[___] := False;


Clear[ATrieMerge]
ATrieMerge[<||>, <||>] := <||>;
ATrieMerge[t1_?ATrieQ, t2_?ATrieQ] :=
    Block[{ckey},
      Which[
        Keys[t1] == Keys[t2],
        ckey = First@Keys[t1];
        <|ckey ->
            Join[Merge[{t1[ckey], t2[ckey]},
              ATrieMerge], <|"$Value" -> (t1[ckey]["$Value"] + t2[ckey]["$Value"])|>]|>,

        True,
        Join[t1, t2]
      ]
    ];
ATrieMerge[{<||>, t2_Association}] := t2;
ATrieMerge[{t1_Association, <||>}] := t1;
ATrieMerge[{t1_Association}] := t1;
ATrieMerge[{t1_Association, t2_Association}] :=
    Block[{},
      Join[Merge[{KeyDrop[t1, "$Value"], KeyDrop[t2, "$Value"]},
        ATrieMerge], <|"$Value" -> (t1["$Value"] + t2["$Value"])|>]
    ];


Clear[ATrieBlank]
ATrieBlank[] := <|None -> <|"$Value" -> 0|>|>;

Clear[ATrieMake, ATrieInsert]

ATrieMake[chars_List] := ATrieMake[chars, 1];
ATrieMake[chars_List, v_Integer] := ATrieMake[chars, v, v];
ATrieMake[chars_List, v_Integer, v0_Integer] :=
    Fold[<|#2 -> <|"$Value" -> v, #1|>|> &, <|Last[chars] -> <|"$Value" -> v0|>|>,
      Reverse@Most@chars];

ATrieInsert[tr_?ATrieQ, word_List] :=
    ATrieMerge[tr, <|None -> Join[<|"$Value" -> 1|>, ATrieMake[word, 1]]|>];

ATrieInsert[tr_, word_List, value_] :=
    Block[{},
      ATrieMerge[
        tr, <|None -> Join[<|"$Value" -> 1|>, ATrieMake[word, 0, value]]|>]
    ];


Clear[ATrieCreate1]
ATrieCreate1[{}] := <|None -> <|"$Value" -> 0|>|>;
ATrieCreate1[words : {_List ..}] :=
    Fold[ATrieInsert, <|
      None -> Join[<|"$Value" -> 1|>, ATrieMake[First[words], 1]]|>, Rest@words];

Clear[ATrieCreate]
ATrieCreate[{}] := <|None -> <|"$Value" -> 0|>|>;
ATrieCreate[words : {_List ..}] :=
    Block[{},
      If[Length[words] <= 5, ATrieCreate1[words],(*ELSE*)
        ATrieMerge[ATrieCreate[Take[words, Floor[Length[words]/2]]],
          ATrieCreate[Take[words, {Floor[Length[words]/2] + 1, Length[words]}]]]
      ]
    ];

Clear[ATrieSubTrie, ATrieSubTriePathRec]

ATrieSubTrie[tr_?ATrieQ, word_List] :=
    Block[{path},
      path = ATrieSubTriePathRec[tr, word];
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
ATriePosition[tr_?ATrieQ, word_List] := ATrieSubTriePathRec[tr, word];

Clear[ATrieRetrieve]
ATrieRetrieve[tr_?ATrieQ, chars_List] :=
    Block[{p},
      p = tr[ Sequence @@ chars ];
      If[ FreeQ[p,_Missing], p,
      (*ELSE*)
        p = TriePosition[tr, chars];
        Which[
          Length[p] == 0, {},
          True, tr[ Sequence @@ p ]
        ]
      ]
    ];


Clear[ATrieNodeProbabilities, ATrieNodeProbabilitiesRec]

Options[ATrieNodeProbabilities] = {"ProbabilityModifier" -> N};
Options[ATrieNodeProbabilitiesRec] = Options[ATrieNodeProbabilities];

ATrieNodeProbabilities[tr_?ATrieQ, opts : OptionsPattern[]] :=
    Block[{},
      <|First[Keys[tr]] ->
          Join[ATrieNodeProbabilitiesRec[First@Values[tr],
            opts], <|"$Value" -> 1|>]|>
    ];

ATrieNodeProbabilitiesRec[trb_?ATrieBodyQ, opts : OptionsPattern[]] :=

    Block[{sum, res, pm = OptionValue["ProbabilityModifier"]},
      Which[
        Length[Keys[trb]] == 1, trb,

        True,
        If[trb["$Value"] == 0,
          sum = Map[#["$Value"] &, Values[KeyDrop[trb, "$Value"]]],
          sum = trb["$Value"]
        ];
        res = Map[ATrieNodeProbabilitiesRec[#] &, KeyDrop[trb, "$Value"]];
        res = Replace[
          res, <|a___, "$Value" -> x_, b___|> :> <|a, "$Value" -> pm[x/sum], b|>, {1}];
        Join[res, KeyTake[trb, "$Value"]]
      ]
    ];


Clear[ATrieLeafProbabilities, ATrieLeafProbabilitiesRec]

ATrieLeafProbabilities::ntnode = "Non trie node was encountered: `1`. A trie node has two elements prefix and frequency.";

ATrieLeafProbabilities[trieArg_?ATrieQ] :=
    Block[{res},
      res =
          Which[
            TrueQ[trieArg[First@Keys@trieArg]["$Value"] == 0],
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
        Length[Keys[trb]] == 1, k -> trb["$Value"],
        True,
        sum = Total@Map[#["$Value"] &, Values[KeyDrop[trb, "$Value"]]];
        res = KeyValueMap[ATrieLeafProbabilitiesRec, KeyDrop[trb, "$Value"]];
        If[sum < 1,
          res = Append[res, k -> (1 - sum)]
        ];
        res = Map[#[[1]] -> #[[2]]*trb["$Value"] &, Flatten[res, 1]]]
    ];


Clear[ATrieNodeCounts]
ATrieNodeCounts[tr_] :=
    Block[{cs},
      cs = {Count[tr, <|___, "$Value" -> _, ___|>, Infinity],
        Count[tr, <|"$Value" -> _|>, Infinity]};
      <|"total" -> cs[[1]], "internal" -> cs[[1]] - cs[[2]],
        "leaves" -> cs[[2]]|>
    ];


Clear[ATrieToRules]
ATrieToRules[tree_?ATrieQ] := Block[{ORDER = 0}, ATrieToRules[tree, 0, 0]];
ATrieToRules[tree_, level_, order_] :=
    Block[{nodeRules, k, v},
      Which[
        tree === <||>, {},
        Keys[tree] === {"$Value"}, {},
        True,
        k = First[Keys[tree]]; v = tree[k, "$Value"];
        nodeRules =
            KeyValueMap[{{k, v}, {level, order}} -> {{#1, #2["$Value"]}, {level + 1,
              ORDER++}} &, KeyDrop[tree[k], "$Value"]];
        Join[nodeRules,
          Flatten[
            MapThread[
              ATrieToRules[<|#1|>, level + 1, #2] &, {Normal@
                KeyDrop[tree[k], "$Value"], nodeRules[[All, 2, 2, 2]]}], 1]]
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


Clear[ATrieClassify]

Options[ATrieClassify] := {"Default" -> None};

ATrieClassify[tr_, record_, opts : OptionsPattern[]] :=
    ATrieClassify[tr, record, "Decision", opts] /; FreeQ[{opts}, "Probability"|"TopProbabilities"];

ATrieClassify[tr_, record_, "Decision", opts : OptionsPattern[]] :=
    First@Keys@ATrieClassify[tr, record, "Probabilities", opts];

ATrieClassify[tr_, record_, "Probability" -> class_, opts : OptionsPattern[]] :=
    Lookup[ATrieClassify[tr, record, "Probabilities", opts], class, 0];

ATrieClassify[tr_, record_, "TopProbabilities", opts : OptionsPattern[]] :=
    Select[ATrieClassify[tr, record, "Probabilities", opts], # > 0 &];

ATrieClassify[tr_, record_, "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Take[ATrieClassify[tr, record, "Probabilities", opts], UpTo[n]];

ATrieClassify[tr_, record_, "Probabilities", opts : OptionsPattern[]] :=
    Block[{res, dval = OptionValue[ATrieClassify, "Default"]},
      res = ATrieSubTrie[tr, record];
      If[Length[res] == 0, <|dval -> 0|>,
        res = ReverseSort[Association[Rule @@@ ATrieLeafProbabilities[res]]];
        res / Total[res]
      ]
    ];

ATrieClassify[tr_, records:(_Dataset|{_List..}), "Decision", opts : OptionsPattern[]] :=
    First @* Keys @* TakeLargest[1] /@ ATrieClassify[tr, records, "Probabilities", opts];

ATrieClassify[tr_, records:(_Dataset|{_List..}), "Probability" -> class_, opts : OptionsPattern[]] :=
    Map[Lookup[#, class, 0]&, ATrieClassify[tr, records, "Probabilities", opts] ];

ATrieClassify[tr_, records:(_Dataset|{_List..}), "TopProbabilities", opts : OptionsPattern[]] :=
    Map[ Select[#, # > 0 &]&, ATrieClassify[tr, records, "Probabilities", opts] ];

ATrieClassify[tr_, records:(_Dataset|{_List..}), "TopProbabilities" -> n_Integer, opts : OptionsPattern[]] :=
    Map[TakeLargest[#, UpTo[n]]&, ATrieClassify[tr, records, "Probabilities", opts] ];

ATrieClassify[tr_, records:(_Dataset|{_List..}), "Probabilities", opts:OptionsPattern[] ] :=
    Block[{clRes, classLabels, stencil},

      clRes = Map[ ATrieClassify[tr, #, "Probabilities", opts] &, Normal@records ];

      classLabels = Union[Flatten[Normal[Keys /@ clRes]]];

      stencil = AssociationThread[classLabels -> 0];

      KeySort[Join[stencil, #]] & /@ clRes
    ];

End[] (* `Private` *)

EndPackage[]