(*
    Tries with frequencies Mathematica unit tests
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
    antononcube @ gmai l . c om,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: TriesWithFrequencies-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2018-04-17 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: prefix tree, trie, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

		The initial version of this test file was made for the development of

    "Tries with frequencies Mathematica through Associations package"
    https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/AssociationTriesWithFrequencies.m

    At this point should be used on/with:

    "Tries with frequencies Mathematica"
    https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m
*)

BeginTestSection["TriesWithFrequencies-Unit-Tests"];


VerificationTest[(* 1 *)
	CompoundExpression[
		ClearAll["Trie*"],
    userDirName = "~/";
    Get[FileNameJoin[ {userDirName, "MathematicaForPrediction", "TriesWithFrequencies.m"} ] ],
		TrieCreate[{}],
    Length[DownValues[TriesWithFrequencies`TrieCreate]] > 0
	]
	,
	True
	,
	TestID->"TrieInitialization"
];


VerificationTest[(* 3 *)
  CompoundExpression[
    Set[words, List["bark", "barkeeper", "barkeepers", "barkeep", "barks", "barking", "barked", "barker", "barkers"]],
    Set[aTr, TrieCreateBySplit[words]],
    TrieQ[aTr]
  ]
  ,
  True
  ,
  TestID->"TrieCreation1"
];

VerificationTest[(* 4 *)
  CompoundExpression[
    Set[words2, List["bar", "barring", "car", "care", "caress", "cold", "colder"]],
    Set[aTr2, TrieCreateBySplit[words2]],
    TrieQ[aTr2]
  ]
  ,
  True
  ,
  TestID->"TrieCreation2"
];

VerificationTest[(* 5 *)
  CompoundExpression[
    Set[aTr1, TrieCreate[Map[Characters, words]]],
    TrieQ[aTr1]
  ]
  ,
  True
];

VerificationTest[(* 3 *)
	CompoundExpression[Set[res, TrieToJSON[TrieShrink[aTr]]], MatchQ[res, List[ Rule["key", $TrieRoot], Rule["value", 9], Rule["children", BlankNullSequence[]]]]]
	,
	True
	,
	TestID->"TrieToJSON"
];

VerificationTest[(* 7 *)
	TrieHasCompleteMatchQ[aTr, Characters[#]] & /@ {"bark", "ba"}
	,
	{True,False}
	,
	TestID->"TrieHasCompleteMatchQ"
];

VerificationTest[(* 8 *)
  Sort @ Cases[TrieToJSON[TrieShrink[aTr]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	Sort @ List[$TrieRoot, "bark", "s", "ing", "e", "r", "s", "d", "ep", "er", "s"]
	,
	TestID->"TrieShrink1"
];

VerificationTest[(* 9 *)
  Sort @ Cases[TrieToJSON[TrieShrink[aTr,"~"]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	Sort @ List[$TrieRoot, "b~a~r~k", "i~n~g", "s", "e", "r", "s", "d", "e~p", "e~r", "s"]
	,
	TestID->"TrieShrink2"
];

VerificationTest[(* 10 *)
	TrieContains[aTr, #]& /@  Map[Characters, List["barked", "balm", "barking"]]
	,
	List[True, False, True]	
	,
	TestID->"TrieContains1"
];

VerificationTest[(* 11 *)
	TrieHasCompleteMatchQ[aTr, #]& /@ Map[Characters, List["barked", "balm", "barking"]]
	,
	List[True, False, True]	
];

VerificationTest[(* 12 *)
	Sort @ Apply[StringJoin, TrieGetWords[aTr2, List["b"]], List[1]]
	,
	Sort @ List["bar", "barring"]
	,
	TestID->"TrieGetWords1"
];

VerificationTest[(* 13 *)
	Sort @ Apply[StringJoin, TrieGetWords[aTr2, List["c"]], List[1]]
	,
	Sort @ List["car", "care", "caress", "cold", "colder"]
	,
	TestID->"TrieGetWords2"
];

VerificationTest[(* 14 *)
	CompoundExpression[Set[aTr3, TrieMerge[aTr, aTr2]], Union[Apply[StringJoin, TrieGetWords[aTr3, List["b"]], List[1]], Apply[StringJoin, TrieGetWords[aTr3, List["c"]], List[1]]]]
	,
	Union[words, words2]	
	,
	TestID->"TrieGetWords"
]

VerificationTest[(* 15 *)
	t = Sort @ TrieRootToLeafPaths[aTr];
	t /. {$TrieRoot->"", v_?NumberQ :> N[v]}
	,
	Sort @ List[List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["d", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["i", 1.`], List["n", 1.`], List["g", 1.`]]]
	,
	TestID->"ARootToLeafPaths1"
];

VerificationTest[(* 16 *)
	Sort @ TrieGetWords[TrieShrink[aTr], List["bark"]]
	,
	Sort @ List[List["bark"], List["bark", "s"], List["bark", "ing"], List["bark", "e", "r"], List["bark", "e", "r", "s"], List["bark", "e", "d"], List["bark", "e", "ep"], List["bark", "e", "ep", "er"], List["bark", "e", "ep", "er", "s"]]
	,
	TestID->"TrieShrinkAndGetWords1"
];

VerificationTest[(* 17 *)
  CompoundExpression[
  	Set[aTr, TrieCreateBySplit[words]],
  	Set[aTr1, TrieCreate[Map[Characters, words]]],
(*  	TrieEqualQ[aTr, aTr1]*)
    aTr == aTr1
	]
  ,
  True
  ,
  TestID->"TrieEqual1"
];


VerificationTest[(* 19 *)
  TrieHasCompleteMatchQ[aTr2, Characters@#] & /@ {"ba", "bar"}
  ,
	{False, True}
  ,
  TestID->"TrieHasCompleteMatchQ1"
];

VerificationTest[(* 20 *)
  TrieGetWords[aTr, {"b", "a", "r", "k"}] ==
      TrieGetWords[TrieNodeProbabilities[aTr], {"b", "a", "r", "k"}]
  ,
  True
  ,
  TestID->"TrieGetWords1"
];

VerificationTest[(* 21 *)
	(TrieRootToLeafPaths[
    TrieSubTrie[aTr, {"b", "a", "r", "k"}]] /. x_?NumberQ :> 0) ==
			(TrieRootToLeafPaths[
        TrieSubTrie[TrieNodeProbabilities[aTr], {"b", "a", "r", "k"}]] /. x_?NumberQ :> 0)
  ,
  True
  ,
  TestID->"TrieRootToLeafPaths1"
];

VerificationTest[(* 22 *)
  Cases[TrieShrink[aTr], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity] ==
      Cases[TrieShrink[TrieNodeProbabilities[aTr]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
  ,
  True
  ,
  TestID->"TrieShrink2"
];


VerificationTest[(* 23 *)
  CompoundExpression[Set[words, List["bar", "bars", "balk", "car", "caress", "card", "cardan", "cardigan"]], Set[aTr3, TrieCreateBySplit[words]], TrieQ[aTr3]]
  ,
  True
  ,
  TestID->"TrieCreation3"
];

VerificationTest[(* 24 *)
  Map[Function[TrieContains[aTr3, Characters[Slot[1]]]], List["balkan", "ba", "bar", "car", "care"]]
  ,
  List[False, False, True, True, False]
  ,
  TestID->"TrieContains1"
];

VerificationTest[(* 25 *)
  Map[Function[TrieHasCompleteMatchQ[aTr3, Characters[Slot[1]]]], List["balkan", "ba", "bar", "car", "care"]]
  ,
  List[True, False, True, True, True]
  ,
  TestID->"TrieHasCompleteMatchQ"
];

VerificationTest[(* 26 *)
  Map[Function[TrieKeyExistsQ[aTr3, Characters[Slot[1]]]], List["balkan", "ba", "bar", "car", "care"]]
  ,
  List[False, True, True, True, True]
  ,
  TestID->"TrieKeyExistsQ"
];

VerificationTest[(* 27 *)
  Map[Depth, List[aTr, TriePrune[aTr, 5]]]
  ,
  List[13, 8]
  ,
  TestID->"TriePrune1"
];

VerificationTest[(* 28 *)
  TrieToListTrie[aTr]
  ,
  List[List[List[], 9], List[List["b", 9], List[List["a", 9], List[List["r", 9], List[List["k", 9], List[List["e", 6], List[List["e", 3], List[List["p", 3], List[List["e", 2], List[List["r", 2], List[List["s", 1]]]]]], List[List["d", 1]], List[List["r", 2], List[List["s", 1]]]], List[List["s", 1]], List[List["i", 1], List[List["n", 1], List[List["g", 1]]]]]]]]]
  ,
  TestID->"TrieToListTrie1"
];


EndTestSection[]
