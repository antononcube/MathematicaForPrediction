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

*)

BeginTestSection["TriesWithFrequencies-Unit-Tests"]


VerificationTest[(* 1 *)
	CompoundExpression[
		ClearAll["ATrie*"],
    Get["/Users/aantonov/MathematicaForPrediction/Misc/AssociationTriesWithFrequencies.m"],
		ATrieCreate[{}],
    Length[DownValues[AssociationTriesWithFrequencies`ATrieCreate]] > 0
	]
	,
	True
	,
	TestID->"ATrieInitialization"
]


VerificationTest[(* 3 *)
  CompoundExpression[
    Set[words, List["bark", "barkeeper", "barkeepers", "barkeep", "barks", "barking", "barked", "barker", "barkers"]],
    Set[aTr, ATrieCreateBySplit[words]],
    ATrieQ[aTr]
  ]
  ,
  True
  ,
  TestID->"ATrieCreation1"
]

VerificationTest[(* 4 *)
  CompoundExpression[
    Set[words2, List["bar", "barring", "car", "care", "caress", "cold", "colder"]],
    Set[aTr2, ATrieCreateBySplit[words2]],
    ATrieQ[aTr2]
  ]
  ,
  True
  ,
  TestID->"ATrieCreation2"
]

VerificationTest[(* 5 *)
  CompoundExpression[
    Set[aTr1, ATrieCreate[Map[Characters, words]]],
    ATrieQ[aTr1]
  ]
  ,
  True
]

VerificationTest[(* 3 *)
	CompoundExpression[Set[res, ATrieToJSON[ATrieShrink[aTr]]], MatchQ[res, List[ Rule["key", $TrieRoot], Rule["value", 9], Rule["children", BlankNullSequence[]]]]]
	,
	True
	,
	TestID->"ATrieToJSON"
]

VerificationTest[(* 7 *)
	ATrieHasCompleteMatchQ[aTr, Characters[#]] & /@ {"bark", "ba"}
	,
	{True,False}
	,
	TestID->"ATrieHasCompleteMatchQ"
]

VerificationTest[(* 8 *)
  Sort @ Cases[ATrieToJSON[ATrieShrink[aTr]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	Sort @ List[$TrieRoot, "bark", "s", "ing", "e", "r", "s", "d", "ep", "er", "s"]
	,
	TestID->"ATrieShrink1"
]

VerificationTest[(* 9 *)
  Sort @ Cases[ATrieToJSON[ATrieShrink[aTr,"~"]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	Sort @ List[$TrieRoot, "b~a~r~k", "i~n~g", "s", "e", "r", "s", "d", "e~p", "e~r", "s"]
	,
	TestID->"ATrieShrink2"
]

VerificationTest[(* 10 *)
	ATrieContains[aTr, #]& /@  Map[Characters, List["barked", "balm", "barking"]]
	,
	List[True, False, True]	
	,
	TestID->"ATrieContains1"
]

VerificationTest[(* 11 *)
	ATrieHasCompleteMatchQ[aTr, #]& /@ Map[Characters, List["barked", "balm", "barking"]]
	,
	List[True, False, True]	
]

VerificationTest[(* 12 *)
	Sort @ Apply[StringJoin, ATrieGetWords[aTr2, List["b"]], List[1]]
	,
	Sort @ List["bar", "barring"]
	,
	TestID->"ATrieGetWords1"
]

VerificationTest[(* 13 *)
	Sort @ Apply[StringJoin, ATrieGetWords[aTr2, List["c"]], List[1]]
	,
	Sort @ List["car", "care", "caress", "cold", "colder"]
	,
	TestID->"ATrieGetWords2"
]

VerificationTest[(* 14 *)
	CompoundExpression[Set[aTr3, ATrieMerge[aTr, aTr2]], Union[Apply[StringJoin, ATrieGetWords[aTr3, List["b"]], List[1]], Apply[StringJoin, ATrieGetWords[aTr3, List["c"]], List[1]]]]
	,
	Union[words, words2]	
	,
	TestID->"ATrieGetWords"
]

VerificationTest[(* 15 *)
	t = Sort @ ATrieRootToLeafPaths[aTr];
	t /. {$TrieRoot->"", v_?NumberQ :> N[v]}
	,
	Sort @ List[List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["d", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["i", 1.`], List["n", 1.`], List["g", 1.`]]]
	,
	TestID->"JavaRootToLeafPaths1"
]

VerificationTest[(* 16 *)
	Sort @ ATrieGetWords[ATrieShrink[aTr], List["bark"]]
	,
	Sort @ List[List["bark"], List["bark", "s"], List["bark", "ing"], List["bark", "e", "r"], List["bark", "e", "r", "s"], List["bark", "e", "d"], List["bark", "e", "ep"], List["bark", "e", "ep", "er"], List["bark", "e", "ep", "er", "s"]]
	,
	TestID->"ATrieShrinkAndGetWords1"
]

VerificationTest[(* 17 *)
  CompoundExpression[
  	Set[aTr, ATrieCreateBySplit[words]],
  	Set[aTr1, ATrieCreate[Map[Characters, words]]],
  	ATrieEqualQ[aTr, aTr1]
	]
  ,
  True
  ,
  TestID->"ATrieEqual1"
]


VerificationTest[(* 19 *)
  ATrieHasCompleteMatchQ[aTr2, Characters@#] & /@ {"ba", "bar"}
  ,
	{False, True}
  ,
  TestID->"ATrieHasCompleteMatchQ1"
]

VerificationTest[(* 20 *)
  ATrieGetWords[aTr, {"b", "a", "r", "k"}] ==
      ATrieGetWords[ATrieNodeProbabilities[aTr], {"b", "a", "r", "k"}]
  ,
  True
  ,
  TestID->"ATrieGetWords1"
]

VerificationTest[(* 21 *)
	(ATrieRootToLeafPaths[
    ATrieSubTrie[aTr, {"b", "a", "r", "k"}]] /. x_?NumberQ :> 0) ==
			(ATrieRootToLeafPaths[
        ATrieSubTrie[ATrieNodeProbabilities[aTr], {"b", "a", "r", "k"}]] /. x_?NumberQ :> 0)
  ,
  True
  ,
  TestID->"ATrieRootToLeafPaths1"
]

VerificationTest[(* 22 *)
  Cases[ATrieShrink[aTr], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity] ==
      Cases[ATrieShrink[ATrieNodeProbabilities[aTr]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
  ,
  True
  ,
  TestID->"ATrieShrink2"
]

EndTestSection[]
