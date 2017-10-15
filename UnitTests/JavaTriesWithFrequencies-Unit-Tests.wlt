(*
    Java tries with frequencies Mathematica unit tests
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

(* :Title: JavaTriesWithFrequencies-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2017-01-19 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: prefix tree, trie, Java, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

  In order to use this unit tests file set the correct paths in the test with ID "JavaTriePaths".

*)

BeginTestSection["JavaTriesWithFrequencies-Unit-Tests"]

VerificationTest[(* 1 *)
  CompoundExpression[
		Set[$JavaTriesWithFrequenciesPath, "/Users/antonov/MathematicaForPrediction/Java/TriesWithFrequencies"],
    Set[dirName, "/Users/antonov/MathematicaForPrediction"],
		{
			DirectoryQ[$JavaTriesWithFrequenciesPath],
			FileExistsQ[FileNameJoin[{$JavaTriesWithFrequenciesPath, "TriesWithFrequencies.jar"}]],
			DirectoryQ[dirName],
      FileExistsQ[FileNameJoin[{dirName, "JavaTriesWithFrequencies.m"}]]
		}]
  ,
		{True, True, True, True}
  ,
  TestID->"JavaTriePaths"
]


VerificationTest[(* 2 *)
	CompoundExpression[AppendTo[$Path, "/Users/antonov/MathFiles/MathematicaForPrediction"],
		Needs["JavaTriesWithFrequencies`"],
		JavaTriesWithFrequencies`JavaTrieInstall[$JavaTriesWithFrequenciesPath]]
	,
	Null	
	,
	TestID->"JavaTrieInitialization"
]

VerificationTest[(* 3 *)
	CompoundExpression[
		Set[words, List["bark", "barkeeper", "barkeepers", "barkeep", "barks", "barking", "barked", "barker", "barkers"]],
		Set[jTr, JavaTrieCreateBySplit[words]],
		StringMatchQ[SymbolName[jTr], StringExpression["JavaObject", BlankSequence[]]]
	]
	,
	True	
	,
	TestID->"JavaTrieCreation1"
]

VerificationTest[(* 4 *)
	CompoundExpression[
		Set[words2, List["bar", "barring", "car", "care", "caress", "cold", "colder"]],
		Set[jTr2, JavaTrieCreateBySplit[words2]],
		StringMatchQ[SymbolName[jTr2], StringExpression["JavaObject", BlankSequence[]]]
	]
	,
	True	
	,
	TestID->"JavaTrieCreation2"
]

VerificationTest[(* 5 *)
	CompoundExpression[
		Set[jTr1, JavaTrieCreate[Map[Characters, words]]],
		StringMatchQ[SymbolName[jTr1], StringExpression["JavaObject", BlankSequence[]]]
	]
	,
	True	
]

VerificationTest[(* 6 *)
	CompoundExpression[Set[res, JavaTrieToJSON[JavaTrieShrink[jTr]]], MatchQ[res, List[Rule["value", 9.`], Rule["key", ""], Rule["children", BlankNullSequence[]]]]]
	,
	True	
	,
	TestID->"JavaTrieToJSON"
]

VerificationTest[(* 7 *)
	JavaTrieHasCompleteMatchQ[jTr, Characters[#]] & /@ {"bark", "ba"}
	,
	{True,False}
	,
	TestID->"JavaTrieHasCompleteMatchQ"
]

VerificationTest[(* 8 *)
	Cases[JavaTrieToJSON[JavaTrieShrink[jTr]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	List["", "bark", "s", "ing", "e", "r", "s", "d", "ep", "er", "s"]	
	,
	TestID->"JavaTrieShrink1"
]

VerificationTest[(* 9 *)
	Cases[JavaTrieToJSON[JavaTrieShrink[jTr, "~"]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	List["", "b~a~r~k", "i~n~g", "s", "e", "r", "s", "d", "e~p", "e~r", "s"]	
	,
	TestID->"JavaTrieShrink2"
]

VerificationTest[(* 10 *)
	JLink`JavaObjectToExpression[JavaTrieContains[jTr, Map[Characters, List["barked", "balm", "barking"]]]]
	,
	List[True, False, True]	
	,
	TestID->"JavaTrieContains1"
]

VerificationTest[(* 11 *)
	JLink`JavaObjectToExpression[JavaTrieHasCompleteMatchQ[jTr, Map[Characters, List["barked", "balm", "barking"]]]]
	,
	List[True, False, True]	
]

VerificationTest[(* 12 *)
	Apply[StringJoin, JavaTrieGetWords[jTr2, List["b"]], List[1]]
	,
	List["bar", "barring"]	
	,
	TestID->"JavaTrieGetWords1"
]

VerificationTest[(* 13 *)
	Apply[StringJoin, JavaTrieGetWords[jTr2, List["c"]], List[1]]
	,
	List["car", "care", "caress", "cold", "colder"]	
	,
	TestID->"JavaTrieGetWords2"
]

VerificationTest[(* 14 *)
	CompoundExpression[Set[jTr3, JavaTrieMerge[jTr, jTr2]], Union[Apply[StringJoin, JavaTrieGetWords[jTr3, List["b"]], List[1]], Apply[StringJoin, JavaTrieGetWords[jTr3, List["c"]], List[1]]]]
	,
	Union[words, words2]	
	,
	TestID->"JavaTrieGetWords"
]

VerificationTest[(* 15 *)
	JavaTrieRootToLeafPaths[jTr]
	,
	List[List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["d", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["i", 1.`], List["n", 1.`], List["g", 1.`]]]	
	,
	TestID->"JavaRootToLeafPaths1"
]

VerificationTest[(* 16 *)
	JavaTrieGetWords[JavaTrieShrink[jTr], List["bark"]]
	,
	List[List["bark"], List["bark", "s"], List["bark", "ing"], List["bark", "e", "r"], List["bark", "e", "r", "s"], List["bark", "e", "d"], List["bark", "e", "ep"], List["bark", "e", "ep", "er"], List["bark", "e", "ep", "er", "s"]]	
	,
	TestID->"JavaTrieShrinkAndGetWords1"
]

VerificationTest[(* 17 *)
  CompoundExpression[
  	Set[jTr, JavaTrieCreateBySplit[words]],
  	Set[jTr1, JavaTrieCreate[Map[Characters, words]]],
  	JavaTrieEqualQ[jTr, jTr1]
	]
  ,
  True
  ,
  TestID->"JavaTrieEqual1"
]

VerificationTest[(* 18 *)
	(JavaTrieToJSON@JavaTrieShrink@jTr) == (JavaTrieToJSON@JavaTrieShrink@JavaTrieClone@jTr)
  ,
  True
  ,
  TestID->"JavaTrieCloneEquality1"
]

VerificationTest[(* 19 *)
  JavaTrieHasCompleteMatchQ[jTr2, Characters@#] & /@ {"ba", "bar"}
  ,
	{False, True}
  ,
  TestID->"JavaTrieHasCompleteMatchQ1"
]

VerificationTest[(* 20 *)
  JavaTrieGetWords[jTr, {"b", "a", "r", "k"}] ==
      JavaTrieGetWords[JavaTrieNodeProbabilities[jTr], {"b", "a", "r", "k"}]
  ,
  True
  ,
  TestID->"JavaTrieGetWords1"
]

VerificationTest[(* 21 *)
	(JavaTrieRootToLeafPaths[
    JavaTrieRetrieve[jTr, {"b", "a", "r", "k"}]] /. x_?NumberQ :> 0) ==
			(JavaTrieRootToLeafPaths[
        JavaTrieRetrieve[JavaTrieNodeProbabilities[jTr], {"b", "a", "r", "k"}]] /. x_?NumberQ :> 0)
  ,
  True
  ,
  TestID->"JavaTrieRootToLeafPaths1"
]

VerificationTest[(* 22 *)
  Cases[JavaTrieToJSON[JavaTrieShrink[jTr]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity] ==
      Cases[JavaTrieToJSON[JavaTrieShrink[JavaTrieNodeProbabilities[jTr]]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
  ,
  True
  ,
  TestID->"JavaTrieShrink2"
]

EndTestSection[]
