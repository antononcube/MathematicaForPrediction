BeginTestSection["JavaTriesWithFrequencies-Unit-Tests"]

VerificationTest[(* 1 *)
	CompoundExpression[AppendTo[$Path, "/Users/antonov/MathFiles/MathematicaForPrediction"], Needs["JavaTriesWithFrequencies`"], Set[$JavaTriesWithFrequenciesPath, "/Users/antonov/MathFiles/MathematicaForPrediction/Java/TriesWithFrequencies"], JavaTriesWithFrequencies`JavaTrieInstall[$JavaTriesWithFrequenciesPath]]
	,
	Null	
	,
	TestID->"JavaTrieInitialization"
]

VerificationTest[(* 2 *)
	CompoundExpression[Set[words, List["bark", "barkeeper", "barkeepers", "barkeep", "barks", "barking", "barked", "barker", "barkers"]], Set[jTr, JavaTrieCreateBySplit[words]], StringMatchQ[SymbolName[jTr], StringExpression["JavaObject", BlankSequence[]]]]
	,
	True	
	,
	TestID->"JavaTrieCreation1"
]

VerificationTest[(* 3 *)
	CompoundExpression[Set[words2, List["bar", "barring", "car", "care", "caress", "cold", "colder"]], Set[jTr2, JavaTrieCreateBySplit[words2]], StringMatchQ[SymbolName[jTr2], StringExpression["JavaObject", BlankSequence[]]]]
	,
	True	
	,
	TestID->"JavaTrieCreation2"
]

VerificationTest[(* 4 *)
	CompoundExpression[Set[jTr1, JavaTrieCreate[Map[Characters, words]]], StringMatchQ[SymbolName[jTr1], StringExpression["JavaObject", BlankSequence[]]]]
	,
	True	
]

VerificationTest[(* 5 *)
	CompoundExpression[Set[res, JavaTrieToJSON[JavaTrieShrink[jTr]]], MatchQ[res, List[Rule["value", 9.`], Rule["key", ""], Rule["children", BlankNullSequence[]]]]]
	,
	True	
	,
	TestID->"JavaTrieToJSON"
]

VerificationTest[(* 6 *)
	JavaTrieCompleteMatch[jTr, Characters[#]] & /@ {"bark", "ba"}
	,
	{True,False}
	,
	TestID->"JavaTrieCompleteMatch"
]

VerificationTest[(* 7 *)
	Cases[JavaTrieToJSON[JavaTrieShrink[jTr]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	List["", "bark", "s", "ing", "e", "r", "s", "d", "ep", "er", "s"]	
	,
	TestID->"JavaTrieShrink1"
]

VerificationTest[(* 8 *)
	Cases[JavaTrieToJSON[JavaTrieShrink[jTr, "~"]], RuleDelayed[Rule["key", Pattern[v, Blank[]]], v], Infinity]
	,
	List["", "b~a~r~k", "i~n~g", "s", "e", "r", "s", "d", "e~p", "e~r", "s"]	
	,
	TestID->"JavaTrieShrink2"
]

VerificationTest[(* 9 *)
	JLink`JavaObjectToExpression[JavaTrieContains[jTr, Map[Characters, List["barked", "balm", "barking"]]]]
	,
	List[True, False, True]	
	,
	TestID->"JavaTrieContains1"
]

VerificationTest[(* 10 *)
	JLink`JavaObjectToExpression[JavaTrieCompleteMatch[jTr, Map[Characters, List["barked", "balm", "barking"]]]]
	,
	List[True, False, True]	
]

VerificationTest[(* 11 *)
	Apply[StringJoin, JavaTrieGetWords[jTr2, List["b"]], List[1]]
	,
	List["bar", "barring"]	
	,
	TestID->"JavaTrieGetWords1"
]

VerificationTest[(* 12 *)
	Apply[StringJoin, JavaTrieGetWords[jTr2, List["c"]], List[1]]
	,
	List["car", "care", "caress", "cold", "colder"]	
	,
	TestID->"JavaTrieGetWords2"
]

VerificationTest[(* 13 *)
	CompoundExpression[Set[jTr3, JavaTrieMerge[jTr, jTr2]], Union[Apply[StringJoin, JavaTrieGetWords[jTr3, List["b"]], List[1]], Apply[StringJoin, JavaTrieGetWords[jTr3, List["c"]], List[1]]]]
	,
	Union[words, words2]	
	,
	TestID->"JavaTrieGetWords"
]

VerificationTest[(* 14 *)
	JavaTrieRootToLeafPaths[jTr]
	,
	List[List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["d", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["e", 6.`], List["e", 3.`], List["p", 3.`], List["e", 2.`], List["r", 2.`], List["s", 1.`]], List[List["", 9.`], List["b", 9.`], List["a", 9.`], List["r", 9.`], List["k", 9.`], List["i", 1.`], List["n", 1.`], List["g", 1.`]]]	
	,
	TestID->"JavaRootToLeafPaths1"
]

VerificationTest[(* 15 *)
	JavaTrieGetWords[JavaTrieShrink[jTr], List["bark"]]
	,
	List[List["bark"], List["bark", "s"], List["bark", "ing"], List["bark", "e", "r"], List["bark", "e", "r", "s"], List["bark", "e", "d"], List["bark", "e", "ep"], List["bark", "e", "ep", "er"], List["bark", "e", "ep", "er", "s"]]	
	,
	TestID->"JavaTrieShrinkAndGetWords1"
]

EndTestSection[]
