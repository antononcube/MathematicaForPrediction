(*
    Monadic phrase completion Mathematica package
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
    antononcube @ gmail . com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicPhraseCompletion *)
(* :Context: MonadicPhraseCompletion` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-10-30 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

  # Introduction

  This monad provides phrase completion functionalities based on the monad TextAMon, [1], which in turn
  uses the Java interface package [2].


  # Usage

  This creates an object using the monad in [1]:

    pObj =
     TextAMonUnit[Take[RandomSample[texts], UpTo[550]]]⟹
       TextAMonSentences[]⟹
       TextAMonEchoFunctionContext["number of sentences:", Length[#["sentences"]] &]⟹
       TextAMonComputePOSTags[]⟹
       TextAMonPOSWordsTrie[];


  This creates an object using the monad of this package:

    tObj31 = PhFillMonUnit[pObj]⟹PhFillMonMakeNGramTrie[{2, 3, 1}]

    (* PhFillMon[<|"total" -> 256387, "internal" -> 95852, "leaves" -> 160535|>,
       <|"trie" -> JLink`Objects`vm8`JavaObject2818572573212673,
       "indexPermutation" -> {2, 3, 1},
       "indexReversePermutation" -> {3, 1, 2}|>] *)

  Here is "phrase completion" for the first word in 3-gram:

    ColumnForm[
    tObj31⟹
      PhFillMonPhraseSuggestions[{"family", "and"}]⟹
      PhFillMonTakeValue
    ]

    (*
      {{<|"length" -> 3, "probability" -> 0.307692, "words" -> {"the", "family", "and"}|>},
       {<|"length" -> 3, "probability" -> 0.153846, "words" -> {"and", "family", "and"}|>},
       {<|"length" -> 3, "probability" -> 0.0769231, "words" -> {"about", "family", "and"}|>},
       ...}
    *)


  # References

  [1] Anton Antonov, Monadic text analyzer Mathematica package, (2017),
      https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTextAnalyzer.m .

  [2] Anton Antonov, Java tries with frequencies Mathematica package, (2016),
      https://github.com/antononcube/MathematicaForPrediction/blob/master/JavaTriesWithFrequencies.m .
*)

(*BeginPackage["MonadicPhraseCompletion`"]*)
(** Exported symbols added here with SymbolName::usage *)

(*Begin["`Private`"]*)

(*Phrase Fill-in Monad (PhFillMon)*)

GenerateStateMonadCode["PhFillMon"]

ClearAll[PhFillMonMakeNGramTrie]
PhFillMonMakeNGramTrie[___][None] := None;
PhFillMonMakeNGramTrie[indexPermutation_: {_Integer ..}][xs_, context_] :=
    Block[{jTr, p, ip},

      If[! TextAMonUnitQ[xs],
        Echo["The pipeline value is expected to be TextAMon.", "PhFillMonMakeNGramTrie:"];
        Return[None]
      ];

      p = FindPermutation[indexPermutation];
      ip = InversePermutation[p];
      ip = Permute[Range[Length[indexPermutation]], ip];

      (*jTr = xs⟹TextAMonMakeNGramTrie[Length[indexPermutation], indexPermutation]⟹TextAMonTakeValue;*)

      jTr = Fold[ TextAMonBind, xs, {TextAMonMakeNGramTrie[Length[indexPermutation], indexPermutation], TextAMonTakeValue} ];

      PhFillMon[JavaTrieNodeCounts[jTr],
        Join[context, <|"trie" -> jTr, "indexPermutation" -> indexPermutation, "indexReversePermutation" -> ip|>]]
    ];


ClearAll[PhFillMonPhraseSuggestionPaths]
Options[PhFillMonPhraseSuggestionPaths] = {Prepend -> True};
PhFillMonPhraseSuggestionPaths[phrase : {_String ..}, maxLength : (Automatic | _Integer) : Automatic][xs_, context_] :=
    Block[{prependQ = TrueQ[OptionValue[PhFillMonPhraseSuggestionPaths, Prepend]], res, phrasePairs},

      If[! KeyExistsQ[context["trie"]],
        Echo["No n-gram trie.", "PhFillMonMakeNGramTrie:"];
        Return[None]
      ];

      If[! JavaTrieKeyQ[context["trie"], phrase],
        PhFillMon[{}, context],
      (*ELSE*)
        If[TrueQ[maxLength === Automatic],
          res =
              SortBy[Rest /@
                  JavaTrieRootToLeafPaths[
                    JavaTrieRetrieve[context["trie"], phrase]], -#[[All, 2]] &],
          res =
              SortBy[Rest /@
                  JavaTrieRootToLeafPaths[
                    JavaTriePrune[JavaTrieRetrieve[context["trie"], phrase], maxLength]], -#[[All, 2]] &]
        ];

        If[prependQ,
          phrasePairs = Transpose[{phrase, Table[1, Length[phrase]]}];
          res = Map[Join[phrasePairs, #] &, res]
        ];

        PhFillMon[res, context]
      ]
    ];


ClearAll[PhFillMonPhraseSuggestions]
PhFillMonPhraseSuggestions[phrase : {_String ...}][xs_, context_] :=
    Block[{res},
      If[Length[phrase] == 0, PhFillMon[{}, context],
      (*ELSE*)

        res = PhFillMonBind[PhFillMon[xs, context], PhFillMonPhraseSuggestionPaths[phrase, Automatic]];

        res = Select[First[res], Length[#] == Length[context["indexPermutation"]] &];

        res =
            Map[
              <|"length" -> Length[#],
                "probability" -> Apply[Times, #[[All, 2]]],
                "words" -> #[[context["indexReversePermutation"], 1]]|> &, res];
        PhFillMon[res, context]
      ]
    ];


ClearAll[PhFillMonPredictedIndex]
PhFillMonPredictedIndex[][xs_,context_] :=
    Block[{},
      If[! KeyExistsQ[context["indexPermutation"]],
        Echo["No n-gram trie is made.", "PhFillMonPredictedIndex:"];
        Return[None]
      ];
      PhFillMon[Last[context["indexPermutation"]], context]
    ];

(*End[] * `Private` *)

(*EndPackage[]*)