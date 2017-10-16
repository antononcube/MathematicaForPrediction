(*
    Monadic text analyzer Mathematica package
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



(* :Title: MonadicTextAnalyzer *)
(* :Context: MonadicTextAnalyzer` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-10-14 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

(*BeginPackage["MonadicTextAnalyzer`"]*)

(*Begin["`Private`"]*)


If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];

If[Length[DownValues[OutlierIdentifiers`OutlierPosition]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]
];

(* This package is based on a Java jar file and appropriate JLink initilization has to be done. *)
If[Length[DownValues[JavaTriesWithFrequencies`JavaTrieCreate]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/JavaTriesWithFrequencies.m"]
];

(*
Needs["StateMonadCodeGenerator`"];
Needs["CrossTabulate`"];
Needs["OutlierIdentifiers`"];
Needs["JavaTriesWithFrequencies`"];
*)

(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of TextAMon monad (through StMon.) *)

GenerateStateMonadCode[ "TextAMon", "StringContextNames" -> True ]


(**************************************************************)
(* Java installation                                          *)
(**************************************************************)


If[ !StringQ[$JavaTriesWithFrequenciesPath],
  $JavaTriesWithFrequenciesPath = "/Users/antonov/MathematicaForPrediction/Java/TriesWithFrequencies";
];

If[ !StringQ[$POSTaggerPath],
  $POSTaggerPath = "/Users/antonov/Java/StanfordPosTagger/stanford-postagger-2015-12-09";
];

If[ ( BooleanQ[$LoadJava] && $LoadJava ) || !( BooleanQ[$LoadJava] && ! $LoadJava ),

  Needs["JLink`"];
  AddToClassPath[$JavaTriesWithFrequenciesPath];
  AddToClassPath[$POSTaggerPath];
  ReinstallJava[JVMArguments -> "-Xmx2g"];

  LoadJavaClass["java.util.Collections"];
  LoadJavaClass["java.util.Arrays"];
  LoadJavaClass["Trie"];
  LoadJavaClass["TrieFunctions"];

  LoadJavaClass["edu.stanford.nlp.tagger.maxent.MaxentTagger"];
  JAVASTABFORDPOSTAGGER =
      JavaNew["edu.stanford.nlp.tagger.maxent.MaxentTagger",
        FileNameJoin[{$POSTaggerPath, "models", "english-left3words-distsim.tagger"}]];

];


(**************************************************************)
(* Stanford POS tagger                                        *)
(**************************************************************)


ClearAll[JavaStanfordTagString]
JavaStanfordTagString[str_String] :=
    JavaBlock[
      JAVASTABFORDPOSTAGGER@tagString[str]
    ];

Clear[SeparatePOSTags]
SeparatePOSTags[tagged_String] :=
    Block[{},
      StringCases[tagged,
        word : Except[WhitespaceCharacter] .. ~~ "_" ~~
            tag : (Except[WhitespaceCharacter] ..) :> {word, tag}]
    ];

aStanfordPOSTagToAbbr = <|"CC" -> "Coordinating conjunction ",
  "CD" -> "Cardinal number ", "DT" -> "Determiner ",
  "EX" -> "Existential there ", "FW" -> "Foreign word ",
  "IN" -> "Preposition or subordinating conjunction ",
  "JJ" -> "Adjective ", "JJR" -> "Adjective, comparative ",
  "JJS" -> "Adjective, superlative ", "LS" -> "List item marker ",
  "MD" -> "Modal ", "NN" -> "Noun, singular or mass ",
  "NNS" -> "Noun, plural ", "NNP" -> "Proper noun, singular ",
  "NNPS" -> "Proper noun, plural ", "PDT" -> "Predeterminer ",
  "POS" -> "Possessive ending ", "PRP" -> "Personal pronoun ",
  "PRP$" -> "Possessive pronoun ", "RB" -> "Adverb ",
  "RBR" -> "Adverb, comparative ", "RBS" -> "Adverb, superlative ",
  "RP" -> "Particle ", "SYM" -> "Symbol ", "TO" -> "to ",
  "UH" -> "Interjection ", "VB" -> "Verb, base form ",
  "VBD" -> "Verb, past tense ",
  "VBG" -> "Verb, gerund or present participle ",
  "VBN" -> "Verb, past participle ",
  "VBP" -> "Verb, non­3rd person singular present ",
  "VBZ" -> "Verb, 3rd person singular present ",
  "WDT" -> "Wh­determiner ", "WP" -> "Wh­pronoun ",
  "WP$" -> "Possessive wh­pronoun ", "WRB" -> "Wh­adverb",
  "," -> ",", "." -> "."|>;


(**************************************************************)
(* Text analysis functions                                    *)
(**************************************************************)

ClearAll[TextAMonSentences]

TextAMonSentences[___][None] := None;
TextAMonSentences[][xs_, context_] :=
    Block[{sentences},
      Which[

        StringQ[xs],
        sentences = TextSentences[ xs ];
        TextAMon[ sentences, Join[ context, <|"text"->xs, "sentences"->sentences|> ] ],

        KeyExistsQ[context, "text"],
        sentences = TextSentences[ context["text"] ];
        TextAMon[ sentences, Join[ context, <|"sentences"->sentences|> ] ],

        True,
        Echo["Ingest texts first.", "TextAMonSentences:"];
        None
      ]
    ];


ClearAll[TextAMonComputePOSTags]

TextAMonComputePOSTags[___][None] := None;
TextAMonComputePOSTags[args___][xs_, context_] :=
    Block[{sentences, posTags, res},

      Which[

        VectorQ[xs, StringQ],
        sentences = xs,

        KeyExistsQ[context, "sentences" ],
        sentences = context["sentences"],

        True,
        res = TextAMonSentences[args][xs, context];
        If[ TrueQ[ res === None ],
          Echo["Calculate POS tags first.", "TextAMonComputePOSTags:"];
          Return[None]
        ];
        sentences = res[[2]]["sentences"]

      ];

      posTags =
            Map[SeparatePOSTags[JavaStanfordTagString[ToLowerCase@#]] &, context["sentences"] ];

      TextAMon[ posTags, Join[ context, <| "posTags" -> posTags |>] ]
];


ClearAll[TextAMonPOSWordsTrie]

TextAMonPOSWordsTrie[___][None] := None;
TextAMonPOSWordsTrie[separator_String:"®"][xs_,context_] :=
    Block[{jPOSWordTrie},

      Which[
        KeyExistsQ[context, "posTags"],

        jPOSWordTrie =
            JavaTrieNodeProbabilities@
                JavaTrieCreateBySplit[
                  Map[
                    StringJoin[Riffle[#, separator]] &,
                    Reverse /@ Flatten[ context["posTags"], 1] ],
                  separator];

        TextAMon[ jPOSWordTrie, Join[ context, <| "jPOSWordTrie"->jPOSWordTrie |>] ],

        True,
        Echo["Calculate POS-to-words trie first.", "TextAMonPOSWordsTrie:"];
        None
      ]

    ];


ClearAll[TextAMonEchoPOSWordsInterface];

Options[TextAMonEchoPOSWordsInterface] = {ImageSize->400};

TextAMonEchoPOSWordsInterface[___][None] := None;
TextAMonEchoPOSWordsInterface[ opts:OptionsPattern[] ][xs_, context_] :=
    Block[{ jPOSWordTrie, allPosTags, tagSelectionRules, abbrTagRules, imSize },

      imSize = OptionValue[TextAMonEchoPOSWordsInterface, ImageSize];
      If[ !IntegerQ[imSize],
        Echo["ImageSize should have values that a positive integers.", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];


      If[ !KeyExistsQ[context, "posTags"],
        Echo["Calculate POS tags first.", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];

      If[ !KeyExistsQ[context, "jPOSWordTrie"],
        Echo["Calculate jPOSWordTrie first.", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];

      jPOSWordTrie = context["jPOSWordTrie"];

      allPosTags = Union[Flatten[context["posTags"], 1][[All, 2]]];
      Length[allPosTags];

      tagSelectionRules =
          Rule @@@ Quiet[
            Select[Map[{#, # <> " : " <> StringTrim[aStanfordPOSTagToAbbr[#]]} &, allPosTags], StringQ[#[[-1]]] &]
          ];
      abbrTagRules = Reverse /@ tagSelectionRules;

      Echo @
          With[{ jPOSWordTrie=jPOSWordTrie, imSize=imSize, abbrTagRules=abbrTagRules},
            Manipulate[
              DynamicModule[{posTag, jjTr, leafProbs},
                posTag = posAbbr /. abbrTagRules;
                jjTr = JavaTrieRetrieve[jPOSWordTrie, {posTag}];
                leafProbs = {"key", "value"} /. JavaTrieLeafProbabilities[jjTr, "Normalized" -> True];
                Grid[{
                  Map[
                    Style[#, GrayLevel[0.5], FontFamily -> "Times"] &,
                    {"Pareto principle adherence", "Word probabilities for " <> posTag}
                  ],
                  {ParetoLawPlot[leafProbs[[All, 2]],
                    ImageSize -> Round[ 0.75*imSize ]],
                    Pane[
                      GridTableForm[SortBy[leafProbs, -#[[2]] &], TableHeadings -> {"Literal", "Probability"}],
                      ImageSize -> {imSize, imSize}, Scrollbars -> {False, True}]
                  }}, Alignment -> {{Left, Left}, {Top, Top}}]
              ],
              {{posAbbr, "JJR : Adjective, comparative", "POS"},
                abbrTagRules[[All, 1]], ControlType -> PopupMenu}]
          ];

      TextAMon[ xs, context ]
    ];


ClearAll[TextAMonMakeWordTrie]

TextAMonMakeWordTrie[___][None] := None;
TextAMonMakeWordTrie[ separator_String:"®" ][xs_, context_] :=
    Block[{words, jWordTrie},

      Which[

        KeyExistsQ[context, "posTags"],
        words = context["posTags"][[All, All, 1]],

        KeyExistsQ[context, "text"],
        words = TextWords[context["text"]],

        True,
        Echo["Ingest text first.", "TextAMonCreateWordTrie:"];
        Return[None]
      ];

      jWordTrie = JavaTrieCreateBySplit[ Map[StringJoin @@ Riffle[#, separator] &, words], separator];

      TextAMon[ jWordTrie, Join[ context, <| "jWordTrie" -> jWordTrie |>] ]
    ];


ClearAll[TextAMonMakeNGramTrie]

TextAMonMakeNGramTrie[___][None] := None;
TextAMonMakeNGramTrie[___][xs_, context_] :=
    Block[{},
      Echo["Specify the number of words for the n-grams. (An integer.)", "TextAMonMakeNGramTrie:"];
      None
    ];
TextAMonMakeNGramTrie[ n_Integer, separator_String:"~" ][xs_, context_] :=
    Block[{words, ngrams, jNGramTrie},

      Which[

        KeyExistsQ[context, "posTags"],
        words = context["posTags"][[All, All, 1]],

        KeyExistsQ[context, "text"],
        words = TextWords[context["text"]],

        True,
        Echo["Ingest text first.", "TextAMonMakeNGramTrie:"];
        Return[None]
      ];

      ngrams = Map[StringJoin @@ Riffle[#, "~"] &, Flatten[Partition[#, n, 1] & /@ words, 1]];

      jNGramTrie = JavaTrieNodeProbabilities[JavaTrieCreateBySplit[ngrams, "~"]];

      TextAMon[ jNGramTrie, context ]
    ];


(*End[] * `Private` *)

(*EndPackage[]*)