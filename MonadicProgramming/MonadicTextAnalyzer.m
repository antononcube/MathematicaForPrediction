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
(* :Discussion:

   # Introduction

   This package provides various functions for analyzing text using the Stanford Parts Of Speech (POS) tagger [1],
   Tries with frequencies, [2,3].

   The Java ARchive (JAR) files of [1,2] are used through JLink. Since full paths are required when hooking up
   with JLink, change accordingly the paths $JavaTriesWithFrequenciesPath and $POSTaggerPath .


   # Usage

   Here we get a collection of texts:

    speeches = ResourceData[ResourceObject["Presidential Nomination Acceptance Speeches"]];
    texts = Normal[speeches[[All, "Text"]]];


   Here is create the monad object, extract sentences, tags POS, and make a trie with the POS-word pairs:

    pObj =
      TextAMonUnit[Take[RandomSample[texts], UpTo[550]]]⟹
        TextAMonSentences[]⟹
        TextAMonEchoFunctionContext["number of sentences:", Length[#["sentences"]] &]⟹
        TextAMonComputePOSTags[]⟹
        TextAMonPOSWordsTrie[];


   Here we invoke an interactive interface for text analysis based on word frequencies for a selected POS:

    pObj⟹
      TextAMonEchoPOSWordsInterface[ImageSize -> 400, "ParetoFraction" -> 0.7, "ParetoApplicationThreshold" -> 300];


   # References

   [1] The Stanford parts of speech tagger,
      https://nlp.stanford.edu/software/tagger.shtml .

   [2] Anton Antonov, Java tries with frequencies Mathematica package, (2017),
      https://github.com/antononcube/MathematicaForPrediction/blob/master/JavaTriesWithFrequencies.m .

   [3] Anton Antonov, Tries with frequencies in Java, (2017),
      https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Tries-with-frequencies-in-Java.md .

*)

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

If[Length[DownValues[Soundex`Soundex]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/Soundex.m"]
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

If[ ( BooleanQ[$LoadJava] && $LoadJava ) || ! BooleanQ[$LoadJava],

  Needs["JLink`"];
  AddToClassPath[$JavaTriesWithFrequenciesPath];
  AddToClassPath[$POSTaggerPath];
  ReinstallJava[JVMArguments -> "-Xmx8g -Xms1g"];

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
    Block[{text, sentences},
      Which[

        StringQ[xs],
        sentences = TextSentences[ xs ];
        TextAMon[ sentences, Join[ context, <|"text"->xs, "sentences"->sentences|> ] ],

        VectorQ[xs,StringQ],
        text = StringJoin[Riffle[xs," "]];
        sentences = TextSentences[ text ];
        TextAMon[ sentences, Join[ context, <|"text"->text, "sentences"->sentences|> ] ],

        KeyExistsQ[context, "text"],
        sentences = TextSentences[ context["text"] ];
        TextAMon[ sentences, Join[ context, <|"sentences"->sentences|> ] ],

        True,
        Echo["Ingest text(s) first.", "TextAMonSentences:"];
        None
      ]
    ];


ClearAll[TextAMonWords]

Options[TextAMonWords] = { Method-> TextWords };

TextAMonWords[___][None] := None;
TextAMonWords[][xs_, context_] :=
    Block[{words, func = OptionValue[TextAMonWords, Method]},
      Which[

        KeyExistsQ[context, "sentences"],
        words = Union[ Flatten[ func /@ context["sentences"] ] ];
        TextAMon[ words, context ],

        True,
        Fold[ TextAMonBind, TextAMon[ xs, context ], { TextAMonSentences[], TextAMonWords[] }]
      ]
    ];


ClearAll[TextAMonComputePOSTags]

TextAMonComputePOSTags[___][None] := None;
TextAMonComputePOSTags[args___][xs_, context_] :=
    TextAMonBind[
      TextAMon[xs,context],
      TextAMonComputeTagWordPairs["SentenceToTagWordPairsFunction" -> (SeparatePOSTags[JavaStanfordTagString[ToLowerCase@#]] &)]
    ];


ClearAll[TextAMonPOSWordsTrie]

TextAMonPOSWordsTrie[___][None] := None;
TextAMonPOSWordsTrie[separator_String:"®"][xs_,context_] :=
    TextAMonBind[ TextAMon[xs,context], TextAMonTagWordsTrie[separator] ];


ClearAll[TextAMonComputeTagWordPairs]

Options[TextAMonComputeTagWordPairs] = {"SentenceToTagWordPairsFunction" -> "StandfordTagger"};

TextAMonComputeTagWordPairs[___][None] := None;
TextAMonComputeTagWordPairs[opts : OptionsPattern[]][xs_, context_] :=
    Block[{sentences, tagWordPairs, res, taggerFunc},

      taggerFunc = OptionValue[TextAMonComputeTagWordPairs, "SentenceToTagWordPairsFunction"];

      Which[
        TrueQ[taggerFunc == "StandfordTagger" || taggerFunc === Automatic],
        taggerFunc = (SeparatePOSTags[JavaStanfordTagString[ToLowerCase@#]] &),

        TrueQ[taggerFunc == "MetaphoneTagger" || taggerFunc == "Metaphone3Tagger"],
        taggerFunc = ({#, JavaMetaphone3[#]} & /@ TextWords[ToLowerCase@#] &),

        TrueQ[taggerFunc == "SoundexTagger"],
        taggerFunc = ({#, Soundex[#]} & /@ TextWords[ToLowerCase@#] &)
      ];

      Which[
        VectorQ[xs, StringQ],
        sentences = xs,

        KeyExistsQ[context, "sentences"],
        sentences = context["sentences"],

        True,
        Return[Fold[TextAMonBind, TextAMon[xs, context], {TextAMonSentences[], TextAMonComputeTagWordPairs[opts]}]];
      ];

      tagWordPairs = Map[taggerFunc, context["sentences"]];
      TextAMon[tagWordPairs, Join[context, <|"tagWordPairs" -> tagWordPairs|>]]
    ];


ClearAll[TextAMonTagWordsTrie]

TextAMonTagWordsTrie[___][None] := None;
TextAMonTagWordsTrie[separator_String: "®"][xs_, context_] :=
    Block[{jTagWordTrie},

      Which[

        KeyExistsQ[context, "tagWordPairs"],
        jTagWordTrie =
            JavaTrieNodeProbabilities@
                JavaTrieCreateBySplit[
                  Map[StringJoin[Riffle[#, separator]] &,
                    Reverse /@ Flatten[context["tagWordPairs"], 1]], separator];
        TextAMon[jTagWordTrie, Join[context, <|"jTagWordTrie" -> jTagWordTrie|>]],

        True,
        Echo["Calculate tag-to-words trie first.", "TextAMonTagWordsTrie:"];
        None
      ]
    ];



ClearAll[TextAMonEchoPOSWordsInterface];

Options[TextAMonEchoPOSWordsInterface] = {ImageSize->400, "ParetoFraction"->0.8, "ParetoApplicationThreshold"->300 };

TextAMonEchoPOSWordsInterface[___][None] := None;
TextAMonEchoPOSWordsInterface[ opts:OptionsPattern[] ][xs_, context_] :=
    Block[{ jTagWordTrie, allPosTags, tagSelectionRules, abbrTagRules, imSize, paretoFraction, paretoApplicationThreshold },

      imSize = OptionValue[TextAMonEchoPOSWordsInterface, ImageSize];
      If[ !IntegerQ[imSize],
        Echo["ImageSize should have values that are positive integers.", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];

      paretoFraction = OptionValue[TextAMonEchoPOSWordsInterface, "ParetoFraction"];
      If[ ! TrueQ[ NumberQ[paretoFraction] && 0 < paretoFraction <=1 ],
        Echo["ParetoFraction should have values that are numbers in (0,1] .", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];

      paretoApplicationThreshold = OptionValue[TextAMonEchoPOSWordsInterface, "ParetoApplicationThreshold"];
      If[ !IntegerQ[paretoApplicationThreshold],
        Echo["ParetoApplicationThreshold should have values that are positive integers.", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];

      If[ !KeyExistsQ[context, "tagWordPairs"],
        Echo["Calculate POS tags first.", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];

      If[ !KeyExistsQ[context, "jTagWordTrie"],
        Echo["Calculate jTagWordTrie first.", "TextAMonEchoPOSWordsInterface:"];
        Return[None]
      ];

      jTagWordTrie = context["jTagWordTrie"];

      allPosTags = Union[Flatten[context["tagWordPairs"], 1][[All, 2]]];
      Length[allPosTags];

      tagSelectionRules =
          Rule @@@ Quiet[
            Select[Map[{#, # <> " : " <> StringTrim[aStanfordPOSTagToAbbr[#]]} &, allPosTags], StringQ[#[[-1]]] &]
          ];
      abbrTagRules = Reverse /@ tagSelectionRules;

      Echo @
          With[{ jTagWordTrie=jTagWordTrie,
            imSize=imSize,
            paretoFraction=paretoFraction,
            paretoApplicationThreshold=paretoApplicationThreshold,
            abbrTagRules=abbrTagRules},
            Manipulate[
              DynamicModule[{posTag, jjTr, leafProbs, paretoLeafProbs},
                posTag = posAbbr /. abbrTagRules;
                jjTr = JavaTrieRetrieve[jTagWordTrie, {posTag}];
                leafProbs = {"key", "value"} /. JavaTrieLeafProbabilities[jjTr, "Normalized" -> True];
                If[ JavaTrieNodeCounts[jjTr]["leaves"] > paretoApplicationThreshold && paretoFraction < 1,
                  jjTr = JavaTrieParetoFractionRemove[jjTr,paretoFraction,True,"LONG_TAIL_WORDS"]
                ];
                paretoLeafProbs = {"key", "value"} /. JavaTrieLeafProbabilities[jjTr, "Normalized" -> True];
                Grid[{
                  Map[
                    Style[#, GrayLevel[0.5], FontFamily -> "Times"] &,
                    {"Pareto principle adherence", "Word probabilities for " <> posTag}
                  ],
                  {ParetoLawPlot[leafProbs[[All, 2]],
                    ImageSize -> Round[ 0.75*imSize ]],
                    Pane[
                      GridTableForm[SortBy[paretoLeafProbs, -#[[2]] &], TableHeadings -> {"Literal", "Probability"}],
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

        KeyExistsQ[context, "tagWordPairs"],
        words = context["tagWordPairs"][[All, All, 1]],

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
TextAMonMakeNGramTrie[ n_Integer, indexPermutation_:Automatic, separator_String:"~" ][xs_, context_] :=
    Block[{words, ngrams, jNGramTrie},

      If[ !(TrueQ[indexPermutation===Automatic] || VectorQ[indexPermutation,IntegerQ] && Sort[indexPermutation] == Range[n]),
        Echo["The argument indexOrder is expected to be Automatic or a variation of Range[n], where n is the first argument.", "TextAMonMakeNGramTrie:"];
        Return[None]
      ];

      Which[

        KeyExistsQ[context, "tagWordPairs"],
        words = context["tagWordPairs"][[All, All, 1]],

        KeyExistsQ[context, "text"],
        words = TextWords[context["text"]],

        True,
        Echo["Ingest text first.", "TextAMonMakeNGramTrie:"];
        Return[None]
      ];

      If[ TrueQ[indexPermutation===Automatic],
        ngrams = Map[StringJoin @@ Riffle[#, separator] &, Flatten[Partition[#, n, 1] & /@ words, 1]],
        ngrams = Map[StringJoin @@ Riffle[#[[indexPermutation]], separator] &, Flatten[Partition[#, n, 1] & /@ words, 1]]
      ];

      jNGramTrie = JavaTrieNodeProbabilities[JavaTrieCreateBySplit[ngrams, separator]];

      TextAMon[ jNGramTrie, context ]
    ];


(*End[] * `Private` *)

(*EndPackage[]*)