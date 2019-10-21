(*
    Monadic Latent Semantic Analysis Mathematica package
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

(* :Title: MonadicLatentSemanticAnalysis *)
(* :Context: MonadicLatentSemanticAnalysis` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-10-06 *)
(* Created with the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ . *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

  # Introduction

    This file (package) provides monad-like implementation for for the following Latent Semantic Analysis (LSA)
    main sequence of steps :

      1. ingesting a collection of documents;

      2. creating a document-term matrix (linear vector space representation);

      3. facilitating term-paragraph matrix creation or other breakdowns;

      4. apply different type of term-weighting functions;

      5. extract topics using NNMF (or SVD) with required parameters;

      6. provide topic interpretation;

      7. produce corresponding statistical thesauri;

      8. provide different statistics over the document collection.


  This monadic implementation is just a wrapper interface to the functions provided by the packages [1,2];
  those functions described in [3].


  # Usage example

      (* Get text data. *)
      speeches = ResourceData[ResourceObject["Presidential Nomination Acceptance Speeches"]];
      texts = Normal[speeches[[All, "Text"]]];

      (* Run the main processing pipeline. *)
      res =
        LSAMonUnit[texts]⟹
        LSAMonMakeDocumentTermMatrix[{}, stopWords]⟹
        LSAMonApplyTermWeightFunctions[]⟹
        LSAMonExtractTopics[5, 60, 12, "MaxSteps" -> 6, "PrintProfilingInfo" -> True];

      (* Show statistical thesaurus in two different ways. *)
      res⟹
        LSAMonExtractStatisticalThesaurus[{"arms", "banking", "economy", "education", "freedom", "tariff", "welfare"}, 6]⟹
        LSAMonRetrieveFromContext["statisticalThesaurus"]⟹
        LSAMonEchoValue⟹
        LSAMonEchoStatisticalThesaurusTable[];

  # References

    [1] Anton Antonov, Implementation of document-term matrix construction and re-weighting functions in Mathematica, (2013),
        MathematicaForPrediction at GitHub.
        https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m

    [2] Anton Antonov, Implementation of the Non-Negative Matrix Factorization algorithm in Mathematica, (2013),
        https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m

    [3] Anton Antonov, "Topic and thesaurus extraction from a document collection", (2013),
        MathematicaForPrediction at GitHub.
        https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf

  Anton Antonov
  2017-10-06

*)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)


If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Echo["StateMonadCodeGenerator.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];

If[Length[DownValues[SSparseMatrix`ToSSparseMatrix]] == 0,
  Echo["SSparseMatrix.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/SSparseMatrix.m"]
];

If[Length[DownValues[DocumentTermMatrixConstruction`DocumentTermMatrix]] == 0,
  Echo["DocumentTermMatrixConstruction.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/DocumentTermMatrixConstruction.m"]
];

If[Length[DownValues[NonNegativeMatrixFactorization`GDCLS]] == 0,
  Echo["NonNegativeMatrixFactorization.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/NonNegativeMatrixFactorization.m"]
];

If[Length[DownValues[IndependentComponentAnalysis`IndependentComponentAnalysis]] == 0,
  Echo["IndependentComponentAnalysis.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/IndependentComponentAnalysis.m"]
];

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Echo["CrossTabulate.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];

If[Length[DownValues[OutlierIdentifiers`OutlierPosition]] == 0,
  Echo["OutlierIdentifiers.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]
];

If[Length[DownValues[MathematicaForPredictionUtilities`RecordsSummary]] == 0,
  Echo["MathematicaForPredictionUtilities.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"]
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicLatentSemanticAnalysis`"];

$LSAMonFailure::usage = "Failure symbol for the monad LSAMon.";

LSAMonApplyTermWeightFunctions::usage = "Apply term weight functions to entries of the document-term matrix.";

LSAMonInterpretBasisVector::usage = "Interpret the a specified basis vector.";

LSAMonEchoStatisticalThesaurusTable::usage = "Echo the statistical thesaurus entries for a specified list of words.";

LSAMonEchoStatisticalThesaurus::usage = "Echo the statistical thesaurus entries for a specified list of words. \
Synonym of LSAMonEchoStatisticalThesaurusTable.";

LSAMonEchoDocumentsStatistics::usage = "Echo statistics for the text collection.";

LSAMonEchoDocumentTermMatrixStatistics::usage = "Echo document-term matrix statistics.";

LSAMonEchoTopicsTable::usage = "Echo the a table with the extracted topics.";

LSAMonGetDocuments::usage = "Get monad's document collection.";

LSAMonMakeDocumentTermMatrix::usage = "Make the document-term matrix.";

LSAMonMakeGraph::usage = "Make a graph of the document-term, document-document, or term-term relationships.";

LSAMonFindMostImportantDocuments::usage = "Find the most important texts in the text collection.";

LSAMonExtractStatisticalThesaurus::usage = "Extract the statistical thesaurus for specified list of words.";

LSAMonDocumentCollectionQ::usage = "Gives True if the argument is a text collection.";

LSAMonExtractTopics::usage = "Extract topics.";

LSAMonNormalizeMatrixProduct::usage = "LSAMonNormalizeMatrixProduct[ Normalized -> (Left|Right) ] \
normalize the matrix factors.";

LSAMonRepresentDocumentTagsByTopics::usage = "Find the topic representation corresponding to a list of tags. \
Each monad document is expected to have a tag. One tag might correspond to multiple documents.";

LSAMonRepresentByTerms::usage = "Find the terms representation of a matrix or a document.";

LSAMonRepresentByTopics::usage = "Find the topics representation of a matrix or a document.";

LSAMonMakeTopicsTable::usage = "Make a table of topics.";

LSAMonTakeTexts::usage = "Gives the value of the key \"texts\" from the monad context.";

LSAMonTakeMatrix::usage = "Gives SSparseMatrix object of the value of the key \"docTermMat\" from the monad context.";

LSAMonTakeWeightedMatrix::usage = "Gives SSparseMatrix object of the value of the key \"wDocTermMat\" from the monad context.";

FindMostImportantSentences::usage = "FindMostImportantSentences[sentences : ( _String | {_String ..} ), nTop_Integer : 5, opts : OptionsPattern[]] \
finds the most important sentences in a text or a list of sentences.";

Begin["`Private`"];

Needs["MathematicaForPredictionUtilities`"];
Needs["StateMonadCodeGenerator`"];
Needs["SSparseMatrix`"];
Needs["DocumentTermMatrixConstruction`"];
Needs["NonNegativeMatrixFactorization`"];
Needs["IndependentComponentAnalysis`"];
Needs["CrossTabulate`"];
Needs["OutlierIdentifiers`"];


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of LSAMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicLatentSemanticAnalysis`LSAMon", "FailureSymbol" -> $LSAMonFailure, "StringContextNames" -> False ];

(**************************************************************)
(* Setters and takers                                         *)
(**************************************************************)

GenerateMonadAccessors[
  "MonadicLatentSemanticAnalysis`LSAMon",
  {"documents", "terms", "documentTermMatrix", "weightedDocumentTermMatrix",
    "stemmingRules", "stopWords",
    "globalWeights", "globalWeightFunction", "localWeightFunction", "normalizerFunction",
    "topicColumnPositions", "automaticTopicNames", "statisticalThesaurus", "topicsTable", "method" },
  "FailureSymbol" -> $LSAMonFailure ];

GenerateMonadAccessors[
  "MonadicLatentSemanticAnalysis`LSAMon",
  {"W", "H" },
  "FailureSymbol" -> $LSAMonFailure, "DecapitalizeElementName" -> False ];

Clear[LSAMonTakeMatrix, LSAMonTakeWeightedMatrix];

LSAMonTakeMatrix = LSAMonTakeDocumentTermMatrix;

LSAMonTakeWeightedMatrix = LSAMonTakeWeightedDocumentTermMatrix;


(**************************************************************)
(* Set document-term matrix                                   *)
(**************************************************************)

(* Here we change the definition made with GenerateMonadAccessors. *)
Clear[LSAMonSetDocumentTermMatrix];

LSAMonSetDocumentTermMatrix[$LSAMonFailure] := $LSAMonFailure;

LSAMonSetDocumentTermMatrix[][xs_, context_] := $LSAMonFailure;

LSAMonSetDocumentTermMatrix[ smat_SSparseMatrix ][xs_, context_Association] :=
    LSAMonUnit[ xs, Join[ context, <| "documentTermMatrix" -> smat |> ] ];

LSAMonSetDocumentTermMatrix[ mat_?MatrixQ ][xs_, context_] :=
    Block[{smat},
      smat =
          ToSSparseMatrix[
            SparseArray[mat],
            "RowNames" -> Map[ToString, Range[Dimensions[mat][[1]]]],
            "ColumnNames" -> Map[ToString, Range[Dimensions[mat][[2]]]]
          ];

      LSAMonSetDocumentTermMatrix[smat][xs, context]
    ];

LSAMonSetDocumentTermMatrix[___][xs_, context_Association] :=
    Block[{},
      Echo[ "The argument is expected to be a matrix or a SSparseMatrix object.", "LSAMonSetDocumentTermMatrix:"];
      $LSAMonFailure
    ];


(**************************************************************)
(* Set document-term matrix                                   *)
(**************************************************************)

(* Here we change the definition made with GenerateMonadAccessors. *)
Clear[LSAMonSetWeightedDocumentTermMatrix];

LSAMonSetWeightedDocumentTermMatrix[$LSAMonFailure] := $LSAMonFailure;

LSAMonSetWeightedDocumentTermMatrix[][xs_, context_] := $LSAMonFailure;

LSAMonSetWeightedDocumentTermMatrix[ smat_SSparseMatrix ][xs_, context_Association] :=
    LSAMonUnit[ xs, Join[ context, <| "weightedDocumentTermMatrix" -> smat |> ] ];

LSAMonSetWeightedDocumentTermMatrix[ mat_?MatrixQ ][xs_, context_] :=
    Block[{smat},
      smat =
          ToSSparseMatrix[
            SparseArray[mat],
            "RowNames" -> Map[ToString, Range[Dimensions[mat][[1]]]],
            "ColumnNames" -> Map[ToString, Range[Dimensions[mat][[2]]]]
          ];

      LSAMonSetWeightedDocumentTermMatrix[smat][xs, context]
    ];

LSAMonSetWeightedDocumentTermMatrix[___][xs_, context_Association] :=
    Block[{},
      Echo[ "The argument is expected to be a matrix or a SSparseMatrix object.", "LSAMonSetWeightedDocumentTermMatrix:"];
      $LSAMonFailure
    ];


(**************************************************************)
(* Get texts                                                  *)
(**************************************************************)

Clear[LSAMonDocumentCollectionQ];
LSAMonDocumentCollectionQ[x_] := AssociationQ[x] && VectorQ[ Values[x], StringQ ];

Clear[LSAMonGetDocuments];

LSAMonGetDocuments[$LSAMonFailure] := $LSAMonFailure;

LSAMonGetDocuments[][xs_, context_] := LSAMonGetDocuments[xs, context];

LSAMonGetDocuments[xs_, context_] :=
    Block[{texts},

      Which[

        KeyExistsQ[context, "documents"] && LSAMonDocumentCollectionQ[ context["documents"] ],
        LSAMonUnit[ context["documents"], context],

        KeyExistsQ[context, "documents"] && VectorQ[ context["documents"], StringQ ],
        texts = ToAutomaticKeysAssociation[context["documents"]];
        LSAMonUnit[ texts, context],

        LSAMonDocumentCollectionQ[xs],
        LSAMonUnit[xs, context],

        VectorQ[xs, StringQ],
        texts = ToAutomaticKeysAssociation[xs];
        LSAMonUnit[ texts, context],

        True,
        Echo["Cannot find documents.", "LSAMonGetDocuments:"];
        $LSAMonFailure
      ]

    ];

LSAMonGetDocuments[___][xs_, context_Association] := $LSAMonFailure;


(**************************************************************)
(* General functions                                          *)
(**************************************************************)

(*------------------------------------------------------------*)
(* Make document-term matrix                                  *)
(*------------------------------------------------------------*)
Clear[LSAMonMakeDocumentTermMatrix];

Options[LSAMonMakeDocumentTermMatrix] = { "StemmingRules" -> {}, "StopWords" -> Automatic };

LSAMonMakeDocumentTermMatrix[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonMakeDocumentTermMatrix[xs_, context_Association] := LSAMonMakeDocumentTermMatrix[][xs, context];

LSAMonMakeDocumentTermMatrix[][xs_, context_Association] := LSAMonMakeDocumentTermMatrix[ {}, Automatic ][xs, context];

LSAMonMakeDocumentTermMatrix[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ stemRules, stopWords },

      stemRules = OptionValue[ LSAMonMakeDocumentTermMatrix, "StemmingRules" ];

      If[ ! ( AssociationQ[stemRules] || DispatchQ[stemRules] || MatchQ[ stemRules, {_Rule...} ] || TrueQ[ stemRules === Automatic ] ),
        Echo[
          "The value of the option \"StemmingRules\" is expected to be a list or rules, dispatch table, an association, or Automatic.",
          "LSAMonMakeDocumentTermMatrix:"
        ];
        Return[$LSAMonFailure]
      ];

      stopWords = OptionValue[ LSAMonMakeDocumentTermMatrix, "StopWords" ];

      If[ ! ( MatchQ[ stopWords, {_String...} ] || TrueQ[ stopWords === Automatic ] ),
        Echo[
          "The value of the option \"StopWords\" is expected to be a list or strings or Automatic.",
          "LSAMonMakeDocumentTermMatrix:"
        ];
        Return[$LSAMonFailure]
      ];

      LSAMonMakeDocumentTermMatrix[ stemRules, stopWords ][xs, context]
    ];

LSAMonMakeDocumentTermMatrix[stemRulesArg : ({ Rule[_String, _String] ... } | _Dispatch | _Association | Automatic), stopWordsArg : {_String ...} | Automatic ][xs_, context_] :=
    Block[{ stemRules = stemRulesArg, stopWords = stopWordsArg, docs, docTermMat },

      docs = Fold[ LSAMonBind, LSAMonUnit[xs, context], { LSAMonGetDocuments, LSAMonTakeValue } ];

      If[ TrueQ[docs === $LSAMonFailure],
        Echo["Ingest texts first.", "LSMonMakeDocumentTermMatrix:"];
        Return[$LSAMonFailure]
      ];

      If[ TrueQ[ stopWords === Automatic ],
        stopWords = DictionaryLookup["*"];
        stopWords = Complement[stopWords, DeleteStopwords[stopWords]];
      ];

      docTermMat = DocumentTermSSparseMatrix[ ToLowerCase /@ docs, {stemRules, stopWords} ];

      stemRules =
          Which[
            MatchQ[ stemRules, { Rule[_, _] .. } ],
            Association[ stemRules ],

            MatchQ[ stemRules, _Dispatch ],
            Association[ Normal[stemRules] ],

            True,
            stemRules
          ];

      LSAMonUnit[xs, Join[context, <| "documents" -> docs, "documentTermMatrix" -> docTermMat, "terms" -> ColumnNames[docTermMat], "stopWords" -> stopWords, "stemmingRules" -> stemRules |>]]

    ];

LSAMonMakeDocumentTermMatrix[__][___] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonMakeDocumentTermMatrix[stemRules : ( { Rule[_String,_String] .. } | _Dispatch | _Association ), stopWords : { _String ... } ] .",
        "LSAMonMakeDocumentTermMatrix:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Apply term weight function to matrix entries               *)
(*------------------------------------------------------------*)

Clear[LSAMonApplyTermWeightFunctions];

Options[LSAMonApplyTermWeightFunctions] = { "GlobalWeightFunction" -> "IDF", "LocalWeightFunction" -> "None", "NormalizerFunction" -> "Cosine" };

LSAMonApplyTermWeightFunctions[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonApplyTermWeightFunctions[xs_, context_Association] := LSAMonApplyTermWeightFunctions[][xs, context];

LSAMonApplyTermWeightFunctions[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ termFuncs, val },

      termFuncs =
          Table[
            (
              val = OptionValue[ LSAMonApplyTermWeightFunctions, funcName ];

              If[ ! StringQ[val],
                Echo[
                  "The value of the option \"" <> funcName <> "\" is expected to be a string.",
                  "LSAMonApplyTermWeightFunctions:"
                ];
                Return[$LSAMonFailure]
              ];

              val
            ),
            { funcName, { "GlobalWeightFunction", "LocalWeightFunction", "NormalizerFunction" } }
          ];

      LSAMonApplyTermWeightFunctions[ Sequence @@ termFuncs ][xs, context]
    ];

LSAMonApplyTermWeightFunctions[globalWeightFunction_String, localWeightFunction_String, normalizerFunction_String][xs_, context_] :=
    Block[{wDocTermMat, globalWeights, aSpec },

      Which[
        KeyExistsQ[context, "documentTermMatrix"] && SSparseMatrixQ[context["documentTermMatrix"]],

        globalWeights =
            AssociationThread[
              ColumnNames[context["documentTermMatrix"]],
              GlobalTermFunctionWeights[ SparseArray[context["documentTermMatrix"]], globalWeightFunction ]
            ];

        wDocTermMat = WeightTermsOfSSparseMatrix[context["documentTermMatrix"], Values[globalWeights], localWeightFunction, normalizerFunction];

        aSpec = AssociationThread[ {"globalWeightFunction", "localWeightFunction", "normalizerFunction"}, {globalWeightFunction, localWeightFunction, normalizerFunction}];

        LSAMonUnit[xs, Join[context, <|"weightedDocumentTermMatrix" -> wDocTermMat, "globalWeights" -> globalWeights |>, aSpec]],

        !KeyExistsQ[context, "documentTermMatrix"],
        Echo["No document-term matrix.", "LSAMonApplyTermWeightFunctions:"];
        $LSAMonFailure,

        True,
        Echo["The document-term matrix is not a SSparseMatrix object.", "LSAMonApplyTermWeightFunctions:"];
        $LSAMonFailure
      ]

    ];

LSAMonApplyTermWeightFunctions[__][___] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonApplyTermWeightFunctions[globalWeightFunction_String, localWeightFunction_String, normalizerFunction_String] .",
        "LSAMonApplyTermWeightFunctions:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Make document-term matrix                                  *)
(*------------------------------------------------------------*)

Clear[LSAMonExtractTopics];

Options[LSAMonExtractTopics] =
    Join[
      { "NumberOfTopics" -> None, Method -> "NNMF", "MinNumberOfDocumentsPerTerm" -> 10, "NumberOfInitializingDocuments" -> 12, Tolerance -> 10^-6  },
      Options[GDCLSGlobal]
    ];

LSAMonExtractTopics[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonExtractTopics[$LSAMonFailure] := $LSAMonFailure;

LSAMonExtractTopics[xs_, context_Association] := $LSAMonFailure;

(*LSAMonExtractTopics[nTopics_Integer, nMinDocumentsPerTerm_Integer, nInitializingDocuments_Integer, opts : OptionsPattern[]][xs_, context_] :=*)
(*    LSAMonExtractTopics[ nTopics, Join[ { "MinNumberOfDocumentsPerTerm" -> nMinDocumentsPerTerm, "NumberOfInitializingDocuments" -> nInitializingDocuments }, {opts}] ][xs, context];*)

LSAMonExtractTopics[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{nTopics},

      nTopics = OptionValue[ LSAMonExtractTopics, "NumberOfTopics" ];

      If[ ! IntegerQ[nTopics],
        Echo[
          "The value of the option \"NumberOfTopics\" is expected to be a integer.",
          "LSAMonExtractTopics:"
        ];
        Return[$LSAMonFailure]
      ];

      LSAMonExtractTopics[ nTopics, opts ][xs, context]
    ];

LSAMonExtractTopics[ nTopics_Integer, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{method, nMinDocumentsPerTerm, nInitializingDocuments,
      docTermMat, documentsPerTerm, pos, W, H, M1, k, p, m, n, U, S, V, nnmfOpts, terms, automaticTopicNames },

      method = OptionValue[ LSAMonExtractTopics, Method ];

      If[ StringQ[method], method = ToLowerCase[method]];

      If[ TrueQ[ MemberQ[ {SingularValueDecomposition, ToLowerCase["SingularValueDecomposition"], ToLowerCase["SVD"] }, method ] ], method = "SVD" ];

      If[ TrueQ[ MemberQ[ ToLowerCase[ { "NNMF", "NMF", "NonNegativeMatrixFactorization" } ], method ] ], method = "NNMF" ];

      If[ TrueQ[ MemberQ[ ToLowerCase[ { "IndependentComponentAnalysis", "ICA" } ], method ] ], method = "ICA" ];

      If[ !MemberQ[ {"SVD", "NNMF", "ICA"}, method ],
        Echo["The value of the option Method is expected to be \"SVD\", \"NNMF\", or \"ICA\".", "LSAMonExtractTopics:"];
        Return[$LSAMonFailure]
      ];

      nMinDocumentsPerTerm = OptionValue[ LSAMonExtractTopics, "MinNumberOfDocumentsPerTerm" ];
      If[ ! ( IntegerQ[ nMinDocumentsPerTerm ] && nMinDocumentsPerTerm >= 0 ),
        Echo["The value of the option \"MinDocumentsPerTerm\" is expected to be a non-negative integer.", "LSAMonExtractTopics:"];
        Return[$LSAMonFailure]
      ];

      nInitializingDocuments = OptionValue[ LSAMonExtractTopics, "NumberOfInitializingDocuments" ];
      If[ ! ( IntegerQ[ nInitializingDocuments ] && nInitializingDocuments > 0 ),
        Echo["The value of the option \"NumberOfInitializingDocuments\" is expected to be a positive integer.", "LSAMonExtractTopics:"];
        Return[$LSAMonFailure]
      ];

      If[ !KeyExistsQ[context, "weightedDocumentTermMatrix"],
        Return[
          Fold[
            LSAMonBind,
            LSAMonUnit[xs, context],
            {
              LSAMonApplyTermWeightFunctions[],
              LSAMonExtractTopics[
                nTopics,
                Join[ { Method -> method, "MinNumberOfDocumentsPerTerm" -> nMinDocumentsPerTerm, "NumberOfInitializingDocuments" -> nInitializingDocuments }, {opts}]
              ]
            }
          ]
        ]
      ];

      If[ nMinDocumentsPerTerm > RowsCount[context["weightedDocumentTermMatrix"]],
        Echo[
          "The value of the option \"MinDocumentsPerTerm\" is expected not to be larger than the number of rows of the weighted document-term matrix.",
          "LSAMonExtractTopics:"];
        Return[$LSAMonFailure]
      ];

      (* Restrictions *)
      docTermMat = Unitize[ SparseArray[ context["weightedDocumentTermMatrix"] ] ];

      documentsPerTerm = Total /@ Transpose[docTermMat];
      pos = Flatten[Position[documentsPerTerm, s_?NumberQ /; s >= nMinDocumentsPerTerm]];

      If[ Length[pos] == 0,
        Echo[
          "The value of the option \"MinDocumentsPerTerm\" produced an empty selection of terms (columns of the weighted document-term matrix.)",
          "LSAMonExtractTopics:"];
        Return[$LSAMonFailure]
      ];

      M1 = SparseArray[ context["weightedDocumentTermMatrix"][[All, pos]] ];

      (* Factorization *)
      Which[

        (* Non-negative matrix factorization *)
        method == "NNMF" && KeyExistsQ[context, "weightedDocumentTermMatrix"] && SSparseMatrixQ[context["weightedDocumentTermMatrix"]],

        {k, p} = {nTopics, nInitializingDocuments};
        {m, n} = Dimensions[M1];
        M1 = Transpose[M1];
        M1 = Map[# &, M1];
        H = ConstantArray[0, {k, n}];
        W = Table[Total[RandomSample[M1, p]], {k}];
        Do[
          W[[i]] = W[[i]] / Norm[W[[i]]];
          , {i, 1, Length[W]}];
        W = Transpose[W];
        M1 = SparseArray[M1];
        M1 = Transpose[M1];

        W = SparseArray[W];
        H = SparseArray[H];

        nnmfOpts = FilterRules[ {opts}, Options[GDCLSGlobal] ];
        If[ TrueQ[ ("MaxSteps" /. nnmfOpts) === Automatic ],
          nnmfOpts = Prepend[ nnmfOpts, "MaxSteps" -> 12 ];
        ];

        {W, H} = GDCLSGlobal[M1, W, H, Evaluate[ nnmfOpts ] ],

        (* Singular Value Decomposition *)
        method == "SVD" && KeyExistsQ[context, "weightedDocumentTermMatrix"] && SSparseMatrixQ[context["weightedDocumentTermMatrix"]],

        {U, S, V} = SingularValueDecomposition[ M1, nTopics, DeleteCases[ FilterRules[ {opts}, Options[SingularValueDecomposition] ], Method -> _ ]];

        (* Re-fit the result to monad's data interpretation. *)
        W = SparseArray[U];
        H = Transpose[V];
        H = S . H,

        !KeyExistsQ[context, "weightedDocumentTermMatrix"],
        Echo["Cannot find a weighted document-term matrix.", "LSAMonExtractTopics:"];
        Return[$LSAMonFailure],

        (* Independent Component Analysis *)
        method == "ICA" && KeyExistsQ[context, "weightedDocumentTermMatrix"] && SSparseMatrixQ[context["weightedDocumentTermMatrix"]],

        {H, W} = IndependentComponentAnalysis[ Transpose[M1], nTopics, DeleteCases[ FilterRules[ {opts}, Options[IndependentComponentAnalysis] ], Method -> _ ]];

        (* Re-fit the result to monad's data interpretation. *)
        W = Transpose[SparseArray[W]];
        H = Transpose[SparseArray[H]],

        !KeyExistsQ[context, "weightedDocumentTermMatrix"],
        Echo["Cannot find a weighted document-term matrix.", "LSAMonExtractTopics:"];
        Return[$LSAMonFailure],

        (* No matrix. *)
        True,
        Echo["The weighted document-term matrix is not a SSparseMatrix object.", "LSAMonExtractTopics:"];
        Return[$LSAMonFailure]
      ];

      terms = ColumnNames[context["weightedDocumentTermMatrix"]];
      automaticTopicNames =
          Table[
            StringJoin[Riffle[BasisVectorInterpretation[Normal@H[[ind]], 3, terms[[pos]]][[All, 2]], "-"]],
            {ind, 1, Dimensions[W][[2]]}];

      If[ ! DuplicateFreeQ[automaticTopicNames],
        automaticTopicNames = MapIndexed[ #1 <> "-" <> ToString[#2]&, automaticTopicNames ];
      ];

      W = ToSSparseMatrix[ SparseArray[W], "RowNames" -> RowNames[context["weightedDocumentTermMatrix"]], "ColumnNames" -> automaticTopicNames ];
      H = ToSSparseMatrix[ SparseArray[H], "RowNames" -> automaticTopicNames, "ColumnNames" -> ColumnNames[context["weightedDocumentTermMatrix"]][[pos]] ];

      LSAMonUnit[xs,
        Join[context, <|
          "W" -> W, "H" -> H, "topicColumnPositions" -> pos,
          "automaticTopicNames" -> automaticTopicNames, "terms" -> terms,
          "method" -> method |>]]

    ];

LSAMonExtractTopics[___][__] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonExtractTopics[ nTopics_Integer, opts___] .",
        "LSAMonExtractTopics::"];
      $LSAMonFailure
    ];

LSAMonTopicExtraction = LSAMonExtractTopics;


(*------------------------------------------------------------*)
(* Extract statistical thesaurus                                 *)
(*------------------------------------------------------------*)

Clear[LSAMonExtractStatisticalThesaurus];

Options[LSAMonExtractStatisticalThesaurus] = { "Words" -> None, "NumberOfNearestNeighbors" -> 12 };

LSAMonExtractStatisticalThesaurus[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonExtractStatisticalThesaurus[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{words, numberOfNNs},

      words = OptionValue[ LSAMonExtractStatisticalThesaurus, "Words" ];

      If[ ! MatchQ[ words, {_String..} ],
        Echo[
          "The value of the option \"Words\" is expected to be a list of strings.",
          "LSAMonExtractStatisticalThesaurus:"
        ];
        Return[$LSAMonFailure]
      ];

      numberOfNNs = OptionValue[ LSAMonExtractStatisticalThesaurus, "NumberOfNearestNeighbors" ];

      If[ ! MatchQ[ words, {_String..} ],
        Echo[
          "The value of the option \"Words\" is expected to be a list of strings.",
          "LSAMonExtractStatisticalThesaurus:"
        ];
        Return[$LSAMonFailure]
      ];

      LSAMonExtractStatisticalThesaurus[ words, numberOfNNs ][xs, context]
    ];

LSAMonExtractStatisticalThesaurus[word_String, numberOfNNs_Integer][xs_, context_Association] :=
    LSAMonExtractStatisticalThesaurus[{word}, numberOfNNs][xs, context];

LSAMonExtractStatisticalThesaurus[words : {_String ..}, numberOfNNs_Integer][xs_, context_Association] :=
    Block[{W, H, HNF, thRes},

      Which[
        KeyExistsQ[context, "H"] && KeyExistsQ[context, "W"],

        {W, H} = NormalizeMatrixProduct[ SparseArray[context["W"]], SparseArray[context["H"]] ];

        HNF = Nearest[Range[Dimensions[H][[2]]], DistanceFunction -> (Norm[H[[All, #1]] - H[[All, #2]]] &)];

        thRes =
            Association[
              Map[
                # -> NearestWords[HNF, #, ColumnNames[context["H"]], {}, numberOfNNs] &,
                Sort[words]]
            ];

        LSAMonUnit[thRes, Join[context, <|"statisticalThesaurus" -> thRes|>]],

        True,
        Echo["No factorization of the document-term matrix is made.", "LSAMonExtractStatisticalThesaurus:"];
        $LSAMonFailure
      ]
    ];

LSAMonExtractStatisticalThesaurus[___][__] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonExtractStatisticalThesaurus[words : {_String ..}, numberOfNNs_Integer] .",
        "LSAMonExtractStatisticalThesaurus::"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Echo statistical thesaurus                                 *)
(*------------------------------------------------------------*)

Clear[LSAMonEchoStatisticalThesaurusTable];

Options[LSAMonEchoStatisticalThesaurusTable] = Options[LSAMonExtractStatisticalThesaurus];

LSAMonEchoStatisticalThesaurusTable[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoStatisticalThesaurusTable[xs_, context_Association] := LSAMonEchoStatisticalThesaurusTable[][xs, context];

LSAMonEchoStatisticalThesaurusTable[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{words},

      words = OptionValue[ LSAMonEchoStatisticalThesaurusTable, "Words" ];

      Which[

        !TrueQ[ words === None ],
        Fold[ LSAMonBind, LSAMonUnit[xs, context], { LSAMonExtractStatisticalThesaurus[opts], LSAMonEchoStatisticalThesaurusTable } ],

        TrueQ[ words === None ] && KeyExistsQ[context, "statisticalThesaurus"],
        Echo[
          Grid[
            Prepend[
              List @@@ Normal[ context["statisticalThesaurus"] ],
              Style[#, Blue, FontFamily -> "Times"] & /@ {"term", "statistical thesaurus entries"}],
            Dividers -> All, Alignment -> Left,
            Spacings -> {Automatic, 0.75}],
          "statistical thesaurus:"
        ];
        LSAMonUnit[xs, context],

        True  ,
        Echo["No statistical thesaurus is computed.", "LSAMonEchoStatisticalThesaurusTable:"];
        $LSAMonFailure
      ]
    ];

LSAMonEchoStatisticalThesaurusTable[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "LSAMonEchoStatisticalThesaurusTable:"];
      $LSAMonFailure
    ];


Clear[LSAMonEchoStatisticalThesaurus];

LSAMonEchoStatisticalThesaurus = LSAMonEchoStatisticalThesaurusTable;


(*------------------------------------------------------------*)
(* Basis vector interpretation                                *)
(*------------------------------------------------------------*)

Clear[LSAMonInterpretBasisVector];

Options[LSAMonInterpretBasisVector] = { "NumberOfTerms" -> 12 };

LSAMonInterpretBasisVector[___][$LSAMonFailure] := $LSAMonFailure;
LSAMonInterpretBasisVector[vectorIndices : (_Integer | {_Integer..}), opts : OptionsPattern[] ][xs_, context_] :=
    Block[{W, H, res, numberOfTerms},

      numberOfTerms = OptionValue[LSAMonInterpretBasisVector, "NumberOfTerms"];

      {W, H} = RightNormalizeMatrixProduct[ SparseArray[context["W"]], SparseArray[context["H"]] ];

      res =
          Map[
            BasisVectorInterpretation[#, numberOfTerms, ColumnNames[context["H"]] ]&,
            Normal @ H[[ Flatten @ {vectorIndices} ]]
          ];

      If[ !MatchQ[res, {{{_?NumberQ, _String}..}..}],
        $LSAMonFailure,
        LSAMonUnit[ res, context ]
      ]

    ];

LSAMonInterpretBasisVector[___][__] :=
    Block[{},
      Echo[
        "The expected arguments are LSAMonInterpretBasisVector[vectorIndices:(_Integer|{_Integer..}), opts___] .",
        "LSAMonInterpretBasisVector:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Topics table making                                        *)
(*------------------------------------------------------------*)

Clear[LSAMonMakeTopicsTable];

Options[LSAMonMakeTopicsTable] = { "NumberOfTerms" -> 12 };

LSAMonMakeTopicsTable[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonMakeTopicsTable[xs_, context_Association] := LSAMonMakeTopicsTable[][xs, context];

LSAMonMakeTopicsTable[opts : OptionsPattern[]][xs_, context_] :=
    Block[{topicsTbl, k, numberOfTerms},

      numberOfTerms = OptionValue["NumberOfTerms"];

      k = Dimensions[context["W"]][[2]];

      topicsTbl =
          Table[
            TableForm[{NumberForm[#[[1]] / t[[1, 1]], {4, 3}], #[[2]]} & /@ t],
            {t, First @ LSAMonInterpretBasisVector[Range[k], "NumberOfTerms" -> numberOfTerms][xs, context] }];

      LSAMonUnit[ topicsTbl, Join[ context, <| "topicsTable" -> topicsTbl|> ] ]
    ];

LSAMonMakeTopicsTable[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonMakeTopicsTable:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Topics table echoing                                       *)
(*------------------------------------------------------------*)

Clear[LSAMonEchoTopicsTable];

Options[LSAMonEchoTopicsTable] = Join[
  {"NumberOfTableColumns" -> Automatic, "NumberOfTerms" -> 12 , "MagnificationFactor" -> Automatic},
  Options[Multicolumn] ];

LSAMonEchoTopicsTable[$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoTopicsTable[ opts : OptionsPattern[] ][$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoTopicsTable[xs_, context_Association] := LSAMonEchoTopicsTable[][xs, context];

LSAMonEchoTopicsTable[][xs_, context_Association] := LSAMonEchoTopicsTable[Options[LSAMonEchoTopicsTable]][xs, context];

LSAMonEchoTopicsTable[opts : OptionsPattern[]][xs_, context_] :=
    Block[{topicsTbl, k, numberOfTableColumns, numberOfTerms, mFactor, tOpts},

      numberOfTableColumns = OptionValue[LSAMonEchoTopicsTable, "NumberOfTableColumns"];

      numberOfTerms = OptionValue[LSAMonEchoTopicsTable, "NumberOfTerms"];

      mFactor = OptionValue[LSAMonEchoTopicsTable, "MagnificationFactor"];
      If[ TrueQ[mFactor === Automatic], mFactor = 0.8 ];

      k = Dimensions[context["W"]][[2]];

      If[ KeyExistsQ[context, "topicsTable"],
        topicsTbl = context["topicsTable"],
        (*ELSE*)
        topicsTbl = First @ LSAMonMakeTopicsTable["NumberOfTerms" -> numberOfTerms][xs, context]
      ];

      tOpts = Join[ FilterRules[ {opts}, Options[Multicolumn] ], {Dividers -> All, Alignment -> Left} ];

      Echo[
        Magnify[#, mFactor] & @
            If[ TrueQ[numberOfTableColumns === Automatic],
              Multicolumn[
                ColumnForm /@ Transpose[{Style[#, Red] & /@ Range[k], topicsTbl}], tOpts],
              (* ELSE *)
              Multicolumn[
                ColumnForm /@ Transpose[{Style[#, Red] & /@ Range[k], topicsTbl}], numberOfTableColumns, tOpts]
            ],
        "topics table:"
      ];

      LSAMonUnit[ topicsTbl, context ]
    ];

LSAMonEchoTopicsTable[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonEchoTopicsTable:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Topics representation of tags                              *)
(*------------------------------------------------------------*)

Clear[LSAMonRepresentDocumentTagsByTopics];

Options[LSAMonRepresentDocumentTagsByTopics] = { "ComputeTopicRepresentation" -> True, "PreserveTagsOrder" -> True };

LSAMonRepresentDocumentTagsByTopics[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonRepresentDocumentTagsByTopics[xs_, context_Association] := LSAMonRepresentDocumentTagsByTopics[][xs, context];

LSAMonRepresentDocumentTagsByTopics[][xs_, context_] :=
    LSAMonRepresentDocumentTagsByTopics[Automatic, "ComputeTopicRepresentation" -> True][xs, context];

LSAMonRepresentDocumentTagsByTopics[tags : (Automatic | _List), opts : OptionsPattern[]][xs_, context_] :=
    Block[{computeTopicRepresentationQ, preserveTagsOrderQ, ctTags, W, H, docTopicIndices, ctMat },

      computeTopicRepresentationQ = TrueQ[ OptionValue[LSAMonRepresentDocumentTagsByTopics, "ComputeTopicRepresentation"] ];
      preserveTagsOrderQ = TrueQ[ OptionValue[LSAMonRepresentDocumentTagsByTopics, "PreserveTagsOrder"] ];

      If[ ! ( KeyExistsQ[context, "documentTermMatrix"] && KeyExistsQ[context, "W"] ),
        Echo["No document-term matrix factorization is computed.", "LSAMonRepresentDocumentTagsByTopics:"];
        Return[$LSAMonFailure]
      ];

      Which[

        TrueQ[tags === Automatic] && KeyExistsQ[context, "docTags"],
        ctTags = context["docTags"],

        TrueQ[tags === Automatic],
        (* Probably using ctTags = RowNames[context["W"]] is better, but using "documentTermMatrix" for clarity. *)
        ctTags = RowNames[context["documentTermMatrix"]],

        Length[tags] == Dimensions[context["documentTermMatrix"]][[1]],
        ctTags = tags,

        True,
        Echo["The length of the argument tags is expected to be same as the number of rows of the document-term matrix.",
          "LSAMonRepresentDocumentTagsByTopics:"];
        Return[$LSAMonFailure]
      ];

      {W, H} = NormalizeMatrixProduct[ SparseArray[context["W"]], SparseArray[context["H"]] ];
      W = Clip[W, {0.01, 1}, {0, 1}];

      If[ computeTopicRepresentationQ || !KeyExistsQ[context, "docTopicIndices"],

        (* This is expected to be fairly quick, less than 1 second. *)
        (* If not, some sort of memoization has to be used, which will require consistency support. *)
        (* Using the option "ComputeTopicRepresentation" comes from those computation management concerns. *)
        docTopicIndices =
            Block[{v = Select[#, # > 0 &], vpos, ts1, ts2},
              vpos = Flatten@Position[#, x_ /; x > 0];
              ts1 =
                  OutlierPosition[v,
                    TopOutliers @* SPLUSQuartileIdentifierParameters];
              ts2 =
                  OutlierPosition[v, TopOutliers @* HampelIdentifierParameters];
              Which[
                Length[ts1] > 0, vpos[[ts1]],
                Length[ts2] > 0, vpos[[ts2]],
                True, vpos
              ]
            ] & /@ W,
        (* ELSE *)
        docTopicIndices = context["docTopicIndices"]
      ];

      (* Note that CrossTabulate is going to sort the matrix rows. *)
      (* The matrix rows correspond to the union of the tags. *)
      ctMat = CrossTabulate[ Flatten[MapThread[Thread[{#1, #2}] &, {ctTags, docTopicIndices}], 1]];
      ctMat = Join[ ctMat, <| "ColumnNames" -> context["automaticTopicNames"][[ ctMat["ColumnNames"] ]] |> ];
      ctMat = ToSSparseMatrix[ ctMat ];

      (* This should be done better. *)
      If[ preserveTagsOrderQ,
        ctTags = DeleteDuplicates[ctTags];
        ctTags = Pick[ ctTags, RowNamesAssociation[ctMat] /@ ctTags, _Integer ];
        ctMat = ctMat[[ ctTags, All ]]
      ];

      LSAMonUnit[ ctMat, Join[ context, <| "docTopicIndices" -> docTopicIndices |> ] ]

    ];

LSAMonRepresentDocumentTagsByTopics[__][___] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonRepresentDocumentTagsByTopics[tags:(Automatic|_List), opts___] .",
        "LSAMonRepresentDocumentTagsByTopics:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Terms representation                                      *)
(*------------------------------------------------------------*)

Clear[QueryPatternQ];
QueryPatternQ[arg_] :=
    MatchQ[ arg,
      {_String .. } |
          { {_String.. } .. } |
          Association[ (_ -> _String) .. ] |
          Association[ (_ -> {_String.. }) ..] ];


Clear[LSAMonRepresentByTerms];

Options[LSAMonRepresentByTerms] = { "ApplyTermWeightFunctions" -> True };

LSAMonRepresentByTerms[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonRepresentByTerms[xs_, context_Association] := $LSAMonFailure;

LSAMonRepresentByTerms[][xs_, context_] := $LSAMonFailure;

LSAMonRepresentByTerms[ query_String, opts : OptionsPattern[] ][xs_, context_] :=
    LSAMonRepresentByTerms[ {query}, opts ][xs, context];

LSAMonRepresentByTerms[ query_?QueryPatternQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{qmat, stopWords = {}, stemmingRules = {} },

      (* This should be optimized. *)
      If[ KeyExistsQ[context, "stopWords" ],
        stopWords = context["stopWords"]
      ];

      If[ KeyExistsQ[context, "stemmingRules" ],
        stemmingRules = context["stemmingRules"]
      ];

      qmat = DocumentTermSSparseMatrix[ ToLowerCase /@ query, {stemmingRules, stopWords} ];

      If[ Total[ SparseArray[qmat] ] == 0,
        Echo["All query terms are stop words.", "LSAMonRepresentByTerms:"];
      ];

      LSAMonRepresentByTerms[ qmat, opts ][xs, context]
    ];

LSAMonRepresentByTerms[ matArg_SSparseMatrix, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{ applyTermWeightFuncsQ, mat = matArg },

      applyTermWeightFuncsQ = TrueQ[ OptionValue[ LSAMonRepresentByTerms, "ApplyTermWeightFunctions" ] ];

      If[ ! KeyExistsQ[context, "weightedDocumentTermMatrix"],
        Echo["No weighted document-term matrix.", "LSAMonRepresentByTerms:"];
        Return[$LSAMonFailure]
      ];

      mat = ImposeColumnNames[ mat, ColumnNames[ context["weightedDocumentTermMatrix"] ] ];

      If[ applyTermWeightFuncsQ,
        If[ ! Apply[ And, KeyExistsQ[context, #]& /@ { "globalWeights", "localWeightFunction", "normalizerFunction" } ],
          Echo[
            "If the option \"ApplyTermWeightFunctions\" is set to True " <>
                "then the monad context is expected to have the elements \"globalWeights\", \"localWeightFunction\", \"normalizerFunction\".",
            "LSAMonRepresentByTerms:"];
          Return[$LSAMonFailure]
        ];
        mat = WeightTermsOfSSparseMatrix[ mat, context["globalWeights"], context["localWeightFunction"], context["normalizerFunction"] ]
      ];

      If[ Max[Abs[ColumnSums[mat]]] == 0,
        Echo["The terms of the argument cannot be found in the weighted document-term matrix.", "LSAMonRepresentByTerms:"];
      ];

      LSAMonUnit[ mat, context ]
    ];

LSAMonRepresentByTerms[__][___] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonRepresentByTerms[ mat_SSparseMatrix | _String | {_String ..} | {{_String ..} ..} ] .",
        "LSAMonRepresentByTerms:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Topics representation                                      *)
(*------------------------------------------------------------*)

Clear[LSAMonRepresentByTopics];

Options[LSAMonRepresentByTopics] = { "ApplyTermWeightFunctions" -> True };

LSAMonRepresentByTopics[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonRepresentByTopics[xs_, context_Association] := $LSAMonFailure;

LSAMonRepresentByTopics[][xs_, context_] := $LSAMonFailure;

LSAMonRepresentByTopics[ query_String, opts : OptionsPattern[] ][xs_, context_] :=
    LSAMonRepresentByTopics[ {query}, opts][xs, context];

LSAMonRepresentByTopics[ query_?QueryPatternQ, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{qmat},

      qmat = Fold[ LSAMonBind, LSAMonUnit[xs, context], {LSAMonRepresentByTerms[query, FilterRules[{opts}, Options[LSAMonRepresentByTerms]]], LSAMonTakeValue}];

      If[ TrueQ[qmat === $LSAMonFailure], Return[$LSAMonFailure] ];

      LSAMonRepresentByTopics[ qmat, opts ][xs, context]
    ];

LSAMonRepresentByTopics[ matArg_SSparseMatrix, opts : OptionsPattern[] ][xs_, context_] :=
    Block[{ mat = matArg, matNew = None, W, H, invH },

      mat = Fold[ LSAMonBind, LSAMonUnit[xs, context], { LSAMonRepresentByTerms[ mat, FilterRules[{opts}, Options[LSAMonRepresentByTerms]]], LSAMonTakeValue }];
      If[ TrueQ[mat === $LSAMonFailure], Return[$LSAMonFailure] ];

      {W, H} = RightNormalizeMatrixProduct[ SparseArray[context["W"]], SparseArray[context["H"]] ];

      mat = ImposeColumnNames[ mat, ColumnNames[ context["H"] ] ];

      Which[

        context["method"] == "NNMF",
        invH = PseudoInverse[H];
        matNew = Map[ # . invH &, SparseArray[mat] ],

        context["method"] == "SVD",
        (* We are using Map in order to prevent too much memory usage. *)
        matNew = Map[ H . # &, SparseArray[mat] ],

        context["method"] == "ICA",
        (* We are using Map in order to prevent too much memory usage. *)
        matNew = Map[ H . # &, SparseArray[mat] ],

        True,
        Echo["Unknown value of the context member \"method\".", "LSAMonRepresentByTopics:"];
        Return[$LSAMonFailure]
      ];

      matNew = ToSSparseMatrix[ SparseArray[matNew], "RowNames" -> RowNames[mat], "ColumnNames" -> RowNames[context["H"]] ];

      LSAMonUnit[matNew, context]
    ];

LSAMonRepresentByTopics[__][___] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonRepresentByTopics[ mat_SSparseMatrix | _String | {_String ..} | {{_String ..} ..} ] .",
        "LSAMonRepresentByTopics:"];
      $LSAMonFailure
    ];

(*------------------------------------------------------------*)
(* Matrix product normalization                               *)
(*------------------------------------------------------------*)

Clear[LSAMonNormalizeMatrixProduct];

Options[LSAMonNormalizeMatrixProduct] = { Normalized -> Right };

LSAMonNormalizeMatrixProduct[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonNormalizeMatrixProduct[xs_, context_Association] := LSAMonNormalizeMatrixProduct[][xs, context];

LSAMonNormalizeMatrixProduct[][xs_, context_Association] := LSAMonNormalizeMatrixProduct[Normalized -> Right][xs, context];

LSAMonNormalizeMatrixProduct[opts : OptionsPattern[]][xs_, context_] :=
    Block[{normalized, W, H},

      normalized = OptionValue[ LSAMonNormalizeMatrixProduct, Normalized];

      W = LSAMonTakeW[xs, context];
      H = LSAMonTakeH[xs, context];

      If[ TrueQ[ W === $LSAMonFailure ] || TrueQ[ H === $LSAMonFailure ],
        Return[$LSAMonFailure]
      ];

      Which[
        TrueQ[ normalized === Left ],
        {W, H} = LeftNormalizeMatrixProduct[ SparseArray[W], SparseArray[H] ],

        TrueQ[ normalized === Right ],
        {W, H} = RightNormalizeMatrixProduct[ SparseArray[W], SparseArray[H] ],

        True,
        Return[LSAMonNormalizeMatrixProduct[None][xs, context]]
      ];

      W = ToSSparseMatrix[ SparseArray[W], "RowNames" -> RowNames[context["W"]], "ColumnNames" -> ColumnNames[context["W"]] ];

      H = ToSSparseMatrix[ SparseArray[H], "RowNames" -> RowNames[context["H"]], "ColumnNames" -> ColumnNames[context["H"]] ];

      LSAMonUnit[xs, Join[ context, <| "W" -> W, "H" -> H |>] ]

    ];

LSAMonNormalizeMatrixProduct[__][___] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonNormalizeMatrixProduct[ Normalized -> (Left | Right) ] .",
        "LSAMonNormalizeMatrixProduct:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Documents collection statistics                            *)
(*------------------------------------------------------------*)

Clear[LSAMonEchoDocumentsStatistics];

Options[LSAMonEchoDocumentsStatistics] = Join[ {"LogBase" -> None}, Options[Histogram] ];

LSAMonEchoDocumentsStatistics[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoDocumentsStatistics[xs_, context_Association] := LSAMonEchoDocumentsStatistics[][xs, context];

LSAMonEchoDocumentsStatistics[][xs_, context_Association] := LSAMonEchoDocumentsStatistics[ImageSize -> 300][xs, context];

LSAMonEchoDocumentsStatistics[opts : OptionsPattern[]][xs_, context_] :=
    Block[{logBase, logFunc, logInsert, texts, textWords, eLabel = None, dOpts, smat},

      logBase = OptionValue[LSAMonEchoDocumentsStatistics, "LogBase"];

      texts = Fold[ LSAMonBind, LSAMonUnit[xs, context], {LSAMonGetDocuments, LSAMonTakeValue} ];

      If[ TrueQ[ texts === $LSAMonFailure], Return[$LSAMonFailure] ];

      If[ KeyExistsQ[context, "documents"],
        eLabel = "Context value \"documents\":",
        eLabel = "Pipeline value:"
      ];

      textWords = StringSplit /@ texts;

      dOpts = FilterRules[ Join[{opts}, {PerformanceGoal -> "Speed", PlotRange -> All, PlotTheme -> "Detailed", ImageSize -> 300}], Options[Histogram] ];

      If[ !KeyExistsQ[context, "documentTermMatrix"],
        smat = None,
        (*ELSE*)
        smat = context["documentTermMatrix"];
        If[ !SSparseMatrixQ[smat],
          smat = None,
          (*ELSE*)
          smat = Unitize[SparseArray[smat]];
        ]
      ];

      If[ NumberQ[logBase],
        logFunc = N[Log[logBase, #]]&;
        logInsert = "log " <> ToString[logBase] <> " number of",
        (* ELSE *)
        logFunc = Identity;
        logInsert = "number of"
      ];

      Echo[
        Grid[{
          {
            Row[{"Number of documents:", Length[texts]}],
            Row[{"Number of unique words:", Length[Union[Flatten[Values[textWords]]]]}],
            If[ TrueQ[smat === None],
              Nothing,
              Row[{"Document-term matrix dimensions:", Dimensions[smat]}]],
            If[ TrueQ[smat === None], Nothing, ""]
          },
          {
            Histogram[ logFunc[ StringLength /@ texts ], PlotLabel -> Capitalize[logInsert] <> " characters per document", FrameLabel -> {"Characters", "Documents"}, dOpts],
            Histogram[ logFunc[ Length /@ textWords ], PlotLabel -> Capitalize[logInsert] <> " words per document", FrameLabel -> {"Words", "Documents"}, dOpts],
            If[ TrueQ[smat === None],
              Nothing,
              Histogram[ logFunc[ Total[smat] ], PlotLabel -> Capitalize[logInsert] <> " documents per term", FrameLabel -> {"Documents", "Terms"}, dOpts]],
            If[ TrueQ[smat === None],
              Nothing,
              Column[{Capitalize[logInsert] <> "\ndocuments per term\nsummary", RecordsSummary[ logFunc[ Total[smat] ], {"# documents"} ]}]]
          }
        }],
        eLabel
      ];

      LSAMonUnit[xs, context]
    ];

LSAMonEchoDocumentsStatistics[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonEchoDocumentsStatistics:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Documents-term matrix statistics                           *)
(*------------------------------------------------------------*)

Clear[LSAMonEchoDocumentTermMatrixStatistics];

Options[LSAMonEchoDocumentTermMatrixStatistics] = Join[ {"LogBase" -> None}, Options[Histogram] ];

LSAMonEchoDocumentTermMatrixStatistics[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoDocumentTermMatrixStatistics[xs_, context_Association] := LSAMonEchoDocumentTermMatrixStatistics[][xs, context];

LSAMonEchoDocumentTermMatrixStatistics[][xs_, context_Association] := LSAMonEchoDocumentTermMatrixStatistics[ImageSize -> 300][xs, context];

LSAMonEchoDocumentTermMatrixStatistics[opts : OptionsPattern[]][xs_, context_] :=
    Block[{logBase, logFunc, logInsert, dOpts, smat},

      logBase = OptionValue[LSAMonEchoDocumentTermMatrixStatistics, "LogBase"];

      If[ TrueQ[ texts === $LSAMonFailure], Return[$LSAMonFailure] ];

      If[ !KeyExistsQ[context, "documentTermMatrix"],
        Echo["No document-term matrix.", "LSAMonEchoDocumentTermMatrixStatistics:" ];
        Return[$LSAMonFailure]
      ];

      dOpts = FilterRules[ Join[{opts}, {PerformanceGoal -> "Speed", PlotRange -> All, PlotTheme -> "Detailed", ImageSize -> 300}], Options[Histogram] ];

      smat = context["documentTermMatrix"];
      smat = Unitize[SparseArray[smat]];

      If[ NumberQ[logBase],
        logFunc = N[Log[logBase, #]]&;
        logInsert = "log " <> ToString[logBase] <> " number of",
        (* ELSE *)
        logFunc = Identity;
        logInsert = "number of"
      ];

      Echo[
        Grid[{
          {
            Row[{"Dimensions:", Dimensions[smat]}],
            Row[{"Density:", SparseArray[smat]["Density"]}],
            SpanFromLeft
          },
          {
            Histogram[ logFunc[ Total[smat] ], PlotLabel -> Capitalize[logInsert] <> " documents per term", FrameLabel -> {"Documents", "Terms"}, dOpts],
            Histogram[ logFunc[ Total[Transpose[smat]] ], PlotLabel -> Capitalize[logInsert] <> " terms per document", FrameLabel -> {"Terms", "Documents"}, dOpts],
            Column[{Capitalize[logInsert] <> "\ndocuments per term\nsummary", RecordsSummary[ logFunc[ Total[smat] ], {"# documents"} ]}]
          }
        }],
        "Context value \"documentTermMatrix\":"
      ];

      LSAMonUnit[xs, context]
    ];

LSAMonEchoDocumentTermMatrixStatistics[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonEchoDocumentTermMatrixStatistics:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Make a graph                                               *)
(*------------------------------------------------------------*)

Clear[LSAMonMakeGraph];

Options[LSAMonMakeGraph] = { "Weighted" -> True, "Type" -> "Bipartite", "RemoveLoops" -> True };

LSAMonMakeGraph[___][$LSAMonFailure] := $LSAMonFailure;
LSAMonMakeGraph[opts : OptionsPattern[]][xs_, context_] :=
    Block[{weightedQ, type, am, res, knownGrTypes, removeLoopsQ },

      weightedQ = TrueQ[OptionValue[LSAMonMakeGraph, "Weighted"]];

      type = OptionValue[LSAMonMakeGraph, "Type"];

      removeLoopsQ = TrueQ[OptionValue[LSAMonMakeGraph, "RemoveLoops"]];

      knownGrTypes = { "Bipartite", "DocumentDocument", "TermTerm", "Document", "Term" };
      If[ !MemberQ[knownGrTypes, type],
        Echo[Row[{"The value of the option \"Type\" is expected to be one of:", knownGrTypes}], "LSAMonMakeGraph:"];
        Return[$LSAMonFailure]
      ];

      Which[
        MatrixQ[xs],
        am = xs,

        KeyExistsQ[context, "weightedDocumentTermMatrix"],
        am = context["weightedDocumentTermMatrix"],

        KeyExistsQ[context, "documentTermMatrix"],
        am = context["documentTermMatrix"],

        True,
        Echo["Make a document-term matrix first.", "LSAMonMakeGraph:"];
        Return[$LSAMonFailure]
      ];

      (* Note that this takes the SparseArray object of a SSparseMatrix object. *)
      am = SparseArray[am];

      Which[

        weightedQ && type == "Bipartite",
        am = SparseArray[ Append[Most[ArrayRules[am]], {_, _} -> Infinity], Dimensions[am] ];
        am = SparseArray[ ArrayFlatten[{{Infinity, am}, {Transpose[am], Infinity}}] ];
        res = WeightedAdjacencyGraph[am, DirectedEdges -> True],

        !weightedQ && type == "Bipartite",
        am = SparseArray[ArrayFlatten[{{0, am}, {Transpose[am], 0}}]];
        res = AdjacencyGraph[am, DirectedEdges -> True],

        weightedQ && ( type == "DocumentDocument" || type == "Document" ),
        am = am . Transpose[am];
        am = Transpose[SparseArray[Map[If[Norm[#1] == 0, #1, #1 / Norm[#1]] &, Transpose[am]]]];
        am = SparseArray[ Append[Most[ArrayRules[am]], {_, _} -> Infinity], Dimensions[am] ];
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = WeightedAdjacencyGraph[am],

        !weightedQ && ( type == "DocumentDocument" || type == "Document" ),
        am = am . Transpose[am];
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = AdjacencyGraph[Unitize[am]],

        weightedQ && ( type == "TermTerm" || type == "Term" ),
        am = Transpose[am] . am;
        am = Transpose[SparseArray[Map[If[Norm[#1] == 0, #1, #1 / Norm[#1]] &, Transpose[am]]]];
        am = SparseArray[ Append[Most[ArrayRules[am]], {_, _} -> Infinity], Dimensions[am] ];
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = WeightedAdjacencyGraph[am],

        !weightedQ && ( type == "TermTerm" || type == "Term" ),
        am = Transpose[am] . am;
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = AdjacencyGraph[Unitize[am]];

      ];

      LSAMonUnit[res, context]

    ];

LSAMonMakeGraph[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonMakeGraph:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Find most important texts                                  *)
(*------------------------------------------------------------*)

Clear[LSAMonFindMostImportantDocuments];

Options[LSAMonFindMostImportantDocuments] = { "NumberOfTopDocuments" -> 3, "CentralityFunction" -> EigenvectorCentrality };

LSAMonFindMostImportantDocuments[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonFindMostImportantDocuments[ opts : OptionsPattern[] ][xs_, context_] :=
    Block[{topN},

      topN = OptionValue[ LSAMonFindMostImportantDocuments, "NumberOfTopDocuments" ];

      If[ ! ( IntegerQ[topN] && topN > 0 ),
        Echo[
          "The value of the option \"NumberOfTopDocuments\" is expected to be a positive integer.",
          "LSAMonFindMostImportantDocuments:"
        ];
        Return[$LSAMonFailure]
      ];

      LSAMonFindMostImportantDocuments[ topN, opts ][xs, context]
    ];

LSAMonFindMostImportantDocuments[topN_Integer, opts : OptionsPattern[]][xs_, context_] :=
    Block[{cFunc, gr, cvec, inds, smat },

      cFunc = OptionValue[LSAMonFindMostImportantDocuments, "CentralityFunction"];

      (* Here we should check that the monad value is a text collection. *)
      If[ !( KeyExistsQ[context, "documents"] && KeyExistsQ[context, "weightedDocumentTermMatrix"] && KeyExistsQ["documentTermMatrix"] ),

        If[ !KeyExistsQ[context, "documents"] && !(KeyExistsQ[context, "weightedDocumentTermMatrix"] || KeyExistsQ["documentTermMatrix"]) ,
          Echo["No texts.", "LSAMonFindMostImportantDocuments:"];
          Return[$LSAMonFailure]
        ];

        If[ !( KeyExistsQ[context, "weightedDocumentTermMatrix"] && KeyExistsQ["documentTermMatrix"] ),
          Echo["No document-term matrices.", "LSAMonFindMostImportantDocuments:"];
          Return[$LSAMonFailure]
        ];
      ];

      Which[
        TrueQ[ Head[xs] === Graph ] && VertexCount[xs] == Length[context["documents"]] ,
        gr = xs,

        TrueQ[ Head[xs] === Graph ] && VertexCount[xs] == Length[context["documents"]] + Length[context["terms"]],
        gr = xs,

        TrueQ[ cFunc === EigenvectorCentrality ] && !GraphQ[xs],
        (* Optimization, see below. *)
        gr = None,

        True,
        gr = Fold[ LSAMonBind, LSAMon[xs, context], {LSAMonMakeGraph["Type" -> "Bipartite"], LSAMonTakeValue} ]
      ];


      (* There is some inconsistencies in handling weighted graphs. *)
      (* That is why the most popular/likely case is computed directly. (For now.) *)
      If[ TrueQ[ cFunc === EigenvectorCentrality ] && ( TrueQ[ gr === None ] || VertexCount[xs] == Length[context["documents"]] ),

        (* Get document-term matrix. *)
        smat =
            If[ TrueQ[ KeyExistsQ[context, "weightedDocumentTermMatrix"] ],
              Lookup[context, "weightedDocumentTermMatrix"],
              Lookup[context, "documentTermMatrix"]
            ];

        (* Take the sparse array from the SSparseMatrix object. *)
        smat = SparseArray[smat];

        (* Make column stochastic. *)
        smat = Transpose[SparseArray[Map[If[Norm[#1] == 0, #1, #1 / Norm[#1]] &, Transpose[smat]]]];

        (* Compute eigenvector. *)
        cvec = SingularValueDecomposition[ N[smat], 1 ];

        cvec = cvec[[1]][[All, 1]];
        cvec = Abs[cvec] / Max[Abs[cvec]],

        (*ELSE*)
        cvec = cFunc[gr];
      ];

      If[ !ListQ[cvec], Return[$LSAMonFailure] ];

      inds = Take[Reverse[Ordering[cvec]], UpTo[topN]];

      Which[

        TrueQ[ gr === None ] || VertexCount[gr] == Length[context["documents"]],
        LSAMonUnit[ Transpose[{cvec[[inds]], inds, Keys @ context["documents"][[inds]], Values @ context["documents"][[inds]]}], context ],

        VertexCount[gr] == Length[context["documents"]] + Length[context["terms"]],
        LSAMonUnit[ Transpose[{cvec[[inds]], inds, Join[ Values[context["documents"]], context["terms"]][[inds]]}], context ],

        True,
        $LSAMonFailure
      ]
    ];

LSAMonFindMostImportantDocuments[___][__] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonFindMostImportantDocuments[topN_Integer, opts___] .",
        "LSAMonFindMostImportantDocuments::"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Find most important sentences stand alone function         *)
(*------------------------------------------------------------*)

Clear[FindMostImportantSentences];

Options[FindMostImportantSentences] =
    Join[
      {
        "Splitter" -> Function[{text}, Select[StringSplit[text, {".", "!", "?", "..."}], StringLength[#] >= 3 &] ],
        "StopWords" -> Automatic
      },
      Options[Grid]
    ];

FindMostImportantSentences[text_String, nTop_Integer : 5, opts : OptionsPattern[] ] :=
    Block[{splitFunc = OptionValue["Splitter"]},
      FindMostImportantSentences[splitFunc[text], nTop, opts]
    ];

FindMostImportantSentences[sentences : {_String ..}, nTop_Integer : 5, opts : OptionsPattern[]] :=
    Block[{res, stopWords = OptionValue["StopWords"]},

      Quiet[
        res =
            Fold[
              LSAMonBind,
              LSAMonUnit[sentences],
              {
                LSAMonMakeDocumentTermMatrix[{}, stopWords],
                LSAMonApplyTermWeightFunctions["IDF", "None", "Cosine"],
                (*                LSAMonMakeGraph["Type" -> "DocumentDocument"],*)
                LSAMonFindMostImportantDocuments[nTop],
                LSAMonTakeValue
              }
            ];
      ];

      Grid[res, FilterRules[{opts}, Options[Grid]], Alignment -> Left]
    ];


End[]; (*`Private`*)

EndPackage[]
