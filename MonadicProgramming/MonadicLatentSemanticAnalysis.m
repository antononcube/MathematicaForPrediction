(*
    Monadic latent semantic analysis Mathematica package
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

(* :Package Version: 0.8 *)
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
        LSAMonTopicExtraction[5, 60, 12, "MaxSteps" -> 6, "PrintProfilingInfo" -> True];

      (* Show statistical thesaurus in two different ways. *)
      res⟹
        LSAMonStatisticalThesaurus[{"arms", "banking", "economy", "education", "freedom", "tariff", "welfare"}, 6]⟹
        LSAMonRetrieveFromContext["statisticalThesaurus"]⟹
        LSAMonEchoValue⟹
        LSAMonEchoStatisticalThesaurus[];

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

If[Length[DownValues[MathematicaForPredictionUtilities`RecordsSummary]] == 0,
  Echo["MathematicaForPredictionUtilities.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"]
];

If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];

If[Length[DownValues[DocumentTermMatrixConstruction`DocumentTermMatrix]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/DocumentTermMatrixConstruction.m"]
];

If[Length[DownValues[NonNegativeMatrixFactorization`GDCLS]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/NonNegativeMatrixFactorization.m"]
];

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];

If[Length[DownValues[SSparseMatrix`ToSSparseMatrix]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/SSparseMatrix.m"]
];

If[Length[DownValues[OutlierIdentifiers`OutlierPosition]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicLatentSemanticAnalysis`"];

$LSAMonFailure::usage = "Failure symbol for the monad LSAMon.";

LSAMonApplyTermWeightFunctions::usage = "Apply term weight functions to entries of the document-term matrix.";

LSAMonBasisVectorInterpretation::usage = "Interpret the a specified basis vector.";

LSAMonEchoStatisticalThesaurus::usage = "Echo the statistical thesaurus entries for a specified list of words.";

LSAMonEchoDocumentsStatistics::usage = "Echo statistics for the text collection.";

LSAMonEchoTopicsTable::usage = "Echo the a table with the extracted topics.";

LSAMonGetDocuments::usage = "Get monad's document collection.";

LSAMonMakeDocumentTermMatrix::usage = "Make the document-term matrix.";

LSAMonMakeGraph::usage = "Make a graph of the document-term, document-document, or term-term relationships.";

LSAMonMostImportantTexts::usage = "Find the most important texts in the text collection.";

LSAMonStatisticalThesaurus::usage = "Compute the statistical thesaurus for specified list of words.";

LSAMonTextCollectionQ::usage = "Gives True if the argument is a text collection.";

LSAMonTopicExtraction::usage = "Extract topics.";

LSAMonTopicsRepresentation::usage = "Find the topic representation corresponding to a list of tags. \
Each monad document is expected to have a tag. One tag might correspond to multiple documents.";

LSAMonTopicsTable::usage = "Make a table of topics.";

LSAMonTakeTexts::usage = "Gives the value of the key \"texts\" from the monad context.";

LSAMonTakeMatrix::usage = "Gives SSparseMatrix object of the value of the key \"docTermMat\" from the monad context.";

LSAMonTakeWeightedMatrix::usage = "Gives SSparseMatrix object of the value of the key \"wDocTermMat\" from the monad context.";

Begin["`Private`"];

Needs["MathematicaForPredictionUtilities`"];
Needs["StateMonadCodeGenerator`"];
Needs["DocumentTermMatrixConstruction`"];
Needs["NonNegativeMatrixFactorization`"];
Needs["CrossTabulate`"];
Needs["SSparseMatrix`"];
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
    "W", "H",  "topicColumnPositions",
    "automaticTopicNames", "statisticalThesaurus", "topicsTable" },
  "FailureSymbol" -> $LSAMonFailure ];

ClearAll[LSAMonTakeMatrix, LSAMonTakeWeightedMatrix];

LSAMonTakeMatrix = LSAMonTakeDocumentTermMatrix;

LSAMonTakeWeightedMatrix = LSAMonTakeWeightedDocumentTermMatrix;

(**************************************************************)
(* Adapter functions                                          *)
(**************************************************************)

ClearAll[DocumentTermSSparseMatrix];

DocumentTermSSparseMatrix[ docs : ( {_String ...} | {{_String...}...} ), {stemmingRules:(_List|_Dispatch|_Association), stopWords_}, opts : OptionsPattern[] ] :=
    DocumentTermSSparseMatrix[ AssociationThread[ Map[ ToString, Range[Length[docs]]], docs ], {stemmingRules, stopWords}, opts ];

DocumentTermSSparseMatrix[
  docs : ( Association[ (_->_String) ...] | Association[ (_-> {_String...}) ... ] ),
  {stemmingRules:(_List|_Dispatch|_Association), stopWords_},
  opts : OptionsPattern[]] :=
    Block[{docIDs, docTermMat, terms},

      docIDs = Keys[docs];

      {docTermMat, terms} = DocumentTermMatrix[ Values[docs], { stemmingRules, stopWords}, opts];

      ToSSparseMatrix[ docTermMat, "RowNames" -> docIDs, "ColumnNames" -> terms ]
    ];

(*------------------------------------------------------------*)
(* Also can be used in SMRMon. *)

ClearAll[WeightTermsOfSSparseMatrix];

WeightTermsOfSSparseMatrix[ smat_SSparseMatrix ] := WeightTermsOfSSparseMatrix[ smat, "IDF", "None", "Cosine"];

WeightTermsOfSSparseMatrix[ smat_SSparseMatrix, globalWeightFunction_, localWeightFunction_, normalizerFunction_ ] :=
    Block[{},
      ToSSparseMatrix[
        WeightTerms[SparseArray[smat], globalWeightFunction, localWeightFunction, normalizerFunction],
        "RowNames" -> RowNames[smat],
        "ColumnNames" -> ColumnNames[smat]
      ]
    ];

(**************************************************************)
(* Get texts                                                  *)
(**************************************************************)

ClearAll[LSAMonTextCollectionQ];
LSAMonTextCollectionQ[x_] := VectorQ[x, StringQ];

(*ClearAll[LSAMonTextCollectionQ]*)
(*LSAMonTextCollectionQ[x_] := VectorQ[x, StringQ] || AssociationQ[x] && VectorQ[ Values[x], StringQ ];*)

ClearAll[LSAMonGetDocuments];

LSAMonGetDocuments[$LSAMonFailure] := $LSAMonFailure;

LSAMonGetDocuments[][xs_, context_] := LSAMonGetDocuments[xs, context];

LSAMonGetDocuments[xs_, context_] :=
    Block[{texts},

      Which[

        KeyExistsQ[context, "documents"] && LSAMonTextCollectionQ[ context["documents"] ],
        LSAMonUnit[ context["documents"], context],

        LSAMonTextCollectionQ[xs],
        LSAMonUnit[xs, context],

        VectorQ[xs, StringQ],
        texts = AssociationThread[ ToString /@ Range[Length[xs]], xs];
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
ClearAll[LSAMonMakeDocumentTermMatrix];

LSAMonMakeDocumentTermMatrix[___][$LSAMonFailure] := $LSAMonFailure;
LSAMonMakeDocumentTermMatrix[stemRules : (_List|_Dispatch|_Association), stopWordsArg : {_String ...} | Automatic ][xs_, context_] :=
    Block[{stopWords = stopWordsArg, docs, docTermMat },

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

      LSAMon[xs, Join[context, <| "documents" -> docs, "documentTermMatrix" -> docTermMat, "terms" -> ColumnNames[docTermMat] |>]]

    ];

LSAMonMakeDocumentTermMatrix[__][___] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonMakeDocumentTermMatrix[stemRules : (_List|_Dispatch|_Association), stopWords : {_String ...}] .",
        "LSAMonMakeDocumentTermMatrix:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Apply term weight function to matrix entries               *)
(*------------------------------------------------------------*)

ClearAll[LSAMonApplyTermWeightFunctions];

LSAMonApplyTermWeightFunctions[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonApplyTermWeightFunctions[globalWeightFunction_String, localWeightFunction_String, normalizerFunction_String][xs_, context_] :=
    Block[{wDocTermMat},

      Which[
        KeyExistsQ[context, "documentTermMatrix"],
        wDocTermMat = WeightTermsOfSSparseMatrix[context["documentTermMatrix"], globalWeightFunction, localWeightFunction, normalizerFunction];
        LSAMon[xs, Join[context, <|"weightedDocumentTermMatrix" -> wDocTermMat|>]],

        True,
        Echo["Cannot find document-term matrix.", "LSAMonApplyTermWeightFunctions:"];
        $LSAMonFailure
      ]

    ];

LSAMonApplyTermWeightFunctions[args___][xs_, context_] :=
    Block[{wDocTermMat},
      (* This code is the same as above. But I want to emphasize the string function names specification. *)
      Which[
        KeyExistsQ[context, "documentTermMatrix"],
        wDocTermMat = WeightTermsOfSSparseMatrix[context["documentTermMatrix"], args];
        LSAMon[xs, Join[context, <|"weightedDocumentTermMatrix" -> wDocTermMat|>]],

        True,
        Echo["Cannot find document-term matrix.", "LSAMonApplyTermWeightFunctions:"];
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

ClearAll[LSAMonTopicExtraction];

Options[LSAMonTopicExtraction] =
    Join[
      { Method -> "NNMF", "MinDocumentsPerTerm" -> 10, "NumberOfInitializingDocuments" -> 12 },
      Options[GDCLSGlobal]
    ];

LSAMonTopicExtraction[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonTopicExtraction[$LSAMonFailure] := $LSAMonFailure;

LSAMonTopicExtraction[xs_, context_Association] := $LSAMonFailure;

(*LSAMonTopicExtraction[nTopics_Integer, nMinDocumentsPerTerm_Integer, nInitializingDocuments_Integer, opts : OptionsPattern[]][xs_, context_] :=*)
(*    LSAMonTopicExtraction[ nTopics, Join[ { "MinDocumentsPerTerm" -> nMinDocumentsPerTerm, "NumberOfInitializingDocuments" -> nInitializingDocuments }, {opts}] ][xs, context];*)

LSAMonTopicExtraction[nTopics_Integer, opts : OptionsPattern[]][xs_, context_] :=
    Block[{method, nMinDocumentsPerTerm, nInitializingDocuments,
      docTermMat, documentsPerTerm, pos, W, H, M1, k, p, m, n, automaticTopicNames },

      method = OptionValue[ LSAMonTopicExtraction, Method ];
      If[ !MemberQ[ {"SVD", "NNMF"}, method ],
        Echo["The value of the option Method is expected to be \"SVD\" or \"NNMF\".", "LSAMonTopicExtraction:"];
        Return[$LSAMonFailure]
      ];

      nMinDocumentsPerTerm = OptionValue[ LSAMonTopicExtraction, "MinDocumentsPerTerm" ];
      If[ ! ( IntegerQ[ nMinDocumentsPerTerm ] && nMinDocumentsPerTerm > 0 ),
        Echo["The value of the option \"MinDocumentsPerTerm\" is expected to be a positive integer.", "LSAMonTopicExtraction:"];
        Return[$LSAMonFailure]
      ];

      nInitializingDocuments = OptionValue[ LSAMonTopicExtraction, "NumberOfInitializingDocuments" ];
      If[ ! ( IntegerQ[ nInitializingDocuments ] && nInitializingDocuments > 0 ),
        Echo["The value of the option \"NumberOfInitializingDocuments\" is expected to be a positive integer.", "LSAMonTopicExtraction:"];
        Return[$LSAMonFailure]
      ];

      Which[
        KeyExistsQ[context, "documentTermMatrix"] && !KeyExistsQ[context, "weightedDocumentTermMatrix"],
        Fold[
          LSAMonBind,
          LSAMonUnit[xs, context],
          {
            LSAMonApplyTermWeightFunctions[],
            LSAMonTopicExtraction[
              nTopics,
              Join[ { Method -> method, "MinDocumentsPerTerm" -> nMinDocumentsPerTerm, "NumberOfInitializingDocuments" -> nInitializingDocuments }, {opts}]
            ]
           }
        ],

        method == "NNMF" && KeyExistsQ[context, "weightedDocumentTermMatrix"] && SSparseMatrixQ[context["weightedDocumentTermMatrix"]],
        (*  Non-negative matrix factorization   *)

        docTermMat = SparseArray[ context["documentTermMatrix"] ];

        documentsPerTerm = Total /@ Transpose[Clip[docTermMat, {0, 1}]];
        pos = Flatten[Position[documentsPerTerm, s_?NumberQ /; s >= nMinDocumentsPerTerm]];

        M1 = SparseArray[ context["weightedDocumentTermMatrix"][[All, pos]] ];

        {k, p} = {nTopics, nInitializingDocuments};
        {m, n} = Dimensions[M1];
        M1 = Transpose[M1];
        M1 = Map[# &, M1];
        H = ConstantArray[0, {k, n}];
        W = Table[Total[RandomSample[M1, p]], {k}];
        Do[
          W[[i]] = W[[i]]/Norm[W[[i]]];
          , {i, 1, Length[W]}];
        W = Transpose[W];
        M1 = SparseArray[M1];
        M1 = Transpose[M1];

        W = SparseArray[W];
        H = SparseArray[H];
        {W, H} = GDCLSGlobal[M1, W, H, Evaluate[ FilterRules[ {opts}, Options[GDCLSGlobal] ] ] ];

        automaticTopicNames =
                Table[
                    StringJoin[Riffle[BasisVectorInterpretation[Normal@H[[ind]], 3, context["terms"][[pos]]][[All, 2]], "-"]],
                  {ind, 1, Dimensions[W][[2]]}];

        If[ ! DuplicateFreeQ[automaticTopicNames],
          automaticTopicNames = MapIndexed[ #1<>"-"<>ToString[#2]&, automaticTopicNames ];
        ];

        LSAMon[xs, Join[context, <|"W" -> W, "H" -> H, "topicColumnPositions" -> pos, "automaticTopicNames"->automaticTopicNames |>]],

        method == "SVD" && KeyExistsQ[context, "weightedDocumentTermMatrix"] && SSparseMatrixQ[context["weightedDocumentTermMatrix"]],
        (*  Singular Value Decomposition  *)
        Echo["SVD method is not implemented yet.", "LSAMonTopicExtraction:"];
        $LSAMonFailure,

        True,
        Echo["Cannot find a document-term matrix.", "LSAMonTopicExtraction:"];
        $LSAMonFailure
      ]
    ];

LSAMonTopicExtraction[___][__] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonTopicExtraction[ nTopics_Integer, opts___] .",
        "LSAMonTopicExtraction::"];
      $LSAMonFailure;
    ];


(*------------------------------------------------------------*)
(* Make statistical thesaurus                                 *)
(*------------------------------------------------------------*)

ClearAll[LSAMonStatisticalThesaurus];

LSAMonStatisticalThesaurus[___][$LSAMonFailure] := $LSAMonFailure;
LSAMonStatisticalThesaurus[words : {_String ..}, numberOfNNs_Integer][xs_, context_] :=
    Block[{W, H, HNF, thRes},
      Which[
        KeyExistsQ[context, "H"] && KeyExistsQ[context, "W"],
        {W, H} = NormalizeMatrixProduct[context["W"], context["H"]];
        HNF = Nearest[Range[Dimensions[H][[2]]], DistanceFunction -> (Norm[H[[All, #1]] - H[[All, #2]]] &)];

        thRes =
            Map[{#, NearestWords[HNF, #,
              context["terms"][[context["topicColumnPositions"]]], {},
              numberOfNNs]} &,
              Sort[words]];

        LSAMon[thRes, Join[context, <|"statisticalThesaurus" -> thRes|>]],

        True,
        Echo["No factorization of the document-term matrix is made.", "LSAMonStatisticalThesaurus:"];
        $LSAMonFailure
      ]
    ];

LSAMonStatisticalThesaurus[___][__] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonStatisticalThesaurus[words : {_String ..}, numberOfNNs_Integer] .",
        "LSAMonStatisticalThesaurus::"];
      $LSAMonFailure;
    ];


(*------------------------------------------------------------*)
(* Echo statistical thesaurus                                 *)
(*------------------------------------------------------------*)

ClearAll[LSAMonEchoStatisticalThesaurus];

LSAMonEchoStatisticalThesaurus[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoStatisticalThesaurus[xs_, context_Association] := LSAMonEchoStatisticalThesaurus[][xs, context];

LSAMonEchoStatisticalThesaurus[][xs_, context_] :=
    Block[{},
      Which[

        KeyExistsQ[context, "statisticalThesaurus"],
        Echo@
            Grid[
              Prepend[
                context["statisticalThesaurus"],
                Style[#, Blue, FontFamily -> "Times"] & /@ {"word", "statistical thesaurus"}],
              Dividers -> All, Alignment -> Left,
              Spacings -> {Automatic, 0.75}];
        LSAMon[xs, context],

        True,
        Echo["No statistical thesurus is computed.", "LSAMonEchoStatisticalThesaurus:"];
        $LSAMonFailure
      ]
    ];

LSAMonEchoStatisticalThesaurus[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "LSAMonEchoStatisticalThesaurus:"];
      $LSAMonFailure;
    ];


(*------------------------------------------------------------*)
(* Basis vector interpretation                                *)
(*------------------------------------------------------------*)

ClearAll[LSAMonBasisVectorInterpretation];

Options[LSAMonBasisVectorInterpretation] = { "NumberOfTerms" -> 12 };

LSAMonBasisVectorInterpretation[___][$LSAMonFailure] := $LSAMonFailure;
LSAMonBasisVectorInterpretation[vectorIndices:(_Integer|{_Integer..}), opts:OptionsPattern[] ][xs_, context_] :=
    Block[{W, H, res, numberOfTerms},

      numberOfTerms = OptionValue[LSAMonBasisVectorInterpretation, "NumberOfTerms"];

      {W, H} = RightNormalizeMatrixProduct[context["W"], context["H"]];

      res =
          Map[
            BasisVectorInterpretation[#, numberOfTerms, context["terms"][[context["topicColumnPositions"]]]]&,
            Normal@H[[Flatten@{vectorIndices}]]
          ];

      If[ !MatchQ[res,{{{_?NumberQ, _String}..}..}],
        $LSAMonFailure,
        LSAMonUnit[ res, context ]
      ]

    ];

LSAMonBasisVectorInterpretation[___][__] :=
    Block[{},
      Echo[
        "The expected arguments are LSAMonBasisVectorInterpretation[vectorIndices:(_Integer|{_Integer..}), opts___] .",
        "LSAMonBasisVectorInterpretation:"];
      $LSAMonFailure;
    ];

(*------------------------------------------------------------*)
(* Topics table making                                        *)
(*------------------------------------------------------------*)

ClearAll[LSAMonTopicsTable];

Options[LSAMonTopicsTable] = { "NumberOfTerms" -> 12 };

LSAMonTopicsTable[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonTopicsTable[xs_, context_Association] := LSAMonTopicsTable[][xs, context];

LSAMonTopicsTable[opts:OptionsPattern[]][xs_, context_] :=
    Block[{topicsTbl, k, numberOfTerms},

      numberOfTerms = OptionValue["NumberOfTerms"];

      k = Dimensions[context["W"]][[2]];

      topicsTbl =
          Table[
            TableForm[{NumberForm[#[[1]]/t[[1, 1]], {4, 3}], #[[2]]} & /@ t],
            {t, First @ LSAMonBasisVectorInterpretation[Range[k], "NumberOfTerms" -> numberOfTerms][xs, context] }];

      LSAMonUnit[ topicsTbl, Join[ context, <| "topicsTable"->topicsTbl|> ] ]
    ];

LSAMonTopicsTable[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonTopicsTable:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Topics table echoing                                       *)
(*------------------------------------------------------------*)

ClearAll[LSAMonEchoTopicsTable];

Options[LSAMonEchoTopicsTable] = Join[
  {"NumberOfTableColumns" -> Automatic, "NumberOfTerms" -> 12 , "MagnificationFactor" -> Automatic},
  Options[Multicolumn] ];

LSAMonEchoTopicsTable[$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoTopicsTable[ opts:OptionsPattern[] ][$LSAMonFailure] := $LSAMonFailure;

LSAMonEchoTopicsTable[xs_, context_Association] := LSAMonEchoTopicsTable[][xs, context];

LSAMonEchoTopicsTable[][xs_, context_Association] := LSAMonEchoTopicsTable[Options[LSAMonEchoTopicsTable]][xs, context];

LSAMonEchoTopicsTable[opts:OptionsPattern[]][xs_, context_] :=
    Block[{topicsTbl, k, numberOfTableColumns, numberOfTerms, mFactor, tOpts},

      numberOfTableColumns = OptionValue[LSAMonEchoTopicsTable, "NumberOfTableColumns"];

      numberOfTerms = OptionValue[LSAMonEchoTopicsTable, "NumberOfTerms"];

      mFactor = OptionValue[LSAMonEchoTopicsTable, "MagnificationFactor"];
      If[ TrueQ[mFactor === Automatic], mFactor = 0.8 ];

      k = Dimensions[context["W"]][[2]];

      If[ KeyExistsQ[context, "topicsTable"],
        topicsTbl = context["topicsTable"],
      (*ELSE*)
        topicsTbl = First @ LSAMonTopicsTable["NumberOfTerms"->numberOfTerms][xs,context]
      ];

      tOpts = Join[ FilterRules[ {opts}, Options[Multicolumn] ], {Dividers -> All, Alignment -> Left} ];

      Echo @ Magnify[#, mFactor] & @
          If[ TrueQ[numberOfTableColumns === Automatic],
            Multicolumn[
              ColumnForm /@ Transpose[{Style[#, Red] & /@ Range[k], topicsTbl}], tOpts],
            (* ELSE *)
            Multicolumn[
              ColumnForm /@ Transpose[{Style[#, Red] & /@ Range[k], topicsTbl}], numberOfTableColumns, tOpts]
          ];

      LSAMonUnit[ topicsTbl, context ]
    ];

LSAMonEchoTopicsTable[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonEchoTopicsTable:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Topics representation                                      *)
(*------------------------------------------------------------*)

ClearAll[LSAMonTopicsRepresentation];

Options[LSAMonTopicsRepresentation] = { "ComputeTopicRepresentation" -> True, "AssignAutomaticTopicNames" -> True };

LSAMonTopicsRepresentation[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonTopicsRepresentation[xs_, context_Association] := LSAMonTopicsRepresentation[][xs, context];

LSAMonTopicsRepresentation[][xs_, context_] :=
    LSAMonTopicsRepresentation[Automatic,"ComputeTopicRepresentation" -> True][xs, context];

LSAMonTopicsRepresentation[tags:(Automatic|_List), opts:OptionsPattern[]][xs_, context_] :=
    Block[{computeTopicRepresentationQ, assignAutomaticTopicNamesQ, ctTags, W, H, docTopicIndices, ctMat },

      computeTopicRepresentationQ = OptionValue[LSAMonTopicsRepresentation, "ComputeTopicRepresentation"];
      assignAutomaticTopicNamesQ = OptionValue[LSAMonTopicsRepresentation, "AssignAutomaticTopicNames"];

      If[ KeyExistsQ[context, "documentTermMatrix"] && KeyExistsQ[context, "W"],

        Which[

          TrueQ[tags===Automatic] && KeyExistsQ[context, "docTags"],
          ctTags = context["docTags"],

          TrueQ[tags===Automatic],
          ctTags = Range[Dimensions[context["documentTermMatrix"]][[1]]],

          Length[tags] == Dimensions[context["documentTermMatrix"]][[1]],
          ctTags = tags,

          True,
          Echo["The length of the argument tags is expected to be same as the number of rows of the document-term matrix.",
            "LSAMonTopicsRepresentation:"];
          Return[$LSAMonFailure]
        ];

        {W, H} = NormalizeMatrixProduct[context["W"], context["H"] ];
        W = Clip[W, {0.01, 1}, {0, 1}];


        If[ computeTopicRepresentationQ || !KeyExistsQ[context, "docTopicIndices"],

        (* This is expected to be fairly quick, less than 1 second. *)
        (* If not, some sort of memoization has to be used, which will require consistency support. *)
        (* Using the option "ComputeTopicRepresentation" comes from those computation management concerns. *)
          docTopicIndices =
              Block[{v = Select[#, # > 0 &], vpos, ts1, ts2},
                vpos = Flatten@Position[#, x_ /; x > 0];
                ts1 =
                    OutlierIdentifiers`OutlierPosition[v,
                      OutlierIdentifiers`TopOutliers@*SPLUSQuartileIdentifierParameters];
                ts2 =
                    OutlierIdentifiers`OutlierPosition[v, OutlierIdentifiers`TopOutliers@*HampelIdentifierParameters];
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
        ctMat = CrossTabulate`CrossTabulate[ Flatten[MapThread[Thread[{#1, #2}] &, {ctTags, docTopicIndices}], 1]];

        If[ assignAutomaticTopicNamesQ,
          ctMat = Join[ ctMat, <| "ColumnNames" -> context["automaticTopicNames"][[ ctMat["ColumnNames"] ]] |> ]
        ];

        LSAMonUnit[ ctMat, Join[ context, <| "docTopicIndices"->docTopicIndices |> ] ],
      (* ELSE *)

        Echo["No document-term matrix factorization is computed.", "LSAMonTopicsRepresentation:"];
        $LSAMonFailure
      ]
    ];

LSAMonTopicsRepresentation[__][___] :=
    Block[{},
      Echo["The expected signature is LSAMonTopicsRepresentation[tags:(Automatic|_List), opts___] .", "LSAMonTopicsRepresentation:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Documents collection statistics                            *)
(*------------------------------------------------------------*)

ClearAll[LSAMonEchoDocumentsStatistics];

Options[LSAMonEchoDocumentsStatistics] = Options[Histogram];

LSAMonEchoDocumentsStatistics[___][$LSAMonFailure] := $LSAMonFailure;
LSAMonEchoDocumentsStatistics[opts:OptionsPattern[]][xs_,context_]:=
    Block[{texts, textWords, eLabel=None, dOpts, smat},

      Which[
        LSAMonTextCollectionQ[xs], texts = xs; eLabel = "Pipeline value:",
        KeyExistsQ[context,"documents"], texts = context["documents"]; eLabel = "Context value \"texts\":",
        True,
        Echo["Ingest texts first.", "LSMonMakeDocumentTermMatrix:"];
        $LSAMonFailure
      ];


      textWords = StringSplit /@ texts;

      dOpts = Join[{opts}, {PlotRange -> All, PlotTheme -> "Detailed", ImageSize->300}];

      If[ !KeyExistsQ[context, "documentTermMatrix"],
        smat = None,
        (*ELSE*)
        smat = context["documentTermMatrix"];
        If[ !MatrixQ[smat],
          smat = None,
          (*ELSE*)
          smat = Clip[smat];
        ]
      ];

      Echo[
        Grid[{
          {Row[{"Number of documents:", Length[texts]}],
            Row[{"Number of unique words:", Length[Union[Flatten[textWords]]]}],
            If[ TrueQ[smat === None],
              Nothing,
              Row[{"Document-term matrix dimensions:", Dimensions[smat]}]],
            If[ TrueQ[smat === None], Nothing, ""]
          },
          {Histogram[StringLength /@ texts, PlotLabel -> "Number of characters", dOpts],
            Histogram[Length /@ textWords, PlotLabel -> "Number of words", dOpts],
            If[ TrueQ[smat === None],
              Nothing,
              Histogram[Total[smat], PlotLabel -> "Number of documents per word", dOpts]],
            If[ TrueQ[smat === None],
              Nothing,
              Column[{"Summary of number of\ndocuments per word", RecordsSummary[Total[smat], {"# documents"}]}]]
          }
        }],
        eLabel
      ];

      LSAMonUnit[xs,context]
    ];

LSAMonEchoDocumentsStatistics[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonEchoDocumentsStatistics:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Make a graph                                               *)
(*------------------------------------------------------------*)

ClearAll[LSAMonMakeGraph];

Options[LSAMonMakeGraph] = { "Weighted"->True, "Type" -> "Bipartite", "RemoveLoops"->True };

LSAMonMakeGraph[___][$LSAMonFailure] := $LSAMonFailure;
LSAMonMakeGraph[opts:OptionsPattern[]][xs_, context_] :=
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
        am = Transpose[SparseArray[Map[If[Norm[#1] == 0, #1, #1/Norm[#1]] &, Transpose[am]]]];
        am = SparseArray[ Append[Most[ArrayRules[am]], {_, _} -> Infinity], Dimensions[am] ];
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = WeightedAdjacencyGraph[am],

        !weightedQ && ( type == "DocumentDocument" || type == "Document" ),
        am = am . Transpose[am];
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = AdjacencyGraph[Unitize[am]],

        weightedQ && ( type == "TermTerm" || type == "Term" ),
        am = Transpose[am] . am;
        am = Transpose[SparseArray[Map[If[Norm[#1] == 0, #1, #1/Norm[#1]] &, Transpose[am]]]];
        am = SparseArray[ Append[Most[ArrayRules[am]], {_, _} -> Infinity], Dimensions[am] ];
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = WeightedAdjacencyGraph[am],

        !weightedQ && ( type == "TermTerm" || type == "Term" ),
        am = Transpose[am] . am;
        If[removeLoopsQ, am = am - DiagonalMatrix[Diagonal[am]]];
        res = AdjacencyGraph[Unitize[am]];

      ];

      LSAMon[res, context]

    ];

LSAMonMakeGraph[__][___] :=
    Block[{},
      Echo["No arguments, just options are expected.", "LSAMonMakeGraph:"];
      $LSAMonFailure
    ];


(*------------------------------------------------------------*)
(* Find most important texts                                  *)
(*------------------------------------------------------------*)

ClearAll[LSAMonMostImportantTexts];

Options[LSAMonMostImportantTexts] = { "CentralityFunction" -> EigenvectorCentrality };

LSAMonMostImportantTexts[___][$LSAMonFailure] := $LSAMonFailure;

LSAMonMostImportantTexts[topN_Integer, opts:OptionsPattern[]][xs_, context_] :=
    Block[{cFunc, gr, cvec, inds, smat },

      cFunc = OptionValue[LSAMonMostImportantTexts, "CentralityFunction"];

      (* Here we should check that the monad value is a text collection. *)
      If[ !( KeyExistsQ[context, "documents"] || KeyExistsQ[context, "weightedDocumentTermMatrix"] || KeyExistsQ["documentTermMatrix"] ),

        If[ !KeyExistsQ[context, "documents"] && !(KeyExistsQ[context, "weightedDocumentTermMatrix"] || KeyExistsQ["documentTermMatrix"]) ,
          Echo["No texts.", "LSAMonMostImportantTexts:"];
          Return[$LSAMonFailure]
        ];


        If[ !( KeyExistsQ[context, "weightedDocumentTermMatrix"] || KeyExistsQ["documentTermMatrix"] ),
          Echo["No texts and document-term matrices.", "LSAMonMostImportantTexts:"];
          Return[$LSAMonFailure]
        ];
      ];

      Which[
        TrueQ[ Head[xs] === Graph ] && VertexCount[xs] == Length[context["documents"]] ,
        gr = xs,

        TrueQ[ Head[xs] === Graph ] && VertexCount[xs] == Length[context["documents"]] + Length[context["terms"]],
        gr = xs,

        True,
        gr = Fold[ LSAMonBind, LSAMon[xs,context], {LSAMonMakeGraph["Type"->"Bipartite"], LSAMonTakeValue} ]
      ];


      (* There is some inconsistencies in handling weighted graphs. *)
      (* That is why the most popular/likely case is computed directly. (For now.) *)
      If[ TrueQ[ cFunc === EigenvectorCentrality ] && VertexCount[xs] == Length[context["documents"]],

        (* Get document-term matrix. *)
        smat = If[ TrueQ[ KeyExistsQ[context, "weightedDocumentTermMatrix"] ], Lookup[context, "weightedDocumentTermMatrix"], Lookup[context, "documentTermMatrix"] ];

        (* Make column stochastic. *)
        smat = Transpose[SparseArray[Map[If[Norm[#1] == 0, #1, #1/Norm[#1]] &, Transpose[smat]]]];

        (* Compute eigenvector. *)
        cvec = SingularValueDecomposition[ N[smat], 1 ];

        cvec = cvec[[1]][[All,1]];
        cvec = Abs[cvec] / Max[Abs[cvec]],

        (*ELSE*)
        cvec = cFunc[gr];
      ];

      If[ !ListQ[cvec], Return[$LSAMonFailure] ];

      inds = Take[Reverse[Ordering[cvec]], UpTo[topN]];

      Which[

        VertexCount[gr] == Length[context["documents"]],
        LSAMonUnit[ Transpose[{cvec[[inds]], inds, context["documents"][[inds]]}], context ],

        VertexCount[gr] == Length[context["documents"]] + Length[context["terms"]],
        LSAMonUnit[ Transpose[{cvec[[inds]], inds, Join[context["documents"],context["terms"]][[inds]]}], context ],

        True,
        $LSAMonFailure
      ]
    ];

LSAMonMostImportantTexts[___][__] :=
    Block[{},
      Echo[
        "The expected signature is LSAMonMostImportantTexts[topN_Integer, opts___] .",
        "LSAMonMostImportantTexts::"];
      $LSAMonFailure;
    ];


End[]; (*`Private`*)

EndPackage[]
