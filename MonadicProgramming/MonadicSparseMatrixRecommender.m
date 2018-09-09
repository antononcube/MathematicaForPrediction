(*
    Monadic Sparse Matrix Recommender Mathematica package
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
(* :Title: MonadicSparseMatrixRecommender *)
(* :Context: MonadicSparseMatrixRecommender` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-09-02 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 antonov *)
(* :Keywords: *)
(* :Discussion:
*
*   # In brief
*
*   This package provides monadic implementation of a Item-Item Recommender (IIR) system.
*   The IIR system is aimed at being fast and agile; see [1,2].
*
*
*   # Discussion on the approach*
*   ...
*
*   # Usage example
*   ...
*
*   # Design considerations
*
*   The monad commands are given "verb-like" names and appropriate synonyms are considered/added.
*
*   It is an interesting question how to create the recommender.
*   If we allow empty IIR monad structure creation SMRMonUnit[] then we have to be able
*   to ingest matrices and datasets for the creation population of the IIR data structures.
*
*
*   # Implementation considerations
*
*   Note that the Sparse Matrix Recommender can be mapped into the Stream Blending Recommender. See [2].
*
*   The easiest way to implement IIR is to reuse the Object-Oriented Programming (OOP) implementation [3].
*   But OOP implementation [3] was developed before the introduction Association and the implementation of the
*   packages SSparseMatrix.m, [4,5], and CrossTabulate.m, [6].
*
*
*   # References
*
*   [1] Anton Antonov, A fast and agile Item-Item Recommender system, (2011), Wolfram Technology Conference.
*
*   [2] Anton Antonov, Mapping Sparse Matrix Recommender to Streams Blending Recommender, (2017), MathematicaForPrediction at GitHub.
*
*   [3] Anton Antonov, Sparse Matrix Recommender Framework Mathematica package, (2014), MathematicaForPrediction at GitHub.
*
*   [4] Anton Antonov, SSparseMatrix Mathematica unit tests, (2018), MathematicaForPrediction at GitHub.
*       URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/SSparseMatrix-tests.wlt
*
*   [5] Anton Antonov, RSparseMatrix Mathematica package, (2015), MathematicaForPrediction at GitHub.
*       URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RSparseMatrix.m
*
*   [6] Anton Antonov, Cross tabulation implementation in Mathematica, (2017), MathematicaForPrediction at GitHub.
*
* *)

(*
*   # TODOs
*
*   1. [X] [A] Implement tag-types re-weighting.
*   2. [X] [A] Implement tags re-weighting.
*   3. [ ] [B] Implement term weight functions application through association of tag-types.
*   4. [ ] [A] Implement creation from long form dataset, and
*   5. [ ] [A] do corresponding refactoring.
*   6. [ ] [B] Refactor the codes of the term weight functions ("SMRMonApply*Function",).
*
* *)

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

If[Length[DownValues[SSparseMatrix`ToSSparseMatrix]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/SSparseMatrix.m"]
];

If[Length[DownValues[DocumentTermMatrixConstruction`WeightTerms]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/DocumentTermMatrixConstruction.m"]
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicSparseMatrixRecommender`"];

$SMRMonFailure::usage = "Failure symbol of SMRMon."

SMRMonScoredItemsQ::usage = "True if the argument is an association with item names or item indices as keys \
and numbers as values."

SMRMonScoredTagsQ::usage = "True if the argument is an association with tags or tag indices as keys \
and numbers as values."

SMRMonCreate::usage = "Creates the recommender structures from a transactions Dataset and a specifications Dataset."

SMRMonApplyLocalWeightFunction::usage = "Applies a specified local weight function to the entries
    of the contingency matrix."

SMRMonApplyGlobalWeightFunction::usage = "Applies a specified global weight function to the entries \
of the contingency matrix."

SMRMonApplyNormalizationFunction::usage = "Applies a specified normalization function to the entries \
of the contingency matrix."

SMRMonApplyTermWeightFunctions::usage = "Apply term weight functions to entries of the recommender matrix."

SMRMonRecommend::usage = "Recommends items based on history."

SMRMonRecommendByHistory::usage = "Recommends items based on history."

SMRMonRecommendByProfile::usage = "Recommends items based on profile."

SMRMonToProfileVector::usage = "Makes a profile vector from an argument that is a list of tags or an Association object."

SMRMonFromProfileVector::usage = "Makes a profile association from a profile vector argument."

SMRMonToItemsDataset::usage = "Converts a recommendations association into a Dataset object."

SMRMonJoinAcross::usage = "Joins the a recommendations association or Dataset object with a given Dataset object."

SMRMonSetTagTypeWeights::usage = "Sets weights (significance factors) to the IIR tag types."

SMRMonSetTagWeights::usage = "Sets weights (significance factors) to the IIR tags."

SMRMonClassify::usage = "Uses IIR as a classifier for specified label tag-type over a vector or a matrix."

SMRMonSetClassificationParameters::usage = "Sets the parameters to be used by SMRMonClassify."

SMRMonProveByHistory::usage = "Proof the recommendations using consumption history."

SMRMonProveByProfile::usage = "Proof the recommendations using consumption profile."

SMRMonTakeMatrices::usage = "Gives an association with the tag (sparse) matrices."

SMRMonTakeMatrix::usage = "Gives the recommendation matrix (SSparseMatrix)."

SMRMonTakeItemNames::usage = "Gives the item names. (Row names of the recommender matrix.)"

SMRMonTakeTags::usage = "Gives the tags. (Column names of the recommender matrix.)"

SMRMonTakeTagTypeWeights::usage = "Takes the tag-type weights."

SMRMonTakeTagTypes::usage = "Takes the tag-types."

Begin["`Private`"];


Needs["MathematicaForPredictionUtilities`"]
Needs["CrossTabulate`"]
Needs["StateMonadCodeGenerator`"]
Needs["SSparseMatrix`"]
Needs["DocumentTermMatrixConstruction`"]


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of SMRMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicSparseMatrixRecommender`SMRMon", "FailureSymbol" -> $SMRMonFailure, "StringContextNames" -> False ]


(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)

ClearAll[SMRMonSetItemNames]

SMRMonSetItemNames[$SMRMonFailure] := $SMRMonFailure;

(* Here we can/have to have a correctness check. It is one of the advantages to SSparseMatrix. *)
(*SMRMonSetItemNames[names_][xs_, context_Association] :=*)
    (*SMRMonUnit[ xs, Join[context, <|"itemNames"->names|>] ];*)

ClearAll[SMRMonGetTagTypeWeights]
SMRMonGetTagTypeWeights[$SMRMonFailure] := $SMRMonFailure;
SMRMonGetTagTypeWeights[][$SMRMonFailure] := $SMRMonFailure;
SMRMonGetTagTypeWeights[xs_, context_] := SMRMonGetTagTypeWeights[][xs, context];
SMRMonGetTagTypeWeights[][xs_, context_Association] := Lookup[context, "tagTypeWeights"];
SMRMonGetTagTypeWeights[__][___] := $SMRMonFailure;

ClearAll[SMRMonGetTagTypes]
SMRMonGetTagTypes[$SMRMonFailure] := $SMRMonFailure;
SMRMonGetTagTypes[][$SMRMonFailure] := $SMRMonFailure;
SMRMonGetTagTypes[xs_, context_] := SMRMonGetTagTypes[][xs, context];
SMRMonGetTagTypes[][xs_, context_Association] := Keys[Lookup[context, "tagTypes"]];
SMRMonGetTagTypes[__][___] := $SMRMonFailure;

ClearAll[SMRMonTakeMatrix]
SMRMonTakeMatrix[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrix[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrix[xs_, context_] := SMRMonTakeMatrix[][xs, context];
SMRMonTakeMatrix[][xs_, context_Association] := Lookup[context, "M", $SMRMonFailure];
SMRMonTakeMatrix[__][___] := $SMRMonFailure;


ClearAll[SMRMonTakeMatrices]
SMRMonTakeMatrices[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrices[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrices[xs_, context_] := SMRMonTakeMatrices[][xs, context];
SMRMonTakeMatrices[][xs_, context_Association] := Lookup[context, "matrices", $SMRMonFailure];
SMRMonTakeMatrices[__][___] := $SMRMonFailure;


ClearAll[SMRMonTakeItemNames]
SMRMonTakeItemNames[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeItemNames[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeItemNames[xs_, context_] := SMRMonTakeItemNames[][xs, context];
SMRMonTakeItemNames[][xs_, context_Association] := Lookup[context, "itemNames", $SMRMonFailure];
SMRMonTakeItemNames[__][___] := $SMRMonFailure;


ClearAll[SMRMonTakeTags]
SMRMonTakeTags[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeTags[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeTags[xs_, context_] := SMRMonTakeTags[][xs, context];
SMRMonTakeTags[][xs_, context_Association] := Lookup[context, "tags", $SMRMonFailure];
SMRMonTakeTags[__][___] := $SMRMonFailure;


(**************************************************************)
(* Predicates                                                 *)
(**************************************************************)

ClearAll[ScoredItemIndexesQ]

ScoredItemIndexesQ[recs_Association, context_Association] :=
    If[ KeyExistsQ[context, "M"],
      AssociationQ[recs] &&
          VectorQ[Values[recs], NumberQ] &&
          Apply[And, Map[TrueQ[0 < # <= RowsCount[context["M"]]]&, Keys[recs]] ],
      True,
      (*ELSE*)
      AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
    ];

ScoredItemIndexesQ[___] := False

ClearAll[SMRMonScoredItemsQ]

SMRMonScoredItemsQ[$SMRMonFailure] := $SMRMonFailure;

SMRMonScoredItemsQ[xs_, context_Association] := SMRMonScoredItemsQ[xs][xs, context];

SMRMonScoredItemsQ[][xs_, context_Association] := SMRMonScoredItemsQ[xs][xs, context];

SMRMonScoredItemsQ[recs_Association][xs_, context_Association] :=
    Block[{res},
      res =
          If[ KeyExistsQ[context, "itemNames"],
            AssociationQ[recs] &&
                VectorQ[Values[recs], NumberQ] &&
                ( Length[ Intersection[ Keys[recs], Keys[context["itemNames"]] ] ] == Length[recs] ||
                    Length[ Intersection[ Keys[recs], Range[Length@context["itemNames"]] ] ] == Length[recs]
                ),
            (*ELSE*)
            AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
          ];
      SMRMonUnit[res, context]
    ];

SMRMonScoredItemsQ[__][___] := $SMRMonFailure;


ClearAll[SMRMonScoredTagsQ]

SMRMonScoredTagsQ[$SMRMonFailure] := $SMRMonFailure;

SMRMonScoredTagsQ[xs_, context_Association] := SMRMonScoredTagsQ[xs][xs, context];

SMRMonScoredTagsQ[][xs_, context_Association] := SMRMonScoredTagsQ[xs][xs, context];

SMRMonScoredTagsQ[recs_Association][xs_, context_Association] :=
    Block[{res},
      res =
          If[ KeyExistsQ[context, "tags"],
            AssociationQ[recs] &&
                VectorQ[Values[recs], NumberQ] &&
                ( Length[ Intersection[ Keys[recs], Keys[context["tags"]] ] ] == Length[recs] ||
                    Length[ Intersection[ Keys[recs], Range[Length@context["tags"]] ] ] == Length[recs]
                ),
            (*ELSE*)
            AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
          ];
      SMRMonUnit[res, context]
    ];

SMRMonScoredTagsQ[__][___] := $SMRMonFailure;


(* Private function. *)
ClearAll[ScoredItemsQ]
ScoredItemsQ[recs_Association, context_Association] :=
    Block[{},
      Fold[ SMRMonBind, SMRMonUnit[None, context], { SMRMonScoredItemsQ[recs], SMRMonTakeValue}]
    ];
ScoredItemsQ[___] := (Echo["Wrong signature!", "ScoredItemsQ:"]; False);


(* Private function. *)
ClearAll[ScoredTagsQ]
ScoredTagsQ[prof_Association, context_Association] :=
    Block[{},
      Fold[ SMRMonBind, SMRMonUnit[None, context], { SMRMonScoredTagsQ[prof], SMRMonTakeValue}]
    ];
ScoredTagsQ[___] := (Echo["Wrong signature!", "ScoredTagsQ:"]; False);


(**************************************************************)
(* Creation                                                   *)
(**************************************************************)

ClearAll[SMRMonCreate]

(*SMRMonCreate::rneq = "The row names of SSparseMatrix objects are not the same."*)
(*SMRMonCreate::niid = "The specified item variable name is not one of the column names of the dataset."*)

Options[SMRMonCreate] = {"AddTagTypesToColumnNames"->False};

SMRMonCreate[$SMRMonFailure] := $SMRMonFailure;

SMRMonCreate[xs_, context_Association] := SMRMonCreate[][xs, context];

SMRMonCreate[ opts:OptionsPattern[] ][xs_, context_Association] :=
    Which[
      MatchQ[xs, _SSparseMatrix] || MatchQ[xs, Association[ (_->_SSparseMatrix) ..] ],
      SMRMonCreate[ xs, opts ][xs, context],

      AssociationQ[xs] && KeyExistsQ[xs, "data"] && KeyExistsQ[xs, "idColumnName"],
      SMRMonCreate[ xs["data"], xs["idColumnName"], opts ][xs, context],

      True,
      SMRMonCreate["Fail"][xs, context]
    ];

SMRMonCreate[ smat_SSparseMatrix, opts:OptionsPattern[] ][xs_, context_Association]:=
    SMRMonCreate[ <| "anonymous" -> smat |>, opts][xs, context];

SMRMonCreate[smats : Association[ (_->_SSparseMatrix) ..], opts:OptionsPattern[]][xs_, context_Association] :=
    Block[{tagTypeNames, rowNames, columnNames, splicedMat},

      tagTypeNames = Keys[smats];

      If[ !(Equal @@ rowNames),
        Echo["The row names of SSparseMatrix objects are not the same.", "SMRMonCreate:"];
        Return[$SMRMonFailure]
      ];

      splicedMat = ColumnBind[Values[smats]];

      rowNames = RowNames[splicedMat];

      columnNames = ColumnNames[splicedMat];

      SMRMonUnit[
        xs,
        Join[
          context,
          <|
            "itemNames" -> AssociationThread[rowNames->Range[Length[rowNames]]],
            "tags" -> AssociationThread[columnNames->Range[Length[columnNames]]],
            "tagTypeWeights" -> AssociationThread[Keys[smats], ConstantArray[1, Length[smats]]],
            "matrices" -> smats,
            "M01" -> splicedMat,
            "M" -> splicedMat
          |>
        ]
      ]
    ];


SMRMonCreate[ds_Dataset, itemVarName_String, opts:OptionsPattern[]][xs_, context_Association] :=
    Block[{ncol, varNames, smats, idPos, rng, addTagTypesToColumnNamesQ},

      addTagTypesToColumnNamesQ = TrueQ[OptionValue[SMRMonCreate, "AddTagTypesToColumnNames"]];

      ncol = Dimensions[ds][[2]];

      varNames = Normal[Keys[ds[1]]];

      idPos = Flatten[Position[varNames, itemVarName]];
      If[ Length[idPos]==0,
        Echo["The specified item variable name is not one of the column names of the dataset.", "SMRMonCreate:"];
        Return[$Failed]
      ];
      idPos = First[idPos];

      rng = Complement[Range[ncol], {idPos}];

      smats = ToSSparseMatrix /@ Table[CrossTabulate[ds[All, {idPos, i}]], {i, rng}];

      smats = AssociationThread[Flatten[List @@ varNames[[rng]] -> smats]];

      If[addTagTypesToColumnNamesQ,
        smats =
            Association @
                KeyValueMap[Function[{k,mat},
                  k -> ToSSparseMatrix[mat,
                    "ColumnNames" -> Map[ k <> ":" <> #&, ColumnNames[mat] ],
                    "RowNames" -> RowNames[mat]
                  ]], smats]
      ];

      SMRMonCreate[smats][xs, context]
    ];

SMRMonCreate[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a Dataset or an Association of SSparseMatrix objects. " <>
            "If the first argument is a Dataset the optional second argument is expected to be a column name in that Dataset. " <>
            "If no arguments are given the pipeline value is used.",
        "SMRMonCreate:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonApplyLocalWeightFunction                             *)
(**************************************************************)

ClearAll[SMRMonApplyLocalWeightFunction]

SMRMonApplyLocalWeightFunction[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyLocalWeightFunction[][xs_, context_Association] := $SMRMonFailure;

SMRMonApplyLocalWeightFunction[funcName_String][xs_, context_Association] :=
    Block[{mat, smats },

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyLocalWeightFunction:"];
        Return[$SMRMonFailure]
      ];

      smats =
          Map[
            ToSSparseMatrix[
              ApplyLocalTermFunction[SparseArray[#], funcName],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"]
          ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyLocalWeightFunction[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonApplyGlobalWeightFunction                            *)
(**************************************************************)

ClearAll[SMRMonApplyGlobalWeightFunction]

SMRMonApplyGlobalWeightFunction[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyGlobalWeightFunction[][xs_, context_Association] := $SMRMonFailure;

SMRMonApplyGlobalWeightFunction[funcName_String][xs_, context_Association] :=
    Block[{mat, smats},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyGlobalWeightFunction:"];
        Return[$SMRMonFailure]
      ];

      (* Quicker, but we have to keep the correspondence between "matrices" and "M" and "M01". *)
      (*mat = ApplyGlobalWeightFunction[ SparseArray[context["M"]], funcName];*)
      (*mat = ToSSparseMatrix[ mat, "RowNames" -> RowNames[context["M"]], "ColumnNames" -> ColumnNames[context["M"]] ];*)

      smats =
          Map[
            ToSSparseMatrix[
              ApplyGlobalTermFunction[SparseArray[#], funcName],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"] ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyGlobalWeightFunction[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonApplyNormalizationFunction                           *)
(**************************************************************)

ClearAll[SMRMonApplyNormalizationFunction]

SMRMonApplyNormalizationFunction[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyNormalizationFunction[][xs_, context_Association] := $SMRMonFailure;

SMRMonApplyNormalizationFunction[funcName_String][xs_, context_Association] :=
    Block[{mat, smats},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyNormalizationFunction:"];
        Return[$SMRMonFailure]
      ];

      smats =
          Map[
            ToSSparseMatrix[
              ApplyNormalizationFunction[SparseArray[#], funcName],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"] ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyNormalizationFunction[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonApplyNormalizationFunction                           *)
(**************************************************************)

ClearAll[SMRMonApplyTermWeightFunctions]

SMRMonApplyTermWeightFunctions[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyTermWeightFunctions[][xs_, context_Association] := $SMRMonFailure;

SMRMonApplyTermWeightFunctions[globalWeightFunction_String, localWeightFunction_String, normalizerFunction_String][xs_, context_Association] :=
    Block[{mat, smats},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyNormalizationFunction:"];
        Return[$SMRMonFailure]
      ];

      smats =
          Map[
            ToSSparseMatrix[
              WeightTerms[SparseArray[#], globalWeightFunction, localWeightFunction, normalizerFunction],
              "RowNames" -> RowNames[#],
              "ColumnNames" -> ColumnNames[#]
            ]&,
            context["matrices"] ];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "matrices" -> smats, "M" -> mat, "M01" -> mat |> ]]
    ];

SMRMonApplyTermWeightFunctions[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonRecommend                                            *)
(**************************************************************)

ClearAll[SMRMonRecommend];

Options[SMRMonRecommend] = {"RemoveHistory"->True, "ItemNames"->True};

SMRMonRecommend[$SMRMonFailure] := $SMRMonFailure;

SMRMonRecommend[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommend[ history:Association[ (_String->_?NumberQ) ... ], nRes_Integer, opts:OptionsPattern[]][xs_, context_Association] :=
    Block[{h},

      h = KeyMap[ context["itemNames"][#]&, history ];
      h = KeySelect[ h, IntegerQ ];

      If[ Length[h] < Length[history],
        Echo["Some of the item names are not known by the recommender.", "SMRMonRecommend:"];
      ];

      SMRMonRecommend[ Keys[h], Values[h], nRes, opts][xs, context]
    ];

SMRMonRecommend[ history:Association[ (_Integer->_?NumberQ) ... ], nRes_Integer, opts:OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ Keys[history], Values[history], nRes, opts][xs, context];

SMRMonRecommend[ itemIndices:{_Integer...}, nRes_Integer, opts:OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ itemIndices, ConstantArray[1,Length[itemIndices]], nRes, opts][xs, context];

SMRMonRecommend[ itemIndices:{_Integer...}, itemRatings:{_?NumberQ...}, nRes_Integer, opts:OptionsPattern[]][xs_, context_Association]:=
    Block[{inds, vec, maxScores, smat, recs, removeHistoryQ, itemNamesQ, rowNames},

      If[Length[itemIndices],
        Echo["Empty history as an argument.", "SMRMonRecommend:"];
        Return[<||>]
      ];

      removeHistoryQ = TrueQ[OptionValue[SMRMonRecommend, "RemoveHistory"]];
      itemNamesQ = TrueQ[OptionValue[SMRMonRecommend, "ItemNames"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommend:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      (*1*)
      vec = SparseArray[Thread[itemIndices->itemRatings],{Length[smat]}];

      (*2*)
      vec = smat.(vec.smat);

      (*3 and 4 and 5*)

      recs = Association[ Most[ArrayRules[vec]] ];

      recs = KeyMap[ First, recs ];

      recs = If[ removeHistoryQ, KeySelect[ recs, !MemberQ[itemIndices, #]&], recs ];

      recs = TakeLargest[recs, UpTo[nRes]];

      If[ itemNamesQ,
        rowNames = RowNames[context["M"]];
        recs = KeyMap[ rowNames[[#]]&, recs]
      ];

      SMRMonUnit[recs, context]

    ]/;Length[itemIndices]==Length[itemRatings];


SMRMonRecommend[___][__] := $SMRMonFailure;

ClearAll[SMRMonRecommendByHistory ]
SMRMonRecommendByHistory = SMRMonRecommend;


(**************************************************************)
(* SMRMonRecommendByProfile                                   *)
(**************************************************************)

ClearAll[SMRMonRecommendByProfile]

SMRMonRecommendByProfile[$SMRMonFailure] := $SMRMonFailure;

SMRMonRecommendByProfile[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommendByProfile[nRes_Integer][xs_, context_Association] :=
    Block[{},
      (* Without verification. *)
      SMRMonRecommendByProfile[xs, nRes][xs, context]
    ];

SMRMonRecommendByProfile[profileInds:{_Integer...}, profileScores:{_?NumberQ...}, nRes_Integer][xs_, context_Association]:=
    Block[{inds, vec, smat},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommend:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = SparseArray[Thread[profileInds->profileScores],{Dimensions[smat][[2]]}];

      SMRMonRecommendByProfile[vec, nRes][xs, context]
    ]/;Length[profileInds]==Length[profileScores];

SMRMonRecommendByProfile[profileVec_SparseArray, nRes_Integer][xs_, context_Association]:=
    Block[{inds, vec, smat, recs},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommend:"];
        Return[$SMRMonFailure]
      ];

      If[ ColumnsCount[context["M"]] != Dimensions[profileVec][[1]],
        Echo["The number of columns of the recommender matrix is different than the length of the profile vector argument.", "SMRMonRecommendByProfile:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = smat.profileVec;
      recs = Association[ Most[ArrayRules[vec]] ];

      recs = KeyMap[ First, recs ];

      recs = TakeLargest[recs, UpTo[nRes]];
      SMRMonUnit[recs, context]

    ];

SMRMonRecommendByProfile[tagsArg:(_Association | _List), nRes_Integer][xs_, context_Association] :=
    Block[{p},

      If[ AssociationQ[tagsArg] && !ScoredTagsQ[tagsArg, context] ||
          ListQ[tagsArg] && !ScoredTagsQ[AssociationThread[tagsArg->1], context] ,
        Echo["The first argument is not an association of tags->score elements or a list of tags.", "SMRMonRecommendByProfile:"];
        Return[$SMRMonFailure]
      ];

      p = SMRMonToProfileVector[tagsArg][xs, context];
      SMRMonRecommendByProfile[p[[1]], nRes][xs, context]
    ];

SMRMonRecommendByProfile[___][__] := $SMRMonFailure;


(**************************************************************)
(* Convert recommendations to a Dataset                       *)
(**************************************************************)

ClearAll[SMRMonToItemsDataset]

SMRMonToItemsDataset[$SMRMonFailure] := $SMRMonFailure;

SMRMonToItemsDataset[xs_, context_Association] := SMRMonToItemsDataset[xs][xs, context];

SMRMonToItemsDataset[][xs_, context_Association] := SMRMonToItemsDataset[xs][xs, context];

SMRMonToItemsDataset[recs_Association][xs_, context_Association] :=
    Block[{res},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonToItemsDataset:"];
        Return[$SMRMonFailure]
      ];

      Which[
        ScoredItemIndexesQ[recs, context],
        res = Dataset[Transpose[{ Values[recs], Keys[recs], RowNames[context["M"]][[Keys[recs]]] }]];
        res = res[All, AssociationThread[{"Score", "Index", "Item"} -> #]&];
        SMRMonUnit[res, context],

        Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonScoredItemsQ[recs], SMRMonTakeValue}],
        res = Dataset[Transpose[{ Values[recs], context["itemNames"][#]& /@ Keys[recs], Keys[recs]}]];
        res = res[All, AssociationThread[{"Score", "Index", "Item"} -> #]&];
        SMRMonUnit[res, context],

        True,
        Return[$SMRMonFailure]
      ]
    ];

SMRMonToItemsDataset[__][___] := $SMRMonFailure;


(**************************************************************)
(* Join recommendations with a Dataset                       *)
(**************************************************************)

ClearAll[SMRMonJoinAcross]

Options[SMRMonJoinAcross] = {"DropJoiningColumnName"->True};

SMRMonJoinAcross[$SMRMonFailure] := $SMRMonFailure;

SMRMonJoinAcross[xs_, context_Association] := $SMRMonFailure;

SMRMonJoinAcross[][xs_, context_Association] := $SMRMonFailure;

SMRMonJoinAcross[dsArg_Dataset, byColName_?AtomQ, opts:OptionsPattern[]][xs_, context_Association] :=
    Block[{ds = dsArg, dsRecs, res, dropQ},

      dropQ = TrueQ[OptionValue[SMRMonJoinAcross, "DropJoiningColumnName"]];

      dsRecs = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonToItemsDataset, SMRMonTakeValue } ];

      If[ TrueQ[dsRecs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[ !VectorQ[Normal[ds[All, byColName]], StringQ],
        Echo["The joining column, \"" <> byColName <> "\", is expected to consist of strings.", "SMRMonJoinAcross:"];

        If[ VectorQ[Normal[ds[All, byColName]], IntegerQ],
          Echo["Proceeding by converting the integers in \"" <> byColName <> "\" into strings.", "SMRMonJoinAcross:"];
          ds = ds[All, Join[#, <| byColName -> ToString[#[byColName]]|>]& ]
          ,
          (*ELSE*)
          Return[$SMRMonFailure]
        ];
      ];

      res = JoinAcross[ Normal[dsRecs[All, {"Score", "Item"}]], Normal[ds], Key["Item"]->Key[byColName] ];
      res = Dataset[res][SortBy[-#Score &]];

      If[ dropQ, res = res[All, KeyDrop[#,byColName]&] ];

      SMRMonUnit[res, context]
    ];

SMRMonJoinAcross[__][___] := $SMRMonFailure;


(**************************************************************)
(* SMRMonProfile                                              *)
(**************************************************************)

(* Essentially repeating SMRMonRecommend but with minor changes. *)
(*ClearAll[SMRMonProfile]*)
(*SMRMonProfile[ ]*)

(*SMRMonRecommendByProfile[___][__] := $SMRMonFailure;*)


(**************************************************************)
(* SMRMonToProfileVector                                      *)
(**************************************************************)

ClearAll[SMRMonToProfileVector]

SMRMonToProfileVector[$SMRMonFailure] := $SMRMonFailure;

SMRMonToProfileVector[xs_, context_Association] := $SMRMonFailure;

SMRMonToProfileVector[ scoredTags:Association[ (_Integer->_?NumberQ)..] ][xs_, context_Association] :=
    Block[{},
      SMRMonUnit[ SparseArray[ Normal@scoredTags, ColumnsCount[context["M"]] ], context]
    ];

SMRMonToProfileVector[ scoredTags:Association[ (_String->_?NumberQ)..] ][xs_, context_Association] :=
    SMRMonToProfileVector[ KeyMap[ context["tags"][#]&, scoredTags] ][xs, context]

SMRMonToProfileVector[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonSetTagTypeWeights                                    *)
(**************************************************************)

ClearAll[SMRMonSetTagTypeWeights]

SMRMonSetTagTypeWeights[$SMRMonFailure] := $SMRMonFailure;

SMRMonSetTagTypeWeights[xs_, context_Association] := $SMRMonFailure;

SMRMonSetTagTypeWeights[ defaultValue_?NumberQ ][xs_, context_Association] :=
    SMRMonSetTagTypeWeights[ <||>, defaultValue][xs, context];

SMRMonSetTagTypeWeights[ scoredTagTypes_Association ][xs_, context_Association] :=
    SMRMonSetTagTypeWeights[ scoredTagTypes, 1][xs, context];

SMRMonSetTagTypeWeights[ scoredTagTypesArg:Association[ (_String->_?NumberQ)...], defaultValue_?NumberQ ][xs_, context_Association] :=
    Block[{ scoredTagTypes = scoredTagTypesArg, smats, mat},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonSetTagTypeWeights:"];
        Return[$SMRMonFailure]
      ];

      scoredTagTypes = Join[ AssociationThread[ Keys[context["matrices"]] -> defaultValue ], scoredTagTypes ];

      smats = Association[KeyValueMap[ #1 -> scoredTagTypes[#1] * #2 &, context["matrices"]]];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "M" -> mat |> ]]

    ];

SMRMonSetTagTypeWeights[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an Association of scored tag-types. " <>
            "The second, optional argument is expected to be a number.",
        "SMRMonSetTagTypeWeights:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonSetTagWeights                                        *)
(**************************************************************)

ClearAll[SMRMonSetTagWeights]

SMRMonSetTagWeights[$SMRMonFailure] := $SMRMonFailure;

SMRMonSetTagWeights[xs_, context_Association] := $SMRMonFailure;

SMRMonSetTagWeights[ defaultValue_?NumberQ ][xs_, context_Association] :=
    SMRMonSetTagWeights[ <||>, defaultValue][xs, context];

SMRMonSetTagWeights[ scoredTags_Association ][xs_, context_Association] :=
    SMRMonSetTagWeights[ scoredTags, 1][xs, context];

SMRMonSetTagWeights[ scoredTagsArg:Association[ (_String->_?NumberQ)...], defaultValue_?NumberQ ][xs_, context_Association] :=
    Block[{ scoredTags = scoredTagsArg, mat},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"matrix\".)", "SMRMonSetTagWeights:"];
        Return[$SMRMonFailure]
      ];

      scoredTags = Join[ AssociationThread[ ColumnNames[context["M"]] -> defaultValue ], scoredTags ];

      mat = DiagonalMatrix[ SparseArray[ scoredTags[#]& /@ ColumnNames[context["M"]] ] ];

      mat = context["M"] . mat;

      SMRMonUnit[xs, Join[ context, <| "M" -> mat |> ]]

    ];

SMRMonSetTagWeights[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an Association of scored tags." <>
            "The second, optional argument is expected to be a number.",
        "SMRMonSetTagWeights:"];
      $SMRMonFailure
    ];


End[]; (* `Private` *)

EndPackage[]