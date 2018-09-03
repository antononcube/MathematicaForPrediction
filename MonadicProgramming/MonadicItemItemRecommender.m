(*
    Monadic Item-Item Recommender Mathematica package
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
(* :Title: MonadicItemItemRecommender *)
(* :Context: MonadicItemItemRecommender` *)
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
*   If we allow empty IIR monad structure creation IIRMonUnit[] then we have to be able
*   to ingest matrices and datasets for the creation population of the IIR data structures.
*
*
*   # Implementation consideration
*
*   Note that the a Sparse Matrix Recommender for IIR is made at first, but the mapping to
*   Stream Blending Recommender is planned to be added soon after. See [2].
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
*
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

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicItemItemRecommender`"];

$IIRMonFailure::usage = "Failure symbol of IIRMon."

IIRMonScoredItemsQ::usage = "True if the argument is an association with item names or item indices as keys \
and numbers as values."

IIRMonScoredTagsQ::usage = "True if the argument is an association with tags or tag indices as keys \
and numbers as values."

IIRMonCreateFromMatrices::usage = "Creates the recommender structures from an association of (sparse) matrices."

IIRMonCreate::usage = "Creates the recommender structures from a transactions Dataset and a specifications Dataset."

IIRMonRecommend::usage = "Recommends items based on history."

IIRMonRecommendByHistory::usage = "Recommends items based on history."

IIRMonRecommendByProfile::usage = "Recommends items based on profile."

IIRMonToProfileVector::usage = "Makes a profile vector from an argument that is a list of tags or an Association object."

IIRMonFromProfileVector::usage = "Makes a profile association from a profile vector argument."

IIRMonToItemsDataset::usage = "Converts a recommendations association into a Dataset object."

IIRMonSetTagTypeWeights::usage = "Sets weights (significance factors) to the IIR tag types."

IIRMonSetTagWeights::usage = "Sets weights (significance factors) to the IIR tags."

IIRMonClassify::usage = "Uses IIR as a classifier for specified label tag-type over a vector or a matrix."

IIRMonSetClassificationParameters::usage = "Sets the parameters to be used by IIRMonClassify."

IIRMonProveByHistory::usage = "Proof the recommendations using consumption history."

IIRMonProveByProfile::usage = "Proof the recommendations using consumption profile."

IIRMonTakeMatrices::usage = "Gives an association with the tag (sparse) matrices."

IIRMonTakeMatrix::usage = "Gives the recommendation matrix (SSparseMatrix)."

IIRMonTakeItemNames::usage = "Gives the item names. (Row names of the recommender matrix.)"

IIRMonTakeTags::usage = "Gives the tags. (Column names of the recommender matrix.)"

IIRMonTakeTagTypeWeights::usage = "Takes the tag-type weights."

IIRMonTakeTagTypes::usage = "Takes the tag-types."

Begin["`Private`"];


Needs["MathematicaForPredictionUtilities`"]
Needs["CrossTabulate`"]
Needs["StateMonadCodeGenerator`"]
Needs["SSparseMatrix`"]


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of IIRMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicItemItemRecommender`IIRMon", "FailureSymbol" -> $IIRMonFailure, "StringContextNames" -> False ]


(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)

ClearAll[IIRMonSetItemNames]

IIRMonSetItemNames[$IIRMonFailure] := $IIRMonFailure;

(* Here we can/have to have a correctness check. It is one of the advantages to SSparseMatrix. *)
(*IIRMonSetItemNames[names_][xs_, context_Association] :=*)
    (*IIRMonUnit[ xs, Join[context, <|"itemNames"->names|>] ];*)

ClearAll[IIRMonGetTagTypeWeights]
IIRMonGetTagTypeWeights[$IIRMonFailure] := $IIRMonFailure;
IIRMonGetTagTypeWeights[][$IIRMonFailure] := $IIRMonFailure;
IIRMonGetTagTypeWeights[][xs_, context_Association] := Lookup[context, "tagTypeWeights"];


ClearAll[IIRMonGetTagTypes]
IIRMonGetTagTypes[$IIRMonFailure] := $IIRMonFailure;
IIRMonGetTagTypes[][$IIRMonFailure] := $IIRMonFailure;
IIRMonGetTagTypes[][xs_, context_Association] := Keys[Lookup[context, "tagTypeWeights"]];


ClearAll[IIRMonTakeMatrix]
IIRMonTakeMatrix[$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeMatrix[][$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeMatrix[xs_, context_] := IIRMonTakeMatrix[][xs, context];
IIRMonTakeMatrix[][xs_, context_Association] := Lookup[context, "M", $IIRMonFailure];
IIRMonTakeMatrix[__][___] := $IIRMonFailure;


ClearAll[IIRMonTakeMatrices]
IIRMonTakeMatrices[$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeMatrices[][$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeMatrices[xs_, context_] := IIRMonTakeMatrices[][xs, context];
IIRMonTakeMatrices[][xs_, context_Association] := Lookup[context, "matrices", $IIRMonFailure];
IIRMonTakeMatrices[__][___] := $IIRMonFailure;


ClearAll[IIRMonTakeItemNames]
IIRMonTakeItemNames[$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeItemNames[][$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeItemNames[xs_, context_] := IIRMonTakeItemNames[][xs, context];
IIRMonTakeItemNames[][xs_, context_Association] := Lookup[context, "itemNames", $IIRMonFailure];
IIRMonTakeItemNames[__][___] := $IIRMonFailure;


ClearAll[IIRMonTakeTags]
IIRMonTakeTags[$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeTags[][$IIRMonFailure] := $IIRMonFailure;
IIRMonTakeTags[xs_, context_] := IIRMonTakeTags[][xs, context];
IIRMonTakeTags[][xs_, context_Association] := Lookup[context, "tags", $IIRMonFailure];
IIRMonTakeTags[__][___] := $IIRMonFailure;


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

ClearAll[IIRMonScoredItemsQ]

IIRMonScoredItemsQ[$IIRMonFailure] := $IIRMonFailure;

IIRMonScoredItemsQ[xs_, context_Association] := IIRMonScoredItemsQ[xs][xs, context];

IIRMonScoredItemsQ[][xs_, context_Association] := IIRMonScoredItemsQ[xs][xs, context];

IIRMonScoredItemsQ[recs_Association][xs_, context_Association] :=
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
      IIRMonUnit[res, context]
    ];

IIRMonScoredItemsQ[__][___] := $IIRMonFailure;


ClearAll[IIRMonScoredTagsQ]

IIRMonScoredTagsQ[$IIRMonFailure] := $IIRMonFailure;

IIRMonScoredTagsQ[xs_, context_Association] := IIRMonScoredTagsQ[xs][xs, context];

IIRMonScoredTagsQ[][xs_, context_Association] := IIRMonScoredTagsQ[xs][xs, context];

IIRMonScoredTagsQ[recs_Association][xs_, context_Association] :=
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
      IIRMonUnit[res, context]
    ];

IIRMonScoredTagsQ[__][___] := $IIRMonFailure;


(**************************************************************)
(* Creation                                                   *)
(**************************************************************)

ClearAll[IIRMonCreate]

(*IIRMonCreate::rneq = "The row names of SSparseMatrix objects are not the same."*)
(*IIRMonCreate::niid = "The specified item variable name is not one of the column names of the dataset."*)

IIRMonCreate[$IIRMonFailure] := $IIRMonFailure;

IIRMonCreate[xs_, context_Association] := $IIRMonFailure;

IIRMonCreate[smats : Association[ (_->_SSparseMatrix) ..]][xs_, context_Association] :=
    Block[{tagTypeNames, rowNames, columnNames, splicedMat},

      tagTypeNames = Keys[smats];

      rowNames = RowNames /@ Values[smats];

      If[ !(Equal @@ rowNames),
        Echo["The row names of SSparseMatrix objects are not the same.", "IIRMonCreate:"];
        Return[$IIRMonFailure]
      ];

      splicedMat = ColumnBind[Values[smats]];

      rowNames = RowNames[splicedMat];

      columnNames = ColumnNames[splicedMat];

      IIRMonUnit[
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


IIRMonCreate[ds_Dataset, itemVarName_String ][xs_, context_Association] :=
    Block[{ncol, varNames, smats, idPos, rng},

      ncol = Dimensions[ds][[2]];

      varNames = Normal[Keys[ds[1]]];

      idPos = Flatten[Position[varNames, itemVarName]];
      If[ Length[idPos]==0,
        Echo["The specified item variable name is not one of the column names of the dataset.", "IIRMonCreate:"];
        Return[$Failed]
      ];
      idPos = First[idPos];

      rng = Complement[Range[ncol], {idPos}];

      smats = ToSSparseMatrix /@
          Table[CrossTabulate[ds[All, {idPos, i}]], {i, rng}];

      smats = AssociationThread[Flatten[List @@ varNames[[rng]] -> smats]];

      IIRMonCreate[smats][xs, context]
    ];

IIRMonCreate[___][__] := $IIRMonFailure;


(**************************************************************)
(* IIRMonRecommend                                            *)
(**************************************************************)

ClearAll[IIRMonRecommend];

Options[IIRMonRecommend] = {"RemoveHistory"->True, "ItemNames"->True};

IIRMonRecommend[$IIRMonFailure] := $IIRMonFailure;

IIRMonRecommend[xs_, context_Association] := $IIRMonFailure;

IIRMonRecommend[ history:Association[ (_String->_?NumberQ) ... ], nRes_Integer, opts:OptionsPattern[]][xs_, context_Association] :=
    Block[{h},

      h = KeyMap[ context["itemNames"][#]&, history ];
      h = KeySelect[ h, IntegerQ ];

      If[ Length[h] < Length[history],
        Echo["Some of item names are not known by the recommender.", "IIRMonRecommend:"];
      ];

      (*If[ Length[h] == 0,*)
        (*Echo["Obtained empty history.", "IIRMonRecommend:"];*)
        (*Return[<||>]*)
      (*];*)

      IIRMonRecommend[ Keys[h], Values[h], nRes, opts][xs, context]
    ];

IIRMonRecommend[ history:Association[ (_Integer->_?NumberQ) ... ], nRes_Integer, opts:OptionsPattern[]][xs_, context_Association] :=
    IIRMonRecommend[ Keys[history], Values[history], nRes, opts][xs, context];

IIRMonRecommend[ itemIndices:{_Integer...}, nRes_Integer, opts:OptionsPattern[]][xs_, context_Association] :=
    IIRMonRecommend[ itemIndices, ConstantArray[1,Length[itemIndices]], nRes, opts][xs, context];

IIRMonRecommend[ itemIndices:{_Integer...}, itemRatings:{_?NumberQ...}, nRes_Integer, opts:OptionsPattern[]][xs_, context_Association]:=
    Block[{inds, vec, maxScores, smat, recs, removeHistoryQ, itemNamesQ, rowNames},

      If[Length[itemIndices],
        Echo["Empty history as an argument.", "IIRMonRecommend:"];
        Return[<||>]
      ];

      removeHistoryQ = TrueQ[OptionValue[IIRMonRecommend, "RemoveHistory"]];
      itemNamesQ = TrueQ[OptionValue[IIRMonRecommend, "ItemNames"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "IIRRecommend:"];
        Return[$IIRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      (*1*)
      vec = SparseArray[Thread[itemIndices->itemRatings],{Length[smat]}];

      (*2*)
      vec = smat.(vec.smat);

      (*3 and 4 and 5*)

      recs = Association[ Most[ArrayRules[vec]] ];

      recs = KeyMap[ First, recs ];

      recs = If[ removeHistoryQ, KeySelect[  recs, !MemberQ[itemIndices, #]&] ];

      recs = TakeLargest[recs, UpTo[nRes]];

      If[ itemNamesQ,
        rowNames = RowNames[context["M"]];
        recs = KeyMap[ rowNames[[#]]&, recs]
      ];

      IIRMonUnit[recs, context]

    ]/;Length[itemIndices]==Length[itemRatings];


IIRMonRecommend[___][__] := $IIRMonFailure;

ClearAll[IIRMonRecommendByHistory ]
IIRMonRecommendByHistory = IIRMonRecommend;


(**************************************************************)
(* IIRMonRecommendByProfile                                   *)
(**************************************************************)

ClearAll[IIRMonRecommendByProfile]

IIRMonRecommendByProfile[$IIRMonFailure] := $IIRMonFailure;

IIRMonRecommendByProfile[xs_, context_Association] := $IIRMonFailure;

IIRMonRecommendByProfile[nRes_Integer][xs_, context_Association] :=
    Block[{},
      (* Without verification. *)
      IIRMonRecommendByProfile[xs, nRes][xs, context]
    ];

IIRMonRecommendByProfile[profileInds:{_Integer...}, profileScores:{_?NumberQ...}, nRes_Integer][xs_, context_Association]:=
    Block[{inds, vec, smat},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "IIRRecommend:"];
        Return[$IIRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = SparseArray[Thread[profileInds->profileScores],{Dimensions[smat][[2]]}];

      IIRMonRecommendByProfile[vec, nRes][xs, context]
    ]/;Length[profileInds]==Length[profileScores];

IIRMonRecommendByProfile[profileVec_SparseArray, nRes_Integer][xs_, context_Association]:=
    Block[{inds, vec, smat, recs},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "IIRRecommend:"];
        Return[$IIRMonFailure]
      ];

      If[ ColumnsCount[context["M"]] != Dimensions[profileVec][[1]],
        Echo["The number of columns of the recommender matrix is different than the length of the profile vector argument.", "IIRMonRecommendByProfile:"];
        Return[$IIRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = smat.profileVec;
      recs = Association[ Most[ArrayRules[vec]] ];

      recs = KeyMap[ First, recs ];

      recs = TakeLargest[recs, UpTo[nRes]];
      IIRMonUnit[recs, context]

    ];

IIRMonRecommendByProfile[tagsArg:(_Association | _List), nRes_Integer][xs_, context_Association] :=
    Block[{p},

      If[ AssociationQ[tagsArg] && !IIRMonScoredTagsQ[tagsArg] ||
          ListQ[tagsArg] && !IIRMonScoredTagsQ[AssociationThread[tags->1]] ,
        Echo["The first argument is not an assocation of tags->score elements or a list of tags.", "IIRMonRecommendByProfile:"];
        Return[$IIRMonFailure]
      ];

      p = IIRMonToProfileVector[tagsArg][xs, context];
      IIRMonRecommendByProfile[p[[1]], nRes][xs, context]
    ];

IIRMonRecommendByProfile[___][__] := $IIRMonFailure;


(**************************************************************)
(* Convert recommendations to a Dataset                       *)
(**************************************************************)

ClearAll[IIRMonToItemsDataset]

IIRMonToItemsDataset[$IIRMonFailure] := $IIRMonFailure;

IIRMonToItemsDataset[xs_, context_Association] := IIRMonToItemsDataset[xs][xs, context];

IIRMonToItemsDataset[][xs_, context_Association] := IIRMonToItemsDataset[xs][xs, context];

IIRMonToItemsDataset[recs_Association][xs_, context_Association] :=
    Block[{res},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "IIRMonToItemsDataset:"];
        Return[$IIRMonFailure]
      ];

      Which[
        ScoredItemIndexesQ[recs, context],
        res = Dataset[Transpose[{ Values[recs], Keys[recs], RowNames[context["M"]][[Keys[recs]]] }]];
        res = res[All, AssociationThread[{"Score", "Index", "Item"} -> #]&];
        IIRMonUnit[res, context],

        Fold[ IIRMonBind, IIRMonUnit[xs, context], {IIRMonScoredItemsQ[recs], IIRMonTakeValue}],
        res = Dataset[Transpose[{ Values[recs], context["itemNames"][#]& /@ Keys[recs], Keys[recs]}]]; Print["here"];
        res = res[All, AssociationThread[{"Score", "Index", "Item"} -> #]&];
        IIRMonUnit[res, context],

        True,
        Return[$IIRMonFailure]
      ]
    ];

IIRMonToItemsDataset[__][___] := $IIRMonFailure;


(**************************************************************)
(* IIRMonProfile                                              *)
(**************************************************************)

(*ClearAll[IIRMonProfile]*)
(*IIRMonProfile[ ]*)

(*IIRMonRecommendByProfile[___][__] := $IIRMonFailure;*)


(**************************************************************)
(* IIRMonToProfileVector                                      *)
(**************************************************************)

ClearAll[IIRMonToProfileVector]

IIRMonToProfileVector[$IIRMonFailure] := $IIRMonFailure;

IIRMonToProfileVector[xs_, context_Association] := $IIRMonFailure;

IIRMonToProfileVector[ scoredTags:Association[ (_Integer->_?NumberQ)..] ][xs_, context_Association] :=
    Block[{},
      IIRMonUnit[ SparseArray[ Normal@scoredTags, ColumnsCount[context["M"]] ], context]
    ];

IIRMonToProfileVector[ scoredTags:Association[ (_String->_?NumberQ)..] ][xs_, context_Association] :=
    IIRMonToProfileVector[ KeyMap[ context["tags"][#]&, scoredTags] ][xs, context]

IIRMonToProfileVector[___][__] := $IIRMonFailure;


End[]; (* `Private` *)

EndPackage[]