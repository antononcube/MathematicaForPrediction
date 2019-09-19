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

If[Length[DownValues[OutlierIdentifiers`OutlierIdentifier]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicSparseMatrixRecommender`"];

$SMRMonFailure::usage = "Failure symbol of SMRMon.";

SMRMonScoredItemsQ::usage = "True if the argument is an association with item names or item indices as keys \
and numbers as values.";

SMRMonScoredTagsQ::usage = "True if the argument is an association with tags or tag indices as keys \
and numbers as values.";

SMRMonCreate::usage = "Creates the recommender structures from a transactions Dataset or an association of sparse matrices.";

SMRMonCreateFromWideForm::usage = "Creates the recommender structures from a transactions Dataset.";

SMRMonCreateFromLongForm::usage = "Creates the recommender structures from a long form Dataset -- \
each row is expected to have values corresponding to item ID, tag type, tag, weight.";

SMRMonApplyLocalWeightFunction::usage = "Applies a specified local weight function to the entries \
of the contingency matrix.";

SMRMonApplyGlobalWeightFunction::usage = "Applies a specified global weight function to the entries \
of the contingency matrix.";

SMRMonApplyNormalizationFunction::usage = "Applies a specified normalization function to the entries \
of the contingency matrix.";

SMRMonApplyTermWeightFunctions::usage = "Apply term weight functions to entries of the recommender matrix.";

SMRMonRecommend::usage = "Recommends items based on history.";

SMRMonRecommendByHistory::usage = "Recommends items based on history.";

SMRMonRecommendByProfile::usage = "Recommends items based on profile.";

SMRMonRecommendByCorrelation::usage = "Recommends items based on a correlation matrix. \
(The context value for the key \"timeSeriesMatrix\" should have the same dimensions and row names as the recommendation matrix.)";

SMRMonToProfileVector::usage = "Makes a profile vector from an argument that is a list of tags or an Association object.";

SMRMonFromProfileVector::usage = "Makes a profile association from a profile vector argument.";

SMRMonProfile::usage = "Profile based on history.";

SMRMonToItemsDataset::usage = "Converts a recommendations association into a Dataset object.";

SMRMonJoinAcross::usage = "Joins a recommendations association with a given Dataset object.";

SMRMonJoin::usage = "Joins the recommender with another recommender. \
(By column-binding the corresponding tag-type sub-matrices.)";

SMRMonRowBind::usage = "Row-binds the recommender with another recommender. \
(By row-binding the corresponding tag-type sub-matrices.)";

SMRMonSetTagTypeWeights::usage = "Sets weights (significance factors) to the IIR tag types.";

SMRMonSetTagWeights::usage = "Sets weights (significance factors) to the IIR tags.";

SMRMonSetTimeSeriesMatrix::usage = "Sets a time series matrix to be used with SMRMonRecommendByCorrelation.";

SMRMonClassifyOriginal::usage = "Uses IIR as a classifier for specified label tag-type over a vector or a matrix.";

SMRMonClassify::usage = "Uses IIR as a classifier for specified label tag-type over a vector or a matrix.";

SMRMonSetClassificationParameters::usage = "Sets the parameters to be used by SMRMonClassifyOriginal.";

SMRMonProveByHistory::usage = "Proof the recommendations using consumption history.";

SMRMonProveByProfile::usage = "Proof the recommendations using consumption profile.";

SMRMonTakeMatrices::usage = "Gives an association with the tag (sparse) matrices.";

SMRMonTakeMatrix::usage = "Gives the recommendation matrix (SSparseMatrix).";

SMRMonTakeItemNames::usage = "Gives the item names. (Row names of the recommender matrix.)";

SMRMonTakeTags::usage = "Gives the tags. (Column names of the recommender matrix.)";

SMRMonTakeTagTypeWeights::usage = "Takes the tag-type weights.";

SMRMonTakeTagTypes::usage = "Takes the tag-types.";

SMRMonTakeMatrixDataset::usage = "Take the Dataset object corresponding to the recommendation matrix.";

SMRMonMetadataProofs::usage = "Metadata proofs for a recommended item and a profile. \
(Tags from item's profile that are found in the given profile.)";

SMRMonHistoryProofs::usage = "History proofs for a recommended item and scored history items.";

SMRMonEchoDataSummary::usage = "Echoes summary of the dataset.";

SMRMonGetProperty::usage = "Get a recommender property.";

SMRMonGetMatrixProperty::usage = "Get a recommender matrix property.";

Begin["`Private`"];


Needs["MathematicaForPredictionUtilities`"];
Needs["CrossTabulate`"];
Needs["StateMonadCodeGenerator`"];
Needs["SSparseMatrix`"];
Needs["DocumentTermMatrixConstruction`"];
Needs["OutlierIdentifiers`"];


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of SMRMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicSparseMatrixRecommender`SMRMon", "FailureSymbol" -> $SMRMonFailure, "StringContextNames" -> False ];

GenerateMonadAccessors[
  "MonadicSparseMatrixRecommender`SMRMon",
  {"data", "M", "M01", "matrices", "itemNames", "tags", "tagTypeWeights", "timeSeriesMatrix" },
  "FailureSymbol" -> $SMRMonFailure ];

GenerateMonadAccessors[
  "MonadicSparseMatrixRecommender`SMRMon",
  {"M", "M01"},
  "DecapitalizeElementName" -> False,
  "FailureSymbol" -> $SMRMonFailure ];

(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)

(* Here we can/have to have a correctness check. It is one of the advantages to SSparseMatrix. *)
(*SMRMonSetItemNames[names_][xs_, context_Association] :=*)
(*SMRMonUnit[ xs, Join[context, <|"itemNames"->names|>] ];*)

Clear[SMRMonTakeTagTypes];
SMRMonTakeTagTypes[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeTagTypes[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeTagTypes[xs_, context_Association] := SMRMonTakeTagTypes[][xs, context];
SMRMonTakeTagTypes[][xs_, context_Association] := Keys[Lookup[context, "matrices"]];
SMRMonTakeTagTypes[__][___] := $SMRMonFailure;

Clear[SMRMonTakeMatrix];
SMRMonTakeMatrix = SMRMonTakeM;

Clear[SMRMonTakeMatrixDataset];
SMRMonTakeMatrixDataset[$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrixDataset[][$SMRMonFailure] := $SMRMonFailure;
SMRMonTakeMatrixDataset[xs_, context_] := SMRMonTakeMatrixDataset[][xs, context];
SMRMonTakeMatrixDataset[][xs_, context_Association] :=
    Block[{smats},
      smats = SMRMonTakeMatrices[][xs, context];
      If[ TrueQ[smats === $SMRMonFailure],
        $SMRMonFailure,
        (*ELSE*)
        smats =
            Map[
              Dataset[SSparseMatrixToTriplets[#]][All, AssociationThread[{"Item", "Tag", "Weight"} -> #]&]&,
              smats
            ];
        smats = KeyValueMap[ Function[{k, v}, v[All, Join[#, <|"TagType" -> k|>]&]], smats];
        (Join @@ smats)[All, {"Item", "TagType", "Tag", "Weight"}]
      ]
    ];
SMRMonTakeMatrixDataset[__][___] := $SMRMonFailure;


(**************************************************************)
(* Predicates                                                 *)
(**************************************************************)

Clear[ScoredItemIndexesQ];

ScoredItemIndexesQ[recs_Association, context_Association] :=
    If[ KeyExistsQ[context, "M"],
      AssociationQ[recs] &&
          VectorQ[Values[recs], NumberQ] &&
          Apply[And, Map[TrueQ[0 < # <= RowsCount[context["M"]]]&, Keys[recs]] ],
      True,
      (*ELSE*)
      AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
    ];

ScoredItemIndexesQ[___] := False;

Clear[SMRMonScoredItemsQ];

SMRMonScoredItemsQ[$SMRMonFailure] := $SMRMonFailure;

SMRMonScoredItemsQ[xs_, context_Association] := SMRMonScoredItemsQ[xs][xs, context];

SMRMonScoredItemsQ[][xs_, context_Association] := SMRMonScoredItemsQ[xs][xs, context];

SMRMonScoredItemsQ[recs_Association][xs_, context_Association] :=
    Block[{res},
      res =
          If[ KeyExistsQ[context, "itemNames"],
            AssociationQ[recs] &&
                VectorQ[Values[recs], NumberQ] &&
                ( VectorQ[context["itemNames"] /@ Keys[recs], IntegerQ] ||
                    1 <= Min[Keys[recs]] && Max[Keys[recs]] <= Length[context["itemNames"]]
                ),
            (*ELSE*)
            AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
          ];
      SMRMonUnit[res, context]
    ];

SMRMonScoredItemsQ[__][___] := $SMRMonFailure;


Clear[SMRMonScoredTagsQ];

SMRMonScoredTagsQ[$SMRMonFailure] := $SMRMonFailure;

SMRMonScoredTagsQ[xs_, context_Association] := SMRMonScoredTagsQ[xs][xs, context];

SMRMonScoredTagsQ[][xs_, context_Association] := SMRMonScoredTagsQ[xs][xs, context];

SMRMonScoredTagsQ[recs_Association][xs_, context_Association] :=
    Block[{res},
      res =
          If[ KeyExistsQ[context, "tags"],
            AssociationQ[recs] &&
                VectorQ[Values[recs], NumberQ] &&
                ( VectorQ[context["tags"] /@ Keys[recs], IntegerQ] ||
                    1 <= Min[Keys[recs]] && Max[Keys[recs]] <= Length[context["tags"]]
                ),
            (*ELSE*)
            AssociationQ[recs] && VectorQ[Values[recs], NumberQ]
          ];
      SMRMonUnit[res, context]
    ];

SMRMonScoredTagsQ[__][___] := $SMRMonFailure;


(* Private function. *)
Clear[ScoredItemsQ];

ScoredItemsQ[recs_Association, context_Association] :=
    Block[{},
      TrueQ[ Fold[ SMRMonBind, SMRMonUnit[None, context], { SMRMonScoredItemsQ[recs], SMRMonTakeValue}] ]
    ];

ScoredItemsQ[_, context_Association] := False;

ScoredItemsQ[___] := (Echo["Wrong signature!", "ScoredItemsQ:"]; False);


(* Private function. *)
Clear[ScoredTagsQ];

ScoredTagsQ[prof_Association, context_Association] :=
    Block[{},
      TrueQ[ Fold[ SMRMonBind, SMRMonUnit[None, context], { SMRMonScoredTagsQ[prof], SMRMonTakeValue}] ]
    ];

ScoredTagsQ[_, context_Association] := False;

ScoredTagsQ[___] := (Echo["Wrong signature!", "ScoredTagsQ:"]; False);

(* Private function. *)
Clear[DatasetWithScoredItemsQ];

DatasetWithScoredItemsQ[recs_Dataset, context_Association] :=
    Block[{colNames = Normal @ Keys @ recs[[1]] },

      If[ Length[ Intersection[ colNames, {"Item", "Score"} ] ] < 2,
        False,
        ScoredItemsQ[ AssociationThread[ Normal[ recs[All, "Item"] ], Normal[ recs[All, "Score"] ] ], context ]
      ]

    ];

DatasetWithScoredItemsQ[_, context_Association] := False;

DatasetWithScoredItemsQ[___] := (Echo["Wrong signature!", "DatasetWithScoredItemsQ:"]; False);


(**************************************************************)
(* Creation                                                   *)
(**************************************************************)

(* This can be a function MathematicaForPredictionUtilities.m . *)
Clear[NumericalColumns];
NumericalColumns[ds_Dataset] :=
    Block[{aNumColsQ},
      aNumColsQ =
          Normal@ds[
            Transpose /*
                Query[All, VectorQ[DeleteMissing[#], NumericQ] &]];

      Keys[Pick[aNumColsQ, Values[aNumColsQ]]]
    ];

(*
I am trying to avoid the package DateReshape.m here. Using ToLongForm from that package we can do:
ToSSparseMatrix@
 CrossTabulate[ToLongForm[ds[All, {idColumnName, varColumnName}], {idColumnName}, {varColumnName}]]
*)
Clear[NumericalColumnToSSparseMatrix];
NumericalColumnToSSparseMatrix[dsArg_Dataset, idColumnName_, varColumnName_] :=
    Block[{ds = dsArg},

      ds = dsArg[All, {idColumnName, varColumnName}][All, Join[#, <|"Variable" -> varColumnName|>]&];
      ds = Query[ReplaceAll[Missing[] -> 0], All][ds];

      ToSSparseMatrix @ CrossTabulate[ ds[All, {idColumnName, "Variable", varColumnName}] ]
    ];

Clear[SMRMonCreate];

(*SyntaxInformation[SMRMonCreate] = {"ArgumentsPattern" -> {_., _., OptionsPattern[]}};*)

(*SMRMonCreate::rneq = "The row names of SSparseMatrix objects are not the same."*)
(*SMRMonCreate::niid = "The specified item variable name is not one of the column names of the dataset."*)

Options[SMRMonCreate] =
    {
      "AddTagTypesToColumnNames" -> False,
      "TagValueSeparator" -> ".",
      "NumericalColumnsAsCategorical" -> False,
      "MissingValuesPattern" -> (None | "None" | Missing[___])
    };

SMRMonCreate[$SMRMonFailure] := $SMRMonFailure;

SMRMonCreate[xs_, context_Association] := SMRMonCreate[][xs, context];

SMRMonCreate[][xs_, context_Association] := SMRMonCreate[Options[SMRMonCreate]][xs, context];

SMRMonCreate[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Which[
      MatchQ[xs, _SSparseMatrix] || MatchQ[xs, Association[ (_ -> _SSparseMatrix) ..] ],
      SMRMonCreate[ xs, opts ][xs, context],

      AssociationQ[xs] && KeyExistsQ[xs, "data"] && KeyExistsQ[xs, "idColumnName"],
      SMRMonCreate[ xs["data"], xs["idColumnName"], opts ][xs, context],

      AssociationQ[xs] && KeyExistsQ[xs, "data"] && KeyExistsQ[xs, "itemColumnName"],
      SMRMonCreate[ xs["data"], xs["itemColumnName"], opts ][xs, context],

      True,
      SMRMonCreate["Fail"][xs, context]
    ];

SMRMonCreate[ smat_SSparseMatrix, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonCreate[ <| "anonymous" -> smat |>, opts][xs, context];

SMRMonCreate[smatsArg : Association[ (_ -> _SSparseMatrix) ..], opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{smats = smatsArg, tagTypeNames, rowNames, columnNames, splicedMat},

      tagTypeNames = Keys[smats];

      rowNames = Map[RowNames, smats];

      If[ !(Equal @@ rowNames),
        (* Echo["The row names of the SSparseMatrix objects are expected to be the same.", "SMRMonCreate:"]; *)
        rowNames = Union[Flatten[Values[rowNames]]];
        smats = Map[ ImposeRowNames[ #, rowNames ]&, smats];
      ];

      splicedMat = ColumnBind[Values[smats]];

      rowNames = RowNames[splicedMat];

      columnNames = ColumnNames[splicedMat];

      SMRMonUnit[
        xs,
        Join[
          context,
          <|
            "itemNames" -> AssociationThread[rowNames -> Range[Length[rowNames]]],
            "tags" -> AssociationThread[columnNames -> Range[Length[columnNames]]],
            "tagTypeWeights" -> AssociationThread[Keys[smats], ConstantArray[1, Length[smats]]],
            "matrices" -> smats,
            "M01" -> splicedMat,
            "M" -> splicedMat
          |>
        ]
      ]
    ];

SMRMonCreate[ds_Dataset, itemVarName_String, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ },
      SMRMonCreateFromWideForm[ds, itemVarName, opts][xs, context]
    ];

SMRMonCreate[ds_Dataset, { itemColumnName_String, tagTypeColumnName_String, tagColumnName_String, weightColumnName_String }, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ },
      SMRMonCreateFromLongForm[
        ds,
        { itemColumnName, tagTypeColumnName, tagColumnName, weightColumnName },
        FilterRules[ opts, Options[SMRMonCreateFromLongForm] ]
      ][xs, context]
    ];

SMRMonCreate[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a Dataset or an Association of SSparseMatrix objects. " <>
            "If the first argument is a Dataset a second argument is expected. " <>
            "That second argument should be an item ID column name in that Dataset (wide form) " <>
            "or a list of four strings specifying the column names that correspond to item ID, tag type, tag, weight. " <>
            "If no arguments are given the pipeline value is used.",
        "SMRMonCreate:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonCreateFromWideForm                                   *)
(**************************************************************)

Clear[SMRMonCreateFromWideForm];

Options[SMRMonCreateFromWideForm] =
    {
      "AddTagTypesToColumnNames" -> False,
      "TagValueSeparator" -> ".",
      "NumericalColumnsAsCategorical" -> False,
      "MissingValuesPattern" -> (None | "None" | Missing[___])
    };

SMRMonCreateFromWideForm[$SMRMonFailure] := $SMRMonFailure;

SMRMonCreateFromWideForm[xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromWideForm[][xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromWideForm[ds_Dataset, itemVarName_String, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ ncol, tagTypeNames, smats, idPos, idName, numCols, ds2, rowNames, allRowNames,
      addTagTypesToColumnNamesQ, numericalColumnsAsCategoricalQ, tagValueSeparator, missingValuesPattern},

      addTagTypesToColumnNamesQ = TrueQ[OptionValue[SMRMonCreateFromWideForm, "AddTagTypesToColumnNames"]];
      numericalColumnsAsCategoricalQ = TrueQ[OptionValue[SMRMonCreateFromWideForm, "NumericalColumnsAsCategorical"]];
      tagValueSeparator = ToString[OptionValue[SMRMonCreateFromWideForm, "TagValueSeparator"]];
      missingValuesPattern = OptionValue[SMRMonCreateFromWideForm, "MissingValuesPattern"];

      ncol = Dimensions[ds][[2]];

      tagTypeNames = Normal[Keys[ds[1]]];

      idPos = Flatten[Position[tagTypeNames, itemVarName]];
      If[ Length[idPos] == 0,
        Echo["The specified item variable name is not one of the column names of the dataset.", "SMRMonCreate:"];
        Return[$Failed]
      ];
      idPos = First[idPos];
      idName = tagTypeNames[[idPos]];

      If[ numericalColumnsAsCategoricalQ,
        (* This is intentionally separated from the 'else' code in order to avoid the redundant call to NumericalColumns. *)
        smats = Table[ v -> ToSSparseMatrix[ CrossTabulate[ds[All, {idName, v}]]], {v, Complement[ tagTypeNames, {idName} ]}],

        (*ELSE*)

        numCols = NumericalColumns[ds];

        smats =
            Table[
              If[ MemberQ[numCols, v],
                v -> NumericalColumnToSSparseMatrix[ds, idName, v],
                (*ELSE*)
                ds2 = ds[All, {idName, v}];
                ds2 = ds2[ Select[ FreeQ[#, missingValuesPattern]& ] ];
                v -> ToSSparseMatrix[ CrossTabulate[ds2] ]
              ], {v, Complement[ tagTypeNames, {idName} ]}];
      ];

      smats = Association[smats];

      If[addTagTypesToColumnNamesQ,

        smats =
            Association @
                KeyValueMap[Function[{k, mat},
                  k -> ToSSparseMatrix[mat,
                    "ColumnNames" -> Map[ k <> tagValueSeparator <> #&, ColumnNames[mat] ],
                    "RowNames" -> RowNames[mat]
                  ]], smats]
      ];

      SMRMonCreate[smats][xs, Join[context, <|"data" -> ds|>]]
    ];

SMRMonCreateFromWideForm[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a Dataset object. " <>
            "The optional second argument is expected to be a column name in that Dataset.",
        "SMRMonCreateFromWideForm:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonCreateFromLongForm                                   *)
(**************************************************************)

Clear[SMRMonCreateFromLongForm];

Options[SMRMonCreateFromLongForm] =
    {
      "AddTagTypesToColumnNames" -> False,
      "TagValueSeparator" -> ".",
      "MissingValuesPattern" -> (None | "None" | Missing[___])
    };

SMRMonCreateFromLongForm[$SMRMonFailure] := $SMRMonFailure;

SMRMonCreateFromLongForm[xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromLongForm[][xs_, context_Association] := $SMRMonFailure;

SMRMonCreateFromLongForm[ds_Dataset, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonCreateFromLongForm[ds, { "Item", "TagType", "Tag", "Weight" }, opts ][xs, context];

SMRMonCreateFromLongForm[ds_Dataset, { itemColumnName_String, tagTypeColumnName_String, tagColumnName_String, weightColumnName_String }, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ tagTypeNames, smats, allRowNames,
      addTagTypesToColumnNamesQ, tagValueSeparator, missingValuesPattern},

      addTagTypesToColumnNamesQ = TrueQ[OptionValue[SMRMonCreateFromLongForm, "AddTagTypesToColumnNames"]];
      tagValueSeparator = ToString[OptionValue[SMRMonCreateFromLongForm, "TagValueSeparator"]];
      missingValuesPattern = OptionValue[SMRMonCreateFromLongForm, "MissingValuesPattern"];


      If[ Length[ Intersection[ Normal @ Keys @ ds[[1]], {itemColumnName, tagTypeColumnName, tagColumnName, weightColumnName}  ] ] != 4,
        Echo["Not all of the specified column names are column names in the dataset.", "SMRMonCreateFromLongForm:"];
        Return[$Failed]
      ];

      tagTypeNames = Union[ Normal[ ds[All, tagTypeColumnName] ] ];

      smats = Association @ Map[Function[{tt}, tt -> ToSSparseMatrix @ CrossTabulate[ ds[ Select[#TagType == tt &], {itemColumnName, tagColumnName, weightColumnName}] ] ], tagTypeNames];

      If[addTagTypesToColumnNamesQ,

        smats =
            Association @
                KeyValueMap[Function[{k, mat},
                  k -> ToSSparseMatrix[mat,
                    "ColumnNames" -> Map[ k <> tagValueSeparator <> #&, ColumnNames[mat] ],
                    "RowNames" -> RowNames[mat]
                  ]], smats]
      ];

      SMRMonCreate[smats][xs, context]
    ];

SMRMonCreateFromLongForm[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a Dataset object. " <>
            "The optional second argument is expected to be of the form : " <>
            "{ itemColumnName_String, tagTypeColumnName_String, tagColumnName_String, weightColumnName_String } .",
        "SMRMonCreateFromLongForm:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonEchoDataSummary                                      *)
(**************************************************************)

Clear[SMRMonEchoDataSummary];

SMRMonEchoDataSummary[$SMRMonFailure] := $SMRMonFailure;

SMRMonEchoDataSummary[][$SMRMonFailure] := $SMRMonFailure;

SMRMonEchoDataSummary[xs_, context_Association] := SMRMonEchoDataSummary[][xs, context];

SMRMonEchoDataSummary[][xs_, context_Association] :=
    Block[{},
      If[ TrueQ[ SMRMonTakeData[xs, context] === $SMRMonFailure ],
        $SMRMonFailure,
        SMRMonBind[ SMRMonUnit[xs, context], SMRMonEchoFunctionContext[ "data summary:", RecordsSummary[#data] & ] ]
      ]
    ];

SMRMonEchoDataSummary[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "SMRMonEchoDataSummary:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonGetProperty                                          *)
(**************************************************************)

Clear[SMRMonGetProperty];

SMRMonGetProperty[$SMRMonFailure] := $SMRMonFailure;

SMRMonGetProperty[][$SMRMonFailure] := $SMRMonFailure;

SMRMonGetProperty[xs_, context_Association] := SMRMonGetProperty[None][xs, context];

SMRMonGetProperty[ property_String ][xs_, context_Association] :=
    Block[{res, cms},

      res =
          Which[
            ToLowerCase[property] == ToLowerCase["TagTypes"],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeTagTypes],

            ToLowerCase[property] == ToLowerCase["TagTypeRanges"],
            cms = ColumnsCount /@ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeMatrices];
            AssociationThread[ Keys[cms], Rest[ FoldList[{#1[[2]] + 1, #1[[2]] + #2} &, {0, 0}, Values[cms]] ] ],

            ToLowerCase[property] == ToLowerCase["ItemColumnName"],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeItemColumnName],

            MemberQ[ ToLowerCase[ {"Matrix", "M"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM],

            MemberQ[ ToLowerCase[ {"IncidenceMatrix", "M01"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM01],

            MemberQ[ ToLowerCase[ {"Matrices", "SubMatrices", "SubMatrixes", "ContingencyMatrices", "ContingencyMatrixes"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeMatrices],

            ToLowerCase[property] == ToLowerCase["Properties"],
            { "Matrix", "IncidenceMatrix", "SubMatrices", "Properties" },

            True,
            Echo[ "Unknown property specification.", "SMRMonGetProperty:"];
            Return[$SMRMonFailure]
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonGetProperty[___][__] :=
    Block[{},
      Echo["One string argument is expected.", "SMRMonGetProperty:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonGetMatrixProperty                                    *)
(**************************************************************)

Clear[SMRMonGetMatrixProperty];

SMRMonGetMatrixProperty[$SMRMonFailure] := $SMRMonFailure;

SMRMonGetMatrixProperty[][$SMRMonFailure] := $SMRMonFailure;

SMRMonGetMatrixProperty[xs_, context_Association] := SMRMonGetMatrixProperty[None][xs, context];

SMRMonGetMatrixProperty[ property_String ][xs_, context_Association] :=
    Block[{res},

      res =
          Which[
            MemberQ[ ToLowerCase[ {"Tags", "Columns"} ], ToLowerCase[property] ],
            ColumnNames[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            ToLowerCase[property] == ToLowerCase["Rows"],
            RowNames[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            MemberQ[ ToLowerCase[ {"NumberOfColumns", "ColumnsCount"} ], ToLowerCase[property] ],
            ColumnsCount[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            MemberQ[ ToLowerCase[ {"NumberOfRows", "ColumnsRows"} ], ToLowerCase[property] ],
            RowsCount[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ],

            MemberQ[ ToLowerCase[ {"Dimensions", "Dim"} ], ToLowerCase[property] ],
            Dimensions[ SparseArray[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ] ],

            ToLowerCase[property] == ToLowerCase["Density"],
            SparseArray[ SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeM] ]["Density"],

            MemberQ[ ToLowerCase[ {"TagTypeWeights", "TagTypeSignificanceFactors"} ], ToLowerCase[property] ],
            SMRMonBind[ SMRMonUnit[xs, context], SMRMonTakeTagTypeWeights],

            ToLowerCase[property] == ToLowerCase["Properties"],
            { "Tags", "Columns", "Rows",
               "NumberOfColumns", "NumberOfRows",
               "Dim", "Dimensions", "Density",
               "TagTypeWeights", "TagTypeSignificanceFactors",
               "Properties" },

            True,
            Echo[ "Unknown property specification.", "SMRMonGetMatrixProperty:"];
            Return[$SMRMonFailure]
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonGetMatrixProperty[___][__] :=
    Block[{},
      Echo["One string argument is expected.", "SMRMonGetMatrixProperty:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonApplyLocalWeightFunction                             *)
(**************************************************************)

Clear[SMRMonApplyLocalWeightFunction];

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

Clear[SMRMonApplyGlobalWeightFunction];

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

Clear[SMRMonApplyNormalizationFunction];

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

Clear[SMRMonApplyTermWeightFunctions];

SMRMonApplyTermWeightFunctions[$SMRMonFailure] := $SMRMonFailure;

SMRMonApplyTermWeightFunctions[xs_, context_Association] := SMRMonApplyTermWeightFunctions[][xs, context];

SMRMonApplyTermWeightFunctions[][xs_, context_Association] := SMRMonApplyTermWeightFunctions["IDF", "None", "Cosine"][xs, context];

SMRMonApplyTermWeightFunctions[globalWeightFunction_String, localWeightFunction_String, normalizerFunction_String][xs_, context_Association] :=
    Block[{mat, smats},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonApplyTermWeightFunctions:"];
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

Clear[GetFilterIDs];

GetFilterIDs[context_Association, callerFunctionName_String] :=
    Block[{},
      Which[
        !KeyExistsQ[context, "filter"],
        Echo["There is no key \"filter\" in the context.", callerFunctionName <> ":"];
        All,

        AssociationQ[context["filter"]] && ScoredItemsQ[context["filter"], context],
        Keys[context["filter"]],

        ListQ[context["filter"]] && ScoredItemsQ[AssociationThread[context["filter"], 1], context],
        context["filter"],

        True,
        Echo["The value for the key \"filter\" in the context is expected to a list of items or an association of scored items.",
          callerFunctionName <> ":"];
        All
      ]
    ];

Clear[SMRMonRecommend];

Options[SMRMonRecommend] = {"RemoveHistory" -> True, "ItemNames" -> True, "Normalize" -> True, "VectorResult" -> False };

SMRMonRecommend[$SMRMonFailure] := $SMRMonFailure;

SMRMonRecommend[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommend[ history : Association[ (_String -> _?NumberQ) ... ], nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{h},

      h = KeyMap[ context["itemNames"][#]&, history ];
      h = KeySelect[ h, IntegerQ ];

      If[ Length[h] < Length[history],
        Echo["Some of the item names are not known by the recommender.", "SMRMonRecommend:"];
      ];

      SMRMonRecommend[ Keys[h], Values[h], nRes, opts][xs, context]
    ];

SMRMonRecommend[ itemName_String, nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ AssociationThread[{itemName}, 1], nRes, opts][xs, context];

SMRMonRecommend[ itemNames : {_String...}, nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ AssociationThread[itemNames, 1], nRes, opts][xs, context];

SMRMonRecommend[ history : Association[ (_Integer -> _?NumberQ) ... ], nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ Keys[history], Values[history], nRes, opts][xs, context];

SMRMonRecommend[ itemIndices : {_Integer...}, nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonRecommend[ itemIndices, ConstantArray[1, Length[itemIndices]], nRes, opts][xs, context];

SMRMonRecommend[ itemIndices : {_Integer...}, itemRatings : {_?NumberQ...}, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{removeHistoryQ, itemNamesQ, normalizeQ, vectorResultQ, vec, filterIDs = All,
      filterInds, smat, fmat, recs, recsInds, recsVals, ordInds, rowNames},

      If[Length[itemIndices],
        Echo["Empty history as an argument.", "SMRMonRecommend:"];
        Return[<||>]
      ];

      removeHistoryQ = TrueQ[OptionValue[SMRMonRecommend, "RemoveHistory"]];
      itemNamesQ = TrueQ[OptionValue[SMRMonRecommend, "ItemNames"]];
      normalizeQ = TrueQ[OptionValue[SMRMonRecommend, "Normalize"]];
      vectorResultQ = TrueQ[OptionValue[SMRMonRecommend, "VectorResult"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommend:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      (*1*)
      vec = SparseArray[Thread[itemIndices -> itemRatings], {Length[smat]}];

      (*2*)
      vec = smat.(vec.smat);

      If[ KeyExistsQ[context, "filter"],
        filterIDs = GetFilterIDs[context, "SMRMonRecommend"];
        rowNames = RowNamesAssociation[context["M"]];
        filterInds = Join[itemIndices, rowNames[#]& /@ filterIDs ];
        fmat = DiagonalMatrix[ SparseArray[Thread[filterInds -> 1.], RowsCount[context["M"]]] ];
        vec = fmat . vec ;
      ];

      (*3 and 4 and 5*)

      If[ normalizeQ && Max[Abs[vec]] > 0,
        vec = vec / Max[vec];
      ];

      If[ vectorResultQ,

        If[ IntegerQ[ nRes ],
          recsInds = Reverse[ Ordering[ Normal[vec] ] ][[ 1 ;; nRes ]];
          vec = SparseArray[ Thread[ recsInds -> vec[[recsInds]] ], RowsCount[context["M"]] ];
        ];

        Return[ SMRMonUnit[ vec, context ] ]
      ];

      If[ removeHistoryQ,
        vec[[itemIndices]] = 0;
        (* This is probably redundant -- Ordering won't be slowed-down that much. *)
        (* vec = SparseArray[vec]; *)
      ];

      recs = Most[ArrayRules[vec]];

      recsInds = Flatten[recs[[All, 1]]];
      recsVals = recs[[All, 2]];

      ordInds = Reverse[ Ordering[ recsVals ] ];

      If[ IntegerQ[nRes],
        ordInds = Take[ ordInds, UpTo[nRes] ]
      ];

      recsInds = recsInds[[ordInds]];
      recsVals = recsVals[[ordInds]];

      If[itemNamesQ,
        rowNames = RowNames[ context["M"] ];
        recs = AssociationThread[ rowNames[[recsInds]], recsVals ],
        (* ELSE *)
        recs = AssociationThread[ recsInds, recsVals ];
      ];

      SMRMonUnit[recs, context]

    ] /; Length[itemIndices] == Length[itemRatings];


SMRMonRecommend[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an association of scored items. " <>
            "The second argument is expected to be a positive integer. " <>
            "The items are recommendation matrix row indices or row names. The scores are positive numbers.",
        "SMRMonRecommend:"];
      $SMRMonFailure
    ];

Clear[SMRMonRecommendByHistory ];
SMRMonRecommendByHistory = SMRMonRecommend;


(**************************************************************)
(* SMRMonRecommendByProfile                                   *)
(**************************************************************)

Clear[SMRMonRecommendByProfile];

Options[SMRMonRecommendByProfile] = {"ItemNames" -> True, "Normalize" -> True, "IgnoreUnknownTags" -> False, "VectorResult" -> False };

SMRMonRecommendByProfile[$SMRMonFailure] := $SMRMonFailure;

SMRMonRecommendByProfile[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommendByProfile[nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{},
      (* Without verification. *)
      SMRMonRecommendByProfile[xs, nRes, opts][xs, context]
    ];

SMRMonRecommendByProfile[profileInds : {_Integer..}, profileScores : {_?NumberQ..}, nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{vec, smat},

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommend:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = SparseArray[Thread[profileInds -> profileScores], {Dimensions[smat][[2]]}];

      SMRMonRecommendByProfile[vec, nRes, opts][xs, context]
    ] /; Length[profileInds] == Length[profileScores];

SMRMonRecommendByProfile[tagsArg : (_Association | _List), nRes_Integer, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{tags = tagsArg, p},

      If[ ListQ[tags], tags = AssociationThread[ tags -> 1. ] ];

      If[ TrueQ[OptionValue[SMRMonRecommendByProfile, "IgnoreUnknownTags"]],
        tags = KeyTake[tags, ColumnNames[context["M"]]];
      ];

      If[ AssociationQ[tags] && !ScoredTagsQ[tags, context],
        Echo["The first argument is not an association of tags->score elements or a list of tags.", "SMRMonRecommendByProfile:"];
        Return[$SMRMonFailure]
      ];

      p = SMRMonToProfileVector[tags][xs, context];
      SMRMonRecommendByProfile[p[[1]], nRes, opts][xs, context]
    ];

SMRMonRecommendByProfile[profileVec_SparseArray, nRes : (_Integer | All), opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{itemNamesQ, normalizeQ, vectorResultQ, vec, smat, recs, recsInds, recsVals, ordInds, filterIDs, filterInds, rowNames, fmat},

      itemNamesQ = TrueQ[OptionValue[SMRMonRecommendByProfile, "ItemNames"]];
      normalizeQ = TrueQ[OptionValue[SMRMonRecommendByProfile, "Normalize"]];
      vectorResultQ = TrueQ[OptionValue[SMRMonRecommendByProfile, "VectorResult"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonRecommendByProfile:"];
        Return[$SMRMonFailure]
      ];

      If[ ColumnsCount[context["M"]] != Dimensions[profileVec][[1]],
        Echo[
          "The number of columns of the recommender matrix is different than the length of the profile vector argument.",
          "SMRMonRecommendByProfile:"
        ];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      vec = smat . profileVec ;

      If[ KeyExistsQ[context, "filter"],
        filterIDs = GetFilterIDs[context, "SMRMonRecommendByProfile"];
        rowNames = RowNamesAssociation[context["M"]];
        filterInds = rowNames[#]& /@ filterIDs;
        If[ Length[filterInds] > 0,
          fmat = DiagonalMatrix[ SparseArray[Thread[filterInds -> 1.], RowsCount[context["M"]]] ];
          vec = fmat . vec ;
        ];
      ];

      If[normalizeQ && Max[Abs[vec]] > 0,
        vec = vec / Max[vec];
      ];

      If[ vectorResultQ,

        If[ IntegerQ[ nRes ],
          resInds = Reverse[ Ordering[ Normal[vec] ] ][[ 1 ;; nRes ]];
          vec = SparseArray[ Thread[ resInds -> vec[[resInds]] ], RowsCount[context["M"]] ];
        ];

        Return[ SMRMonUnit[ vec, context ] ]
      ];

      (*  recs = Association[ Most[ArrayRules[vec]] ]; *)
      recs = Most[ArrayRules[vec]];

      (*  recs = KeyMap[ First, recs ]; *)
      recsInds = Flatten[recs[[All, 1]]];
      recsVals = recs[[All, 2]];

      ordInds = Reverse[ Ordering[ recsVals ] ];

      If[ IntegerQ[nRes],
        ordInds = Take[ ordInds, UpTo[nRes] ]
      ];

      recsInds = recsInds[[ordInds]];
      recsVals = recsVals[[ordInds]];

      If[itemNamesQ,
        rowNames = RowNames[ context["M"] ];
        recs = AssociationThread[ rowNames[[recsInds]], recsVals ],
        (* ELSE *)
        recs = AssociationThread[ recsInds, recsVals ];
      ];

      SMRMonUnit[recs, context]

    ];

SMRMonRecommendByProfile[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an association of scored tags. " <>
            "The second argument is expected to be a positive integer. " <>
            "The tags are recommendation matrix column names. The scores are positive numbers.",
        "SMRMonRecommendByProfile:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonRecommendByCorrelation                               *)
(**************************************************************)

Clear[SMRMonRecommendByCorrelation];

Options[SMRMonRecommendByCorrelation] = { Method -> Correlation, "SMRNumberOfRecommendations" -> 200 };

SMRMonRecommendByCorrelation[xs_, context_Association] := $SMRMonFailure;

SMRMonRecommendByCorrelation[ searchVector_?VectorQ, nRes_Integer, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{recs, corRecs, smrNRecs, methodFunc, corMat},

      methodFunc = OptionValue[ SMRMonRecommendByCorrelation, Method ];

      smrNRecs = OptionValue[ SMRMonRecommendByCorrelation, "SMRNumberOfRecommendations"];
      If[ ! TrueQ[ IntegerQ[smrNRecs] && smrNRecs > 0 ],
        Echo["Positive integer is expected as a value of the option \"SMRNumberOfRecommendations\".", "SMRMonRecommendByCorrelation:"];
        Return[$SMRMonFailure]
      ];

      If[ !KeyExistsQ[context, "timeSeriesMatrix"],
        Echo["Cannot find a time series matrix, context key \"timeSeriesMatrix\".", "SMRMonRecommendByCorrelation:"];
        Return[$SMRMonFailure]
      ];

      recs = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonRecommendByProfile[ searchVector, smrNRecs ], SMRMonTakeValue }];

      If[ TrueQ[recs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[
        ! TrueQ[
          SSparseMatrixQ[context["timeSeriesMatrix"]] &&
              RowNames[context["timeSeriesMatrix"]] == RowNames[context["M"]] &&
              ColumnsCount[context["timeSeriesMatrix"]] == ColumnsCount[context["M"]] ],
        Echo["The value of \"timeSeriesMatrix\" is not a SSparseMatrix object that has the same dimensions and row names as the recommendation matrix."];
        Return[$SMRMonFailure]
      ];

      corMat = context["timeSeriesMatrix"][[Keys[recs], All]];

      (* TO DO: methodFunc is expected to be one of {Correlation, SpearmanRho, KendallTau, Dot} *)
      corRecs = Flatten @ methodFunc[ Transpose[SparseArray[corMat]], Transpose[{searchVector}] ];

      corRecs = AssociationThread[ RowNames[corMat], corRecs];

      corRecs = TakeLargest[corRecs, UpTo[nRes]];

      SMRMonUnit[corRecs, context]
    ];

SMRMonRecommendByCorrelation[___][__] := $SMRMonFailure;


(**************************************************************)
(* Convert recommendations to a Dataset                       *)
(**************************************************************)

Clear[SMRMonToItemsDataset];

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

Clear[SMRMonJoinAcross];

Options[SMRMonJoinAcross] = {"DropJoiningColumnName" -> True, "AsDataset" -> True };

SMRMonJoinAcross[$SMRMonFailure] := $SMRMonFailure;

SMRMonJoinAcross[xs_, context_Association] := $SMRMonFailure;

SMRMonJoinAcross[][xs_, context_Association] := $SMRMonFailure;

SMRMonJoinAcross[dsArg_Dataset, byColName_?AtomQ, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{ds = dsArg, dropQ, dsRecs, res},

      dropQ = TrueQ[ OptionValue[SMRMonJoinAcross, "DropJoiningColumnName"] ];

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

      res = JoinAcross[ Normal[dsRecs[All, {"Score", "Item"}]], Normal[ds], Key["Item"] -> Key[byColName] ];
      res = Dataset[res][SortBy[-#Score &]];

      If[ dropQ, res = res[All, KeyDrop[#, byColName]&] ];

      SMRMonUnit[res, context]
    ];

SMRMonJoinAcross[ asc_Association, opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonJoinAcross[  "Value" -> asc, opts ][xs, context];

SMRMonJoinAcross[ dataName_String -> asc_Association, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{dsRecs, dsQ, datasetQ},

      dsQ = TrueQ[ DatasetWithScoredItemsQ[xs, context] ];

      datasetQ = TrueQ[ OptionValue["AsDataset"] ] || dsQ ;

      (* The assumption is that the monad value xs is recommendations result. *)
      (* I.e. an association of with item names as keys and real numbers as values. *)
      (* This should be checked and verified. *)

      If[ !( TrueQ[ ScoredItemsQ[xs, context] ] || dsQ ),
        Echo[
          "The pipeline value is not an association of scored items or a Dataset that with scored items.",
          "SMRMonJoinAcross:"
        ];
        Return[$SMRMonFailure];
      ];

      If[ datasetQ,

        If[ dsQ,
          dsRecs = xs,
          dsRecs = Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonToItemsDataset, SMRMonTakeValue } ]
        ];

        If[ TrueQ[dsRecs === $SMRMonFailure],
          Return[$SMRMonFailure]
        ];

        SMRMonUnit[ dsRecs[ All, Join[ #, <| dataName -> asc[#Item] |>]& ], context ],
        (* ELSE *)


        dsRecs = KeyValueMap[ Prepend[ asc[#1], "RecommendationScore" -> #2 ]&, xs ];
        SMRMonUnit[ dsRecs, context ]
      ]
    ];

SMRMonJoinAcross[__][___] :=
    Block[{},
      Echo[
        "The first argument is expected to be an Association or a Dataset. " <>
            "If the first argument is a Dataset then the second argument is expected to be a column name to do the joining with.",
        "SMRMonJoinAcross:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonProfile                                              *)
(**************************************************************)

(* Essentially repeating SMRMonRecommend but with minor changes. *)

Clear[SMRMonProfile];

Options[SMRMonProfile] = {"TagNames" -> True};

SMRMonProfile[$SMRMonFailure] := $SMRMonFailure;

SMRMonProfile[xs_, context_Association] := $SMRMonFailure;

SMRMonProfile[ itemName_String, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ AssociationThread[ itemName, 1], opts ][xs, context];

SMRMonProfile[ itemNames : {_String .. }, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ AssociationThread[itemNames, 1], opts ][xs, context];

SMRMonProfile[ history : Association[ (_String -> _?NumberQ) ... ], opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{h},

      h = KeyMap[ context["itemNames"][#]&, history ];
      h = KeySelect[ h, IntegerQ ];

      If[ 0 == Length[h],
        Echo["None of the item names is known by the recommender.", "SMRMonProfile:"];
        Return[$SMRMonFailure];
      ];

      If[ 0 < Length[h] < Length[history],
        Echo["Some of the item names are not known by the recommender.", "SMRMonProfile:"];
      ];

      SMRMonProfile[ Keys[h], Values[h], opts][xs, context]
    ];

SMRMonProfile[ history : Association[ (_Integer -> _?NumberQ) ... ], opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ Keys[history], Values[history], opts][xs, context];

SMRMonProfile[ itemIndices : {_Integer...}, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonProfile[ itemIndices, ConstantArray[1, Length[itemIndices]], opts][xs, context];

SMRMonProfile[ itemIndices : {_Integer...}, itemRatings : {_?NumberQ...}, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{vec, smat, prof, tagNamesQ, columnNames},

      If[Length[itemIndices],
        Echo["Empty history as an argument.", "SMRMonProfile:"];
        Return[<||>]
      ];

      tagNamesQ = TrueQ[OptionValue[SMRMonProfile, "TagNames"]];

      If[!KeyExistsQ[context, "M"],
        Echo["Cannot find the recommendation matrix. (The context key \"M\".)", "SMRMonProfile:"];
        Return[$SMRMonFailure]
      ];

      smat = SparseArray[context["M"]];

      (*1*)
      vec = SparseArray[Thread[itemIndices -> itemRatings], {Length[smat]}];

      (*2*)
      vec = vec.smat;

      (*3 and 4 and 5*)

      prof = Association[ Most[ArrayRules[vec]] ];

      prof = KeyMap[ First, prof ];

      If[tagNamesQ,
        columnNames = ColumnNames[ context["M"] ];
        prof = KeyMap[ columnNames[[#]]&, prof];
      ];

      SMRMonUnit[prof, context]

    ] /; Length[itemIndices] == Length[itemRatings];


SMRMonProfile[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be an association of scored items. " <>
            "The items are recommendation matrix row indices or row names. The scores are positive numbers.",
        "SMRMonProfile:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMRMonToProfileVector                                      *)
(**************************************************************)

Clear[SMRMonToProfileVector];

SMRMonToProfileVector[$SMRMonFailure] := $SMRMonFailure;

SMRMonToProfileVector[xs_, context_Association] := $SMRMonFailure;

SMRMonToProfileVector[ scoredTags : Association[ (_Integer -> _?NumberQ)..] ][xs_, context_Association] :=
    Block[{},
      SMRMonUnit[ SparseArray[ Normal@scoredTags, ColumnsCount[context["M"]] ], context]
    ];

SMRMonToProfileVector[ scoredTags : Association[ (_String -> _?NumberQ)..] ][xs_, context_Association] :=
    SMRMonToProfileVector[ KeyMap[ context["tags"][#]&, scoredTags] ][xs, context];

SMRMonToProfileVector[___][__] := $SMRMonFailure;


(**************************************************************)
(* SMRMonSetTagTypeWeights                                    *)
(**************************************************************)

Clear[SMRMonSetTagTypeWeights];

SMRMonSetTagTypeWeights[$SMRMonFailure] := $SMRMonFailure;

SMRMonSetTagTypeWeights[xs_, context_Association] := $SMRMonFailure;

SMRMonSetTagTypeWeights[ defaultValue_?NumberQ ][xs_, context_Association] :=
    SMRMonSetTagTypeWeights[ <||>, defaultValue][xs, context];

SMRMonSetTagTypeWeights[ scoredTagTypes_Association ][xs_, context_Association] :=
    SMRMonSetTagTypeWeights[ scoredTagTypes, 1][xs, context];

SMRMonSetTagTypeWeights[ scoredTagTypesArg : Association[ (_String -> _?NumberQ)...], defaultValue_?NumberQ ][xs_, context_Association] :=
    Block[{ scoredTagTypes = scoredTagTypesArg, smats, mat},

      If[!KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonSetTagTypeWeights:"];
        Return[$SMRMonFailure]
      ];

      scoredTagTypes = KeySort[ Join[ AssociationThread[ Keys[context["matrices"]] -> defaultValue ], scoredTagTypes ] ];

      (* Optimization -- useful with interactive interfaces. *)
      If[ KeyExistsQ[ context, "TagTypeWeights" ] && context["TagTypeWeights"] == scoredTagTypes,
        Return[ SMRMonUnit[xs, context] ]
      ];

      smats = Association[KeyValueMap[ #1 -> scoredTagTypes[#1] * #2 &, context["matrices"]]];

      mat = ColumnBind[ Values[smats] ];

      SMRMonUnit[xs, Join[ context, <| "M" -> mat, "TagTypeWeights" -> scoredTagTypes |> ]]

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

Clear[SMRMonSetTagWeights];

SMRMonSetTagWeights[$SMRMonFailure] := $SMRMonFailure;

SMRMonSetTagWeights[xs_, context_Association] := $SMRMonFailure;

SMRMonSetTagWeights[ defaultValue_?NumberQ ][xs_, context_Association] :=
    SMRMonSetTagWeights[ <||>, defaultValue][xs, context];

SMRMonSetTagWeights[ scoredTags_Association ][xs_, context_Association] :=
    SMRMonSetTagWeights[ scoredTags, 1][xs, context];

SMRMonSetTagWeights[ scoredTagsArg : Association[ (_String -> _?NumberQ)...], defaultValue_?NumberQ ][xs_, context_Association] :=
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


(**************************************************************)
(* SMR Join                                                   *)
(**************************************************************)

Clear[SMRMonJoin];

SMRMonJoin[$SMRMonFailure] := $SMRMonFailure;

SMRMonJoin[xs_, context_Association] := $SMRMonFailure;

SMRMonJoin[ smr2_SMRMon ][xs_, context_Association] :=
    SMRMonJoin[ smr2, "same", Automatic, Automatic ][xs, context];

SMRMonJoin[ smr2_SMRMon, joinType_String ][xs_, context_Association] :=
    SMRMonJoin[ smr2, joinType, Automatic, Automatic ][xs, context];

SMRMonJoin[ smr2_SMRMon, joinType_String, colnamesPrefix1 : (_String | Automatic), colnamesPrefix2 : (_String | Automatic) ][xs_, context_Association] :=
    Block[{smats1, smats2, allRownames, matrices},

      (*  Get the appropriate all row names. *)
      If[ joinType != "same",

        Which[
          joinType == "outer",
          allRownames =
              Union[ Flatten[
                RowNames[ context["M"] ],
                RowNames[ SMRMonBind[ smr2, SMRMonTakeContext]["M"] ] ] ],

          joinType == "inner",
          allRownames =
              Intersection[
                RowNames[ context["M"] ],
                RowNames[ SMRMonBind[ smr2, SMRMonTakeContext]["M"] ] ],

          joinType == "left",
          allRownames = RowNames[ context["M"] ],

          True,
          Echo[ "The second argument is expected to be one of \"same\", \"outer\", \"inner\", \"left\".", "SMRMonJoin:"];
          Return[$SMRMonFailure];

        ];

      ];

      smats1 = context["matrices"];

      smats2 = SMRMonBind[ smr2, SMRMonTakeContext]["matrices"];

      If[ Length[ Intersection[ Keys[smats1], Keys[smats2] ] ] > 0,
        Echo[
          "The tag types " <> ToString[ Intersection[ Keys[smats1], Keys[smats2] ] ] <>
              " are also in the SMRMon argument, hence will be dropped.",
          "SMRMonJoin:"
        ];
      ];

      matrices =
          Join[
            Map[ ImposeRowNames[#, allRownames]&, smats1 ],
            Map[ ImposeRowNames[#, allRownames]&, smats2 ]
          ];

      Fold[ SMRMonBind, SMRMonUnit[], {SMRMonCreate[ matrices ] } ]
    ];

SMRMonJoin[___][__] :=
    Block[{},
      Echo[
        "The first argument is expected to be a SMRMon object. " <>
            "The second argument is expected to be a string specifying the type of joining.",
        "SMRMonJoin:"];
      $SMRMonFailure
    ];


(**************************************************************)
(* SMR row-bind                                               *)
(**************************************************************)

Clear[SMRMonRowBind];

SMRMonRowBind[$SMRMonFailure] := $SMRMonFailure;

SMRMonRowBind[xs_, context_Association] := $SMRMonFailure;

SMRMonRowBind[smr2_SMRMon][xs_, context_Association] :=
    Block[{smats1, smats2, resSMmats, cnames},

      smats1 = context["matrices"];

      smats2 = SMRMonBind[ smr2, SMRMonTakeMatrices ];

      Which[

        Keys[smats1] == Keys[smats2],

        (* Row bind each pair. *)
        resSMmats =
            MapThread[
              ( cnames = Union[Join[ColumnNames[#1], ColumnNames[#2]]];
              RowBind[ ImposeColumnNames[#1, cnames], ImposeColumnNames[#2, cnames] ])&,
              {smats1, smats2}];


        SMRMonCreate[resSMmats][xs, context],

        True,
        Echo["The tag types of the SMRMon objects to be joined (row-bound) are not the same.", "SMRMonJoin:"];
        $SMRMonFailure
      ]

    ];

SMRMonRowBind[___][__] :=
    Block[{},
      Echo[ "The first argument is expected to be a SMRMon object.", "SMRMonRowBind:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Classify (original implementation)                      *)
(*=========================================================*)

Clear[SMRMonClassifyOriginal];

Options[SMRMonClassifyOriginal] = {
  "TagType" -> None, "Profile" -> None, "Property" -> "Probabilities",
  "NumberOfNearestNeighbors" -> Automatic, "NumberOfResults" -> All,
  "Voting" -> False, "DropZeroScoredLabels" -> True
};

SMRMonClassifyOriginal[$SMRMonFailure] := $SMRMonFailure;

(*SMRMonClassifyOriginal[xs_, context_Association] := (Print["here"];$SMRMonFailure);*)

SMRMonClassifyOriginal[][xs_, context_Association] := SMRMonClassifyOriginal[None][xs, context];

SMRMonClassifyOriginal[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{tagType, profile},

      tagType = OptionValue[SMRMonClassifyOriginal, "TagType"];
      profile = OptionValue[SMRMonClassifyOriginal, "Profile"];

      If[ ! StringQ[ tagType ],
        Echo["The value of the option \"TagType\" is expected to be a string.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( AssociationQ[ profile ] || VectorQ[ profile, StringQ ] ),
        Echo["The value of the option \"Profile\" is expected to be an association or a list of strings.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      SMRMonClassifyOriginal[tagType, profile, opts][xs, context]
    ];

SMRMonClassifyOriginal[tagType_String, profile : {_String..}, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonClassifyOriginal[tagType, AssociationThread[profile, 1], opts][xs, context];

SMRMonClassifyOriginal[tagType_String, profile_Association, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{recs, clMat, clMat01, s, t,
      property, expectedProperties, numberOfNearestNeighbors, numberOfResults,
      votingQ, dropZeroScoredLabelsQ, qProfile, res, recsVec},

      property = OptionValue[SMRMonClassifyOriginal, "Property"];
      numberOfNearestNeighbors = OptionValue[SMRMonClassifyOriginal, "NumberOfNearestNeighbors"];
      numberOfResults = OptionValue[SMRMonClassifyOriginal, "NumberOfResults"];

      votingQ = TrueQ[OptionValue[SMRMonClassifyOriginal, "Voting"]];
      dropZeroScoredLabelsQ = TrueQ[OptionValue[SMRMonClassifyOriginal, "DropZeroScoredLabels"]];

      expectedProperties = { "Decision", "Probabilities", "Properties" };
      If[ ! MemberQ[ expectedProperties, property ],
        Echo["The value of the option \"Property\" is expected to be one of " <> ToString[expectedProperties] <> ".", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfNearestNeighbors === Automatic] || NumberQ[ numberOfNearestNeighbors ] ),
        Echo["The value of the option \"NumberOfNearestNeighbors\" is expected to be a number or Automatic.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfResults === All] || NumberQ[ numberOfResults ] ),
        Echo["The value of the option \"NumberOfResults\" is expected to be a number or All.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ !MemberQ[ Keys[context["matrices"]], tagType],
        Echo[
          "Unknown tag type \"" <> tagType <> "\"; the first argument should be one of: " <>
              ToString[ "\"" <> # <> "\""& /@ Keys[context["matrices"]] ] <> " .",
          "SMRMonClassifyOriginal:"
        ];
        Return[$SMRMonFailure]
      ];

      clMat = context["matrices", tagType];

      qProfile = KeySelect[ profile, !MemberQ[ColumnNames[clMat], #]& ];

      If[ Length[qProfile] == 0,
        Echo[
          "The profile argument has to have at least one tag that does not belong to the tag type \"" <> tagType <> "\".",
          "SMRMonClassifyOriginal:"
        ];
        Return[$SMRMonFailure]
      ];

      If[ TrueQ[ numberOfNearestNeighbors === Automatic ],
        numberOfNearestNeighbors = Ceiling[ 0.05 * RowsCount[ clMat ] ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors < 10, 10, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors > 200, 200, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = Min[ numberOfNearestNeighbors, RowsCount[clMat] ];
      ];

      recs = Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonRecommendByProfile[qProfile, numberOfNearestNeighbors], SMRMonTakeValue}];

      If[ TrueQ[recs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[ votingQ,
        clMat01 = SparseArray[clMat];
        t = Most[ArrayRules[clMat]]; t[[All, 2]] = 1.;
        clMat01 = SparseArray[t, Dimensions[clMat]];
        clMat = ToSSparseMatrix[ clMat01, "RowNames" -> RowNames[clMat], "ColumnNames" -> ColumnNames[clMat] ];
        recs = AssociationThread[ Keys[recs], 1.];
      ];

      (* Finally the "classification" computation follows. *)
      s = ( Values[recs] / Max[recs] ) . SparseArray[ clMat[[ Keys[recs], All ]] ];

      If[ Max[Abs[s]] > 0, s = s / Max[s] ];

      s = AssociationThread[ ColumnNames[clMat], s];

      If[ dropZeroScoredLabelsQ, s = Select[s, # > 0&] ];

      res = ReverseSort[s];

      res =
          Which[
            property == "Decision", First[Keys[res]],

            property == "Probabilities" && TrueQ[ numberOfResults === All ], res,

            property == "Probabilities", TakeLargest[ res, UpTo[numberOfResults] ],

            property == "Properties", expectedProperties,

            True, res
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonClassifyOriginal[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonClassifyOriginal[tagType_String, profile_Association, opts___].",
        "SMRMonClassifyOriginal:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Classify                                                *)
(*=========================================================*)

Clear[SMRMonClassify];

Options[SMRMonClassify] = {
  "TagType" -> None, "Profile" -> None, "Property" -> "Probabilities",
  "NumberOfNearestNeighbors" -> Automatic, "NumberOfResults" -> All,
  "Voting" -> False, "DropZeroScoredLabels" -> True
};

SMRMonClassify[$SMRMonFailure] := $SMRMonFailure;

(*SMRMonClassifyOriginal[xs_, context_Association] := (Print["here"];$SMRMonFailure);*)

SMRMonClassify[][xs_, context_Association] := SMRMonClassifyOriginal[None][xs, context];

SMRMonClassify[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{tagType, profile},

      tagType = OptionValue[SMRMonClassifyOriginal, "TagType"];
      profile = OptionValue[SMRMonClassifyOriginal, "Profile"];

      If[ ! StringQ[ tagType ],
        Echo["The value of the option \"TagType\" is expected to be a string.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( AssociationQ[ profile ] || VectorQ[ profile, StringQ ] ),
        Echo["The value of the option \"Profile\" is expected to be an association or a list of strings.", "SMRMonClassifyOriginal:"];
        Return[$SMRMonFailure]
      ];

      SMRMonClassify[tagType, profile, opts][xs, context]
    ];

SMRMonClassify[tagType_String, profile : {_String..}, opts : OptionsPattern[]][xs_, context_Association] :=
    SMRMonClassify[tagType, AssociationThread[profile, 1], opts][xs, context];

SMRMonClassify[tagType_String, profile_Association, opts : OptionsPattern[]][xs_, context_Association] :=
    Block[{recs, clMat, clMat01, cnAsc, s, t,
      property, expectedProperties, numberOfNearestNeighbors, numberOfResults,
      votingQ, dropZeroScoredLabelsQ, qProfile, resInds, res},

      property = OptionValue[SMRMonClassify, "Property"];
      numberOfNearestNeighbors = OptionValue[SMRMonClassify, "NumberOfNearestNeighbors"];
      numberOfResults = OptionValue[SMRMonClassify, "NumberOfResults"];

      votingQ = TrueQ[OptionValue[SMRMonClassify, "Voting"]];
      dropZeroScoredLabelsQ = TrueQ[OptionValue[SMRMonClassify, "DropZeroScoredLabels"]];

      expectedProperties = { "Decision", "Probabilities", "Properties" };
      If[ ! MemberQ[ expectedProperties, property ],
        Echo["The value of the option \"Property\" is expected to be one of " <> ToString[expectedProperties] <> ".", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfNearestNeighbors === Automatic] || NumberQ[ numberOfNearestNeighbors ] ),
        Echo["The value of the option \"NumberOfNearestNeighbors\" is expected to be a number or Automatic.", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ ! ( TrueQ[numberOfResults === All] || NumberQ[ numberOfResults ] ),
        Echo["The value of the option \"NumberOfResults\" is expected to be a number or All.", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ ! KeyExistsQ[context, "matrices"],
        Echo["Cannot find the recommendation sub-matrices. (The context key \"matrices\".)", "SMRMonClassify:"];
        Return[$SMRMonFailure]
      ];

      If[ !MemberQ[ Keys[context["matrices"]], tagType],
        Echo[
          "Unknown tag type \"" <> tagType <> "\"; the first argument should be one of: " <>
              ToString[ "\"" <> # <> "\""& /@ Keys[context["matrices"]] ] <> " .",
          "SMRMonClassify:"
        ];
        Return[$SMRMonFailure]
      ];

      clMat = context["matrices", tagType];

      cnAsc = ColumnNamesAssociation[clMat];
      qProfile = KeyComplement[ {profile, cnAsc} ];

      If[ Length[qProfile] == 0,
        Echo[
          "The profile argument has to have at least one tag that does not belong to the tag type \"" <> tagType <> "\".",
          "SMRMonClassify:"
        ];
        Return[$SMRMonFailure]
      ];

      If[ TrueQ[ numberOfNearestNeighbors === Automatic ],
        numberOfNearestNeighbors = Ceiling[ 0.05 * RowsCount[ clMat ] ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors < 10, 10, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = If[ numberOfNearestNeighbors > 200, 200, numberOfNearestNeighbors ];
        numberOfNearestNeighbors = Min[ numberOfNearestNeighbors, RowsCount[clMat] ];
      ];

      recs = Fold[ SMRMonBind, SMRMonUnit[xs, context], {SMRMonRecommendByProfile[qProfile, numberOfNearestNeighbors, "VectorResult" -> True ], SMRMonTakeValue}];

      If[ TrueQ[recs === $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      If[ votingQ,
        clMat01 = SparseArray[clMat];
        t = Most[ArrayRules[clMat]]; t[[All, 2]] = 1.;
        clMat01 = SparseArray[t, Dimensions[clMat]];
        clMat = ToSSparseMatrix[ clMat01, "RowNames" -> RowNames[clMat], "ColumnNames" -> ColumnNames[clMat] ];
        recs = SparseArray[ Thread[ Flatten[First /@ Most[ArrayRules[recs]]] -> 1. ], Length[recs] ];
      ];

      (* Finally the "classification" computation follows. *)
      s = recs . SparseArray[ clMat ];

      If[ Max[s] > 0, s = s / Max[s] ];

      If[ dropZeroScoredLabelsQ,
        resInds = Range[Length[s]];
        resInds = Pick[resInds, UnitStep[-s], 0];
        resInds = resInds[[ Reverse[ Ordering[ Normal @ s[[resInds]] ] ] ]],

        (* ELSE  *)
        resInds = Reverse[ Ordering[ Normal[s] ] ]
      ];

      res = AssociationThread[ ColumnNames[clMat][[resInds]], Normal[s[[resInds]]] ];

      res =
          Which[
            property == "Decision", First[Keys[res]],

            property == "Probabilities" && TrueQ[ numberOfResults === All ], res,

            property == "Probabilities", TakeLargest[ res, UpTo[numberOfResults] ],

            property == "Properties", expectedProperties,

            True, res
          ];

      SMRMonUnit[ res, context ]
    ];

SMRMonClassify[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonClassify[tagType_String, profile_Association, opts___].",
        "SMRMonClassify:"];
      $SMRMonFailure
    ];


(*=========================================================*)
(* Metadata Proofs                                         *)
(*=========================================================*)

Clear[SMRMonMetadataProofs];

Options[SMRMonMetadataProofs] = { "OutlierIdentifierParameters" -> None, "Normalize" -> True };

SMRMonMetadataProofs[$SMRMonFailure] := $SMRMonFailure;

SMRMonMetadataProofs[xs_, context_Association] := $SMRMonFailure;

SMRMonMetadataProofs[][xs_, context_Association] := $SMRMonFailure;

SMRMonMetadataProofs[ profile_Association, itemName_String, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{scores, oiFunc, normalizeQ, res},

      oiFunc = OptionValue[SMRMonMetadataProofs, "OutlierIdentifierParameters"];
      normalizeQ = TrueQ[ OptionValue[SMRMonMetadataProofs, "Normalize"] ];

      (* Check that the argument profile is a profile according to the monad object. *)
      If[ ! ScoredTagsQ[ profile, context ],
        Echo[ "The first argument is not a profile (an association of scored tags) in the monad object.", "SMRMonMetadataProofs:"];
        Return[$SMRMonFailure]
      ];

      scores = ColumnSumsAssociation[ context["M"][[ {itemName}, Keys[profile] ]] ];
      scores = Select[scores, # > 0 &];

      scores = KeySelect[ profile, MemberQ[Keys[scores], #]& ];

      res =
          If[TrueQ[oiFunc === None],
            scores,
            scores[[ If[# === {}, All, #]& @ OutlierPosition[ Values[scores], TopOutliers @* oiFunc ] ]]
          ];

      If[ normalizeQ && Length[res] > 0,
        res = N[ res / Max[res] ]
      ];

      SMRMonUnit[ <|itemName -> ReverseSort[res] |>, context ]
    ];

SMRMonMetadataProofs[ profile_Association, itemNames : {_String..}, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{res},

      res = Map[ Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonMetadataProofs[ profile, #, opts ], SMRMonTakeValue } ]& , itemNames ];

      If[ !FreeQ[res, $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      SMRMonUnit[ Join @@ res, context ]
    ];

SMRMonMetadataProofs[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonMetadataProofs[profile_Association, itemNames:( _String | {_String..} ), opts___] .",
        "SMRMonMetadataProofs:"];
      $SMRMonFailure
    ];



(*=========================================================*)
(* History  Proofs                                         *)
(*=========================================================*)

Clear[SMRMonHistoryProofs];

Options[SMRMonHistoryProofs] = { "OutlierIdentifierParameters" -> None, "Normalize" -> True };

SMRMonHistoryProofs[$SMRMonFailure] := $SMRMonFailure;

SMRMonHistoryProofs[xs_, context_Association] := $SMRMonFailure;

SMRMonHistoryProofs[][xs_, context_Association] := $SMRMonFailure;

SMRMonHistoryProofs[ history_Association, itemName_String, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{ oiFunc, normalizeQ, scores, maxScore, res},

      oiFunc = OptionValue[SMRMonMetadataProofs, "OutlierIdentifierParameters"];
      normalizeQ = TrueQ[ OptionValue[SMRMonMetadataProofs, "Normalize"] ];

      (* Check that the argument profile is a profile according to the monad object. *)
      If[ ! ScoredItemsQ[ history, context ],
        Echo[ "The first argument is not an association of scored items in the monad object.", "SMRMonHistoryProofs:"];
        Return[$SMRMonFailure]
      ];

      scores = SparseArray[ context["M"][[ Keys[history], All ]] . context["M"][[ itemName, All ]] ];

      If[ normalizeQ && Length[scores] > 0,
        maxScore = context["M"][[ itemName, All ]] . context["M"][[ itemName, All ]];
        scores = N[ scores / maxScore ]
      ];

      scores = AssociationThread[ Keys[history], Normal[SparseArray[scores]] * Values[history] ];

      scores = Select[scores, # > 0 &];

      res =
          If[TrueQ[oiFunc === None],
            scores,
            scores[[ If[# === {}, All, #]& @ OutlierPosition[ Values[scores], TopOutliers @* oiFunc ] ]]
          ];

      If[ normalizeQ && Length[res] > 0,
        res = N[ res / Max[res] ]
      ];

      SMRMonUnit[ <| itemName -> ReverseSort[res] |>, context ]
    ];

SMRMonHistoryProofs[ history_Association, itemNames : {_String..}, opts : OptionsPattern[] ][ xs_, context_ ] :=
    Block[{res},

      res = Map[ Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonHistoryProofs[ history, #, opts ], SMRMonTakeValue } ]& , itemNames ];

      If[ !FreeQ[res, $SMRMonFailure],
        Return[$SMRMonFailure]
      ];

      SMRMonUnit[ Join @@ res, context ]
    ];

SMRMonHistoryProofs[___][__] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonHistoryProofs[history_Association, itemName : ( _String | {_String..} ), opts___] .",
        "SMRMonHistoryProofs:"];
      $SMRMonFailure
    ];


End[]; (* `Private` *)

EndPackage[]