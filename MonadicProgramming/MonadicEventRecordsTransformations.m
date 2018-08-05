(*
    Monadic Event Records Transformations Mathematica package
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
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicEventRecordsTransformations *)
(* :Context: MonadicEventRecordsTransformations` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-08-03 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

    This package implements a monad for the transformation of event records in the following (long) form:

    ItemID, SourceID, Variable, ObservationTimeEpoch, Value
      2321,       a1,       HR,           1512429528,    78
      2321,       a1,       RR,           1512429628,    12

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

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];

If[Length[DownValues[OutlierIdentifiers`OutlierPosition]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicEventRecordsTransformations`"];
(* Exported symbols added here with SymbolName::usage *)

ERTMonSetComputationSpecifications::usage = "Assigns the argument to the key \"compSpec\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetEventRecords::usage = "Assigns the argument to the key \"eventRecords\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetItemData::usage = "Assigns the argument to the key \"itemData\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetVariableOutlierBoundaries::usage = "Assigns the argument to the key \"variableOutlierBoundaries\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonTakeComputationSpecifications::usage = "Gives the value of the key \"compSpec\" from the monad context."

ERTMonTakeEventRecords::usage = "Gives the value of the key \"eventRecords\" from the monad context."

ERTMonTakeItemData::usage = "Gives the value of the key \"itemData\" from the monad context."

ERTMonTakeItemVariableRecordGroups::usage = "Gives the value of the key \"itemVariableRecordGroups\" from the monad context."

ERTMonTakeTimeSeries::usage = "Gives the value of the key \"timeSeries\" from the monad context."

ERTMonTakeVariableOutlierBoundaries::usage = "Gives the value of the key \"variableOutlierBoundaries\" from the monad context."

ERTMonTakeContingencyMatrices::usage = "Gives the value of the key \"contingencyMatrices\" from the monad context."

ERTMonReadData::usage = "Reads data from specified files or directory."

ERTMonGroupItemVariableRecords::usage = "Groups item-variable records. \
Only the variables in the specification are used."

ERTMonRecordGroupsToTimeSeries::usage = "Converts the groups of item-variable records into time series. \
The time series are restricted to the corresponding variable maximum time given in the specification."

ERTMonTimeSeriesAggregation::usage = "Aggregates the event records time series according to the specification."

ERTMonFindVariableOutlierBoundaries::usage = "Find outlier boundaries for each variable in the item-variable record groups."

ERTMonMakeContingencyMatrices::usage = "Make contingency matrices for the time series."

Begin["`Private`"];

Needs["MathematicaForPredictionUtilities`"]
Needs["StateMonadCodeGenerator`"]
Needs["CrossTabulate`"]
Needs["OutlierIdentifiers`"]


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of LSAMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicEventRecordsTransformations`ERTMon", "FailureSymbol" -> $ERTMonFailure, "StringContextNames" -> False ]


(**************************************************************)
(* Ingestion of transformation specifications                 *)
(**************************************************************)

Clear[GetAssociation]
GetAssociation[compSpec_Dataset, colName_] := Normal[compSpec[All, colName]];

Clear[ProcessComputationalSpecification]
ProcessComputationalSpecification[fname_String] :=
    ProcessComputationalSpecification[DeleteCases[Import[fname, "CSV"], {}]];

ProcessComputationalSpecification[compSpecArg_?MatrixQ] :=
    Block[{compSpec = compSpecArg, compSpecColumnNames, rowIDs},
      compSpecColumnNames = First[compSpec];
      compSpec = Select[Rest[compSpec], Length[#] > 0 &];
      compSpec =
          Dataset[compSpec][All,
            AssociationThread[compSpecColumnNames, #] &];
      compSpec = Dataset[compSpec];
      rowIDs =
          StringRiffle[{##} /. "NULL" -> Nothing, "."] & @@@
              Normal[Query[All, Values]@
                  compSpec[All, {"Variable", "Aggregation function"}]];
      compSpec = Dataset[AssociationThread[rowIDs -> Normal[compSpec]]];
      compSpec
    ];


aAggregationFunctionSpec = <| "Mean"->Mean, "Range"-> (Max[#]-Min[#]&), "Count"->Length, "None"->Identity |>;

(**************************************************************)
(* Setters and takers                                         *)
(**************************************************************)

ClearAll[ERTMonSetComputationSpecifications]
ERTMonSetComputationSpecifications[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetComputationSpecifications[][$ERTMonFailure] := $ERTMonFailure;
ERTMonSetComputationSpecifications[xs_, context_] := $ERTMonFailure;
ERTMonSetComputationSpecifications[data_List][xs_, context_] := ERTMonUnit[ xs, Join[ context, <|"compSpec"->data|> ] ];
ERTMonSetComputationSpecifications[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeComputationSpecifications]
ERTMonTakeComputationSpecifications[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeComputationSpecifications[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeComputationSpecifications[xs_, context_] := ERTMonTakeComputationSpecifications[][xs, context];
ERTMonTakeComputationSpecifications[][xs_, context_] := context["compSpec"];
ERTMonTakeComputationSpecifications[__][___] := $ERTMonFailure;


ClearAll[ERTMonSetEventRecords]
ERTMonSetEventRecords[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetEventRecords[][$ERTMonFailure] := $ERTMonFailure;
ERTMonSetEventRecords[xs_, context_] := $ERTMonFailure;
ERTMonSetEventRecords[data_List][xs_, context_] := ERTMonUnit[ xs, Join[ context, <|"eventRecords"->data|> ] ];
ERTMonSetEventRecords[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeEventRecords]
ERTMonTakeEventRecords[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEventRecords[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEventRecords[xs_, context_] := ERTMonTakeEventRecords[][xs, context];
ERTMonTakeEventRecords[][xs_, context_] :=
    Block[{ds},
      ds = Dataset[context["eventRecords"]];
      Dataset[ds[All, AssociationThread[context["eventRecordsColumnNames"], #] &]]
    ];
ERTMonTakeEventRecords[__][___] := $ERTMonFailure;


ClearAll[ERTMonSetItemData]
ERTMonSetItemData[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetItemData[][$ERTMonFailure] := $ERTMonFailure;
ERTMonSetItemData[xs_, context_] := $ERTMonFailure;
ERTMonSetItemData[data_List][xs_, context_] := ERTMonUnit[ xs, Join[ context, <|"itemData"->data|> ] ];
ERTMonSetItemData[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeItemData]
ERTMonTakeItemData[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeItemData[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeItemData[xs_, context_] := ERTMonTakeItemData[][xs, context];
ERTMonTakeItemData[][xs_, context_] :=
    Block[{ds},
      ds = Dataset[context["itemData"]];
      Dataset[ds[All, AssociationThread[context["itemDataColumnNames"], #] &]]
    ];
ERTMonTakeItemData[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeItemVariableRecordGroups]
ERTMonTakeItemVariableRecordGroups[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeItemVariableRecordGroups[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeItemVariableRecordGroups[xs_, context_] := ERTMonTakeItemVariableRecordGroups[][xs, context];
ERTMonTakeItemVariableRecordGroups[][xs_, context_] := context["itemVariableRecordGroups"];
ERTMonTakeItemVariableRecordGroups[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeTimeSeries]
ERTMonTakeTimeSeries[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeTimeSeries[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeTimeSeries[xs_, context_] := ERTMonTakeTimeSeries[][xs, context];
ERTMonTakeTimeSeries[][xs_, context_] := context["timeSeries"];
ERTMonTakeTimeSeries[__][___] := $ERTMonFailure;


ClearAll[ERTMonSetVariableOutlierBoundaries]
ERTMonSetVariableOutlierBoundaries[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetVariableOutlierBoundaries[][$ERTMonFailure] := $ERTMonFailure;
ERTMonSetVariableOutlierBoundaries[xs_, context_] := $ERTMonFailure;
ERTMonSetVariableOutlierBoundaries[data_Association][xs_, context_] := ERTMonUnit[ xs, Join[ context, <|"variableOutlierBoundaries"->data|> ] ];
ERTMonSetVariableOutlierBoundaries[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeVariableOutlierBoundaries]
ERTMonTakeVariableOutlierBoundaries[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeVariableOutlierBoundaries[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeVariableOutlierBoundaries[xs_, context_] := ERTMonTakeVariableOutlierBoundaries[][xs, context];
ERTMonTakeVariableOutlierBoundaries[][xs_, context_] := context["variableOutlierBoundaries"];
ERTMonTakeVariableOutlierBoundaries[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeContingencyMatrices]
ERTMonTakeContingencyMatrices[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeContingencyMatrices[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeContingencyMatrices[xs_, context_] := ERTMonTakeContingencyMatrices[][xs, context];
ERTMonTakeContingencyMatrices[][xs_, context_] := context["contingencyMatrices"];
ERTMonTakeContingencyMatrices[__][___] := $ERTMonFailure;


(**************************************************************)
(* Ingestion of data                                          *)
(**************************************************************)

ClearAll[ERTMonReadData];

ERTMonReadData[$ERTMonFailure] := $ERTMonFailure;

ERTMonReadData[xs_, context_] := $ERTMonFailure

ERTMonReadData[dirName_String][xs_, context_] :=
    ERTMonReadData[
      <| "eventRecords" -> FileNameJoin[{dirName, "medicalRecordsData.csv"}],
         "itemData" -> FileNameJoin[{dirName, "patientData.csv"}],
         "dataIngestionSpecifications" -> FileNameJoin[{dirName, "dataIngestionSpecifications.csv"}]|> ][xs, context];

ERTMonReadData[aFileNames_Association][xs_, context_] :=
    Block[{compSpec, eventRecords, eventRecordsColumnNames, itemData, itemDataColumnNames, itemID},

      compSpec = ProcessComputationalSpecification[ aFileNames["dataIngestionSpecifications"] ];

      eventRecords = Import[aFileNames["eventRecords"], "CSV"];

      eventRecordsColumnNames = First[eventRecords];
      eventRecords = Rest[eventRecords];

      itemData = Import[aFileNames["itemData"], "CSV"];

      itemDataColumnNames = First[itemData];
      itemData = Rest[itemData];

      (* Determining the item ID column name. *)
      itemID = Flatten[StringCases[itemDataColumnNames, __~~("ID"|"Id")]];

      If[Length[itemID]==0,
        Echo["Cannot find name of item ID columns.", "ERTMontReadData:"];
        Return[$ERTMonFailure]
      ];

      eventRecordsColumnNames = StringReplace[ eventRecordsColumnNames, itemID -> "ItemID" ];
      itemDataColumnNames = StringReplace[ itemDataColumnNames, itemID -> "ItemID"];

      ERTMonUnit[
        xs,
        Join[context,
          <| "eventRecords"->eventRecords, "itemData"->itemData,
             "eventRecordsColumnNames"->eventRecordsColumnNames, "itemDataColumnNames"->itemDataColumnNames,
             "compSpec"->compSpec|>] ]

    ];

ERTMonReadData[___][__] := $ERTMonFailure;


(**************************************************************)
(* Medical records item-variable groups                    *)
(**************************************************************)

ClearAll[ERTMonGroupItemVariableRecords]

ERTMonGroupItemVariableRecords[$ERTMonFailure] := $ERTMonFailure;

ERTMonGroupItemVariableRecords[xs_, context_] := ERTMonGroupItemVariableRecords[][xs, context];

ERTMonGroupItemVariableRecords[][xs_, context_] :=
    Block[{ds, dsTSGroups, tsGroups, csVars},

      csVars = Values[ GetAssociation[context["compSpec"], "Variable"] ];

      ds = Dataset[ context["eventRecords"] ];
      ds = Dataset[ ds[All, AssociationThread[ context["eventRecordsColumnNames"], #] &] ];

      ds = ds[ Select[ MemberQ[ csVars, #Variable ]& ] ];

      dsTSGroups =
          Query[GroupBy[{#["ItemID"], #["Variable"]} &], All, {"ObservationTimeEpoch", "Value"}] @ ds;
      tsGroups = Normal@dsTSGroups;

      ERTMonUnit[xs, Join[context, <| "itemVariableRecordGroups"->tsGroups |>]]
    ];

ERTMonGroupItemVariableRecords[___][__] := $ERTMonFailure;


(**************************************************************)
(* Item-variable records groups to time series             *)
(**************************************************************)

ClearAll[ERTMonRecordGroupsToTimeSeries]

ERTMonRecordGroupsToTimeSeries[$ERTMonFailure] := $ERTMonFailure;

ERTMonRecordGroupsToTimeSeries[xs_, context_] := ERTMonRecordGroupsToTimeSeries[][xs, context];

ERTMonRecordGroupsToTimeSeries[][xs_, context_] :=
    Block[{ts},

      (* For each group of item and variable records find the most recent measurement time and
         make a time series going back from that most recent time. *)
      ts =
          Map[
            TimeSeries[Transpose[{Through[#["ObservationTimeEpoch"]] - Max[Through[#["ObservationTimeEpoch"]]], Through[#["Value"]]}]] &,
            context["itemVariableRecordGroups"]];

      ERTMonUnit[xs, Join[context, <| "timeSeries"->ts |>]]
    ];

ERTMonRecordGroupsToTimeSeries[___][__] := $ERTMonFailure;


(**************************************************************)
(* Time series aggregation                                    *)
(**************************************************************)

Clear[AggregateBySpec]
AggregateBySpec[timeSeries_Association, specRow_Association] :=
    Block[{ts},

      Which[
        MemberQ[ {"Unit", "Age", "Label"}, specRow["Variable"]],
        <||>,

        NumberQ[specRow["Aggregation time interval"]] && NumberQ[specRow["Max history length"]],

        ts = KeySelect[timeSeries, MatchQ[#, {_, specRow["Variable"]}] &];

        ts = Map[ TimeSeriesWindow[#, {-specRow["Max history length"], 0} ]&, ts];

        ts = Map[ TimeSeriesAggregate[#, specRow["Aggregation time interval"], aAggregationFunctionSpec[specRow["Aggregation function"]] ]&, ts];

        ts = KeyMap[ {#[[1]], StringJoin[specRow["Variable"], ".", specRow["Aggregation function"]]}&, ts];
        ts,

        True,
        <||>
      ]
    ];

ClearAll[ERTMonTimeSeriesAggregation]

ERTMonTimeSeriesAggregation[$ERTMonFailure] := $ERTMonFailure;

ERTMonTimeSeriesAggregation[xs_, context_] := ERTMonTimeSeriesAggregation[][xs, context];

ERTMonTimeSeriesAggregation[][xs_, context_] :=
    Block[{compSpec, ts},

      compSpec = context["compSpec"];
      ts = context["timeSeries"];

      ts = Join @@ Map[ AggregateBySpec[ts, #]&, Normal[compSpec] ];

      ERTMonUnit[xs, Join[context, <| "timeSeries"->ts |>]]
    ];

ERTMonTimeSeriesAggregation[___][__] := $ERTMonFailure;


(**************************************************************)
(* Find variable outliers                                     *)
(**************************************************************)

ClearAll[ERTMonFindVariableOutlierBoundaries]

ERTMonFindVariableOutlierBoundaries[$ERTMonFailure] := $ERTMonFailure;

ERTMonFindVariableOutlierBoundaries[xs_, context_] := ERTMonFindVariableOutlierBoundaries[][xs, context];

ERTMonFindVariableOutlierBoundaries[][xs_, context_] := ERTMonFindVariableOutlierBoundaries[HampelIdentifierParameters][xs, context];

ERTMonFindVariableOutlierBoundaries[outlierParametersFunction_][xs_, context_] :=
    Block[{ivRowSpecIDs, outlierBoundaries},

      ivRowSpecIDs = Union[Keys[context["itemVariableRecordGroups"]][[All, 2]]];

      outlierBoundaries = Map[ # -> outlierParametersFunction[Map[#["Value"] &, Flatten[Values[KeySelect[context["itemVariableRecordGroups"], MatchQ[{_, #}]]]]]]&, ivRowSpecIDs];

      ERTMonUnit[xs, Join[context, <| "variableOutlierBoundaries"->outlierBoundaries |>]]
    ];

ERTMonFindVariableOutlierBoundaries[___][__] := $ERTMonFailure;


(**************************************************************)
(* Make contingency matrices                                  *)
(**************************************************************)

ClearAll[ERTMonMakeContingencyMatrices]

ERTMonMakeContingencyMatrices[$ERTMonFailure] := $ERTMonFailure;

ERTMonMakeContingencyMatrices[xs_, context_] := ERTMonMakeContingencyMatrices[][xs, context];

ERTMonMakeContingencyMatrices[][xs_, context_] :=
    Block[{tsRowSpecIDs, tbls, cmats},

      tsRowSpecIDs = Union[Keys[context["timeSeries"]][[All, 2]]];

      tbls =
          Map[
            Function[{rsId},
              Join @@ KeyValueMap[Flatten /@ Thread[{#1[[1]], #2["Path"]}] &,
                KeySelect[context["timeSeries"], MatchQ[#, {_, rsId}] &]]
            ], tsRowSpecIDs];

      cmats = CrossTabulate /@ tbls;

      ERTMonUnit[xs, Join[context, <| "contingencyMatrices"->cmats |>]]
    ];

ERTMonTimeSeriesAggregation[___][__] := $ERTMonFailure;


End[]; (* `Private` *)

EndPackage[]