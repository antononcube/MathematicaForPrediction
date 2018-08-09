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

    EntityID, SourceID, Variable, ObservationTimeEpoch, Value
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

$ERTMonFailure::usage = "Failure symbol for the monad ERTMon."

ERTMonSetComputationSpecifications::usage = "Assigns the argument to the key \"compSpec\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetEventRecords::usage = "Assigns the argument to the key \"eventRecords\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetEntityData::usage = "Assigns the argument to the key \"entityData\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetVariableOutlierBoundaries::usage = "Assigns the argument to the key \"variableOutlierBoundaries\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonTakeComputationSpecifications::usage = "Gives the value of the key \"compSpec\" from the monad context."

ERTMonTakeEventRecords::usage = "Gives the value of the key \"eventRecords\" from the monad context."

ERTMonTakeEntityData::usage = "Gives the value of the key \"entityData\" from the monad context."

ERTMonTakeEntityVariableRecordGroups::usage = "Gives the value of the key \"entityVariableRecordGroups\" from the monad context."

ERTMonTakeTimeSeries::usage = "Gives the value of the key \"timeSeries\" from the monad context."

ERTMonTakeVariableOutlierBoundaries::usage = "Gives the value of the key \"variableOutlierBoundaries\" from the monad context."

ERTMonTakeContingencyMatrices::usage = "Gives the value of the key \"contingencyMatrices\" from the monad context."

ERTMonReadData::usage = "Reads data from specified files or directory."

ERTMonGroupEntityVariableRecords::usage = "Groups entity-variable records. \
Only the variables in the specification are used."

ERTMonRecordGroupsToTimeSeries::usage = "Converts the groups of entity-variable records into time series. \
The time series are restricted to the corresponding variable maximum time given in the specification."

ERTMonTimeSeriesAggregation::usage = "Aggregates the event records time series according to the specification."

ERTMonFindVariableOutlierBoundaries::usage = "Find outlier boundaries for each variable in the entity-variable record groups."

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


aAggregationFunctionSpec = <| "Mean"->Mean, "Range"-> (Max[#]-Min[#]&), "Count"->Length, "None"->Identity,
  "OutliersCount"->OutliersCount, "OutliersFraction"->OutliersFraction |>;

ClearAll[OutliersCount]
OutliersCount[vec:{_?NumberQ..}, {lower_?NumberQ, upper_?NumberQ}] :=
    Length[Select[vec, # < lower || upper < #&]];

ClearAll[OutliersFraction]
OutliersFraction[vec:{_?NumberQ..}, {lower_?NumberQ, upper_?NumberQ}] :=
    Length[Select[vec, # < lower || upper < #&]] / Length[vec];


(**************************************************************)
(* Setters and takers                                         *)
(**************************************************************)

ClearAll[ERTMonSetComputationSpecifications]
ERTMonSetComputationSpecifications[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetComputationSpecifications[][$ERTMonFailure] := $ERTMonFailure;
ERTMonSetComputationSpecifications[xs_, context_] := $ERTMonFailure;
ERTMonSetComputationSpecifications[ds_Dataset][xs_, context_] := ERTMonUnit[ xs, Join[ context, <|"compSpec"->ds|> ] ];
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


ClearAll[ERTMonSetEntityData]
ERTMonSetEntityData[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetEntityData[][$ERTMonFailure] := $ERTMonFailure;
ERTMonSetEntityData[xs_, context_] := $ERTMonFailure;
ERTMonSetEntityData[data_List][xs_, context_] := ERTMonUnit[ xs, Join[ context, <|"entityData"->data|> ] ];
ERTMonSetEntityData[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeEntityData]
ERTMonTakeEntityData[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEntityData[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEntityData[xs_, context_] := ERTMonTakeEntityData[][xs, context];
ERTMonTakeEntityData[][xs_, context_] :=
    Block[{ds},
      ds = Dataset[context["entityData"]];
      Dataset[ds[All, AssociationThread[context["entityDataColumnNames"], #] &]]
    ];
ERTMonTakeEntityData[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeEntityVariableRecordGroups]
ERTMonTakeEntityVariableRecordGroups[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEntityVariableRecordGroups[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEntityVariableRecordGroups[xs_, context_] := ERTMonTakeEntityVariableRecordGroups[][xs, context];
ERTMonTakeEntityVariableRecordGroups[][xs_, context_] := context["entityVariableRecordGroups"];
ERTMonTakeEntityVariableRecordGroups[__][___] := $ERTMonFailure;


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

ERTMonReadData[xs_, context_Association] := $ERTMonFailure

ERTMonReadData[dirName_String][xs_, context_] :=
    ERTMonReadData[
      <| "eventRecords" -> FileNameJoin[{dirName, "medicalRecordsData.csv"}],
         "entityData" -> FileNameJoin[{dirName, "patientData.csv"}],
         "dataIngestionSpecifications" -> FileNameJoin[{dirName, "dataIngestionSpecifications.csv"}]|> ][xs, context];

ERTMonReadData[aFileNames_Association][xs_, context_] :=
    Block[{compSpec, eventRecords, eventRecordsColumnNames, entityData, entityDataColumnNames, entityID},

      compSpec = ProcessComputationalSpecification[ aFileNames["dataIngestionSpecifications"] ];

      eventRecords = Import[aFileNames["eventRecords"], "CSV"];

      eventRecordsColumnNames = First[eventRecords];
      eventRecords = Rest[eventRecords];

      entityData = Import[aFileNames["entityData"], "CSV"];

      entityDataColumnNames = First[entityData];
      entityData = Rest[entityData];

      (* Determining the entity ID column name. *)
      entityID = Flatten[StringCases[entityDataColumnNames, __~~("ID"|"Id")]];

      If[Length[entityID]==0,
        Echo["Cannot find name of entity ID columns.", "ERTMontReadData:"];
        Return[$ERTMonFailure]
      ];

      eventRecordsColumnNames = StringReplace[ eventRecordsColumnNames, entityID -> "EntityID" ];
      entityDataColumnNames = StringReplace[ entityDataColumnNames, entityID -> "EntityID"];

      ERTMonUnit[
        xs,
        Join[context,
          <| "eventRecords"->eventRecords, "entityData"->entityData,
             "eventRecordsColumnNames"->eventRecordsColumnNames, "entityDataColumnNames"->entityDataColumnNames,
             "compSpec"->compSpec|>] ]

    ];

ERTMonReadData[___][__] := $ERTMonFailure;


(**************************************************************)
(* Medical records entity-variable groups                    *)
(**************************************************************)

ClearAll[ERTMonGroupEntityVariableRecords]

ERTMonGroupEntityVariableRecords[$ERTMonFailure] := $ERTMonFailure;

ERTMonGroupEntityVariableRecords[xs_, context_Association] := ERTMonGroupEntityVariableRecords[][xs, context];

ERTMonGroupEntityVariableRecords[][xs_, context_] :=
    Block[{ds, dsTSGroups, tsGroups, csVars},

      csVars = Values[ GetAssociation[context["compSpec"], "Variable"] ];

      ds = Dataset[ context["eventRecords"] ];
      ds = Dataset[ ds[All, AssociationThread[ context["eventRecordsColumnNames"], #] &] ];

      ds = ds[ Select[ MemberQ[ csVars, #Variable ]& ] ];

      dsTSGroups =
          Query[GroupBy[{#["EntityID"], #["Variable"]} &], All, {"ObservationTimeEpoch", "Value"}] @ ds;
      tsGroups = Normal@dsTSGroups;

      ERTMonUnit[xs, Join[context, <| "entityVariableRecordGroups"->tsGroups |>]]
    ];

ERTMonGroupEntityVariableRecords[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "ERTMonGroupEntityVariableRecords:"];
      $ERTMonFailure
    ];

(**************************************************************)
(* Entity-variable records groups to time series             *)
(**************************************************************)

ClearAll[ERTMonRecordGroupsToTimeSeries]

ERTMonRecordGroupsToTimeSeries[$ERTMonFailure] := $ERTMonFailure;

ERTMonRecordGroupsToTimeSeries[xs_, context_Association] := ERTMonRecordGroupsToTimeSeries[][xs, context];

ERTMonRecordGroupsToTimeSeries[zeroTimeArg_String:"MaxTime"][xs_, context_] :=
    Block[{ts, zeroTime=zeroTimeArg},

      Which[
        MemberQ[{"MinTime", "MinimumTime", "StartTime"}, zeroTime],
        zeroTime = "MinTime",

        MemberQ[{"MaxTime", "MaximumTime", "EndTime"}, zeroTime],
        zeroTime = "MaxTime",

        True,
        Echo["The allowed values for the first argument are \"MinTime\" and \"MaxTime\".", "ERTMonRecordGroupsToTimeSeries:"];
        Return[$ERTMonFailure]
      ];

      If[ !KeyExistsQ[context, "entityVariableRecordGroups"],
        Echo["Cannot find entity-variable records groups. (Call ERTMonGroupEntityVariableRecords first.)", "ERTMonRecordGroupsToTimeSeries:"];
        Return[$ERTMonFailure]
      ];

      (* Assume zeroTime == "MaxTime".
         Then for each group of entity and variable records find the most recent measurement time and
         make a time series going back from that most recent time.
      *)
      ts =
          Map[
            TimeSeries[
              Transpose[{
                Through[#["ObservationTimeEpoch"]] -
                    If[ zeroTime == "MinTime",
                      Min[Through[#["ObservationTimeEpoch"]]],
                      Max[Through[#["ObservationTimeEpoch"]]]
                    ],
                Through[#["Value"]]}
              ]
            ] &,
            context["entityVariableRecordGroups"]];

      ERTMonUnit[xs, Join[context, <| "timeSeries"->ts |>]]
    ];

ERTMonRecordGroupsToTimeSeries[___][__] :=
    Block[{},
      Echo["One or no arguments are expected. The argument can have the values \"MinTime\" or \"MaxTime\".", "ERTMonRecordGroupsToTimeSeries:"];
      $ERTMonFailure
    ];


(**************************************************************)
(* Time series aggregation                                    *)
(**************************************************************)

Clear[AggregateBySpec]
AggregateBySpec[timeSeries_Association, specRow_Association, aAggregationFunctionSpec_Association] :=
    Block[{ts, timeWindowSpec},

      Which[
        MemberQ[ {"Unit", "Age", "Label"}, specRow["Variable"]],
        <||>,

        NumberQ[specRow["Aggregation time interval"]] && NumberQ[specRow["Max history length"]],

        ts = KeySelect[timeSeries, MatchQ[#, {_, specRow["Variable"]}] &];

        ts = Map[ TimeSeriesWindow[#, If[ #["FirstTime"] == 0, {0, specRow["Max history length"]}, {-specRow["Max history length"], 0}] ]&, ts];

        ts = Map[ TimeSeriesAggregate[#, specRow["Aggregation time interval"], aAggregationFunctionSpec[specRow["Aggregation function"]] ]&, ts];

        ts = KeyMap[ {#[[1]], StringJoin[specRow["Variable"], ".", specRow["Aggregation function"]]}&, ts];
        ts,

        True,
        <||>
      ]
    ];

ClearAll[ERTMonTimeSeriesAggregation]

ERTMonTimeSeriesAggregation[$ERTMonFailure] := $ERTMonFailure;

ERTMonTimeSeriesAggregation[xs_, context_Association] := ERTMonTimeSeriesAggregation[][xs, context];

ERTMonTimeSeriesAggregation[][xs_, context_] :=
    Block[{compSpec, ts, aAggrFuncs},

      compSpec = context["compSpec"];
      ts = context["timeSeries"];

      If[ Length[Intersection[{"OutliersCount", "OutliersFraction"}, Normal[compSpec[Values,"Aggregation function"]]]] > 0 &&
          !KeyExistsQ[context, "variableOutlierBoundaries"],

        Echo["Calculate outlier boundaries first.", "ERTMonTimeSeriesAggregation"];
        Return[$ERTMonFailure]
      ];

      ts =
          Map[
            With[{lu = context["variableOutlierBoundaries"][#["Variable"]] },
              aAggrFuncs = Join[ aAggregationFunctionSpec, <| "OutliersCount" -> (OutliersCount[#, lu]&), "OutliersFraction" -> (OutliersFraction[#, lu]&) |> ];
              AggregateBySpec[ts, #, aAggrFuncs]
            ]&,
            Normal[compSpec]
          ];

      ts = Join @@ ts;

      ERTMonUnit[xs, Join[context, <| "timeSeries"->ts |>]]
    ];

ERTMonTimeSeriesAggregation[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "ERTMonTimeSeriesAggregation:"];
      $ERTMonFailure
    ];

(**************************************************************)
(* Find variable outliers                                     *)
(**************************************************************)

ClearAll[ERTMonFindVariableOutlierBoundaries]

ERTMonFindVariableOutlierBoundaries[$ERTMonFailure] := $ERTMonFailure;

ERTMonFindVariableOutlierBoundaries[xs_, context_Association] := ERTMonFindVariableOutlierBoundaries[][xs, context];

ERTMonFindVariableOutlierBoundaries[][xs_, context_] := ERTMonFindVariableOutlierBoundaries[HampelIdentifierParameters][xs, context];

ERTMonFindVariableOutlierBoundaries[outlierParametersFunction_][xs_, context_] :=
    Block[{ivRowSpecIDs, outlierBoundaries},

      ivRowSpecIDs = Union[Keys[context["entityVariableRecordGroups"]][[All, 2]]];

      outlierBoundaries =
          Association @
              Map[ # -> outlierParametersFunction[Map[#["Value"] &, Flatten[Values[KeySelect[context["entityVariableRecordGroups"], MatchQ[{_, #}]]]]]]&, ivRowSpecIDs];

      ERTMonUnit[xs, Join[context, <| "variableOutlierBoundaries"->outlierBoundaries |>]]
    ];

ERTMonFindVariableOutlierBoundaries[___][__] :=
    Block[{},
      Echo[
        StringRiffle[
          {"One or no arguments are expected. The argument is a function that finds lower and upper outlier boundaries for a list of numbers.",
            "(Here are such function names from the package \"OutlierIdentifiers.m\": " <> ToString[Names["OutlierIdentifiers`*Parameters"]] <> ".)"
          }," "],
        "ERTMonFindVariableOutlierBoundaries:"
      ];
      $ERTMonFailure
    ];


(**************************************************************)
(* Make contingency matrices                                  *)
(**************************************************************)

ClearAll[ERTMonMakeContingencyMatrices]

ERTMonMakeContingencyMatrices[$ERTMonFailure] := $ERTMonFailure;

ERTMonMakeContingencyMatrices[xs_, context_Association] := ERTMonMakeContingencyMatrices[][xs, context];

ERTMonMakeContingencyMatrices[][xs_, context_] :=
    Block[{tsRowSpecIDs, tbls, cmats},

      tsRowSpecIDs = Union[Keys[context["timeSeries"]][[All, 2]]];

      tbls =
          Association @
              Map[
                Function[{rsId},
                  rsId ->
                      Apply[
                        Join,
                        KeyValueMap[
                          Flatten /@ Thread[{#1[[1]], #2["Path"]}] &,
                          KeySelect[context["timeSeries"], MatchQ[#, {_, rsId}] &]]]
                ],
                tsRowSpecIDs];

      cmats = CrossTabulate /@ tbls;

      ERTMonUnit[xs, Join[context, <| "contingencyMatrices"->cmats |>]]
    ];

ERTMonTimeSeriesAggregation[___][__] := $ERTMonFailure;


End[]; (* `Private` *)

EndPackage[]