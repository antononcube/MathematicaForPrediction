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

    EntityID, SourceID, Variable, ObservationTime, Value
      2321,       a1,       HR,        1512429528,    78
      2321,       a1,       RR,        1512429628,    12


    TODO:
    1. [ ] Consider moving the data into Dataset objects with named columns.
           Currently, say, the event records data is stored in a matrix and a vector of column names.

    2. [ ] Consider the splicing of the sparse matrices. This would mean using the SSparseMatrix.m package.

    3. [ ] Investigate does the computational specification Dataset object has to have named rows.

    4. [ ] Implement data column types check for event records data.
           Only column names are enforced through ERTMonSetEventRecords.

    5. [ ] Better initialization with ERTMonUnit, using datasets for event records and entity data (and computation specification.)

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

ERTMonSetComputationSpecification::usage = "Assigns the argument to the key \"compSpec\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetEventRecords::usage = "Assigns the argument to the key \"eventRecords\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetEntityAttributes::usage = "Assigns the argument to the key \"entityData\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonSetVariableOutlierBoundaries::usage = "Assigns the argument to the key \"variableOutlierBoundaries\" in the monad context. \
(The rest of the monad context is unchanged.)"

ERTMonTakeComputationSpecification::usage = "Gives the value of the key \"compSpec\" from the monad context."

ERTMonTakeEventRecords::usage = "Gives the value of the key \"eventRecords\" from the monad context."

ERTMonTakeEntityAttributes::usage = "Gives the value of the key \"entityData\" from the monad context."

ERTMonTakeEntityVariableRecordGroups::usage = "Gives the value of the key \"entityVariableRecordGroups\" from the monad context."

ERTMonTakeTimeSeries::usage = "Gives the value of the key \"timeSeries\" from the monad context."

ERTMonTakeVariableOutlierBoundaries::usage = "Gives the value of the key \"variableOutlierBoundaries\" from the monad context."

ERTMonTakeContingencyMatrices::usage = "Gives the value of the key \"contingencyMatrices\" from the monad context."

ERTMonReadData::usage = "Reads data from specified files or directory."

ERTMonEchoDataSummary::usage = "Echoes a summary of the data."

ERTMonGroupEntityVariableRecords::usage = "Groups entity-variable records. \
Only the variables in the specification are used."

ERTMonRecordGroupsToTimeSeries::usage = "Converts the groups of entity-variable records into time series. \
The time series are restricted to the corresponding variable maximum time given in the specification."

ERTMonAggregateTimeSeries::usage = "Aggregates the event records time series according to the specification."

ERTMonFindVariableDistributions::usage = "Find the distribution of each variable in the entity-variable record groups."

ERTMonFindVariableOutlierBoundaries::usage = "Find outlier boundaries for each variable in the entity-variable record groups."

ERTMonMakeContingencyMatrices::usage = "Make contingency matrices for the time series."

ERTMonProcessComputationSpecification::usage = "Process computations specifications. \
The argument can be a file name string, a matrix, or a dataset."

ProcessComputationSpecification::usage = "Process computations specifications. \
The argument can be a file name string, a matrix, or a dataset."

EmptyComputationSpecificationRow::usage = "Gives empty computation specification row."

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

Clear[ProcessComputationSpecification]
ProcessComputationSpecification[fname_String] :=
    ProcessComputationSpecification[DeleteCases[Import[fname, "CSV"], {}]];

ProcessComputationSpecification[ds_Dataset] :=
    Block[{mat},
      If[ AssociationQ[Normal[ds[1]]],
        mat = Normal[ds[All,Values]];
        If[ AssociationQ[mat], mat = Values[mat] ];
        ProcessComputationSpecification[ Prepend[ mat, Keys[Normal[ds[1]]] ] ],
        (*ELSE*)
        $ERTMonFailure
      ]
    ];

ProcessComputationSpecification[compSpecArg_?MatrixQ] :=
    Block[{compSpec = compSpecArg, compSpecColumnNames, rowIDs},
      compSpecColumnNames = First[compSpec];
      compSpecColumnNames =
          Map[
            StringJoin@StringReplace[StringReplace[#, WordBoundary ~~ x_ :> ToUpperCase[x]], " " -> ""] &,
            compSpecColumnNames
          ];
      compSpec = Select[Rest[compSpec], Length[#] > 0 &];
      compSpec =
          Dataset[compSpec][All,
            AssociationThread[compSpecColumnNames, #] &];
      compSpec = Dataset[compSpec];
      rowIDs =
          StringRiffle[{##} /. "NULL" -> Nothing, "."] & @@@
              Normal[Query[All, Values]@
                  compSpec[All, {"Variable", "AggregationFunction"}]];
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
(* Empty computation specification                            *)
(**************************************************************)

compSpecRowKeys = {"Variable", "Explanation", "Type", "ConvertType",
  "AggregationTimeInterval", "AggregationFunction",
  "MaxHistoryLength", "Normalization", "MovingAverageWindow",
  "CriticalConditionLabel"};

Clear[EmptyComputationSpecificationRow]
EmptyComputationSpecificationRow[] =
    Association[{"Variable" -> Missing[], "Explanation" -> "",
      "Type" -> Missing[], "ConvertType" -> "NULL",
      "AggregationTimeInterval" -> 60, "AggregationFunction" -> "Mean",
      "MaxHistoryLength" -> 3600, "Normalization" -> "NULL",
      "MovingAverageWindow" -> "NULL", "CriticalLabel" -> "NULL"}];


(**************************************************************)
(* Setters and takers                                         *)
(**************************************************************)

ClearAll[ERTMonSetComputationSpecification]
ERTMonSetComputationSpecification[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetComputationSpecification[][___] := $ERTMonFailure;
ERTMonSetComputationSpecification[xs_, context_] := $ERTMonFailure;
ERTMonSetComputationSpecification[ds_Dataset][xs_, context_] := ERTMonUnit[ xs, Join[ context, <|"compSpec"->ds|> ] ];
ERTMonSetComputationSpecification[__][___] := $ERTMonFailure;


ClearAll[ERTMonTakeComputationSpecification]
ERTMonTakeComputationSpecification[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeComputationSpecification[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeComputationSpecification[xs_, context_] := ERTMonTakeComputationSpecification[][xs, context];
ERTMonTakeComputationSpecification[][xs_, context_] := context["compSpec"];
ERTMonTakeComputationSpecification[__][___] := $ERTMonFailure;


ClearAll[ERTMonSetEventRecords]
ERTMonSetEventRecords[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetEventRecords[][___] := $ERTMonFailure;
ERTMonSetEventRecords[xs_, context_Association] := $ERTMonFailure;
ERTMonSetEventRecords[data_?MatrixQ, colNames_?VectorQ][xs_, context_] :=
    If[ Dimensions[data][[2]] == Length[colNames] &&
        Length[Intersection[colNames, {"EntityID", "LocationID", "Variable", "Value", "ObservationTime"}]] == 5,
      ERTMonUnit[ xs, Join[ context, <|"eventRecords"->data, "eventRecordsColumnNames"->colNames|> ] ],
      (*ELSE*)
      ERTMonSetEventRecords[""][]
    ];
ERTMonSetEventRecords[data_Dataset][xs_, context_] :=
    ERTMonSetEventRecords[ Normal@data[All, Values], Normal@Keys[data[1]] ][xs, context];
ERTMonSetEventRecords[__][___] :=
    Block[{},
      Echo[
        "It is expected to have (i) one argument that is a dataset with named columns," <>
            " or (ii) two arguments the first being a matrix, the second a list of corresponding column names." <>
                "The column names are expected to include the names:" <>
            ToString["\"" <> # <> "\"" & /@ {"EntityID", "LocationID", "Variable", "Value", "ObservationTime"}],
        "ERTMonSetEventRecords:"
      ];
      $ERTMonFailure
    ];


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


ClearAll[ERTMonSetEntityAttributes]
ERTMonSetEntityAttributes[$ERTMonFailure] := $ERTMonFailure;
ERTMonSetEntityAttributes[][___] := $ERTMonFailure;
ERTMonSetEntityAttributes[xs_, context_Association] := $ERTMonFailure;
ERTMonSetEntityAttributes[data_?MatrixQ, colNames_?VectorQ][xs_, context_] :=
    If[ Dimensions[data][[2]] == Length[colNames] &&
        Length[Intersection[colNames, {"EntityID"}]] == 1,
      ERTMonUnit[ xs, Join[ context, <|"entityAttributes"->data, "entityAttributesColumnNames"->colNames|> ] ],
      (*ELSE*)
      ERTMonSetEntityAttributes[""][]
    ];
ERTMonSetEntityAttributes[data_Dataset][xs_, context_] :=
    ERTMonSetEntityAttributes[ Normal@data[All, Values], Normal@Keys[data[1]] ][xs, context];
ERTMonSetEntityAttributes[___][___] :=
    Block[{},
      Echo[
        "It is expected to have (i) one argument that is a dataset with named columns," <>
            " or (ii) two arguments the first being a matrix, the second a list of corresponding column names." <>
                "The column names are expected to include the name \"EntityID\".",
        "ERTMonSetEntityData:"
      ];
      $ERTMonFailure
    ];

ClearAll[ERTMonTakeEntityAttributes]
ERTMonTakeEntityAttributes[$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEntityAttributes[][$ERTMonFailure] := $ERTMonFailure;
ERTMonTakeEntityAttributes[xs_, context_] := ERTMonTakeEntityAttributes[][xs, context];
ERTMonTakeEntityAttributes[][xs_, context_] :=
    Block[{ds},
      ds = Dataset[context["entityAttributes"]];
      Dataset[ds[All, AssociationThread[context["entityAttributesColumnNames"], #] &]]
    ];
ERTMonTakeEntityAttributes[__][___] := $ERTMonFailure;


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
ERTMonSetVariableOutlierBoundaries[][___] := $ERTMonFailure;
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

Options[ERTMonReadData] = {"UseHeuristics"->False};

ERTMonReadData[$ERTMonFailure] := $ERTMonFailure;

ERTMonReadData[xs_, context_Association] := $ERTMonFailure

ERTMonReadData[dirName_String, opts:OptionsPattern[]][xs_, context_] :=
    Block[{},
      If[ TrueQ[ OptionValue[ERTMonReadData, "UseHeuristics"] ],
        ERTMonReadData[
          <| "eventRecords" -> FileNameJoin[{dirName, "eventRecords.csv"}],
             "entityAttributes" -> FileNameJoin[{dirName, "entityAttributes.csv"}],
             "dataIngestionSpecifications" -> FileNameJoin[{dirName, "dataIngestionSpecifications.csv"}]|> ][xs, context],
        (*ELSE*)
        ERTMonReadData[][xs,context]
      ]
    ];

ERTMonReadData[aFileNames_Association, opts:OptionsPattern[]][xs_, context_] :=
    Block[{compSpec,
      eventRecords, eventRecordsColumnNames,
      entityAttributes, entityAttributesColumnNames,
      entityID, locationID, expectedColNames,
      useHeuristicsQ
    },

      useHeuristicsQ = TrueQ[ OptionValue[ERTMonReadData, "UseHeuristics"] ];

      compSpec = ProcessComputationSpecification[ aFileNames["dataIngestionSpecifications"] ];

      eventRecords = Import[aFileNames["eventRecords"], "CSV"];
      If[ eventRecords === $Failed,
        Return[$ERTMonFailure]
      ];

      eventRecordsColumnNames = First[eventRecords];
      eventRecords = Rest[eventRecords];

      entityAttributes = Import[aFileNames["entityAttributes"], "CSV"];
      If[ entityAttributes === $Failed,
        Return[$ERTMonFailure]
      ];

      entityAttributesColumnNames = First[entityAttributes];
      entityAttributes = Rest[entityAttributes];

      If[ useHeuristicsQ,

        (* Determining the entity ID and location ID column names by heuristics. *)
        entityID = Flatten[StringCases[eventRecordsColumnNames, ("PatientID"|"ItemID"|"Entity"|"EntityID"), IgnoreCase->True]];
        locationID = Flatten[StringCases[eventRecordsColumnNames, ("LocationID"|"Location"), IgnoreCase->True]];

        If[Length[entityID]==0,
          Echo["Cannot find name of entity ID columns.", "ERTMontReadData:"];
          Return[$ERTMonFailure]
        ];

        eventRecordsColumnNames = StringReplace[ eventRecordsColumnNames, {entityID -> "EntityID", locationID -> "LocationID"}];
        entityAttributesColumnNames = StringReplace[ entityAttributesColumnNames, {entityID -> "EntityID", locationID -> "LocationID"}];
      ];

      expectedColNames = {"EntityID", "LocationID", "ObservationTime", "Variable", "Value"};
      If[ Length[Intersection[expectedColNames, eventRecordsColumnNames]] != Length[expectedColNames],
        Echo[
          "The event records data is expected to have the column names: " <>
              ToString[expectedColNames],
          "ERTMontReadData:"
        ];
        Return[$ERTMonFailure]
      ];

      expectedColNames = {"EntityID", "Attribute", "Value"};
      If[ Length[Intersection[expectedColNames, entityAttributesColumnNames]] != Length[expectedColNames],
        Echo[
          "The entity attributes data is expected to have the column names: " <>
              ToString[expectedColNames],
          "ERTMontReadData:"
        ];
        Return[$ERTMonFailure]
      ];

      ERTMonUnit[
        xs,
        Join[context,
          <| "eventRecords"->eventRecords, 
             "entityAttributes"->entityAttributes,
             "eventRecordsColumnNames"->eventRecordsColumnNames, 
             "entityAttributesColumnNames"->entityAttributesColumnNames,
             "compSpec"->compSpec|>] ]

    ];

ERTMonReadData[___][__] := $ERTMonFailure;


(**************************************************************)
(* ProcessComputationalSpecification                         *)
(**************************************************************)

ClearAll[ERTMonProcessComputationSpecification]

ERTMonProcessComputationSpecification[ arg:(_String|_?MatrixQ|_Dataset) ][xs_, context_Association] :=
    Block[{res},

      res = ProcessComputationSpecification[arg];

      If[ TrueQ[Head[res] === Dataset],
        ERTMonUnit[ res, context ],
        (*ELSE*)
        $ERTMonFailure
      ]
    ];

ERTMonProcessComputationSpecification[___][__] := $ERTMonFailure;


(**************************************************************)
(* Echo data summary                                          *)
(**************************************************************)

ClearAll[ERTMonEchoDataSummary];

ERTMonEchoDataSummary[$ERTMonFailure] := $ERTMonFailure;

ERTMonEchoDataSummary[xs_, context_] := ERTMonEchoDataSummary[][xs, context];

ERTMonEchoDataSummary[][xs_, context_] :=
    ERTMonEchoFunctionContext[
      "Data summary:",
      Association @
          MapThread[
            #1 -> RecordsSummary[#2,#3]&,
            { {"eventRecords", "entityAttributes"},
              Values @ KeyTake[#, {"eventRecords", "entityAttributes"}],
              Values @ KeyTake[#, {"eventRecordsColumnNames", "entityAttributesColumnNames"}]
            }]&][xs, context];

ERTMonEchoDataSummary[___][__] := $ERTMonFailure;


(**************************************************************)
(* Medical records entity-variable groups                     *)
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
          Query[GroupBy[{#["EntityID"], #["Variable"]} &], All, {"ObservationTime", "Value"}] @ ds;
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

ERTMonRecordGroupsToTimeSeries[zeroTimeArg:(_String|_?NumberQ|_DateObject):"MaxTime"][xs_, context_] :=
    Block[{ts, zeroTime=zeroTimeArg},

      Which[
        MemberQ[{"None", None}, zeroTime],
        zeroTime = "None",

        MemberQ[{"MinTime", "MinimumTime", "StartTime"}, zeroTime],
        zeroTime = "MinTime",

        MemberQ[{"MaxTime", "MaximumTime", "EndTime"}, zeroTime],
        zeroTime = "MaxTime",

        TrueQ[Head[zeroTime] === DateObject],
        zeroTime = AbsoluteTime[zeroTime],

        !NumberQ[zeroTime],
        Echo["The allowed values for the first argument are \"MinTime\", \"MaxTime\", a date object, or a number.", "ERTMonRecordGroupsToTimeSeries:"];
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
                Through[#["ObservationTime"]] -
                    Which[
                      zeroTime == "None",
                      0,

                      zeroTime == "MinTime",
                      Min[Through[#["ObservationTime"]]],

                      zeroTime == "MaxTime",
                      Max[Through[#["ObservationTime"]]],

                      True,
                      zeroTime
                    ],
                Through[#["Value"]]}
              ]
            ] &,
            context["entityVariableRecordGroups"]];

      ERTMonUnit[xs, Join[context, <| "timeSeries"->ts |>]]
    ];

ERTMonRecordGroupsToTimeSeries[___][__] :=
    Block[{},
      Echo["One or no arguments are expected. The allowed values for the first argument are \"MinTime\", \"MaxTime\", a date object, or a number.", "ERTMonRecordGroupsToTimeSeries:"];
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

        NumberQ[specRow["AggregationTimeInterval"]] && NumberQ[specRow["MaxHistoryLength"]],

        ts = KeySelect[timeSeries, MatchQ[#, {_, specRow["Variable"]}] &];

        ts = Map[ TimeSeriesWindow[#, If[ #["FirstTime"] >= 0, {#["FirstTime"], #["FirstTime"] + specRow["MaxHistoryLength"]}, {-specRow["MaxHistoryLength"], 0}] ]&, ts];

        ts = Map[ TimeSeriesAggregate[#, specRow["AggregationTimeInterval"], aAggregationFunctionSpec[specRow["AggregationFunction"]] ]&, ts];

        ts = KeyMap[ {#[[1]], StringJoin[specRow["Variable"], ".", specRow["AggregationFunction"]]}&, ts];
        ts,

        True,
        <||>
      ]
    ];

ClearAll[ERTMonAggregateTimeSeries]

ERTMonAggregateTimeSeries[$ERTMonFailure] := $ERTMonFailure;

ERTMonAggregateTimeSeries[xs_, context_Association] := ERTMonAggregateTimeSeries[][xs, context];

ERTMonAggregateTimeSeries[][xs_, context_] :=
    Block[{compSpec, ts, aAggrFuncs},

      If[ !KeyExistsQ[context, "compSpec"],
        Echo["Cannot find computations specifications.", "ERTMonTimeSeriesAggregation:"];
        Return[$ERTMonFailure]
      ];

      If[ !KeyExistsQ[context, "timeSeries"],
        Echo["Calculate time series first. (With ERTMonRecordGroupsToTimeSeries.)", "ERTMonTimeSeriesAggregation:"];
        Return[$ERTMonFailure]
      ];

      compSpec = context["compSpec"];
      ts = context["timeSeries"];

      If[ Length[Intersection[{"OutliersCount", "OutliersFraction"}, Normal[compSpec[Values,"AggregationFunction"]]]] > 0 &&
          !KeyExistsQ[context, "variableOutlierBoundaries"],

        Echo["Calculate outlier boundaries first.", "ERTMonTimeSeriesAggregation:"];
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

ERTMonAggregateTimeSeries[___][__] :=
    Block[{},
      Echo["No arguments are expected.", "ERTMonTimeSeriesAggregation:"];
      $ERTMonFailure
    ];


(**************************************************************)
(* Find variable distributions                                *)
(**************************************************************)

ClearAll[ERTMonFindVariableDistributions]

ERTMonFindVariableDistributions[$ERTMonFailure] := $ERTMonFailure;

ERTMonFindVariableDistributions[xs_, context_Association] := ERTMonFindVariableDistributions[Histogram[#, PlotRange -> All, ImageSize -> Medium]&][xs, context];

ERTMonFindVariableDistributions[][xs_, context_] := ERTMonFindVariableDistributions[Histogram[#, PlotRange -> All, ImageSize -> Medium]&][xs, context];

ERTMonFindVariableDistributions[distFunc_][xs_, context_] :=
    Block[{ivRowSpecIDs, distributions},

      ivRowSpecIDs = Union[Keys[context["entityVariableRecordGroups"]][[All, 2]]];

      distributions =
          Association @
              Map[ # -> distFunc[Map[#["Value"] &, Flatten[Values[KeySelect[context["entityVariableRecordGroups"], MatchQ[{_, #}]]]]]]&, ivRowSpecIDs];

      ERTMonUnit[distributions, Join[context, <| "variableDistributions"->distributions |>]]
    ];

ERTMonFindVariableDistributions[___][__] :=
    Block[{},
      Echo[
        StringRiffle[
          {"One or no arguments are expected. The argument is a function that finds the distribution of a list of numbers.",
            "(Here are such built-in function names : Histogram (default), EmpiricalDistribution, SmoothKernelDistribution, etc.)"
          }," "],
        "ERTMonFindVariableDistributions:"
      ];
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

      ERTMonUnit[outlierBoundaries, Join[context, <| "variableOutlierBoundaries"->outlierBoundaries |>]]
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

ERTMonAggregateTimeSeries[___][__] := $ERTMonFailure;


End[]; (* `Private` *)

EndPackage[]