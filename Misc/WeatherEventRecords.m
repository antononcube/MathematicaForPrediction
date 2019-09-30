(*
    Weather event records data Mathematica package
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

(* :Title: WeatherEventRecords *)
(* :Context: WeatherEventRecords` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-09-20 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: weather data, long form, database, star schema, even data, time series *)
(* :Discussion:


    # In brief

    This package provides a function that extracts weather data using specifications for:
    cities, date range, weather data variables, and number of stations.

    The main inspiration to make this package is need for example data in order to document
    the ERTMon package [1]; see [2].


    # Usage example

      citiesSpec = {{"Miami", "USA"}, {"Chicago", "USA"}, {"London", "UK"}, {"Melbourne", "Australia"}};
      wProps = {"Temperature", "Pressure", "Humidity", "WindSpeed"};

      res = WeatherEventRecords[citiesSpec, {{2018, 5, 1}, {2018, 8, 31}}, wProps, 2]

      RecordsSummary[res]

      DateListPlot[res["eventRecords"][Select[#EntityID == "KMFL" && #Variable == "Pressure" &], {"ObservationTime", "Value"}]]


    # References

    [1] Anton Antonov, Monadic Event Records Transformations Mathematica package, (2018), MathematicaForPrediction at GitHub.
        URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicEventRecordsTransformations.m .

    [2] Anton Antonov, Parametrized event records data transformations, (2018), MathematicaForPrediction at GitHub.
        URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Parametrized-event-records-data-transformations.md


    Anton Antonov
    2018-09-20
    Windermere, FL, USA

*)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MathematicaForPredictionUtilities`RecordsSummary]] == 0,
  Echo["MathematicaForPredictionUtilities.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"]
];

If[Length[DownValues[DataReshape`ToLongForm]] == 0,
  Echo["DataReshape.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/DataReshape.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["WeatherEventRecords`"];
(* Exported symbols added here with SymbolName::usage *)

WeatherEventRecords::usage = "\
WeatherEventRecords[ \
  citiesSpec_: {{_String, _String}..}, \
  dateRange:{{_Integer, _Integer, _Integer}, {_Integer, _Integer, _Integer}}, \
  wProps:{_String..} : {\"Temperature\"}, \
  nStations_Integer : 1 ] \
gives an association with event records data."

Begin["`Private`"];

Needs["DataReshape`"];

Clear[WeatherEventRecords];

WeatherEventRecords[
  citiesSpec_: {{_String, _String}..},
  dateRange:{{_Integer, _Integer, _Integer}, {_Integer, _Integer, _Integer}},
  wProps:{_String..} : {"Temperature"},
  nStations_Integer : 1 ] :=

    Block[{wStations, aWStations, tsData, XXX, eventRecords, entityAttributes},

      wStations = WeatherData[{#, nStations}] & /@ citiesSpec;
      wStations = Map[#[[2]] &, wStations, {2}];

      aWStations =
          Join @@ MapThread[
            AssociationThread[#1, XXX] /. XXX -> #2 &, {wStations, citiesSpec}];

      tsData =
          Association@
              Flatten@Outer[{#1, #2} ->
                  WeatherData[#1, #2, {dateRange[[1]], dateRange[[2]], "Day"}] &,
                Keys[aWStations], wProps, 1];

      If[ !AssociationQ[tsData], Return[$Failed]];

      tsData = Select[tsData, MatchQ[#, _TemporalData] &];

      If[ Length[tsData] == 0, Return[$Failed]];

      eventRecords =
          Dataset[ Flatten[#, 1] &@ KeyValueMap[Thread[List[Sequence @@ #1, #2["Times"], #2["Values"]]] &, tsData] ];

      eventRecords =
          eventRecords[All, AssociationThread[{"EntityID", "Variable", "ObservationTime", "Value"} -> #] &];

      eventRecords =
          eventRecords[All, Join[#, <|"LocationID" -> aWStations[#EntityID][[1]]|>] &];

      eventRecords =
          eventRecords[All, {"EntityID", "LocationID", "ObservationTime", "Variable", "Value"}];

      eventRecords = DeleteMissing[eventRecords, 1, 2];
      eventRecords = eventRecords[Select[NumberQ[#Value] &]];

      entityAttributes =
          Dataset[KeyValueMap[Flatten[{#1, #2[[1]], #2[[-1]]}] &, aWStations]][All, AssociationThread[{"Station", "City", "Country"} -> #] &];

      entityAttributes =
          ToLongForm[entityAttributes, "Station", {"City", "Country"}];

      entityAttributes =
          entityAttributes[All, Association[{"EntityID" -> #Station, "Attribute" -> #Variable, "Value" -> #Value}] &];

      <| "eventRecords"->eventRecords, "entityAttributes"->entityAttributes |>
    ];

End[]; (* `Private` *)

EndPackage[]