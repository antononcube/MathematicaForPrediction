(*
    Monadic Anomalies Finder Mathematica package
    Copyright (C) 2019  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: MonadicAnomaliesFinder *)
(* :Context: MonadicAnomaliesFinder` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-09-13 *)
(* Created with the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ . *)

(* :Package Version: 0.5 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: Quantile Regression, Anomalies, Outliers *)
(* :Discussion:

   # In brief

   This package provides functions to for the Quantile Regression Monad package (QRMon):

     https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m .

   Here is an example usage:

      ts = FinancialData[Entity["Financial", "^SPX"], {{2015, 1, 1}, Date[]}];

      qrObj =
        QRMonUnit[ts]⟹
          QRMonEchoDataSummary⟹
          QRMonQuantileRegression[100, 0.5]⟹
          QRMonDateListPlot⟹
          QRMonFindAnomaliesByResiduals["RelativeErrors" -> False, "OutlierIdentifier" -> SPLUSQuartileIdentifierParameters]⟹
          QRMonEchoFunctionValue[RecordsSummary];

      DateListPlot[{ts, qrObj⟹QRMonTakeValue}, Joined -> {True, False}, PlotStyle -> {Gray, {Red, PointSize[0.01]}}]

   # Future plans

   It would be nice the data and anomalies plot to be part of the monadic pipeline.

   Anton Antonov
   Windermere, Florida, USA
   2019-09-13

*)


(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MonadicQuantileRegression`QRMonUnit]] == 0,
  Echo["MonadicQuantileRegression.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]
];

If[Length[DownValues[MonadicSparseMatrixRecommender`SMRMonUnit]] == 0,
  Echo["MonadicSparseMatrixRecommender.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicSparseMatrixRecommender.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)


BeginPackage["MonadicAnomaliesFinder`"];

QRMonFindAnomaliesByResiduals::usage = "QRMonFindAnomaliesByResiduals[ opts:OptionsPattern[] ] \
finds structural breaks in the data using the Chow Test. \
It takes as options the options of QRMonQuantileRegression, QRMonFindLocalExtrema, and QRMonPlot.";

SMRMonFindAnomalies::usage = "SMRMonFindAnomalies[ opts:OptionsPattern[] ] \
finds rows of the recommendation matrix that are anomalies.";

Begin["`Private`"];

Needs["OutlierIdentifiers`"];
Needs["MonadicQuantileRegression`"];
Needs["MonadicSparseMatrixRecommender`"];
Needs["SSparseMatrix`"];


(**************************************************************)
(* Find point anomalies using approximation residuals         *)
(**************************************************************)

Clear[QRMonFindAnomaliesByResiduals];

SyntaxInformation[QRMonFindAnomaliesByResiduals] = { "ArgumentsPattern" -> { OptionsPattern[] } };

Options[QRMonFindAnomaliesByResiduals] = { "Threshold" -> None, "OutlierIdentifier" -> HampelIdentifierParameters, "RelativeErrors" -> False };

QRMonFindAnomaliesByResiduals[$QRMonFailure] := $QRMonFailure;

QRMonFindAnomaliesByResiduals[xs_, context_Association] := QRMonFindAnomaliesByResiduals[ Options[QRMonFindAnomaliesByResiduals] ][xs, context];

QRMonFindAnomaliesByResiduals[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ threshold, relativeErrorsQ, outlierFunc, errs, outPos, outliers },

      threshold = OptionValue[ QRMonFindAnomaliesByResiduals, "Threshold" ];
      relativeErrorsQ = TrueQ[ OptionValue[ QRMonFindAnomaliesByResiduals, "RelativeErrors" ] ];
      outlierFunc = OptionValue[ QRMonFindAnomaliesByResiduals, "OutlierIdentifier" ];

      Which[
        NumberQ[threshold],
        outliers =
            Fold[ QRMonBind,
              QRMonUnit[xs, context],
              {
                QRMonErrors[ "RelativeErrors" -> relativeErrorsQ ],
                QRMonPickPathPoints[ threshold, "PickAboveThreshold" -> True ],
                QRMonTakeValue
              }];
        outliers = outliers[[1]],

        TrueQ[ outlierFunc =!= None],
        errs =
            Fold[ QRMonBind, QRMonUnit[xs, context], { QRMonErrors[ "RelativeErrors" -> relativeErrorsQ], QRMonTakeValue }];

        outPos = OutlierPosition[ Abs[errs[[1, All, 2]]], TopOutliers @* outlierFunc ];

        outliers = QRMonBind[ QRMonUnit[xs, context], QRMonTakeData];
        outliers = outliers[[outPos]],

        True,
        Return[ QRMonFindAnomaliesByResiduals[ "not good" ][xs, context] ]
      ];

      QRMonUnit[ outliers, context ]
    ];

QRMonFindAnomaliesByResiduals[___][xs_, context_Association] :=
    Block[{},
      Echo[
        "The expected signature is QRMonFindAnomaliesByResiduals[ opts:OptionsPattern[] ]." <>
            " The option \"Threshold\" is expected to be None or a number (an errors threshold.)." <>
            " The option \"OutlierIdentifier\" is expected to be None" <>
            " or a function that give a list of lower and upper thresholds when applied to a list of numbers (errors absolute values.)",
        "QRMonFindAnomaliesByResiduals:"
      ];
      $QRMonFailure
    ];


(**************************************************************)
(* Find anomalies by nearest neighbors                        *)
(**************************************************************)

Clear[SMRMonFindAnomalies];

SyntaxInformation[SMRMonFindAnomalies] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[SMRMonFindAnomalies] = {
  "NumberOfNearestNeighbors" -> 10,
  "OutlierIdentifier" -> (BottomOutliers @* SPLUSQuartileIdentifierParameters),
  "AggregationFunction" -> Mean,
  "Property" -> "SSparseMatrix"
};

SMRMonFindAnomalies[$SMRMonFailure] := $SMRMonFailure;

SMRMonFindAnomalies[xs_, context_Association] := SMRMonFindAnomalies[][xs, context];

SMRMonFindAnomalies[ opts : OptionsPattern[] ][xs_, context_Association] :=
    SMRMonFindAnomalies[ Automatic, opts ][xs, context];

SMRMonFindAnomalies[ arg_, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ nns, outFunc, aggrFunc, newContext, prop, smat, recs, outlierInds, outlierThresholds, knownProperties },

      (* Preliminary assignments. *)

      knownProperties = { "Distances", "SSparseMatrix", "RowNames", "OutlierThresholds", "Properties" };

      nns = OptionValue[ SMRMonFindAnomalies, "NumberOfNearestNeighbors" ];
      outFunc = OptionValue[ SMRMonFindAnomalies, "OutlierIdentifier" ];
      aggrFunc = OptionValue[ SMRMonFindAnomalies, "AggregationFunction" ];
      prop = OptionValue[ SMRMonFindAnomalies, "Property" ];

      If[ ! (IntegerQ[nns] && nns > 0),
        Echo[
          "The value of the options \"NumberOfNearestNeighbors\" is expected to be a positive integer.",
          "SMRMonFindAnomalies:"
        ];
        Return[$SMRMonFailure]
      ];

      If[ !KeyExistsQ[context, "M"],
        Echo["Cannot find a recommendation matrix.", "SMRMonFindAnomalies:"];
        Return[$SMRMonFailure]
      ];

      (* If arg is a standard matrix make it an SSparseMatrix. *)
      smat = arg;
      If[ MatrixQ[arg, NumberQ],
        smat = ToSSparseMatrix[ SparseArray[arg], "RowNames" -> Map[ToString, Range[Length[arg]]] ]
      ];

      (* Compute the Cosine/Dot distances to the rows of monad's matrix. *)
      (* If arg/smat is Automatic use monad's matrix. *)
      Which[
        smat === Automatic,
        recs =
            Association[
              Map[
                # -> Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonRecommendByHistory[#, nns], SMRMonTakeValue} ]&,
                RowNames[context["M"]]
              ]
            ],

        SSparseMatrixQ[smat] && ColumnsCount[smat] == ColumnsCount[context["M"]],
        nns = Lookup[ context, "numberOfNNs" ];
        recs =
            Association[
              Map[
                # -> Fold[ SMRMonBind, SMRMonUnit[xs, context], { SMRMonRecommendByProfile[ SparseArray[smat[[#,All]]], nns], SMRMonTakeValue} ]&,
                RowNames[smat]
              ]
            ],

        True,
        Echo[
          "The first argument is expected to be Automatic, a matrix, or SSparseMatrix object.",
          "SMRMonFindAnomalies:"
        ];
        Return[$SMRMonFailure]
      ];

      (* If smat is not Automatic and the outliers thresholds are already in the context
         then this means with have "trained" already. *)
      If[ SSparseMatrixQ[smat] && KeyExistsQ[context, "outlierThresholds"],
        aggrFunc = context["aggregationFunction"]
      ];

      (* Aggregate *)
      recs = Map[ aggrFunc@*Values, recs ];
      (* Probably not a good hack, but for now produces expected results. *)
      (* Takes care of Mean[{}] and similar cases. *)
      recs = If[ !NumericQ[#], 0., #]& /@ recs;

      (* If smat is not Automatic and the outliers thresholds are already in the context then use them. *)
      (* If smat is Automatic then (re-)compute the outlier thresholds. *)
      If[ SSparseMatrixQ[smat] && KeyExistsQ[context, "outlierThresholds"],

        aggrFunc = context["aggregationFunction"];
        outlierThresholds = context["outlierThresholds"];
        With[ {th = outlierThresholds},
          outlierInds = OutlierPosition[ Values[recs], th& ]
        ],
        (* ELSE *)

        outlierThresholds = outFunc[ Values[recs] ];
        outlierInds = OutlierPosition[ Values[recs], outFunc ]
      ];

      (* The updated context. *)
      newContext = Join[ context, <| "outlierThresholds" -> outlierThresholds, "aggregationFunction" -> aggrFunc, "numberOfNNs" -> nns |>];

      (* Return result according to the "Property" option. *)
      Which[
        Length[outlierInds] == 0,
        SMRMonUnit[{}, newContext],

        MemberQ[ ToLowerCase[{ "Distances", "AggregatedDistances" }], ToLowerCase[prop] ],
        SMRMonUnit[ recs, newContext ],

        MemberQ[ ToLowerCase[{ "RowNames", "Indices" }], ToLowerCase[prop] ],
        SMRMonUnit[ outlierInds, newContext ],

        MemberQ[ ToLowerCase[{ "Thresholds", "OutlierThresholds" }], ToLowerCase[prop] ],
        SMRMonUnit[ outlierThresholds, newContext ],

        ToLowerCase["Properties"] == ToLowerCase[prop],
        Echo[ knownProperties, "SMRMonFindAnomalies:"];
        SMRMonUnit[ knownProperties, newContext ],

        ToLowerCase["SSparseMatrix"] == ToLowerCase[prop],
        SMRMonUnit[ context["M"][[ outlierInds, All ]], newContext ],

        True,
        Echo[ "Unknown property. The property should be one of : " <> ToString[knownProperties] <> ".", "SMRMonFindAnomalies:"];
        Return[$SMRMonFailure]
      ]
    ];

SMRMonFindAnomalies[___][xs_, context_Association] :=
    Block[{},
      Echo[
        "The expected signature is SMRMonFindAnomalies[ opts:OptionsPattern[] ].",
        "SMRMonFindAnomalies:"
      ];
      $SMRMonFailure
    ];

End[]; (* `Private` *)

EndPackage[]