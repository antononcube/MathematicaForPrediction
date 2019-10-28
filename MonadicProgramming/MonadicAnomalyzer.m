(*
    Monadic Anomalyzer Mathematica package
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

(* :Title: MonadicAnomalyzer *)
(* :Context: MonadicAnomalyzer` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-10-25 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: Quantile Regression, Anomaly, Outlier, Outlier detection, Unit tests *)
(* :Discussion: *)


(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MonadicQuantileRegression`QRMonUnit]] == 0,
  Echo["MonadicQuantileRegression.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicAnomalyzer`"];

QRMonAnomalyze::usage = "QRMonAnomalyze[opts] adds or places anomalies into the data of the monad.";

QRMonPlaceOutliers::usage = "QRMonPlaceOutliers[n, opts] adds or places n outliers into the data of the monad. \
QRMonPlaceOutliers[ {nb, nt}, opts] adds or places nb bottom outliers and nt top outliers into the data of the monad.";

QRMonComponentsPartition::usage = "QRMonComponentsPartition[n, opts] partitions the data of the monad into n components.";

Begin["`Private`"];

Needs["MonadicQuantileRegression`"];

(**************************************************************)
(* Anomalyze                                                  *)
(**************************************************************)

Clear[QRMonAnomalyze];

SyntaxInformation[QRMonAnomalyze] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[QRMonAnomalyze] = { "NumberOfOutliers" -> Automatic, "NumberOfComponents" -> Automatic };

QRMonAnomalyze[$QRMonFailure] := $QRMonFailure;

QRMonAnomalyze[xs_, context_Association] := QRMonAnomalyze[][xs, context];

QRMonAnomalyze[][xs_, context_Association] := QRMonAnomalyze[ Automatic, Automatic ][xs, context];

QRMonAnomalyze[ opts : OptionsPattern[] ][xs_, context_Association] := QRMonAnomalyze[ Automatic, Automatic, opts ][xs, context];

QRMonAnomalyze[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{numberOfOutliers, numberOfComponents},

      numberOfOutliers = OptionValue[ QRMonAnomalyze, "NumberOfOutliers" ];
      numberOfComponents = OptionValue[ QRMonAnomalyze, "NumberOfComponents" ];

      If[ !( IntegerQ[numberOfOutliers] && numberOfOutliers > 0 || TrueQ[ numberOfComponents === Automatic ] ),
        Echo["The value of the option \"NumberOfOutliers\" is expected to be a positive integer.", "QRMonAnomalyze:"];
        Return[$QRMonFailure];
      ];

      If[ !( IntegerQ[numberOfComponents] && numberOfComponents > 0 || TrueQ[ numberOfComponents === Automatic ] ),
        Echo["The value of the option \"NumberOfComponents\" is expected to be a positive integer.", "QRMonAnomalyze:"];
        Return[$QRMonFailure];
      ];


    ];

QRMonAnomalyze[___][xs_, context_Association] :=
    Block[{},
      Echo["No arguments are expected;" <>
          "the expected option",
        "QRMonAnomalyze:"
      ];
      $QRMonFailure
    ];


(**************************************************************)
(* PlaceOutliers                                              *)
(**************************************************************)

Clear[QRMonPlaceOutliers];

SyntaxInformation[QRMonPlaceOutliers] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[QRMonPlaceOutliers] =
    {
      "NumberOfBottomOutliers" -> Automatic, "NumberOfTopOutliers" -> Automatic,
      "Factor" -> Automatic, "Offset" -> Automatic
    };

QRMonPlaceOutliers[$QRMonFailure] := $QRMonFailure;

QRMonPlaceOutliers[xs_, context_Association] := QRMonPlaceOutliers[][xs, context];

QRMonPlaceOutliers[][xs_, context_Association] := QRMonPlaceOutliers[ {5, 5}, Options[QRMonPlaceOutliers] ][xs, context];

QRMonPlaceOutliers[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ nPointsBottom, nPointsTop},

      nPointsBottom = OptionValue[ QRMonPlaceOutliers, "NumberOfBottomOutliers" ];
      nPointsTop = OptionValue[ QRMonPlaceOutliers, "NumberOfBottomTop" ];

      If[ !( IntegerQ[nPointsBottom] && nPointsBottom >= 0 ),
        Echo["The value of the option \"NumberOfBottomOutliers\" is expected to be a non-negative integer.", "QRMonPlaceOutliers:"];
        Return[$QRMonFailure];
      ];

      If[ !( IntegerQ[nPointsTop] && nPointsTop >= 0 ),
        Echo["The value of the option \"NumberOfBottomTop\" is expected to be a non-negative integer.", "QRMonPlaceOutliers:"];
        Return[$QRMonFailure];
      ];

      QRMonPlaceOutliers[ {nPointsBottom, nPointsTop}, opts ][xs, context]
    ];

QRMonPlaceOutliers[ n_?IntegerQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    QRMonPlaceOutliers[ {n - Floor[n/2], Floor[n/2]}, opts][xs, context];

QRMonPlaceOutliers[ {nBottom_?IntegerQ, nTop_?IntegerQ}, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{factor, offset, res, pointsBottom, pointsTop, pointsBottom2, pointsTop2, data, qFunc},

      factor = OptionValue[ QRMonPlaceOutliers, "Factor" ];
      offset = OptionValue[ QRMonPlaceOutliers, "Offset" ];

      If[ !( NumericQ[factor] && factor > 0 || TrueQ[factor == Automatic] ),
        Echo["The value of the option \"Factor\" is expected to be a positive real or Automatic.", "QRMonPlaceOutliers:"];
        Return[$QRMonFailure];
      ];

      If[ !( NumericQ[offset] && offset >= 0 || TrueQ[offset == Automatic]),
        Echo["The value of the option \"Offset\" is expected to be a non-negative real or Automatic.", "QRMonPlaceOutliers:"];
        Return[$QRMonFailure];
      ];

      data = Fold[ QRMonBind, QRMonUnit[xs, context], { QRMonGetData, QRMonTakeValue }];
      If[ TrueQ[data === $QRMonFailure], Return[$QRMonFailure] ];

      res = Fold[ QRMonBind, QRMonUnit[xs, context], { QRMonOutliers, QRMonTakeValue }];

      pointsBottom = RandomSample[ res["bottomOutliers"], UpTo[nBottom] ];
      pointsTop = RandomSample[ res["topOutliers"], UpTo[nTop] ];

      If[ TrueQ[factor === Automatic], factor = 1. ];

      Which[

        KeyExistsQ[ context["regressionFunctions"], 0.5 ],
        If[ TrueQ[offset === Automatic], offset = 0. ];
        qFunc = context["regressionFunctions"][0.5],

        KeyExistsQ[ context["regressionFunctions"], "mean" ],
        If[ TrueQ[offset === Automatic], offset = 0. ];
        qFunc = context["regressionFunctions"]["mean"],

        True,
        If[ TrueQ[offset === Automatic], offset = Abs[ Subtract @@ MinMax[ data[[All,2]] ] ] ];
        qFunc = 0&
      ];

      pointsBottom2 = Map[ { #[[1]], factor * (#[[2]] - qFunc[ #[[1]] ]) + qFunc[ #[[1]] ] - offset }&, pointsBottom];
      pointsTop2 = Map[ { #[[1]], factor * (#[[2]] - qFunc[ #[[1]] ]) + qFunc[ #[[1]] ] + offset }&, pointsTop];

      data = data /. Join[ Thread[ pointsBottom -> pointsBottom2 ], Thread[ pointsTop -> pointsTop2 ] ];

      QRMonUnit[ <| "bottomOutliers" -> pointsBottom2, "topOutliers" -> pointsTop2 |>, Join[ context, <| "data" -> data |> ] ]

    ] /; nBottom >= 0 && nTop >= 0;

QRMonPlaceOutliers[___][xs_, context_Association] :=
    Block[{},
      Echo[
        "QRMonPlaceOutliers[n, opts] adds or places n outliers into the data of the monad. "<>
            "QRMonPlaceOutliers[ {nb, nt}, opts] adds or places nb bottom outliers and nt top outliers into the data of the monad.",
        "QRMonPlaceOutliers:"
      ];
      $QRMonFailure
    ];


(**************************************************************)
(* Components partition                                       *)
(**************************************************************)

Clear[QRMonComponentsPartition];

SyntaxInformation[QRMonComponentsPartition] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[QRMonComponentsPartition] =
    {
      "NumberOfComponents" -> Automatic,
      "FactorRange" -> Automatic, "OffsetRange" -> Automatic, "AlternateSigns" -> True
    };

QRMonComponentsPartition[$QRMonFailure] := $QRMonFailure;

QRMonComponentsPartition[xs_, context_Association] := QRMonComponentsPartition[][xs, context];

QRMonComponentsPartition[][xs_, context_Association] := QRMonComponentsPartition[ 5, Options[QRMonPlaceOutliers] ][xs, context];

QRMonComponentsPartition[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ nComponents },

      nComponents = OptionValue[ QRMonComponentsPartition, "NumberOfComponents" ];

      If[ !( IntegerQ[nComponents] && nComponents > 0 || TrueQ[ nComponents === Automatic ] ),
        Echo["The value of the option \"nComponents\" is expected to be a positive integer or Automatic.", "QRMonComponentsPartition:"];
        Return[$QRMonFailure];
      ];

      If[ TrueQ[nComponents === Automatic], nComponents = 5 ];

      QRMonComponentsPartition[ nComponents, opts ][xs, context]
    ];

QRMonComponentsPartition[ nComponents_?IntegerQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{factorRange, offsetRange, alternateSigns, data, factors, offsets},

      factorRange = OptionValue[ QRMonComponentsPartition, "FactorRange" ];
      offsetRange = OptionValue[ QRMonComponentsPartition, "OffsetRange" ];
      alternateSigns = TrueQ[ OptionValue[ QRMonComponentsPartition, "AlternateSigns" ] ];

      If[ !( VectorQ[factorRange, NumericQ] || TrueQ[factorRange == Automatic] || TrueQ[factorRange == None] ),
        Echo["The value of the option \"FactorRange\" is expected to be a pair of numbers, Automatic.", "QRMonComponentsPartition:"];
        Return[$QRMonFailure];
      ];

      If[ !( VectorQ[factorRange, NumericQ] || TrueQ[offsetRange == Automatic]),
        Echo["The value of the option \"OffsetRange\" is expected to be a pair of numbers, Automatic.", "QRMonComponentsPartition:"];
        Return[$QRMonFailure];
      ];

      data = Fold[ QRMonBind, QRMonUnit[xs, context], { QRMonGetData, QRMonTakeValue }];
      If[ TrueQ[data === $QRMonFailure], Return[$QRMonFailure] ];

      If[ TrueQ[factorRange === Automatic], factorRange = {1, 1,5} ];
      If[ TrueQ[offsetRange === Automatic], offsetRange = {1, 3} * Mean[data[[All,2]]] ];

      If[ alternateSigns,
        factors = Table[ (-1)^i, {i,nComponents}] * RandomReal[factorRange, nComponents];
        offsets = Table[ (-1)^i, {i,nComponents}] * RandomReal[offsetRange, nComponents],
        (* ELSE *)
        factors = RandomReal[factorRange, nComponents];
        offsets = RandomReal[offsetRange, nComponents]
      ];

      QRMonComponentsPartition[ Transpose[ {factors, offsets}], opts ][xs, context]
    ] /; nComponents > 0;


QRMonComponentsPartition[ factorOffsetPairs : { {_?NumericQ, _?NumericQ} .. }, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{comps, data, nComponents = Length[factorOffsetPairs]},

      data = Fold[ QRMonBind, QRMonUnit[xs, context], { QRMonGetData, QRMonTakeValue }];
      If[ TrueQ[data === $QRMonFailure], Return[$QRMonFailure] ];

      comps = Partition[ SortBy[data, #[[1]]& ], Floor[ Length[data] / nComponents ] ];

      comps =
          MapThread[
            Transpose @ { #1[[All, 1]], Rescale[ #1[[All, 2]], MinMax[#1[[All, 2]]], #2 * MinMax[#1[[All, 2]]] ] + #3 }&,
            { comps, factorOffsetPairs[[All,1]],  factorOffsetPairs[[All,2]] }
          ];

      data = Join @@ comps;

      QRMonUnit[ comps, Join[ context, <| "data" -> data |> ] ]

    ];

QRMonComponentsPartition[___][xs_, context_Association] :=
    Block[{},
      Echo[
        "QRMonComponentsPartition[k, opts] partitions the data of the monad into k components." <>
        "QRMonComponentsPartition[ { {f1, off1} ... {fK, offK}}, opts] partitions the data of the monad into K components \
         using the specified factors and offsets.",
        "QRMonComponentsPartition:"
      ];
      $QRMonFailure
    ];


(**************************************************************)
(* Change conditional variance                                *)
(**************************************************************)

Clear[QRMonChangeConditionalVariance];

SyntaxInformation[QRMonChangeConditionalVariance] = { "ArgumentsPattern" -> { _., _., OptionsPattern[] } };

Options[QRMonChangeConditionalVariance] =
    {
      "Points" -> Automatic, "Radius" -> Automatic,
      "Factor" -> Automatic, "AggregateFunction" -> Median
    };

QRMonChangeConditionalVariance[$QRMonFailure] := $QRMonFailure;

QRMonChangeConditionalVariance[xs_, context_Association] := QRMonChangeConditionalVariance[][xs, context];

QRMonChangeConditionalVariance[][xs_, context_Association] := QRMonChangeConditionalVariance[ Options[QRMonPlaceOutliers] ][xs, context];

QRMonChangeConditionalVariance[ opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{ nComponents },

      nComponents = OptionValue[ QRMonComponentsPartition, "NumberOfComponents" ];

      If[ !( IntegerQ[nComponents] && nComponents > 0 ),
        Echo["The value of the option \"nComponents\" is expected to be a positive integer.", "QRMonComponentsPartition:"];
        Return[$QRMonFailure];
      ];

      QRMonChangeConditionalVariance[ nComponents, opts ][xs, context]
    ];

QRMonChangeConditionalVariance[ points_?VectorQ, radius_?NumberQ, opts : OptionsPattern[] ][xs_, context_Association] :=
    Block[{},

      QRMonUnit[ None, Join[ context, <| "data" -> data |> ] ]

    ] /; nComponents > 0;

QRMonChangeConditionalVariance[___][xs_, context_Association] :=
    Block[{},
      Echo[
        "QRMonChangeConditionalVariance[points, radius, opts] changes the variance of the data the specified points and using the specified radius.",
        "QRMonChangeConditionalVariance:"
      ];
      $QRMonFailure
    ];



End[]; (* `Private` *)

EndPackage[]