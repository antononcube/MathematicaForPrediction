(*
    Symbolic Regression by QRMon Mathematica package
    Copyright (C) 2020 Anton Antonov

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
    Mathematica is (C) Copyright 1988-2020 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: SymbolicRegressionByQRMon *)
(* :Context: SymbolicRegressionByQRMon` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-11-27 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: Symbolic regression, Quantile regression, Function basis, Random search *)
(* :Discussion:

TODO:
   1. [ ] Implement the use of standard function bases. (B-splines, Chebyshev, Sin/Cos.)
   2. [ ] Implement options to determine target (basis) functions.
   3. [ ] Improve the random search algorithm.
   4. [ ] Option-specified parallel computations.
   5. [ ] Implement random fits over segments derived by found structural breaks.
   6. [ ] Unit tests.
*)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MonadicQuantileRegression`QRMonUnit]] == 0,
  Echo["MonadicQuantileRegression.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]
];

If[Length[DownValues[MonadicStructuralBreaksFinder`QRMonFindStructuralBreaks]] == 0,
  Echo["MonadicStructuralBreaksFinder.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicStructuralBreaksFinder.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["SymbolicRegressionByQRMon`"];
(* Exported symbols added here with SymbolName::usage *)

GenerateBasisFunctions::usage = "GenerateBasisFunctions[x_Symbol, k_Integer] \
generates basis function for symbol x with k functions per function type.";

RandomFunctionBasis::usage = "RandomFunctionBasis[x_Symbol, n_Integer, k_Integer] \
selects (at most) n random functions from a function basis generated with GenerateBasisFunctions[x,k].";

FindFormulaByQRMon::usage = "FindFormulaByQRMon[data_, x_Symbol, n_Integer] \
finds n formulas for data using basis functions that have x as argument.";

PlotDataAndFit::usage = "PlotDataAndFit[data, x_Symbol, fitRes_Association, ___];";

Begin["`Private`"];

Needs["MonadicQuantileRegression`"];

(**************************************************************)
(* GenerateBasisFunctions                                     *)
(**************************************************************)

Clear[GenerateBasisFunctions];

Options[GenerateBasisFunctions] = {"FunctionRatios" -> False};

GenerateBasisFunctions[x_Symbol, k_Integer, opts : OptionsPattern[]] :=
    Block[{lsBases},
      lsBases = {
        Table[Sin[x * i], {i, 0, k}],
        Table[Log[x + i], {i, 1, k}],
        Table[1 / Sin[x + i], {i, 1, k}],
        Table[Sqrt[x + i], {i, 0, k}],
        Table[1 / Sqrt[x + i], {i, 1, k}],
        Table[x^i, {i, 0, k}],
        Table[1 / (x + 1)^i, {i, 0, k}]
      };
      If[TrueQ[OptionValue[GenerateBasisFunctions, "FunctionRatios"]],
        lsBases = Join[
          lsBases,
          {
            Table[Log[x + i] / Sin[x * i + 1], {i, 1, k}],
            Table[Sin[x * i] / Log[x + i], {i, 1, k}],
            Table[Sin[x * i] / (x^i + 1), {i, 1, k}]
          }]
      ];
      Select[Flatten[lsBases], ! FreeQ[#, x] &]
    ] /; k > 0;


(**************************************************************)
(* RandomFunctionBasis                                       *)
(**************************************************************)

Clear[RandomFunctionBasis];

Options[RandomFunctionBasis] = {"AddRandomBasisRatios" -> True, "FunctionRatios" -> False};

RandomFunctionBasis[x_Symbol, n_Integer, k_Integer, opts : OptionsPattern[]] :=

    Block[{lsAllBasisFuncs, lsRBasis},

      lsAllBasisFuncs =
          GenerateBasisFunctions[x, k,
            FilterRules[{opts}, Options[GenerateBasisFunctions]]];

      lsRBasis = Union@RandomChoice[lsAllBasisFuncs, n];

      If[TrueQ[OptionValue[RandomFunctionBasis, "AddRandomBasisRatios"]],
        lsRBasis =
            RandomSample[
              Join[lsRBasis,
                RandomChoice[lsAllBasisFuncs, n] / RandomChoice[lsAllBasisFuncs, n]],
              Length[lsRBasis]];
        lsRBasis =
            Quiet[Select[lsRBasis,
              NumberQ[# /. x -> 0.] && NumberQ[# /. x -> 1.] &]];
      ];
      lsRBasis
    ] /; n > 0 && k > 0;


(**************************************************************)
(* FindFormulaByQRMon                                        *)
(**************************************************************)

Clear[FindFormulaByQRMon];

Options[FindFormulaByQRMon] =
    Join[
      {"Bases" -> Automatic,
        "LeafCountWeight" -> 1 / 100,
        "MaxNumberOfBases" -> Infinity,
        "ErrorAggregationFunction" -> Max@*Abs},
      Options[RandomFunctionBasis]
    ];

FindFormulaByQRMon[data_, x_Symbol, n_ : 1, opts : OptionsPattern[]] :=
    Module[{lsBases, lcWeight, maxNBases, lsRes, maxLeafCount},

      lsBases = OptionValue[FindFormulaByQRMon, "Bases"];
      lcWeight = OptionValue[FindFormulaByQRMon, "LeafCountWeight"];
      maxNBases = OptionValue[FindFormulaByQRMon, "MaxNumberOfBases"];

      If[TrueQ[lsBases === Automatic],
        Block[{opts2 = FilterRules[{opts}, Options[RandomFunctionBasis]]},
          Quiet[
            lsBases = Union@
                Join[
                  Flatten[
                    Table[RandomFunctionBasis[x, i, 20, opts2], {i, 1, 3}, {j, 1, 10}],
                    1],
                  Flatten[
                    Table[RandomFunctionBasis[x, 20, 5, opts2], {i, 1, 3}, {j, 1, 10}],
                    1],
                  Flatten[
                    Table[RandomFunctionBasis[x, i, RandomInteger[{4, 6}],
                      "BasisRatios" -> RandomChoice[{True, False}], opts2], {i, 1,
                      20}, {j, 1, 20}], 1]
                ];
            lsBases = Select[lsBases, Length[#] > 0 &]
          ]
        ];

        lsBases =
            RandomSample[Map[1 / Sqrt[Length[#]] &, lsBases] -> lsBases, maxNBases];
        (*lsBases=Append[lsBases,Flatten@Table[{Sin[x*i],Cos[x*i]},{i,0,30}]];*)

        lsBases = Select[lsBases, ! FreeQ[#, x] &];
      ];

      If[! MatchQ[lsBases, {{__} ..}],
        Return[$Failed]
      ];

      Quiet[
        lsRes = Map[FindFormulaByQRMon[data, x, #, opts] &, lsBases];
      ];

      maxLeafCount = Max[LeafCount[#Fit] & /@ lsRes];
      lsRes =
          SortBy[DeleteDuplicates[
            lsRes, #1["Fit"] == #2["Fit"] &], {#Error +
              LeafCount[#Fit] / maxLeafCount * lcWeight} &];
      If[TrueQ[n === All], lsRes, Take[lsRes, UpTo[n]]]

    ] /; TrueQ[n === All] || IntegerQ[n] && n > 0;


FindFormulaByQRMon[data_, x_Symbol, basis_List, opts : OptionsPattern[]] :=
    Block[{errorAggrFunc, qrObj, k, qFunc, qFuncExpr, qFuncExpr2, qErrs},

      errorAggrFunc = OptionValue[FindFormulaByQRMon, "ErrorAggregationFunction"];

      qrObj =
          Fold[
            QRMonBind,
            QRMonUnit[data],
            {
              QRMonRescale[Axes -> {True, True}],
              QRMonQuantileRegressionFit[basis, 0.5, FilterRules[{opts}, Options[QRMonQuantileRegression]]]
            }];

      qFunc = First @ QRMonBind[qrObj, QRMonTakeRegressionFunctions];

      qErrs = First @ Fold[ QRMonBind, qrObj, {QRMonErrors["RelativeErrors" -> False], QRMonTakeValue }];

      qFuncExpr = Simplify[qFunc[x]];
      qFuncExpr2 =
          Simplify[Rescale[
            qFuncExpr /. x -> Rescale[x, MinMax[data[[All, 1]]], {0, 1}], {0, 1},
            MinMax[data[[All, 2]]]]];

      <|"Fit" -> qFuncExpr2, "Error" -> errorAggrFunc[qErrs[[All, 2]]]|>
    ];


(**************************************************************)
(* PlotDataAndFit                                            *)
(**************************************************************)

Clear[PlotDataAndFit];
PlotDataAndFit[data_, x_Symbol, aFFRes_Association, opts : OptionsPattern[]] :=
    ListPlot[
      {data, Tooltip[{#, aFFRes["Fit"] /. x -> #} & /@ N[data][[All, 1]],
        aFFRes["Fit"]]},
      opts,
      Joined -> {False, True},
      PlotLegends -> {"Data", "Fit"},
      PlotTheme -> "Detailed", ImageSize -> Medium];

End[]; (* `Private` *)

EndPackage[]