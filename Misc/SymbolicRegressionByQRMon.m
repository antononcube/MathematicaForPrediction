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
   1. [X] Implement the use of standard function bases.
      - [X] B-splines
      - [X] Chebyshev
      - [X] (Centered) polynomials
      - [X] Sin/Cos
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

If[Length[SubValues[MonadicStructuralBreaksFinder`QRMonFindChowTestLocalMaxima]] == 0,
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

FindFormulaByQRMonBasis::usage = "FindFormulaByQRMonBasis[data_, x_Symbol, basis : (_List | _Integer) ] \
finds a formula for data using specified basis functions that have x as argument.";

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
      {
        "Bases" -> Automatic,
        "ErrorAggregationFunction" -> Max@*Abs,
        "LeafCountWeight" -> 1 / 100,
        "MaxNumberOfBases" -> Infinity,
        "RegressionFunction" -> Automatic,
        "RescaleSpec" -> Automatic,
        "Simplify" -> False
      },
      Options[RandomFunctionBasis]
    ];

FindFormulaByQRMon::args = "The expected signatures are \
FindFormulaByQRMon[data_, x_Symbol, n_Integer, ___] or FindFormulaByQRMon[data_, x_Symbol, basis_List, ___].";

FindFormulaByQRMon::nfbs = "Unknown bases specified with \"Bases\". Computing with Chebyshev polynomials.";

FindFormulaByQRMon::nrgf = "Unknown regression function specified with \"RegressionFunction\". Computing with QRMonQuantileRegressionFit.";

FindFormulaByQRMon[data_, x_Symbol, n_ : 1, opts : OptionsPattern[]] :=
    Module[{lsBases, lcWeight, maxNBases, regFunc, rescaleSpec, simplifyQ, lsRes, maxLeafCount},

      lsBases = OptionValue[FindFormulaByQRMon, "Bases"];
      lcWeight = OptionValue[FindFormulaByQRMon, "LeafCountWeight"];
      maxNBases = OptionValue[FindFormulaByQRMon, "MaxNumberOfBases"];
      regFunc = OptionValue[FindFormulaByQRMon, "RegressionFunction"];
      rescaleSpec = OptionValue[FindFormulaByQRMon, "RescaleSpec"];
      simplifyQ = OptionValue[FindFormulaByQRMon, "Simplify"];

      If[ TrueQ[rescaleSpec === Automatic], rescaleSpec = {False, False}];

      (* Derive search space of function bases. *)
      Which[
        TrueQ[lsBases === Automatic],
        (* Random basis generation *)
        Block[{opts2 = FilterRules[{opts}, Options[RandomFunctionBasis]]},
          Quiet[
            lsBases = Union@
                Join[
                  Flatten[Table[RandomFunctionBasis[x, i, 20, opts2], {i, 1, 3}, {j, 1, 10}], 1],
                  Flatten[Table[RandomFunctionBasis[x, 20, 5, opts2], {i, 1, 3}, {j, 1, 10}], 1],
                  Flatten[Table[RandomFunctionBasis[x, i, RandomInteger[{4, 6}], "BasisRatios" -> RandomChoice[{True, False}], opts2], {i, 1, 20}, {j, 1, 20}], 1]
                ];
            lsBases = Select[lsBases, Length[#] > 0 &]
          ];

          lsBases = RandomSample[Map[1 / Sqrt[Length[#]] &, lsBases] -> lsBases, maxNBases];

          lsBases = Select[lsBases, ! FreeQ[#, x] &];

          rescaleSpec = {True, False}
        ],

        MemberQ[{ "CenteredPolynomials", "Polynomials", "PolynomialBasis" }, lsBases],
        Block[{ m = Mean @ data[[All, 1]] },
          lsBases = Rest @ Rest @ FoldList[ Append, {}, Table[ (x - m)^i, {i, 0, maxNBases} ] ]
        ],

        MemberQ[{ "ChebyshevPolynomials", "Chebyshev", "ChebyshevBasis" }, lsBases],
        lsBases = Range[maxNBases],

        MemberQ[{ "Sin", "Sine", "SinBasis", "SineBasis" }, lsBases],
        lsBases = FoldList[ Append, {1}, Table[ Sin[x * i], {i, 1, maxNBases} ] ];
        rescaleSpec = {True, False},

        MemberQ[{ "Cos", "Cosine", "CosBasis", "CosineBasis" }, lsBases],
        lsBases = Rest @ FoldList[ Append, {}, Table[ Cos[x * i], {i, 0, maxNBases} ] ];
        rescaleSpec = {True, False},

        MemberQ[{ "SinCos", "SineCosine", "SinCosBasis", "SineCosineBasis" }, lsBases],
        lsBases = Rest @ FoldList[ Join, {}, Table[ { Sin[x * i], Cos[x * i] }, {i, 0, maxNBases} ] ];
        rescaleSpec = {True, False},

        MemberQ[{ "BSplines", "BSplinePolynomials", "BSplineBasis" }, lsBases],
        regFunc = QRMonQuantileRegression;
        lsBases = Range[maxNBases],

        StringQ[lsBases],
        Message[FindFormulaByQRMon::nfbs];
        lsBases = Rest @ FoldList[ Append, {}, Range[maxNBases] ]
      ];

      If[! MatchQ[lsBases, {{__} ..} | {_Integer..}],
        Return[$Failed]
      ];

      (* Fit for each basis. *)
      Quiet[
        lsRes = Map[FindFormulaByQRMonBasis[data, x, #, "RegressionFunction" -> regFunc, "RescaleSpec" -> rescaleSpec, opts] &, lsBases];
      ];

      (* Simplify fit expressions *)
      lsRes = Join[ #, <| "SimplifiedFit" -> Simplify[#Fit] |> ] & /@ lsRes;

      (* Sort taking both fitting error and expression complexity into account. *)
      maxLeafCount = Max[LeafCount[#Fit] & /@ lsRes];

      lsRes =
          SortBy[
            DeleteDuplicates[lsRes, #1["Fit"] == #2["Fit"] &],
            {#Error + LeafCount[#SimplifiedFit] / maxLeafCount * lcWeight} &
          ];

      (* Simplify fit expressions *)
      If[ simplifyQ,
        lsRes = Map[ <| "Fit" -> #SimplifiedFit, "Error" -> #Error |>&, lsRes ]
      ];

      (* End result *)
      If[TrueQ[n === All], lsRes, Take[lsRes, UpTo[n]]]

    ] /; TrueQ[n === All] || IntegerQ[n] && n > 0;

FindFormulaByQRMon[___] :=
    Block[{},
      Message[FindFormulaByQRMon::args];
      $Failed
    ];


(**************************************************************)
(* FindFormulaByQRMonBasis                                    *)
(**************************************************************)

Clear[FindFormulaByQRMonBasis];

Options[FindFormulaByQRMonBasis] = Options[FindFormulaByQRMon];

FindFormulaByQRMonBasis[data_, x_Symbol, basis : (_List | _Integer), opts : OptionsPattern[]] :=
    Block[{errorAggrFunc, regFunc, qrObj, qFunc, qFuncExpr, qFuncExpr2, qErrs, rescaleSpec},

      (* Error aggregation *)
      errorAggrFunc = OptionValue[FindFormulaByQRMonBasis, "ErrorAggregationFunction"];

      (* Regression function *)
      regFunc = OptionValue[FindFormulaByQRMonBasis, "RegressionFunction"];

      If[ TrueQ[regFunc === Automatic],
        regFunc = QRMonLeastSquaresFit
      ];

      If[ !MemberQ[{QRMonQuantileRegressionFit, QRMonQuantileRegression, QRMonLeastSquaresFit}, regFunc],
        Message[FindFormulaByQRMon::nrgf];
        regFunc = QRMonQuantileRegressionFit
      ];

      (* Rescale specification *)
      rescaleSpec = OptionValue[FindFormulaByQRMonBasis, "RescaleSpec"];
      If[ rescaleSpec === Automatic,
        rescaleSpec = If[ IntegerQ[basis], {False, False}, {True, False} ];
      ];

      (* Fit with basis *)
      Which[

        TrueQ[regFunc === QRMonLeastSquaresFit ],
        qrObj =
            Fold[
              QRMonBind,
              QRMonUnit[data],
              {
                QRMonRescale[Axes -> rescaleSpec],
                If[ IntegerQ[basis], QRMonLeastSquaresFit[basis], QRMonLeastSquaresFit[basis, x]]
              }],

        True,
        qrObj =
            Fold[
              QRMonBind,
              QRMonUnit[data],
              {
                QRMonRescale[Axes -> rescaleSpec],
                regFunc[basis, 0.5, FilterRules[{opts}, Options[regFunc]]]
              }]
      ];


      (* Get fit error *)
      qErrs = First @ Fold[ QRMonBind, qrObj, {QRMonErrors["RelativeErrors" -> False], QRMonTakeValue }];

      (* Get regression functions *)
      qFunc = First @ QRMonBind[qrObj, QRMonTakeRegressionFunctions];

      (* Get fit expression *)
      qFuncExpr = qFunc[x];

      If[ TrueQ[ rescaleSpec == {False, False} ],
        qFuncExpr2 = qFuncExpr,
        (*ELSE*)
        qFuncExpr2 = qFuncExpr /. x -> Rescale[x, MinMax[data[[All, 1]]], {0, 1}]
      ];

      (*      qFuncExpr2 =*)
      (*          Simplify[Rescale[*)
      (*            qFuncExpr /. x -> Rescale[x, MinMax[data[[All, 1]]], {0, 1}], {0, 1},*)
      (*            MinMax[data[[All, 2]]]]];*)

      <|"Fit" -> qFuncExpr2, "Error" -> errorAggrFunc[qErrs[[All, 2]]]|>
    ];


(**************************************************************)
(* PlotDataAndFit                                             *)
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