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
   2. [X] Distributions finding by CDF data.
   3. [ ] Implement options to determine target (basis) functions.
   4. [ ] Improve the random search algorithm.
   5. [ ] Option-specified parallel computations.
   6. [ ] Implement random fits over segments derived by found structural breaks.
   7. [ ] Unit tests.
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
selects (at most) n random functions from a function basis generated with GenerateBasisFunctions[x, k].";

FindFormulaByQRMon::usage = "FindFormulaByQRMon[data_, x_Symbol, n_Integer] \
finds n formulas for data using basis functions that have x as argument.";

FindFormulaByBasisFit::usage = "FindFormulaByBasisFit[data_, x_Symbol, basis : (_List | _Integer) ] \
finds a formula for data using specified basis functions that have x as argument.";

FindDistributionByCDFValues::usage = "FindDistributionByCDFData[ cdfValues : {{_,_}..}, n_Integer, ___] \
finds n distributions that are best fit for cdfValues.";

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

      lsAllBasisFuncs = GenerateBasisFunctions[x, k, FilterRules[{opts}, Options[GenerateBasisFunctions]]];

      lsRBasis = Union@RandomChoice[lsAllBasisFuncs, n];

      If[TrueQ[OptionValue[RandomFunctionBasis, "AddRandomBasisRatios"]],
        lsRBasis =
            RandomSample[
              Join[lsRBasis, RandomChoice[lsAllBasisFuncs, n] / RandomChoice[lsAllBasisFuncs, n]],
              Length[lsRBasis]
            ];
        lsRBasis =
            Quiet[Select[lsRBasis, NumberQ[# /. x -> 0.] && NumberQ[# /. x -> 1.] &]];
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
        "MaxNumberOfBases" -> 5,
        "RegressionFunction" -> Automatic,
        "RelativeErrors" -> True,
        "RescaleSpec" -> Automatic,
        "Simplify" -> False
      },
      Options[RandomFunctionBasis],
      Options[QRMonQuantileRegression]
    ];

FindFormulaByQRMon::args = "The expected signatures are \
FindFormulaByQRMon[data_, x_Symbol, n_Integer, ___] or FindFormulaByQRMon[data_, x_Symbol, basis_List, ___].";

FindFormulaByQRMon::nfbs = "Unknown bases specified with \"Bases\". Computing with Chebyshev polynomials.";

FindFormulaByQRMon::nrgf = "Unknown regression function specified with \"RegressionFunction\". Computing with QRMonQuantileRegressionFit.";

FindFormulaByQRMon[data_, x_Symbol, n_ : 1, opts : OptionsPattern[]] :=
    Module[{lsBases, lcWeight, maxNBases, regFunc, rescaleSpec, simplifyQ,
      lsBases2, lsAllAutomaticBasisNames, lsRes, maxLeafCount},

      lsBases = OptionValue[FindFormulaByQRMon, "Bases"];
      lcWeight = OptionValue[FindFormulaByQRMon, "LeafCountWeight"];
      maxNBases = OptionValue[FindFormulaByQRMon, "MaxNumberOfBases"];
      regFunc = OptionValue[FindFormulaByQRMon, "RegressionFunction"];
      rescaleSpec = OptionValue[FindFormulaByQRMon, "RescaleSpec"];
      simplifyQ = OptionValue[FindFormulaByQRMon, "Simplify"];

      If[ TrueQ[rescaleSpec === Automatic], rescaleSpec = {False, False}];

      (* Derive search space of function bases. *)
      Which[
        MemberQ[ {Random, "Random"}, lsBases ],
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

          lsBases = Select[lsBases, ! FreeQ[#, x] &];
          lsBases = RandomSample[Map[1 / Sqrt[Length[#]] &, lsBases] -> lsBases, 3 * maxNBases];

          lsBases2 = Flatten @ lsBases;
          lsBases2 = Table[ RandomSample[Map[1 / LeafCount[#] &, lsBases2] -> lsBases2, i], {i, 2 * maxNBases}];

          lsBases = RandomSample[ Join[lsBases, lsBases2], maxNBases];

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

        MemberQ[{ "BSpline", "BSplines", "BSplinePolynomials", "BSplineBasis", BSplineBasis }, lsBases],
        regFunc = QRMonQuantileRegression;
        lsBases = Range[maxNBases],

        StringQ[lsBases],
        Message[FindFormulaByQRMon::nfbs];
        lsBases = Rest @ FoldList[ Append, {}, Range[maxNBases] ]
      ];

      If[!( VectorQ[lsBases, StringQ] || MatchQ[lsBases, {{__} ..} | {_Integer..} | Automatic ] ),
        Return[$Failed]
      ];

      (* All automatic basis names. *)
      lsAllAutomaticBasisNames = Flatten @ {
        { "CenteredPolynomials", "Polynomials", "PolynomialBasis" },
        { "ChebyshevPolynomials", "Chebyshev", "ChebyshevBasis" },
        { "Sin", "Sine", "SinBasis", "SineBasis" },
        { "Cos", "Cosine", "CosBasis", "CosineBasis" },
        { "SinCos", "SineCosine", "SinCosBasis", "SineCosineBasis" },
        { "BSpline", "BSplines", "BSplinePolynomials", "BSplineBasis", BSplineBasis }
      };

      (* Fit for each basis. *)
      Which[
        TrueQ[ lsBases === Automatic ],
        lsRes = Flatten @ Map[ FindFormulaByQRMon[data, x, Ceiling[ n / 2 ], "Bases" -> #, opts]&, { "Chebyshev", "BSplines", "Sin", "Cos"} ],

        VectorQ[lsBases, StringQ] &&
            Length[ Intersection[ lsBases, lsAllAutomaticBasisNames ] ] == Length[lsBases],
        lsRes = Flatten @ Map[ FindFormulaByQRMon[data, x, Ceiling[ n / 2 ], "Bases" -> #, opts]&, Union @ lsBases ],

        True,
        Quiet[
          lsRes = Map[FindFormulaByBasisFit[data, x, #, "RegressionFunction" -> regFunc, "RescaleSpec" -> rescaleSpec, opts] &, lsBases];
        ]
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
(* FindFormulaByBasisFit                                      *)
(**************************************************************)

Clear[FindFormulaByBasisFit];

Options[FindFormulaByBasisFit] = Options[FindFormulaByQRMon];

FindFormulaByBasisFit[data_, x_Symbol, basis : (_List | _Integer), opts : OptionsPattern[]] :=
    Block[{errorAggrFunc, regFunc, relativeErrorsQ, qrObj, qFunc, qFuncExpr, qFuncExpr2, qErrs, rescaleSpec},

      (* Error aggregation *)
      errorAggrFunc = OptionValue[FindFormulaByBasisFit, "ErrorAggregationFunction"];

      (* Regression function *)
      regFunc = OptionValue[FindFormulaByBasisFit, "RegressionFunction"];

      If[ TrueQ[regFunc === Automatic],
        regFunc = QRMonLeastSquaresFit
      ];

      If[ !MemberQ[{QRMonQuantileRegressionFit, QRMonQuantileRegression, QRMonLeastSquaresFit}, regFunc],
        Message[FindFormulaByQRMon::nrgf];
        regFunc = QRMonQuantileRegressionFit
      ];

      (* Relative errors *)
      relativeErrorsQ = TrueQ[ OptionValue[FindFormulaByBasisFit, "RelativeErrors"] ];

      (* Rescale specification *)
      rescaleSpec = OptionValue[FindFormulaByBasisFit, "RescaleSpec"];
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
      qErrs = First @ Fold[ QRMonBind, qrObj, {QRMonErrors["RelativeErrors" -> relativeErrorsQ], QRMonTakeValue }];

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
(* Distributions                                              *)
(**************************************************************)

(* The distributions were obtained with this code: *)
(*
lsDistributions = ToExpression /@ Names["*Distribution"];

lsTwoParamDist =
  Select[lsDistributions,
    FreeQ[PDF[#[a, b], x], #] &&
      DistributionDomain[#[a, b]] ===
       Interval[{-Infinity, Infinity}] &] // Quiet;

lsThreeParamDist =
  Select[lsDistributions,
    FreeQ[PDF[#[a, b, c], x], #] &&
      DistributionDomain[#[a, b, c]] ===
       Interval[{-Infinity, Infinity}] &] // Quiet;
*)

lsTwoParamDists = {CauchyDistribution, ExtremeValueDistribution,
  FisherZDistribution, GumbelDistribution, LaplaceDistribution,
  LogisticDistribution, MaxStableDistribution, MinStableDistribution,
  MoyalDistribution, NoncentralStudentTDistribution,
  NormalDistribution, SechDistribution, VoigtDistribution};

lsThreeParamDists = {ExpGammaDistribution, ExponentialPowerDistribution,
  MaxStableDistribution, MinStableDistribution, SkewNormalDistribution,
  StudentTDistribution, TsallisQGaussianDistribution};


(**************************************************************)
(* CDFValuesQ                                                 *)
(**************************************************************)

Clear[CDFValuesQ];
CDFValuesQ[x : {{_, _} ..}] :=
    VectorQ[x[[All, 2]], NumericQ[#] || MatchQ[#, DirectedInfinity[_]] &] && 0 <= Min[x[[All, 2]]] && Max[x[[All, 2]]] <= 1;
CDFValuesQ[___] := False;


(**************************************************************)
(* FindDistributionByCDFData                                  *)
(**************************************************************)

Clear[FindDistributionByCDFData];

Options[FindDistributionByCDFData] = Join[{"NumberOfPoints" -> 1000}, Options[FindDistribution]];

FindDistributionByCDFData[cdfValues_?CDFValuesQ, n_Integer, opts : OptionsPattern[]] :=
    Module[{nPoints, cdfFunc, dist, x, rData},
      nPoints = OptionValue[FindDistributionByCDFData, "NumberOfPoints"];

      cdfFunc = Interpolation[cdfValues, InterpolationOrder -> 1];
      dist = ProbabilityDistribution[{"CDF", cdfFunc[x]}, {x,
        Min[cdfValues[[All, 1]]], Max[cdfValues[[All, 1]]]},
        Method -> "Normalize"];
      rData = RandomVariate[dist, 1000];

      FindDistribution[rData, n, FilterRules[opts, Options[FindDistribution]]]
    ];


(**************************************************************)
(* FindDistributionByCDFFit                                   *)
(**************************************************************)

Clear[FindDistributionByCDFFit];

Options[FindDistributionByCDFFit] = Join[{TargetFunctions -> Automatic, TimeConstraint -> 1}, Options[NonlinearModelFit]];

FindDistributionByCDFFit[cdfValues_?CDFValuesQ, n_Integer, opts : OptionsPattern[]] :=
    Module[{targetFunctions, timeConstraint, a, b, c, x, aRes, aRes2 = <||>, aRes3 = <||>},

      targetFunctions = OptionValue[FindDistributionByCDFFit, TargetFunctions];
      timeConstraint = OptionValue[FindDistributionByCDFFit, TimeConstraint];

      If[MemberQ[{2, "TwoArgumentDistributions", All, Automatic}, targetFunctions],
        aRes2 =
            Association@
                Map[
                  # -> Quiet[ TimeConstrained[
                    NonlinearModelFit[
                      cdfValues, CDF[#[a, b], x], {{a, Mean[cdfValues[[All, 1]]]}, b}, x,
                      FilterRules[{opts}, Options[NonlinearModelFit]],
                      PrecisionGoal -> 3, AccuracyGoal -> 4, Method -> "Gradient", MaxIterations -> 1000
                    ],
                    timeConstraint, $Failed ] ]&,
                  lsTwoParamDists
                ];
        aRes2 = Select[ aRes2, !TrueQ[$Failed === #]& ]
      ];

      If[MemberQ[{3, "ThreeArgumentDistributions", All}, targetFunctions],
        aRes3 =
            Association@
                Map[
                  # -> Quiet[ TimeConstrained[
                    NonlinearModelFit[
                      cdfValues,
                      CDF[#[a, b, c], x], {{a, Mean[cdfValues[[All, 1]]]}, b, c}, x,
                      FilterRules[{opts}, Options[NonlinearModelFit]],
                      PrecisionGoal -> 3, AccuracyGoal -> 4, Method -> "Gradient", MaxIterations -> 1000
                    ],
                    timeConstraint, $Failed] ] &,
                  lsThreeParamDists
                ];
        aRes3 = Select[ aRes3, !TrueQ[$Failed === #]& ]
      ];

      aRes = Join[aRes2, aRes3];
      aRes = Take[Quiet@SortBy[aRes, RootMeanSquare[#["FitResiduals"]] &], UpTo[n]];

      Quiet @ KeyValueMap[#1[Sequence @@ #2["BestFitParameters"][[All, 2]]] &, aRes]
    ];


(**************************************************************)
(* FindDistributionByCDF                                      *)
(**************************************************************)

Clear[FindDistributionByCDFValues];

SyntaxInformation[FindDistributionByCDFValues] = "ArgumentsPattern" -> {_, _., OptionsPattern[]};

FindDistributionByCDFValues::args = "The first argument is expected to be a list of CDF-value-and-probability pairs. \
The second argument is expected to be a positive integer.";

FindDistributionByCDFValues::nmeth = "Unknown method.";

Options[FindDistributionByCDFValues] = {Method -> "Fit"};

FindDistributionByCDFValues[cdfValues_?CDFValuesQ, n_Integer : 1,
  opts : OptionsPattern[]] :=
    Block[{method},
      method = Flatten@List@OptionValue[opts, Method];

      Which[

        MemberQ[{"FindDistribution", FindDistribution}, First@method],
        FindDistributionByCDFData[cdfValues, n,
          FilterRules[Cases[method, _Rule], Options[FindDistributionByCDFData]]],

        MemberQ[{"Fit", Fit, "NonlinearModelFit", NonlinearModelFit}, First@method],
        FindDistributionByCDFFit[cdfValues, n,
          FilterRules[Cases[method, _Rule], Options[FindDistributionByCDFFit]]],

        True,
        Message[FindDistributionByCDFValues::nmeth];
        $Failed
      ]
    ] /; CDFValuesQ[cdfValues] && n > 0;

FindDistributionByCDFValues[___] :=
    Block[{},
      Message[FindDistributionByCDFValues::args];
      $Failed
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