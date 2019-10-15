(*
    Quantile Regression Mathematica package
    Copyright (C) 2014  Anton Antonov

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
	  antononcube @ gmail.com,
	  Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 1.1 *)
(*

  # In brief

  The two main functions of this package are QuantileRegressionFit and QuantileRegression.

  The functions QuantileEnvelope and QuantileEnvelopeRegion are still experimental, but developed enough to be useful.

  For an introduction to Quantile Regression see the article [RK1] and the book [RK2].

  The implementations in this package are explained in [AA1, AA2, AA3].

  The software monad QRMon, [AA4, AAp1], for rapid specification of Quantile Regression workflows is based on
  this package. The package [AAp1] and the documents [AA4, AA4a] provide extensive usage examples of Quantile Regression.


  # QuantileRegressionFit

  The arguments and the result of QuantileRegressionFit are very similar to those of the function Fit.

  In order to find the quantile functions that fit through the data QuantileRegressionFit can use
  LinearProgramming, Minimize, or NMinimize through the Method option,
  e.g. Method->Minimize or Method->{LinearProgramming, Method->"Simplex", Tolerance->10^-6.0} .

  Using Minimize can be very slow for large data sets -- that method is included for didactic purposes.

  The linear programming implementation is based on the (non-dual) formulation in the article [RK1].

  I experimented with using DualLinearProgramming (provided by Mathematica) and with the dozen experiments I made
  I obtained the same results for the same computing time.


  # QuantileRegression

  The function QuantileRegression uses B-splines in order to calculate the regression quantiles.

  The regression quantiles are returned as pure functions.

  The option InterpolationOrder can be used to specify the order of the splines.

  Similar to QuantileRegressionFit the Method option can take LinearProgramming, Minimize, and NMinimize specifications.


  # References

  [RK1] Roger Koenker, Gilbert Bassett, Jr., "Regression Quantiles", Econometrica, Vol. 46, No. 1. (Jan., 1978), pp. 33-50.

  [RK2] Roger Koenker, Quantile Regression, ‪Cambridge University Press, 2005‬.

  [AA1] Anton Antonov, "Quantile regression through linear programming", (2013), MathematicaForPrediction at WordPress.
        URL: https://mathematicaforprediction.wordpress.com/2013/12/16/quantile-regression-through-linear-programming/ .

  [AA2] Anton Antonov, "Quantile regression through linear programming", (2013), MathematicaForPrediction at GitHub.
        URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Quantile%20regression%20through%20linear%20programming.pdf .

  [AA3] Anton Antonov, "Quantile regression with B-splines", (2014), MathematicaForPrediction at WordPress.
        URL: https://mathematicaforprediction.wordpress.com/2014/01/01/quantile-regression-with-b-splines/ .

  [AA4] Anton Antonov, "A monad for Quantile Regression workflows", (2018), MathematicaForPrediction at WordPress.
        URL: https://mathematicaforprediction.wordpress.com/2018/08/01/a-monad-for-quantile-regression-workflows/ .

  [AA4a] Anton Antonov, "A monad for Quantile Regression workflows", (2018), MathematicaForPrediction at WordPres.
         URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-Quantile-Regression-workflows.md .

  [AAp1] Anton Antonov, Monadic Quantile Regression Mathematica package, (2018), MathematicaForPrediction at GitHub.
         URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m .

*)

(*
   For version 1.1 implemented quantile regression with B-splines.

   Renamed the original function QuantileRegression to QuantileRegressionFit.

   Overloading the original function QuantileRegression with the B-splines implementation is not a good idea because:

   1. the original quantile regression function returns function expressions,

   2. the B-spline quantile regression function returns anonymous functions.
*)

(*
   2016.04.30
   Added experimental implementations for finding of quantile regression envelope regions for 2D and 3D data.

   2014.11.01
   Added experimental implementation for finding of the points of quantile regression envelopes for 2D data.

   2019.02.16
   Better explanations in Markdown style with added references. Minor code changes.
*)

(*
  TODO
  1. [ ] Return a vector with the weights of the used basis functions.
  2. [ ] Better messages.
  3. [ ] Creating (more) signatures with default values.
         E.g. no knots specification and/or no quantile specifications for QuantileRegression.
         Maybe not that needed because of QRMon, [AA4a, AAp1].

*)

BeginPackage["QuantileRegression`"];

QuantileRegressionFit::usage = "QuantileRegressionFit[data,funs,var,probs] finds the regression quantiles corresponding \
to the probabilities probs for a list of data as linear combinations of the functions funs of the variable var.";

QuantileRegression::usage = "QuantileRegression[data,ks_List,probs] finds the regression quantiles corresponding \
to the probabilities probs for a list of data as linear combinations of splines generated over the knots ks. \
With the signature QuantileRegression[data,n_Integer,probs] n equally spaced knots are generated. \
The order of the splines is specified with the option InterpolationOrder.";

QuantileEnvelope::usage = "QuantileEnvelope[data_?MatrixQ,probs:(_?NumberQ|{_?NumberQ..}),ndir_Integer] \
experimental implementation of quantile envelopes points finding.";

QuantileEnvelopeRegion::usage = "QuantileEnvelopeRegion[data_?MatrixQ,q_?NumberQ,ndir_Integer] \
experimental implementation of 2D or 3D quantile envelope region finding.";

Begin["`Private`"];

(************************************************************)
(* QuantileRegressionFit                                    *)
(************************************************************)

QuantileRegressionFit::"nmat" = "The first argument is expected to be a matrix of numbers with two columns.";

QuantileRegressionFit::"fvlen" = "The second argument is expected to be list of functions to be fitted with at least one element.";

QuantileRegressionFit::"nvar" = "The third argument is expected to be a symbol.";

QuantileRegressionFit::"nprobs" = "The fourth argument is expected to be a list of numbers representing probabilities.";

QuantileRegressionFit::"nmeth" = "The value of the method option is expected to be \
LinearProgramming, Minimize, NMinimize or a list with LinearProgramming, Minimize, or NMinimize as a first element.";

QuantileRegressionFit::"mmslow" = "With the method Minimize the computations can be very slow for large data sets.";

QuantileRegressionFit::"nargs" = "Four arguments are expected.";

Clear[QuantileRegressionFit];

Options[QuantileRegressionFit] = {Method -> LinearProgramming};

QuantileRegressionFit[data_, funcs_, var_?AtomQ, probs_, opts : OptionsPattern[]] :=
    Block[{mOptVal},
      (*This check should not be applied because the first function can be a constant.*)
      (*!Apply[And,Map[!FreeQ[#,var]&,funcs]],Message[QuantileRegressionFit::\"fvfree\"],*)
      Which[
        ! ( MatrixQ[data, NumericQ] && Dimensions[data][[2]] >= 2 ),
        Message[QuantileRegressionFit::"nmat"]; Return[{}],
        Length[funcs] < 1,
        Message[QuantileRegressionFit::"fvlen"]; Return[{}],
        Head[var] =!= Symbol,
        Message[QuantileRegressionFit::"nvar"]; Return[{}],
        ! VectorQ[probs, NumericQ[#] && 0 <= # <= 1 &],
        Message[QuantileRegressionFit::"nprobs"]; Return[{}]
      ];
      mOptVal = OptionValue[QuantileRegressionFit, Method];
      Which[
        TrueQ[mOptVal === LinearProgramming],
        LPQuantileRegressionFit[data, funcs, var, probs],
        ListQ[mOptVal] && TrueQ[mOptVal[[1]] === LinearProgramming],
        LPQuantileRegressionFit[data, funcs, var, probs, Rest[mOptVal]],
        TrueQ[mOptVal === Minimize || mOptVal === NMinimize],
        MinimizeQuantileRegressionFit[mOptVal, data, funcs, var, probs],
        ListQ[mOptVal] && TrueQ[mOptVal[[1]] === Minimize || mOptVal[[1]] === NMinimize],
        MinimizeQuantileRegressionFit[mOptVal[[1]], data, funcs, var, probs, Rest[mOptVal]],
        True,
        Message[QuantileRegressionFit::"nmeth"]; Return[{}]
      ]
    ];

QuantileRegressionFit[___] :=
    Block[{},
      Message[QuantileRegressionFit::"nargs"];
      {}
    ];

Clear[LPQuantileRegressionFit];

LPQuantileRegressionFit[dataArg_?MatrixQ, funcs_, var_Symbol, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{data = dataArg, yMedian = 0, yFactor = 1, yShift = 0, mat, n = Dimensions[dataArg][[1]], pfuncs, c, t, qrSolutions},
      If[Min[data[[All, 2]]] < 0,
        yMedian = Median[data[[All, 2]]];
        yFactor = InterquartileRange[data[[All, 2]]];
        data[[All, 2]] = Standardize[data[[All, 2]], Median, InterquartileRange];
        yShift = Abs[Min[data[[All, 2]]]];(*this is Min[dataArg[[All,2]]-Median[dataArg[[All,2]]*)

        data[[All, 2]] = data[[All, 2]] + yShift ;
      ];

      pfuncs = Map[Function[{fb}, With[{f = fb /. (var -> Slot[1])}, f &]], funcs];
      mat = Map[Function[{f}, f /@ data[[All, 1]]], pfuncs];
      mat = Map[Flatten, Transpose[Join[mat, {IdentityMatrix[n], -IdentityMatrix[n]}]]];
      mat = N[SparseArray[mat]];

      qrSolutions =
          Table[
            c = Join[ConstantArray[0, Length[funcs]], ConstantArray[1, n] q, ConstantArray[1, n] (1 - q)];
            t = LinearProgramming[c, mat, Transpose[{data[[All, 2]], ConstantArray[0, n]}], opts];
            If[ !(VectorQ[t, NumberQ] && Length[t] > Length[funcs]), ConstantArray[0, Length[funcs]], t ]
            , {q, probs}];


      If[yMedian == 0 && yFactor == 1,
        Map[funcs.# &, qrSolutions[[All, 1 ;; Length[funcs]]]],
        Map[Expand[yFactor ((funcs.#) - yShift) + yMedian] &, qrSolutions[[All, 1 ;; Length[funcs]]]]
      ]
    ];


Clear[MinimizeQuantileRegressionFit];

MinimizeQuantileRegressionFit[methodFunc_, data_?MatrixQ, funcs_, var_Symbol, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{minFunc, Tilted, QRModel, b, bvars, qrSolutions},

      If[Length[data] > 300,
        Message[QuantileRegressionFit::"mmslow"]
      ];

      bvars = Array[b, Length[funcs]];

      Tilted[t_?NumberQ, x_] := Piecewise[{{(t - 1) x, x < 0}, {t x, x >= 0}}] /; t <= 1;
      QRModel[x_] := Evaluate[(bvars.funcs) /. var -> x];

      qrSolutions =
          Table[
            minFunc = Total[(Tilted[q, #1[[2]] - QRModel[#1[[1]]]] &) /@ data];
            methodFunc[{minFunc}, bvars, opts]
            , {q, probs}];

      Map[funcs.# &, qrSolutions[[All, 2, All, 2]]]
    ];


(************************************************************)
(* QuantileRegression                                       *)
(************************************************************)

Clear[QuantileRegression];

SyntaxInformation[QuantileRegression] = { "ArgumentsPattern" -> { _., _, _, OptionsPattern[] } };

QuantileRegression::"nmat" = "The first argument is expected to be a matrix of numbers with two columns.";

QuantileRegression::"knord" = "The specified knots `1` and interpolation order `2` produce no B-Spline basis functions. \
The expression n - i - 2 should be non-negative, where n is the number of knots and i is the interpolation order.";

QuantileRegression::"zerob" = "The specified knots `1` and interpolation order `2` produced a list of zeroes instead of a list of B-Spline basis functions.";

QuantileRegression::"knspec" = "The knots specification (for using B-splines) has to be an integer or a list of numbers.";

QuantileRegression::"nprobs" = "The third argument is expected to be a list of numbers representing probabilities.";

QuantileRegression::"nmeth" = "The value of the method option is expected to be \
LinearProgramming, Minimize, NMinimize or a list with LinearProgramming, Minimize, or NMinimize as a first element.";

QuantileRegression::"norder" = "The value of the option InterpolationOrder is expected to be a non-negative integer.";

QuantileRegression::"nargs" = "Three arguments are expected.";

QuantileRegression::"mmslow" = "With the method Minimize the computations can be very slow for large data sets.";

Options[QuantileRegression] = {InterpolationOrder -> 3, Method -> LinearProgramming};

QuantileRegression[data_?VectorQ, knots_, probs_, opts : OptionsPattern[]] :=
    QuantileRegression[ Transpose[{ Range[Length[data]], data}], knots, probs, opts];

QuantileRegression[data : (_TimeSeries | _TemporalData), knots_, probs_, opts : OptionsPattern[]] :=
    QuantileRegression[ QuantityMagnitude[data["Path"]], knots, probs, opts];

QuantileRegression[data_, knots_, probsArg_, opts : OptionsPattern[]] :=
    Block[{mOptVal, intOrdOptVal, probs = Flatten @ List @ probsArg},

      Which[
        ! ( MatrixQ[data, NumericQ] && Dimensions[data][[2]] >= 2 ),
        Message[QuantileRegression::"nmat"]; Return[{}],

        !(IntegerQ[knots] && knots > 0 || VectorQ[knots, NumericQ]),
        Message[QuantileRegression::"knspec"]; Return[{}],

        ! VectorQ[probs, NumericQ[#] && 0 <= # <= 1 &],
        Message[QuantileRegression::"nprobs"]; Return[{}]
      ];

      mOptVal = OptionValue[QuantileRegression, Method];
      intOrdOptVal = OptionValue[QuantileRegression, InterpolationOrder];

      Which[
        !( IntegerQ[intOrdOptVal] && intOrdOptVal >= 0 ),
        Message[QuantileRegression::"norder"]; Return[{}],

        TrueQ[mOptVal === LinearProgramming],
        LPSplineQuantileRegression[data, knots, intOrdOptVal, probs],

        ListQ[mOptVal] && TrueQ[mOptVal[[1]] === LinearProgramming],
        LPSplineQuantileRegression[data, knots, intOrdOptVal, probs, Rest[mOptVal]],

        TrueQ[mOptVal === Minimize || mOptVal === NMinimize],
        MinimizeSplineQuantileRegression[mOptVal, data, knots, intOrdOptVal, probs],

        ListQ[mOptVal] && TrueQ[mOptVal[[1]] === Minimize || mOptVal[[1]] === NMinimize],
        MinimizeSplineQuantileRegression[mOptVal[[1]], data, knots, intOrdOptVal, probs, Rest[mOptVal]],

        True,
        Message[QuantileRegression::"nmeth"]; Return[{}]
      ]
    ];

QuantileRegression[___] :=
    Block[{},
      Message[QuantileRegression::"nargs"];
      {}
    ];

Clear[LPSplineQuantileRegression];

LPSplineQuantileRegression[data_?MatrixQ, npieces_Integer, order_Integer, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    LPSplineQuantileRegression[data, Rescale[Range[0, 1, 1 / npieces], {0, 1}, {Min[data[[All, 1]]], Max[data[[All, 1]]]}], order, probs, opts];

LPSplineQuantileRegression[dataArg_?MatrixQ, knotsArg : {_?NumberQ ..}, order_Integer, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{data = dataArg, knots = Sort[knotsArg], yMedian = 0, yFactor = 1, yShift = 0, n = Dimensions[dataArg][[1]], pfuncs, c, t, qrSolutions, mat},

      If[Min[data[[All, 2]]] < 0,
        yMedian = Median[data[[All, 2]]];
        yFactor = InterquartileRange[data[[All, 2]]];
        data[[All, 2]] = Standardize[data[[All, 2]], Median, InterquartileRange];
        yShift = Abs[Min[data[[All, 2]]]];
        data[[All, 2]] = data[[All, 2]] + yShift ;
      ];

      (* Enhance the knots list with additional clamped knots. *)
      knots = Join[Table[Min[knots], {order}], knots, Table[Max[knots], {order}]];

      If[Length[knots] - order - 2 < 0,
        Message[QuantileRegression::"knord", knots, order];
        Return[{}]
      ];

      (* B-spline basis expressions *)
      pfuncs = Table[PiecewiseExpand[BSplineBasis[{order, knots}, i, t]], {i, 0, Length[knots] - order - 2}];

      If[VectorQ[pfuncs, # == 0 &],
        Message[QuantileRegression::"zerob", knots, order];
        Return[{}]
      ];

      (* B-spline basis functions *)
      pfuncs = Function[{f}, With[{bf = f /. t -> #}, bf &]] /@ pfuncs;

      (* Create the conditions matrix *)
      mat = Table[Join[Through[pfuncs[data[[i, 1]]]]], {i, 1, n}];
      mat = MapThread[Join, {mat, IdentityMatrix[n], -IdentityMatrix[n]}];

      mat = SparseArray[mat];

      (* Find the regression quantiles *)
      qrSolutions =
          Table[
            c = Join[ConstantArray[0, Length[pfuncs]], ConstantArray[1, n] q, ConstantArray[1, n] (1 - q)];
            t = LinearProgramming[c, mat, Transpose[{data[[All, 2]], ConstantArray[0, n]}], DeleteCases[{opts}, InterpolationOrder -> _]];
            If[! (VectorQ[t, NumberQ] && Length[t] > Length[pfuncs]), ConstantArray[0, Length[pfuncs]], t]
            , {q, probs}];

      If[yMedian == 0 && yFactor == 1,
        Table[ With[{f = pfuncs[[All, 1]].qrSolutions[[i, 1 ;; Length[pfuncs]]]}, f &], {i, 1, Length[probs]}],
        (*ELSE*)
        Map[Function[{ws}, With[{f = yFactor ((pfuncs[[All, 1]].ws) - yShift) + yMedian}, f &]], qrSolutions[[All, 1 ;; Length[pfuncs]]]]
      ]
    ];

Clear[MinimizeSplineQuantileRegression];

MinimizeSplineQuantileRegression[methodFunc_, data_?MatrixQ, npieces_Integer, order_Integer, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    MinimizeSplineQuantileRegression[methodFunc, data, Rescale[Range[0, 1, 1 / npieces], {0, 1}, {Min[data[[All, 1]]], Max[data[[All, 1]]]}], order, probs, opts];

MinimizeSplineQuantileRegression[methodFunc_, dataArg_?MatrixQ, knotsArg : {_?NumberQ ..}, order_Integer, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{data = dataArg, knots = Sort[knotsArg], bvars, pfuncs, b, c, t, Tilted, QRModel, qrSolutions, minFunc},

      If[Length[data] > 300,
        Message[QuantileRegression::"mmslow"]
      ];

      (* Enhance the knots list with additional clamped knots. *)
      knots = Join[Table[Min[knots], {order}], knots, Table[Max[knots], {order}]];

      If[Length[knots] - order - 2 < 0,
        Message[QuantileRegression::"knord", knots, order];
        Return[{}]
      ];

      (* B-spline basis expressions *)
      pfuncs = Table[PiecewiseExpand[BSplineBasis[{order, knots}, i, t]], {i, 0, Length[knots] - order - 2}];

      If[VectorQ[pfuncs, # == 0 &],
        Message[QuantileRegression::"zerob", knots, order];
        Return[{}]
      ];

      (* B-spline basis functions *)
      pfuncs = Function[{f}, With[{bf = f /. t -> #}, bf &]] /@ pfuncs;

      (* Create the model *)
      Tilted[t_?NumberQ, x_] := Piecewise[{{(t - 1) x, x < 0}, {t x, x >= 0}}] /; t <= 1;
      bvars = Array[b, Length[pfuncs]];
      QRModel = With[{tf = bvars.pfuncs[[All, 1]]}, tf &];

      (* Find the regression quantiles *)
      qrSolutions =
          Table[
            minFunc = Total[(Tilted[q, #1[[2]] - QRModel[#1[[1]]]] &) /@ data];
            methodFunc[minFunc, bvars, opts]
            , {q, probs}];

      Table[ With[{f = pfuncs[[All, 1]].(bvars /. qrSolutions[[i, 2]])}, f &], {i, 1, Length[probs]}]
    ] /; order > 0;


(**************************************************************)
(* QuantileEnvelope                                           *)
(**************************************************************)

QuantileEnvelope::qenargs = "Three arguments are expected, two column data matrix, probabilities, and a number of curve points.";
QuantileEnvelope::qemat = "The first argument is expected to be a numeric two column data matrix.";
QuantileEnvelope::qeqs = "The second argument is expected to be a number or a list of numbers between 0 and 1.";
QuantileEnvelope::qen = "The third argument is expected to be an integer greater than 2.";

Clear[QuantileEnvelope];

Options[QuantileEnvelope] =
    {"Tangents" -> True, "StandardizingShiftFunction" -> Mean, "StandardizingScaleFunction" -> InterquartileRange };

QuantileEnvelope[data_, probs_, n_, opts : OptionsPattern[]] :=
    Block[{},
      If[! MatrixQ[data, NumberQ],
        Message[QuantileEnvelope::qemat];
        Return[{}]
      ];
      If[! (TrueQ[ VectorQ[probs, NumberQ] && Apply[And, Map[0 <= # <= 1 &, probs]]] || TrueQ[NumberQ[probs] && (0 <= probs <= 1)]),
        Message[QuantileEnvelope::qeqs];
        Return[{}]
      ];
      If[! TrueQ[IntegerQ[n] && (n > 2)],
        Message[QuantileEnvelope::qen];
        Return[{}]
      ];
      QuantileEnvelopeSimple[data, probs, n, opts]
    ];

Clear[QuantileEnvelopeSimple];

Options[QuantileEnvelopeSimple] = Options[QuantileEnvelope];

QuantileEnvelopeSimple[data_?MatrixQ, q_?NumberQ, n_Integer, opts : OptionsPattern[]] := QuantileEnvelopeSimple[data, {q}, n, opts];

QuantileEnvelopeSimple[dataArg_?MatrixQ, probs : {_?NumberQ ..}, n_Integer, opts : OptionsPattern[]] :=
    Block[{data = dataArg, center, scale, rmat, rmats, qfuncs, x1, x2, y1, rqfuncs, intPoints, t,
      tangentsQ, sdShiftFunc, sdScaleFunc},

      (* Option values *)
      tangentsQ = TrueQ[OptionValue[QuantileEnvelopeSimple, "Tangents"]];

      (* Standardize *)
      sdShiftFunc = OptionValue[QuantileEnvelopeSimple, "StandardizingShiftFunction"];
      If[ TrueQ[sdShiftFunc === Automatic], sdShiftFunc = Mean];

      sdScaleFunc = OptionValue[QuantileEnvelopeSimple, "StandardizingScaleFunction"];
      If[ TrueQ[sdScaleFunc === Automatic], sdScaleFunc = InterquartileRange];

      center = sdShiftFunc @ data;
      scale = sdScaleFunc /@ Transpose[data];
      data = Map[(# - center) / scale &, data];

      (* Rotation matrices *)
      rmat = N[RotationMatrix[2 Pi / n]];
      rmats = NestList[rmat.# &, rmat, n - 1];
      qfuncs = Transpose[ Map[Function[{m}, Quantile[(m.Transpose[data])[[2]], probs]], rmats]];
      If[tangentsQ,
        rqfuncs = Map[Function[{qfs}, MapThread[ Flatten[Expand[{x1, y1}.#1 /. y1 -> #2]] &, {rmats, qfs}]], qfuncs];
        intPoints = Table[(
          t =
              Equal @@@ Transpose[{rqfuncs[[k, i]], rqfuncs[[k, If[i >= Length[rqfuncs[[k]]], 1, i + 1]]] /. x1 -> x2}];
          t = {x1, x2} /. ToRules[Reduce[t, {x1, x2}]];
          rqfuncs[[k, i]] /. x1 -> t[[1]]
        ), {k, 1, Length[rqfuncs]}, {i, 1, Length[rqfuncs[[k]]]}],
        (*ELSE*)

        intPoints = Map[Function[{qfs}, MapThread[{0, #2}.#1 &, {rmats, qfs}]], qfuncs];
      ];

      (* Reverse standardizing *)

      intPoints = Map[(# * scale + center) &, intPoints, {2}];
      intPoints
    ];


(**************************************************************)
(* QuantileEnvelopeRegion                                     *)
(**************************************************************)

QuantileEnvelopeRegion::qemat = "The first argument is expected to be a numeric two or three column data matrix.";

Clear[QuantileEnvelopeRegion];
QuantileEnvelopeRegion[points_?MatrixQ, quantile_?NumberQ, numberOfDirections_Integer] :=
    Which[
      Dimensions[points][[2]] == 2, QuantileEnvelopeRegion2D[points, quantile, numberOfDirections ],
      Dimensions[points][[2]] == 3, QuantileEnvelopeRegion3D[points, quantile, numberOfDirections ],
      True,
      Message[QuantileEnvelopeRegion::qemat]; $Failed
    ];

Clear[QuantileEnvelopeRegion2D];
QuantileEnvelopeRegion2D[points_?MatrixQ, quantile_?NumberQ, numberOfDirections_Integer] :=
    Block[{nd = numberOfDirections, dirs, rmats, qDirPoints, qRegion},
      dirs =
          N@ Table[{Cos[th], Sin[th]}, {th, 2 Pi / (10 nd), 2 Pi, 2 Pi / nd}];
      rmats = RotationMatrix[{{1, 0}, #}] & /@ dirs;
      qDirPoints =
          Flatten[Map[
            Function[{m}, Quantile[(m.Transpose[points])[[2]], quantile]],
            rmats]];
      qRegion =
          ImplicitRegion[ MapThread[(#1.{x, y})[[2]] <= #2 &, {rmats, qDirPoints}], {x, y}];
      qRegion
    ] /; Dimensions[points][[2]] == 2 && 0 < quantile <= 1;


Clear[QuantileEnvelopeRegion3D];
QuantileEnvelopeRegion3D[points_?MatrixQ, quantile_?NumberQ, numberOfDirections_Integer] :=
    Block[{nd = numberOfDirections, dirs, rmats, qDirPoints, qRegion},
      dirs =
          N@Flatten[
            Table[{Cos[th] Cos[phi], Sin[th] Cos[phi], Sin[phi]},
              {th, 2 Pi / (10 nd), 2 Pi, 2 Pi / nd},
              {phi, -Pi, Pi, 2 Pi / nd}], 1];
      rmats = RotationMatrix[{{1, 0, 0}, #}] & /@ dirs;
      qDirPoints =
          Flatten[Map[
            Function[{m}, Quantile[(m.Transpose[points])[[3]], quantile]],
            rmats]];
      qRegion =
          ImplicitRegion[ MapThread[(#1.{x, y, z})[[3]] <= #2 &, {rmats, qDirPoints}], {x, y, z}];
      qRegion
    ] /; Dimensions[points][[2]] == 3 && 0 < quantile <= 1;

End[];

EndPackage[]