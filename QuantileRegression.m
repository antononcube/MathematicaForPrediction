(*
    Quantile regression Mathematica package
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
	antononcube@gmail.com, 
	7320 Colbury Ave, 
	Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2013 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 1.1 *)
(* 
  This package has two functions, QuantileRegressionFit and QuantileRegression. 

  1. QuantileRegressionFit
  The arguments and the result of QuantileRegressionFit are very similar to those of the function Fit. In order to find the quantile functions that fit through the data QuantileRegressionFit can use LinearProgramming, Minimize, or NMinimize through the Method option, e.g. Method->Minimize or Method->{LinearProgramming, Method->"Simplex", Tolerance->10^-6.0} . Using Minimize can be very slow for large data sets -- that method is included for didactic purposes.

  The linear programming implementation is based on the (non-dual) formulation in the article 
 
  Roger Koenker, Gilbert Bassett, Jr., "Regression Quantiles", Econometrica, Vol. 46, No. 1. (Jan., 1978), pp. 33-50.

  I experimented with using DualLinearProgramming (provided by Mathematica) and with the dozen experiments I made I obtained the same results for the same computing time.

  2. QuantileRegression
  The function QuantileRegression uses B-splines in order to calculate the regression quantiles. The regression quantiles are returned as pure functions. The option InterpolationOrder can be used to specify the order of the splines. Similar to QuantileRegressionFit the Method option can take LinearProgramming, Minimize, and NMinimize specifications. 

*)

(* For version 1.1 implemenented quantile regression with B-splines. 
  Renamed the original function QuantileRegression to QuantileRegressionFit.
  Overloading the original function QuantileRegression with the B-splines implementation is not a good idea because:
  1. the original quantile regression function returns function expressions,
  2. the B-spline quantile regression function returns anonymous functions.
*)

(*
   2014.11.01
   Added experimental implementation for finding of the points of quantile regression envelopes for 2D data.
*)

(*
  TODO
  1. Better messages.
*)

BeginPackage["QuantileRegression`"]

QuantileRegressionFit::usage = "QuantileRegression[data,funs,var,qs] finds the regression quantiles corresponding to the quantiles qs for a list of data as linear combinations of the functions funs of the variable var."

QuantileRegression::usage = "QuantileRegression[data,ks_List,qs] finds the regression quantiles corresponding to the quantiles qs for a list of data as linear combinations splines generated over the knots ks. With the signature QuantileRegression[data,n_Integer,qs] n equally spaced knots are generated. The order of the splines is specified with the option InterpolationOrder."

QuantileEnvelope::usage = "QuantileRegression[data_?MatrixQ,qs:(_?NumberQ|{_?NumberQ..}),n_Integer] experimental implementation of quantile envelopes points finding."

Begin["`Private`"]

(************************************************************)
(* QuantileRegressionFit                                    *)
(************************************************************)

QuantileRegressionFit::"nmat" = "The first argument is expected to be a matrix of numbers with two columns.";

QuantileRegressionFit::"fvlen" = "The second argument is expected to be list of functions to be fitted with at least one element.";

QuantileRegressionFit::"nvar" = "The third argument is expected to be a symbol.";

QuantileRegressionFit::"nqntls" = "The fourth argument is expected to be a list of numbers representing quantiles.";

QuantileRegressionFit::"nmeth" = "The value of the method option is expected to be LinearProgramming, Minimize, NMinimize or a list with LinearProgramming, Minimize, or NMinimize as a first element.";

QuantileRegressionFit::"mmslow" = "With the method Minimize the computations can be very slow for large data sets.";

QuantileRegressionFit::"nargs" = "Four arguments are expected."

Clear[QuantileRegressionFit]
Options[QuantileRegressionFit] = {Method -> LinearProgramming};
QuantileRegressionFit[data_, funcs_, var_?AtomQ, qs_, opts : OptionsPattern[]] :=
  Block[{mOptVal},
   (*This check should not be applied because the first function can be a constant.*)
   (*!Apply[And,Map[!FreeQ[#,var]&,funcs]],Message[QuantileRegressionFit::\"fvfree\"],*)
   Which[
    ! ( MatrixQ[data,NumericQ] && Dimensions[data][[2]] >= 2 ),
    Message[QuantileRegressionFit::"nmat"]; Return[{}],
    Length[funcs] < 1,
    Message[QuantileRegressionFit::"fvlen"]; Return[{}],
    Head[var] =!= Symbol,
    Message[QuantileRegressionFit::"nvar"]; Return[{}],
    ! VectorQ[qs, NumericQ[#] && 0 <= # <= 1 &],
    Message[QuantileRegressionFit::"nqntls"]; Return[{}]
   ];
   mOptVal = OptionValue[QuantileRegressionFit, Method];
   Which[
    TrueQ[mOptVal === LinearProgramming],
    LPQuantileRegressionFit[data, funcs, var, qs],
    ListQ[mOptVal] && TrueQ[mOptVal[[1]] === LinearProgramming],
    LPQuantileRegressionFit[data, funcs, var, qs, Rest[mOptVal]],
    TrueQ[mOptVal === Minimize || mOptVal === NMinimize],
    MinimizeQuantileRegressionFit[mOptVal, data, funcs, var, qs],
    ListQ[mOptVal] && TrueQ[mOptVal[[1]] === Minimize || mOptVal[[1]] === NMinimize],
    MinimizeQuantileRegressionFit[mOptVal[[1]], data, funcs, var, qs, Rest[mOptVal]],
    True,
    Message[QuantileRegressionFit::"nmeth"]; Return[{}]
   ]
  ];

QuantileRegressionFit[___]:=
  Block[{},
    Message[QuantileRegressionFit::"nargs"]; 
    {}
  ];

Clear[LPQuantileRegressionFit]
LPQuantileRegressionFit[dataArg_?MatrixQ, funcs_, var_Symbol, qs : {_?NumberQ ..}, opts : OptionsPattern[]] := 
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
      If[ !(VectorQ[t, NumberQ] && Length[t] > Length[funcs]), ConstantArray[0,Length[funcs]], t ]
      , {q, qs}];
      
    
    If[yMedian == 0 && yFactor == 1,
     Map[funcs.# &, qrSolutions[[All, 1 ;; Length[funcs]]]],
     Map[Expand[yFactor ((funcs.#) - yShift) + yMedian] &, qrSolutions[[All, 1 ;; Length[funcs]]]]
    ]
   ];


Clear[MinimizeQuantileRegressionFit]
MinimizeQuantileRegressionFit[methodFunc_, data_?MatrixQ, funcs_, var_Symbol, qs : {_?NumberQ ..}, opts : OptionsPattern[]] :=  
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
     , {q, qs}];

   Map[funcs.# &, qrSolutions[[All, 2, All, 2]]]
  ];


(************************************************************)
(* QuantileRegression                                       *)
(************************************************************)

QuantileRegression::"nmat" = "The first argument is expected to be a matrix of numbers with two columns.";

QuantileRegression::"knord" = "The specified knots `1` and interpolation order `2` produce no B-Spline basis functions. The expression n - i - 2 should be non-negative, where n is the number of knots and i is the interpolation order."

QuantileRegression::"zerob" = "The specified knots `1` and interpolation order `2` produced a list of zeroes instead of a list of B-Spline basis functions."

QuantileRegression::"knspec" = "The knots specification (for using B-splines) has to be an integer or a list of numbers."

QuantileRegression::"nqntls" = "The third argument is expected to be a list of numbers representing quantiles.";

QuantileRegression::"nmeth" = "The value of the method option is expected to be LinearProgramming, Minimize, NMinimize or a list with LinearProgramming, Minimize, or NMinimize as a first element.";

QuantileRegression::"norder" = "The value of the option InterpolationOrder is expected to be a non-negative integer."

QuantileRegression::"nargs" = "Three arguments are expected."

QuantileRegression::"mmslow" = "With the method Minimize the computations can be very slow for large data sets.";

Options[QuantileRegression] = {InterpolationOrder -> 3, Method -> LinearProgramming};
QuantileRegression[data_, knots_, qs_, opts : OptionsPattern[]] :=
  Block[{mOptVal, intOrdOptVal},
   Which[
    ! ( MatrixQ[data,NumericQ] && Dimensions[data][[2]] >= 2 ),
    Message[QuantileRegression::"nmat"]; Return[{}],
    !(IntegerQ[knots] && knots > 0 || VectorQ[knots,NumericQ]),
    Message[QuantileRegression::"knspec"]; Return[{}],
    ! VectorQ[qs, NumericQ[#] && 0 <= # <= 1 &],
    Message[QuantileRegression::"nqntls"]; Return[{}]
   ];
   mOptVal = OptionValue[QuantileRegression, Method];
   intOrdOptVal = OptionValue[QuantileRegression, InterpolationOrder];
   Which[
    !( IntegerQ[intOrdOptVal] && intOrdOptVal >= 0 ),
    Message[QuantileRegression::"norder"]; Return[{}],
    TrueQ[mOptVal === LinearProgramming],
    LPSplineQuantileRegression[data, knots, intOrdOptVal, qs],
    ListQ[mOptVal] && TrueQ[mOptVal[[1]] === LinearProgramming],
    LPSplineQuantileRegression[data, knots, intOrdOptVal, qs, Rest[mOptVal]],
    TrueQ[mOptVal === Minimize || mOptVal === NMinimize],
    MinimizeSplineQuantileRegression[mOptVal, data, knots, intOrdOptVal, qs],
    ListQ[mOptVal] && TrueQ[mOptVal[[1]] === Minimize || mOptVal[[1]] === NMinimize],
    MinimizeSplineQuantileRegression[mOptVal[[1]], data, knots, intOrdOptVal, qs, Rest[mOptVal]],
    True,
    Message[QuantileRegression::"nmeth"]; Return[{}]
   ]
  ];

QuantileRegression[___]:=
  Block[{},
    Message[QuantileRegression::"nargs"]; 
    {}
  ];

Clear[LPSplineQuantileRegression]
LPSplineQuantileRegression[data_?MatrixQ, npieces_Integer, order_Integer, qs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
  LPSplineQuantileRegression[data, Rescale[Range[0, 1, 1/npieces], {0, 1}, {Min[data[[All, 1]]], Max[data[[All, 1]]]}], order, qs, opts];
LPSplineQuantileRegression[dataArg_?MatrixQ, knotsArg : {_?NumberQ ..}, order_Integer, qs : {_?NumberQ ..}, opts : OptionsPattern[]] := 
  Block[{data = dataArg, knots = Sort[knotsArg], yMedian = 0, yFactor = 1, yShift = 0, bvars, n = Dimensions[dataArg][[1]], pfuncs, c, t, qrSolutions, mat},

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
     , {q, qs}];
   
   If[yMedian == 0 && yFactor == 1,
    Table[ With[{f = pfuncs[[All, 1]].qrSolutions[[i, 1 ;; Length[pfuncs]]]}, f &], {i, 1, Length[qs]}],
    (*ELSE*)
    Map[Function[{ws}, With[{f = yFactor ((pfuncs[[All, 1]].ws) - yShift) + yMedian}, f &]], qrSolutions[[All, 1 ;; Length[pfuncs]]]]
   ]
  ];

Clear[MinimizeSplineQuantileRegression]
MinimizeSplineQuantileRegression[methodFunc_, data_?MatrixQ, npieces_Integer, order_Integer, qs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
  MinimizeSplineQuantileRegression[methodFunc, data, Rescale[Range[0, 1, 1/npieces], {0, 1}, {Min[data[[All, 1]]], Max[data[[All, 1]]]}], order, qs, opts];
MinimizeSplineQuantileRegression[methodFunc_, dataArg_?MatrixQ, knotsArg : {_?NumberQ ..}, order_Integer, qs : {_?NumberQ ..}, opts : OptionsPattern[]] := 
  Block[{data = dataArg, knots = Sort[knotsArg], bvars, n = Dimensions[dataArg][[1]], pfuncs, b, c, t, Tilted, QRModel, qrSolutions, minFunc},

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
      , {q, qs}];
 
   Table[ With[{f = pfuncs[[All, 1]].(bvars /. qrSolutions[[i, 2]])}, f &], {i, 1, Length[qs]}]
  ] /; order > 0;


	   (**************************************************************)
	   (* QuantileEnvelope                                           *)
	   (**************************************************************)
	   
QuantileEnvelope::qenargs = "Three arguments are expected, two column data matrix, quantiles, and a number of curve points.";
QuantileEnvelope::qemat = "The first argument is expected to be a numeric two column data matrix.";
QuantileEnvelope::qeqs = "The second argument is expected to be a number or a list of numbers between 0 and 1.";
QuantileEnvelope::qen = "The third argument is expected to be an integer greater than 2.";

Clear[QuantileEnvelope]
Options[QuantileEnvelope] = {"Tangents" -> True};
QuantileEnvelope[data_, qs_, n_, opts : OptionsPattern[]] :=
  Block[{},
   If[! MatrixQ[data, NumberQ],
    Message[QuantileEnvelope::qemat];
    Return[{}]
   ];
   If[! (TrueQ[ VectorQ[qs, NumberQ] && Apply[And, Map[0 <= # <= 1 &, qs]]] || TrueQ[NumberQ[qs] && (0 <= qs <= 1)]),
    Message[QuantileEnvelope::qeqs];
    Return[{}]
   ];
   If[! TrueQ[IntegerQ[n] && (n > 2)],
    Message[QuantileEnvelope::qen];
    Return[{}]
   ];
   QuantileEnvelopeSimple[data, qs, n, opts]
  ];

Clear[QuantileEnvelopeSimple]
Options[QuantileEnvelopeSimple] = Options[QuantileEnvelope];
QuantileEnvelopeSimple[data_?MatrixQ, q_?NumberQ, n_Integer, opts : OptionsPattern[]] := QuantileEnvelopeSimple[data, {q}, n, opts];
QuantileEnvelopeSimple[dataArg_?MatrixQ, qs : {_?NumberQ ..}, n_Integer, opts : OptionsPattern[]] :=
  Block[{data = dataArg, center, scale, rmat, rmats, qfuncs, x1, x2, y1, rqfuncs, intPoints, t, tangentsQ},
   
   (* Option values *)   
   tangentsQ = TrueQ[OptionValue[QuantileEnvelopeSimple, "Tangents"]];
   
   (* Standardize *)
   center = Mean /@ Transpose[data];
   scale = InterquartileRange /@ Transpose[data];
   data = Map[(# - center)/scale &, data];
   
   (* Rotation matrices *)
   rmat = N[RotationMatrix[2 \[Pi]/n]];
   rmats = NestList[rmat.# &, rmat, n - 1];
   qfuncs = Transpose[ Map[Function[{m}, Quantile[(m.Transpose[data])[[2]], qs]], rmats]];
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
   
   intPoints = Map[(#*scale + center) &, intPoints, {2}];
   intPoints
  ];
	   
End[]

EndPackage[]