(*
    Quantile regression Mathematica package
    Copyright (C) 2013  Anton Antonov

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

(* Version 1.0 *)
(* 
  This package has only one function, QuantileRegression. The arguments and the result of QuantileRegression are very similar to those of the function Fit. In order to find the quantile functions that fit through the data QuantileRegression can use both LinearProgramming or Minimize through the Method option, e.g. Method->Minimize or Method->{LinearProgramming, Method->"Simplex", Tolerance->10^-6.0} . Using Minimize can be very slow for large data sets -- that method is included for didactic purposes.

  The linear programming implementation is based on the (non-dual) formulation in the article 
 
  Roger Koenker, Gilbert Bassett, Jr., "Regression Quantiles", Econometrica, Vol. 46, No. 1. (Jan., 1978), pp. 33-50.

*)

BeginPackage["QuantileRegression`"]

QuantileRegression::usage = "QuantileRegression[data,funs,var,qs] finds the regression quantiles corresponding to the quantiles qs for a list of data as linear combinations of the functions funs of the variable var."

Begin["`Private`"]

QuantileRegression::"nmat" = "The first argument is expected to be a matrix of numbers with two columns.";

QuantileRegression::"fvlen" = "The second argument is expected to be list of functions to be fitted with at least one element.";

QuantileRegression::"nvar" = "The third argument is expected to be a symbol.";

QuantileRegression::"nqntls" = "The fourth argument is expected to be a list of numbers representing quantiles.";

QuantileRegression::"nmeth" = "The value of the method option is expected to be LinearProgramming, Minimize, or a list with LinearProgramming or Minimize as a first element.";

QuantileRegression::"mmslow" = "With the method Minimize the computations can be very slow for large data sets.";

Clear[QuantileRegression]
Options[QuantileRegression] = {Method -> LinearProgramming};
QuantileRegression[data_, funcs_, var_, qs_, opts : OptionsPattern[]] :=
  Block[{mOptVal},
   (*This check should not be applied because the first function can be a constant.*)
   (*!Apply[And,Map[!FreeQ[#,var]&,funcs]],Message[QuantileRegression::\"fvfree\"],*)
   Which[
    ! ( MatrixQ[data] && Dimensions[data][[2]] >= 2 ),
    Message[QuantileRegression::"nmat"]; Return[{}],
    Length[funcs] < 1,
    Message[QuantileRegression::"fvlen"]; Return[{}],
    Head[var] =!= Symbol,
    Message[QuantileRegression::"nvar"]; Return[{}],
    ! VectorQ[qs, NumericQ[#] && 0 <= # <= 1 &],
    Message[QuantileRegression::"nqntls"]; Return[{}]
   ];
   mOptVal = OptionValue[QuantileRegression, Method];
   Which[
    mOptVal === LinearProgramming,
    LPQuantileRegression[data, funcs, var, qs],
    ListQ[mOptVal] && mOptVal[[1]] === LinearProgramming,
    LPQuantileRegression[data, funcs, var, qs, Rest[mOptVal]],
    mOptVal === Minimize,
    MinimizeQuantileRegression[data, funcs, var, qs],
    ListQ[mOptVal] && mOptVal[[1]] === Minimize,
    MinimizeQuantileRegression[data, funcs, var, qs, Rest[mOptVal]],
    True,
    Message[QuantileRegression::"nmeth"]; Return[{}]
   ]
  ];

Clear[LPQuantileRegression]
LPQuantileRegression[dataArg_?MatrixQ, funcs_, var_Symbol, qs : {_?NumberQ ..}, opts : OptionsPattern[]] := 
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

Clear[MinimizeQuantileRegression]
MinimizeQuantileRegression[data_?MatrixQ, funcs_, var_Symbol, qs : {_?NumberQ ..}, opts : OptionsPattern[]] :=  
  Block[{minFunc, Tilted, QRModel, b, bvars, qrSolutions},
   
   If[Length[data] > 300,
    Message[QuantileRegression::"mmslow"]
   ];
   
   bvars = Array[b, Length[funcs]];
   
   Tilted[t_?NumberQ, x_] := Piecewise[{{(t - 1) x, x < 0}, {t x, x >= 0}}] /; t <= 1;
   QRModel[x_] := Evaluate[(bvars.funcs) /. var -> x];
   
   qrSolutions =
    Table[
     minFunc = Total[(Tilted[q, #1[[2]] - QRModel[#1[[1]]]] &) /@ data];
     Minimize[{minFunc}, bvars, opts]
     , {q, qs}];

   Map[funcs.# &, qrSolutions[[All, 2, All, 2]]]
  ];


End[]

EndPackage[]