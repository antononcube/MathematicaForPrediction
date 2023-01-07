(*
    Multi-Dimensional Quantile Regression Mathematica package
    Copyright (C) 2023  Anton Antonov

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
	  ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
	  Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2023 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)
(* :Title: MDQuantileRegression *)
(* :Context: MDQuantileRegression` *)
(* :Author: antonov *)
(* :Date: 2023-01-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.1 *)
(* :Copyright: (c) 2023 antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["MDQuantileRegression`"];
(* Exported symbols added here with SymbolName::usage *)

QRBasisFunctions::usage = "QRBasisFunctions[data_, knots_, order_]";

MDQuantileRegression::usage = "MDQuantileRegression[data_, knots_, order_, probs, opts___]";

Begin["`Private`"];

End[]; (* `Private` *)

(************************************************************)
(* Make basis functions                                     *)
(************************************************************)

Clear[QRBasisFunctions];

QRBasisFunctions::nargs = "The first argument is expected to be a matrix; \
the second argument is expected to be a positive integer or a list of positive integers; \
the third argument is expected to be an integer.";

QRBasisFunctions[data_?MatrixQ, nPieces_Integer, order_Integer, opts : OptionsPattern[]] :=
    QRBasisFunctions[data, Rescale[Range[0, 1, 1 / nPieces], {0, 1}, {Min[data[[All, 1]]], Max[data[[All, 1]]]}], order, opts] /; Dimensions[data][[2]] == 2;

QRBasisFunctions[data_?MatrixQ, knots_List, order_Integer, opts : OptionsPattern[]] :=
    Block[{},
      Echo["Not implemented yet.", "QRBasisFunctions::"];
      $Failed
    ] /; Dimensions[data][[2]] == 2;

QRBasisFunctions[data_?MatrixQ, nKnots_Integer, order_Integer, opts : OptionsPattern[]] :=
    QRBasisFunctions[data, Table[nKnots, Dimensions[data][[2]] - 1], order, opts] /; Dimensions[data][[2]] > 2;

QRBasisFunctions[data_?MatrixQ, nKnotsArg : {_Integer..}, order_Integer, opts : OptionsPattern[]] :=
    Block[{nKnots = nKnotsArg, m, n, lsMinMaxes, cpts0, lsInds,
      lsSlotArgs, lsSlotArgsRescaled, lsBasis, lsBasisRescaled},

      (* Get dimensions *)
      {m, n} = Dimensions[data];

      (* Gin min-max for each column *)
      lsMinMaxes = Table[ MinMax[data[[All, i]]], {i, 1, n - 1}];

      (* Process knots argument *)
      If[ Length[nKnots] < n - 1, nKnots = Flatten[Table[nKnots, n - 1]][[ 1 ;; n - 1 ]]];

      (* Prepare control points *)
      cpts0 = ConstantArray[{0}, nKnots];

      (* Cartesian product of indexes *)
      lsInds = Flatten[Outer[List, Sequence @@ Map[Range, nKnots]], Length[nKnots] - 1];

      (* Make basis functions slots *)
      lsSlotArgs = Table[Slot[i], {i, Length[lsMinMaxes]}];
      lsSlotArgsRescaled = Table[Rescale[Slot[i], lsMinMaxes[[i]], {0, 1}], {i, Length[lsMinMaxes]}];

      (* Make basis functions control points *)
      lsBasis = Map[ReplacePart[cpts0, # -> {1}]&, lsInds];

      (* Make basis functions *)
      lsBasis = With[{f = BSplineFunction[#, Sequence @@ FilterRules[{opts}, Options[BSplineFunction]]], args = lsSlotArgsRescaled}, f @@ args&]& /@ lsBasis;

      (* Result *)
      (* <| "Unitary" -> lsBasis, "Rescaled" -> lsBasisRescaled |> *)
      lsBasis
    ] /; Dimensions[data][[2]] > 2;

QRBasisFunctions[___] := $Failed;

(************************************************************)
(* MDQuantileRegression                                     *)
(************************************************************)

Clear[MDQuantileRegression];

MDQuantileRegression::"knord" = "The specified knots `1` and interpolation order `2` produce no B-Spline basis functions. \
The expression n - i - 2 should be non-negative, where n is the number of knots and i is the interpolation order.";

MDQuantileRegression::"zerob" = "The specified knots `1` and interpolation order `2` produced a list of zeroes instead of a list of B-Spline basis functions.";

MDQuantileRegression[dataArg_?MatrixQ, knotsSpec : ( _Integer | {_Integer..} ), order_Integer, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{data = dataArg, yMedian = 0, yFactor = 1, yShift = 0, n = Dimensions[dataArg][[1]],
      aRes, pFuncs, pFuncsRescaled, c, t, qrSolutions, dataRescaled, mat},

      (* Transform data *)
      If[Min[data[[All, -1]]] < 0,
        yMedian = Median[data[[All, -1]]];
        yFactor = InterquartileRange[data[[All, -1]]];
        data[[All, -1]] = Standardize[data[[All, -1]], Median, InterquartileRange];
        yShift = Abs[Min[data[[All, -1]]]];
        data[[All, -1]] = data[[All, -1]] + yShift ;
      ];

      (* B-spline basis functions *)
      pFuncs = QRBasisFunctions[data, knotsSpec, order];
(*      pFuncs = aRes["Unitary"];*)
(*      pFuncsRescaled = aRes["Rescaled"];*)

      (* Create the conditions matrix *)
(*      dataRescaled = Transpose[ Clip[Rescale[#, MinMax[#], {0, 1}], {0, 1}] & /@ Transpose[ data[[All, 1 ;; -2]] ]];*)
      mat = Transpose @ Map[ Flatten[# @@@ data[[All, 1 ;; -2]]]&, pFuncs];
(*      Print[Dimensions[mat]];*)
(*      Print[MatrixQ[mat, NumericQ]];*)
(*      Print[mat];*)
      mat = MapThread[Join, {mat, IdentityMatrix[n], -IdentityMatrix[n]}];

      mat = SparseArray[mat];

      (* Find the regression quantiles *)
      qrSolutions =
          Table[
            c = Join[ConstantArray[0, Length[pFuncs]], ConstantArray[1, n] * q, ConstantArray[1, n] * (1 - q)];
            t = LinearProgramming[c, mat, Transpose[{data[[All, -1]], ConstantArray[0, n]}], DeleteCases[{opts}, InterpolationOrder -> _]];
            If[! (VectorQ[t, NumberQ] && Length[t] > Length[pFuncs]), ConstantArray[0, Length[pFuncs]], t]
            , {q, probs}];

      If[yMedian == 0 && yFactor == 1,
        Table[ With[{f = pFuncs[[All, 1]] . qrSolutions[[i, 1 ;; Length[pFuncs]]]}, f &], {i, 1, Length[probs]}],
        (*ELSE*)
        Map[Function[{ws}, With[{f = yFactor * ((pFuncs[[All, 1]].ws) - yShift) + yMedian}, f &]], qrSolutions[[All, 1 ;; Length[pFuncs]]]]
      ]
    ];

EndPackage[]