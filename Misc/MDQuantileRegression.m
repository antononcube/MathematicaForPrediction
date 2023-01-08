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
(* :Author: Anton Antonov *)
(* :Date: 2023-01-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.1 *)
(* :Copyright: (c) 2023 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["MDQuantileRegression`"];
(* Exported symbols added here with SymbolName::usage *)

BSplineFunctionsBasis::usage = "BSplineFunctionsBasis[data_, knots_, order_]";

MDQuantileRegression::usage = "MDQuantileRegression[data_, knots_, order_, probs, opts___]";

Begin["`Private`"];

End[]; (* `Private` *)

(************************************************************)
(* Make basis functions                                     *)
(************************************************************)

Clear[BSplineFunctionsBasis];

BSplineFunctionsBasis::nargs = "The first argument is expected to be a matrix; \
the second argument is expected to be a positive integer or a list of positive integers;
the third argument is expected to be one List of Association.";

BSplineFunctionsBasis::nfmt = "The third argument is expected to be one of List or Association. (Continuing with List.)";

Options[BSplineFunctionsBasis] = Options[BSplineFunction];

BSplineFunctionsBasis[data_?MatrixQ, nPieces_Integer, format_?AtomQ, opts : OptionsPattern[]] :=
    BSplineFunctionsBasis[data, Rescale[Range[0, 1, 1 / nPieces], {0, 1}, {Min[data[[All, 1]]], Max[data[[All, 1]]]}], format, opts] /; Dimensions[data][[2]] == 2;

BSplineFunctionsBasis[data_?MatrixQ, knots_List, format_?AtomQ, opts : OptionsPattern[]] :=
    Block[{},
      Echo["Not implemented yet.", "BSplineFunctionsBasis::"];
      $Failed
    ] /; Dimensions[data][[2]] == 2;

BSplineFunctionsBasis[data_?MatrixQ, nKnots_Integer, format_?AtomQ, opts : OptionsPattern[]] :=
    BSplineFunctionsBasis[data, Table[nKnots, Dimensions[data][[2]] - 1], format, opts] /; Dimensions[data][[2]] > 2;

BSplineFunctionsBasis[data_?MatrixQ, nKnotsArg : {_Integer..}, format_?AtomQ, opts : OptionsPattern[]] :=
    Block[{nKnots = nKnotsArg, m, n, lsMinMaxes, cpts0, lsIndexes,
      lsSlotArgs, lsSlotArgsRescaled, lsBasis, lsKeys},

      (* Get dimensions *)
      {m, n} = Dimensions[data];

      (* Gin min-max for each column *)
      lsMinMaxes = Table[ MinMax[data[[All, i]]], {i, 1, n - 1}];

      (* Process knots argument *)
      If[ Length[nKnots] < n - 1,
        nKnots = Flatten[Table[nKnots, n - 1]][[ 1 ;; n - 1 ]]
      ];

      (* Prepare control points *)
      cpts0 = ConstantArray[{0}, nKnots];

      (* Cartesian product of indexes *)
      lsIndexes = Flatten[Outer[List, Sequence @@ Map[Range, nKnots]], Length[nKnots] - 1];

      (* Make basis functions slots *)
      lsSlotArgs = Table[Slot[i], {i, Length[lsMinMaxes]}];
      lsSlotArgsRescaled = Table[Rescale[Slot[i], lsMinMaxes[[i]], {0, 1}], {i, Length[lsMinMaxes]}];

      (* Make basis functions control points *)
      lsBasis = Map[ReplacePart[cpts0, # -> {1}]&, lsIndexes];

      (* Make basis functions *)
      lsBasis = With[{f = BSplineFunction[#, Sequence @@ FilterRules[{opts}, Options[BSplineFunction]]], args = lsSlotArgsRescaled}, f @@ args&]& /@ lsBasis;

      (* Result *)
      (* <| "Unitary" -> lsBasis, "Rescaled" -> lsBasisRescaled |> *)
      Which[
        MemberQ[{List, "List"}, format], lsBasis,

        MemberQ[{Association, "Association"}, format],
        lsKeys = Transpose @ MapThread[Rescale[#1, {1, #2}, #3]&, {Transpose[lsIndexes], nKnots, lsMinMaxes}];
        AssociationThread[lsKeys, lsBasis],

        True,
        Message[BSplineFunctionsBasis::nfmt];
        lsBasis
      ]
    ] /; Dimensions[data][[2]] > 2;

BSplineFunctionsBasis[___] := $Failed;

(************************************************************)
(* MDQuantileRegression                                     *)
(************************************************************)

Clear[MDQuantileRegression];

MDQuantileRegression::"knord" = "The specified knots `1` and interpolation order `2` produce no B-Spline basis functions. \
The expression n - i - 2 should be non-negative, where n is the number of knots and i is the interpolation order.";

MDQuantileRegression::"zerob" = "The specified knots `1` and interpolation order `2` produced a list of zeroes instead of a list of B-Spline basis functions.";

MDQuantileRegression[dataArg_?MatrixQ, knotsSpec : ( _Integer | {_Integer..} ), splineDegree_Integer, probs : {_?NumberQ ..}, opts : OptionsPattern[]] :=
    Block[{data = dataArg, yMedian = 0, yFactor = 1, yShift = 0, n = Dimensions[dataArg][[1]],
      pFuncs, c, t, qrSolutions, mat},

      (* Transform data *)
      If[Min[data[[All, -1]]] < 0,
        yMedian = Median[data[[All, -1]]];
        yFactor = InterquartileRange[data[[All, -1]]];
        data[[All, -1]] = Standardize[data[[All, -1]], Median, InterquartileRange];
        yShift = Abs[Min[data[[All, -1]]]];
        data[[All, -1]] = data[[All, -1]] + yShift ;
      ];

      (* B-spline basis functions *)
      pFuncs = BSplineFunctionsBasis[data, knotsSpec, List, "SplineDegree" -> splineDegree];
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