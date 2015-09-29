(*
    QuantileRegressionForLocalExtrema Mathematica package
    Copyright (C) 2015  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2015 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: QuantileRegressionForLocalExtrema *)
(* :Author: Anton Antonov *)
(* :Date: 2015-09-27 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 10.2 *)
(* :Copyright: (c) 2015 Anton Antonov *)
(* :Keywords: Quantile Regression, local maxima, local minima, local extrema, model fitting, regression *)
(* :Discussion:

The implemented algorithms have the following steps:

1. Fit a polynomial/curve through the data (using LinearModelFit or QuantileRegression).
2. Find the local extrema of the fitted polynomial. Call them fit estimated extrema.
3. Around each of the fit estimated extrema find the most extreme point in
    the data using nearest neighbors search (by using Nearest).

The algorithm for LFMFindExtrema was proposed and initially implemented by Leonid Shifrin.
I took it from Mathematica StackExchange, see:

[1] Mathematica StackExchange discussion. "Finding Local Minima / Maxima in Noisy Data",
   URL: http://mathematica.stackexchange.com/questions/23828/finding-local-minima-maxima-in-noisy-data/ .

The implementation of LFMFindExtrema is included for comparison with QRFindExtrema.

The implementation of QRFindExtrema uses the Quantile Regression package from my project
MathematicaForPrediction at GitHub, see:

[2] Anton Antonov, Quantile regression Mathematica package, source code at GitHub,
https://github.com/antononcube/MathematicaForPrediction, package QuantileRegression.m, (2013).

The algorithm QRFindExtrema has as parameters: the data, number of B-spline knots, interpolation order, and quantiles.
It also takes an option should the Nearest functions for finding the extrema be constructed using
all data points or just the outliers (the points outside of the found regression quantiles).

QRFindExtrema returns a list of two elements:
the first element is a list of fitted regression quantiles functions,
the second element is a list of lists with local minima and local maxima.

QRFindExtrema uses Reduce instead of NSolve because Reduce deals better with the Piecewise functions found by
QuantileRegression.

It is important that QRFindExtrema can use two curves for finding the local extrema:
one for local minima, and one for local maxima.

I am using the option Method -> {LinearProgramming, Method -> "CLP"} for QuantileRegression because with
Method -> {LinearProgramming, Method -> Automatic} Mathematica crashes sometimes for large data sets.

See the document "Finding local extrema in noisy data using Quantile Regression" in
https://github.com/antononcube/MathematicaForPrediction/tree/master/Documentation
for extened discussion with experimental results.

This file was created using Mathematica Plugin for IntelliJ IDEA.

Anton Antonov
2015-09-27

*)


Clear[LMFFindExtrema]
LMFFindExtrema[points_List, fitOrder_Integer, around_Integer: 5,
  fitFunctionType_: "Polynomial"] :=

    Module[{fit, fn, extrema, x, signs, extPoints},
    (* Step 1 *)
      Which[
        fitFunctionType == "ChebyshevT",
        fit =
            LinearModelFit[points, ChebyshevT[#, x] & /@ Range[0, fitOrder],
              x],
        True,(*fitFunctionType=="Polynomial",*)

        fit = LinearModelFit[points, x^Range[0, fitOrder], x]
      ];
      fn = fit["Function"];
      (* Step 2 *)
      extrema = NSolve[fn'[x] == 0, x, Reals];
      signs = Sign[fn''[x] /. extrema];
      (* Step 3 *)

      extPoints =
          MapThread[
            First@SortBy[Nearest[points, #, around], #2] &, {{x, fn[x]} /.
              extrema, signs /. {1 -> Last, -1 -> (-Last[#] &)}}];
      {fn, Map[Pick[extPoints, signs, #] &, {1, -1}]}
    ];

Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/QuantileRegression.m"]

Clear[QRFindExtrema]
Options[QRFindExtrema] = { "NearestWithOutliers" -> False };
QRFindExtrema[points_List, nknots_Integer, nfitOrder_Integer, around_Integer, quantiles_: {0.1,0.9}, opts:OptionsPattern[] ] :=
    Module[{fit, fn, extrema1, extrema2, minima, maxima, x, signs1, signs2, extremaPoints, nearestByOutliersQ, nfMax, nfMin },

      nearestByOutliersQ = OptionValue[ "NearestWithOutliers" ];

      (* Step 1 *)
      fn = Simplify[ QuantileRegression[points, nknots, quantiles, InterpolationOrder -> nfitOrder, Method -> {LinearProgramming, Method -> "CLP"}]];

      (* Step 2 *)
      extrema1 = Reduce[fn[[1]]'[x] == 0, x, Reals];
      extrema1 = Cases[{ToRules[extrema1]}, _Rule, Infinity ];
      signs1 = Sign[fn[[1]]''[#] & /@ extrema1[[All, 2]]];
      extrema2 = Reduce[fn[[-1]]'[x] == 0, x, Reals];
      extrema2 = Cases[{ToRules[extrema2]}, _Rule, Infinity ];
      signs2 = Sign[fn[[-1]]''[#] & /@ extrema2[[All, 2]]];

      (* Step 3 *)
      minima =
          Map[{#, fn[[1]][#]} &,
              Pick[extrema1[[All, 2]], # > 0 & /@ signs1]];
      maxima =
            Map[{#, fn[[-1]][#]} &,
              Pick[extrema2[[All, 2]], # < 0 & /@ signs2]];

      If[ nearestByOutliersQ,
        nfMin = Nearest[ Select[ points, #[[2]] <= fn[[1]][#[[1]]]& ] ];
        nfMax = Nearest[ Select[ points, #[[2]] >= fn[[-1]][#[[1]]]& ] ];,
        (* ELSE *)
        nfMin = Nearest[points];
        nfMax = nfMin;
      ];

      extremaPoints = {
        Map[ First@SortBy[ nfMin[#, around], #[[-1]]& ]&, minima ],
        Map[ First@SortBy[ nfMax[#, around], -#[[-1]]& ]&, maxima ]
      };
      {fn, extremaPoints }
    ];
