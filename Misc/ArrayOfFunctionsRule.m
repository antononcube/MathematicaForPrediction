(*
    Array of functions numerical integration rule Mathematica Package
    Copyright (C) 2016  Anton Antonov

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

(* :Title: ArrayOfFunctionsRule *)
(* :Context: ArrayOfFunctionsRule` *)
(* :Author: Anton Antonov *)
(* :Date: 2016-09-10 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 antonov *)
(* :Keywords: *)
(* :Discussion: *)
(*

  When given an array of integrands NIntegrate is run separately over each array element. That is not
  necessary though -- the core NIntegrate integration strategies can work with any integrands as long as
  the error estimates are real numbers.

  The motivation for implementing ArrayOfFunctionsRule is to provide a significant speed-up for integrands
  that are arrays of functions. That is achived by evaluating all functions with the same integration rule
  abscissas and weights.

  -------
  Example
  -------

  Create a matrix of functions:

    funcsExpr = {Sqrt[x], Sin[x], 1/(1 + x), x^3};
    funcsExpr = Table[i*funcsExpr, {i, 3}];
    funcs = Map[Function[{fx}, Function[Evaluate[fx /. x -> #]]], funcsExpr, {2}]


  Numerically integrate with ArrayOfFunctionsRule:

    res =
      NIntegrate[1, {x, 0, 2},
        Method -> {"GlobalAdaptive", "SingularityHandler" -> None,
          Method -> {ArrayOfFunctionsRule, "Functions" -> funcs}}]

    (* {{1.88562, 1.41615, 1.09861, 4.}, {3.77124, 2.83229, 2.19722, 8.}, {5.65685, 4.24844, 3.29584, 12.}} *)


  Compare with the standard NIntegrate

    res0 = NIntegrate[funcsExpr, {x, 0, 2}]

    (* {{1.88562, 1.41615, 1.09861, 4.}, {3.77124, 2.83229, 2.19722, 8.}, {5.65685, 4.24844, 3.29584, 12.}} *)

    Norm[res - res0, 2]

    (* 7.17894*10^-7 *)

  Note that the rule has to be used with a strategy specification that has the option

   "SingularityHandler" -> None

  and that the rule does not perform correctly over ranges with infinity.

TODO:

  1. Re-design signatures.
  2. Implement multi-dimensional integration.
  3. Better error or wrong specifications handling.

  This file was created using Mathematica Plugin for IntelliJ IDEA.

  Anton Antonov
  2016-09-10

*)

BeginPackage["ArrayOfFunctionsRule`"]

ArrayOfFunctionsRule::usage = "An NIntegrate numerical integration rule for integrands that are arrays of functions."

Begin["`Private`"]

Clear[ArrayOfFunctionsRule];
Options[ArrayOfFunctionsRule] = {"Method" -> "GaussKronrodRule",
  "Functions" -> {},
  "ErrorsNormFunction" -> (Norm[#, Infinity] &)};

ArrayOfFunctionsRuleProperties = Part[Options[ArrayOfFunctionsRule], All, 1];

ArrayOfFunctionsRule /:
    NIntegrate`InitializeIntegrationRule[ArrayOfFunctionsRule, nfs_, ranges_, ruleOpts_, allOpts_] :=

    Module[{t, methodSpec, funcsArr, errNormFunc, pos, absc, weights,
      errweights, x, vars, funcsExpr, nf},

      t = NIntegrate`GetMethodOptionValues[ArrayOfFunctionsRule,
        ArrayOfFunctionsRuleProperties, ruleOpts];
      If[t === $Failed, Return[$Failed]];
      {methodSpec, funcsArr, errNormFunc} = t;

      t = NIntegrate`MOptionValue[methodSpec, nfs, ranges, allOpts];

      If[t === $Failed, Return[$Failed]];

      vars = {x};
      funcsExpr = Map[#[x] &, funcsArr, {Length@Dimensions[funcsArr]}];
      nf = Experimental`CreateNumericalFunction[{#, {}} & /@ vars, funcsExpr, Dimensions[funcsExpr], _Real & /@ vars];

      ArrayOfFunctionsRule[t, funcsArr, errNormFunc, nf]
    ];


ArrayOfFunctionsRule[methodRule_, funcsVec_, errNormFunc_, nf_]["ApproximateIntegral"[region_]] :=
    Block[{a, b, currentRule, integrals, errors, absc, weights, errweights, res},

      {absc, weights, errweights} = methodRule[[1]];

      {a, b} = region["Boundaries"][[1]];
      currentRule = region["GetRule"[]];
      region["SetRule"[methodRule]];
      region["SetIntegrand"[nf]];
      res = region["ApplyRule"];
      region["SetRule"[currentRule]];
      {integrals, errors} = res[[1 ;; 2]];
      (*If[k < 3,*)
        (*k++;*)
        (*Print[region["Properties"]];*)
        (*Print[region["GetSamplingPoints"[]]];*)
        (*Print[region["EvaluateTransformedIntegrand"[#]] & /@ absc];*)
        (*Print[region["Integrand"]];*)
        (*Print[region["Boundaries"]];*)
        (*Print[region["OriginalBoundaries"]];*)
      (*];*)
      {integrals, errNormFunc[errors], 1}
    ];
End[] (* `Private` *)

EndPackage[]