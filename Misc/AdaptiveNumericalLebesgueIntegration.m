(*
    Adaptive Numerical Lebesgue Integration Mathematica Package
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

(* :Title: AdaptiveNumericalLebesgueIntegration *)
(* :Context: AdaptiveNumericalLebesgueIntegration` *)
(* :Author: Anton Antonov *)
(* :Date: 2016-06-19 *)

(* :Package Version: 0.9 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 Anton Antonov *)
(* :Keywords: NIntegrate, Lebesgue integral, set measure, integration strategy, dimension reduction *)
(* :Discussion:

  ## Some theory

    Brief theory for the Lebesgue integration algorithms that are implemented.
    (Should be rendered with Markdown+LaTeX editors/tools. I used Emacs+Pandoc. )

    Consider the non-negative boundable measurable function $f$:

    $$ y=f(x), f(x) \geq 0, x \in \Omega .$$

    We denote by $\mu(y)$ the measure for the points in $\Omega$ for which $f(x)>=y$, i.e.

    $$ \mu(y) := \left| \{ x: x\in \Omega \land f(x) \geq y\} \right| . $$

    The Lebesgue integral of $f(x)$ over $\Omega$ can be be defined as:

    $$ \int_{\Omega } f (x) dx = y_0 \mu (y_0) + \lim_{n \to \infty ,\max
         \left(y_i-y_{i-1}\right)\to 0} \sum_{i=1}^n \mu(y_i)(y_i-y_{i-1}) .$$

    Further, we can write the last formula as

    $$ \int_{\Omega } f(x) dx = y_0 \mu(y_0) + \int_{y_0}^{y_n}\mu(y) dy.$$

    The restriction $f(x)>=0$ can be handled by defining the following functions $f_1$ and $f_2$ :

    $$ f_1(x) := \frac{1}{2} (\left| f(x) \right| + f(x) ), $$

    $$ f_2(x) := \frac{1}{2} (\left| f(x) \right| - f(x) ), $$

    and using the formula

    $$ \int_{\Omega}f(x) dx= \int_{\Omega} f_1(x) dx - \int_{\Omega}f_2(x) dx. $$

  ## Usage as integration strategy

  ### 1D integration

    NIntegrate[Sqrt[x], {x, 0, 2}, Method -> LebesgueIntegrationStrategy]

    NIntegrate[Sqrt[x], {x, 0, 2}, Method -> {LebesgueIntegrationStrategy, "Points" -> 2000,
            "PointGenerator" -> "Sobol", "Partitioning" -> "VoronoiMesh"},
             PrecisionGoal -> 3]


  ### 2D integration with evaluation monitor

    res = Reap@
       NIntegrate[Sin[x + y], {x, -1, 2}, {y, -1, 1},
          Method -> {LebesgueIntegrationStrategy, "Points" -> 10000,
          "PointGenerator" -> "Sobol", "Partitioning" -> "RegularGrid",
          "LebesgueIntegralVariableSymbol" -> fval},
          EvaluationMonitor :> {Sow[fval]},
          PrecisionGoal -> 2, MaxRecursion -> 5];
    res = DeleteCases[res, fval, Infinity]


  ### nD

    NIntegrate[x1 + x2 + x3 + Sqrt[x4] + x5, {x1, 0, 1}, {x2, 0, 2}, {x3, 0, 3}, {x4, 0, 5}, {x5, 0, 10},
      Method -> {LebesgueIntegration}]
    (* 2847.06 *)


  ### Handling of variable ranges

    Explicit nesting in "UnitCubeRescaling" has to be done:

    NIntegrate[1/(x + y)^2, {x, 1, 2}, {y, x, 12},
      Method -> {"UnitCubeRescaling",
          Method -> {LebesgueIntegrationStrategy, "PointGenerator" -> Random}},
      PrecisionGoal -> 3]


  ## Usage as an integration rule

  ### 1D

    NIntegrate[Sqrt[x], {x, 0, 2}, Method -> LebesgueIntegrationRule, PrecisionGoal -> 3]

    NIntegrate[Sqrt[x], {x, 0, 2}, Method -> GridLebesgueIntegrationRule, PrecisionGoal -> 3]


  ### 2D

    NIntegrate[Sin[x + y], {x, 1, 2}, {y, -1, 1}, Method -> {"GlobalAdaptive",
      Method -> {LebesgueIntegrationRule, "Points" -> 3000,
      Method -> "ClenshawCurtisRule"}, "SingularityHandler" -> None},
      PrecisionGoal -> 4]

    NIntegrate[Sin[x + y], {x, 1, 2}, {y, -1, 1}, Method -> {"GlobalAdaptive",
      Method -> {GridLebesgueIntegrationRule, "Points" -> 300, "GridSizes"->5,
      Method -> "ClenshawCurtisRule"}, "SingularityHandler" -> None},
      PrecisionGoal -> 4]

  ### nD
    NIntegrate[ x1 + x2 + x3 + Sqrt[x4] + x5, {x1, 0, 1}, {x2, 0, 2}, {x3, 0, 3}, {x4, 0, 5}, {x5, 0, 10},
      Method -> LebesgueIntegrationRule, PrecisionGoal -> 2]
    (* 2846.31 *)


  ## References

    [1] B. L. Burrows, A new approach to numerical integration, 1. Inst. Math. Applics., 26(1980), 151-173.

    [2] T. He, Dimensionality Reducing Expansion of Multivariate Integration, 2001, Birkhauser Boston.
        ISBN-13:978-1-4612-7414-8 .

    [3] William H. Press, Saul A. Teukolsky, William T. Vetterling, and Brian P. Flannery.
        Numerical Recipes in C (2nd Ed.): The Art of Scientific Computing, 1992,
        Cambridge University Press, New York, NY, USA.
        URL: http://www.nrbook.com/a/bookcpdf.php
        Chapter "7.8 Adaptive and Recursive Monte Carlo Methods" page 316.

  ## TODO

     1. [X] HIGH Proper computation of volume estimates corresponding to the sampling points for
            "Partitioning"->"RegularGrid".
            Instead of fiddling with the cell and point volumes, I implementended a separate integration rule,
            GridLebesgueIntegrationRule, that uses a regular grid over a set of random points. That rule adheres
            very closely to the algorithm descriptions in the article [1] and book [2].

     2. [X] HIGH Deterministic computation of the splitting axis for LebesgueIntegrationRule and
            GridLebesgueIntegrationRule.
            This should be same/similar as for MonteCarloRule see [3].
            Right now random selection of the axis very often produces results faster.
            It is not necessarily a good idea to apply Monte Carlo estimates for Lebesgue integration.

     3. [X] MEDIUM Tests showing functionality.

     4. [ ] LOW Messages for no Handling of variable ranges for "SymbolicProcessing"->0 .
        As in NIntegrate[f[x,y],{x,0,1},{y,x,2}] .
        Right now explicit specification of "UnitCubeRescaling" is required if "SymbolicProcessing"->0 .

     5. [ ] LOW Better handling of evaluation monitoring.

     6. [ ] LOW IntegrationMonitor handling implementation.

     7. [ ] LOW Code refactoring.
        The strategy and rule have common code, especially for initialization.
        But I do expect those algorithms to devlop into more peculiar way.
        So I am not sure refactoring needed. This package was also created in part for didactic reasons, so
        it is better the keep the defintions self-contained for easier study and/or copying.



  This file was created using Mathematica Plugin for IntelliJ IDEA.

  Anton Antonov
  2016-06-19
*)


BeginPackage["AdaptiveNumericalLebesgueIntegration`"]
(* Exported symbols added here with SymbolName::usage *)

LebesgueIntegration::usage = "Implementation of a Lebesgue integration algorithm (strategy) to be used \
as an NIntegrate method."

LebesgueIntegrationRule::usage = "Implementation of a Lebesgue integration algorithm as an integration rule \
for NIntegrate."

GridLebesgueIntegrationRule::usage = "Implementation of a Lebesgue integration algorithm as an integration rule \
that uses regular grid membership of (pseudo-)random points."

Begin["`Private`"]

(**************************************************************************)
(* Definition as an integration strategy                                  *)
(**************************************************************************)

(* Integration strategy algorithm initialization *)

Clear[LebesgueIntegration];
Options[LebesgueIntegration] = {
  "Method" -> Automatic,
  "PointGenerator" -> "Sobol",
  "Partitioning" -> Automatic,
  "RegularGridDimensions" -> Automatic,
  "Points" -> Automatic,
  "LebesgueIntegralVariableSymbol" -> Automatic,
  "SymbolicProcessing" -> 5
};

LebesgueIntegrationProperties = Options[LebesgueIntegration][[All, 1]];

LebesgueIntegration::vmesh =
"The value \"VoronoiMesh\" of the option \"Partitioning\" can be used only \
for dimension 2. Proceeding with \"RegularGrid\" instead.";

LebesgueIntegration /:
    NIntegrate`InitializeIntegrationStrategy[LebesgueIntegration, nfs_, ranges_, strOpts_, allOpts_] :=
    Block[{method, RNGenerator, npoints, regularGridStep, regionPartitioning, lebesgueIntegralVar,
      t, symbproctime},


      t = NIntegrate`GetMethodOptionValues[LebesgueIntegration, LebesgueIntegrationProperties, strOpts];

      (* Method *)
      If[t === $Failed, Return[$Failed]];
      {method, RNGenerator, regionPartitioning, regularGridStep, npoints, lebesgueIntegralVar, symbproctime} = t;

      If[TrueQ[method === Automatic], method = "GlobalAdaptive"];

      t = NIntegrate`MOptionValue[method, nfs, ranges, allOpts];
      If[t === $Failed, Return[$Failed]];

      (* Partitioning *)
      If[regionPartitioning === Automatic,
        If[Length[ranges] == 2,
          regionPartitioning = "VoronoiMesh",
          regionPartitioning = "RegularGrid"
        ];
      ];

      If[MemberQ[{"VoronoiMesh", VoronoiMesh}, regionPartitioning] && Length[ranges] != 2,
        Message[LebesgueIntegration::vmesh];
        regionPartitioning = "RegularGrid";
      ];

      If[! MemberQ[{"VoronoiMesh", VoronoiMesh, "RegularGrid"},
        regionPartitioning],
        Message[NIntegrate::moptxn, regionPartitioning, "RegionGrid", {"VoronoiMesh", "RegularGrid"}];
        Return[$Failed];
      ];


      (* Points *)
      Which[
        npoints === Automatic && Length[ranges] == 1, npoints = 1000,
        npoints === Automatic && Length[ranges] > 1, npoints = 10^4
      ];

      If[! TrueQ[IntegerQ[npoints] && npoints >= 0] ,
        Message[NIntegrate::intpm, "Points" -> npoints, 2];
        Return[$Failed];
      ];

      If[TrueQ[lebesgueIntegralVar === Automatic], lebesgueIntegralVar = f ];

      LebesgueIntegration[{method, nfs, ranges, RNGenerator, regionPartitioning,
        regularGridStep, npoints, lebesgueIntegralVar, symbproctime}]
    ];


(* Integration strategy algorithm implementation *)

Clear[EstimateMeasure]
EstimateMeasure[fval_?NumericQ, pointVals_, pointVolumes_] :=
    Block[{pinds},
      pinds = Clip[Sign[pointVals - fval], {0, 1}, {0, 1}];
      pointVolumes.pinds
    ];

LebesgueIntegration[{method_, nfs_, ranges_, RNGenerator_, regionPartitioning_,
  regularGridStep_, npoints_, lebesgueIntegralVar_, symbproctime_}]["Algorithm"[regionsArg_, opts___]] :=
    Module[{regions = regionsArg, error = Infinity, integral,
      wprec = WorkingPrecision /. opts, k = 0, dim, t, oldMinMaxPrec, points,
      pointVolumes, pointVals, pointFuncVals, pointAbsVals, integral1,
      integral2, vmesh, offset},

      (* integral dimension *)
      dim = regions[[1]]@"Dimension";

      (* Generate points *)
      If[TrueQ[RNGenerator == "Sobol" || RNGenerator == "Niederreiter"],
      (* We cannot set the working precision here.*)
        BlockRandom[
          SeedRandom[0,
            Method -> {"MKL", Method -> {RNGenerator, "Dimension" -> dim}}];
          points = RandomReal[{0, 1}, {npoints, dim}];
        ],
        (*ELSE*)
        points = RandomReal[{0, 1}, {npoints, dim}, WorkingPrecision -> wprec];
      ];

      (* Rescale to region boundaries *)
      points =
          Transpose@
              MapThread[
                Rescale[#1, {0, 1}, #2] &, {Transpose[points], regions[[1]]["Boundaries"]}];

      (* Find point volumes *)

      If[TrueQ[(regionPartitioning === VoronoiMesh ||
          regionPartitioning == "VoronoiMesh") && dim == 2],
      (* There is some problem in the VoronoiMesh 1D case, VoronoiMesh[__,{{a, b}}], so we skip it.*)

        vmesh = VoronoiMesh[points, regions[[1]]["Boundaries"]];
        pointVolumes = PropertyValue[{vmesh, dim}, MeshCellMeasure],
      (*ELSE*)
      (* Quite a shortcut, but should work well enough. To be properly implmenented... *)
        pointVolumes =
            Apply[Times, Abs[Subtract @@@ regions[[1]]["Boundaries"]]]*
                Table[1/Length[points], {Length[points]}]
      ];
      pointVolumes = N[pointVolumes, wprec];

      (* First integral calculation *)
      pointFuncVals = nfs[[1]] @@@ points;
      pointAbsVals = Abs[pointFuncVals];

      pointVals = 1/2 (pointAbsVals + pointFuncVals);

      offset = Min[pointVals]*EstimateMeasure[Min[pointVals], pointVals, pointVolumes];

      integral1 = offset +
          NIntegrate[
            EstimateMeasure[lebesgueIntegralVar, pointVals, pointVolumes],
            {lebesgueIntegralVar, Min[pointVals], Max[pointVals]}, Method -> method, opts];

      (* Second integral calculation *)
      pointVals = 1/2 (pointAbsVals - pointFuncVals);

      offset = Min[pointVals]*EstimateMeasure[Min[pointVals], pointVals, pointVolumes];

      integral2 = offset +
          NIntegrate[
            EstimateMeasure[lebesgueIntegralVar, pointVals, pointVolumes],
            {lebesgueIntegralVar, Min[pointVals], Max[pointVals]}, Method -> method, opts];

      integral1 - integral2
    ];


(**************************************************************************)
(* Definition of point-wise integration rule                                      *)
(**************************************************************************)

(* Assuming the points are in [0,1] hyper-cube *)
Clear[AxisSelectionPoints]
AxisSelectionPoints[points : {{_?NumberQ ..} ..}, fraction_?NumberQ] :=
    Block[{pinds, signs, pos, t1, t2},
      pinds = Range[Length[points]];
      Map[
        Function[{i},
          signs = Sign[points[[All, i]] - 0.5];
          pos = Pick[pinds, True*Clip[-signs, {0, 1}, {0, 1}]];
          t1 = RandomSample[pos, Max[Ceiling[Length[pos] fraction],1]];
          pos = Pick[pinds, True*Clip[signs, {0, 1}, {0, 1}]];
          t2 = RandomSample[pos, Max[Ceiling[Length[pos] fraction],1]];
          {i, t1, t2}
        ], Range[Length[points[[1]]]]]
    ] /; 0 < fraction <= 1.0;

Clear[SelectAxis]
SelectAxis[fvals : {_?NumberQ ..}, axisSplitPositions : {{_Integer, {_Integer ..}, {_Integer ..}}..}] :=
    Block[{vs},
      vs = Map[Variance[fvals[[#[[2]]]]] + Variance[fvals[[#[[3]]]]] &, axisSplitPositions];
      Position[vs, Min[vs]][[1, 1]]
    ];


Clear[LebesgueIntegrationRule]
Options[LebesgueIntegrationRule] = {
  "Method" -> "ClenshawCurtisRule",
  "PointGenerator" -> "Sobol",
  "PointwiseMeasure" -> Automatic,
  "Points" -> Automatic,
  "AxisSelector" -> Automatic
};

LebesgueIntegrationRuleProperties = Part[Options[LebesgueIntegrationRule], All, 1];

LebesgueIntegrationRule::vmesh =
    "The value \"VoronoiMesh\" of the option \"PoinwiseMeasure\" can be used only \
for dimension 2. Proceeding with \"Uniform\" instead.";

LebesgueIntegrationRule::noptval = "The value `1` given to the option `2` is not one of `3`.";

LebesgueIntegrationRule /:
    NIntegrate`InitializeIntegrationRule[LebesgueIntegrationRule, nfs_, ranges_, ruleOpts_, allOpts_] :=
    Module[{t, method, RNGenerator, pointwiseMeasure, npoints, axisSelector, axisSelectorPointsFraction,
      absc, weights, errweights, points, pointVolumes, wprec, dim, vmesh, axisSplitPositions},

      t = NIntegrate`GetMethodOptionValues[LebesgueIntegrationRule, LebesgueIntegrationRuleProperties, ruleOpts];

      If[t === $Failed, Return[$Failed]];
      {method, RNGenerator, pointwiseMeasure, npoints, axisSelector} = t;

      (* Method *)
      If[TrueQ[method === Automatic], method = "GlobalAdaptive"];

      t = NIntegrate`MOptionValue[method, nfs, ranges, allOpts];
      If[t === $Failed, Return[$Failed]];
      {absc, weights, errweights} = t[[1]];

      (* Pointwise measure *)
      If[pointwiseMeasure === Automatic,
        If[Length[ranges] <= 2,
          pointwiseMeasure = "VoronoiMesh",
          pointwiseMeasure = "Uniform"
        ];
      ];

      If[MemberQ[{"VoronoiMesh", VoronoiMesh}, pointwiseMeasure] && Length[ranges] > 2,
        Message[LebesgueIntegrationRule::vmesh];
        pointwiseMeasure = "Uniform";
      ];

      If[! MemberQ[{"VoronoiMesh", VoronoiMesh, "Uniform"}, pointwiseMeasure],
        Message[LebesgueIntegrationRule::noptval pointwiseMeasure, "PointwiseMeasure", {"VoronoiMesh", "Uniform"}];
        Return[$Failed];
      ];

      (* Points *)
      Which[
        npoints === Automatic && Length[ranges] == 1, npoints = 1000,
        npoints === Automatic && Length[ranges] > 1, npoints = 10^4
      ];

      If[! TrueQ[npoints >= 0] ,
        Message[NIntegrate::intpm, "Points" -> npoints, 2];
        Return[$Failed];
      ];

      (* AxisSelector *)
      t = NIntegrate`MonteCarloRuleDump`AxisSelectorParser[axisSelector, ruleOpts];
      If[t === $Failed, Return[$Failed]];
      {axisSelector, axisSelectorPointsFraction} = t;

      (* Lebesgue rule points and volumes. *)
      wprec = WorkingPrecision /. allOpts;
      dim = Length[ranges];

      (* Generate points *)
      If[TrueQ[RNGenerator == "Sobol" || RNGenerator == "Niederreiter"],
      (* We cannot set the working precision here. *)
        BlockRandom[
          SeedRandom[0,
            Method -> {"MKL", Method -> {RNGenerator, "Dimension" -> dim}}];
          points = RandomReal[{0, 1}, {npoints, dim}];
        ],
        (*ELSE*)
        points = RandomReal[{0, 1}, {npoints, dim}, WorkingPrecision -> wprec];
      ];

      (* Find point volumes *)
      If[TrueQ[(pointwiseMeasure === VoronoiMesh || pointwiseMeasure == "VoronoiMesh") && dim == 2],
        (* There is some problem in the VoronoiMesh 1D case, VoronoiMesh[__,{{a, b}}], so we skip it.*)
        vmesh = VoronoiMesh[points, ranges];
        pointVolumes = PropertyValue[{vmesh, dim}, MeshCellMeasure],
        (*ELSE*)
        (* Quite a shortcut, but should work well enough. To be properly implmenented... *)
        pointVolumes = Table[1/Length[points], {Length[points]}]
      ];
      pointVolumes = N[pointVolumes, wprec];

      (* Splitting axis selection data *)
      (* Note the hard-coded fraction. *)
      If[ TrueQ[axisSelector == "MinVariance"] && dim > 1,
        axisSplitPositions = AxisSelectionPoints[ points, axisSelectorPointsFraction],
        axisSplitPositions = None
      ];

      LebesgueIntegrationRule[{{absc, weights, errweights}, method, points, pointVolumes, axisSplitPositions}]
    ];

(* Using custom made rule in order to mininmize the dependence with on the region objects. *)
IRuleEstimate[f_, {a_, b_}, {absc_, weights_, errweights_}] :=
    Block[{integral, error},
      {integral, error} = (b - a) Total@
        MapThread[{f[#1] #2, f[#1] #3} &, {Rescale[absc, {0, 1}, {a, b}], weights, errweights}];
      {integral, Abs[error]}
    ];


(* The integration rule algorithm implementation uses EstimateMeasure defined above for the strategy. *)
LebesgueIntegrationRule[{{absc_, weights_, errweights_}, method_, points_,
  pointVolumes_, axisSplitPositions_}]["ApproximateIntegral"[region_]] :=
    Block[{regionPoints, factor, pointFuncVals, pointAbsVals, pointVals, offset, integral1, integral2},

      regionPoints =
          Transpose@
              MapThread[
                Rescale[#1, {0, 1}, #2] &, {Transpose[points], region["Boundaries"]}];

      factor = Apply[Times, Abs[Subtract @@@ region["Boundaries"]]];

      (* Integrals calculation *)
      pointFuncVals = region["NumericalFunction"] @@@ regionPoints;
      pointAbsVals = Abs[pointFuncVals];

      (* First integral calculation *)
      pointVals = 1/2 (pointAbsVals + pointFuncVals);

      offset = Min[pointVals] * EstimateMeasure[Min[pointVals], pointVals, factor*pointVolumes];

      integral1 = {offset, 0} +
          IRuleEstimate[
            EstimateMeasure[#, pointVals, factor*pointVolumes] &,
            {Min[pointVals], Max[pointVals]}, {absc, weights, errweights}];

      (* Second integral calculation *)
      pointVals = 1/2 (pointAbsVals - pointFuncVals);

      offset = Min[pointVals] * EstimateMeasure[Min[pointVals], pointVals, factor*pointVolumes];

      integral2 = {offset, 0} +
          IRuleEstimate[
            EstimateMeasure[#, pointVals, factor*pointVolumes] &,
            {Min[pointVals], Max[pointVals]}, {absc, weights, errweights}];

      (* Proper splitting axis selection as in MonteCarloRule or just random axis pick. *)
      { integral1[[1]] - integral2[[1]],
        integral1[[2]] + integral2[[2]],
        If[ TrueQ[ axisSplitPositions === None ],
          RandomInteger[{1, Length[region["Boundaries"]]}],
          SelectAxis[pointVals,axisSplitPositions]
        ]}
    ];


(**************************************************************************)
(* Measure estimates by a regular grid integration rule                   *)
(**************************************************************************)

Clear[CellPointIndices]
CellPointIndices[points : {{_?NumberQ ..} ..}, ncells : {_?NumberQ ..}] :=
    Block[{cellsOrigins, cellSizes, cellOriginToIndexRules, pointCells,
      fvec, pointToCellIndexRules, pointIndexToCellIndexRules,
      cellIndexToPointCountRules, t, missingCells},
      cellsOrigins = N[MakeCells[ncells]];
      cellSizes = N[1/ncells];
      cellOriginToIndexRules = Dispatch@Thread[cellsOrigins -> Range[Length[cellsOrigins]]];
      pointCells =
          Transpose[
            MapThread[
              QuotientRemainder[#1, #2][[All, 1]] &, {Transpose[points], cellSizes}]] /. cellOriginToIndexRules;
      fvec = Reverse@FoldList[Times, 1, Reverse[Rest[ncells]]];
      pointCells = Map[fvec.# + 1 &, pointCells];
      pointToCellIndexRules = Thread[points -> pointCells];
      pointIndexToCellIndexRules = Thread[Range[Length[points]] -> pointCells];
      (*cellIndexToPointCountRules = Rule @@@ Tally[Range[Length[points]] /. pointIndexToCellIndexRules];*)
      t = SortBy[ Map[#[[1, 2]] -> #[[All, 1]] &,
        GatherBy[List @@@ pointIndexToCellIndexRules, #[[2]] &]], First];
      missingCells = Complement[ Range[Apply[Times,ncells]], t[[All,1]]];
      If[Length[missingCells] > 0, t = Join[ t, Map[#->{}&, missingCells]] ];
      {t, #[[2]] & /@ t, missingCells}
    ];

Clear[CellMinMaxValues]
CellMinMaxValues[func_, points : {{_?NumberQ ..} ..}, cellPointIndices : {{_Integer ..} ..}] :=
    CellMinMaxValues[func, points, cellPointIndices, Table[{0, 1}, {Length[points[[1]]]}]];
CellMinMaxValues[func_, points : {{_?NumberQ ..} ..}, cellPointIndices : {{_Integer ..} ..},
  boundaries : {{_?NumberQ, _?NumberQ} ..}] :=
    Block[{fs, fs1, fs2},
      fs = func @@@
          Transpose[ MapThread[ Rescale[#1, {0, 1}, #2] &, {Transpose[points], boundaries}] ];
      fs1 = (Abs[fs] + fs)/2;
      fs2 = (Abs[fs] - fs)/2;
      {Transpose@Map[Through[{Min, Max}[fs1[[#]]]] &, cellPointIndices],
        Transpose@Map[Through[{Min, Max}[fs2[[#]]]] &, cellPointIndices],
        fs}
    ] /; Length[points[[1]]] == Length[boundaries];

CellEstimateMeasure[fval_?NumericQ, regionMinVals : {_?NumberQ ...},
  regionMaxVals : {_?NumberQ ...}, cellVolume_?NumberQ] :=
    Block[{pindsMin, pindsMax, fmaxdiffs, denomdiffs},
      pindsMin = Clip[Sign[regionMinVals - fval], {0, 1}, {0, 1}];
      fmaxdiffs = regionMaxVals - fval;
      pindsMax = Clip[Sign[fmaxdiffs], {0, 1}, {0, 1}];
      denomdiffs = Map[If[# == 0, 1, #] &, Abs[regionMaxVals - regionMinVals]];
      cellVolume (Total[pindsMin] + ((Abs[fmaxdiffs]/denomdiffs).((1 - pindsMin)* pindsMax)))
    ]

Clear[GridLebesgueIntegrationRule]
Options[GridLebesgueIntegrationRule] = {
  "Method" -> "ClenshawCurtisRule",
  "PointGenerator" -> "Sobol",
  "GridSizes" -> Automatic,
  "Points" -> Automatic,
  "AxisSelector" -> Automatic
};
GridLebesgueIntegrationRuleProperties = Part[Options[GridLebesgueIntegrationRule], All, 1];


GridLebesgueIntegrationRule::gsizes =
    "The value of the option \"GridSizes\" is expected to be Automatic or a list of positive integers.";

GridLebesgueIntegrationRule::ecells =
    "Using the specified option values `1` cells of the grid for measure estimation are have not points.";

GridLebesgueIntegrationRule::noptval = "The value `1` given to the option `2` is not one of `3`.";

GridLebesgueIntegrationRule /:
    NIntegrate`InitializeIntegrationRule[GridLebesgueIntegrationRule, nfs_, ranges_, ruleOpts_, allOpts_] :=
    Module[{t, method, RNGenerator, gridSizes, npoints,
      absc, weights, errweights, points, pointVolumes, wprec, dim, cellPointIndices, cellVolume,
      axisSelector, axisSelectorPointsFraction, axisSplitPositions},

      t = NIntegrate`GetMethodOptionValues[GridLebesgueIntegrationRule, GridLebesgueIntegrationRuleProperties, ruleOpts];

      If[t === $Failed, Return[$Failed]];
      {method, RNGenerator, gridSizes, npoints, axisSelector} = t;

      (* Method *)
      If[TrueQ[method === Automatic], method = "GlobalAdaptive"];

      t = NIntegrate`MOptionValue[method, nfs, ranges, allOpts];
      If[t === $Failed, Return[$Failed]];
      {absc, weights, errweights} = t[[1]];

      (* Points *)
      Which[
        npoints === Automatic && Length[ranges] == 1, npoints = 1000,
        npoints === Automatic && Length[ranges] > 1, npoints = 10^4
      ];

      If[! TrueQ[npoints >= 0] ,
        Message[NIntegrate::intpm, "Points" -> npoints, 2];
        Return[$Failed];
      ];

      (* AxisSelector *)
      t = NIntegrate`MonteCarloRuleDump`AxisSelectorParser[axisSelector, ruleOpts];
      If[t === $Failed, Return[$Failed]];
      {axisSelector, axisSelectorPointsFraction} = t;

      (* Lebesgue rule points and volumes. *)
      wprec = WorkingPrecision /. allOpts;
      dim = Length[ranges];

      (* Generate points *)
      If[TrueQ[RNGenerator == "Sobol" || RNGenerator == "Niederreiter"],
      (* We cannot set the working precision here. *)
        BlockRandom[
          SeedRandom[0,
            Method -> {"MKL", Method -> {RNGenerator, "Dimension" -> dim}}];
          points = RandomReal[{0, 1}, {npoints, dim}];
        ],
      (*ELSE*)
        points = RandomReal[{0, 1}, {npoints, dim}, WorkingPrecision -> wprec];
      ];

      (* Grid sizes *)
      If[ IntegerQ[gridSizes], gridSizes = {gridSizes}];
      If[ TrueQ[gridSizes === Automatic], gridSizes = Table[10, {Length[ranges]}] ];

      If[ ! ( MatchQ[gridSizes,{_Integer ..}] && Apply[And,Map[#>0&, gridSizes]] ),
        Message[GridLebesgueIntegrationRule::gsizes];
        Return[$Failed];
      ];

      Which[
        Length[gridSizes] < Length[ranges],
          gridSizes = Join[ gridSizes, Table[gridSizes[[-1]],{Length[ranges]-Length[gridSizes]}] ],
        Length[gridSizes] > Length[ranges],
          gridSizes = Take[gridSizes,Length[ranges]]
      ];

      (* Find point-cell associations *)
      t = CellPointIndices[ points, gridSizes ];

      (* There should be a check does every cell have at least a point. *)
      If[ Length[ t[[3]] ] > 0,
        Message[GridLebesgueIntegrationRule::ecells,Length[t[[3]]]];
        Return[$Failed];
      ];
      cellPointIndices = t[[2]];

      cellVolume = N[ 1 / Apply[Times,gridSizes], wprec ];

      (* Splitting axis selection data *)
      (* Note the hard-coded fraction. *)
      If[ TrueQ[axisSelector == "MinVariance"] && dim > 1,
        axisSplitPositions = AxisSelectionPoints[ points, axisSelectorPointsFraction],
        axisSplitPositions = None
      ];

      GridLebesgueIntegrationRule[{{absc, weights, errweights}, method, points, gridSizes, cellPointIndices,
        cellVolume, axisSplitPositions}]
    ];

GridLebesgueIntegrationRule[{{absc_, weights_, errweights_}, method_, points_, gridSizes_, cellPointIndices_,
  cellVolume_, axisSplitPositions_}]["ApproximateIntegral"[region_]] :=
    Block[{regionPoints, factor, offset, integral1, integral2, minVals, maxVals, pointVals, t},

      factor = Apply[Times, Abs[Subtract @@@ region["Boundaries"]]];

      (* Integrals calculation *)
      t = CellMinMaxValues[ region["NumericalFunction"], points, cellPointIndices, region["Boundaries"]];

      pointVals = t[[3]];

      (* First integral calculation *)
      {minVals, maxVals} = t[[1]];

      offset = Min[minVals] * CellEstimateMeasure[Min[minVals], minVals, maxVals, factor*cellVolume];

      integral1 = {offset, 0} +
          IRuleEstimate[
            CellEstimateMeasure[#, minVals, maxVals, factor*cellVolume] &,
            {Min[minVals], Max[maxVals]}, {absc, weights, errweights}];

      (* Second integral calculation *)
      {minVals, maxVals} = t[[2]];

      offset = Min[minVals] * CellEstimateMeasure[Min[minVals], minVals, maxVals, factor*cellVolume];

      integral2 = {offset, 0} +
          IRuleEstimate[
            CellEstimateMeasure[#, minVals, maxVals, factor*cellVolume] &,
            {Min[minVals], Max[maxVals]}, {absc, weights, errweights}];

      (* Proper splitting axis selection as in MonteCarloRule or just random axis pick. *)
      { integral1[[1]] - integral2[[1]],
        integral1[[2]] + integral2[[2]],
        If[ TrueQ[ axisSplitPositions === None ],
          RandomInteger[{1, Length[region["Boundaries"]]}],
          SelectAxis[pointVals,axisSplitPositions]
        ]}
    ];


End[] (* `Private` *)

EndPackage[]