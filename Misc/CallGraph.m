(*
    Call graph generation for context functions Mathematica package
    Copyright (C) 2018  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)
(* :Title: CallGraph *)
(* :Context: CallGraph` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-12-31 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: call graph, dependencies, down values, sub values *)
(* :Discussion:

   # In brief

   This package provides functions for making a call graph between the functions that belong to specified contexts.

   The main function is `CallGraph` that gives a graph with vertices that are functions names and edges that show
   which functions call which other functions. With the default option values the graph vertices are labeled and
   have tooltips with function usage messages.


   # General design

   The call graphs produced by the main package function `CallGraph` are assumed to be used for studying or refactoring
   of large code bases written with Mathematica / Wolfram Language.

   The argument of `CallGraph` is a context string or a list of context strings.

   With the default values of its options `CallGraph` produces a graph with labeled nodes and the labels have tooltips
   that show the usage messages of the functions from the specified contexts.
   It is assumed that this would be the most useful call graph type for studying the codes of different sets of packages.

   We can make simple, non-label, non-tooltip call graph using `CallGraph[ ... , "UsageTooltips" -> False ]`.

   The simple call graph can be modified with the functions:

      CallGraphAddUsageMessages, CallGraphAddPrintDefinitionsButtons, CallGraphBiColorCircularEmbedding

   Each of those functions is decorating the simple graph in a particular way.


   # Usage examples

   This imports a package from GitHub:

      Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]

   ## Generate a call graph with usage tooltips

      CallGraph["MonadicQuantileRegression`", GraphLayout -> "SpringElectricalEmbedding", ImageSize -> Large]


   ## Generate a call graph with exclusions

      gr =
        CallGraph["MonadicQuantileRegression`",
                  Exclusions -> {QRMonUnit, QRMon, QRMonBind, $QRMonFailure, ToExpression /@ Names["QRMonTake*"], ToExpression /@ Names["QRMonSet*"]},
                  GraphLayout -> "SpringEmbedding", ImageSize -> Large]


   ## Generate call graph with buttons to print definitions

      gr0 = CallGraph["MonadicQuantileRegression`", "UsageTooltips" -> False];
      gr1 = CallGraphAddPrintDefinitionsButtons[gr0, GraphLayout -> "StarEmbedding", ImageSize -> 900]


   ## Generate circular embedding graph color

      cols = RandomSample[ ColorData["Rainbow"] /@ Rescale[Range[VertexCount[gr1]]]];

      CallGraphBiColorCircularEmbedding[ gr1, "VertexColors" -> cols, ImageSize -> 900 ]


   # Options

   The package functions "CallGraph*" take all of the options of the function Graph.
   Below are described the additional options of CallGraph.

   - "PrivateContexts"
     Should the functions of the private contexts be included in the call graph.

   - "SelfReferencing"
     Should the self referencing edges be excluded or not.

   - "AtomicSymbols"
     Should atomic symbols be included in the call graph.

   - Exclusions
     Symbols to be excluded from the call graph.

   - "UsageTooltips"
     Should vertex labels with the usage tooltips be added.

   - "UsageTooltipsStyle"
     The style of the usage tooltips.


   # Possible issues

   - With large context (e.g. "System`") the call graph generation might take long time. (See the TODOs below.)

   - With "PrivateContexts"->False the call graph will be empty if the public functions do not depend on each other.

   - For certain packages the scanning of the down values would produce (multiple) error messages or warnings.


   Anton Antonov
   Windermere, Florida, USA
   2019-01-01
*)

(*
   TODO
   1. [ ] Special handling for the "System`" context.
   2. [ ] Use the symbols up-values to make the call graph.
   3. [ ] Consider/implement call graph making with specified patterns and list of symbols.
          Instead of just using contexts and exclusions. (The current approach/implementation.)
   4. [X] Implement special radial graph visualization. (Circular embedding with edges that are Bezier curves.)
   5. [ ] Provide special functions for "call sequence" tracing for a specified symbol.

*)

BeginPackage["CallGraph`"];

FunctionDependencies::usage = "Find functions called by the down values and sub values of the argument."

CallGraph::usage = "CallGraph[contexts:{_String..}, opts___] makes a call graph for the functions of \
specified (package) contexts."

CallGraphAddUsageMessages::usage = "CallGraphAddUsageMessages[gr_Graph] adds tooltips with usage messages for \
the nodes of the call graph gr."

CallGraphAddPrintDefinitionsButtons::usage = "CallGraphAddPrintDefinitionsButtons[gr_Graph] adds buttons for \
printing the codes corresponding to the nodes of the call graph gr."

CallGraphBiColorCircularEmbedding::usage = "CallGraphBiColorCircularEmbedding[gr_Graph] applies to the graph gr \
the layout \"CircularEmbedding\" and renders the gr edges using Bezier curves and two colors."

Begin["`Private`"];

(*Needs["GeneralUtilities`"];*)

Clear[SymbolQ]
SymbolQ[x_] := Head[x] === Symbol;

(***********************************************************)
(* Dependencies                                               *)
(***********************************************************)

(*
  Initial implementation by Szabolcs Horvat:
    https://mathematica.stackexchange.com/a/4344/34008
*)

Clear[FunctionQ]
SetAttributes[FunctionQ, HoldAll]
FunctionQ[sym_Symbol] :=
    ((DownValues[sym] =!= {}) || SubValues[sym] =!= {}) && (OwnValues[sym] === {});

Clear[FunctionDependencies]
Options[FunctionDependencies] = {"AtomicSymbols" -> False };
SetAttributes[FunctionDependencies, HoldAll]
FunctionDependencies[sym_Symbol, opts:OptionsPattern[] ] :=
    Block[{atomicSymbolsQ, testFunc},
      atomicSymbolsQ = TrueQ[OptionValue[FunctionDependencies, "AtomicSymbols"]];
      With[{ functionQ = If[ atomicSymbolsQ, SymbolQ, FunctionQ] },
        Union @
            Join[
              List @@
                  Select[Union@
                      Level[(Hold @@ DownValues[sym])[[All, 2]], {-1}, Hold, Heads -> True],
                    functionQ],
              List @@
                  Select[Union@
                      Level[(Hold @@ SubValues[sym])[[All, 2]], {-1}, Hold, Heads -> True],
                    functionQ]
            ]
      ]
    ];


(***********************************************************)
(* CallGraph                                               *)
(***********************************************************)

Clear[CallGraph]

Options[CallGraph] =
    Join[
      { "PrivateContexts" -> False, "SelfReferencing" -> False, "AtomicSymbols" -> True, Exclusions -> {},
        "UsageTooltips" -> True, "UsageTooltipsStyle" -> "Subsubsection" },
      Options[Graph]
    ];

CallGraph[context_String, opts:OptionsPattern[] ] := CallGraph[{context}, opts ];

CallGraph[contexts:{_String..}, opts:OptionsPattern[] ] :=
    Block[{pSymbs, pPrivateSymbs, exclSymbs, atomicSymbolsQ, dRes, aDependencyRules, gRules, grOpts, utStyle, styleFunc},

      (* Find the symbols in the contexts. *)
      pSymbs =
          Flatten@
              Map[
                Function[{c},
                  Block[{p = Names[c <> "*"]},
                    Select[Map[ToExpression[c <> #] &, p], Head[#] === Symbol &]]
                ], contexts];

      (* Find the symbols in the private contexts. *)
      If[ TrueQ[OptionValue[CallGraph, "PrivateContexts"]],
        pPrivateSymbs =
            Flatten@
                Map[ToExpression[Names[# <> "Private`*"]] &, contexts];

        pSymbs = Join[pSymbs, pPrivateSymbs];
      ];

      (* Drop atomic symbols. *)
      atomicSymbolsQ = TrueQ[OptionValue[CallGraph, "AtomicSymbols"]];
      If[ !atomicSymbolsQ,
        pSymbs = Select[ pSymbs, FunctionQ ]
      ];

      (* Exclude specified symbols. *)
      exclSymbs = Flatten[{OptionValue[CallGraph, Exclusions]}];
      pSymbs = Complement[pSymbs, exclSymbs];

      (* Find dependencies. *)
      dRes =
          AssociationThread[
            pSymbs,
            Map[ FunctionDependencies[#, "AtomicSymbols" -> atomicSymbolsQ]&, pSymbs]
          ];

      aDependencyRules = Map[ Intersection[pSymbs, #]&, dRes];

      gRules = Reverse /@ Flatten[Thread /@ Normal[aDependencyRules]];

      (* Delete self-referencing rules. *)
      If[ !TrueQ[OptionValue[CallGraph, "SelfReferencing"]],
        gRules = DeleteCases[gRules, x_ -> x_];
      ];

      (* Add tooltips with usage messages. *)
      If[ TrueQ[OptionValue[CallGraph, "UsageTooltips"]],
        utStyle = OptionValue[CallGraph, "UsageTooltipsStyle"];
        styleFunc = If[ TrueQ[ utStyle === None ], Identity, Style[#,utStyle]& ];
        gRules = Map[Tooltip[#, styleFunc @ Row[{Style[SymbolName[#],Italic,Bold], ": ", ToString[#::usage]}] ]&, gRules, {2}]
      ];

      (* Make the graph. *)
      grOpts = Normal @ KeyTake[ {opts}, First /@ Options[Graph]];

      If[ TrueQ[OptionValue[CallGraph, "UsageTooltips"]],
        Graph[gRules, Sequence @@ grOpts, VertexLabels -> "Name"],
        (*ELSE*)
        Graph[gRules, Sequence @@ grOpts ]
      ]
    ];


CallGraph[___] :=
    Block[{},
      Message[CallGraph::args];
      $Failed
    ];

CallGraph::args = "The first argument is expected to be a string or a list of strings; each string corresponds to a context."


(***********************************************************)
(* CallGraphAddUsageMessages                               *)
(***********************************************************)

Clear[CallGraphAddUsageMessages];

Options[CallGraphAddUsageMessages] = Options[Graph];

CallGraphAddUsageMessages[gr_Graph, opts:OptionsPattern[] ] :=
    Block[{grInfoRules},
      grInfoRules = Map[Tooltip[#, #::usage] &, EdgeList[gr], {2}];
      Graph[grInfoRules, opts, VertexLabels -> "Name"]
    ];


(***********************************************************)
(* CallGraphAddPrintDefinitionsButtons                     *)
(***********************************************************)

Clear[CallGraphAddPrintDefinitionsButtons];

Options[CallGraphAddPrintDefinitionsButtons] = Options[Graph];

CallGraphAddPrintDefinitionsButtons[gr_Graph, opts:OptionsPattern[] ] :=
    Block[{grDefRules},
      grDefRules = Map[Button[#, GeneralUtilities`PrintDefinitions[#], Appearance->Frameless]&, EdgeList[gr], {2}];
      Graph[grDefRules, opts, VertexLabels -> "Name"]
    ];


(***********************************************************)
(* CallGraphBiColorCircularEmbedding                       *)
(***********************************************************)

(*
  The functions eSf and vSf were taken from kglr's Mathematica Stack Exchange answer:

    https://mathematica.stackexchange.com/a/188390/34008 .

  Those functions were modified to take additional arguments for:
  point size, edge thickness, edge halves drawing functions.
*)

ClearAll[eSf, vSf];

eSf[g_, cols_, edgeThickness_: Thin, firstHalfEdgeFunc_: Line,
  secodHalfEdgeFunc_: Arrow] :=
    Module[{bsf =
        BSplineFunction[{#[[1]],
          RegionNearest[
            Disk[Mean[#[[{1, -1}]]], Norm[#[[1]] - #[[-1]]]], {0,
            0}], #[[-1]]}], p1 = Subdivide[0, 1/2, 50],
      p2 = Subdivide[1/2, 1, 50]}, {edgeThickness,
      cols[[VertexIndex[g, #2[[1]]]]], firstHalfEdgeFunc[bsf /@ p1],
      cols[[VertexIndex[g, #2[[2]]]]], secodHalfEdgeFunc[bsf /@ p2]}] &;

vSf[g_, cols_, pointSize_: Large] :=
    Module[{off = If[-Pi/2 < ArcTan @@ # < Pi/2, Left, Right]}, {cols[[
        VertexIndex[g, #2]]],
      Text[Style[Framed[#2, FrameStyle -> None],
        FontSize -> Scaled[.03]], #, {off, Center},
        ArcTan[#] (off /. {Left -> 1, Right -> -1})],
      PointSize[pointSize], Point@#}] &;


Clear[CallGraphBiColorCircularEmbedding]

Options[CallGraphBiColorCircularEmbedding] = Join[ {"VertexColors" -> Automatic }, Options[Graph] ];

CallGraphBiColorCircularEmbedding[gr_Graph, opts:OptionsPattern[] ] :=
    Block[{cc, cols, grOpts},

      cols = OptionValue["VertexColors"];
      If[ TrueQ[ cols === Automatic ],
        cc = PageRankCentrality[gr];
        cols = ColorData[{"Rainbow", {1, VertexCount@gr}}] /@ Range[VertexCount[gr]];
        cols = cols[[Ordering[cc]]];
      ];

      grOpts = Normal @ KeyTake[ {opts}, First /@ Options[Graph]];

      Graph[EdgeList[gr], Sequence @@ grOpts,
        VertexLabels -> None, GraphLayout -> "CircularEmbedding",
        VertexShapeFunction -> vSf[gr, cols],
        EdgeShapeFunction -> eSf[gr, cols, If[EdgeCount[gr]/(VertexCount[gr]+1) > 1.5 || EdgeCount[gr] > 400, Thin, Thick, Thin]]]
    ];

End[]; (* `Private` *)

EndPackage[]