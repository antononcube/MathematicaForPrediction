(*
    Call graph for context functions Mathematica package
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
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

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

   The main function is CallGraph that gives a graph with vertices that are functions names and edges that
   show which function calls which other functions. With the default option values the graph vertices labeled with
   function names that have as tooltips the corresponding usage messages.


   # Usage examples

   This imports a package from GitHub:

      Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]

   ## Generate a call graph with usage tooltips

      CallGraph["MonadicQuantileRegression`", GraphLayout -> "SpringElectricalEmbedding" ]


   ## Generate a call graph with exclusions

      gr =
        CallGraph["MonadicQuantileRegression`",
                  Exclusions -> {QRMonUnit, QRMon, QRMonBind, $QRMonFailure, ToExpression /@ Names["QRMonTake*"], ToExpression /@ Names["QRMonSet*"]},
                  GraphLayout -> "SpringEmbedding", ImageSize -> Large];


   ## Generate call graph with buttons to print definitions

      CallGraphAddPrintDefinitionsButtons[gr, GraphLayout -> "SpringElectricalEmbedding", ImageSize -> 900]


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
     Should vertex labesl with the usage tooltips be added.

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
   4. [ ] Implement special radial graph visualization.
   5. [ ] Provide special functions for "call sequence" tracing for a specified symbol.

*)

BeginPackage["CallGraph`"];

CallGraph::usage = "CallGraph[contexts:{_String..}, opts___] makes a call graph for the functions of \
specified (package) contexts."

CallGraphAddUsageMessages::usage = "CallGraphAddUsageMessages[gr_Graph] adds tooltips with usage messages for \
the nodes of the call graph gr."

CallGraphAddPrintDefinitionsButtons::usage = "CallGraphAddPrintDefinitionsButtons[gr_Graph] adds buttons for \
printing the codes corresponding to the nodes of the call graph gr."

Begin["`Private`"];

(*Needs["GeneralUtilities`"];*)

Clear[SymbolQ]
SymbolQ[x_] := Head[x] === Symbol;


Clear[CallGraph]

Options[CallGraph] =
    Join[
      { "PrivateContexts" -> False, "SelfReferencing" -> False, "AtomicSymbols" -> True, Exclusions -> {},
        "UsageTooltips" -> True, "UsageTooltipsStyle" -> "Subsubsection" },
      Options[Graph]
    ];

CallGraph[context_String, opts:OptionsPattern[] ] := CallGraph[{context}, opts ];

CallGraph[contexts:{_String..}, opts:OptionsPattern[] ] :=
    Block[{pSymbs, pPrivateSymbs, exclSymbs, dvs, dRes, aDependencyRules, gRules, grOpts, utStyle, styleFunc},

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

      (* Find the definitions: down-values and sub-values. *)
      dvs = AssociationThread[pSymbs, MapThread[Join, {DownValues /@ pSymbs, SubValues /@ pSymbs}] ];
      dvs = Select[dvs, Length[#] > 0 &];

      (* Drop atomic symbols. *)
      If[ !TrueQ[OptionValue[CallGraph, "AtomicSymbols"]],
        pSymbs = Keys[dvs]
      ];

      (* Exclude specified symbols. *)
      exclSymbs = Flatten[{OptionValue[CallGraph, Exclusions]}];
      pSymbs = Complement[pSymbs, exclSymbs];
      dvs = KeyDrop[dvs, exclSymbs];

      (* Find dependencies. *)
      dRes = AssociationThread[
        pSymbs ->
            Map[Function[{s}, Map[! FreeQ[HoldPattern[#], s] &, dvs]], pSymbs]];

      aDependencyRules = Map[Pick[Keys[#], Values[#]] &, dRes];

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


Clear[CallGraphAddUsageMessages];

Options[CallGraphAddUsageMessages] = Options[Graph];

CallGraphAddUsageMessages[gr_Graph, opts:OptionsPattern[] ] :=
    Block[{grInfoRules},
      grInfoRules = Map[Tooltip[#, #::usage] &, EdgeList[gr], {2}];
      Graph[grInfoRules, opts, VertexLabels -> "Name"]
    ];


Clear[CallGraphAddPrintDefinitionsButtons];

Options[CallGraphAddPrintDefinitionsButtons] = Options[Graph];

CallGraphAddPrintDefinitionsButtons[gr_Graph, opts:OptionsPattern[] ] :=
    Block[{grDefRules},
      grDefRules = Map[Button[#, GeneralUtilities`PrintDefinitions[#], Appearance->Frameless]&, EdgeList[gr], {2}];
      Graph[grDefRules, opts, VertexLabels -> "Name"]
    ];

End[]; (* `Private` *)

EndPackage[]