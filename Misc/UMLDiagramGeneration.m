(*
    UML Diagram Generation Mathematica package
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

(*
    Mathematica is (C) Copyright 1988-2016 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: UMLDiagramGeneration *)
(* :Author: Anton Antonov *)
(* :Date: 2016-03-13 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 10.3.1 *)
(* :Copyright: (c) 2016 Anton Antonov *)
(* :Keywords: UML, Graph, diagram, plot *)
(* :Discussion:

    This package generates UML diagrams according to specified relationships of symbols that represent classes.

    The main reason for programming the package functions came from the need to illustrate the implementations
    of Object-Oriented Design Patterns in Mathematica.

    Consider the following implementation of the Template Method Design Pattern:

      Clear[AbstractClass, ConcreteOne, ConcreteTwo];

      CLASSHEAD = AbstractClass;
      AbstractClass[d_]["Data"[]] := d;
      AbstractClass[d_]["PrimitiveOperation1"[]] := d[[1]];
      AbstractClass[d_]["PrimitiveOperation2"[]] := d[[2]];
      AbstractClass[d_]["TemplateMethod"[]] :=
      CLASSHEAD[d]["PrimitiveOperation1"[]] + CLASSHEAD[d]["PrimitiveOperation2"[]]

      ConcreteOne[d_][s_] := Block[{CLASSHEAD = ConcreteOne}, AbstractClass[d][s]]
      ConcreteOne[d_]["PrimitiveOperation1"[]] := d[[1]];
      ConcreteOne[d_]["PrimitiveOperation2"[]] := d[[1]]*d[[2]];

      ConcreteTwo[d_][s_] := Block[{CLASSHEAD = ConcreteTwo}, AbstractClass[d][s]]
      ConcreteTwo[d_]["PrimitiveOperation1"[]] := d[[1]];
      ConcreteTwo[d_]["PrimitiveOperation2"[]] := d[[3]]^d[[2]];

    Then the corresponding UML diagram can be generated with the commands:

      1. UMLClassGraph[{AbstractClass, ConcreteOne, ConcreteTwo}]

    or

      2. UMLClassGraph[{AbstractClass, ConcreteOne, ConcreteTwo},
                       {AbstractClass -> {"PrimitiveOperation1", "PrimitiveOperation2"}},
                       "Abstract" -> {AbstractClass}, VertexLabelStyle -> "Subsubsection"]

    The function UMLClassGraph takes all options of Graph.

    The function UMLClassNode can be used to create custom graphs.

    Here is an example of UMLClassNode usage:

      Grid @
        Table[ UMLClassNode[AbstractClass, "Abstract" -> a, "EntityColumn" -> ec],
               {a, {{}, {"PrimitiveOperation1", "PrimitiveOperation2", AbstractClass}}},
               {ec, {False, True}} ]



    This file was created using Mathematica Plugin for IntelliJ IDEA.

    Anton Antonov
    2016-03-13

*)

BeginPackage["UMLDiagramGeneration`"]

UMLClassNode::usage = "UMLClassNode[classSymbol, opts] creates a Grid object with a class name and its methods \
for the specified class symbol. The option \"Abstact\" can be used to specify abstract class names and methods. \
The option \"EntityColumn\" can be used to turn on and off the explanations column."

UMLClassGraph::usage = "UMLClassGraph[symbols,abstractMethodsPerSymbol,symbolAssociations,symbolAggregations,opts] \
creates an UML graph diagram for the specified symbols (representing classes) and their relationships. It takes \
as options the options of UMLClassNode and Graph."

SubValueReferenceRules::usage = "SubValueReferenceRules[symbols] gives a list of directed edge specifications that \
correspond to references within the sub-values of the specified symbols."

Begin["`Private`"]


(*********************************************************)
(* UMLClassNode                                          *)
(*********************************************************)

Clear[UMLClassNode]
Options[UMLClassNode] = {"Abstract" -> {}, "EntityColumn" -> True};

UMLClassNode[classSymbol_Symbol, opts : OptionsPattern[]] :=
    Block[{abstract = OptionValue["Abstract"],
      enColQ = TrueQ[OptionValue["EntityColumn"]], res, tbl},
      res = Cases[
        SubValues[Evaluate@classSymbol][[All, 1]], _String[___], Infinity];
      res = res /. Map[# -> Style[#, Italic] &, abstract];
      (*Print[{abstract,MemberQ[abstract,classSymbol]}];*)

      If[Length[res] == 0, res = {None}];
      tbl =
          Transpose@{Join[Style[#, Gray] & /@ {"Class", "Methods"},
            Table[SpanFromAbove, {Length[res] - 1}]],
            Join[{Style[ToString[classSymbol], Bold,
              FontSlant -> If[MemberQ[abstract, classSymbol], Italic, Plain]]},
              res]};
      If[res === {None}, tbl = Most[tbl]];
      Grid[If[enColQ, tbl, tbl[[All, {2}]]],
        Dividers -> {All, {True, True, False}, -1 -> True}, Alignment -> Left]
    ];


(*********************************************************)
(* Graph edge functions                                  *)
(*********************************************************)

Clear[UMLInheritanceEdgeFunc]
UMLInheritanceEdgeFunc[pts_List, e_] :=
    Block[{color =
        Darker[Blend[{Black, Cyan, Blue}]]}, {Arrowheads[{{0.015, 0.85, Graphics[{FaceForm[White], EdgeForm[color],
        Polygon[{{-1.5, -1}, {1.5, 0}, {-1.5, 1}}]}]}}], {color, Arrow[pts]}}
    ];

Clear[UMLAssociationEdgeFunc]
UMLAssociationEdgeFunc[pts_List, e_] :=
    Block[{color = Darker[Blend[{Black, Cyan, Blue}]]}, {color, Line[pts]}];

Clear[UMLDirectedAssociationEdgeFunc]
UMLDirectedAssociationEdgeFunc[pts_List, e_] :=
    Block[{color = Darker[Blend[{Black, Cyan, Blue}]]}, {color, Arrow[pts]}];

Clear[UMLAggregationEdgeFunc]
UMLAggregationEdgeFunc[pts_List, e_] :=
    Block[{color =
        Darker[Blend[{Black, Cyan, Blue}]]}, {Arrowheads[{{0.015, 0.85, Graphics[{FaceForm[White], EdgeForm[color],
        Polygon[{{-1, -1}, {1, 0}, {-1, 1}, {-3, 0}}]}]}}], {color, Arrow[pts]}}
    ];

(*********************************************************)
(* SubValueReferenceRules                                *)
(*********************************************************)
Clear[SubValueReferenceRules]

SubValueReferenceRules[symbols:{_Symbol..}] :=
  DeleteCases[#, None] &@
      Flatten@Outer[
        If[#1 =!= #2 && ! FreeQ[Cases[SubValues[#1][[All]],
            RuleDelayed[x_, y_] :> HoldForm[y]], #2], #1 \[DirectedEdge] #2, None] &, symbols, symbols ];

(*********************************************************)
(* UMLClassGraph                                         *)
(*********************************************************)
Clear[UMLClassGraph]
Options[UMLClassGraph] = Join[Options[UMLClassNode], Options[Graph]];

UMLClassGraph[symbols:{_Symbol..}, abstractMethodsPerSymbol : {_Rule ...} : {},
  symbolAssociations : {(_DirectedEdge | _UndirectedEdge) ...} : {},
  symbolAggregations : {(_DirectedEdge | _UndirectedEdge) ...} : {},
  opts : OptionsPattern[]] :=
    Block[{grRules, assocOpts},
      grRules = SubValueReferenceRules[symbols];
      UMLClassGraph[grRules, abstractMethodsPerSymbol, symbolAssociations, symbolAggregations, opts]
    ];

UMLClassGraph[inheritanceRules : {DirectedEdge[_Symbol, _Symbol] ..},
              abstractMethodsPerSymbol : {_Rule ...} : {},
              symbolAssociations : {(_DirectedEdge | _UndirectedEdge) ...} : {},
              symbolAggregations : {(_DirectedEdge | _UndirectedEdge) ...} : {},
              opts : OptionsPattern[]] :=
    Block[{grRules = inheritanceRules, assocOpts, symbols},
      symbols = Union[Flatten[List @@@ Join[inheritanceRules,symbolAssociations, symbolAggregations]]];
      grRules = Map[
        Which[
          MemberQ[symbolAssociations, #],
          Property[#, EdgeShapeFunction -> UMLDirectedAssociationEdgeFunc],
          MemberQ[
            symbolAssociations, (UndirectedEdge @@ #) | (UndirectedEdge @@
              Reverse[#])],
          Property[#, EdgeShapeFunction -> UMLAssociationEdgeFunc],
          True,
          Property[#, EdgeShapeFunction -> UMLInheritanceEdgeFunc]
        ] &, Union[grRules, symbolAssociations]];
      grRules =
          Join[grRules,
            Map[Property[#, EdgeShapeFunction -> UMLAggregationEdgeFunc] &,
              symbolAggregations]];
      Graph[grRules,
        VertexLabels ->
            Map[# ->
                UMLClassNode[#, "EntityColumn" -> OptionValue["EntityColumn"],
                  "Abstract" ->
                      Flatten[Join[{# /. Append[abstractMethodsPerSymbol, _ -> {}]},
                        OptionValue["Abstract"]]]] &, symbols],
        Sequence @@ DeleteCases[{opts}, ("EntityColumn" -> _) | ("Abstract" -> _)]]
    ];

End[] (* `Private` *)

EndPackage[]