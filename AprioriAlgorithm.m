(*
    Implementation of the Apriori algorithm in Mathematica
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
(* This package contains definitions for the Apriori algorithm application. *)

BeginPackage["AprioriAlgorithm`"]

AprioriApplication::usage = "AprioriApplication[setOfItemSets,minProb,opts] returns a list of three elements: the association sets with indexes, the item to indexes rules, and the indexes to item rules. The association sets appear in setOfItemSets with frequency that is at least minProb. AprioriApplication takes the option \"MaxNumberOfItems\" -> (All | _Integer) ."

AssociationRules::usage = "AssociationRules[setOfItemSets,assocItemSet,minConfidence] finds the possible association rules for assocItemSet using setOfItemSets that have confidence at least minConfidence and calculates for each of the rules the measures: Support, Confidence, Lift, Leverage, and Conviction. AssociationRules[setOfItemSets,assocItemSets,minConfidence,minSupport] takes the association sets from assocItemSets that have support at least minSupport and finds the association rules for them."

Support::usage = "Support[setOfItemSets, itemSet] gives the fraction of the sets in setOfItemSets that contain itemSet."

QuantileReplacementFunc::usage = "QuantileReplacementFunc[qBoundaries] makes a piece-wise function for mapping of a real value to the enumerated intervals Partition[Join[{-Infinity}, qBoundaries, {Infinity}], 2, 1]."

RymonTree::usage = "RymonTree[numberOfItems] gives the Rymon tree for numberOfItems."

TreeToRules::usage = "TreeToRules[tree] returns rules for the argument tree that can be used in GraphPlot."

Begin["`Private`"]

(* Rymon tree *)
Clear[RymonTree, RymonChildren]
RymonChildren[set : {_Integer ...}, m_Integer, n_Integer] :=
  Block[{},
   If[m < n,
    Map[Append[set, #] &, Range[m + 1, n]],
    {}]
   ];

RymonTree[set : {_Integer ...}, n_Integer] :=
  Block[{m},
   m = If[set === {}, 0, Max[set]];
   If[m < n,
    Prepend[
     DeleteCases[RymonTree[#, n] & /@ RymonChildren[set, m, n], {}], 
     set],
    {set}
    ]
   ];

RymonTree[n_Integer] :=
  Block[{},
   RymonTree[{}, n]
   ];

(* Convert to rules *)

Clear[TreeToRules]
TreeToRules[tree_] :=
  Which[
   tree === {}, {},
   Rest[tree] === {}, {},
   True, Join[Map[tree[[1]] -> #[[1]] &, Rest[tree], {1}], 
    Flatten[TreeToRules[#] & /@ Rest[tree], 1]]
   ];

(* AprioriGenerator *)

(* It assumed that the item sets are sorted and i.e. come from a Rymon tree. (See the "Most[F[[i]]] == Most[F[[j]]]" line.) *)

Clear[AprioriGenerator];
AprioriGenerator[Mu_, F_] :=
  Block[{res},
   res = {};
   Do[
    If[Most[F[[i]]] == Most[F[[j]]],
     (*AppendTo[res,Union[F\[LeftDoubleBracket]i\[RightDoubleBracket],
     F\[LeftDoubleBracket]j\[RightDoubleBracket]]]*) 
     (* the line above is probably slower than the line below *)
     
     AppendTo[res, Join[Most[F[[i]]], {Last[F[[i]]]}, {Last[F[[j]]]}]]
     ],
    {i, 1, Length[F]}, {j, i + 1, Length[F]}];
   PRINT[res];
   Select[res, Apply[And, MemberQ[F, #] & /@ Subsets[#, {Length[#] - 1}]] &]
   ];

(* AprioriAlgorithm *)

Clear[Support, AprioriAlgorithm]

Support[T_, s_] := 
  Support[T, s] = Count[T, d_ /; Intersection[d, s] == s]/Length[T];

Options[AprioriAlgorithm] = {"MaxNumberOfItems" -> All};
AprioriAlgorithm[T : {{_Integer ...} ...}, Mu_?NumberQ, opts : OptionsPattern[]] :=
 Block[{CSet, FSet, i = 1, F = {}, contQ = True, 
    maxNumberOfItems = OptionValue[AprioriAlgorithm, "MaxNumberOfItems"]},
   If[maxNumberOfItems === All, maxNumberOfItems = \[Infinity]];
   CSet = List /@ Range[Min[T], Max[T]];
   While[CSet =!= {} && contQ,
    FSet = Pick[CSet, Support[T, #] >= Mu & /@ CSet];
    AppendTo[F, FSet];
    If[FSet =!= {} && Length[FSet[[-1]]] < maxNumberOfItems,
     CSet = AprioriGenerator[Mu, FSet],
     contQ = False
     ];
    i++
    ];
   F
  ];


(* AssociationRules *)

(* For the basket given as an argument is calculated and returned:
Confidence, Lift, Leverage, Conviction, Condition, Implication *)

Clear[AssociationRules]
AssociationRules[T : {{_Integer ...} ...}, basketArg : {_Integer ...}, confidence_?NumberQ] :=
  Block[{basket = Sort[basketArg], basketSupport, antecedents, consequents, t},
   basketSupport = N[Support[T, basket]];
   antecedents = Most@Rest@Subsets[basket];
   consequents = Complement[basket, #] & /@ antecedents;
   t =
   SortBy[
    Select[
     MapThread[{N[basketSupport/Support[T, #1]], 
        N[(basketSupport/Support[T, #1])/Support[T, #2]], 
        N[basketSupport - Support[T, #1]*Support[T, #2]], 
        N[If[(1 - basketSupport/Support[T, #1]) == 0, 
          1000, (1 - Support[T, #2])/(1 - 
             basketSupport/Support[T, #1])]], #1, #2} &, {antecedents, 
       consequents}], #[[1]] >= confidence &],
    -#[[1]] &];
    Prepend[#,basketSupport]& /@ t
   ];

AssociationRules[dataWithIDs : {{_Integer ..} ..}, aprioriResRecsArg : {{_Integer ..} ...}, minConfidence_? NumberQ, minSupport_?NumberQ] := 
   Block[{eligible,aprioriResRecs=Sort/@aprioriResRecsArg},
    eligible = Select[Transpose[{aprioriResRecs, N[Support[dataWithIDs, #] & /@ aprioriResRecs]}], #[[2]] >= minSupport &];
    If[Length[eligible] == 0, {},
     Flatten[#,1]& @
     MapThread[
      Function[{assoc, supp},
         DeleteCases[AssociationRules[dataWithIDs, assoc, minConfidence], {}]],
      Transpose[eligible]
     ]
    ]
   ];

(* AprioriApplcation *)

(* Returns the association sets with indexes, the item to idexes rules, and the idexes to item rules. *)

Clear[AprioriApplication];
AprioriApplication[itemLists : {_List ...}, Mu_?NumberQ, opts : OptionsPattern[]] := 
  Block[{uniqueItemToIDRules, uniqueItems, dataWithIDs},
    uniqueItems = Union[Flatten[itemLists]];
    uniqueItemToIDRules = 
     Dispatch[Thread[uniqueItems -> Range[1, Length[uniqueItems]]]];
    dataWithIDs = itemLists /. uniqueItemToIDRules;
    dataWithIDs = Sort /@ (dataWithIDs);
    {AprioriAlgorithm[dataWithIDs, Mu, opts], uniqueItemToIDRules, 
     Dispatch[Reverse /@ uniqueItemToIDRules[[1]]]}
    ] /; 0 < Mu < 1;


(* Supporting Defintions *)

Clear[QuantileReplacementFunc]
QuantileReplacementFunc[qBoundaries : {_?NumberQ ...}] :=
  Block[{XXX, t = Partition[Join[{-\[Infinity]}, qBoundaries, {\[Infinity]}], 2, 1]},
   Function[
    Evaluate[Piecewise[
       MapThread[{#2, #1[[1]] < XXX <= #1[[2]]} &, {t, 
         Range[1, Length[t]]}]] /. {XXX -> #}]]
   ];

End[]

EndPackage[]