(*
    Implementation of the Apriori algorithm in Mathematica
    Copyright (C) 2014-2016  Anton Antonov

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

(* Version 2.0 *)
(* This package contains definitions for the Apriori algorithm application. *)

(* October, 30, 2014

   Updated the function definitions to use Mathematica 10.1 dispatch rules.
*)

(* February, 20, 2014 

   Updated the package with a faster implementation of the Apriori algorithm that uses sparse algebra operations.
   The most significant change is of the function Support.

   The new definition scales quite well with the number of baskets. 
   For example, the timing for 20,000 baskets was only ~12% larger than the timing for 2000 baskets, both sets having the same 1355 items.

   Renamed the functions of the older, original version into "Appriori*Original" .

   Overloaded the definition of Support.

   At this time it is not clear to me what is the best interface of AssociationRules for the end user.
   Nevertheless, I made AssociationRules to take both a list of integer sets and a list of sparse arrays.

   Added a new function, AprioriSparseArrayRepresentation, the result of which can be used with the overloaded version of Support, and for AssociationRules.
*)

(* January, 06, 2016
   Added a new function, ItemRules, that makes it easier to obtain association rules for specified items using the
   results of AprioriApplication.
*)

BeginPackage["AprioriAlgorithm`"];

AprioriApplication::usage = "AprioriApplication[setOfItemSets,minProb,opts] returns a list of three elements: \
association sets with indexes, item to indexes rules, and indexes to item rules. \
The association sets appear in setOfItemSets with frequency that is at least minProb. \
AprioriApplication takes the option \"MaxNumberOfItems\" -> (All | _Integer) .";

AprioriApplicationOriginal::usage = "AprioriApplicationOriginal[setOfItemSets,minProb,opts] returns a list of three elements: \
association sets with indexes, item to indexes rules, and indexes to item rules. \
The association sets appear in setOfItemSets with frequency that is at least minProb. \
AprioriApplication takes the option \"MaxNumberOfItems\" -> (All | _Integer) . \
This is the original, slower version of AprioriApplication.";

AprioriSparseArrayRepresentation::usage = "AprioriSparseArrayRepresentation[setOfItemSets] returns a list of three elements: \
(1) a 0-1 matrix M (a list of sparse arrays) for which M[[i,j]]==1 if the i-th item belongs to setOfItemSets[[j]], \
(2) item to indexes rules, and (3) indexes to item rules.";

AssociationRules::usage = "AssociationRules[setOfItemSets,assocItemSet,minConfidence] finds the possible association rules \
for assocItemSet using setOfItemSets that have confidence at least minConfidence and calculates for each of the rules the measures: \
Support, Confidence, Lift, Leverage, and Conviction. AssociationRules[setOfItemSets,assocItemSets,minConfidence,minSupport] \
takes the association sets from assocItemSets that have support at least minSupport and finds the association rules for them.";

Support::usage = "Support[setOfItemSets, itemSet] gives the fraction of the sets in setOfItemSets that contain itemSet.";

QuantileReplacementFunc::usage = "QuantileReplacementFunc[qBoundaries] makes a piece-wise function \
for mapping of a real value to the enumerated intervals Partition[Join[{-Infinity}, qBoundaries, {Infinity}], 2, 1].";

RymonTree::usage = "RymonTree[numberOfItems] gives the Rymon tree for numberOfItems.";

TreeToRules::usage = "TreeToRules[tree] returns rules for the argument tree that can be used in GraphPlot.";

ItemRules::usage = "ItemRules[setOfItemSets, frequentSetsOfIDs, itemToIDRules, idToItemRules, itemSpec, minConfidence, minSupport, nAssocItems] \
finds rules for a specified item or list if items using the baskets data and the result of AprioriApplication.";


Begin["`Private`"];

(* Rymon tree *)
Clear[RymonTree, RymonChildren];
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

Clear[TreeToRules];
TreeToRules[tree_] :=
    Which[
      tree === {}, {},
      Rest[tree] === {}, {},
      True, Join[Map[tree[[1]] -> #[[1]] &, Rest[tree], {1}],
      Flatten[TreeToRules[#] & /@ Rest[tree], 1]]
    ];

(* AprioriGenerator *)

(* It is assumed that the item sets are sorted and i.e. come from a Rymon tree. (See the "Most[F[[i]]] == Most[F[[j]]]" line.) *)

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

(* AprioriAlgorithmOriginal *)

Clear[Support, AprioriAlgorithmOriginal];

Support[T_, s_] :=
    Support[T, s] = Count[T, d_ /; Intersection[d, s] == s] / Length[T];

Options[AprioriAlgorithmOriginal] = {"MaxNumberOfItems" -> All};
AprioriAlgorithmOriginal[T : {{_Integer ...} ...}, Mu_?NumberQ, opts : OptionsPattern[]] :=
    Block[{CSet, FSet, i = 1, F = {}, contQ = True,
      maxNumberOfItems = OptionValue[AprioriAlgorithmOriginal, "MaxNumberOfItems"]},
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


(* AprioriAlgorithmOriginal *)
(* These two functions provide the sparse array implementation. *)

Clear[AprioriAlgorithm];

(* I overloaded the non-sparse array Support definition with this one because Support is provided as package function. *)
Support[Tcolumns : {_SparseArray ..}, s : {_Integer ..}] :=
    Which[
      Length[s] == 1, Total[Tcolumns[[s[[1]]]]],
      Length[s] == 2, Tcolumns[[s[[1]]]].Tcolumns[[s[[2]]]],
      True,
      Total[Fold[Times[#1, Tcolumns[[#2]]] &, Tcolumns[[s[[1]]]], Rest[s]]]
    ] / Length[Tcolumns[[1]]];

(* This definition is almost exact copy of the previous one, AprioriAlgorithmOriginal, given above. *)
Options[AprioriAlgorithm] = {"MaxNumberOfItems" -> All};
AprioriAlgorithm[Tcolumns : {_SparseArray ...}, Mu_?NumberQ, opts : OptionsPattern[]] :=
    Block[{CSet, FSet, i = 1, F = {}, contQ = True,
      maxNumberOfItems = OptionValue[AprioriAlgorithm, "MaxNumberOfItems"]},
      If[maxNumberOfItems === All, maxNumberOfItems = \[Infinity]];
      CSet = List /@ Range[1, Length[Tcolumns]];
      While[CSet =!= {} && contQ,
        FSet =
            Pick[CSet, Support[Tcolumns, #] >= Mu & /@ CSet];
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

Clear[AssociationRules];
AssociationRules[T : ({{_Integer ...} ...} | {_SparseArray ..}), basketArg : {_Integer ...}, confidence_?NumberQ] :=
    Block[{basket = Sort[basketArg], basketSupport, antecedents, consequents, t},
      basketSupport = N[Support[T, basket]];
      antecedents = Most@Rest@Subsets[basket];
      consequents = Complement[basket, #] & /@ antecedents;
      t =
          SortBy[
            Select[
              MapThread[{
                N[basketSupport / Support[T, #1]],
                N[(basketSupport / Support[T, #1]) / Support[T, #2]],
                N[basketSupport - Support[T, #1] * Support[T, #2]],
                N[If[(1 - basketSupport / Support[T, #1]) == 0,
                  1000,
                  (1 - Support[T, #2]) / (1 - basketSupport / Support[T, #1])
                ]],
                #1,
                #2} &,
                {antecedents, consequents}
              ],
              #[[1]] >= confidence &],
            -#[[1]] &];
      Prepend[#, basketSupport]& /@ t
    ] /; If[! MatchQ[T, {_SparseArray ..}], True, Apply[And, Map[1 <= # <= Length[T] &, basketArg]]];

AssociationRules[T : ({{_Integer ...} ...} | {_SparseArray ..}), aprioriResRecsArg : {{_Integer ..} ...}, minConfidence_? NumberQ, minSupport_?NumberQ] :=
    Block[{eligible, aprioriResRecs = Sort /@ aprioriResRecsArg},
      eligible = Select[Transpose[{aprioriResRecs, N[Support[T, #] & /@ aprioriResRecs]}], #[[2]] >= minSupport &];
      If[Length[eligible] == 0, {},
        Flatten[#, 1]& @
            MapThread[
              Function[{assoc, supp},
                DeleteCases[AssociationRules[T, assoc, minConfidence], {}]],
              Transpose[eligible]
            ]
      ]
    ];

(* AprioriApplcationOriginal *)

(* Returns the association sets with indexes, the item to idexes rules, and the idexes to item rules. *)
(* This is the original implementation that does not use sparse algebra, hence it is much slower. *)

Clear[AprioriApplicationOriginal];
AprioriApplicationOriginal[itemLists : {_List ...}, Mu_?NumberQ, opts : OptionsPattern[]] :=
    Block[{uniqueItemToIDRules, uniqueItems, dataWithIDs},
      uniqueItems = Union[Flatten[itemLists]];
      uniqueItemToIDRules =
          Dispatch[Thread[uniqueItems -> Range[1, Length[uniqueItems]]]];
      dataWithIDs = itemLists /. uniqueItemToIDRules;
      dataWithIDs = Sort /@ (dataWithIDs);
      {AprioriAlgorithmOriginal[dataWithIDs, Mu, opts], uniqueItemToIDRules,
        Dispatch[Reverse /@ Normal[uniqueItemToIDRules]]}
    ] /; 0 < Mu < 1;

(* AprioriApplcation *)

(* Returns the association sets with indexes, the item to idexes rules, and the idexes to item rules. *)
(* This definition is almost the same as the original one above. I don't see the point having the two 
  definitions accessed through an option value, so I kept the definitions separated. *)

Clear[AprioriSparseArrayRepresentation];
AprioriSparseArrayRepresentation[itemLists : {_List ...}] :=
    Block[{uniqueItemToIDRules, uniqueItems, dataWithIDs, arrayRules, Tcolumns},
      uniqueItems = Union[Flatten[itemLists]];
      uniqueItemToIDRules =
          Dispatch[Thread[uniqueItems -> Range[1, Length[uniqueItems]]]];
      dataWithIDs = itemLists /. uniqueItemToIDRules;
      dataWithIDs = Sort /@ (dataWithIDs);
      arrayRules =
          Flatten[MapIndexed[Thread[Thread[{#2[[1]], #1}] -> 1] &, dataWithIDs], 1];
      Tcolumns = Map[# &, Transpose[SparseArray[arrayRules]]];
      {Tcolumns, uniqueItemToIDRules, Dispatch[Reverse /@ Normal[uniqueItemToIDRules]]}
    ];

Clear[AprioriApplication];
Options[AprioriApplication] = {"MaxNumberOfItems" -> All};
AprioriApplication[itemLists : {_List ...}, Mu_?NumberQ, opts : OptionsPattern[]] :=
    Block[{Tcolumns, uniqueItemToIDRules, uniqueIDToItemRules, mni},
      mni = OptionValue[AprioriApplication, "MaxNumberOfItems"];
      {Tcolumns, uniqueItemToIDRules, uniqueIDToItemRules} =
          AprioriSparseArrayRepresentation[itemLists];
      {AprioriAlgorithm[Tcolumns, Mu, "MaxNumberOfItems" -> mni], uniqueItemToIDRules,
        Dispatch[Reverse /@ Normal[uniqueItemToIDRules]]}
    ] /; 0 < Mu < 1;

(* Supporting Definitions *)

Clear[QuantileReplacementFunc];
QuantileReplacementFunc[qBoundaries : {_?NumberQ ...}] :=
    Block[{XXX, t = Partition[Join[{-\[Infinity]}, qBoundaries, {\[Infinity]}], 2, 1]},
      Function[
        Evaluate[Piecewise[
          MapThread[{#2, #1[[1]] < XXX <= #1[[2]]} &, {t,
            Range[1, Length[t]]}]] /. {XXX -> #}]]
    ];


(* Item rules *)

Clear[ItemRules];

ItemRules[setOfItemSets_, frequentSetsOfIDs : {{{_Integer ..} ..} ..},
  itemToIDRules_, idToItemRules_, itemSpec_, minConfidence_?NumberQ,
  minSupport_?NumberQ] :=
    ItemRules[setOfItemSets, frequentSetsOfIDs, itemToIDRules, idToItemRules, itemSpec, minConfidence, minSupport, All];

ItemRules[setOfItemSets_, frequentSetsOfIDs : {{{_Integer ..} ..} ..},
  itemToIDRules_, idToItemRules_, itemSpec : (_?AtomQ | {_?AtomQ ..}),
  minConfidence_?NumberQ, minSupport_?NumberQ,
  nAssocItems : (All | _Integer)] :=
    Block[{itemAprioriRes, basketItemRows, t, r1, r2, items, itemsInds},
      {basketItemRows, r1, r2} = AprioriSparseArrayRepresentation[setOfItemSets];
      If[ListQ[itemSpec] && Length[itemSpec] == 1, items = itemSpec[[1]], items = itemSpec ];
      If[AtomQ[items],
        itemAprioriRes =
            Cases[#, {___, items /. itemToIDRules, ___}, Infinity] & /@ frequentSetsOfIDs,
        (* ELSE *)
        itemsInds = items /. itemToIDRules;
        itemAprioriRes =
            Cases[#, r_List /; Length[Intersection[r, itemsInds]] == Length[itemsInds], 3] & /@ frequentSetsOfIDs
      ];
      t = AssociationRules[basketItemRows, itemAprioriRes[[#]], minConfidence, minSupport] & /@
          If[TrueQ[nAssocItems === All], Range[2, Length[itemAprioriRes]], {nAssocItems}];
      DeleteCases[t /. idToItemRules, {}, 2]
    ];

End[];

EndPackage[];