(*
    Implementation of the Apriori algorithm via Tries in Mathematica
    Copyright (C) 2022  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2022 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


If[Length[DownValues[TriesWithFrequencies`TriesCreate]] == 0,
  Echo["TriesWithFrequencies.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/TriesWithFrequencies.m"]
];

(***********************************************************)
(* Package definition                                      *)
(***********************************************************)

BeginPackage["AprioriAlgorithmViaTries`"];

Apriori::usage = "Apriori[baskets : {_List ..}, minSupport_?NumericQ, minItemsNumber_Integer, maxItemsNumberArg : (_Integer | Infinity)]";

Begin["`Private`"];

Needs["TriesWithFrequencies`"];

Clear[ScanBasket];
ScanBasket[basket_List, k_Integer, aFreqSets_?AssociationQ] :=
    Block[{candidates},
      candidates = Subsets[basket, {k, k}];
      Select[candidates, Lookup[aFreqSets, Key@Most[#], False] && Lookup[aFreqSets, Key@{Last[#]}, False] &]
    ] /; k > 1;

Clear[Apriori];
Options[Apriori] = {Counts -> False};
Apriori[
  lsBasketsArg : {_List ..},
  minSupportArg_?NumericQ,
  minItemsNumber_Integer : 1,
  maxItemsNumberArg : (_Integer | Infinity) : Infinity,
  opts :OptionsPattern[]] :=
    Block[{lsBaskets = lsBasketsArg, minSupport = minSupportArg, maxItemsNumber, countsQ,
      aFreqSets, lsFreqSets,
      trBase, trSets, trSets2, aAllTries, k = 1, contQ = True,
      res},

      (*Max number of items processing*)
      minSupport = If[ 0 <= minSupport <= 1, minSupport * Length[lsBaskets], minSupport];

      (*Max number of items processing*)
      maxItemsNumber = Min[maxItemsNumberArg, Max[Length /@ lsBaskets]];

      (*To use counts or not*)
      countsQ = TrueQ @ OptionValue[Apriori, Counts];

      (*Make sure all baskets unique items*)
      lsBaskets = Union /@ lsBaskets;

      (*Make single items baskets trie*)
      trBase = TrieCreate[List /@ Flatten[lsBaskets]];

      (*Remove the items that are not frequent enough*)
      trBase = TrieThresholdRemove[trBase, minSupport, "Postfix" -> None];

      (*Verify early stop*)
      If[TrieDepth[trBase] == 1,
        Echo[Row[{"All items have support smaller than", Spacer[3], minSupport}]];
        Return[{}]
      ];

      (*Initial set of frequent sets*)
      aFreqSets =
          AssociationThread[List /@ Select[Tally[Flatten[lsBaskets]], #[[2]] >= minSupport &][[All, 1]], True];

      (*First gathered trie*)
      aAllTries = <|k -> trBase|>;

      (*Main loop*)
      While[contQ && k < maxItemsNumber,

        k++;

        (*Scan the baskets and make trie with viable candidates*)
        trSets = TrieCreate[Join @@ Map[ScanBasket[#, k, aFreqSets] &, lsBaskets]];

        (*Remove baskets that are not frequent enough*)
        trSets2 = TrieThresholdRemove[trSets, minSupport, "Postfix" -> None];

        (*Get frequent sets from the trie*)
        lsNew = Select[Rest /@ TrieGetWords[trSets2], Length[#] == k &];

        (*Update frequent sets*)
        If[Length[lsNew] == 0,
          contQ = False,
          (*ELSE*)
          aFreqSets = Join[aFreqSets, AssociationThread[lsNew, True]];
          (*Add to gathered tries*)
          AppendTo[aAllTries, k -> trSets2]
        ];

      ];

      lsFreqSets = Select[Keys[aFreqSets], Length[#] >= minItemsNumber &];
      res = Association@Map[# -> N[TrieRetrieve[aAllTries[Length[#]], #][$TrieValue]] &, lsFreqSets];
      If[countsQ, res, res / Length[lsBaskets]]
    ];

End[];

EndPackage[];