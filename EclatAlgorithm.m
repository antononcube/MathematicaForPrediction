(*
    Implementation of the Eclat algorithm in Mathematica
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

(* Version 2.0 *)

(* :Title: EclatAlgorithm *)
(* :Context: EclatAlgorithm` *)
(* :Author: antonov *)
(* :Date: 2022-06-16 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2022 antonov *)
(* :Keywords: *)
(* :Discussion: *)


If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Echo["CrossTabulate.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
];

If[Length[DownValues[SSparseMatrix`ToSSparseMatrix]] == 0,
  Echo["SSparseMatrix.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/SSparseMatrix.m"]
];

(***********************************************************)
(* Package definition                                      *)
(***********************************************************)

BeginPackage["EclatAlgorithm`"];
(* Exported symbols added here with SymbolName::usage *)

Eclat::usage = "Eclat[data, minSupport, opts] finds frequent sets using the Eclat algorithm.";

Begin["`Private`"];

Needs["CrossTabulate`"];
Needs["SSparseMatrix`"];

(***********************************************************)
(* EclatIntersect                                          *)
(***********************************************************)

Clear[EclatIntersect];
EclatIntersect[aTransactions_?AssociationQ, items : {_String ..}] :=
    Block[{},
      If[KeyExistsQ[aTransactions, items],
        aTransactions[items],
        (*ELSE*)
        Fold[#1*aTransactions[#2] &, aTransactions[First@items], Rest[items]]
      ]
    ];

EclatIntersect[aTransactions_?AssociationQ, items1 : {_String ..}, items2 : {_String ..}] :=
    EclatIntersect[aTransactions, items1] * EclatIntersect[aTransactions, items2];

(***********************************************************)
(* EclatExtendTransactions                                 *)
(***********************************************************)

Clear[EclatExtendTransactions];

EclatExtendTransactions[aTransactions_?AssociationQ, items : {_String ..}] :=
    Append[aTransactions, items -> EclatIntersect[aTransactions, items]];

EclatExtendTransactions[aTransactions_?AssociationQ, items1 : {_String ..}, items2 : {_String ..}] :=
    Append[aTransactions, Union[Join[items1, items2]] -> EclatIntersect[aTransactions, items1, items2]];

(***********************************************************)
(* EclatSupport                                            *)
(***********************************************************)

Clear[EclatSupport];
EclatSupport[aTransactions_?AssociationQ, items : {_String ..}] :=
    Total[EclatIntersect[aTransactions, items], 2];

EclatSupport[aTransactions_?AssociationQ, items1 : {_String ..}, items2 : {_String ..}] :=
    Total[EclatIntersect[aTransactions, items1, items2], 2];


(***********************************************************)
(* Eclat                                                   *)
(***********************************************************)

Clear[Eclat];

Options[Eclat] = {"MaxNumberOfItems" -> Infinity, "Separator" -> "."};

aECLATTransactions = None;

Eclat[dsTransactions_Dataset, minSupport_?NumberQ, opts : OptionsPattern[]] :=
    Block[{t, sep = OptionValue[Eclat, "Separator"]},
      t = Normal[dsTransactions[All, Association @ KeyValueMap[#1 -> ToString[#1] <> sep <> ToString[#2] &, #] &][Values]];
      Eclat[t, minSupport, opts]
    ];

Eclat[lsTransactions : {_List ..}, minSupport_?NumberQ,
  opts : OptionsPattern[]] :=
    Block[{t},
      t = Join @@ MapIndexed[Thread[{#2[[1]], #1}] &, lsTransactions];
      t = ToSSparseMatrix[CrossTabulate[t]];
      Eclat[t, minSupport, opts]
    ];

Eclat[matTransactions_SSparseMatrix, minSupport_?NumericQ, opts : OptionsPattern[]] :=
    Block[{P = List /@ ColumnNames[matTransactions], aTransactions, res},
      aTransactions =
          AssociationThread[ColumnNames[matTransactions] -> Map[Identity, Transpose[SparseArray[matTransactions]]]];
      Eclat[aTransactions, minSupport, opts]
    ];

Eclat[aTransactions : Association[(_ -> _?SparseArrayQ) ..], minSupport_?NumericQ, opts : OptionsPattern[]] :=
    Block[{P = List /@ Sort[Keys[aTransactions]], res},
      P = Select[P, EclatSupport[aTransactions, #] >= minSupport &];
      aECLATTransactions = aTransactions;
      res = EclatRec[aTransactions, P, minSupport, {}, 0, opts];
      AssociationThread[res, EclatSupport[aECLATTransactions, #] & /@ res]
    ];

(*---------------------------------------------------------*)

Clear[EclatRec];
Options[EclatRec] = Options[Eclat];
EclatRec[aTransactions_?AssociationQ, P_List, minSupport_?NumericQ, Farg_List, k_Integer, opts : OptionsPattern[]] :=
    Block[{F = Farg, maxNumberOfItems, P2 = {}, Xab, tXab, PRINT},
      maxNumberOfItems = OptionValue[EclatRec, "MaxNumberOfItems"];
      PRINT[Style[Row[{"rec : ", k}], Purple, Bold]];
      PRINT["P: ", P];
      Do[

        AppendTo[F, Xa];
        PRINT["F: ", F];
        P2 = {};

        Do[
          Xab = Union[Xa, Xb];
          PRINT["{Xa,Xb} : ", {Xa, Xb}];
          If[Length[Xab] <= maxNumberOfItems,
            aECLATTransactions = EclatExtendTransactions[aECLATTransactions, Xa, Complement[Xab, Xa]];
            tXab = EclatSupport[aECLATTransactions, Xab];
            (*tXab=EclatSupport[aECLATTransactions,Xab];*)

            PRINT["Xab->tXab : ", Xab -> tXab];
            If[tXab >= minSupport,
              AppendTo[P2, Xab];
            ]
          ];
          PRINT["P2: ", P2]
          , {Xb, Select[P, Order[Xa, #] > 0 &]}];

        If[Length[P2] > 0,
          F = EclatRec[aTransactions, P2, minSupport, F, k + 1, opts]
        ]

        , {Xa, Select[P, Length[#] <= maxNumberOfItems &]}];
      F
    ];

End[]; (* `Private` *)

EndPackage[]