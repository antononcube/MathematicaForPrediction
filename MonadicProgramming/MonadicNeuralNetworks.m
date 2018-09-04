(*
    Monadic Neural Networks Mathematica package
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

(* :Title: MonadicNeuralNetworks *)
(* :Context: MonadicNeuralNetworks` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-08-18 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: neural net, monad *)
(* :Discussion:
*
*
* *)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MathematicaForPredictionUtilities`RecordsSummary]] == 0,
  Echo["MathematicaForPredictionUtilities.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"]
];

If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["MonadicNeuralNetworks`"];

$NetMonFailure::usage = "Failure symbol for the monad NetMon."

NetMonSetNet::usage = "Assigns the argument to the key \"net\" in the monad context."

NetMonTakeNet::usage = "Gives the value of the key \"net\" from the monad context."

NetMonSetEncoder::usage = "Assigns the argument to the key \"encoder\" in the monad context."

NetMonTakeEncoder::usage = "Gives the value of the key \"encoder\" from the monad context."

NetMonSetDecoder::usage = "Assigns the argument to the key \"decoder\" in the monad context."

NetMonTakeDecoder::usage = "Gives the value of the key \"decoder\" from the monad context."

NetMonSetTrainedNet::usage = "Assigns the argument to the key \"trainedNet\" in the monad context."

NetMonTakeTrainedNet::usage = "Gives the value of the key \"trainedNet\" from the monad context."

NetMonTakeNetTrainResultsObject::usage = "Gives the value of the key \"netTrainResultsObject\" from the monad context."

NetMonTrain::usage = "Train the network. If no data is given the pipeline value is used."

Begin["`Private`"];

Needs["MathematicaForPredictionUtilities`"]
Needs["StateMonadCodeGenerator`"]


(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

(* Generate base functions of NetMon monad (through StMon.) *)

GenerateStateMonadCode[ "MonadicQuantileRegression`NetMon", "FailureSymbol" -> $NetMonFailure, "StringContextNames" -> False ]

(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)


(**************************************************************)
(* Training                                                   *)
(**************************************************************)

ClearAll[NetMonTrain];

NetMonTrain[$NetMonFailure] := $NetMonFailure;

NetMonTrain[xs_, context_] := NetMonTrain[][xs, context];

NetMonTrain[opts:OptionsPattern[]][xs_, context_] :=
    Block[{},
      If[DataRulesForClassifyQ[xs],
        NetMonTrain[xs, opts][xs, context]
      ]
    ];

NetMonTrain[trainingData_?DataRulesForClassifyQ, opts:OptionsPattern[]][xs_, context_] :=
    Block[{res},
      res = NetTrain[context["netChain"], trainingData, All, opts]
      NetMonUnit[res, <||>]
    ];

NetMonTrain[___][__] := $NetMonFailure;

(**************************************************************)
(* Setters / getters                                          *)
(**************************************************************)


End[]; (* `Private` *)

EndPackage[]