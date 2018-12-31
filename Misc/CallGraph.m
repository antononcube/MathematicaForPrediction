(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)

(* :Title: CallGraph *)
(* :Context: CallGraph` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-12-31 *)

(* :Package Version: 0.6 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["CallGraph`"];
(* Exported symbols added here with SymbolName::usage *)

CallGraph::usage = "CallGraph[contexts:{_String..}, opts___] makes a call graph for the functions of \
specified (package) contexts."

Begin["`Private`"];

Clear[SymbolQ]
SymbolQ[x_] := Head[x] === Symbol;


Clear[CallGraph]

Options[CallGraph] = { "PrivateContexts" -> False, "SelfReferencing" -> False, "UsageTooltips" -> True };

CallGraph[context_String, opts:OptionsPattern[] ] := CallGraph[{context}, opts ];

CallGraph[contexts:{_String..}, opts:OptionsPattern[] ] :=
    Block[{pSymbs, pPrivateSymbs, dvs, dRes, aDependencyRules, gRules},

      pSymbs =
          Flatten@
              Map[
                Function[{c},
                  Block[{p = Names[c <> "*"]},
                    Select[Map[ToExpression[c <> #] &, p], Head[#] === Symbol &]]
                ], contexts];

      If[ TrueQ[OptionValue[CallGraph, "PrivateContexts"]],
        pPrivateSymbs =
            Flatten@
                Map[ToExpression[Names[# <> "Private`*"]] &, contexts];

        pSymbs = Join[pSymbs, pPrivateSymbs];
      ];

      dvs = AssociationThread[pSymbs, MapThread[Join, {DownValues /@ pSymbs, SubValues /@ pSymbs}] ];
      dvs = Select[dvs, Length[#] > 0 &];

      Block[{pSymbs = Keys[dvs]},
        dRes = AssociationThread[
          pSymbs ->
              Map[Function[{s}, Map[! FreeQ[HoldPattern[#], s] &, dvs]], pSymbs]];
      ];

      aDependencyRules = Map[Pick[Keys[#], Values[#]] &, dRes];

      gRules = Reverse /@ Flatten[Thread /@ Normal[aDependencyRules]];

      (*Delete the self-referencing rules:*)
      If[ !TrueQ[OptionValue[CallGraph, "SelfReferencing"]],
        gRules = DeleteCases[gRules, x_ -> x_];
      ];

      If[ TrueQ[OptionValue[CallGraph, "UsageTooltips"]],
        gRules = Map[Tooltip[#, #::usage] &, gRules, {2}];
      ];

      Graph[gRules, VertexLabels -> "Name"]
    ];

End[]; (* `Private` *)

EndPackage[]