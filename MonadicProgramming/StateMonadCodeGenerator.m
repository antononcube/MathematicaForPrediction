(*
    State monad code generator Mathematica package
    Copyright (C) 2017  Anton Antonov

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
    antononcube @ gmail.com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2017 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: StateMonadCodeGenerator *)
(* :Context: StateMonadCodeGenerator` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-06-05 *)

(* :Package Version: 0.8 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:


    ## Introduction

    The code generator in this package adhere to the so called Monadic Programming paradigm.
    The idea is that instead of having monad types we generate monad codes for given prefixes of monad functions names.

    Here is the programming monad definition used (taken from Wikipedia, [1]):

      A monad is created by defining a type constructor M and two operations,
      bind and return (where return is often also called unit):

      1. The unary return operation takes a value from a plain type (a) and puts it into a container using
         the constructor, creating a monadic value (with type M a).

      2. The binary bind operation ">>=" takes as its arguments a monadic value with type M a and
         a function (a -> M b) that can transform the value.

          2.1. The bind operator unwraps the plain value with type a embedded in its input monadic value with type M a,
               and feeds it to the function.

          2.2. The function then creates a new monadic value, with type M b,
               that can be fed to the next bind operators composed in the pipeline.


    ## State monads generation

    This generates code of a monad the functions of which have prefixes "StMon" :

        GenerateStateMonadCode["StMon"]

    The monad pipeline objects have the form

        StMon[ value_, context_Association ]

    Every function in the monad pipeline should return a result in that form. (Per point 2. of the definition.)

    The failure symbol of the generated state monad is `None`. The option "FailureSymbol" can be used to
    specify a different symbol.

    By default the binding function -- `StMonBind` in this case -- overloads the operator `NonCommutativeMultiply`.
    This allows concise pipeline specification. (See the example.)


    ## Contexts
    
    The contexts are assumed to be Association objects, but if the state monad functions are generated with
    the option `"StringContextNames" -> True`,

            GenerateStateMonadCode["StMon", "StringContextNames" -> True]

    then the pipeline objects have the form

            StMon[ value_, context: (_String | _Association) ]

    If a string S is given as a context within a pipeline then an attempt is made in `StMonBind` to replace S with
    `StMonContextes[S]` before proceeding with the binding.

    The keys of the `Association` contexts are expected to be strings made of word characters.
    (I.e. this function `StringMatch[#, WordCharacter..]&` gives `True` applied to each key.)


    ## Base functions

    The base State monad functions give access to the value and the context and allow changing and modifying contexts.

    Here are the access functions:

        Names["StMonEcho*"]
        (* {"StMonEchoContext",
            "StMonEchoFunctionContext",
            "StMonEchoFunctionValue",
            "StMonEchoValue"} *)

    Here are the state changing functions:

        Complement[Names["StMon*Context"], Names["StMonEcho*"]]
        (* {"StMonModifyContext", "StMonPutContext"} *)

    The optional failure function

        StMonOption[f_][x_,context_Association]

    returns `StMon[x,context]` if `f[x]` would produce failure.


    ### Adding the current pipeline value to the context

    Adding the current pipeline value to the context associated with the key "data" can be done in two ways:

    1. with `StMonAddToContext["data"] ⟹`, or

    2. with `(StMon[#1, Join[#2, <|"data" -> #1|>]]&) ⟹` .


    ## Example

    Here is an example:

        GenerateStateMonadCode["StMon"]

        SeedRandom[34]
        StMon[RandomReal[{0, 1}, {3, 2}], <|"mark" -> "None", "threshold" -> 0.5|>] ⟹
           StMonEchoValue ⟹
           StMonEchoContext ⟹
           StMonAddToContext["data"] ⟹
           (StMon[#1 /. (x_ /; x < #2["threshold"] :> #2["mark"]), #2] &) ⟹
           StMonEchoValue ⟹
           StMonModifyContext[Join[#1, <|"mark" -> "Lesser", "threshold" -> 0.8|>] &] ⟹
           StMonEchoContext ⟹
           (StMon[#2["data"] /. (x_ /; x < #2["threshold"] :> #2["mark"]), #2] &) ⟹
           StMonEchoValue;

        (*
        {{0.789884,0.831468},{0.421298,0.50537},{0.0375957,0.289442}}

        <|mark->None,threshold->0.5|>

        {{0.789884,0.831468},{None,0.50537},{None,None}}

        <|mark->Lesser,threshold->0.8,data->{{0.789884,0.831468},{0.421298,0.50537},{0.0375957,0.289442}}|>

        {{Lesser,0.831468},{Lesser,Lesser},{Lesser,Lesser}}
        *)

    In the example code above:

       - we generated the code for the monad `StMon`,

       - then we started a pipeline with a monad object made of

         - 3x2 random real matrix, and

         - a context that holds values associated with "mark" and "threshold".

    In example's monadic pipeline:

       - pipeline's current value is added to pipeline's context with `StMonAddToContext`;

       - numbers of the matrix that are less than the context threshold are replaced with the context mark;

       - at some point pipeline's context is replaced with a new context by `StMonModifyContext`;

       - pipeline's current value and context are shown by `StMonEchoValue` and `StMonEchoContext` respectively.


    ## Extension functions

    Project specific, extension functions have the signatures

        _StMonNewFunc[xs_, context_Association]

    or
        _StMonNewFunc[f_][xs_, context_Association]

    Here is an example of a function that splits the current value and just passes the current context:

        StMonSplitData[_][None] := None
        StMonSplitData[fr_?NumberQ][xs_, context_Association] :=
           StMon[AssociationThread[{"trainData", "testData"} ->
                                   TakeDrop[xs, Floor[fr*Length[xs]]]], context] /; 0 < fr <= 1;


    ## Context modules

    Instead of making calls like

        (StMon[#1 /. (x_ /; x < #2["threshold"] :> #2["mark"]), #2] &) ⟹

    in the example above we can make the call

        StMonModule[$Value /. (x_ /; x < threshold :> mark)] ⟹

    The elements of the context are turned into symbol assignments by the package function `AssociationModule`
    (implemented by Mr.Wizard in [2].)

    The variable `$Value` is for the current value of the pipeline.


    ## It is a monad indeed

    Let us show that `StMon` satisfies the monad laws.

    In monad laws formulas given below

    - ">>=" denotes the monad binding operation,

    - "===" stands for "is the same as", and

    - an expression of the form `(x -> expr)` is for a function in anonymous form.


    #### Left identity:

        unit a >>= f === f a

    #### Right identity:

        m >>= unit === m

    #### Associativity:

        (m >>= f) >>= g === m >>= (x -> f x >>= g)


    ### Verification

    Note, that instead of the binding symbol ">>=" the code uses the binding infix operator "⟹".

    - Left identity:

        StMon[a, <|"k1" -> "v1"|>] ⟹ f

        (* f[a, <|"k1" -> "v1"|>] *)

    - Right identity:

        StMon[a, <|"k1" -> "v1"|>] ⟹ StMon

        (* StMon[a, <|"k1" -> "v1"|>] *)

    - Associativity:

        (StMon[a, <|"k1" -> "v1"|>] ⟹ (StMon[f1[#1, #2], #2] &)) ⟹ (StMon[f2[#1, #2], #2] &)

        (* StMon[f2[f1[a, <|"k1" -> "v1"|>], <|"k1" -> "v1"|>], <|"k1" -> "v1"|>] *)

        StMon[a, <|"k1" -> "v1"|>] ⟹ Function[{x, c}, StMon[f1[x, c], c] ⟹ (StMon[f2[#1, #2], #2] &)]

        (* StMon[f2[f1[a, <|"k1" -> "v1"|>], <|"k1" -> "v1"|>], <|"k1" -> "v1"|>] *)

        %% == %
        (* True *)


    ## References

    [1] Wikipedia entry: Monad (functional programming),
        URL: https://en.wikipedia.org/wiki/Monad_(functional_programming) .

    [2] Mathematica Stack Exchange discussion,
        "Functions with changeable global variables",
        URL: https://mathematica.stackexchange.com/q/134381/34008 .


    ## End matters

    This file was created by Mathematica Plugin for IntelliJ IDEA.

    Anton Antonov
    Windermere, FL, USA
    2017-06-05
*)

BeginPackage["StateMonadCodeGenerator`"]
(* Exported symbols added here with SymbolName::usage *)

GenerateStateMonadCode::usage = "GenerateStateMonadCode[monadName_String] generates the basic functions \
of a State monad that allows computations with a mutable context. Code for handling context string names \
is generated depending on the Boolean values of the option \"StringContextNames\". \
Monad's failure symbol is specified with the option \"FailureSymbol\"."

AssociationModule::usage = "AssociationModule[asc_Association, body_] transforms the elements of asc into \
symbol assignments ascAssign and executes Module[ ascAssign, body ]. The keys of asc are assumed to be strings."

Begin["`Private`"]

Attributes[AssociationModule] = HoldRest;
AssociationModule[asc_Association, body_] :=
    Replace[Join @@
        Cases[Hold @@ Normal @@ {asc},
          h_[L : _Symbol | _String, R_] :>
              With[{sy = Quiet@Symbol@ToString@L},
                Hold[h[sy, R]] /; Depth[sy] === 1]], {(a_ -> b_) :> (a = b), (a_ :> b_) :> (a := b)}, {1}] /.
                    _[sets__] :> Module[{sets}, body]

ClearAll[GenerateStateMonadCode]
Options[GenerateStateMonadCode] = {"StringContextNames" -> True, "FailureSymbol" -> None, "EchoFailingFunction"->True};
GenerateStateMonadCode[monadName_String, opts : OptionsPattern[]] :=
    With[{
      MState = ToExpression[monadName],
      MStateUnit = ToExpression[monadName <> "Unit"],
      MStateUnitQ = ToExpression[monadName <> "UnitQ"],
      MStateBind = ToExpression[monadName <> "Bind"],
      MStateFail = ToExpression[monadName <> "Fail"],
      MStateSucceed = ToExpression[monadName <> "Succeed"],
      MStateEchoValue = ToExpression[monadName <> "EchoValue"],
      MStateEchoFunctionValue = ToExpression[monadName <> "EchoFunctionValue"],
      MStateEchoContext = ToExpression[monadName <> "EchoContext"],
      MStateEchoFunctionContext = ToExpression[monadName <> "EchoFunctionContext"],
      MStatePutContext = ToExpression[monadName <> "PutContext"],
      MStateModifyContext = ToExpression[monadName <> "ModifyContext"],
      MStateAddToContext = ToExpression[monadName <> "AddToContext"],
      MStateRetrieveFromContext = ToExpression[monadName <> "RetrieveFromContext"],
      MStateOption = ToExpression[monadName <> "Option"],
      MStateWhen = ToExpression[monadName <> "When"],
      MStateIfElse = ToExpression[monadName <> "IfElse"],
      MStateModule = ToExpression[monadName <> "Module"],
      MStateContexts = ToExpression[monadName <> "Contexts"],
      MStateFailureSymbol = OptionValue["FailureSymbol"],
      MStateEchoFailingFunction = TrueQ[OptionValue["EchoFailingFunction"]]
    },

      ClearAll[MState, MStateUnit, MStateUnitQ, MStateBind, MStateFail, MStateSucceed,
        MStateEchoValue, MStateEchoFunctionValue,
        MStateEchoContext, MStateEchoFunctionContext,
        MStatePutContext, MStateModifyContext,
        MStateAddToContext, MStateRetrieveFromContext,
        MStateOption, MStateWhen, MStateModule];

      (* What are the assumptions for monad's failure symbol? *)
      (*If[ !MemberQ[Attributes[MStateFailureSymbol], System`Protected]], ClearAll[MStateFailureSymbol] ];*)

      MStateBind::ffail = "Fail when applying: `1`";
      MStateContexts::nocxt = "The string \"`1`\" does not refer to a known context.";
      MStateContexts::nocxtp = MStateContexts::nocxt <> " Associating with an empty context and proceeding.";

      MStateFail[__] := MStateFailureSymbol;

      MStateSucceed[][__] := MState[{}];
      MStateSucceed[s_][__] := MState[s];

      MStateUnit[MStateFailureSymbol] := MStateFailureSymbol;
      MStateUnit[x_] := MState[x, <||>];
      MStateUnit[{x_, c:(_String|_Association)}] := MState[x,c];
      MStateUnit[ x_, c:(_String|_Association) ] := MState[x,c];

      MStateUnitQ[x_] := MatchQ[x, MStateFailureSymbol] || MatchQ[x, MState[_, _Association]];

      MStateBind[MStateFailureSymbol] := MStateFailureSymbol;
      MStateBind[MState[x_, context_Association], f_] :=
          Block[{res = f[x, context]},
            If[FreeQ[res, MStateFailureSymbol], res,
              If[MStateEchoFailingFunction,
                Echo[TemplateApply[StringTemplate[MStateBind::ffail], HoldForm[f]]]
              ];
              MStateFailureSymbol
            ]
          ];
      If[TrueQ[OptionValue["StringContextNames"]],
        MStateBind[MState[x_, context_String], f_] :=
            Block[{res},
              If[! MatchQ[MStateContexts[context], _Association],
                Echo[TemplateApply[StringTemplate[MStateContexts::nocxtp], context]];
                MStateContexts[context] = <||>;
              ];
              res = f[x, MStateContexts[context]];
              Which[
                ! FreeQ[res, MStateFailureSymbol],
                If[MStateEchoFailingFunction,
                  Echo[TemplateApply[StringTemplate[MStateBind::ffail], HoldForm[f]]]
                ];
                MStateFailureSymbol,
                StringQ[res[[2]]], res,
                MatchQ[res,MState[_,_]], MStateContexts[context] = res[[2]]; MState[res[[1]], context],
                True, MStateFailureSymbol
              ]
            ];
      ];
      MStateBind[___] := MStateFailureSymbol;

      MStateEchoValue[MStateFailureSymbol] := (Echo[MStateFailureSymbol]; MStateFailureSymbol);
      MStateEchoValue[x_, context_Association] := (Echo[x,"value:"]; MState[x, context]);

      MStateEchoFunctionValue[f___][MStateFailureSymbol] := (Echo[MStateFailureSymbol]; MStateFailureSymbol);
      MStateEchoFunctionValue[f___][x_, context_Association] := (EchoFunction[f][x]; MState[x, context]);

      MStateEchoContext[MStateFailureSymbol] := (Echo[MStateFailureSymbol]; MStateFailureSymbol);
      MStateEchoContext[x_, context_Association] := (Echo[context,"context:"]; MState[x, context]);

      MStateEchoFunctionContext[f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateEchoFunctionContext[f___][x_, context_Association] := (EchoFunction[f][context]; MState[x, context]);

      MStatePutContext[MStateFailureSymbol] := MStateFailureSymbol;
      MStatePutContext[newContext_Association][x_, context_Association] := MState[x, newContext];
      If[TrueQ[OptionValue["StringContextNames"]],
        MStatePutContext[newContext_String][x_, context_Association] :=
            If[! MatchQ[MStateContexts[newContext], _Association],
              Echo[TemplateApply[StringTemplate[MStateContexts::nocxt], newContext]];
              MStateFailureSymbol,
              MState[x, newContext]
            ];
      ];

      MStateModifyContext[f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateModifyContext[f_][x_, context_Association] := MState[x, f[context]];

      MStateAddToContext[MStateFailureSymbol] := MStateFailureSymbol;
      MStateAddToContext[varName_String][x_, context_Association] := MState[x, Join[context,<|varName->x|>]];

      MStateRetrieveFromContext[MStateFailureSymbol] := MStateFailureSymbol;
      MStateRetrieveFromContext[varName_String][x_, context_Association] := MState[context[varName], context];

      MStateOption[f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateOption[f_][xs_, context_] :=
          Block[{res = f[xs, context]}, If[FreeQ[res, MStateFailureSymbol], res, MState[xs, context]]];

      MStateIfElse[testFunc_, fYes_, fNo_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateIfElse[testFunc_, fYes_, fNo_][xs_, context_] :=
          Block[{testRes = testFunc[xs, context]}, If[TrueQ[testRes], fYes[xs, context], fNo[xs, context]]];

      MStateWhen[testFunc_, f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateWhen[testFunc_, f_][xs_, context_] := MStateIfElse[testFunc, f, MState][xs, context];

      Attributes[MStateModule] = HoldAll;
      MStateModule[body___][value_, context_Association] :=
          MState[AssociationModule[Join[context, <|"$Value" -> value|>], body], context];

      (* We should have an option for the pipeline symbol. *)
      (* This looks much more like a pipeline operator than (**): *)
      DoubleLongRightArrow[Global`x_?MStateUnitQ, Global`f_] := MStateBind[Global`x, Global`f];
      DoubleLongRightArrow[Global`x_, Global`y_, Global`z__] :=
          DoubleLongRightArrow[DoubleLongRightArrow[Global`x, Global`y], Global`z];

      (*Unprotect[NonCommutativeMultiply];*)
      (*ClearAttributes[NonCommutativeMultiply, Attributes[NonCommutativeMultiply]]*)
      (*MState /: NonCommutativeMultiply[MState[Global`x_], Global`f_] := MStateBind[MState[Global`x], Global`f];*)
      (*NonCommutativeMultiply[Global`x_, Global`y_, Global`z__] :=*)
      (*NonCommutativeMultiply[NonCommutativeMultiply[Global`x, Global`y], Global`z];*)

    ];

End[] (* `Private` *)

EndPackage[]
