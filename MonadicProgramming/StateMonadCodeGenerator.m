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

    This generates code of a monad the functions of which have prefixes "MState" :

        GenerateStateMonadCode["MState"]

    The monad pipeline objects have the form

        MState[ value_, context_Association ]

    Every function in the monad pipeline should return a result in that form. (Per point 2. of the definition.)

    The failure symbol of the generated state monad is `None`. The option "FailureSymbol" can be used to
    specify a different symbol.

    By default the binding function -- `MStateBind` in this case -- overloads the operator `NonCommutativeMultiply`.
    This allows concise pipeline specification. (See the example.)


    ## Contexts
    
    The contexts are assumed to be Association objects, but if the state monad functions are generated with
    the option `"StringContextNames" -> True`,

            GenerateStateMonadCode["MState", "StringContextNames" -> True]

    then the pipeline objects have the form

            MState[ value_, context: (_String | _Association) ]

    If a string S is given as a context within a pipeline then an attempt is made in `MStateBind` to replace S with
    `MStateContextes[S]` before proceeding with the binding.

    The keys of the `Association` contexts are expected to be strings made of word characters.
    (I.e. this function `StringMatch[#, WordCharacter..]&` gives `True` applied to each key.)


    ## Base functions

    The base State monad functions give access to the value and the context and allow changing and modifying contexts.

    Here are the access functions:

        Names["MStateEcho*"]
        (* {"MStateEchoContext",
            "MStateEchoFunctionContext",
            "MStateEchoFunctionValue",
            "MStateEchoValue"} *)

    Here are the state changing functions:

        Complement[Names["MState*Context"], Names["MStateEcho*"]]
        (* {"MStateModifyContext", "MStatePutContext"} *)

    The optional failure function

        MStateOption[f_][x_,context_Association]

    returns `MState[x,context]` if `f[x]` would produce failure.


    ### Adding the current pipeline value to the context

    Adding the current pipeline value to the context associated with the key "data" can be done in two ways:

    1. with `MStateAddToContext["data"] ⟹`, or

    2. with `(MState[#1, Join[#2, <|"data" -> #1|>]]&) ⟹` .


    ## Example

    Here is an example:

        GenerateStateMonadCode["MState"]

        SeedRandom[34]
        MState[RandomReal[{0, 1}, {3, 2}], <|"mark" -> "None", "threshold" -> 0.5|>] ⟹
           MStateEchoValue ⟹
           MStateEchoContext ⟹
           MStateAddToContext["data"] ⟹
           (MState[#1 /. (x_ /; x < #2["threshold"] :> #2["mark"]), #2] &) ⟹
           MStateEchoValue ⟹
           MStateModifyContext[Join[#1, <|"mark" -> "Lesser", "threshold" -> 0.8|>] &] ⟹
           MStateEchoContext ⟹
           (MState[#2["data"] /. (x_ /; x < #2["threshold"] :> #2["mark"]), #2] &) ⟹
           MStateEchoValue;

        (*
        {{0.789884,0.831468},{0.421298,0.50537},{0.0375957,0.289442}}

        <|mark->None,threshold->0.5|>

        {{0.789884,0.831468},{None,0.50537},{None,None}}

        <|mark->Lesser,threshold->0.8,data->{{0.789884,0.831468},{0.421298,0.50537},{0.0375957,0.289442}}|>

        {{Lesser,0.831468},{Lesser,Lesser},{Lesser,Lesser}}
        *)

    In the example code above:

       - we generated the code for the monad `MState`,

       - then we started a pipeline with a monad object made of

         - 3x2 random real matrix, and

         - a context that holds values associated with "mark" and "threshold".

    In example's monadic pipeline:

       - pipeline's current value is added to pipeline's context with `MStateAddToContext`;

       - numbers of the matrix that are less than the context threshold are replaced with the context mark;

       - at some point pipeline's context is replaced with a new context by `MStateModifyContext`;

       - pipeline's current value and context are shown by `MStateEchoValue` and `MStateEchoContext` respectively.


    ## Extension functions

    Project specific, extension functions have the signatures

        _MStateNewFunc[xs_, context_Association]

    or
        _MStateNewFunc[f_][xs_, context_Association]

    Here is an example of a function that splits the current value and just passes the current context:

        MStateSplitData[_][None] := None
        MStateSplitData[fr_?NumberQ][xs_, context_Association] :=
           MState[AssociationThread[{"trainData", "testData"} ->
                                   TakeDrop[xs, Floor[fr*Length[xs]]]], context] /; 0 < fr <= 1;


    ## Context modules

    Instead of making calls like

        (MState[#1 /. (x_ /; x < #2["threshold"] :> #2["mark"]), #2] &) ⟹

    in the example above we can make the call

        MStateModule[$Value /. (x_ /; x < threshold :> mark)] ⟹

    The elements of the context are turned into symbol assignments by the package function `AssociationModule`
    (implemented by Mr.Wizard in [2].)

    The variable `$Value` is for the current value of the pipeline.


    ## It is a monad indeed

    Let us show that `MState` satisfies the monad laws.

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

        MState[a, <|"k1" -> "v1"|>] ⟹ f

        (* f[a, <|"k1" -> "v1"|>] *)

    - Right identity:

        MState[a, <|"k1" -> "v1"|>] ⟹ MState

        (* MState[a, <|"k1" -> "v1"|>] *)

    - Associativity:

        (MState[a, <|"k1" -> "v1"|>] ⟹ (MState[f1[#1, #2], #2] &)) ⟹ (MState[f2[#1, #2], #2] &)

        (* MState[f2[f1[a, <|"k1" -> "v1"|>], <|"k1" -> "v1"|>], <|"k1" -> "v1"|>] *)

        MState[a, <|"k1" -> "v1"|>] ⟹ Function[{x, c}, MState[f1[x, c], c] ⟹ (MState[f2[#1, #2], #2] &)]

        (* MState[f2[f1[a, <|"k1" -> "v1"|>], <|"k1" -> "v1"|>], <|"k1" -> "v1"|>] *)

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

BeginPackage["StateMonadCodeGenerator`"];
(* Exported symbols added here with SymbolName::usage *)

GenerateStateMonadCode::usage = "GenerateStateMonadCode[monadName_String] generates the basic functions \
of a State monad that allows computations with a mutable context. Code for handling context string names \
is generated depending on the Boolean values of the option \"StringContextNames\". \
Monad's failure symbol is specified with the option \"FailureSymbol\".";

AssociationModule::usage = "AssociationModule[asc_Association, body_] transforms the elements of asc into \
symbol assignments ascAssign and executes Module[ ascAssign, body ]. The keys of asc are assumed to be strings.";

GenerateMonadDroper::usage = "GenerateMonadDroper[monadName_String, elementName_String] generates monadic \
droper functions for specified monad and element names.";

GenerateMonadSetter::usage = "GenerateMonadSetter[monadName_String, elementName_String] generates monadic \
setter functions for specified monad and element names.";

GenerateMonadTaker::usage = "GenerateMonadTaker[monadName_String, elementName_String] generates monadic \
taker functions for specified monad and element names.";

GenerateMonadAccessors::usage = "GenerateMonadAccessors[monadName_String, elementNames:{_String..}] generates monadic \
droper, setter, and taker functions for specified monad and element names.";

Begin["`Private`"];

Attributes[AssociationModule] = HoldRest;
AssociationModule[asc_Association, body_] :=
    Replace[Join @@
        Cases[Hold @@ Normal @@ {asc},
          h_[L : _Symbol | _String, R_] :>
              With[{sy = Quiet@Symbol@ToString@L},
                Hold[h[sy, R]] /; Depth[sy] === 1]], {(a_ -> b_) :> (a = b), (a_ :> b_) :> (a := b)}, {1}] /.
        _[sets__] :> Module[{sets}, body]

ClearAll[GenerateStateMonadCode]
Options[GenerateStateMonadCode] = {"StringContextNames" -> True, "FailureSymbol" -> None, "EchoFailingFunction" -> True};
GenerateStateMonadCode[monadName_String, opts : OptionsPattern[]] :=
    With[{
      MState = ToExpression[monadName],
      MStateUnit = ToExpression[monadName <> "Unit"],
      MStateUnitQ = ToExpression[monadName <> "UnitQ"],
      MStateBind = ToExpression[monadName <> "Bind"],
      MStateFail = ToExpression[monadName <> "Fail"],
      MStateSucceed = ToExpression[monadName <> "Succeed"],
      MStateEcho = ToExpression[monadName <> "Echo"],
      MStateEchoValue = ToExpression[monadName <> "EchoValue"],
      MStateEchoFunctionValue = ToExpression[monadName <> "EchoFunctionValue"],
      MStateEchoContext = ToExpression[monadName <> "EchoContext"],
      MStateEchoFunctionContext = ToExpression[monadName <> "EchoFunctionContext"],
      MStateTakeContext = ToExpression[monadName <> "TakeContext"],
      MStateTakeValue = ToExpression[monadName <> "TakeValue"],
      MStatePutContext = ToExpression[monadName <> "PutContext"],
      MStatePutValue = ToExpression[monadName <> "PutValue"],
      MStateModifyContext = ToExpression[monadName <> "ModifyContext"],
      MStateAddToContext = ToExpression[monadName <> "AddToContext"],
      MStateRetrieveFromContext = ToExpression[monadName <> "RetrieveFromContext"],
      MStateDropFromContext = ToExpression[monadName <> "DropFromContext"],
      MStateOption = ToExpression[monadName <> "Option"],
      MStateWhen = ToExpression[monadName <> "When"],
      MStateIfElse = ToExpression[monadName <> "IfElse"],
      MStateIterate = ToExpression[monadName <> "Iterate"],
      MStateIf = ToExpression[monadName <> "If"],
      MStateNest = ToExpression[monadName <> "Nest"],
      MStateNestWhile = ToExpression[monadName <> "NestWhile"],
      MStateFold = ToExpression[monadName <> "Fold"],
      MStateModule = ToExpression[monadName <> "Module"],
      MStateContexts = ToExpression[monadName <> "Contexts"],
      MStateFailureSymbol = OptionValue["FailureSymbol"],
      MStateEchoFailingFunction = TrueQ[OptionValue["EchoFailingFunction"]],
      MStateSetContext = ToExpression[monadName <> "SetContext"],
      MStateSetValue = ToExpression[monadName <> "SetValue"]
    },

      ClearAll[MState, MStateUnit, MStateUnitQ, MStateBind, MStateFail, MStateSucceed, MStateEcho,
        MStateEchoValue, MStateEchoFunctionValue,
        MStateEchoContext, MStateEchoFunctionContext,
        MStatePutContext, MStatePutValue, MStateModifyContext,
        MStateAddToContext, MStateRetrieveFromContext,
        MStateOption, MStateWhen, MStateIfElse, MStateIterate,
        MStateIf, MStateNest, MStateNestWhile, MStateFold,
        MStateModule, MStateContexts,
        MStateSetContext, MStateSetValue
      ];

      (* What are the assumptions for monad's failure symbol? *)
      (*If[ !MemberQ[Attributes[MStateFailureSymbol], System`Protected]], ClearAll[MStateFailureSymbol] ];*)

      MStateBind::ffail = "Failure when applying: `1`";
      MStateBind::mscxt = "The result is missing a context. Reusing the context argument.";
      MStateContexts::nocxt = "The string \"`1`\" does not refer to a known context.";
      MStateContexts::nocxtp = MStateContexts::nocxt <> " Associating with an empty context and proceeding.";

      MStateFail[__] := MStateFailureSymbol;
      MStateFail[][___] := MStateFailureSymbol;
      (*MStateFail[echoArgs__][x_, c:(_String|_Association)] := (Echo[echoArgs]; MStateFailureSymbol);*)

      MStateSucceed[___] := MState[{}];
      MStateSucceed[][__] := MState[{}];
      MStateSucceed[s__][___] := MState[s];

      MStateUnit[MStateFailureSymbol] := MStateFailureSymbol;
      MStateUnit[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateUnit[] := MState[None, <||>];
      MStateUnit[x_] := MState[x, <||>];
      MStateUnit[{x_, c : (_String | _Association)}] := MState[x, c];
      MStateUnit[ x_, c : (_String | _Association) ] := MState[x, c];

      MStateUnitQ[x_] := MatchQ[x, MStateFailureSymbol] || MatchQ[x, MState[_, _Association]];

      MStateBind[MStateFailureSymbol] := MStateFailureSymbol;
      MStateBind[MState[x_, context_Association], f_] :=
          Block[{res = f[x, context]},
            Which[
              !FreeQ[res, MStateFailureSymbol],

              If[MStateEchoFailingFunction,
                Echo[
                  TemplateApply[
                    StringTemplate[MStateBind::ffail],
                    If[ LeafCount[HoldForm[f]] > 200, Short[HoldForm[f]], HoldForm[f] ]
                  ], ToString[MStateBind] <> ":" ]
              ];
              MStateFailureSymbol,

              MatchQ[res, MState[_]],

              If[MStateEchoFailingFunction,
                Echo[
                  TemplateApply[
                    StringTemplate[MStateBind::mscxt],
                    If[ LeafCount[HoldForm[f]] > 200, Short[HoldForm[f]], HoldForm[f] ]
                  ], ToString[MStateBind] <> ":"]
              ];
              MState[res[[1]], context],

              True, res
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
                  Echo[
                    TemplateApply[
                      StringTemplate[MStateBind::ffail],
                      If[ LeafCount[HoldForm[f]] > 200, Short[HoldForm[f]], HoldForm[f] ]
                    ], ToString[MStateBind] <> ":"]
                ];
                MStateFailureSymbol,
                StringQ[res[[2]]], res,
                MatchQ[res, MState[_, _]], MStateContexts[context] = res[[2]]; MState[res[[1]], context],
                True, MStateFailureSymbol
              ]
            ];
      ];
      MStateBind[___] := MStateFailureSymbol;
      MStateBind::usage = "Monad binding function.";

      MStateEcho[MStateFailureSymbol] := MStateFailureSymbol;
      MStateEcho[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateEcho[echoArgs__][x_, context_Association] := (Echo[echoArgs]; MState[x, context]);
      MStateEcho[x_, context_Association] := (Echo[Short[MState[x, context]]]; MState[x, context]);
      MStateEcho[][x_, context_Association] := MStateEcho[x, context];
      MStateEcho::usage = "Echoes the argument. If no argument is given the short print of the monad object is echoed.";


      MStateEchoValue[MStateFailureSymbol] := (Echo[MStateFailureSymbol]; MStateFailureSymbol);
      MStateEchoValue[x_, context_Association] := (Echo[x, "value:"]; MState[x, context]);

      MStateEchoValue[][MStateFailureSymbol] := MStateEchoValue[MStateFailureSymbol];
      MStateEchoValue[][x_, context_Association] := MStateEchoValue[x, context];
      MStateEchoValue::usage = "Echoes the monad value.";


      MStateEchoFunctionValue[f___][MStateFailureSymbol] := (Echo[MStateFailureSymbol]; MStateFailureSymbol);
      MStateEchoFunctionValue[f___][x_, context_Association] := (EchoFunction[f][x]; MState[x, context]);
      MStateEchoFunctionValue::usage = "Echoes function application over the monad value.";


      MStateEchoContext[MStateFailureSymbol] := (Echo[MStateFailureSymbol]; MStateFailureSymbol);
      MStateEchoContext[x_, context_Association] := (Echo[context, "context:"]; MState[x, context]);

      MStateEchoContext[][MStateFailureSymbol] := MStateEchoContext[MStateFailureSymbol];
      MStateEchoContext[][x_, context_Association] := MStateEchoContext[x, context];
      MStateEchoContext::usage = "Echoes the monad context.";


      MStateEchoFunctionContext[f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateEchoFunctionContext[f___][x_, context_Association] := (EchoFunction[f][context]; MState[x, context]);
      MStateEchoFunctionContext::usage = "Echoes function application over the monad context.";


      MStateTakeValue[MStateFailureSymbol] := MStateFailureSymbol;
      MStateTakeValue[x_, context_] := x;

      MStateTakeValue[][MStateFailureSymbol] := MStateFailureSymbol;
      MStateTakeValue[][x_, context_] := x;
      MStateTakeValue::usage = "Takes the monad value.";


      MStateTakeContext[MStateFailureSymbol] := MStateFailureSymbol;
      MStateTakeContext[x_, context_] := context;

      MStateTakeContext[][MStateFailureSymbol] := MStateFailureSymbol;
      MStateTakeContext[][x_, context_] := context;
      MStateTakeContext::usage = "Takes the monad context.";


      MStatePutContext[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStatePutContext[newContext_Association][x_, context_Association] := MState[x, newContext];
      If[TrueQ[OptionValue["StringContextNames"]],
        MStatePutContext[newContext_String][x_, context_Association] :=
            If[! MatchQ[MStateContexts[newContext], _Association],
              Echo[TemplateApply[StringTemplate[MStateContexts::nocxt], newContext]];
              MStateFailureSymbol,
              MState[x, newContext]
            ];
      ];
      MStatePutContext::usage = "Replaces the monad context with the argument.";


      MStateSetContext = MStatePutContext;

      MStatePutValue[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStatePutValue[newValue_][x_, context_] := MState[newValue, context];
      MStatePutValue::usage = "Replaces the monad value with the argument.";


      MStateSetValue = MStatePutValue;

      MStateModifyContext[f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateModifyContext[f_][x_, context_Association] := MState[x, f[context]];
      MStateModifyContext::usage = SymbolName[MState] <> "ModifyContext[f] replaces the monad context f[context].";


      MStateAddToContext[MStateFailureSymbol] := MStateFailureSymbol;
      MStateAddToContext[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateAddToContext[varName_String][x_, context_Association] := MState[x, Join[context, <|varName -> x|>]];
      MStateAddToContext[][x_Association, context_Association] := MState[{}, Join[context, x]];
      MStateAddToContext[x_Association, context_Association] := MState[{}, Join[context, x]];
      MStateAddToContext[arg_Association][x_, context_Association] := MState[x, Join[context, arg]];
      MStateAddToContext::usage =
          SymbolName[MState] <> "AddToContext[varName_String] adds to the monad context the monad value under key varName.\n" <>
              SymbolName[MState] <> "AddToContext[arg_Association] joins the monad context with arg.\n" <>
              SymbolName[MState] <> "AddToContext[] joins the monad context with the monad value.";


      MStateRetrieveFromContext[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateRetrieveFromContext[varName_String][x_, context_Association] := MState[context[varName], context];
      MStateRetrieveFromContext::usage =
          SymbolName[MState] <> "RetrieveFromContext[varName_String] retrieves from the monad context the value of the key varName.";


      MStateDropFromContext[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateDropFromContext[varNames : (_String | {_String..})][x_, context_Association] := MState[x, KeyDrop[context, varNames]];
      MStateDropFromContext::usage = "Drop from the monad context elements withe specified keys.";


      MStateOption[f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateOption[f_][xs_, context_] :=
          Block[{res = f[xs, context]}, If[FreeQ[res, MStateFailureSymbol], res, MState[xs, context]]];
      MStateOption::usage =
          "If the application of the argument to the monad produces monad failure the monad is unchanged.";


      MStateIfElse[testFunc_, fYes_, fNo_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateIfElse[testFunc_, fYes_, fNo_][xs_, context_] :=
          Block[{testRes = testFunc[xs, context]},
            If[TrueQ[testRes], fYes[xs, context], fNo[xs, context]]
          ];
      MStateIfElse::usage =
          SymbolName[MState] <> "IfElse[testFunc_, fYes_, fNo_] if TrueQ[testFunc[xs, context]] then fYes[xs, context], otherwise fNo[xs, context].";


      MStateWhen[testFunc_, f_][MStateFailureSymbol] := MStateFailureSymbol;
      MStateWhen[testFunc_, f_][xs_, context_] := MStateIfElse[testFunc, f, MStateUnit][xs, context];
      MStateWhen::usage = "Shorter version of " <> SymbolName[MState] <> "IfElse.";


      (* Iteration functions *)
      MStateIterate[___][___] := MStateFailureSymbol;

      MStateIterate[itFunc : (Nest | NestWhile | FixedPoint), f_, args___][x_, context_Association] :=
          itFunc[MStateBind[#, f] &, MStateUnit[x, context], args];

      MStateIterate[itFunc : (NestList | NestWhileList | FixedPointList),
        f_, args___, contextVar : (None | _String) : None][x_, context_Association] :=
          Block[{res},
            res = itFunc[MStateBind[#, f] &, MStateUnit[x, context], args];
            If[contextVar === None,
              MStateUnit[res[[All, 1]], res[[-1, 2]]],
              (*ELSE*)
              MStateUnit[res[[All, 1]], Join[res[[-1, 2]], <|contextVar -> res|>]]
            ]
          ];

      MStateIterate[itFunc : (Fold | FoldList | Composition[__, FoldList]),
        f_, args___][x_, context_Association] :=
          itFunc[f, MStateUnit[x, context], args];

      (* Flow functions  with non-monadic function argument *)
      (* If, Nest, NestWhile, Fold *)
      MStateIf[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateIf[f_, fYes_][xs_, context_] := If[f[MStateUnit[xs, context]], fYes[MStateUnit[xs, context]]];
      MStateIf[f_, fYes_, fNo_][xs_, context_] :=
          If[f[MStateUnit[xs, context]],
            fYes[MStateUnit[xs, context]],
            fNo[MStateUnit[xs, context]]
          ];
      MStateIf[f_, fYes_, fNo_, fMu_][xs_, context_] :=
          If[f[MStateUnit[xs, context]],
            fYes[MStateUnit[xs, context]],
            fNo[MStateUnit[xs, context]],
            fMu[MStateUnit[xs, context]]
          ];
      MStateIf[___][xs_, context : (_Association | _String)] := MStateFailureSymbol;

      MStateNest[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateNest[f_, n_Integer][xs_, context_] := Nest[f, MStateUnit[xs, context], n];

      MStateNestWhile[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateNestWhile[f_, args__][xs_, context_] := NestWhile[f, MStateUnit[xs, context], args];

      MStateFold[___][MStateFailureSymbol] := MStateFailureSymbol;
      MStateFold[f_, list_][xs_, context_] := Fold[f, MStateUnit[xs, context], list];

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


Clear[GenerateMonadSetter]
Options[GenerateMonadSetter] = {"FailureSymbol" -> None};
GenerateMonadSetter[monadName_String, elementName_String, opts : OptionsPattern[]] :=
    With[{
      MStateUnit = ToExpression[monadName <> "Unit"],
      MStateSetter = ToExpression[monadName <> "Set" <> Capitalize[elementName]],
      MStateFailureSymbol = OptionValue["FailureSymbol"],
      dElementName = Decapitalize[elementName]
    },

      ClearAll[MStateSetter];

      MStateSetter[MStateFailureSymbol] := MStateFailureSymbol;
      MStateSetter[][MStateFailureSymbol] := MStateFailureSymbol;
      MStateSetter[xs_, context_] := MStateFailureSymbol;
      MStateSetter[arg_?AtomQ][xs_, context_] := MStateUnit[ xs, Join[ context, <|dElementName -> arg|> ] ];
      MStateSetter[arg_List][xs_, context_] := MStateUnit[ xs, Join[ context, <|dElementName -> arg|> ] ];
      MStateSetter[args__][xs_, context_] := MStateSetter[{args}][xs, context];
      MStateSetter[__][___] := MStateFailureSymbol;

      MStateSetter::usage = "Assigns the argument to the key \"" <> dElementName <> "\" in the monad context."
    ];


Clear[GenerateMonadTaker]
Options[GenerateMonadTaker] = {"FailureSymbol" -> None};
GenerateMonadTaker[monadName_String, elementName_String, opts : OptionsPattern[]] :=
    With[{
      MState = ToExpression[monadName],
      MStateTaker = ToExpression[monadName <> "Take" <> Capitalize[elementName]],
      MStateFailureSymbol = OptionValue["FailureSymbol"],
      dElementName = Decapitalize[elementName]
    },

      ClearAll[MStateTaker];

      MStateTaker[MStateFailureSymbol] := MStateFailureSymbol;
      MStateTaker[][MStateFailureSymbol] := MStateFailureSymbol;
      MStateTaker[xs_, context_] := context[dElementName];
      MStateTaker[][xs_, context_] := context[dElementName];
      MStateTaker[__][___] := MStateFailureSymbol;

      MStateTaker::usage = "Gives the value of the key \"" <> dElementName <> "\" from the monad context."
    ];


Clear[GenerateMonadDroper]
Options[GenerateMonadDroper] = {"FailureSymbol" -> None};
GenerateMonadDroper[monadName_String, elementName_String, opts : OptionsPattern[]] :=
    With[{
      MState = ToExpression[monadName],
      MStateDroper = ToExpression[monadName <> "Drop" <> Capitalize[elementName]],
      MStateFailureSymbol = OptionValue["FailureSymbol"],
      MStateDropFromContext = ToExpression[monadName <> "DropFromContext"],
      dElementName = Decapitalize[elementName]
    },

      ClearAll[MStateDroper];

      MStateDroper[MStateFailureSymbol] := MStateFailureSymbol;
      MStateDroper[][MStateFailureSymbol] := MStateFailureSymbol;
      MStateDroper[xs_, context_] := MStateDroper[][xs, context];
      MStateDroper[][xs_, context_] := MStateDropFromContext[dElementName][xs, context];
      MStateDroper[__][___] := MStateFailureSymbol;

      MStateDroper::usage = "Drops from the context the element with key \"" <> dElementName <> "\"."
    ];


Clear[GenerateMonadAccessors]
Options[GenerateMonadAccessors] = Options[GenerateMonadSetter];
GenerateMonadAccessors[monadName_String, elementName_String, opts : OptionsPattern[]] :=
    GenerateMonadAccessors[monadName, {elementName}, opts ];
GenerateMonadAccessors[monadName_String, elementNames : {_String..}, opts : OptionsPattern[]] :=
    Block[{},
      Map[GenerateMonadSetter[monadName, #, opts]&, elementNames];
      Map[GenerateMonadTaker[monadName, #, opts]&, elementNames];
      Map[GenerateMonadDroper[monadName, #, opts]&, elementNames];
    ];


End[] (* `Private` *)

EndPackage[]
