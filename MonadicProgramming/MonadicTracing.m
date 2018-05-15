(*
    Monadic tracing Mathematica package
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

(* :Title: MonadicTracing *)
(* :Context: MonadicTracing` *)
(* :Author: Anton Antonov *)
(* :Date: 2017-06-13 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

    ## Introduction

    The monadic implementations in this file (not a package yet) allow tracking of the pipeline execution
    of functions with other monads.

    The primary reason for developing package was he desire to have the ability to print a tabulated trace
    of code and comments using the usual monad pipeline notation. (I.e. without conversion to strings etc.)

    From that perspective the main function is `TraceMonadEchoGrid`.

    ## Example

    In this example we can see that pipeline functions of the Maybe monad are interleaved with comment strings.
    Producing the grid of functions and comments happens "naturally" with the monad function `TraceMonadEchoGrid`.

        data = RandomInteger[10, 15];

        TraceMonadUnit[MaybeUnit[data]] ⟹ "lift to monad" ⟹
          TraceMonadEchoContext ⟹
          MaybeFilter[# > 3 &] ⟹ "filter current value" ⟹
          MaybeEcho ⟹ "display current value" ⟹
          MaybeWhen[#[[3]] > 3 &,
          MaybeEchoFunction[Style[#, Red] &]] ⟹
          (Maybe[#/4] &) ⟹
          MaybeEcho ⟹ "display current value again" ⟹
          TraceMonadEchoGrid[Grid[#, Alignment -> Left] &];

    Note that :
      1) the tracing is initiated by just using `TraceMonadUnit`;
      2) pipeline functions (actual code) and comments are interleaved;
      3) putting a comment string after a pipeline function is optional.


    ## Longer motivational discussion

    While writing an article on monadic programming in Mathematica / WL I made a function
    that tabulates code and comments. (See the definition below.)

    Here is an example:

        code = "
          FoldList[(* reduction function *)
            Plus,(* function to apply repeatedly *)
            0,(* initial value *)
            {1,2,3,3,100}(* arguments in repeated computations *)]";
        GridOfCodeAndComments[ code, "GridFunction" -> (Panel@Grid[#, Alignment -> Left] &)]

    I have several problems with the implementation of `GridOfCodeAndComments`,
    the main one being that I have to give a string to the function instead of (commented) code.

    For example, I would like to be able to write the tabulate code directly to `GridOfCodeAndComments`:

        GridOfCodeAndComments[
         FoldList[(* reduction function *)
            Plus,(* function to apply repeatedly *)
            0,(* initial value *)
            {1, 2, 3, 3, 100}(* arguments in repeated computations *)],
         "GridFunction" -> (Panel@Grid[#, Alignment -> Left] &)]


    See [2] for a larger discussion on that problem.


    ### Definition of GridOfCodeAndComments

        ClearAll[GridOfCodeAndComments]

        Options[GridOfCodeAndComments] = {"GridFunction" -> (Grid[#, Alignment -> Left] &)};

        GridOfCodeAndComments[code_String, opts : OptionsPattern[]] :=
          Block[{grData, codeLines, commentLines, comPat, gridFunc},

           gridFunc = OptionValue["GridFunction"];
           If[TrueQ[gridFunc === Automatic], gridFunc = (Grid[#, Alignment -> Left] &)];

           (* Split the code into lines *)
           codeLines = StringSplit[code, "\n"];

           (* Split each line into a {code, comment} pair *)
           comPat = ("(*" ~~ (Except["*"] ..) ~~ "*)");
           grData =
            Map[
             If[StringFreeQ[#, "("~~"*"], {#, ""},
               StringCases[#, (x__ ~~ y : (comPat) ~~ z___) :> {x <> z, y}][[1]]
             ] &, codeLines];

           (* Style the code and comments *)
           grData[[All, 1]] = Map[Style[#, "Input"] &, grData[[All, 1]]];
           grData[[All, 2]] =
           Map[Style[#, "CommentStyle" /. Options[$FrontEnd, AutoStyleOptions][[1, 2]]] &, grData[[All, 2]]];

           (* Show result *)
           gridFunc[grData]

          ];

    The problem gets simplified if we want to build code-comment grids for monad pipelines only.
    The resulting `TraceMonad` code is fairly simple, and demonstrates well the "programming semicolon"
    view of the binding operator in monadic programming.


    ## Monad code design and steps outline

    Generate a State monad code for the name `TraceMonad`.

    The `TraceMonad` has a context with keys "binder", "data", "commands", "comments".
    The values of "commands" and "comments" are incremented with new commands and comments
    given in the pipeline. Each command has a corresponding comment. (Empty comments are appended if needed.)

    Here are the steps for tracing a pipeline of a monad, e.g.

          Maybe[data] ⟹ MaybeEchoValue ⟹ (Maybe[#/2]&)

    1) `TraceMonadUnit`, monad's unit operator takes another monad object. E.g.

         TraceMonad[Maybe[data]] ⟹ MaybeEchoValue ⟹ (Maybe[#/2]&)

    2) The content of `TraceMonad[__]` is stored in the context for the key "data".

    3) If a call to `TraceMonadBind` is with a pipeline "function" that is a string,
       then append that string to the "comments" value in the context.

    4) If the pipeline function -- converted to a string -- matches `"TraceMonad"~~__`
       then handle as a `TraceMonad` pipeline function.

    5) Otherwise
    5.1. Call the wrapped monad binding operation with the function.
    5.2. Check is the result a failure.
    5.2.1.  If "yes", dump the current values of the context.
    5.2.2.  If "no", append the function to a "commands" value in the context.

    6) A grid of pipeline functions and comments can be displayed with `TraceMonadEchoGrid`.

    7) If the result of the pipeline is assigned to a variable, say, `res`
       then the pipeline can be can be re-executed with:

          Fold[MaybeBind, ReleaseHold[res[[2]]["data"]], res[[2]]["commands"]]


    ## References

    [1] Anton Antonov, State monad code generator Mathematica package, MathematicaForPrediction at GitHub, (2017),
        URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m

    [2] Mathematica Stack Exchange discussion, "How to improve the creation of tables of code and comments", 2017,
        URL: https://mathematica.stackexchange.com/q/148160/34008


    ## End matters

    This file was created by Mathematica Plugin for IntelliJ IDEA.

    Anton Antonov
    Windermere, FL, USA
    2017-06-13

*)

If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];

(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

BeginPackage["MonadicTracing`"]

$TraceMonadFailure::usage = "Failure symbol for TraceMonad."

TraceMonadUnit::usage = "Lifting a monad object into TraceMonad."

TraceMonadBind::usage = "The binding function of TraceMonad."

TraceMonadEchoGrid::usage = "Echoes a tabulation of the traced monad functions using Grid."

TraceMonadTakeGrid::usage = "Gives a tabulation of the traced monad functions using Grid."

Grid87::usage = "A modified version of Grid."

Begin["`Private`"]

ClearAll[$TraceMonadFailure]
(*$TraceMonadFailure = None;*)

(**************************************************************)
(* Generation                                                 *)
(**************************************************************)
Needs["StateMonadCodeGenerator`"]
GenerateStateMonadCode["MonadicTracing`TraceMonad", "StringContextNames" -> False, "FailureSymbol" -> $TraceMonadFailure ];

(**************************************************************)
(* Infix operators                                            *)
(**************************************************************)

(* Not needed here -- should have been already done by GenerateStateMonadCode. *)
(*DoubleLongRightArrow[x_?TraceMonadUnitQ, f_] := TraceMonadBind[x, f];*)


(**************************************************************)
(* Monad specific functions                                   *)
(**************************************************************)


ClearAll[TraceMonadUnit]

SetAttributes[TraceMonadUnit, HoldAll];

TraceMonadUnit[x_] := TraceMonadUnit[x, DoubleLongRightArrow ];

TraceMonadUnit[x_, binder_Symbol] :=
    TraceMonad[x, <|"data" -> HoldForm[x], "binder" -> binder, "commands" -> {}, "comments" -> {""}, "contextKeys" -> {{}} |>];

ClearAll[TraceMonadBind]

TraceMonadBind[___] := $TraceMonadFailure;

TraceMonadBind[TraceMonad[x_, context_], f_] :=
    Block[{res = f[x, context]}, If[FreeQ[res, $TraceMonadFailure], res, $TraceMonadFailure]] /;
        TrueQ[StringMatchQ[ToString[f], "TraceMonad" ~~ __]];

TraceMonadBind[TraceMonad[x_, context_], com_String] :=
    Block[{res},
      TraceMonad[x, Join[context, <|"comments" -> ReplacePart[context["comments"], -1 -> com]|>]]
    ];

TraceMonadBind[TraceMonad[x_, context_], f_] :=
    Block[{res},
      res = context["binder"][x, f];
      Which[

        (* Applying a heuristic for the failure symbol of the wrapped monad. *)
        res === None || Developer`SymbolQ[res] && StringMatchQ[SymbolName[res], "$"~~___~~"Failure"~~___],
        TraceMonadEchoGrid[][x, context];
        res, (* This should not be $TraceMonadFailure .*)

        Length[res] >= 2 && AssociationQ[ res[[2]] ],
        (* Assuming State monad. *)
        TraceMonad[res,
          Join[context,
            <|"commands" -> Append[context["commands"], f],
              "comments" -> Append[context["comments"], ""],
              "contextKeys" -> Append[context["contextKeys"], Keys[res[[2]]] ] |>]
        ],

        True,
        TraceMonad[res,
          Join[context,
            <|"commands" -> Append[context["commands"], f],
              "comments" -> Append[context["comments"], ""],
              "contextKeys" -> Append[context["contextKeys"], {}]|>]
        ]

      ]
    ];


ClearAll[Grid87]

Grid87 = Framed@
    Grid[#, Alignment -> Left, Dividers -> All, FrameStyle -> Directive[Dashing[2], GrayLevel[0.87]]] &;


ClearAll[TraceMonadEchoGrid]

Options[TraceMonadEchoGrid] = { "ComplexStyling" -> True, "ContextKeys" -> False };

TraceMonadEchoGrid[x_, context_Association] := TraceMonadEchoGrid[][x, context];

TraceMonadEchoGrid[][x_, context_] := TraceMonadEchoGrid[Grid87][x, context];

TraceMonadEchoGrid[opts:OptionsPattern[] ][x_, context_] := TraceMonadEchoGrid[Grid87, opts ][x, context];

TraceMonadEchoGrid[gridFunc_, opts:OptionsPattern[] ][x_, context_] :=
    Block[{res},

      res = TraceMonadBind[ TraceMonad[x, context], TraceMonadTakeGrid[gridFunc, opts] ];

      (* Show result *)
      Echo[res];
      TraceMonad[x, context]
    ];


ClearAll[TraceMonadTakeGrid];

Options[TraceMonadTakeGrid] = Options[TraceMonadEchoGrid];

TraceMonadTakeGrid[x_, context_Association] := TraceMonadTakeGrid[][x, context];

TraceMonadTakeGrid[][x_, context_] := TraceMonadTakeGrid[Grid87][x, context];

TraceMonadTakeGrid[opts:OptionsPattern[] ][x_, context_] := TraceMonadTakeGrid[Grid87, opts ][x, context];

TraceMonadTakeGrid[gridFunc_, opts:OptionsPattern[] ][x_, context_] :=
    Block[{grData, delim, cStyleQ, cKeysQ},

      cStyleQ = TrueQ[OptionValue[TraceMonadTakeGrid, "ComplexStyling"]];
      cKeysQ = TrueQ[OptionValue[TraceMonadTakeGrid, "ContextKeys"]];

      If[ !cKeysQ,
        grData =
            Transpose[{Prepend[HoldForm /@ context["commands"], context["data"]], context["comments"]}],
        (* ELSE *)
        grData =
            Transpose[{Prepend[HoldForm /@ context["commands"], context["data"]], context["comments"], context["contextKeys"]}];
      ];

      If[ context["binder"] === NonCommutativeMultiply, delim = "**", delim = "\[DoubleLongRightArrow]"];
      delim = "\[ThinSpace]" <> delim;

      (* Style the code and comments *)
      If[ cStyleQ,
      (* Using RuleCondition because the pipeline functions are kept in HoldForm. *)
      (* Note that RuleCondition is undocumented. *)
      (* The alternative is to use /. (z_String :> With[{eval = "\"" <> z <> "\""}, eval /; True]) *)
        grData[[All, 1]] =
            Map[
              Row[{"  ",
                Style[ # /. (z_String :> RuleCondition[("\"" <> z <> "\"")]), "Input"],
                Style[delim, "Input"]}] &,
              grData[[All, 1]]],
      (* ELSE *)
        grData[[All, 1]] = Map[Row[{"  ", Style[#, "Input"], Style[delim, "Input"]}] &, grData[[All, 1]]];
      ];
      grData[[1, 1]] = Row[Rest@grData[[1, 1, 1]]];
      grData[[-1, 1]] = Row[Most@grData[[-1, 1, 1]]];
      grData[[All, 2]] =
          Map[Style[#, "CommentStyle" /. Options[$FrontEnd, AutoStyleOptions][[1, 2]]] &, grData[[All, 2]]];

      If[Dimensions[grData][[2]] == 3, (*i.e. cKeysQ *)
        grData[[All, 3]] =
            Map[Style[#, "CommentStyle" /. Options[$FrontEnd, AutoStyleOptions][[1, 2]]] &, grData[[All, 3]]];
      ];

      (* Show result *)
      gridFunc[grData]
    ];


End[] (* `Private` *)

EndPackage[]