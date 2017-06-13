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

    The primary reason for developing it was the ability to print a tabulated trace of code and comments
    using the usual monad pipeline notation. (I.e. without conversion to strings etc.)

    From that perspective the main function is `TraceMonadEchoGrid`.

    ## Example

    In this example we can see that pipeline functions of the Maybe monad are interleaved with comment strings.
    Producing the grid of functions and comments happens "naturally" with the monad function `TraceMonadEchoGrid`.

        data = RandomInteger[10, 15];

        TraceMonadUnit[MaybeUnit[data]] ** "lift to monad" **
          TraceMonadEchoContext **
          MaybeFilter[# > 3 &] ** "filter current value" **
          MaybeEcho ** "display current value" **
          MaybeWhen[#[[3]] > 3 &,
          MaybeEchoFunction[Style[#, Red] &]] **
          (Maybe[#/4] &) **
          MaybeEcho ** "display current value again" **
          TraceMonadEchoGrid[Grid[#, Alignment -> Left] &];

    Note that :
      1) the tracing is initiated by just using TraceMonadUnit;
      2) putting a comment string after a pipeline function is optional.


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


    [ The simplification of the problem for monad pipelines only. ]


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

(*BeginPackage["MonadicTracing`"]*)
(** Exported symbols added here with SymbolName::usage *)

(*Begin["`Private`"]*)

If[Length[DownValues[StateMonadCodeGenerator`GenerateStateMonadCode]] == 0,
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/StateMonadCodeGenerator.m"]
];

(**************************************************************)
(* Generation                                                 *)
(**************************************************************)

GenerateStateMonadCode["TraceMonad", "StringContextNames" -> False]

(**************************************************************)
(* Infix operators                                            *)
(**************************************************************)

DoubleLongRightArrow[x_?TraceMonadUnitQ, f_] := TraceMonadBind[x, f];
DoubleLongRightArrow[x_, y_, z__] := DoubleLongRightArrow[DoubleLongRightArrow[x, y], z];

(* Not needed here -- should have been already done by GenerateStateMonadCode. *)
(*Unprotect[NonCommutativeMultiply]*)
(*NonCommutativeMultiply[x_?TraceMonadUnitQ, f_] := TraceMonadBind[x, f];*)
(*NonCommutativeMultiply[x_, y_, z__] := NonCommutativeMultiply[NonCommutativeMultiply[x, y], z];*)


(**************************************************************)
(* Monad specific functions                                   *)
(**************************************************************)

ClearAll[TraceMonadUnit]

SetAttributes[TraceMonadUnit, HoldAll];

TraceMonadUnit[x_, binder_Symbol] :=
    TraceMonad[x, <|"data" -> HoldForm[x], "binder" -> binder, "commands" -> {}, "comments" -> {""}|>];

TraceMonadUnit[x_] :=
    TraceMonad[x, <|"data" -> HoldForm[x], "binder" -> NonCommutativeMultiply, "commands" -> {}, "comments" -> {""}|>];


ClearAll[TraceMonadBind]

TraceMonadBind[___] := None;

TraceMonadBind[TraceMonad[x_, context_], f_] :=
    Block[{res = f[x, context]}, If[FreeQ[res, None], res, None]] /;
        TrueQ[StringMatchQ[ToString[f], "TraceMonad" ~~ __]];

TraceMonadBind[TraceMonad[x_, context_], com_String] :=
    Block[{res},
      TraceMonad[x, Join[context, <|"comments" -> ReplacePart[context["comments"], -1 -> com]|>]]
    ];

TraceMonadBind[TraceMonad[x_, context_], f_] :=
    Block[{res},
      res = context["binder"][x, f];
      If[res === None,
        TraceMonadEchoGrid[Grid[#, Alignment -> Left] &][x, context];
        None,
      (*ELSE*)
        TraceMonad[res,
          Join[context,
            <|"commands" -> Append[context["commands"], f],
              "comments" -> Append[context["comments"], ""]|>]]
      ]
    ];

TraceMonadEchoGrid[gridFunc_][x_, context_] :=
    Block[{grData, delim},
      grData =
          Transpose[{Prepend[HoldForm /@ context["commands"], context["data"]], context["comments"]}];

      If[ context["binder"] === NonCommutativeMultiply, delim = "**", delim = "\[DoubleLongRightArrow]"];
      delim = "\[ThinSpace]" <> delim;

      (* Style the code and comments *)
      grData[[All, 1]] = Map[Row[{"  ", Style[#, "Input"], Style[delim, "Input"]}] &, grData[[All, 1]]];
      grData[[1, 1]] = Row[Rest@grData[[1, 1, 1]]];
      grData[[-1, 1]] = Row[Most@grData[[-1, 1, 1]]];
      grData[[All, 2]] =
          Map[Style[#, "CommentStyle" /. Options[$FrontEnd, AutoStyleOptions][[1, 2]]] &, grData[[All, 2]]];

      (* Show result *)
      Echo[gridFunc[grData]];
      TraceMonad[x, context]
    ];


(*End[] * `Private` *)

(*EndPackage[]*)