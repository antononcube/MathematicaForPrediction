(*
    Functional parsers Mathematica package
    Copyright (C) 2014  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2014 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 1.0 *)
(* 
   This package provides an implementation of a system of functional parsers. 
   The implementation follows closely the article:

   "Functional parsers" by Jeroen Fokker

   The parsers are categorized in the groups: basic, combinators, and transformers.
   The basic parsers parse specified strings and strings adhering to predicates.
   The combinator parsers allow sequential and alternative combinations of parsers.
   The transformer parsers change the input or the output of the parsers that are transformed.

   A basic or a combinator parser takes a list of strings and returns a list of pairs, {{o1,r1},{o2,r2},...}. Each pair has as elements a parsed output and the rest of the input list.

   Functions for splitting the input text into tokens are provided.

   The package also have functions to generate parsers from a string of the Extended Backus-Naur Form (EBNF) definition of a grammar. 
   In the current version of the EBNF parser functions there is a requirement that all elements of the parsed EBNF forms have to be separated by space. The EBNF grammar string can have the pick-left and pick-right combinators (\[LeftTriangle] and \[RightTriangle] respectively) and a ParseApply specification can be given within the form "<rhs> = parsers <@ transformer" . The application of functions can be done over the whole definition of an EBNF non-terminal symbol, not over the individual parts.

   There is a function, InterpretWithContext, for interpreting parsed results with a context. The context is given as a list of rules. There are two forms for the context rules: {(_Symbol->_)..} and {"data"->{(_Symbol->_)...}, "functions"->{(_Symbol->_)...}} . If during the interpretation the context functions change the context data the result of InterpretWithContext will return the changed data.

*)

BeginPackage["FunctionalParsers`"]

ParseSymbol::usage = "ParseSymbol[s] parses a specified symbol s."
ParseToken::usage = "ParseToken[t] parses the token t."
ParsePredicate::usage = "ParsePredicate[p] parses strings that give True for the predicate p."
ParseEpsilon::usage = "ParseEpsilon parses and empty string."
ParseSucceed::usage = "ParseSucceed[v] does not consume input and always returns v."
ParseFail::usage = "ParseFail fails to recognize any input string."

ParseSequentialComposition::usage = "ParseSequentialComposition parses a sequential composition of two or more parsers."
ParseAlternativeComposition::usage = "ParseAlternativeComposition parses a composition of two or more alternative parsers."


ParseSpaces::usage = "ParseSpaces[p] is a transformation of the parser p: the leading spaces of the input are dropped then the parser p is applied." 
ParseJust::usage = "ParseJust[p] is a transformation of the parser p: those parts of output of p are selected that have empty rest strings."
ParseApply::usage = "ParseApply[f,p] applies the function f to the output of p. ParseApply[{fNo, fYes}, p] applies the function fNo not unsuccessful parsing and the function fYes the output of successful parsing using p."
ParseSome::usage = "ParseSome[p] applies ParseJust[p] and takes the first non-empty output if it exists."
ParseShortest::usage = "ParseShortest[p] takes the output with the shortests rest string." 
ParseSequentialCompositionPickLeft::usage = "ParseSequentialCompositionPickLeft[p1,p2] drops the output of the p2 parser."
ParseSequentialCompositionPickRight::usage = "ParseSequentialCompositionPickRight[p1,p2] drops the output of the p1 parser."

ParseChoice::usage = "ParseChoice[p1,p2,p3,...] a version of ParseAlternativeComposition for more than two parsers."

CircleDot::usage = "CircleDot[f_, p_] applies the function f to the output of p. It can be entered with \"Exc c . Esc\" ."
CircleTimes::usage = "CircleTimes[p1,p2,p3] sequential composition of the parsers p1, p2, p3, ... It can be entered with \"Exc c * Esc\" ."
CirclePlus::usage = "CirclePlus[p1,p2,p3] alternatives composition of the parsers p1, p2, p3, ... It can be entered with \"Exc c + Esc\" ."
LeftTriangle::usage = "LeftTriangle[p1_, p2_] drops the output of the right parser, p2. It can be entered with \"\:22B2\"."
RightTriangle::usage = "RightTriangle[p1_, p2_] drops the output of the left parser, p1. It can be entered with \"\:22B3\"."

ParsePack::usage = "ParsePack[s1,p,s2] parses the sequential composition of s1, p, s2 and drops the output of s1 and s2."
ParseParenthesized::usage = "ParseParenthesized[p] parses with p input enclosed in parentheses \"(\",\")\"."
ParseBracketed::usage = "ParseBracketed[p] parses with p input enclosed in brackets \"[\",\"]\"."
ParseCurlyBracketed::usage = "ParseCurlyBracketed[p] parses with p input enclosed in curly brackets \"{\",\"}\"."

ParseOption::usage = "ParseOption[p] is optional parsing of p.";
ParseOption1::usage = "ParseOption1[p] is optional parsing of p. (Different implementation of ParseOption.)";

ParseMany1::usage = "ParseMany1[p] attempt to parse one or many times with p."
ParseMany::usage = "ParseMany[p] attempt to parse zero or many times with p."

ParseListOf::usage = "ParseListOf[p_, sep_] parse a list of elements parsed by p and seprated by elements parsed by sep."

ParseChainLeft::usage = "ParseChainLeft[p_, sep_] parse a nested application of the function with a name parsed by sep."
ParseChain1Left::usage = "ParseChain1Left[p_, sep_] parse a nested application of the function with a name parsed by sep."
ParseChainRight::usage = "ParseChainRight[p_, sep_] parse a nested application of the function with a name parsed by sep."

ParseRecursiveDefinition::usage = "ParseRecursiveDefinition[pname, rhs] makes a parser with name pname defined by rhs that can be used in recursive definitions."

ToTokens::usage = "ToTokens[text] breaks down text into tokens. ToTokens[text,terminals] breaks down text using specified terminals." 

ParseToTokens::usage = "ParseToTokens[text, terminalDelimiters, whitespaces] breaks down text into tokens using specified terminal symbols and white spaces."

ParsingTestTable::usage = "ParsingTestTable[p, s, opts] parses a list of strings with the parser p and tabulates the result. The options allow to specify terminal symbols and the table layout."

EBNFNonTerminal::usage = "EBNFNonTerminal head for parsers for non-terminal symbols of EBNF grammars."
EBNFTerminal::usage = "EBNFTerminal head for parsers for terminal symbols of EBNF grammars."
EBNFOption::usage = "EBNFOption head for parsers for optional symbols of EBNF grammars."
EBNFRepetition::usage = "EBNFRepetition head for parsers for repeating symbols of EBNF grammars."
EBNFSequence::usage = "EBNFSequence head for parsers for sequential composition of symbols of EBNF grammars."
EBNFAlternatives::usage = "EBNFAlternatives head for parsers for alternatives sequential composition of symbols of EBNF grammars."
EBNFRule::usage = "EBNFRule head for parsers of EBNF grammar rules."
EBNF::usage = "Head symbol used to for parsed EBNF grammars."

EBNFNonTerminalInterpreter::usage = "EBNFNonTerminal generates parsers for non-terminal symbols of EBNF grammars."
EBNFTerminalInterpreter::usage = "EBNFTerminal generates parsers for terminal symbols of EBNF grammars."
EBNFOptionInterpreter::usage = "EBNFOption generates parsers for optional symbols of EBNF grammars."
EBNFRepetitionInterpreter::usage = "EBNFRepetition generates parsers for repeating symbols of EBNF grammars."
EBNFSequenceInterpreter::usage = "EBNFSequence generates parsers for sequential composition of symbols of EBNF grammars."
EBNFAlternativesInterpreter::usage = "EBNFAlternatives generates parsers for alternatives sequential composition of symbols of EBNF grammars."
EBNFRuleInterpreter::usage = "EBNFRule generates parsers of EBNF grammar rules."

SetParserModifier::usage = "SetParserModifier[p_Symbol, f_] sets the function f to modify the output of the parser p. (Replaces the previous modifier.)"
AddParserModifier::usage = "AddParserModifier[p_Symbol, f_] makes the function f to modify the output of the parser p."

InterpretWithContext::usage = "InterpretWithContext[pout_,cr_] interprets the parser output pout with the context rules cr."

EBNFContextRules::usage = "Context rules for EBNF parser generation."

ParseEBNF::usage = "ParseEBNF[gr:{_String..}] parses the EBNF grammar gr."
GenerateParsersFromEBNF::usage = "GenerateParsersFromEBNF[gr:{_String..}] generate parsers the EBNF grammar gr."

Begin["`Private`"]

Clear["Parse?*"]


(************************************************************)
(* Basic parsers                                            *)
(************************************************************)

ParseSymbol[a_] := 
  Function[If[Length[#] > 0 && a === First[#], {{Rest[#], a}}, {}]];

ParseToken[k_][xs_] := 
 With[{n = Length[k]}, 
  If[Length[xs] >= n && k == Take[xs, n], {{Drop[xs, n], k}}, {}]];

ParsePredicate[pred_][xs_] :=
    If[TrueQ[Length[xs] > 0 && pred[First[xs]]], {{Rest[xs], First[xs]}}, {}];

(* 
  Note that
  ParseSymbol[a] = ParsePredicate[# == a &] 
*)

ParseEpsilon = Function[{xs}, {{xs, {}}} ];

ParseSucceed[v_] := Function[{xs}, {{xs, v}}];

ParseFail[xs_] := {}


(************************************************************)
(* Parse combinators                                        *)
(************************************************************)

ParseComposeWithResults[p_][{}] := {};
ParseComposeWithResults[p_][res : {{_, _} ..}] :=
  Block[{t},
   Flatten[#, 1] &@
    Map[
     Function[{r},
      If[r === {}, {},
       t = p[r[[1]]];
       If[t === {}, {},
        Map[{#[[1]], {r[[2]], #[[2]]}} &, t]
        ]]],
     res]
   ];
ParseSequentialComposition[p1_][xs_] := p1[xs];

ParseSequentialComposition[args__][xs_] :=
  With[{parsers = {args}},
    Fold[ParseComposeWithResults[#2][#1] &, First[parsers][xs], 
     Rest[parsers]]
    ] /; Length[{args}] > 1;

ParseAlternativeComposition[args__][xs_] := Join @@ Map[#[xs] &, {args}];



(************************************************************)
(* Next combinators                                         *)
(************************************************************)
(* ParseSpaces[p_][xs_]:=p[NestWhile[Rest,xs,First[#]==""||First[#]==" "&]]; *)

ParseSpaces[pArg_] := 
 With[{p = pArg}, 
  Function[p[
    NestWhile[Rest, #, 
     Length[#] > 
        0 && (First[#] == "" || First[#] == " " || 
         First[#] == "\n") &]]]]

ParseJust[p_][xs_] := With[{res = p[xs]}, Select[res, First[#] === {} &]];

ParseApply[f_, p_][xs_] := Map[{#[[1]], f[#[[2]]]} &, p[xs]];

ParseApply[{fNo_, fYes_}, p_] := 
  With[{res = p[#]}, 
    Map[{#[[1]], If[#[[2]] === {}, fNo, fYes[#[[2]]]]} &, res]] &;

ParseSome[p_][xs_] := 
  With[{parsed = ParseJust[p][xs]}, 
   If[Length[parsed] > 0, Take[parsed, 1], parsed]];

ParseShortest[p_] := With[{parsed = p[#]}, If[parsed === {}, parsed, {First@SortBy[parsed, Length[#[[1]]] &]}]] &;

ParseSequentialCompositionPickLeft[p1_, p2_][xs_] := ParseApply[#[[1]] &, ParseSequentialComposition[p1, p2]][xs];

ParseSequentialCompositionPickRight[p1_, p2_][xs_] := ParseApply[#[[2]] &, ParseSequentialComposition[p1, p2]][xs];

ParseChoice[args__][xs_] := 
  With[{parsers = {args}}, Fold[Join[#2[xs], #1] &, ParseFail[xs], Reverse@parsers]];


(************************************************************)
(* Binding for infix notation                               *)
(************************************************************)

CircleDot[f_, p_] := ParseApply[f, p];(* Exc c . Esc *)

CircleTimes[args___] := ParseSequentialComposition[args];(* Exc c * Esc *)

CirclePlus[args___] := ParseAlternativeComposition[args];(* Exc c + Esc *)

LeftTriangle[p1_, p2_] := ParseSequentialCompositionPickLeft[p1, p2]; (* \:22B2 *)

RightTriangle[p1_, p2_] := ParseSequentialCompositionPickRight[p1, p2]; (* \:22B3 *)

(* Note that the precedence pre-assigned of the operators \[CircleDot], \[CircleTimes] and \[CirclePlus] gives the expected grouping:

Block[{a, b, c, d, f, h},
 Print[f\[CircleDot]a\[CircleTimes]b\[CirclePlus]c\[CircleTimes]h\[CircleDot]d]
 ]

ParseAlternativeComposition[ParseSequentialComposition[ParseApply[f,a],b],ParseSequentialComposition[c,ParseApply[h,d]]]
*)

(*
Note that the precedence pre-assigned of the operators \[LeftTriangle] and \[RightTriangle] gives the expected grouping:

Block[{x, y, z},
 Print[x \[RightTriangle] y \[LeftTriangle] z]
 ]

ParseSequentialCompositionPickLeft[ParseSequentialCompositionPickRight[x,y],z]
*)


(************************************************************)
(* Second next combinators                                  *)
(************************************************************)

ParsePack[s1_, p_, s2_] := ParseSequentialCompositionPickLeft[ ParseSequentialCompositionPickRight[s1, p], s2];

ParseParenthesized[p_] := ParsePack[ParseSymbol["("], p, ParseSymbol[")"]];
ParseBracketed[p_] := ParsePack[ParseSymbol["["], p, ParseSymbol["]"]];
ParseCurlyBracketed[p_] := ParsePack[ParseSymbol["{"], p, ParseSymbol["}"]]

ParseOption[p_] := (ParseAlternativeComposition[ParseApply[{#1} &, p], ParseApply[{} &, ParseSucceed[{}]]]);

ParseOption1[p_] := Block[{res = p[#]}, If[TrueQ[res === {}], {{#, {}}}, res]] &;

ParseMany1[p_][xs_] :=
  Module[{t = {}, res},
   res = ParseShortest[ParseOption1[p]][xs];
   While[! (res === {} || res[[1, 2]] === {}),
    AppendTo[t, res[[1, 2]]];
    res = ParseShortest[ParseOption1[p]][res[[1, 1]]];
   ];
   {{res[[1, 1]], t}}
  ];

ParseMany[p_] := ParseMany1[p]\[CirclePlus]ParseSucceed[{}];

ParseListOf[p_, 
   separatorParser_] := (Prepend[#[[2]], #[[1]]] &)\[CircleDot](p\[CircleTimes]ParseMany[separatorParser \[RightTriangle] p])\[CirclePlus]ParseSucceed[{}];

ParseChainLeft[p_, separatorParser_] := 
  Fold[#2[[1]][#1, #2[[2]]] &, #[[1]], #[[2]]] &\[CircleDot](p\[CircleTimes]ParseMany[separatorParser\[CircleTimes]p])\[CirclePlus]ParseSucceed[{}];

ParseChain1Left[p_, separatorParser_] := 
  Fold[#2[[1]][#1, #2[[2]]] &, #[[1]], #[[2]]] &\[CircleDot](p\[CircleTimes]ParseMany1[separatorParser\[CircleTimes]p]);

ParseChainLeft[p_, {separatorParser_, func_}] := 
(Fold[func[#1, #2[[2]]] &, #[[1]], #[[2]]] &)\[CircleDot](p\[CircleTimes]ParseMany[separatorParser\[CircleTimes]p])\[CirclePlus]ParseSucceed[{}];

ParseChainRight[p_, separatorParser_] := 
  Fold[#2[[2]][#2[[1]], #1] &, #[[2]], 
      Reverse[#[[1]]]] &\[CircleDot](ParseMany[p\[CircleTimes]separatorParser]\[CircleTimes]p)\[CirclePlus]ParseSucceed[{}];

ParseChainRight[p_, {separatorParser_, func_}] := 
  Fold[func[#2[[1]], #1] &, #[[2]], 
      Reverse[#[[1]]]] &\[CircleDot](ParseMany[p\[CircleTimes]separatorParser]\[CircleTimes]p)\[CirclePlus]ParseSucceed[{}];


(************************************************************)
(* ParseRecursiveDefinition                                 *)
(************************************************************)

SetAttributes[ParseRecursiveDefinition, HoldAll]
ParseRecursiveDefinition[parserName_Symbol, rhs__] :=
  Block[{},
   parserName[xs_] := rhs[xs]
  ];


(************************************************************)
(* Tokenizer                                                *)
(************************************************************)

ToTokens[text_String] := StringSplit[text];
ToTokens[text_String, {}] := StringSplit[text];
ToTokens[text_String, terminals : {_String ...}] :=
  StringSplit[StringReplace[text, Map[# -> " " <> # <> " " &, terminals]]];

ToTokens[text_, "EBNF"] := 
  ToTokens[text, {"|", ",", ";", "=", "[", "]", "(", ")", "{", "}"}];

Clear[ParseToTokens];
ParseToTokens[text_String, terminalDelimiters_: {}, whitespaces_: {" ", "\n"}] :=
  Block[{pWord, pQWord, pTermDelim, res},
   pWord = 
    ParseSpaces[
     ParseMany1[
      ParsePredicate[! 
         MemberQ[Join[terminalDelimiters, whitespaces], #] &]]];
   pQWord = 
    ParseSpaces[(ParseSymbol["'"]\[CirclePlus]ParseSymbol["\""])\[CircleTimes]ParseMany1[
       ParsePredicate[# != "'" && # != "\"" &]]\[CircleTimes](ParseSymbol["'"]\[CirclePlus]ParseSymbol["\""])];
   pTermDelim = 
    ParseSpaces[ParsePredicate[MemberQ[terminalDelimiters, #] &]];
   res = ParseMany1[((If[Length[#] > 0, StringJoin @@ #, #] &)\[CircleDot](pQWord\[CirclePlus]pWord))\[CirclePlus]pTermDelim][Characters[text]];
   res[[1, 2]]
  ];


(************************************************************)
(* ParsingTestTable                                         *)
(************************************************************)

Clear[ParsingTestTable]

ParsingTestTable::unval = "Unknown value `2` for the option `1`."

Options[ParsingTestTable] = {FontFamily -> "Times", FontSize -> 16, "Terminals" -> {}, "Layout" -> "Horizontal"};
ParsingTestTable[parser_, statements : {_String ..}, optsArg : OptionsPattern[]] :=
  Block[{res, ff = OptionValue[ParsingTestTable, FontFamily], 
    fs = OptionValue[ParsingTestTable, FontSize], 
    ts = OptionValue[ParsingTestTable, "Terminals"], 
    layout = OptionValue[ParsingTestTable, "Layout"], opts, ptbl, vptbl},
   	opts = {FontFamily -> ff, FontSize -> fs};
   	res = Map[parser[ToTokens[#, ts]] &, statements];
   	ptbl = 
    TableForm[Transpose[{Map[Style[#, opts] &, statements], res}], 
     TableDepth -> 2, 
     TableHeadings -> {Style[#, Darker[Red], opts] & /@ 
        Range[Length[statements]], 
       Style[#, Darker[Red], opts] & /@ {"Statement", "Parser output"}}];
   Which[
    TrueQ[layout == "Horizontal"], ptbl,
    TrueQ[layout == "Vertical"],
    vptbl = Flatten[
      Transpose[{statements, ptbl[[1, All, 2, 1, 2]], 
        ptbl[[1, All, 2, 1, 1]]}], 1];
    vptbl = 
     Transpose[{Flatten[
        Table[{Style[i, Darker[Red], opts], "", ""}, {i, 1, 
          Length[statements]}]], 
       Style[#, Gray] & /@ 
        Flatten[Table[{"command:", "parsed:", 
           "residual:"}, {Length[vptbl]/3}]], vptbl}];
    Grid[vptbl, Alignment -> {{Right, Right, Left}}, Spacings -> {0.5, 0.75}, 
     Dividers -> {{True, True, False, True}, 
       Join[{True}, Flatten@Table[{False, False, True}, {Length[vptbl]}]]}],
    True,
    Message[ParsingTestTable::unval,"Layout",layout]; ptbl
   ]
  ];


(***************************************************************************)
(* EBNF Parsers with parenthesis and \[RightTriangle] and \[LeftTriangle]  *)
(***************************************************************************)

(* All parsers start with the prefix "pG" followed by a capital letter. ("p" is for "parser", "G" is for "grammar".) *)


Clear[EBNFNonTerminal, EBNFTerminal, EBNFOption, EBNFRepetition, EBNFSequence, EBNFAlternatives, EBNFRule, EBNF]

Clear["pG*"]

(* Parse typeTerminal. All teminals are assumed to be between single or double quotes. *)

EBNFSymbolTest = 
  TrueQ[# == "|" || # == "," || # == "=" || # == ";" || # == "\[LeftTriangle]" || # == "\[RightTriangle]"] &;

NonTerminalTest = 
  TrueQ[StringMatchQ[#, "<" ~~ (WordCharacter | WhitespaceCharacter | "-" | "_") .. ~~ ">"]] &;

InQuotesTest = TrueQ[StringMatchQ[#, ("'" | "\"") ~~ __ ~~ ("'" | "\"")]] &;

pGTerminal = 
  ParsePredicate[StringQ[#] && InQuotesTest[#] && ! EBNFSymbolTest[#] &];

(* Parser typeNonTerminal. I prefer the <xxx> format for non-terminals instead of allowing any string without quotes. *)

pGNonTerminal = 
  ParsePredicate[StringQ[#] && NonTerminalTest[#] && ! EBNFSymbolTest[#] &];

pGOption = EBNFOption\[CircleDot]ParseBracketed[pGExpr];

pGRepetition = EBNFRepetition\[CircleDot]ParseCurlyBracketed[pGExpr];

pGNode[xs_] := (EBNFTerminal\[CircleDot]pGTerminal\[CirclePlus]EBNFNonTerminal\[CircleDot]pGNonTerminal\[CirclePlus]ParseParenthesized[pGExpr]\[CirclePlus]pGRepetition\[CirclePlus]pGOption)[xs];

pGTerm = EBNFSequence\[CircleDot]ParseChainRight[pGNode, ParseSymbol[","]\[CirclePlus]ParseSymbol["\[LeftTriangle]"]\[CirclePlus]ParseSymbol["\[RightTriangle]"]];

pGExpr = EBNFAlternatives\[CircleDot]ParseListOf[pGTerm, ParseSymbol["|"]];

pGRule = EBNFRule\[CircleDot](pGNonTerminal\[CircleTimes](ParseSymbol["="] \[RightTriangle] pGExpr)\[CircleTimes](ParseSymbol[";"]\[CirclePlus](ParseSymbol["<@"]\[CircleTimes]ParsePredicate[StringQ[#] &] \[LeftTriangle] ParseSymbol[";"])));

pEBNF = EBNF\[CircleDot]ParseMany1[pGRule];


(********************************************************************************)
(* EBNF grammar parser generators with \[RightTriangle] and \[LeftTriangle]     *)
(********************************************************************************)

Clear[EBNFMakeSymbolName, EBNFNonTerminal, EBNFTerminalInterpreter, EBNFOptionInterpreter, EBNFRepetitionInterpreter, EBNFSequenceInterpreter, EBNFAlternativesInterpreter, EBNFRuleInterpreter]

Clear[pNumber, pWord, pLetterWord, pIdentifierWord]
pNumber = ToExpression\[CircleDot]ParsePredicate[StringMatchQ[#, NumberString] &];

pInteger = ToExpression\[CircleDot]ParsePredicate[StringMatchQ[#, DigitCharacter ..] &];

pWord = ParsePredicate[StringMatchQ[#, (WordCharacter | "_") ..] &];

pLetterWord = ParsePredicate[StringMatchQ[#, LetterCharacter ..] &];

pIdentifierWord = ParsePredicate[StringMatchQ[#, LetterCharacter ~~ (WordCharacter ... )] &];

Clear[pNumberRange]
pNumberRange[{s_?NumberQ, e_?NumberQ}] :=
  ToExpression\[CircleDot]ParsePredicate[StringMatchQ[#, NumberString] && s <= ToExpression[#] <= e &];

EBNFMakeSymbolName[p_String] := 
  "p" <> ToUpperCase[StringReplace[p, {"<" -> "", ">" -> "", "_" -> "", "-" -> ""}]];

EBNFTerminalInterpreter[parsed_] :=
  Which[
   StringMatchQ[parsed, ("'" | "\"") ~~ "_?NumberQ" ~~ ("'" | "\"")], 
   pNumber,
   StringMatchQ[parsed, ("'" | "\"") ~~ "_?IntegerQ" ~~ ("'" | "\"")], 
   pInteger,
   StringMatchQ[
    parsed, ("'" | "\"") ~~ "Range[" ~~ NumberString ~~ "," ~~ NumberString ~~ "]" ~~ ("'" | "\"")], 
   pNumberRange[
    Flatten@StringCases[
      parsed, ("'" | "\"") ~~ "Range[" ~~ (s : NumberString) ~~ "," ~~ (e : NumberString) ~~ "]" ~~ ("'" | "\"") :> Map[ToExpression, {s, e}]]],
   parsed == "\"_String\"" || parsed == "'_String'", 
   ParsePredicate[StringQ[#] &],
   parsed == "\"_WordString\"" || parsed == "'_WordString'", pWord,
   parsed == "\"_LetterString\"" || parsed == "'_LetterString'", pLetterWord,
   parsed == "\"_IdentifierString\"" || parsed == "'_IdentifierString'", pIdentifierWord,
   True, ParseSymbol[
    If[StringMatchQ[parsed, ("'" | "\"") ~~ ___ ~~ ("'" | "\"")], 
     StringTake[parsed, {2, -2}], parsed]]
  ];

EBNFNonTerminalInterpreter[parsed_] := ToExpression[EBNFMakeSymbolName[parsed]];

EBNFRepetitionInterpreter[parsed_] := ParseMany1[parsed];

EBNFOptionInterpreter[parsed_] := ParseOption1[parsed];

EBNFSequenceInterpreter[parsedArg_] :=
  Block[{parsed = parsedArg, crules},
   (*Print["before:",parsed];*)
   
   crules = {ParseSymbol[","] -> "X$$#$#$#1", 
     ParseSymbol["\[LeftTriangle]"] -> "X$$#$#$#2", 
     ParseSymbol["\[LeftTriangle]"] -> "X$$#$#$#2"};
   parsed = parsed //. crules;
   (*Print["mid:",parsed];*)
   
   parsed = parsed /. {"," -> ParseSequentialComposition, 
      "\[LeftTriangle]" -> ParseSequentialCompositionPickLeft, 
      "\[RightTriangle]" -> ParseSequentialCompositionPickRight};
   parsed = parsed //. (Reverse /@ crules);
   (*Print["after:",parsed];*)
   Which[
    ! ListQ[parsed], parsed,
    Length[parsed] == 1, parsed[[1]],
    True, ParseSequentialComposition @@ parsed
   ]
  ];


EBNFAlternativesInterpreter[parsed_] :=
  Which[
   ! ListQ[parsed], parsed,
   Length[parsed] == 1, parsed[[1]],
   True, ParseAlternativeComposition @@ parsed
  ];

EBNFRuleInterpreter[parsed_] :=
  Block[{lhsSymbolName, lhsSymbol, res},
   lhsSymbolName = EBNFMakeSymbolName[parsed[[1, 1]]];
   With[{sn = lhsSymbolName}, Clear[sn]];
   lhsSymbol = ToExpression[lhsSymbolName];
   (*Print[lhsSymbol];*)
   If[ListQ[parsed[[2]]],
    With[{lhs = lhsSymbol, rhs = parsed[[1, 2]], func = parsed[[2, 2]]},
     lhs[xs_] := ParseApply[ToExpression[func], rhs][xs]];
    res = Row[{lhsSymbolName, " = ", parsed[[1, 2]], parsed[[2, 1]], 
       parsed[[2, 2]]}],
    (* assumed Length[parsed] == 2*)
    
    With[{lhs = lhsSymbol, rhs = parsed[[1, 2]]}, lhs[xs_] := rhs[xs]];
    res = Row[{lhsSymbolName, " = ", parsed[[1, 2]]}]
   ];
   res
  ];


(************************************************************)
(* Parser definition modification                           *)
(************************************************************)

(* one downvalue per parser is assumed *)
Clear[AddParserModifier]
AddParserModifier[parser_Symbol, func_] :=
  Block[{dvs = Cases[DownValues[parser], _RuleDelayed]},
   If[Length[dvs] == 0, {},
    With[{parserBody = dvs[[1, 2, 0]], parserVar = dvs[[1, 1, 1, 1, 1]]},
     DownValues[
       parser] = {dvs[[1, 1]] :> ParseApply[func, parserBody][parserVar]}
    ]
   ]
  ];

Clear[SetParserModifier]
SetParserModifier[parser_Symbol, func_] :=
  Block[{dvs = Cases[DownValues[parser], _RuleDelayed]},
   Which[
    Length[dvs] == 0, {},
    Length[dvs] > 0 && Head[dvs[[1, 2, 0]]] === ParseApply,
    DownValues[parser] = {ReplacePart[dvs, {1, 2, 0, 1} -> func]},
    True,
    AddParserModifier[parser, func]
   ]
  ];

(************************************************************)
(* Interpretation                                           *)
(************************************************************)

EBNFContextRules = 
 {EBNFNonTerminal -> EBNFNonTerminalInterpreter, 
 EBNFTerminal -> EBNFTerminalInterpreter, 
 EBNFOption -> EBNFOptionInterpreter, 
 EBNFRepetition -> EBNFRepetitionInterpreter, 
 EBNFSequence -> EBNFSequenceInterpreter, 
 EBNFAlternatives -> EBNFAlternativesInterpreter, 
 EBNFRule -> EBNFRuleInterpreter}

Clear[InterpretWithContext]
InterpretWithContext[parsed_, contextRules : {_Rule ...}] :=
  Block[{},
   {parsed /. contextRules, {} }
  ];

InterpretWithContext[parsed_, contextRules : {"data" -> {}, "functions" -> {(_Symbol -> _) ...}}] :=
  InterpretWithContext[parsed, "functions" /. contextRules];

InterpretWithContext[parsed_, contextRules : {"data" -> {(_Symbol -> _) ..}, "functions" -> {(_Symbol -> _) ...}}] :=
  Block[{dataVars, res, newData},
   dataVars = ("data" /. contextRules)[[All, 1]];
   {res, newData} =
    Block[Evaluate[dataVars],
     Do[
      Evaluate[r[[1]]] = r[[2]]
      , {r, ("data" /. contextRules)}];
     {parsed /. ("functions" /. contextRules), dataVars}
    ];
   {res, Thread[dataVars -> newData]}
  ];

ParseEBNF[code_] := pEBNF[code];

GenerateParsersFromEBNF[code_] := InterpretWithContext[ pEBNF[code], EBNFContextRules ];


End[]

EndPackage[]