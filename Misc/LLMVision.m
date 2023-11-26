(* LLMVision Mathematica Package *)
(*
MIT License

Copyright (c) 2023 Anton Antonov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: LLMVision *)
(* :Context: LLMVision` *)
(* :Author: Anton Antonov *)
(* :Date: 2023-11-26 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.1 *)
(* :Copyright: (c) 2023 Anton Antonov *)
(* :Keywords: LLM, OpenAI, ChatGPT, Vision, Images, Interpretation*)
(* :Discussion: *)


(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[OpenAIRequest]] == 0,
  Echo["OpenAIRequest.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/OpenAIRequest.m"];
];

(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["LLMVision`"];
(* Exported symbols added here with SymbolName::usage *)

LLMVisionSynthesize::usage = "Generate texts from prompts and images.";

LLMVisionFunction::usage = "Create a function of generation of texts from prompts and images.";

LLMVisionFunctionTemplate::usage = "Head of LLMVisionFunction objects.";

Begin["`Private`"];

Needs["OpenAIRequest`"];

toVision = TemplateExpression[<|
  "model" -> "gpt-4-vision-preview",
  "messages" -> {
    <|"role" -> "user",
      "content" -> {
        <|"type" -> "text",
          "text" -> TemplateSlot["text"]|>,
        Sequence @@ TemplateSlot["imageSpecs"]
      }
    |>}
|>];


(**********************************************************)
(* LLMVisionSynthesize                                    *)
(**********************************************************)

Clear[LLMVisionSynthesize];

Options[LLMVisionSynthesize] =
    Join[{"MaxTokens" -> Automatic, "Temperature" -> Automatic,
      "Format" -> Automatic}, Options[OpenAIRequest]];

SyntaxInformation[LLMVisionSynthesize] = {"ArgumentsPattern" -> {_, _, OptionsPattern[]}};

LLMVisionSynthesize::nimg =
    "The second argument is expected to be an image, image URL, image file \
path, or a list of those objects.";
LLMVisionSynthesize::nargs =
    "The first argument is expected to be a string or list of strings. The \
second argument is expected to be an image, image URL, image file path, or a \
list of those objects.";

LLMVisionSynthesize[prompt_String, imageSpecs_, opts : OptionsPattern[]] :=
    LLMVisionSynthesize[{prompt}, imageSpecs, opts];

LLMVisionSynthesize[prompt_, imageSpec : (_Image | _String | _File | _URL),
  opts : OptionsPattern[]] := LLMVisionSynthesize[prompt, {imageSpec}, opts];

LLMVisionSynthesize[prompts : {_String ..},
  imageSpecs : {(_Image | _String | _File | _URL) ..},
  opts : OptionsPattern[]] :=
    Block[{maxTokens, temp, format, images, t, imageRecs, body, res},

      (*Import File objects and Strings that are paths and pick the images *)
      images = Map[
        If[MatchQ[#, _File] || StringQ[#] && FileExistsQ[#],
          t = Import[#];
          If[! ImageQ[t], Message[LLMVisionSynthesize::nimg], t]
          ,
          (*ELSE*)
          #
        ] &, imageSpecs];

      (*Convert the images into Base64 strings*)
      images =
          Map[If[ImageQ[#],
            "data:image/jpeg;base64," <> ExportString[#, {"Base64", "JPEG"}], #] &,
            images];

      (*Prepare image strings and URLs for OpenAI's vision*)
      imageRecs = Map[<|"type" -> "image_url", "image_url" -> #|> &, images];

      body =
          TemplateApply[
            toVision, <|"text" -> StringRiffle[prompts, " "],
            "imageSpecs" -> imageRecs|>];

      (*Add options to body*)
      maxTokens = OptionValue[LLMVisionSynthesize, "MaxTokens"];
      If[IntegerQ[maxTokens], body = Append[body, "max_tokens" -> maxTokens]];

      temp = OptionValue[LLMVisionSynthesize, "Temperature"];
      If[NumericQ[temp], body = Append[body, "temperature" -> temp]];

      (*Delegage*)
      res = OpenAIRequest[{"v1", "chat", "completions"}, body];

      (*Postprocess*)
      format = OptionValue[LLMVisionSynthesize, "Format"];
      Which[
        MemberQ[{Automatic, String}, format],
        res["choices"][[1]]["message"]["content"],
        True,
        res
      ]
    ];

LLMVisionSynthesize[___] := (Message[LLMVisionSynthesize::nargs]; $Failed);


(**********************************************************)
(* LLMVisionFunction                                      *)
(**********************************************************)

Clear[LLMVisionFunction, LLMVisionFunctionTemplate];

Options[LLMVisionFunction] = Join[
  Options[StringTemplate],
  Options[LLMVisionSynthesize]
] // System`Private`SortOptions;

LLMVisionFunction[prompt_, imageSpec_, opts : OptionsPattern[]] :=
    LLMVisionFunction[prompt, {imageSpec}, opts];

LLMVisionFunction[prompt_String, imageSpecs_, opts : OptionsPattern[]] :=
    LLMVisionFunction[StringTemplate[prompt], imageSpecs, opts];

LLMVisionFunction[prompt_TemplateObject,
  imageSpecs : {(_Image | _String | _File | _URL) ..},
  opts : OptionsPattern[]] :=
    LLMVisionFunctionTemplate[prompt, imageSpecs, {opts}];

llmFunc_LLMVisionFunctionTemplate[args___] :=
    Block[{tres = TemplateApply[llmFunc[[1]], {args}]},
      LLMVisionSynthesize[tres, llmFunc[[2]], llmFunc[[3]]]
    ];


(* Summary box *)

$LLMVisionFunctionTemplate = Automatic;

ValidArgs[args___] := Length[{args}] == 3 && TrueQ[Head[{args}[[1]]] === TemplateObject];

(***** Summary Box *****)

LLMVisionFunctionTemplate /: MakeBoxes[llmFunc : LLMVisionFunctionTemplate[args___] /; ValidArgs[args], form_] := (
  BoxForm`ArrangeSummaryBox[
    LLMVisionFunctionTemplate,
    llmFunc,
    None,
    (*the next argument is the always visible properties*)
    {
      BoxForm`SummaryItem@{"Template: ", llmFunc[[1]]},
      BoxForm`SummaryItem@{"Images count: ", Length@llmFunc[[2]]}
    },
    {
      BoxForm`SummaryItem@{"Images: ", llmFunc[[2]]},
      If[Length[llmFunc[[3]]] > 0,
        BoxForm`SummaryItem @{"Options: ", llmFunc[[3]]},
        Nothing
      ]
    },
    form
  ]);


End[]; (* `Private` *)

EndPackage[]