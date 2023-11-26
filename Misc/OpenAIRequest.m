(* OpenAI request Mathematica Package *)

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

(* :Title: OpenAIRequest *)
(* :Context: OpenAIRequest` *)
(* :Author: Anton Antonov *)
(* :Date: 2023-11-26 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.1 *)
(* :Copyright: (c) 2023 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

  The code below is taken and a _little_ simplified from: https://github.com/chriswolfram/OpenAILink .

  This package makes the OpenAI utilization simpler and more direct.
*)

BeginPackage["OpenAIRequest`"];
(* Exported symbols added here with SymbolName::usage *)

OpenAIRequest::usage = "OpenAI's Web API invoker.";

Begin["`Private`"];

(*****************************************************************)
(*	OpenAIRequest[path, body, opts, head]                        *)
(*		makes a request to the OpenAI API at the specified path and with the specified body.*)
(*		path: a list of strings in the format expected by URLRead. *)
(*		body: an expression which is automatically converted to JSON. If the body is None, no body will be specified. *)
(*		opts: a list containing the options passed to the higher-level request function. *)
(*		head: the head of the higher-level request function for use in option resolution and messages. *)
(*****************************************************************)

ClearAll[OpenAIRequest];

Options[OpenAIRequest] = {
  "APIKey" -> Automatic,
  "User" -> Automatic
};

OpenAIRequest::invalidOpenAIAPIKey = "`1` is not a valid OpenAI API key. Consider setting the OpenAIKey option or the $OpenAIKey constant to a string containing a valid OpenAI API key.";
OpenAIRequest::openAIResponseFailureCode = "Request to the OpenAI API failed with status code `1`.";
OpenAIRequest::openAIResponseFailureMessage = "Request to the OpenAI API failed with message: `1`.";

OpenAIRequest[path_, body : _Association | None : None, opts_List : {}, head_ : OpenAIRequest] :=
    Enclose[
      Module[{apiKey, user, bodyRule, resp, multipartQ = body =!= None && MemberQ[body, _Association]},

        apiKey = OptionValue[head, opts, "APIKey"];
        If[ TrueQ[apiKey === Automatic],
          apiKey = SystemCredential["OPENAI_API_KEY"]
        ];

        user = OptionValue[head, opts, "User"];
        If[ TrueQ[user === Automatic],
          user = SystemCredential["OPENAI_USER"];
          user = If[!StringQ[user], "user", user];
        ];

        ConfirmBy[apiKey, StringQ,
          Message[head::invalidOpenAIAPIKey, apiKey];
          Failure["InvalidOpenAIKey", <|
            "MessageTemplate" :> head::invalidOpenAIAPIKey,
            "MessageParameters" -> {apiKey}
          |>]
        ];

        bodyRule =
            If[body === None,
              Nothing,
              "Body" -> Confirm[
                If[  multipartQ,
                  Identity,
                  ExportByteArray[#, "JSON"] &
                ] @ If[user =!= None, Append[body, "user" -> user], body],
                $Failed
              ]
            ];

        resp = URLRead[
          <|
            "Scheme" -> "https",
            "Domain" -> "api.openai.com",
            "Method" -> If[body === None, "GET", "POST"],
            "ContentType" -> If[multipartQ, "multipart/form-data", "application/json"],
            "Path" -> path,
            bodyRule,
            "Headers" -> {
              "Authorization" -> "Bearer " <> apiKey
            }
          |>
        ];

        ConformResponse[head, resp]

      ],
      "InheritedFailure"
    ];


(*****************************************************************)
(*	ConformResponse[head, resp]                                  *)
(*		takes an HTTPResponse and returns the contained JSON data. *)
(*    Returns a Failure if the request failed.                   *)
(*		head is used for messages.                                 *)
(*****************************************************************)

ConformResponse[head_, resp_] :=
    Catch@Module[{statusCode, body},

      statusCode = resp["StatusCode"];
      body = ImportByteArray[resp["BodyByteArray"], "RawJSON"];

      If[FailureQ[body],
        Throw@ResponseFailureCode[head, resp, statusCode]
      ];

      Which[

        KeyExistsQ[body, "error"],
        If[KeyExistsQ[body["error"], "message"],
          Throw@ResponseFailureMessage[head, resp, body["error"]["message"]],
          Throw@ResponseFailureCode[head, resp, statusCode]
        ],

        statusCode =!= 200,
        Throw@ResponseFailureCode[head, resp, statusCode],

        True,
        body
      ]
    ];


ResponseFailureCode[head_, resp_, code_] :=
    (
      Message[head::openAIResponseFailureCode, code];
      Failure["OpenAIResponseFailure", <|
        "MessageTemplate" :> head::openAIResponseFailureCode,
        "MessageParameters" -> {code},
        "HTTPResponse" -> resp
      |>]
    );


ResponseFailureMessage[head_, resp_, message_] :=
    (
      Message[head::openAIResponseFailureMessage, message];
      Failure["OpenAIResponseFailure", <|
        "MessageTemplate" :> head::openAIResponseFailureMessage,
        "MessageParameters" -> {message},
        "HTTPResponse" -> resp
      |>]
    );


End[]; (* `Private` *)

EndPackage[]