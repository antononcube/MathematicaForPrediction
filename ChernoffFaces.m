(*
    Chernoff faces implementation in Mathematica
    Copyright (C) 2016 Anton Antonov

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
    Mathematica is (C) Copyright 1988-2014 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: Chernoff faces implementation in Mathematica *)
(* :Context: ChernoffFaces` *)
(* :Author: Anton Antonov *)
(* :Date: 2016-05-27 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 Anton Antonov *)
(* :Keywords: Chernoff face, Data visualization *)
(* :Discussion:

    Face diagrams for multidimensional data visualization are introduced by Herman Chernoff with the article:

    [1] Herman Chernoff (1973). "The Use of Faces to Represent Points in K-Dimensional Space Graphically" (PDF).
        Journal of the American Statistical Association (American Statistical Association) 68 (342): 361–368.
        doi:10.2307/2284077. JSTOR 2284077.
        URL: http://lya.fciencias.unam.mx/rfuentes/faces-chernoff.pdf .

    The Chernoff face drawn with the function `ChernoffFace` of this package can be parameterized to be asymmetric.

    The parameters argument of `ChernoffFace` mixes
    (1) face parts placement, sizes, and rotation, with
    (2) face parts colors, and
    (3) a parameter should it be attempted to make the face symmetric.

    1. The parameters for face parts placement, rotation, sizes are:

       Keys[ChernoffFace["FacePartsProperties"]]

       {"FaceLength", "ForeheadShape", "EyesVerticalPosition", "EyeSize", \
        "EyeSlant", "LeftEyebrowSlant", "LeftIris", "NoseLength", \
        "MouthSmile", "LeftEyebrowTrim", "LeftEyebrowRaising", "MouthTwist", \
        "MouthWidth", "RightEyebrowTrim", "RightEyebrowRaising", \
        "RightEyebrowSlant", "RightIris"}

       The values of those parameters are expected to be in the interval [0,1] . The order of the parameters
       is chosen to favor making symmetric faces when a list of random numbers is given as an argument, and
       to make it easier to discern the faces when multiple records are visualized.

    2. Here is the face parts colors group:

       { "FaceColor", "EyeBallColor", "IrisColor", "NoseColor", "MouthColor" }

       If the nose color is the same as the face color the nose is going to be shown "in profile", otherwise as
       a filled polygon.

    3. The parameter "MakeSymmetric" is by default True. Setting "MakeSymmetric" to true turns an incomplete face
       specification into a complete specification with the missing paired parameters filled in. In other words,
       the symmetry is not enforced on the specified paired parameters, only on the ones with missing specifications.
       The facial parts parameters of ChernoffFace are ordered in a way that would likely produce
       symmetric faces when using a random vector as an argument.

    Examples of usage follow.

    1. Very basic example:

       ChernoffFace[]

       With this signature ChernoffFace makes a random symmetric face.

    2. Basic example:

       ChernoffFace[ RandomReal[1, 12 ] ]

       With this signature ChernoffFace simply rescales the argument if it has elements outside of [0,1] and
       then matches as many as possibe of ChernoffFace["FacePartsProperties"] with the elements in the argument
       (or their rescaled values).

    2. "Proper" basic example:

       ChernoffFace[ AssociationThread[
         Keys[ChernoffFace["FacePartsProperties"]] -> RandomReal[1, Length[ChernoffFace["FacePartsProperties"]]]],
         ImageSize->Small ]

       The "proper" way to call ChernoffFace is to use an association for the face parts placement, size, rotation,
       and color. The options are passed to Graphics.

    3. A grid of faces:

       Grid[ArrayReshape[#, {3, 5}, ""], Dividers -> All] &@
           Table[(
             props = Keys[ChernoffFace["FacePartsProperties"]];
             asc = AssociationThread[props -> RandomReal[1, Length[props]]];
             asc = Join[ asc,
                         <|"FaceColor" -> Blend[{White, Brown}, RandomReal[1]],
                           "IrisColor" -> Blend[{Brown, Green, Blue}, RandomReal[1]]|>];
             asc = Merge[{<|"RightIris" -> asc["LeftIris"], "NoseColor" -> Darker[asc["FaceColor"]]|>, asc}, First];
             ChernoffFace[asc, ImageSize -> 150, AspectRatio -> Automatic]), {15}]

    In the second example notice that with `"RightIris" -> asc["LeftIris"]` both eyes are made to look into the same
    direction. Using `"RightIris" -> 1 - asc["LeftIris"]` instead would change the gap between the irises and
    their placement is going be symmetric.

    Given a full array of records, we most likely have to standardize and rescale the columns in order to use the
    function ChernoffFace. To help with that the package provides the function VariablesRescale which takes has
    the options "StandardizingFunction" and "RescaleRangeFunction".

    Consider the following example of VariableRescale invocation in which:
    1. each column is centered around its median and then devided by the inter-quartile distance,
    2. followed by clipping of the outliers that are outside of the disk with radius 3 times the quartile deviation, and
    3. rescaling to the unit interval.

      VariablesRescale[N@data,
        "StandardizingFunction" -> (Standardize[#, Median, QuartileDeviation] &),
        "RescaleRangeFunction" -> ({-3, 3} QuartileDeviation[#] &)];

    (Remark: the bottom outliers are replaced with 0 and the top outliers with 1 using Clip.)

    Anton Antonov
    2016-05-27
    Windermere, FL, USA
*)

(*

  TODO:
    1. Better explanations of the parameters ranges.
    2. Make/describe comparison between the package function faces and the faces in the original Chernoff article.
    3. (One more) advanced example.
    4. Optional use of tooltips for the facial parts interpretation.
    5. Better integration of auto coloring.
    6. Handling of a dataset with named rows and columns.

*)

(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginPackage["ChernoffFaces`"];

ChernoffFace::usage = "ChernoffFace[pars_Association,opts] plots a Chernoff face specified by pars.\n \
ChernoffFace[vec_?VectorQ,opts] plots a Chernoff face for a numeric vector by face parts implication.\n \
ChernoffFace[data : ( {_Association..| | _?MatrixQ ), opts] rescales the columns of the full array that \
corresponds to the argument data and gives a list of Chernoff faces.\n \
ChernoffFace takes all options of Graphics. Use ChernoffFace[\"Properties\"] to see the parameter names.";

ChernoffFacePartsParameters::usage := "ChernoffFacePartsParameters[] returns only those parameter associations \
taken by ChernoffFace the keys of which specify face parts placement, rotation, and sizes.";

VariablesRescale::usage = "VariablesRescale[data, opts] standardizes and rescales the columns of the data.";

PrototypeDeviationsRescale::usage = "PrototypeDeviationsRescale[proto, data] standardizes and rescales \
the columns of the data assuming the prototype argument proto is the most normal (central) row of data.";

ChernoffFaceAutoColored::usage = "ChernoffFaceAutoColored[vec_?VectorQ, colorFunc_, opts_] calls ChernoffFace \
with automatically computed colors for the face parts using a specified color function. \
If the vector has less than 2 elements the face color is determined by the first element. \
If the vector has more than 2 elements the face color is determined by the mean of the first and second elements. \
(more...)";

ChernoffFaceRecordsSummary::usage = "RecordsSummary of data with Chernoff faces. (The data is not rescaled.)";

Begin["`Private`"];

(************************************************************)
(* Variables rescale                                        *)
(************************************************************)

Clear[VariablesRescale];

Options[VariablesRescale] = {
  "StandardizingFunction" -> ( Standardize[#, Mean, StandardDeviation] &),
  "RescaleRangeFunction" -> MinMax };

VariablesRescale[ data_, opts:OptionsPattern[] ] :=
    Block[{ stFunc, rangeFunc },
      stFunc = OptionValue[VariablesRescale, "StandardizingFunction" ];
      rangeFunc = OptionValue[VariablesRescale, "RescaleRangeFunction" ];
      Transpose @ Map[ Clip[ Rescale[ #, rangeFunc[#], {0,1}], {0,1} ]&, stFunc /@ Transpose[data] ]
    ] /; MatrixQ[data, NumberQ];


Clear[PrototypeDeviationsRescale];

PrototypeDeviationsRescale[prototypeItem_, items_] :=
    Block[{},
      (* If QuartileDeviation the {0,3} range is the same as the upper bound obtained from
         OutlierIdentifiers::SPLUSQuartileIdentifierParameters . *)
      Transpose@Map[
        Clip[Rescale[Standardize[#, 0 &, StandardDeviation], {0, 3}, {0.5, 1}], {0, 1}] &,
        Transpose@Map[# - prototypeItem &, items]]
    ];


(************************************************************)
(* ChernoffFace Parameters                                  *)
(************************************************************)

(* This was the initial development ordering (in case need for debugging.) *)
(*DefaultChernoffFaceParameters[] := <|"ForeheadShape" -> 0.5,*)
(*"FaceLength" -> 0.5, "EyesVerticalPosition" -> 0.5,*)
(*"LeftEyebrowTrim" -> 0.5, "RightEyebrowTrim" -> 0.5,*)
(*"LeftEyebrowRaising" -> 0.5, "RightEyebrowRaising" -> 0.5,*)
(*"LeftEyebrowSlant" -> 0.5, "RightEyebrowSlant" -> 0.5,*)
(*"EyeSize" -> 0.5, "EyeSlant" -> 0.5, "LeftIris" -> 0.5,*)
(*"RightIris" -> 0.5, "NoseLength" -> 0.5, "MouthSmile" -> 0.5,*)
(*"MouthTwist" -> 0.5, "MouthWidth" -> 0.5, "FaceColor" -> White,*)
(*"EyeBallColor" -> White, "IrisColor" -> GrayLevel[0.85],*)
(*"NoseColor" -> White, "MouthColor" -> Black,*)
(*"MakeSymmetric" -> True|>;*)

DefaultChernoffFaceParameters[] := <|
  "FaceLength" -> 0.5, "ForeheadShape" -> 0.5, "EyesVerticalPosition" -> 0.5,
  "EyeSize" -> 0.5, "EyeSlant" -> 0.5,
  "LeftEyebrowSlant" -> 0.5, "LeftIris" -> 0.5,
  "NoseLength" -> 0.5, "MouthSmile" -> 0.5,
  "LeftEyebrowTrim" -> 0.5, "LeftEyebrowRaising" -> 0.5,
  "MouthTwist" -> 0.5, "MouthWidth" -> 0.5,
  "RightEyebrowTrim" -> 0.5, "RightEyebrowRaising" -> 0.5,
  "RightEyebrowSlant" -> 0.5, "RightIris" -> 0.5,
  "FaceColor" -> Automatic, "IrisColor" -> Automatic,
  "NoseColor" -> Automatic, "MouthColor" -> Automatic, "EyeBallColor" -> Automatic,
  "MakeSymmetric" -> True |>;

Clear[ChernoffFacePartsParameters];
ChernoffFacePartsParameters[] :=
    Pick[ChernoffFace["Properties"],
      Not /@ StringMatchQ[Keys[ChernoffFace["Properties"]], ___ ~~ ("Color" | "Symmetric")]];


(************************************************************)
(* MakeSymmetricChernoffFaceParameters                      *)
(************************************************************)

Clear[MakeSymmetricChernoffFaceParameters];

MakeSymmetricChernoffFaceParameters[pars_Association, defaultPars : (_Association | {}) : {}] :=
    Block[{symPairs, res},

      symPairs = {
        {"LeftEyebrowTrim", "RightEyebrowTrim", # &},
        {"LeftEyebrowRaising", "RightEyebrowRaising", # &},
        {"LeftEyebrowSlant", "RightEyebrowSlant", # &},
        {"LeftIris", "RightIris", 1 - # &}};

      res = Association @@
          Fold[
            Which[
              KeyExistsQ[pars, #2[[1]]] && ! KeyExistsQ[pars, #2[[2]]],
              Append[#1, #2[[2]] -> #2[[3]][pars[#2[[1]]]]],

              ! KeyExistsQ[pars, #2[[1]]] && KeyExistsQ[pars, #2[[2]]],
              Append[#1, #2[[1]] -> #2[[3]][pars[#2[[2]]]]],

              True, #1
            ] &,
            {},
            symPairs
          ];

      If[defaultPars === {},
        Merge[{pars, res}, First],
        Merge[{pars, res, defaultPars}, First]
      ]
    ];


(************************************************************)
(* ChernoffFace                                             *)
(************************************************************)

Clear[ChernoffFace];

SyntaxInformation[ChernoffFace] = { "ArgumentsPattern" -> { _., OptionsPattern[] } };

Options[ChernoffFace] =
    Join[
      { ColorFunction -> None, "MakeSymmetric" -> Automatic },
      Options[VariablesRescale],
      Options[Graphics]
    ];

ChernoffFace::pars = "The first argument is expected to be an association, a numeric list, \
a list of associations, or a list of numeric lists.";
ChernoffFace::colfunc = "The value of the option ColorFunction is expected to have the form ColorDataFunction[___] \
or be one of None or Automatic.";
ChernoffFace::sarg = "Chernoff faces summaries are made for lists of numerical vectors or lists of associations.";

ChernoffFace["Properties"] := DefaultChernoffFaceParameters[];
ChernoffFace["FaceParts"] := ChernoffFacePartsParameters[];
ChernoffFace["FacePartsProperties"] := ChernoffFacePartsParameters[];

ChernoffFace[ opts:OptionsPattern[] ] :=
    Block[{asc},
      asc = AssociationThread[
        Keys[ChernoffFace["FacePartsProperties"]] -> RandomReal[1, Length[ChernoffFace["FacePartsProperties"]]]];
      asc = Pick[asc, StringMatchQ[Keys[asc], StartOfString ~~ Except["R"] ~~ __]];
      ChernoffFace[asc,opts]
    ];

ChernoffFace[vec_?(VectorQ[#,NumberQ]&), opts:OptionsPattern[]] :=
    Block[{mn, mx, pars},
      pars = Take[vec, UpTo[Min[Length[vec],Length[ChernoffFace["FacePartsProperties"]]]] ];
      {mn, mx} = MinMax[pars];
      If[ mn< 0 || mx > 1, pars = Rescale[pars] ];
      pars = AssociationThread[ Take[Keys[ChernoffFace["FacePartsProperties"]], Length[pars]] -> pars];
      ChernoffFace[ pars, opts ]
    ];

ChernoffFace[parsArg_Association, opts : OptionsPattern[]] :=
    Block[{pars = parsArg, scaledPars,
      cdf, colorParts,
      foreheadPts, forheadTh, faceLength, eyesVerticalPos,
      rightIrisOffset, leftIrisOffset,
      eyebrRaiseLeft, eyebrRaiseRight, eyebrSlantLeft, eyebrSlantRight,
      eyebrLeftTrim, eyebrRightTrim, eyeSize, eyesSlant, leftEye,
      rightEye,
      noseLength, mouthWidth, a, b, c, faceColor, eyeBallsColor,
      irisColor, noseColor, mouthColor,
      makeSymmetric},

      (* Color function *)
      cdf = OptionValue[ChernoffFace, ColorFunction];

      If[ StringQ[cdf] || IntegerQ[cdf], cdf = ColorData[cdf] ];

      If[ ! ( TrueQ[cdf === None] || TrueQ[cdf === Automatic] || TrueQ[ Head[cdf]===ColorDataFunction] ),
        Message[ChernoffFace::colfunc];
        Return[$Failed]
      ];

      If[ TrueQ[cdf === Automatic], cdf = ColorData["Pastel"] ];

      (* Make symmetric *)
      makeSymmetric = OptionValue[ ChernoffFace, "MakeSymmetric" ];

      If[ TrueQ[ makeSymmetric === Automatic ],
        makeSymmetric = Lookup[pars, "MakeSymmetric", True],
        (* ELSE *)
        makeSymmetric = TrueQ[makeSymmetric]
      ];

      pars =
          If[TrueQ[makeSymmetric],
            MakeSymmetricChernoffFaceParameters[pars, ChernoffFace["Properties"]],
            Merge[{pars, ChernoffFace["Properties"]}, First]
          ];

      (* Color parts *)
      colorParts = KeyTake[ pars, {"FaceColor", "IrisColor", "NoseColor", "MouthColor", "EyeBallColor" } ];
      colorParts = Select[colorParts, !TrueQ[ # === Automatic]& ];

      Which[
        cdf === None,
        pars =
            Join[ pars,
              <| "FaceColor" -> White, "IrisColor" -> GrayLevel[0.85], "NoseColor" -> Automatic, "MouthColor" -> Black, "EyeBallColor" -> White |> ],

        True,
        pars = Join[ pars, ChernoffFaceAutoColors[pars, cdf]]
      ];

      scaledPars = KeyTake[pars, ChernoffFace["FaceParts"]];
      scaledPars = AssociationThread[ Keys[scaledPars], Rescale[Values[scaledPars]] ];

      (* Parameters *)
      pars = Join[pars, scaledPars, colorParts];

      forheadTh = 2*Round@Rescale[pars["ForeheadShape"], {0, 1}, {2, 15}];
      faceLength = Rescale[pars["FaceLength"], {0, 1}, {2, 3}];

      eyesVerticalPos = Rescale[pars["EyesVerticalPosition"], {0, 1}, {0.2, 0.6}];

      eyebrLeftTrim = Rescale[pars["LeftEyebrowTrim"], {0, 1}, {0, 1}];
      eyebrRightTrim = Rescale[pars["RightEyebrowTrim"], {0, 1}, {0, 1}];
      eyebrRaiseLeft = Rescale[pars["LeftEyebrowRaising"], {0, 1}, {0.5, 0.8}];
      eyebrRaiseRight = Rescale[pars["RightEyebrowRaising"], {0, 1}, {0.5, 0.8}];
      eyebrSlantLeft = Rescale[pars["LeftEyebrowSlant"], {0, 1}, {-Pi/6, Pi/6}];
      eyebrSlantRight = Rescale[pars["RightEyebrowSlant"], {0, 1}, {Pi/6, -Pi/6}];

      eyeSize = Rescale[pars["EyeSize"], {0, 1}, {0.4, 1}];
      eyesSlant = Rescale[pars["EyeSlant"], {0, 1}, {-Pi/6, Pi/6}];

      leftIrisOffset = Rescale[pars["LeftIris"], {0, 1}, {-0.63, -0.37}];
      rightIrisOffset = Rescale[pars["RightIris"], {0, 1}, {0.37, 0.63}];

      noseLength = Rescale[pars["NoseLength"], {0, 1}, {0.2, 0.65}];
      a = Rescale[pars["MouthSmile"], {0, 1}, {-2, 2}];
      b = Rescale[pars["MouthTwist"], {0, 1}, {-0.25, 0.25}];
      c = -0.8;(*Rescale[pars["MouthPosition"],{0,1},{-1,-0.7}];*)
      mouthWidth = Rescale[pars["MouthWidth"], {0, 1}, {0.1, 0.7}];

      faceColor = pars["FaceColor"];
      eyeBallsColor = pars["EyeBallColor"];
      irisColor = pars["IrisColor"];
      noseColor = pars["NoseColor"];
      mouthColor = pars["MouthColor"];

      If[ TrueQ[noseColor === Automatic],
        noseColor = If[ TrueQ[faceColor == White], White, Darker[faceColor] ]
      ];

      (*foreheadPts={{-1,0},{-1+0.3forheadTh,1.2},{1-0.3forheadTh,1.2},{1,0}};*)
      (*{Thick,BSplineCurve[foreheadPts,SplineWeights\[Rule]({1,3,3,1}/8)]},*)
      foreheadPts = Table[{x, (1 - x^forheadTh)*faceLength*eyesVerticalPos}, {x, Range[-1, 1, 0.05]}];
      leftEye =
          {{eyeBallsColor, Disk[{-0.5, 0}, eyeSize {0.4, 0.2}]}, {Thick,
            Gray, Circle[{-0.5, 0}, eyeSize {0.4, 0.2}]},
            {irisColor, Disk[{leftIrisOffset, 0.02}, eyeSize 0.15, {0, 2 Pi}]}, {Black,
            Disk[{leftIrisOffset, 0.02}, eyeSize 0.05, {0, 2 Pi}]}
          };
      rightEye =
          {{eyeBallsColor, Disk[{0.5, 0}, eyeSize {0.4, 0.2}]}, {Thick,
            Gray, Circle[{0.5, 0}, eyeSize {0.4, 0.2}]},
            {irisColor, Disk[{rightIrisOffset, 0.02}, eyeSize 0.15, {0, 2 Pi}]},
            {Black, Disk[{rightIrisOffset, 0.02}, eyeSize 0.05, {0, 2 Pi}]}
          };

      Graphics[{
        {EdgeForm[None], FaceForm[faceColor], Polygon[foreheadPts], Thick, Black, Line[foreheadPts]},
        {faceColor,
          Disk[{0, 0}, {1, faceLength (1 - eyesVerticalPos)}, {Pi, 2 Pi}]},
        {Thick, Circle[{0, 0}, {1, faceLength (1 - eyesVerticalPos)}, {Pi, 2 Pi}]},
      (* Eyes *)
        {GeometricTransformation[leftEye, RotationTransform[eyesSlant, {-0.5, 0}]],
          GeometricTransformation[rightEye, RotationTransform[-eyesSlant, {0.5, 0}]]},
      (* Eyebrows trimming, raising, and slant *)
        {Thickness[0.02],
          GeometricTransformation[
            Circle[{-0.5, -0.2}, {0.7, eyebrRaiseLeft}, {Pi/2 - (eyebrLeftTrim Pi)/6, Pi/2 + (eyebrLeftTrim Pi)/6}],
            RotationTransform[eyebrSlantLeft, {-0.5, -0.2 + eyebrRaiseLeft}]],
          GeometricTransformation[
            Circle[{0.5, -0.2}, {0.7, eyebrRaiseRight}, {Pi/2 - (eyebrRightTrim Pi)/6, Pi/2 + (eyebrRightTrim Pi)/6}],
            RotationTransform[eyebrSlantRight, {0.5, -0.2 + eyebrRaiseRight}]]},
        If[TrueQ[noseColor =!= faceColor],
          {EdgeForm[Black], Thick, FaceForm[noseColor],
            Polygon[{{0.012, 0}, {-0.012, 0}, {-0.1, -noseLength}, {0.1, -noseLength}}]},
          {Thick, Black, Line[{{0.012, 0}, {-0.012, 0}, {-0.1, -noseLength}, {0.1, -noseLength}}]}
        ], {mouthColor, Thickness[0.02],
          Line[Table[{x, a x^2 + b x + c}, {x, -mouthWidth/2, mouthWidth/2, 0.01}]]}},
        FilterRules[{opts}, Options[Graphics]], PlotRange -> All, AspectRatio -> Automatic]
    ];


(*---------------------------------------------------------*)
(* Listable                                                *)
(*---------------------------------------------------------*)

Clear[AssociationRecordsRescale];
Options[AssociationRecordsRescale] = Options[VariablesRescale];
AssociationRecordsRescale[parsArgs : { _Association .. }, opts:OptionsPattern[]]:=
    Block[{rdata},
      rdata = Map[ KeyTake[#, Keys@ChernoffFace["Properties"] ]&, parsArgs];
      If[ (Equal @@ Map[Keys, rdata]) && MatrixQ[Values /@ rdata],
        rdata = Map[ AssociationThread[Keys[rdata][[1]], #]&, VariablesRescale[ Values /@ rdata, FilterRules[{opts}, Options[VariablesRescale]] ]]
      ];
      rdata
    ];

ChernoffFace[parsArgs : { _Association .. }, opts : OptionsPattern[]] :=
    Block[{rdata},
      rdata = AssociationRecordsRescale[parsArgs, FilterRules[{opts}, Options[AssociationRecordsRescale]] ];
      ChernoffFace[#, opts]& /@ rdata
    ];

ChernoffFace[parsArgs : { _Association .. }, "RecordsSummary" | "Summary", opts:OptionsPattern[] ] :=
    Block[{rdata},
      rdata = AssociationRecordsRescale[parsArgs, FilterRules[{opts}, Options[AssociationRecordsRescale]] ];
      ChernoffFaceRecordsSummary[ rdata, FilterRules[{opts}, Options[ChernoffFace]] ]
    ];

ChernoffFace[data_?(MatrixQ[#, NumericQ]&), opts : OptionsPattern[]] :=
    Block[{rdata},
      rdata = VariablesRescale[N@data, FilterRules[{opts}, Options[VariablesRescale]]];
      ChernoffFace[#, opts]& /@ rdata
    ];

ChernoffFace[data_?(MatrixQ[#, NumericQ]&), "RecordsSummary" | "Summary", opts:OptionsPattern[] ] :=
    Block[{rdata},
      rdata = VariablesRescale[N@data, FilterRules[{opts}, Options[VariablesRescale]]];
      ChernoffFaceRecordsSummary[rdata, FilterRules[{opts}, Options[ChernoffFace]] ]
    ];


(*---------------------------------------------------------*)
(* Failing                                                 *)
(*---------------------------------------------------------*)

ChernoffFace[_, "RecordsSummary" | "Summary", ___] :=
    Block[{},
      Message[ChernoffFace::sarg];
      $Failed
    ];

ChernoffFace[___] := (Message[ChernoffFace::pars]; $Failed);


(************************************************************)
(* ChernoffFaceAutoColors                                   *)
(************************************************************)

Clear[ChernoffFaceAutoColors];

ChernoffFaceAutoColors[asc_Association, cdf_ColorDataFunction] :=
    Block[{vec, priorityKeys},
      priorityKeys = {"FaceLength", "ForeheadShape", "EyesVerticalPosition", "EyeSize", "EyeSlant", "LeftEyebrowSlant", "LeftIris"};
      vec = Values @ KeyTake[asc, priorityKeys];
      Join[ asc, ChernoffFaceAutoColors[vec, cdf] ]
    ];

ChernoffFaceAutoColors[vec_?VectorQ, cdf_ColorDataFunction] :=
    Block[{asc = <||>},

      Which[
        Length[vec] == 1,
        asc = Join[asc, <|"FaceColor" -> cdf[vec[[1]]]|>],
        Length[vec] <= 2,
        asc = Join[asc, <|"FaceColor" -> cdf[vec[[1]]], "IrisColor" -> cdf[vec[[2]]]|>],
        Length[vec] <= 6,
        asc = Join[asc, <|"FaceColor" -> cdf[Mean@vec[[1 ;; 2]]], "IrisColor" -> cdf[Mean@vec[[3 ;; -1]]]|>],
        True,
        asc = Join[asc, <|
          "FaceColor" -> cdf[Mean@vec[[1 ;; 2]]],
          "MouthColor" -> cdf[Mean@vec[[1 ;; 3]]],
          "IrisColor" -> cdf[Mean@vec[[4 ;; 6]]],
          "NoseColor" -> cdf[Mean@vec[[7 ;; -1]]]|>]
      ];

      Join[ asc, <| "EyeBallColor" -> White |> ]
    ];


(************************************************************)
(* ChernoffFaceAutoColored                                  *)
(************************************************************)

Clear[ChernoffFaceAutoColored];

ChernoffFaceAutoColored[vec_ : ( _?VectorQ | _Association ), opts : OptionsPattern[]] :=
    ChernoffFaceAutoColored[vec, ColorData["Pastel"], opts];

ChernoffFaceAutoColored[vec : ( _?VectorQ | _Association ), cdf_ColorDataFunction, opts : OptionsPattern[]] :=
    Block[{asc},
      asc = AssociationThread[Take[Keys[ChernoffFace["FacePartsProperties"]], UpTo[Length[vec]]] -> vec];
      ChernoffFace[ Join[ asc, ChernoffFaceAutoColors[vec, cdf] ], opts]
    ];


(************************************************************)
(* ChernoffFaceRecordsSummary                               *)
(************************************************************)

(* Find the median and quartile faces (used to help interpretation.) *)
ClearAll[ChernoffFaceRecordsSummary];

Options[ChernoffFaceRecordsSummary] = Options[ChernoffFace];

ChernoffFaceRecordsSummary[rdata_, opts : OptionsPattern[]] :=
    ChernoffFaceRecordsSummary[rdata, OptionValue[ChernoffFaceRecordsSummary, ColorFunction], opts ];

ChernoffFaceRecordsSummary[rdata_, cdf:(_ColorDataFunction|None|Automatic|_String|_Integer), opts : OptionsPattern[]] :=
    Block[{cdfLocal, qvals, cfFunc, quantileFaces, rangeFaces},

      Which[
        StringQ[cdf] || IntegerQ[cdf],
        cdfLocal = ColorData[cdf],

        True,
        cdfLocal = cdf
      ];

      qvals = Map[Quantile[#,{0,0.25,0.5,0.75,1}]&, Transpose[N@rdata]];

      cfFunc = ChernoffFace[#1, PlotLabel -> #2, ColorFunction -> cdfLocal, opts] &;

      quantileFaces =
          MapThread[ cfFunc, {Transpose[qvals], {"Min", "1st Qu", "Median", "3d Qu", "Max"}}];
      rangeFaces =
          MapThread[
            cfFunc,
            {Transpose[ ConstantArray[{0, 0.25, 0.5, 0.75, 1}, Length@Transpose[rdata]]],
            {"All 0", "All 0.25", "All 0.5", "All 0.75", "All 1"}}
          ];

      <| "QuantileChernoffFaces"->quantileFaces, "RangeChernoffFaces"->rangeFaces |>
    ];


End[]; (* `Private` *)

EndPackage[]