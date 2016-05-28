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

    The parameters argument mixes (1) face parts placement, sizes, and rotation, with (2) face parts colors,
    and (3) a parameter should it be attempted to make the face symmetric.

    1. The parameters for face parts placement, rotation, sizes are:

       Keys[ChernoffFace["Properties"]]

       {"ForheadShape", "FaceLength", "EyesVerticalPosition", \
        "LeftEyebrowTrim", "RightEyebrowTrim", "LeftEyebrowRaising", \
        "RightEyebrowRaising", "LeftEyebrowSlant", "RightEyebrowSlant", \
        "EyeSize", "EyeSlant", "LeftIris", "RightIris", "NoseLength", \
        "MouthSmile", "MouthTwist", "MouthWidth"}

       The values of those parameters are expected to be in the interval [0,1] .

    2. Here is the face parts colors group:

       { "FaceColor", "EyeBallColor", "IrisColor", "NoseColor", "MouthColor" }

       If the nose color is the same as the face color the nose is going to be shown "in profile", otherwise as
       a filled polygon.

    3. The parameter "MakeSymmetric" is by default True. Setting "MakeSymmetric" turns incomplete face specification
       into a complete specification with the missing paired parameters made filled in. In other words, the symmetricity
       is not enforced on the  specified paired parameters, only on the ones for which specifications are missing.


    Examples of usage follow.

    1. Basic example:

       ChernoffFace[ AssociationThread[
         Keys[ChernoffFace["FacePartsProperties"]] -> RandomReal[1, Length[ChernoffFace["FacePartsProperties"]]]]]

    2. A grid of faces:

       Grid[ArrayReshape[#, {3, 5}, ""], Dividers -> All] &@
           Table[(
             props = {"ForheadShape", "FaceLength", "LeftEyebrowTrim",
                      "RightEyebrowTrim", "LeftEyebrowRaising", "RightEyebrowRaising",
                      "EyeSlant", "LeftIris", "RightIris", "NoseLength", "MouthSmile",
                      "MouthTwist", "MouthWidth"};
             asc = AssociationThread[props -> RandomReal[1, Length[props]]];
             asc = Join[ asc,
                         <|"FaceColor" -> Blend[{White, Brown}, RandomReal[1]],
                           "IrisColor" -> Blend[{Brown, Green, Blue}, RandomReal[1]]|>];
             asc = Merge[{<|"RightIris" -> asc["LeftIris"], "NoseColor" -> Darker[asc["FaceColor"]]|>, asc}, First];
             ChernoffFace[asc, ImageSize -> 150, AspectRatio -> Automatic]), {15}]

    In the second example notice that with `"RightIris" -> asc["LeftIris"]` both eyes are made to look into the same
    direction. Using `"RightIris" -> 1 - asc["LeftIris"]` instead would change the gap between the irises and
    their placement is going be symmetric.

    Anton Antonov
    2016-05-27
    Windermere, FL, USA
*)

(*

  TODO:
    1. Better explanations of the parameters ranges.
    2. Make/describe comparison between the package function faces and the faces in original Chernoff article.
    3. (One more) advanced example.

*)

(* Created by Mathematica Plugin for IntelliJ IDEA *)

BeginPackage["ChernoffFaces`"]

ChernoffFace::usage = "ChernoffFace[pars_Association,opts] plots a Gernoff face specified by pars. The options \
paramerter opts is passed to Graphics. Use ChernoffFace[\"Properties\"] to see the parameter names."

ChernoffFacePartsParameters::usage := "ChernoffFacePartsParameters[] returns only the names(keys) only of those \
parameters taken by ChernoffFace that specify face parts placement, rotation, and sizes."

Begin["`Private`"]

DefaultChernoffFaceParameters[] := <|"ForheadShape" -> 0.5,
  "FaceLength" -> 0.5, "EyesVerticalPosition" -> 0.5,
  "LeftEyebrowTrim" -> 0.5, "RightEyebrowTrim" -> 0.5,
  "LeftEyebrowRaising" -> 0.5, "RightEyebrowRaising" -> 0.5,
  "LeftEyebrowSlant" -> 0.5, "RightEyebrowSlant" -> 0.5,
  "EyeSize" -> 0.5, "EyeSlant" -> 0.5, "LeftIris" -> 0.5,
  "RightIris" -> 0.5, "NoseLength" -> 0.5, "MouthSmile" -> 0.5,
  "MouthTwist" -> 0.5, "MouthWidth" -> 0.5, "FaceColor" -> White,
  "EyeBallColor" -> White, "IrisColor" -> GrayLevel[0.85],
  "NoseColor" -> White, "MouthColor" -> Black,
  "MakeSymmetric" -> True|>;

Clear[ChernoffFacePartsParameters]
ChernoffFacePartsParameters[] :=
    Pick[ChernoffFace["Properties"],
      Not /@ StringMatchQ[Keys[ChernoffFace["Properties"]], ___ ~~ ("Color" | "Symmetric")]];

Clear[MakeSymmetricChernoffFaceParameters]
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
            ] &, {}, symPairs];
      If[defaultPars === {},
        Merge[{pars, res}, First],
        Merge[{pars, res, defaultPars}, First]
      ]
    ];

Clear[ChernoffFace]
ChernoffFace["Properties"] := DefaultChernoffFaceParameters[];
ChernoffFace["FacePartsProperties"] := ChernoffFacePartsParameters[];
ChernoffFace[parsArg_Association, opts : OptionsPattern[]] :=
    Block[{pars = parsArg,
      forheadPts, forheadTh, gr, faceLength, eyesVerticalPos,
      rightIrisOffset, leftIrisOffset,
      eyebrRaiseLeft, eyebrRaiseRight, eyebrSlantLeft, eyebrSlantRight,
      eyebrLeftTrim, eyebrRightTrim, eyeSize, eyesSlant, leftEye,
      rightEye,
      noseLength, mouthWidth, a, b, c, faceColor, eyeBallsColor,
      irisColor, noseColor, mouthColor,
      makeSymmetric},
      makeSymmetric = Lookup[pars, "MakeSymmetric", True];
      pars =
          If[TrueQ[makeSymmetric],
            MakeSymmetricChernoffFaceParameters[pars, ChernoffFace["Properties"]],
            Merge[{pars, ChernoffFace["Properties"]}, First]
          ];
      forheadTh = 2*Round@Rescale[pars["ForheadShape"], {0, 1}, {2, 15}];
      faceLength = Rescale[pars["FaceLength"], {0, 1}, {2, 3}];
      eyesVerticalPos = Rescale[pars["EyesVerticalPosition"], {0, 1}, {0.2, 0.7}];
      eyebrLeftTrim = Rescale[pars["LeftEyebrowTrim"], {0, 1}, {0, 1}];
      eyebrRightTrim = Rescale[pars["RightEyebrowTrim"], {0, 1}, {0, 1}];
      eyebrRaiseLeft = Rescale[pars["LeftEyebrowRaising"], {0, 1}, {0.6, 0.73}];
      eyebrRaiseRight = Rescale[pars["RightEyebrowRaising"], {0, 1}, {0.6, 0.73}];
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
      (*forheadPts={{-1,0},{-1+0.3forheadTh,1.2},{1-0.3forheadTh,1.2},{1,0}};*)
      (*{Thick,BSplineCurve[forheadPts,SplineWeights\[Rule]({1,3,3,1}/8)]},*)
      forheadPts = Table[{x, (1 - x^forheadTh)*faceLength eyesVerticalPos}, {x, Range[-1, 1, 0.05]}];
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
        {EdgeForm[None], FaceForm[faceColor], Polygon[forheadPts], Thick,
          Black, Line[forheadPts]},
        {faceColor,
          Disk[{0, 0}, {1, faceLength (1 - eyesVerticalPos)}, {Pi, 2 Pi}]},
          {Thick, Circle[{0, 0}, {1, faceLength (1 - eyesVerticalPos)}, {Pi, 2 Pi}]},
      (* Eyes *)
        {GeometricTransformation[leftEye, RotationTransform[eyesSlant, {-0.5, 0}]],
          GeometricTransformation[rightEye, RotationTransform[-eyesSlant, {0.5, 0}]]},
      (* Eyebrows trimming, raising, and slant *)
        {Thick,
          GeometricTransformation[
            Circle[{-0.5, -0.2}, {0.7,
              eyebrRaiseLeft}, {Pi/2 - (eyebrLeftTrim Pi)/6, Pi/
                2 + (eyebrLeftTrim Pi)/6}],
            RotationTransform[
              eyebrSlantLeft, {-0.5, -0.2 + eyebrRaiseLeft}]],
          GeometricTransformation[
            Circle[{0.5, -0.2}, {0.7,
              eyebrRaiseRight}, {Pi/2 - (eyebrRightTrim Pi)/6, Pi/
                2 + (eyebrRightTrim Pi)/6}],
            RotationTransform[
              eyebrSlantRight, {0.5, -0.2 + eyebrRaiseRight}]]},
        If[TrueQ[noseColor =!= faceColor],
          {EdgeForm[Black], Thick, FaceForm[noseColor],
            Polygon[{{0.012, 0}, {-0.012,
              0}, {-0.1, -noseLength}, {0.1, -noseLength}}]},
          {Thick, Black,
            Line[{{0.012, 0}, {-0.012,
              0}, {-0.1, -noseLength}, {0.1, -noseLength}}]}
        ], {mouthColor, Thickness[0.02],
          Line[Table[{x, a x^2 + b x + c}, {x, -mouthWidth/2,
            mouthWidth/2, 0.01}]]}}, opts, PlotRange -> All,
        AspectRatio -> Automatic]
    ];

End[] (* `Private` *)

EndPackage[]