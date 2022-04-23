(*
    LSAMon for Image Collections Mathematica package
    Copyright (C) 2022 Anton Antonov

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
  	ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
  	Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2022 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: LSAMonForImageCollections *)
(* :Context: LSAMonForImageCollections` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-04-23 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2022 Anton Antonov *)
(* :Keywords: LSA, Image, Image Processing, Dimension Reduction*)
(* :Discussion: *)

(**************************************************************)
(* Importing packages (if needed)                             *)
(**************************************************************)

If[Length[DownValues[MonadicLatentSemanticAnalysis`LSAMonUnit]] == 0,
  Echo["MonadicLatentSemanticAnalysis.m", "Importing from GitHub:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m"]
];


(**************************************************************)
(* Package definition                                         *)
(**************************************************************)

BeginPackage["LSAMonForImageCollections`"];
(* Exported symbols added here with SymbolName::usage *)

ImageToVector::usage = "ImageToVector";
PrepareImageData::usage = "PrepareImageData";
DimensionReductionWithLSA::usage = "DimensionReductionWithLSA";
GetImageBasis::usage = "GetImageBasis";
RepresentImage::usage = "RepresentImage";

Begin["`Private`"];

Needs["SSparseMatrix`"];
Needs["MonadicLatentSemanticAnalysis`"];

(*----------------------------------------------------------*)
(* ImageToVector                                            *)
(*----------------------------------------------------------*)

Clear[ImageToVector];
ImageToVector[img_Image] := Flatten[ImageData[ColorConvert[img, "Grayscale"]]];
ImageToVector[img_Image, imgSize_] := Flatten[ImageData[ColorConvert[ImageResize[img, imgSize], "Grayscale"]]];
ImageToVector[___] := $Failed;

(*----------------------------------------------------------*)
(* Data preparation                                         *)
(*----------------------------------------------------------*)

Clear[PrepareImageData];

Options[PrepareImageData] = {"NumberOfImages" -> 400, ImageSize -> 200, "RorschachEffect" -> True, "Dilation" -> True, "Generator" -> Automatic, "LoggerFunction" -> Print};

PrepareImageData[opts : OptionsPattern[]] :=
    Block[{n, imgSize, rorschachEffectQ, scribblesQ, dilationQ, generator, PRINT,
      lsDoodles, lsDoodleImages, lsDoodleImages2, aDImages, aDImageVecs},

      n = OptionValue[PrepareImageData, "NumberOfImages"];
      imgSize = OptionValue[PrepareImageData, ImageSize];
      rorschachEffectQ = OptionValue[PrepareImageData, "RorschachEffect"];
      scribblesQ = OptionValue[PrepareImageData, "NumberOfImages"];
      dilationQ = OptionValue[PrepareImageData, "Dilation"];
      PRINT = OptionValue[PrepareImageData, "LoggerFunction"];

      generator = OptionValue[PrepareImageData, "Generator"];

      generator =
          Which[
            MemberQ[{"Scribble", "RandomScribble", Automatic}, generator],

            Hold[ResourceFunction["RandomScribble"]["NumberOfStrokes" -> {20, 16},
              "OrderedStrokePoints" -> False,
              "ConnectingFunction" -> FilledCurve@*BezierCurve,
              ColorFunction -> (Black&),
              ImageSize -> Small]],

            MemberQ[{"Mandala", "RandomMandala"}, generator],

            Hold[ResourceFunction["RandomMandala"]["RotationalSymmetryOrder" -> 6,
              "ConnectingFunction" -> FilledCurve@*BezierCurve, ColorFunction -> None,
              ImageSize -> Small]]

          ];

      PRINT["Make doodles:"];
      PRINT @ AbsoluteTiming[ lsDoodles = Table[ReleaseHold[generator], n]; ];

      PRINT["Color negate and dilate:"];
      PRINT @ AbsoluteTiming[
        If[dilationQ,
          lsDoodles = ParallelMap[ColorNegate[Dilation[ColorNegate[#], 3]] &, lsDoodles];
        ]
      ];

      PRINT["Binarize and resize:"];
      PRINT @ AbsoluteTiming[
        With[{imgSize = imgSize},
          lsDoodleImages = ParallelMap[ColorNegate@Binarize@ImageResize[Image[#], {imgSize, imgSize}] &, lsDoodles];
        ]
      ];

      (* Rorschach effect :*)

      If[rorschachEffectQ,
        PRINT["Apply Rorschach effect:"];
        PRINT @ AbsoluteTiming[
          lsDoodleImages =
              ParallelMap[ImageAssemble[{#, ImageReflect[#, Left -> Right]}] &, ImageCrop /@ lsDoodleImages];
        ]
      ];

      PRINT["Image resize:"];
      PRINT @ AbsoluteTiming[
        Block[{w, h},
          {w, h} = Max /@ Transpose[Map[ImageDimensions, lsDoodleImages]];
          With[{w = w, h = h},
            lsDoodleImages2 = ParallelMap[ImageResize[#, {w, h}] &, lsDoodleImages];
          ]
        ]
      ];

      aDImages =
          AssociationThread[
            Map[ToString, Range[Length[lsDoodleImages2]]] -> lsDoodleImages2];

      (* Flatten each of the images into vectors :*)

      PRINT["Color convert to \"GrayScale\":"];
      PRINT @ AbsoluteTiming[
        aDImageVecs =
            ParallelMap[Flatten[ImageData[Binarize@ColorConvert[#, "Grayscale"]]] &, aDImages];
      ];

      <| "Images" -> aDImages, "ImageVectors" -> aDImageVecs|>
    ];
PrepareImageData[___] := $Failed;


(*----------------------------------------------------------*)
(* DimensionReductionWithLSA                                *)
(*----------------------------------------------------------*)

Clear[DimensionReductionWithLSA];
Options[DimensionReductionWithLSA] = Options[LSAMonNormalizeMatrixProduct];
DimensionReductionWithLSA[aDImageVecs : Association[(_ -> _?VectorQ) ..], opts : OptionsPattern[] ] :=
    Block[{lsaObj},

      lsaObj =
          Fold[LSAMonBind,
            LSAMonUnit[],
            {
              LSAMonSetDocumentTermMatrix[SparseArray[Values[aDImageVecs]]],
              LSAMonApplyTermWeightFunctions["None", "None", "Cosine"],
              LSAMonExtractTopics["NumberOfTopics" -> 80, Method -> "SVD", "MaxSteps" -> 160, "MinNumberOfDocumentsPerTerm" -> 0, opts],
              LSAMonNormalizeMatrixProduct[Normalized -> Left]
            }];

      lsaObj
    ];
DimensionReductionWithLSA[___] := $Failed;

(*----------------------------------------------------------*)
(* GetImageBasis                                            *)
(*----------------------------------------------------------*)

Clear[GetImageBasis];
GetImageBasis[ lsaObj_LSAMon, aDImages : Association[(_ -> _?ImageQ)..]] :=
    Block[{H, lsBasis},
      H =
          Fold[LSAMonBind,
            lsaObj,
            {
              LSAMonNormalizeMatrixProduct[Normalized -> Right],
              LSAMonTakeH
            }];

      lsBasis = ImageAdjust[Image[Partition[#, ImageDimensions[aDImages[[1]]][[1]]]]] & /@ SparseArray[H];

      <| "H" -> H, "ImageBasis" -> lsBasis |>
    ];
GetImageBasis[___] := $Failed;

(*----------------------------------------------------------*)
(* RepresentImage                                           *)
(*----------------------------------------------------------*)

Clear[RepresentImage];
RepresentImage[lsaObj_LSAMon, imgExample_?ImageQ, img_?ImageQ] :=
    Block[{matImage, matRepresentation, lsCoeff},

      matImage =
          ToSSparseMatrix[
            SparseArray@List@ImageToVector[img, ImageDimensions[imgExample]],
            "RowNames" -> Automatic, "ColumnNames" -> Automatic];

      matRepresentation =
          Fold[
            LSAMonBind,
            lsaObj,
            {
              LSAMonRepresentByTopics[matImage],
              LSAMonTakeValue
            }];

      lsCoeff = Normal@SparseArray[matRepresentation[[1, All]]];
      lsCoeff
    ];
RepresentImage[___] := $Failed;


End[]; (* `Private` *)

EndPackage[]