(*
    Left Aligned Presentation Re-styling Mode Mathematica package

    BSD 3-Clause License

    Copyright (c) 2022, Anton Antonov
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice, this
      list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
    FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
    SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
    OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    Written by Anton Antonov,
    ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ,
    Windermere, Florida, USA.
*)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: LeftAlignedPresentationReStyling *)
(* :Context: LeftAlignedPresentationReStyling` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-02-21 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 12.1 *)
(* :Copyright: (c) 2022 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["LeftAlignedPresentationReStyling`"];
(* Exported symbols added here with SymbolName::usage *)
LeftAlignedPresentationMode::usage = "Restyle the notebook to use the left border aligned presentation cell styles.";

Begin["`Private`"];

nbReStyle =
    Notebook[{
      Cell[StyleData[StyleDefinitions -> "Default.nb"]],
      
      (* Original *)
      (*      Cell[StyleData["Input", "SlideShow"],*)
      (*        CellMargins->{{*)
      (*          0.135 FrontEnd`AbsoluteCurrentValue[{WindowSize, 1}], 0.01*)
      (*              FrontEnd`AbsoluteCurrentValue[{WindowSize, 1}]}, {8, 15}},*)
      (*        LinebreakAdjustments->{1, 2., 12., 1., 1.},*)
      (*        FontSize->20],*)
      Cell[StyleData["Text", "SlideShow"], CellMargins->{{66, 10}, {5, 8}}],
      Cell[StyleData["Item", "SlideShow"], CellMargins->{{81, 10}, {4, 8}}],
      Cell[StyleData["ItemParagraph", "SlideShow"], CellMargins->{{81, 10}, {4, 1}}],
      Cell[StyleData["Subitem", "SlideShow"], CellMargins->{{105, 12}, {4, 4}}],
      Cell[StyleData["Subsubitem", "SlideShow"], CellMargins->{{129, 12}, {4, 4}}],
      Cell[StyleData["SubsubitemParagraph", "SlideShow"], CellMargins->{{129, 12}, {4, 4}}],

      Cell[StyleData["ItemNumbered", "SlideShow"], CellMargins->{{81, 10}, {4, 8}}],
      Cell[StyleData["SubitemNumbered", "SlideShow"], CellMargins->{{105, 12}, {4, 4}}],
      Cell[StyleData["SubsubitemNumbered", "SlideShow"], CellMargins->{{129, 12}, {4, 4}}],

      Cell[StyleData["Section", "SlideShow"],  CellMargins->{{27, Inherited}, {8, 18}}],
      Cell[StyleData["Subsection", "SlideShow"], CellMargins->{{50.34765625, 3.}, {8., 20.}}],
      Cell[StyleData["Subsubsection", "SlideShow"], CellMargins->{{66, Inherited}, {8, 12}}],

      Cell[StyleData["Input", "SlideShow"], CellMargins->{{66, 10}, {5, 8}}],

      Cell[StyleData["Output", "SlideShow"], CellMargins->{{66, 10}, {5, 8}}],

      Cell[StyleData["Echo", "SlideShow"], CellMargins->{{66, 10}, {5, 8}}],

      Cell[StyleData["ExternalLanguage", "SlideShow"], CellMargins->{{66, 10}, {5, 8}}],

      Cell[StyleData["Code", "SlideShow"], CellMargins->{{66, 10}, {5, 8}}]
    },
      WindowSize -> {857, 887},
      WindowMargins -> {{373, Automatic}, {Automatic, 219}},
      FrontEndVersion -> "12.1 for Mac OS X x86 (64-bit) (June 19, 2020)",
      StyleDefinitions -> "PrivateStylesheetFormatting.nb"
    ];

Clear[LeftAlignedPresentationMode] ;
LeftAlignedPresentationMode[True] = LeftAlignedPresentationMode[];

LeftAlignedPresentationMode[] := LeftAlignedPresentationMode[EvaluationNotebook[]];

LeftAlignedPresentationMode[nb_NotebookObject, True] := LeftAlignedPresentationMode[nb];

LeftAlignedPresentationMode[nb_NotebookObject] :=
    Block[{},
      SetOptions[nb, StyleDefinitions -> BinaryDeserialize[BinarySerialize[nbReStyle]]]
    ];

LeftAlignedPresentationMode[ False] := SetOptions[EvaluationNotebook[], StyleDefinitions -> "Default.nb"];

LeftAlignedPresentationMode[nb_NotebookObject, False] := SetOptions[nb, StyleDefinitions -> "Default.nb"];

End[]; (* `Private` *)

EndPackage[]