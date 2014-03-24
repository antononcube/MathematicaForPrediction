

(*
    Mosaic plot for data visualization implementation in Mathematica
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
    Mathematica is (C) Copyright 1988-2012 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 1.0 *)
(* 
  This package defines the function MosaicPlot that summarizes the conditional probabilities of co-occurrence of the categorical values in a list of records of the same length. (The list of records is assumed to be a full array and the columns to represent categorical values.) Note, that if a column is numerical but has a small number of different values it can be seen as categorical.

  Descriptions of the mosaic plots can be found in books about programming and statistics with R. See for example "R in Action" by Robert Kabacoff.

  MosaicPlot has options for adjusting the gap between the rectangles, the style of the labels, the rotation of the labels, and from which side to start the rectangle splitting. MosaicPlot also takes all the options of Graphics (because MosaicPlot is implemented with Graphics).

  The mosaic plot is made within the rectangle {{0,0},{1,1}}. Using the options PlotRange and Frame one make a frame that encompasses the rotated labels.

  TODO
  1. Pearson chi-squared correlation coloring.
  2. Better description of the functionalities.
*)

BeginPackage["MosaicPlot`"]

MosaicPlot::usage = "MosaicPlot[rarr] makes a mosaic plot that summarizes the conditional probabilities of categorical values co-occurrence in a list of records of the same length (a full array). MosaicPlot has options for adjusting the gap between the rectangles, the style of the labels, the rotation of the labels, and from which side to start the rectangle splitting. MosaicPlot also takes all the options of Graphics."

Begin["`Private`"]

Needs["TriesWithFrequencies`"]

Clear[TrieUniqueRecords]
TrieUniqueRecords[data_?ArrayQ] :=
  Block[{uniqCVals, zeroRecs},
   uniqCVals = Table[Union[data[[All, i]]], {i, Dimensions[data][[2]]}];
   zeroRecs = Flatten[Outer[List, Sequence @@ uniqCVals], Length[uniqCVals] - 1];
   TrieCreate[zeroRecs] /. {h_, p_?NumberQ} :> {h, 0}
  ];

Clear[TrieAddMissingValues]
TrieAddMissingValues[trie_, data_?ArrayQ] := TrieMerge[trie, TrieUniqueRecords[data]];

Clear[TrieSortNodes]
TrieSortNodes[trie_] :=
  If[Length[trie] == 1, trie,
   Join[{trie[[1]]}, TrieSortNodes /@ SortBy[Rest[trie], #[[1, 1]] &]]
  ];

Clear[TriePruneNumericalLevel]
TriePruneNumericalLevel[trie_, pruneLevel_Integer] := TriePruneNumericalLevel[trie, pruneLevel, 1];
TriePruneNumericalLevel[trie_, pruneLevel_Integer, level_Integer] :=
  Block[{t},
   Which[
    Length[trie] == 1 || pruneLevel < level, trie,
    pruneLevel == level && VectorQ[Rest[trie][[All, 1, 1]], NumberQ], {{trie[[1, 1]], Total[Rest[trie][[All, 1, 1]]]}},
    t = TriePruneNumericalLevel[#, pruneLevel, level + 1] & /@ Rest[trie];
    True, Join[{{trie[[1, 1]], Total[t[[All, 1, 2]]]}}, t]
   ]
  ];

Clear[RectanglePartition]
Options[RectanglePartition] = {"Gap" -> 0.01, "ZeroWidth" -> 0.001, "SortNodes" -> False};
RectanglePartition[trie_, 
   Rectangle[{x0_?NumberQ, y0_?NumberQ}, {x1_?NumberQ, y1_?NumberQ}], 
   axis : ("x" | "y"), opts : OptionsPattern[]] :=
  
  Block[{ps, aps, xs, ys, gap = OptionValue["Gap"],
    zwidth = OptionValue["ZeroWidth"], sortQ = OptionValue["SortNodes"]},
   If[TrueQ[sortQ],
    ps = #[[1, 2]] & /@ SortBy[Rest[trie], #[[1]] &],
    ps = #[[1, 2]] & /@ Rest[trie]
   ];

   aps = FoldList[Plus, 0, ps /. (0 -> zwidth)];

   If[axis == "x",
    Map[Rectangle[{#[[1]], y0}, {#[[2]], y1}] &, 
     MapIndexed[#1 + (#2[[1]] - 1) {gap, gap} &, 
      Partition[Rescale[aps, {0, If[aps[[-1]] > 1, aps[[-1]], 1]}, {x0, x1 - gap (Length[ps] - 1)}], 2, 1]]],
    Map[Rectangle[{x0, #[[1]]}, {x1, #[[2]]}] &, 
     MapIndexed[#1 + (#2[[1]] - 1) {gap, gap} &, 
      Partition[Rescale[aps, {0, If[aps[[-1]] > 1, aps[[-1]], 1]}, {y0, y1 - gap (Length[ps] - 1)}], 2, 1]]]
    ]
   ];

(* The original version of the function TrieMosaicRec gives much better idea of what it does:

Clear[TrieMosaicRec]
TrieMosaicRec[trie_, r_Rectangle, axis : ("x" | "y"), gap_?NumberQ, zwidth_?NumberQ] :=
  Block[{rs},
   If[Length[trie] == 1 || r[[2, 1]] - r[[1, 1]] <= gap || r[[2, 2]] - r[[1, 2]] <= gap, r,
    rs = RectanglePartition[trie, r, axis, "Gap" -> gap, "ZeroWidth" -> zwidth];
    MapThread[TrieMosaicRec[#1, #2, axis /. {"x" -> "y", "y" -> "x"}, gap/2, zwidth] &, {Rest[trie], rs}, 1]
   ]
  ];

*)


Clear[MakeTooltipTable]
MakeTooltipTable[triePath_] :=
  Block[{t},
   t =
    DeleteCases[
     Join @@ Table[{triePath[[1 ;; i, 1]], triePath[[i + 1 ;; j, 1]],
        Apply[Times, triePath[[i ;; j, 2]]]/triePath[[i, 2]]},
       {j, 2, Length[triePath]}, {i, 1, j - 1}],
     {}, 3];
   
   t = Map[{If[Length[#[[1]]] == 0, "", 
        DisplayForm[
         FormBox[RowBox[
           Riffle[If[StringQ[#], "\"" <> # <> "\"", #] & /@ #[[1]], 
            "\[Intersection]"]], TraditionalForm]]], 
       DisplayForm[
        FormBox[RowBox[
          Riffle[If[StringQ[#], "\"" <> # <> "\"", #] & /@ #[[2]], 
           "\[Intersection]"]], TraditionalForm]], #[[3]]} &, t];

   Grid[Prepend[t, Style[#, Blue, FontFamily -> "Times"] & /@ {"condition", "event", "probability"}], Alignment -> Left, 
    Dividers -> {None, {False, True, False}}]
  ];

SIDEChangeRules = {Left -> Top, Top -> Right, Right -> Bottom, Bottom -> Left};
SIDEToCoordinateRules = {Left -> 0, Right -> 1, Top -> 1, Bottom -> 0};

Clear[TrieMosaicRec]
TrieMosaicRec[trie_, triePath_, posPath_, r_Rectangle, 
   axis : ("x" | "y"), side : (Top | Bottom | Left | Right), 
   gap_?NumberQ, gapFactor_?NumberQ, 
   zwidth_?NumberQ, {xLabelRotation_, yLabelRotation_}, labelStyle_, 
   addTooltipQ_, colors_, colorInds_] :=
  
  Block[{rs, t, c = side /. SIDEToCoordinateRules},
   If[Length[trie] == 1 || r[[2, 1]] - r[[1, 1]] <= gap || r[[2, 2]] - r[[1, 2]] <= gap, 
    t = If[TrueQ[addTooltipQ], Tooltip[r, MakeTooltipTable[Append[triePath, trie[[1]]]]], r];
    If[Length[colorInds] == 0, t,
     {Which[
       Max[Abs[colorInds]] > Length[posPath], GrayLevel[0.7],
       Length[colorInds] == 1 && Length[posPath] == 1 && ! ListQ[colors[[1]]], colors[[1]],
       Length[colorInds] == 1 && ! ListQ[colors[[1]]], Blend[{White, colors[[1]]}, posPath[[colorInds[[1]]]]],
       Length[colorInds] == 1, 
       Blend[colors[[1]], posPath[[colorInds[[1]]]]],
       True, Blend[colors, posPath[[colorInds]]]
       ], t}
    ],
    (*ELSE*)
    rs = RectanglePartition[trie, r, axis, "Gap" -> gap, "ZeroWidth" -> zwidth];
    If[axis == "x", t = Select[rs, #[[1, 2]] == c || #[[2, 2]] == c &];
     If[Length[t] == Length[rs], 
      AppendTo[LABELS, 
       MapThread[Text[Style[#1, labelStyle], {Mean[{#2[[1, 1]], #2[[2, 1]]}], c}, If[side === Top, -{0, 2}, {0, 2}], 
          xLabelRotation] &, {Rest[trie][[All, 1, 1]], rs}]]],
     (*ELSE*)
     t = Select[rs, #[[1, 1]] == c || #[[2, 1]] == c &];
     If[Length[t] == Length[rs], 
      AppendTo[LABELS, 
       MapThread[Text[Style[#1, labelStyle], {c, Mean[{#2[[1, 2]], #2[[2, 2]]}]}, If[side === Left, -{0, 2}, {0, 2}], 
          yLabelRotation] &, {Rest[trie][[All, 1, 1]], rs}]]]
    ];
    
    MapThread[
     TrieMosaicRec[#1, Append[triePath, trie[[1]]], Append[posPath, #3], #2, 
       axis /. {"x" -> "y", "y" -> "x"}, 
       side /. SIDEChangeRules, gap*gapFactor, gapFactor, 
       zwidth, {xLabelRotation, yLabelRotation}, labelStyle, 
       addTooltipQ, colors, colorInds] &, {Rest[trie], rs, Range[Length[rs]]/Length[rs]}, 1]
   ]
  ];

MosaicPlot::nargs = "MosaicPlot takes as an argument a full array (that is list of records).";
MosaicPlot::ncno = "The value of the option \"ColumnNamesOffset\" should be a number.";
MosaicPlot::npnum = "The value of the option `1` should be a positive number.";
MosaicPlot::nfax = "The value of the option \"FirstAxis\" should be either \"x\" or \"y\".";
MosaicPlot::nlr = "The value of the option \"LabelRotation\" should be a pair of numbers or two pairs of numbers.";
MosaicPlot::ncr = "The value of the option ColorRules should be a list of rules of the form columnIndex->color. If coloring for only one column index is specified its rule can be of the form colorIndex->{color1,color2,...} .";

Clear[MosaicPlot]
Options[MosaicPlot] = 
  Join[{"ColumnNames" -> None, "ColumnNamesOffset"->0.05, "Gap" -> 0.02, "GapFactor" -> 0.5,  
    "ZeroProbability" -> 0.001, "FirstAxis" -> "y", 
    "LabelRotation" -> {{1, 0}, {0, 1}}, "LabelStyle" -> {}, 
    "ExpandLastColumn" -> False, "Tooltips"->True, ColorRules -> Automatic}, Options[Graphics]];
MosaicPlot[dataRecords_, opts : OptionsPattern[]] :=
  Block[{trie, rs,
   gap = OptionValue[MosaicPlot, "Gap"],
   gapFactor = OptionValue[MosaicPlot, "GapFactor"],
   zwidth = OptionValue[MosaicPlot, "ZeroProbability"],
   firstAxis = OptionValue[MosaicPlot, "FirstAxis"],
   labelRotation = OptionValue[MosaicPlot, "LabelRotation"],
   labelStyle = OptionValue[MosaicPlot, "LabelStyle"],
   columnNames = OptionValue[MosaicPlot, "ColumnNames"],
   frameLabelOffset = OptionValue[MosaicPlot, "ColumnNamesOffset"],
   expandLastColumnQ = TrueQ[OptionValue[MosaicPlot, "ExpandLastColumn"]],
   addTooltipQ = TrueQ[OptionValue[MosaicPlot, "Tooltips"]],
   colorRules = OptionValue[MosaicPlot, ColorRules],
   LABELS = {}, frameLabels, frameLabelCoords, frameLabelRotation, 
   colors, colorInds, t, nvals},
   
   If[! (ArrayQ[dataRecords] && Length[Dimensions[dataRecords]]==2),
    Message[MosaicPlot::nargs];
    Return[{}]
   ];
   
   If[! TrueQ[ NumberQ[gap] && gap > 0 ],
    Message[MosaicPlot::npnum, "\"Gap\""];
    gap = 0.02;
   ];

   If[! TrueQ[ NumberQ[gapFactor] && gapFactor > 0 ],
    Message[MosaicPlot::npnum, "\"GapFactor\""];
    gapFactor = 0.5;
   ];
      
   If[! TrueQ[ NumberQ[zwidth] && zwidth > 0 ],
    Message[MosaicPlot::npnum, "\"ZeroProbability\""];
    zwidth = 0.001;
   ];
   
   If[! (TrueQ[colorRules === None] || TrueQ[colorRules === Automatic] ||
      MatchQ[colorRules, {_Integer -> {(_RGBColor | _GrayLevel) ..}} | {(_Integer -> (_RGBColor | _GrayLevel)) | (Rule[Blank[], (_RGBColor | _GrayLevel)]) ...}]),
    Message[MosaicPlot::"ncr"]
   ];

   Which[
    TrueQ[firstAxis == "x" || firstAxis == "X" || firstAxis == "Top"], firstAxis = "x",
    TrueQ[firstAxis == "y" || firstAxis == "Y" || firstAxis == "Left"], firstAxis = "y",
    True,
    Message[MosaicPlot::nfax];
    firstAxis = "y"
   ];
   
   If[VectorQ[labelRotation, NumberQ] && Length[labelRotation] == 2, labelRotation = {labelRotation, labelRotation}];
   If[TrueQ[labelRotation === None], labelRotation = {{1, 0}, {1, 0}}];
   If[! (MatrixQ[labelRotation, NumberQ] && Dimensions[labelRotation] == {2, 2}),
    Message[MosaicPlot::nlr];
    labelRotation = {{1, 0}, {0, 1}};
   ];
   
   If[TrueQ[labelStyle === None], labelStyle = {}];
   
   If[! NumberQ[frameLabelOffset],
    Message[MosaicPlot::ncno];
    frameLabelOffset = 0.05;
   ];
   
   If[Length[columnNames] == 0,
    frameLabels = {},
    (*ELSE*)
    frameLabelCoords = {{-frameLabelOffset, 0.5}, {0.5, 1 + frameLabelOffset}, {1 + frameLabelOffset, 0.5}, {0.5, -frameLabelOffset}};
    frameLabelRotation = {{0, 1}, {1, 0}, {0, -1}, {1, 0}};
    If[firstAxis == "x",
      frameLabelCoords = RotateLeft[frameLabelCoords, 1];
      frameLabelRotation = RotateLeft[frameLabelRotation, 1];
    ];
    frameLabels = MapThread[Text[#1, #2, {0, 0}, #3] &, 
      {If[Length[columnNames] >= 4, columnNames[[1 ;; 4]], Join[columnNames, Table["", {4 - Length[columnNames]}]]], frameLabelCoords, frameLabelRotation}];
    frameLabels = Select[frameLabels, !TrueQ[#[[1]]==""]&];
   ];
   
   If[TrueQ[colorRules === None], colorRules = {}];
   If[! TrueQ[colorRules === Automatic],
    colorRules = 
     Map[If[NumberQ[#[[1]]] && #[[1]] < 1, (Dimensions[dataRecords][[2]] + #[[1]] + 1) -> #[[2]], #] &, colorRules];
    colors = Map[{#, # /. colorRules} &, Range[1, Dimensions[dataRecords][[2]]]];
    colors = DeleteCases[colors, {_Integer, _Integer}];
    If[Length[colors] == 0, colorInds = {}, {colorInds, colors} = Transpose[colors]]
  ];

   trie = TrieCreate[dataRecords];

   If[expandLastColumnQ,
    trie = TriePruneNumericalLevel[trie, Dimensions[dataRecords][[2]]];
    trie = TrieNodeProbabilities[trie];
    trie = TrieAddMissingValues[trie, dataRecords[[All, 1 ;; Dimensions[dataRecords][[2]] - 1]]],
    (* ELSE *)
    trie = TrieNodeProbabilities[trie];
    trie = TrieAddMissingValues[trie, dataRecords]
   ];

   (* If the color rules are Automatic we pick the column with the largest number of unique values *)
   If[TrueQ[colorRules === Automatic],
    nvals = {}; t = trie;
    While[Length[Rest[t]] > 0, AppendTo[nvals, Length[Rest[t]]]; t = t[[2]]];
    (*colors={{Lighter[Blue],Lighter[Red]}};*) 
    colors = {ColorData[7, "ColorList"]};
    colorInds = Take[Flatten[Position[nvals, Max[nvals]]], 1]
   ];

   trie = TrieSortNodes[trie];
   rs = TrieMosaicRec[trie, {}, {}, Rectangle[{0, 0}, {1, 1}], firstAxis, firstAxis /. {"x" -> Top, "y" -> Left}, gap, gapFactor, zwidth, labelRotation, labelStyle, addTooltipQ, colors, colorInds];

   Graphics[{rs, Black, LABELS, frameLabels}, 
    DeleteCases[{opts}, ("Gap" | "GapFactor" | "ZeroProbability" | "FirstAxis" | "LabelRotation" | "ExpandLastColumn" | "ColumnNames" | "ColumnNamesOffset" | "Tooltips" | ColorRules) -> _]]
   
  ];
MosaicPlot[___] := Block[{}, Message[MosaicPlot::nargs]; {}];

End[]

EndPackage[]