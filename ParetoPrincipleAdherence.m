(*
    Pareto principle adherence Mathematica package
    Copyright (C) 2019  Anton Antonov

    BSD 3-Clause License

    Copyright (c) 2019, Anton Antonov
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
    antononcube @ gmail . com,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(*
    # In brief

    The adherence of a data vector to the Pareto principle can be easily verified with the plots of ParetoPrinciplePlot.

    - ParetoPrinciplePlot[vec] plots the normalized cumulative sum of the reverse-sorted vec.

    - The argument data in ParetoPrinciplePlot[data, opts]

      - can be a numerical list,
      - a list of numerical lists, or
      - a list of numerical lists wrapped with tooltips and callouts.

    - For a given numerical vector vec the Pareto principle plot data is computed with the formula:

         Accumulate[ReverseSort[vec]]/Total[vec].

    - ParetoPrinciplePlot takes all options of ListPlot.

    - The option "ParetoGridLines" can be used to define Pareto principle specific grid lines.

    - The option "Tooltip" can be used to specify the use of automatic Tooltip wrappers.

    -  Here is the options table:

       "ParetoGridLines"  Automatic    Pareto-specific grid lines.
       "Tooltip"          True         Should automatic Tooltip wrappers be placed?


    Anton Antonov
    Windermere, FL, USA
    2019-12-04
*)


BeginPackage["ParetoPrincipleAdherence`"];

ParetoPrinciplePlot::usage = "ParetoPrinciplePlot[data, opts] makes Pareto principle adherence plots. \
The argument data can be a numerical list, a list of numerical lists, \
or a list of numerical lists wrapped with tooltips and callouts. \
Takes all options of ListPlot.";

Begin["`Private`"];

(***********************************************************)
(* Pareto help functions                                   *)
(***********************************************************)

Clear[ParetoDataSpecQ];
ParetoDataSpecQ[data_] :=
    MatchQ[data, {((Tooltip | Callout)[(Tooltip | Callout)[{_?NumericQ ...}, __], __] | (Tooltip | Callout)[{_?NumericQ ...}, __] | {_?NumericQ ...}) ..}] || MatchQ[data, {{_?NumericQ ...} ..}];

Clear[ParetoGetData];
ParetoGetData[data : {(Tooltip | Callout)[{_?NumericQ ...}, __] ..}] := data[[All, 1]];

ParetoGetData[data : {((Tooltip | Callout)[(Tooltip | Callout)[{_?NumericQ ...}, __], __] | (Tooltip | Callout)[{_?NumericQ ..}, __] | {_?NumericQ ...}) ..}] :=
    Cases[data, {_?NumericQ ...}, 3];

ParetoGetData[data : {{_?NumericQ ...} ..}] := data;


(***********************************************************)
(* ParetoPrinciplePlot                                     *)
(***********************************************************)

Clear[ParetoPrinciplePlot];

ParetoPrinciplePlot::args = "The first argument is expected to be a numerical vector or a list of numerical vectors.";

ParetoPrinciplePlot::pgl = "The value of the option \"ParetoGridLines\" is expected to be Automatic or \
a list of two numerical vectors, each with numbers between 0 and 1.";

Options[ParetoPrinciplePlot] = Join[{"Tooltip" -> True, "ParetoGridLines" -> Automatic}, Options[ListPlot]];

ParetoPrinciplePlot[dataVec : {_?NumericQ ...}, opts : OptionsPattern[]] :=
    ParetoPrinciplePlot[{dataVec}, opts];

ParetoPrinciplePlot[dataVecs : {{_?NumericQ ...} ..},
  opts : OptionsPattern[]] :=
    Block[{tooltipQ, t},

      tooltipQ = TrueQ[OptionValue[ParetoPrinciplePlot, "Tooltip"]];
      t = Map[Accumulate[#] / Total[#] &, ReverseSort /@ dataVecs];

      If[tooltipQ,
        t = MapThread[Tooltip, {t, Range[Length[dataVecs]]}]
      ];

      ParetoPrinciplePlotFrame[t, FilterRules[{opts}, Options[ParetoPrinciplePlotFrame]]]
    ];

ParetoPrinciplePlot[dataVecs : {(Tooltip | Callout)[{_?NumericQ ...}, __] ..},
  opts : OptionsPattern[]] :=
    Block[{t},
      t = Map[#[[0]][(Accumulate[#] / Total[#] &)[ReverseSort[#[[1]]]], Sequence @@ Rest[#]] &, dataVecs];
      ParetoPrinciplePlotFrame[t, FilterRules[{opts}, Options[ParetoPrinciplePlotFrame]]]
    ];

ParetoPrinciplePlot[
  dataVecs : {((Tooltip | Callout)[(Tooltip | Callout)[{_?NumericQ ...}, __], __] | (Tooltip | Callout)[{_?NumericQ ...}, __] | {_?NumericQ ...}) ..},
  opts : OptionsPattern[]] :=
    Block[{t},
      t = dataVecs /.
          x : {_?NumberQ ..} :> (Accumulate[#] / Total[#] &)[ReverseSort[x]];
      ParetoPrinciplePlotFrame[t,
        FilterRules[{opts}, Options[ParetoPrinciplePlotFrame]]]
    ];

ParetoPrinciplePlot[___] :=
    Block[{},
      Message[ParetoPrinciplePlot::args];
      $Failed
    ];


(***********************************************************)
(* ParetoPrinciplePlotFrame                                *)
(***********************************************************)

Clear[ParetoPrinciplePlotFrame];

Options[ParetoPrinciplePlotFrame] = Join[{"MaxParetoFraction" -> 0.5, "ParetoGridLines" -> Automatic}, Options[ListPlot]];

ParetoPrinciplePlotFrame[t_?ParetoDataSpecQ, opts : OptionsPattern[]] :=
    Block[{data = ParetoGetData[t], maxLen, paretoGridSpec, gridSpec},

      paretoGridSpec = OptionValue[ParetoPrinciplePlotFrame, "ParetoGridLines"];

      If[TrueQ[paretoGridSpec === Automatic], paretoGridSpec = {Range[0, 0.5, 0.1], {0.8}}];
      If[MatchQ[paretoGridSpec, {Automatic, _}], paretoGridSpec = {Range[0, 1, 0.1], paretoGridSpec[[2]]}];
      If[MatchQ[paretoGridSpec, {_, Automatic}], paretoGridSpec = {paretoGridSpec[[1]], Range[0, 1, 0.1]}];

      If[TrueQ[paretoGridSpec === None], paretoGridSpec = {{}, {}}];
      If[MatchQ[paretoGridSpec, {None, _}], paretoGridSpec = {{}, paretoGridSpec[[2]]}];
      If[MatchQ[paretoGridSpec, {_, None}], paretoGridSpec = {paretoGridSpec[[1]], {}}];

      Which[
        ! MatchQ[paretoGridSpec, {{_?NumericQ ...}, {_?NumericQ ...}}],
        Message[ParetoPrinciplePlot::pgl];
        paretoGridSpec = {Range[0.1, 0.5, 0.1], {0.8}},

        ! (VectorQ[paretoGridSpec[[1]], 0 <= # <= 1 &] && VectorQ[paretoGridSpec[[2]], 0 <= # <= 1 &]),
        Message[ParetoPrinciplePlot::pgl];
        paretoGridSpec = Map[Sort[Select[#, 0 <= # <= 1 &]] &, paretoGridSpec],

        True,
        paretoGridSpec = Map[Sort[Select[#, 0 <= # <= 1 &]] &, paretoGridSpec];
      ];

      maxLen = Max[Length /@ data];
      gridSpec = {maxLen * paretoGridSpec[[1]], paretoGridSpec[[2]]};

      ListPlot[t,
        FilterRules[{opts}, Options[ListPlot]],
        PlotRange -> All,
        GridLines -> gridSpec,
        Frame -> True,
        FrameTicks -> {{Automatic, Automatic}, {Automatic, Table[{maxLen * c, ToString[Round[100 c]] <> "%"}, {c, paretoGridSpec[[1]]}]}}]
    ];

End[]; (* `Private` *)

EndPackage[]