(*
    Chow test statistic Mathematica package
    Copyright (C) 2019  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2019 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: ChowTestStatistic *)
(* :Context: ChowTestStatistic` *)
(* :Author: Anton Antonov *)
(* :Date: 2019-06-15 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: 12.0 *)
(* :Copyright: (c) 2019 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

    The [Chow test](https://en.wikipedia.org/wiki/Chow_test) is a test of whether the true coefficients
    in two linear regressions on different data sets are equal.

    In econometrics, Chow test is most commonly used in time series analysis to test for the presence
    of a [structural break](https://en.wikipedia.org/wiki/Structural_break).

    ## Data

    Here is data taken from [the explanation image](https://en.wikipedia.org/wiki/Chow_test#/media/File:Chowtest4.svg)
    of the referenced [Wikipedia article for Chow test](https://en.wikipedia.org/wiki/Chow_test).

        data = {{0.08`, 0.34`}, {0.16`, 0.55`}, {0.24`, 0.54`}, {0.32`,
        0.77`}, {0.4`, 0.77`}, {0.48`, 1.2`}, {0.56`, 0.57`}, {0.64`,
        1.3`}, {0.72`, 1.`}, {0.8`, 1.3`}, {0.88`, 1.2`}, {0.96`,
        0.88`}, {1.`, 1.2`}, {1.1`, 1.3`}, {1.2`, 1.3`}, {1.3`,
        1.4`}, {1.4`, 1.5`}, {1.4`, 1.5`}, {1.5`, 1.5`}, {1.6`,
        1.6`}, {1.7`, 1.1`}, {1.8`, 0.98`}, {1.8`, 1.1`}, {1.9`,
        1.4`}, {2.`, 1.3`}, {2.1`, 1.5`}, {2.2`, 1.3`}, {2.2`,
        1.3`}, {2.3`, 1.2`}, {2.4`, 1.1`}, {2.5`, 1.1`}, {2.6`,
        1.2`}, {2.6`, 1.4`}, {2.7`, 1.3`}, {2.8`, 1.6`}, {2.9`,
        1.5`}, {3.`, 1.4`}, {3.`, 1.8`}, {3.1`, 1.4`}, {3.2`,
        1.4`}, {3.3`, 1.4`}, {3.4`, 2.`}, {3.4`, 2.`}, {3.5`,
        1.5`}, {3.6`, 1.8`}, {3.7`, 2.1`}, {3.8`, 1.6`}, {3.8`,
        1.8`}, {3.9`, 1.9`}, {4.`, 2.1`}};
        ListPlot[data]


    The `ChowTestStatistic` function definition follows the formula given in
    [Wikipedia's Chow test entry](https://en.wikipedia.org/wiki/Chow_test).

    Here is a usage example:

        res = ChowTestStatistic[data, Min[data[[All, 1]]] + Accumulate[Differences[Sort[data[[All, 1]]]]], {1, x}, x];
        ListPlot[res, Filling -> Axis, PlotRange -> All]

    The example code above is equivalent to this one:

        res = {#, ChowTestStatistic[data, #, {1, x}, x]} & /@
           (Min[data[[All, 1]]] + Accumulate[Differences[Sort[data[[All, 1]]]]]);
        ListPlot[res, Filling -> Axis, PlotRange -> All]

*)

BeginPackage["ChowTestStatistic`"];
(* Exported symbols added here with SymbolName::usage *)

ChowTestStatistic::usage = "ChowTestStatistic[data : {{_?NumberQ, _?NumberQ} ..}, splitPoints : ({_?NumberQ..} | _?NumberQ), funcs_: {1, x}, var_: x] \
computes the Chow test statistic for identifying structural breaks in time series.\n\
ChowTestStatistic[data1, data2, funcs, var] computes the Chow test statistic for two datasets.";

Begin["`Private`"];

Clear[ChowTestStatistic];

ChowTestStatistic::empfuncs = "A non empty list of functions is expected.";
ChowTestStatistic::novar = "The specified variable is not a symbol.";
ChowTestStatistic::nofuncsvar = "The specified variable should be found in the functions list.";
ChowTestStatistic::empdata = "The split point `1` produced an empty split dataset.";
ChowTestStatistic::expargs = "The implemented signatures are:\n\
ChowTestStatistic[data : {{_?NumberQ, _?NumberQ} ..}, splitPoint_?NumberQ, funcs_List: {1, x}, var_: x],\n\
ChowTestStatistic[data : {{_?NumberQ, _?NumberQ} ..}, splitPoints : {_?NumberQ ..}, funcs_List: {1, x}, var_: x], and\n\
ChowTestStatistic[data1 : {{_?NumberQ, _?NumberQ} ..}, data2 : {{_?NumberQ, _?NumberQ} ..}, funcs_List: {1, x}, var_: x].";


ChowTestStatistic[data : {{_?NumberQ, _?NumberQ} ..}, splitPoint_?NumberQ, funcs_List : {1, x}, var_: x] :=
    Block[{res},

      res = ChowTestStatistic[ data, {splitPoint}, funcs, var];

      If[TrueQ[res === $Failed] || !FreeQ[res, $Failed],
        $Failed,
        res[[1,2]]
      ]
    ];


ChowTestStatistic[data : {{_?NumberQ, _?NumberQ} ..}, splitPoints : {_?NumberQ ..}, funcs_List : {1, x}, var_: x] :=
    Block[{data1, data2, S, S1, S2, k, fm, res},

      If[Length[funcs] == 0,
        Message[ChowTestStatistic::empfuncs];
        Return[$Failed]
      ];

      If[! Developer`SymbolQ[var],
        Message[ChowTestStatistic::novar];
        Return[$Failed]
      ];

      If[FreeQ[funcs, var],
        Message[ChowTestStatistic::nofuncsvar];
        Return[$Failed]
      ];

      k = Count[Developer`SymbolQ /@ funcs, True];

      res = Fit[data, funcs, var, "FitResiduals"];
      S = res.res;

      Map[
        Function[{sp},

          data1 = Select[data, #[[1]] < sp &];
          data2 = Select[data, #[[1]] >= sp &];

          If[ Length[data1] == 0 || Length[data2] == 0,

            Message[ChowTestStatistic::empdata, sp];
            $Failed,

            (* ELSE *)

            {S1, S2} =
                Map[
                  Function[{d},
                    res = Fit[d, funcs, var, "FitResiduals"];
                    res.res
                  ],
                  {data1, data2}];

            {sp, ((S - (S1 + S2))/ k)/((S1 + S2)/(Length[data1] + Length[data2] - 2 k)) }
          ]

        ],
        splitPoints
      ]

    ];


ChowTestStatistic[data1 : {{_?NumberQ, _?NumberQ} ..}, data2 : {{_?NumberQ, _?NumberQ} ..}, funcs_List : {1, x}, var_: x] :=
    Block[{data, S, S1, S2, k, fm, res},

      If[Length[funcs] == 0,
        Message[ChowTestStatistic::empfuncs];
        Return[$Failed]
      ];

      If[! Developer`SymbolQ[var],
        Message[ChowTestStatistic::novar];
        Return[$Failed]
      ];

      If[FreeQ[funcs, var],
        Message[ChowTestStatistic::nofuncsvar];
        Return[$Failed]
      ];

      k = Count[Developer`SymbolQ /@ funcs, True];

      {S, S1, S2} =
          Map[
            Function[{d},
              res = Fit[d, funcs, var, "FitResiduals"];
              res.res
            ],
            {Join[data1, data2], data1, data2}];

      ((S - (S1 + S2))/ k)/((S1 + S2)/(Length[data1] + Length[data2] - 2 k))

    ];

ChowTestStatistic[___] := Message[ChowTestStatistic::expargs];

End[]; (* `Private` *)

EndPackage[]