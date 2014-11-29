(* ::Package:: *)

(* ::Title:: *)
(*Simple Time Series Conversational Engine*)


(* ::Subsubtitle:: *)
(*Anton Antonov*)
(*MathematicaForPrediction blog at WordPress*)
(*MathematicaForPrediction project at GitHub*)
(*November 2014*)


(* ::Section:: *)
(*Copyright notice*)


(*
    Simple Time Series Conversational Engine Mathematica package
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



(* ::Section:: *)
(*Preliminary code*)


(* 
In order to run this package the packages FunctionalParsers.m and QuantileRegression.m has to be loaded. 
These packages are provided by the project MathematicaForPrediction at GitHub. 
*)


Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/FunctionalParsers.m"]
Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/QuantileRegression.m"]


(* ::Section:: *)
(*Grammar*)


timeSeriesCode = timeSeriesEBNFCode = "
<preamble> = 'find' | 'compute' | 'calculate' | 'show' ;
<weather-spec-straight> = ( ( 'temperature' | 'pressure' | 'wind' , 'speed' ) \[LeftTriangle] ( 'of' | 'for' ) ) , <city-spec> <@ TSWeatherSpec[#]& ;
<weather-spec-reverse> = [ 'the' ] \[RightTriangle] <city-spec> , ( 'temperature' | 'pressure' | 'wind' , 'speed' ) <@ TSWeatherSpec[Reverse[#]]& ;
<weather-spec> = <weather-spec-straight> | <weather-spec-reverse> ;
<city-spec> = '_LetterString' | '_LetterString' , [ ',' ] , '_LetterString' , [ [ ',' ] , '_LetterString' ] <@ TSCitySpec[Flatten[{#}]]& ;
<company-spec> = '_String' <@ TSCompanySpec ;
<finance-spec-straight> = ( [ 'the' ] \[RightTriangle] ( [ 'stock' ] \[RightTriangle] 'price' | 'trade' \[RightTriangle] 'volume' ) \[LeftTriangle] [ 'of' | 'for' ] ) , <company-spec> <@ TSFinancialData ;
<finance-spec-reverse> =  [ 'the' ] \[RightTriangle] <company-spec> , ( [ 'stock' ] \[RightTriangle] 'price' | 'trade' \[RightTriangle] 'volume' ) <@ TSFinancialData[Reverse[#]]& ;
<finance-spec> = <finance-spec-straight> | <finance-spec-reverse> ; 
<past-data-spec> = [ 'the' ] \[RightTriangle] 'last' , [ 'loaded' ] , ( 'data' | 'file' ) <@ TSPastData[Flatten[{#}]]& ;
<data-spec> = <weather-spec> | <finance-spec> | <past-data-spec> ;
<regression-quantile-bsplines> = [ ( '1' | 'one' | 'a' ) ] , 'regression' , 'quantile' | 'quantile' , 'regression' <@ TSBSplineQRegression[1]& ;
<regression-quantiles-bsplines> = [ 'Range[1,40]' ] \[LeftTriangle] ( 'regression' , 'quantiles' ) <@ TSBSplineQRegression[#]& ;
<regression-quantiles> = <regression-quantile-bsplines> |  <regression-quantiles-bsplines> ;
<outliers> = [ 'the' ] \[RightTriangle] [ ( 'top' | 'bottom' | 'largest' | 'smallest' | 'all' ) ] , 'outliers' <@ TSOutliers[Flatten[#]]& ;
<least-squares> = ( 'least' , 'squares' , [ 'fit' ] , [ 'with' | 'of' ] ) \[RightTriangle] '_String' <@ TSLeastSquaresFit[#]& ;
<operation-spec> = <regression-quantiles> | <least-squares> | <outliers> ;
<operate-command> = [ <preamble> ] \[RightTriangle] <operation-spec> <@ TSOperateCommand[#]& ;
<operate-on-data-command> = <operate-command> , ( 'for' | 'on' | 'in'  | 'over' | 'of' ) \[RightTriangle] <data-spec> <@ TSOperateOnDataCommand[#]& ;
<load-file-command> = ( 'load' , [ 'data' ] , 'file'  ) \[RightTriangle] ( '_String' ) <@ TSLoadFile[#]& ;
<load-data-command> = ( 'load' , [ 'the' ] , [ 'data' ] ) \[RightTriangle] <data-spec> <@ TSLoadData[#]& ;
<start-over> = 'start' , 'over' | 'clear' <@ TSStartOver[Flatten[{#}]]& ;
<clear-graphics> = 'clear' , ( 'plots' | 'plots' | 'graphics' ) <@ TSClearGraphics ;
<what-operations> = 'what' , ( ( 'operations' , 'are'  |  [ 'are' ] , [ 'the' ] , 'operations' ) , [ 'implemented' | 'in' ] ) | [ 'what' ] , ( 'operation' | 'operations' ) , ( 'I' , 'can' | 'to' ) , ( 'use' | 'do' ) <@ TSWhatOperations[Flatten[{#}]]& ;
<help-all> = 'help' | [ 'all' ] , 'commands' <@ TSHelp[Flatten[{#}]]& ;
<help> = <help-all> | <what-operations> ;
<plot-joined> = [ 'plot' | 'plots' ] , 'joined' | 'Joined' , '->' , 'True' | 'Joined->True' <@ TSPlotJoined ;
<plot-not-joined> = [ 'plot' | 'plots' ] ,  ( 'not' | 'non' ) , 'joined' | 'Joined' , '->' , 'False' | 'Joined->False' <@ TSPlotNotJoined ;
<plot-data> = 'plot' , 'data' <@ TSPlotData ;
<plot-command> = <plot-data> | <plot-joined> | <plot-not-joined> ;
<ts-command> = <load-data-command> | <load-file-command> | <operate-command> | <operate-on-data-command> | <clear-graphics> | <start-over> | <help> | <plot-command> ;
";


(* ::Section:: *)
(*Parser generation*)


tokens = ToTokens[timeSeriesCode];
res = GenerateParsersFromEBNF[tokens];
Print["Leaf count of the parsers genereated for the EBNF grammar: ", res // LeafCount]


(* ::Section:: *)
(*Interpreters*)


Clear["TS*"]


TSLoadFileInterpreter[parsed_String] :=
    Block[{t, cn, mess, fname = parsed},
      If[StringMatchQ[fname, ("'" | "\"") ~~ __ ~~ ("'" | "\"")],
        fname = StringTake[fname, {2, -2}]
      ];
      (* parsed is assumed to be a file name of say a CSV file, each line of which is a pair of numbers  *)
      t = ReadList[fname, {Word, Word}, WordSeparators -> {",", " ", "\t", "\n"}];
      If[MatrixQ[t],
        If[!NumberQ[t[[1, 1]]] && StringMatchQ[t[[1, 1]], WordCharacter..] && !NumberQ[t[[1, 2]]] && StringMatchQ[t[[1, 2]], WordCharacter..],
          cn = First[t];t = Rest[t],
          cn = ToString /@ Range[1, Length[t[[1]]]];
        ];
        mess = "DataLoaded";
        TSMESSAGE = "Data file loaded.";
        TSDATA = ToExpression[t];
        ,
        mess = "DataNotLoaded";
        TSMESSAGE = "Data file NOT loaded.";
        cn = {};TSDATA = {}
      ];
      mess
    ];


Clear[TSMakeProperty];
TSMakeProperty[s : {_String..}] := StringJoin[Map[TSMakeProperty, s]];
TSMakeProperty[s_String] :=
    If[StringLength[s] > 0, ToUpperCase[StringTake[s, 1]] <> ToLowerCase[StringTake[s, {2, -1}]], s];


TSLoadDataInterpreter[parsed_] :=
    Block[{citySpec, companySpec, prop, pos, t, messPart = ""},
    (* parsed is assumed to be weather data specification *)
      prop = TSMakeProperty[parsed[[1, 1]]];
      Which[
        MemberQ[{"Price", "Volume"}, prop],
        companySpec = Cases[parsed, TSCompanySpec[x__] :> x, \[Infinity]][[1]];
        If[Length[companySpec] == 1, companySpec = First[companySpec]];t = FinancialData[companySpec, prop, {DatePlus[Take[Date[], 3], {-365, "Day"}], Take[Date[], 3]}];
        messPart = "Financial",
        True,
        citySpec = Cases[parsed, TSCitySpec[x__] :> x, \[Infinity]][[1]];
        If[Length[citySpec] == 1, citySpec = First[citySpec]];
        t = WeatherData[citySpec, prop, {DatePlus[Take[Date[], 3], {-60, "Day"}], Take[Date[], 3]}];
        t = t["Path"] /. Quantity[x_, _] :> x;
        messPart = "Weather"
      ];
      pos = Position[t[[All, 2]], Missing["NotAvailable"]];
      If[Length[pos] > 0,
        t[[All, 2]] = Fold[ReplacePart[#1, #2 -> Mean[#1[[{#2 - 1, #2 + 1}]]]]&, t[[All, 2]], Flatten[pos]]
      ];
      t[[All, 1]] = AbsoluteTime /@ t[[All, 1]];
      If[MatrixQ[t],
        TSDATA = t;
        TSGRAPHICS = None;
        TSMESSAGE = messPart <> " data loaded.";
        "DataLoaded",
      (*ELSE*)
        TSMESSAGE = messPart <> " data NOT loaded.";
        "DataNotLoaded"
      ]
    ];


Clear[TSFindOutliers]
TSFindOutliers[type_String, dataArg_?MatrixQ, nsplines_Integer : 5] :=
    Block[{tfunc = None, bfunc = None, outliers = {}, data},
      data = N@Select[dataArg, VectorQ[#, NumberQ]&];
      If[
        type == "top" || type == "largest" || type == "all",
        tfunc = QuantileRegression[data, nsplines, {.98}][[1]];
        outliers = Join[outliers, Select[data, tfunc[#[[1]]] <= #[[2]]&]]
      ];
      If[
        type == "bottom" || type == "smallest" || type == "all",
        bfunc = QuantileRegression[data, nsplines, {.02}][[1]];
        outliers = Join[outliers, Select[data, bfunc[#[[1]]] >= #[[2]]&]]
      ];
      {outliers, tfunc, bfunc}
    ];


Clear[GetVariableName]
GetVariableName[fexpr_] :=
    Block[{},
      ToExpression[Flatten[Cases[fexpr, s_Symbol /; !NumericQ[s], \[Infinity]]][[1]]]
    ];


TSOperateOnDataCommandInterpreter[parsed_] :=
    Block[{op, dataSpec, data, n, res, mess = "ok", op1},
      op = Cases[parsed, TSOperateCommand[x___] :> x, \[Infinity]][[1]];
      Print["TSOperateOnDataCommandInterpreter:: parsed=", parsed];
      Print["TSOperateOnDataCommandInterpreter:: op=", op];
      dataSpec = parsed[[2]];
      Which[
        MatchQ[dataSpec, TSPastData[___]],
        data = TSDATA,
        MatchQ[dataSpec, TSWeatherSpec[___]],
        TSLoadDataInterpreter[dataSpec];
        data = TSDATA,
        True,
        TSRESULT = {};
        Return["UnknownDataSpec"]
      ];
      If[!MatrixQ[data],
        TSRESULT = {};
        TSMESSAGE = "The data should be a numerical matrix.";
        Return["WrongData"]
      ];
      data = N@Select[data, VectorQ[#, NumberQ]&];
      If[Length[data] == 0 || !MatrixQ[data, NumberQ],
        TSRESULT = {};
        TSMESSAGE = "The data should be a numerical matrix.";
        Return["WrongData"]
      ];
      Which[
        MatchQ[op, TSBSplineQRegression[___]],
        n = op[[1]] /. {x_ /; NumberQ[ToExpression[x]] :> ToExpression[x], _ -> 5};
        res = QuantileRegression[data, 10, Rescale[Range[0, n + 1], {0, n + 1}, {0, 1}][[2 ;; -2]], InterpolationOrder -> 2, Method -> {LinearProgramming, Method -> "CLP"}];
        TSMESSAGE = "Found regression quantiles.",
        MatchQ[op, TSLeastSquaresFit[___]],
        op1 = ToExpression[StringReplace[op[[1]], {"table[" -> "Table[", "log[" -> "Log[", "sin[" -> "Sin[", "cos[" -> "Cos[", "sqrt[" -> "Sqrt["}]];
        res = Fit[data, op1, GetVariableName[op1]];
        If[Head[res] === Fit,
          res = None;
          TSMESSAGE = "Did NOT fit least squares.",
          TSMESSAGE = "Fitted least squares."
        ],
        MatchQ[op, TSOutliers[___]],
        n = DeleteCases[op[[1]], "outliers"];
        If[Length[n] == 0, n = {"all"}];
        res = TSFindOutliers[n[[1]], data, 7];
        TSMESSAGE = "Found outliers.",
        True,
        TSMESSAGE = "Not clear what to do for " <> ToString[op];
        Return["UnknownOperation"]
      ];
      TSRESULT = Head[op][res];
      "OperationOnDataCompleted"
    ];


TSOperateCommandInterpreter[parsed_] :=
    Block[{},
      Print["TSOperateCommandInterpreter::", parsed];
      TSOperateOnDataCommandInterpreter[{TSOperateCommand[parsed], TSPastData[{"last", "loaded", "data"}]}]
    ];


TSStartOverInterpreter[parsed_] :=
    Block[{},
      TSDATA = {};
      TSRESULT = {};
      TSGRAPHICS = None;
      TSPLOTJOINED = False;
      TSMESSAGE = "Cleaned data and graphics.";
      "StartOver"
    ];


TSWhatOperationsInerpreter[parsed_] :=
    Block[{},
      TSRESULT = "The operations are:\n(all|top|bottom|largest|smallest) outliers,\nfind quantile(s),\nfind least squares fit <Mathematica-expression>.";
      TSMESSAGE = "See note.";
      "Note"
    ];


TSHelpInterpreter[parsed_] :=
    Block[{},
      TSWhatOperationsInerpreter[parsed];
      TSRESULT = "The data commands are:\nload data file <file-name>,\nload data (temperature|pressure|wind speed) (of|for) <city-spec>\nload data (company price|trade volume) (of|for) <company-spec>\nplot joined|plot not joined\nplot data\nclear graphics\nstart over|clear." <> "\n" <> TSRESULT;
      TSMESSAGE = "See note.";
      "Note"
    ];


TSClearGraphicsInterpreter[parsed_] :=
    Block[{},
      TSGRAPHICS = None;
      TSMESSAGE = "Graphics cleared.";
      "None"
    ];


TSPlotJoinedInterpreter[parsed_] :=
    Block[{},
      TSPLOTJOINED = True;
      TSGRAPHICS = None;
      TSMESSAGE = "Using joined plots from now on.";
      "None"
    ];


TSPlotNotJoinedInterpreter[parsed_] :=
    Block[{},
      TSPLOTJOINED = False;
      TSGRAPHICS = None;
      TSMESSAGE = "Using non-joined plots from now on.";
      "None"
    ];


TSPlotDataInterpreter[parsed_] :=
    Block[{},
      TSGRAPHICS = None;
      TSRESULT = None;
      TSMESSAGE = "Plotted data.";
      "OperationOnDataCompleted"
    ];


(* ::Section:: *)
(*Visualize results*)


Clear[VisualizeResults]
VisualizeResults[contextDataRules_, isize_ : 800] :=
    Block[{data = TSDATA /. (contextDataRules), ds, outliers, tfunc, bfunc, gr1, gr2, gr3, opts},
      data = N@Select[data, VectorQ[#, NumberQ]&];
      gr1 = DateListPlot[data, Joined -> TrueQ[TSPLOTJOINED /. contextDataRules]];
      Which[
        Head[TSRESULT /. (contextDataRules)] === TSBSplineQRegression ,
        Block[{x},
          gr2 = Plot[Through[First[TSRESULT /. (contextDataRules)][x]], Evaluate[{x, Min[data[[All, 1]]], Max[data[[All, 1]]]}], PlotStyle -> Darker[Red]]
        ]
        ,
        Head[TSRESULT /. (contextDataRules)] === TSLeastSquaresFit,
        gr2 = Plot[First[TSRESULT /. (contextDataRules)], Evaluate[{GetVariableName[First[TSRESULT /. (contextDataRules)]], Min[data[[All, 1]]], Max[data[[All, 1]]]}], PlotStyle -> Green],
        Head[TSRESULT /. (contextDataRules)] === TSOutliers,
        {outliers, tfunc, bfunc} = (TSRESULT /. (contextDataRules))[[1]];
        ds = (TSDATA /. contextDataRules)[[All, 1]];
        opts = {Joined -> {False, True, True}, PlotStyle -> {{PointSize[0.007], Red}, {Thickness[0.001], Lighter[Blue]}, {Thickness[0.001], Lighter[Blue]}}, PerformanceGoal -> "Speed"};
        gr2 =
            Which[
              Head[tfunc] === Symbol,
              DateListPlot[{outliers, Transpose[{ds, bfunc /@ ds}]}, opts],
              Head[bfunc] === Symbol,
              DateListPlot[{outliers, Transpose[{ds, tfunc /@ ds}]}, opts],
              True,
              DateListPlot[{outliers, Transpose[{ds, tfunc /@ ds}], Transpose[{ds, bfunc /@ ds}]}, opts]
            ];
        outliers = Tooltip[#, DateString[#[[1]], {"Year", ".", "Month", ".", "Day"}] <> ", " <> ToString[#[[2]]]]& /@ outliers;
        gr3 = ListPlot[outliers, PlotStyle -> {Red, PointSize[0.007]}]
      ];
      If[Head[TSGRAPHICS /. contextDataRules] === Graphics,
        gr1 = TSGRAPHICS /. contextDataRules;
      ];
      Print["VisualizeResults :", Head[TSGRAPHICS /. contextDataRules]];
      Which[
        Head[gr2] === Symbol,
        Show[{gr1}, PlotRange -> All, ImageSize -> isize],
        Head[gr3] === Symbol,
        Show[{gr1, gr2}, PlotRange -> All, ImageSize -> isize],
        True,
        Show[{gr1, gr2, gr3}, PlotRange -> All, ImageSize -> isize]
      ]
    ];


(* ::Section:: *)
(*Interpretation Interface*)


states = Sort@{"WaitingForARequest", "DataAvailable", "OperationOnDataCompleted", "WaitingForADataSpec", "WaitingForAnOpSpec"}


messages = Sort@{"StarOver", "WrongData", "UnknownDataSpec", "UnknownOperation", "DataLoaded", "DataNotLoaded", "None", "Note", "Help"}


contextFunctionRules = {TSHelp -> TSHelpInterpreter, TSWhatOperations -> TSWhatOperationsInerpreter, TSStartOver -> TSStartOverInterpreter, TSClearGraphics -> TSClearGraphicsInterpreter, TSLoadFile -> TSLoadFileInterpreter, TSLoadData -> TSLoadDataInterpreter, TSOperateOnDataCommand -> TSOperateOnDataCommandInterpreter, TSPlotJoined -> TSPlotJoinedInterpreter, TSPlotNotJoined -> TSPlotNotJoinedInterpreter, TSPlotData -> TSPlotDataInterpreter};


(* ::Text:: *)
(*Should not be included : TSOperateCommand->TSOperateCommandInterpreter*)


contextDataRules = {TSDATA -> {}, TSRESULT -> {}, TSMESSAGE -> "", TSGRAPHICS -> None, TSPLOTJOINED -> False};


FirstLetterToLowerCase[s_String] := If[StringLength[s] > 0, ToLowerCase[StringTake[s, 1]] <> StringTake[s, {2, -1}], s];


{viWidth} = {650};
{textOffset, rx, ry} = {0.01, 0.01, 0.004};
Magnify[
  DynamicModule[{input = "", fsmState = "WaitingForARequest", fsmMessage = {}, fsmContext = {}, speechMessage = "", visualMessage = ""},

  (* Clear the data symbols and assign to context *)
    Clear[TSDATA, TSRESULT, TSMESSAGE, TSGRAPHICS];
    fsmContext = {"data" -> {TSDATA -> {}, TSRESULT -> {}, TSMESSAGE -> "", TSGRAPHICS -> None, TSPLOTJOINED -> False}, "functions" -> contextFunctionRules};

    ColumnForm[{
      Panel[
        Row[
          {Style["speech input : ", Blue],
            InputField[Dynamic[input], String, ImageSize -> viWidth - 50]
          }]
        , ImageSize -> {viWidth, 60}],
      Dynamic[
        t = ParseShortest[pTSCOMMAND][ParseToTokens[FirstLetterToLowerCase[input]]];
        Print["t=", t];
        If[StringLength[input] > 0 && ListQ[t] && (Length[t] == 0 || ListQ[t[[1]]] && Length[t[[1, 1]]] > 0),
          fsmContext = {"data" -> Append[DeleteCases[fsmContext[[1, 2]], Rule[TSMESSAGE, ___]], Rule[TSMESSAGE, "Unknown input."]], "functions" -> contextFunctionRules},
        (*ELSE*)
          t = InterpretWithContext[ParseShortest[pTSCOMMAND][ParseToTokens[FirstLetterToLowerCase[input]]], fsmContext];
          If[ListQ[t] && Length[t] > 0 && ListQ[t[[1]]] && Length[t[[1]]] > 0 && TrueQ[Head[t[[1, 1, 2]]] === TSOperateCommand],
            t = InterpretWithContext[ParseShortest[pTSCOMMAND][ParseToTokens[FirstLetterToLowerCase[input] <> " for last loaded data"]], fsmContext]
          ]
        ];
        PRINT["ListQ[t]&&Length[t]\[Equal]2 : ", ListQ[t] && Length[t] == 2];
        If[ListQ[t] && Length[t] == 2,
          fsmMessage = If[Length[#] > 0, #[[1]], None]&[Flatten[t[[1]]]];
          fsmContext = {"data" -> t[[2]], "functions" -> contextFunctionRules};
        ];
        PRINT[fsmMessage];
        speechMessage = TSMESSAGE /. ("data" /. fsmContext);
        PRINT[speechMessage];
        (*Print[ColumnForm/@fsmContext];*)
        ColumnForm[
          {Panel[
            Column[
              {Style["spoken", Blue],
                If[StringQ[speechMessage], Speak[speechMessage];speechMessage, Null]
              }],
            ImageSize -> {viWidth, 50}],
            Panel[
              Column[{Style["shown", Blue],
                Which[
                  TrueQ[fsmMessage == "OperationOnDataCompleted"],
                  visualMessage = VisualizeResults["data" /. fsmContext, 600];
                  fsmContext = {"data" -> Append[DeleteCases[fsmContext[[1, 2]], Rule[TSGRAPHICS, ___]], Rule[TSGRAPHICS, visualMessage]], "functions" -> contextFunctionRules};
                  visualMessage,
                  TrueQ[fsmMessage == "Note"],
                  visualMessage = TSRESULT /. ("data" /. fsmContext),
                  True,
                  None
                ]
              }],
              ImageSize -> {viWidth, 450}]}],
        TrackedSymbols
            :> {input}]
    }]
  ], 1.4]
