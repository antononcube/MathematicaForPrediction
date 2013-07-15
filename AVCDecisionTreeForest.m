
(*
	Decision tree and random forest implementations in Mathematica
    Copyright (C) 2013  Anton Antonov

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

(* Version 0.8 *)
(* This version contains functions to build decision trees and forests and classify with them. The building functions take options to control the recursive process. *)

BeginPackage["AVCDecisionTreeForest`"]

BuildDecisionTree::usage = "BuildDecisionTree[dataMat,th,opts] makes a decision tree of the matrix dataMat the last column of which has the classification labels. BuildDecisionTree takes options to specify the impurity threshold at which the recursive process stops, should linear combinations of the variable be used as decision axes, the number of strata into which the ranges of the real variables are partitioned at each recursive step, and should the decision axis be determined looking into all axes or at a random sample of them."

BuildDecisionForest::usage = "BuildDecisionForest[dataMat,n,opts] makes a forest of n decision trees of the matrix dataMat the last column of which has the classification labels."

DecisionTreeClassify::usage = "DecisionTreeClassify[dTree,rec] predicts the label for the record rec using the decision tree dTree."

DecisionForestClassify::usage = "DecisionForestClassify[dtForest,rec] predicts the label for the record rec using the forest of decision trees dtForest."

DecisionTreeToRules::usage = "DecisionTreeToRules[dTree] transforms a decision tree into rules to be given to GraphPlot and related functions."

CentralizeDataMatrix::usage = "CentralizeDataMatrix[mat,colIndexes] transforms each of the specified columns of mat. The data of each of the specified columns is translated at the median and divided by the quartile distance. The returned result is a two element list of the data matrix with centralized columns and centralizing parameters. The centralizing parameters is a list of median and quartile distance pairs."

DecisionTreeClassificationSuccess::usage = "DecisionTreeClassificationSuccess[dTree, testDataArray, lbls] finds the classification success using dTree over the test data testDataArray for each classification label in lbls. If the last argument, lbls, is omitted then Union[testDataArray[[All,-1]]] is taken as the set of labels. The returned result is a set of rules {{_,True|False}->_?NumberQ..}. The rules {_,True}->_ are for the fractions of correct guesses; the rules {_,False}->_ are for the fractions of incorrect guesses. The rules {_,All}->_ are for the classification success fractions using all records of testDataArray."

DecisionForestClassificationSuccess::usage = "DecisionForestClassificationSuccess[dForest, testDataArray, lbls] finds the classification success using dForest over the test data testDataArray for each classification label in lbls. If the last argument, lbls, is omitted then Union[testDataArray[[All,-1]]] is taken as the set of labels. The returned result is a set of rules {{_,True|False}->_?NumberQ..}. The rules {_,True}->_ are for the fractions of correct guesses; the rules {_,False}->_ are for the fractions of incorrect guesses. The rules {_,All}->_ are for the classification success fractions using all records of testDataArray."


Begin["`Private`"]

(* In this version I added directions of splitting using SVD vectors and more options. *)

(* Decision tree building *)

Clear[AVC]
AVC[dataVar_?VectorQ, classLabels_?VectorQ] :=
  Block[{vals},
    Flatten /@ Tally[Transpose[{dataVar, classLabels}]]
    ] /; Length[dataVar] == Length[classLabels];
AVC[dataRecs_?MatrixQ, classLabels_?VectorQ, ind_Integer] :=
  Block[{vals},
    Flatten /@ Tally[Transpose[{dataRecs[[All, ind]], classLabels}]]
    ] /; Length[dataRecs] == Length[classLabels];

AVCEntropy[d_, d1_, d2_] :=
  Block[{ld, ld1, ld2},
   {ld, ld1, ld2} = {Total[d], Total[d1], Total[d2]};
   Which[TrueQ[ld > 0 && ld1 > 0 && ld2 > 0],
    N[Total[Map[-# Log[#] &, d/ld]] - ld1/ld Total[Map[-# Log[#] &, d1/ld1]] -
       ld2/ld Total[Map[-# Log[#] &, d2/ld2]]],
    True, 0
    ]
   ];

CAVCEntropy = Compile[{{d, _Real, 1}, {d1, _Real, 1}, {d2, _Real, 1}},
   Block[{ld, ld1, ld2},
    {ld, ld1, ld2} = {Total[d], Total[d1], Total[d2]};
    Which[TrueQ[ld > 0 && ld1 > 0 && ld2 > 0],
     N[Total[Map[-# Log[#] &, d/ld]] - 
       ld1/ld Total[Map[-# Log[#] &, d1/ld1]] - 
       ld2/ld Total[Map[-# Log[#] &, d2/ld2]]],
     True, 0
     ]
    ]];

AVCGini[d_, d1_, d2_] :=
  Block[{ld, ld1, ld2},
   {ld, ld1, ld2} = {Total[d], Total[d1], Total[d2]};
   Which[TrueQ[ld > 0 && ld1 > 0 && ld2 > 0],
    N[(1 - Total[Map[#^2 &, d/ld]]) - 
      ld1/ld (1 - Total[Map[# ^2 &, d1/ld1]]) - 
      ld2/ld (1 - Total[Map[# ^2 &, d2/ld2]])],
    True, 0
    ]
   ];

(*
avcTally in the impurity functions has the form 

{{varVal,classLabel,tallyNumber_Integer},...}

Here we are grouping across the class labels. Then for each label is found the sum of the counts across the variable values.
The entropy is calculated over the variable value sums.
*)

Clear[AVCCategoricalImpurity]
AVCCategoricalImpurity[avcTally_, varValue_, impFunc_] :=
  Block[{d, d1, d2},
   (*d1=GatherBy[Select[avcTally,#[[1]]==
   varValue&],#[[2]]&];
   d1=Map[{#[[1,2]],
   Total[#[[All,3]]]}&,d1];*)
   
   d1 = Rest /@ Select[avcTally, #[[1]] == varValue &];
   d2 = GatherBy[Select[avcTally, #[[1]] != varValue &], #[[2]] &];
   d2 = Map[{#[[1, 2]], Total[#[[All, 3]]]} &, d2];
   d = GatherBy[avcTally, #[[2]] &];
   d = Map[{#[[1, 2]], Total[#[[All, 3]]]} &, d];
   (*AVCEntropy[d,d1,d2]*)
   
   impFunc[d[[All, 2]], d1[[All, 2]], d2[[All, 2]]]
   ];

Clear[AVCNumericalImpurity]
AVCNumericalImpurity[avcTally_, varValue_, impFunc_] :=
  Block[{d, d1, d2},
   d1 = GatherBy[Select[avcTally, #[[1]] <= varValue &], #[[2]] &];
   d1 = Map[{#[[1, 2]], Total[#[[All, 3]]]} &, d1];
   d2 = GatherBy[Select[avcTally, #[[1]] > varValue &], #[[2]] &];
   d2 = Map[{#[[1, 2]], Total[#[[All, 3]]]} &, d2];
   d = GatherBy[avcTally, #[[2]] &];
   d = Map[{#[[1, 2]], Total[#[[All, 3]]]} &, d];
   (*AVCEntropy[d,d1,d2]*)
   
   impFunc[d[[All, 2]], d1[[All, 2]], d2[[All, 2]]]
   ];

(* Returns {{impurity,value}..} *)
Clear[AVCFindBestSplitValueCategorical]
AVCFindBestSplitValueCategorical[avcTally_, impFunc_] :=
  Block[{varVals, labelVals},
   varVals = Union[avcTally[[All, 1]]];
   First@SortBy[
     Map[{AVCCategoricalImpurity[avcTally, #, impFunc], #} &, 
      varVals], -#[[1]] &]
   ];

(* Returns {{impurity,value}..} *)
Clear[AVCFindBestSplitValueNumerical]
AVCFindBestSplitValueNumerical[avcTally_, nStrata_Integer, impFunc_] :=
  Block[{t, varVals, h},
   varVals = Union[avcTally[[All, 1]]];
   If[Max[varVals] == Min[varVals],
    Return[{0, Max[varVals]}]
    ];
   (*h=Min[Differences[Sort[varVals]]];*)
   
   h = (Max[varVals] - Min[varVals])/nStrata;
   First@SortBy[
     Map[{AVCNumericalImpurity[avcTally, #, impFunc], #} &, 
      Range[Min[varVals], Max[varVals], h]], -#[[1]] &]
   ];

Clear[AVCFindBestSplitValue]
AVCFindBestSplitValue[avcTally_, varType_, nStrata_Integer, impFunc_] :=
  Block[{},
   If[varType === Number,
    AVCFindBestSplitValueNumerical[avcTally, nStrata, impFunc],
    AVCFindBestSplitValueCategorical[avcTally, impFunc]
    ]
   ];

Clear[AVCSplitSelection]
AVCSplitSelection[dataRecs_?MatrixQ, classLabels_?VectorQ, 
   columnTypes_?VectorQ, axesArg : (All | {_Integer ..}), nStrata_Integer, 
   impFunc_, {linCombMinRecs_Integer, svdRank_Integer}] :=
  Block[{avcs, res, axes = axesArg, numAxes, numAvcs, numDataRecs, U, S, V, 
     numRes = {}},
    If[axes === All,
     axes = Range[1, Dimensions[dataRecs][[2]]]
     ];
    (* should we do the AVC before the quantization of the numerical variables? *)
    avcs = Map[AVC[dataRecs[[All, #]], classLabels] &, axes];
    res = Table[
      Append[AVCFindBestSplitValue[avcs[[i]], columnTypes[[axes[[i]]]], 
        nStrata, impFunc], axes[[i]]], {i, Length[axes]}];
    
    (* select linear combination of numerical variables (axes) using thin SVD *)
    If[svdRank > 0 && Dimensions[dataRecs][[1]] > linCombMinRecs,
     numAxes = Pick[axes, Map[# === Number &, columnTypes[[axes]]]];
     If[Length[numAxes] >= svdRank,
      (* find the splitting class label *)
      
      numAvcs = SortBy[Tally[classLabels], -#[[2]] &];
      PRINT["AVCSplitSelection:: splitting class ratio=", 
       N[numAvcs[[1, 2]]/Total[numAvcs[[All, 2]]]]];
      If[numAvcs[[1, 2]]/Total[numAvcs[[All, 2]]] <= 1/2,
       numDataRecs = 
        Pick[dataRecs, Map[# == numAvcs[[1, 1]] &, classLabels]],
       numDataRecs = Pick[dataRecs, Map[# != numAvcs[[1, 1]] &, classLabels]]
       ];
      
      (* check is the set too pure *)
      
      If[Length[numDataRecs] > 0.1*linCombMinRecs && Length[numDataRecs] > svdRank,
       PRINT["AVCSplitSelection:: Dimensions[numDataRecs] = ", Dimensions[numDataRecs]];
       
       (* find splitting directions using SVD *)
       
       PRINT["AVCSplitSelection:: SVD timing",
        AbsoluteTiming[
         (* the union is needed in order to avoid singular matrices *)
       
         numDataRecs = Union[numDataRecs[[All, numAxes]]];
         {U, S, V} = SingularValueDecomposition[numDataRecs, svdRank, Tolerance -> 0.01];
         ]
        ];
       
       (* compute the variable columns of the linear combinations *)
       
       numDataRecs = dataRecs[[All, numAxes]].V;
       numAvcs = Map[AVC[numDataRecs[[All, #]], classLabels] &, Range[svdRank]];
       PRINT["AVCSplitSelection:: Length/@numAvcs = ", Length /@ numAvcs];
       numRes = 
        Table[Append[
          AVCFindBestSplitValue[numAvcs[[i]], Number, nStrata, impFunc], {numAxes, V[[All, i]]}], {i, svdRank}];
       ];
      ];
     ];
    
    res = SortBy[Join[res, numRes], -#[[1]] &];
    First[res]
    ] /; Length[dataRecs[[1]]] == Length[columnTypes] && 
    Length[dataRecs] == 
     Length[classLabels] && (axesArg === All || 
      Min[axesArg] >= 1 && Max[axesArg] <= Length[dataRecs[[1]]]);

Clear[RandomAxes]
RandomAxes[nDimensions_] :=
  Which[
   nDimensions > 10, RandomSample[Range[nDimensions], 5],
   nDimensions > 5, RandomSample[Range[nDimensions], 3],
   nDimensions > 2, RandomSample[Range[nDimensions], 2],
   True, Range[nDimensions]
   ];

Clear[BuildDecisionTree]
Options[BuildDecisionTree] = {"RandomAxes" -> False, "ImpurityFunction" -> "Gini", 
   "ImpurityThreshold" -> 0, "NumberOfStrata" -> 100, 
   "LinearCombinations" -> {"MinSize" -> 200, "SVDRank" -> 2}};
BuildDecisionTree[data_, columnTypes_, level_Integer, Theta_, opts : OptionsPattern[]] :=
  Block[{res, d1, d2, axesArg,
     randomAxes = OptionValue[BuildDecisionTree, "RandomAxes"],
     impFunc = OptionValue[BuildDecisionTree, "ImpurityFunction"],
     impurityTh = OptionValue[BuildDecisionTree, "ImpurityThreshold"],
     nStrata = OptionValue[BuildDecisionTree, "NumberOfStrata"],
     linComb = OptionValue[BuildDecisionTree, "LinearCombinations"],
     linCombMinRecs, svdRank},
    
    (* Options handling *)
    {linCombMinRecs, svdRank} = {"MinSize", "SVDRank"} /. linComb /. {"MinSize" -> 200, "SVDRank" -> 2};

    Which[
      TrueQ[svdRank === All], svdRank = Count[columnTypes, Number],
      ! IntegerQ[svdRank], svdRank = 0
    ];

	impFunc = If[ TrueQ[ impFunc == "Entropy"], AVCEntropy, AVCGini ];    

    If[Length[data] < 1, {{{None, 0}}},
     (* Random axes assignment *)
     axesArg =
      Which[
       TrueQ[randomAxes] || 
        IntegerQ[randomAxes] && randomAxes >= Length[data[[1]]] - 1,
       RandomAxes[Length[data[[1]]] - 1],
       IntegerQ[randomAxes],
       Sort[RandomSample[Range[Length[data[[1]]] - 1], randomAxes]],
       True,
       All
       ];
     
     (* Splitting axis and value finding *)
     
     res = AVCSplitSelection[data[[All, 1 ;; -2]], data[[All, -1]], 
       Most[columnTypes], axesArg, nStrata, 
       impFunc, {linCombMinRecs, svdRank}];
     
     (* Recursive calling *)
     Which[
      (*res[[1]]<=impurityTh,{{{Length[data],data[[1,-1]]}}},*)
      
      res[[1]] <= impurityTh || Length[data] < Theta,
      {SortBy[Reverse /@ Tally[data[[All, -1]]], -#[[1]] &]},
      True,
      Which[
       MatrixQ[res[[3]], NumberQ],
       PRINT["BuildDecisionTree:: res[[3]]=", 
        res[[3]]];
       d1 = Select[data, #[[res[[3, 1]]]].res[[3, 2]] <= res[[2]] &];
       d2 = Select[data, #[[res[[3, 1]]]].res[[3, 2]] > res[[2]] &],
       columnTypes[[res[[3]]]] === Number,
       d1 = Select[data, #[[res[[3]]]] <= res[[2]] &];
       d2 = Select[data, #[[res[[3]]]] > res[[2]] &],
       True,
       d1 = Select[data, #[[res[[3]]]] === res[[2]] &];
       d2 = Select[data, #[[res[[3]]]] =!= res[[2]] &]
       ];
      {Join[
        res, {If[MatrixQ[res[[3]], NumberQ], Dot, columnTypes[[res[[3]]]]], 
         Length[data]}],
       BuildDecisionTree[d1, columnTypes, level + 1, Theta, opts],
       BuildDecisionTree[d2, columnTypes, level + 1, Theta, opts]}
      ]
     ]
    ] /; Length[data[[1]]] == Length[columnTypes];
BuildDecisionTree[data_, th_: 1, opts : OptionsPattern[]] :=
  Block[{columnTypes},
    columnTypes = 
     Map[Apply[And, NumericQ /@ data[[All, #]]] &, 
      Range[1, Length[data[[1]]]]];
    columnTypes = columnTypes /. {True -> Number, False -> Symbol};
    BuildDecisionTree[data, columnTypes, 0, th, opts]
    ] /; NumberQ[th];

(* Forest *)

Clear[BuildDecisionForest]
BuildDecisionForest[data_, th_, n_Integer, opts : OptionsPattern[]] :=
  Table[BuildDecisionTree[data, th, "RandomAxes" -> True, opts], {n}];

Clear[ParallelBuildDecisionForest]
ParallelBuildDecisionForest[data_, th_, n_Integer, opts : OptionsPattern[]] :=
  ParallelTable[BuildDecisionTree[data, th, "RandomAxes" -> True, opts], {n}];

(* Classify by tree *)

(*
Each node of the tree has the following signature
{splitting strength, splitting value, column index of splitting variable, type of the variable}
*)

Clear[DecisionTreeClassify]
DecisionTreeClassify[dTree_, record_] :=
  Block[{},
   Which[
    MatrixQ[dTree] && Dimensions[dTree][[2]] == 2, dTree,
    MatrixQ[dTree[[1]]] && Dimensions[dTree[[1]]][[2]] == 2, dTree[[1]],
    MatrixQ[dTree[[1, 3]], NumberQ],
    If[record[[dTree[[1, 3, 1]]]].dTree[[1, 3, 2]] <= dTree[[1, 2]], 
     DecisionTreeClassify[dTree[[2]], record], DecisionTreeClassify[dTree[[3]], record]],
    dTree[[1, 4]] === Number,
    If[record[[dTree[[1, 3]]]] <= dTree[[1, 2]], 
     DecisionTreeClassify[dTree[[2]], record], DecisionTreeClassify[dTree[[3]], record]],
    dTree[[1, 4]] === Symbol,
    If[record[[dTree[[1, 3]]]] === dTree[[1, 2]], 
     DecisionTreeClassify[dTree[[2]], record], DecisionTreeClassify[dTree[[3]], record]],
    True, PRINT[dTree]; If[MatrixQ[dTree[[1]]], dTree[[1]], {dTree[[1]]}]
    ]
   ];

(* Classify by forest *)

Clear[DecisionForestClassify]
DecisionForestClassify[forest_, record_] :=
  Block[{res},
   res = DecisionTreeClassify[#, record] & /@ forest;
   res = Flatten[res, 1];
   res = GatherBy[res, #[[2]] &];
   res = Map[{Total[#[[All, 1]]], #[[1, 2]]} &, res];
   SortBy[res, -#[[1]] &]
   ];

Clear[ParallelDecisionForestClassify]
ParallelDecisionForestClassify[forest_, record_] :=
  Block[{res},
   res = ParallelMap[DecisionTreeClassify[#, record] &, forest, Method -> "CoarsestGrained", DistributedContexts -> Automatic];
   res = Flatten[res, 1];
   res = GatherBy[res, #[[2]] &];
   res = Map[{Total[#[[All, 1]]], #[[1, 2]]} &, res];
   SortBy[res, -#[[1]] &]
   ];

(* Convert to rules *)

(* This function transforms a decision tree into rules to be given to GraphPlot and related functions. *)

Clear[DecisionTreeToRules]
DecisionTreeToRules[tree_] :=
  Which[
    tree === {}, {},
    Rest[tree] === {}, {},
    MatrixQ[tree] && Dimensions[tree][[2]] == 2, {},
    Length[Rest[tree]] == 2,
    Join[
     MapThread[
      Which[
        #2 == "True" && Length[tree[[1]]] >= 4 && 
         (tree[[1, 4]] === Number || tree[[1, 4]] === Dot), {tree[[1]] -> #1[[1]], 
         "\[LessEqual] " <> ToString[NumberForm[tree[[1, 2]], 2]]},
        #2 == "False" && Length[tree[[1]]] >= 4 && 
         (tree[[1, 4]] === Number || tree[[1, 4]] === Dot), {tree[[1]] -> #1[[1]], 
         "> " <> ToString[NumberForm[tree[[1, 2]], 2]]},
        #2 == "True" && Length[tree[[1]]] >= 4 && 
         tree[[1, 4]] === Symbol, {tree[[1]] -> #1[[1]], 
         "= " <> ToString[tree[[1, 2]]]},
        #2 == "False" && Length[tree[[1]]] >= 4 && 
         tree[[1, 4]] === Symbol, {tree[[1]] -> #1[[1]], 
         "\[NotEqual] " <> ToString[tree[[1, 2]]]},
        True, {tree[[1]] -> #1[[1]], #2}
        ] &, {Rest[tree], {"True", "False"}}, 1],
     Flatten[DecisionTreeToRules[#] & /@ Rest[tree], 1]
     ],
    True,
    Join[Map[{tree[[1]] -> #[[1]]} &, Rest[tree], {1}], 
     Flatten[DecisionTreeToRules[#] & /@ Rest[tree], 1]]
    ] /. {{r_Rule, edge_String} :> {r, 
      Style[edge, Background -> White, FontSlant -> Plain]}};

(* Centralize data *)

(*
This function takes as arguments a data matrix and specified indexes of numerical columns. The data of each of the specified columns is translated at the median and dividied by the quartile distance. The returned result is the data matrix with centralized columns and the centralizing parameters, a list of pairs median and quartile distance.
*)

Clear[CentralizeDataMatrix]
CentralizeDataMatrix[dataArg_?MatrixQ] := CentralizeDataMatrix[dataArg, All];
CentralizeDataMatrix[dataArg_?MatrixQ, indsArg : ({_Integer ..} | All)] :=  
  Block[{data = dataArg, m, qd, inds = indsArg, centralizers = {}},
   If[inds === All, inds = Range[Dimensions[data][[2]]]];
   Do[
    m = Median[data[[All, i]]];
    qd = Quantile[data[[All, i]], {1/4, 1/2, 3/4}];
    qd = qd[[3]] - qd[[1]];
    data[[All, i]] = (data[[All, i]] - m)/qd;
    AppendTo[centralizers, {m, qd}]
    , {i, inds}];
   {data, centralizers}
   ];


(* DecisionTreeClassificationSuccess *)

Clear[DecisionTreeOrForestClassificationSuccess, DecisionTreeClassificationSuccess, DecisionForestClassificationSuccess]
DecisionTreeOrForestClassificationSuccess[classFunc : (DecisionTreeClassify | DecisionForestClassify), dTreeOrForest_, dataArr_?MatrixQ] := DecisionTreeOrForestClassificationSuccess[classFunc, dTreeOrForest, dataArr, Union[dataArr[[All, -1]]]];
DecisionTreeOrForestClassificationSuccess[classFunc : (DecisionTreeClassify | DecisionForestClassify), dTreeOrForest_, dataArr_?MatrixQ, labels_?VectorQ] :=
  Block[{guesses, guessStats, tdata, t},
   t =
    Table[
     (tdata = Select[dataArr, #[[-1]] == lbl &];
      guesses = classFunc[dTreeOrForest, Most[#]][[1, 2]] & /@ tdata;
      guessStats = MapThread[Equal, {guesses, tdata[[All, -1]]}];
      {Count[guessStats, True], Count[guessStats, False]}/
        Length[tdata] // N)
     , {lbl, labels}];
   t = MapThread[{{#1, True} -> #2[[1]], {#1, False} -> #2[[
         2]]} &, {labels, t}];
   guesses = classFunc[dTreeOrForest, Most[#]][[1, 2]] & /@ dataArr;
   guessStats = MapThread[Equal, {guesses, dataArr[[All, -1]]}];
   Flatten[#, 1] &@
    Join[t, {{All, 
        True} -> (Count[guessStats, True]/Length[dataArr] // N), {All,
         False} -> (Count[guessStats, False]/Length[dataArr] // N)}]
  ];

DecisionTreeClassificationSuccess[dTreeOrForest_, dataArr_?MatrixQ, x___] := 
  DecisionTreeOrForestClassificationSuccess[DecisionTreeClassify, dTreeOrForest, dataArr, x];

DecisionForestClassificationSuccess[dTreeOrForest_, dataArr_?MatrixQ, x___] := 
  DecisionTreeOrForestClassificationSuccess[DecisionForestClassify, dTreeOrForest, dataArr, x];

End[]

EndPackage[]