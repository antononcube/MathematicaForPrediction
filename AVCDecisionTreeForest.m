
(*
	Decision tree and random forest implementations in Mathematica
    Copyright (C) 2013-2016  Anton Antonov

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
(* ToDo implement functions for conversion to ClassifierFunction objects. *)


BeginPackage["AVCDecisionTreeForest`"]

BuildDecisionTree::usage = "BuildDecisionTree[dataMat,{minSizeTh,impTh},opts] makes a decision tree of the matrix dataMat the last column of which has the classification labels. The recursive tree building stops when the data set length is less than minSizeTh or the data set impurity is less than impTh. BuildDecisionTree takes options to specify should linear combinations of the variable be used as decision axes, the number of strata into which the ranges of the real variables are partitioned at each recursive step, and should the decision axis be determined looking into all axes or at a random sample of them. BuildDecisionTree[dataMat,minSizeTh,n,opts] calls BuildDecisionTree[dataMat,{minSizeTh,0},n,opts] ."

BuildDecisionForest::usage = "BuildDecisionForest[dataMat,{minSizeTh,impTh},n,opts] makes a forest of n decision trees of the matrix dataMat the last column of which has the classification labels. The recursive tree building stops when the data set length is less than minSizeTh or the data set impurity is less than impTh. BuildDecisionForest[dataMat,minSizeTh,n,opts] calls BuildDecisionForest[dataMat,{minSizeTh,0},n,opts] ."

DecisionTreeClassify::usage = "DecisionTreeClassify[dTree,rec] predicts the label for the record rec using the decision tree dTree."

DecisionForestClassify::usage = "DecisionForestClassify[dtForest,rec] predicts the label for the record rec using the forest of decision trees dtForest."

DecisionTreeToRules::usage = "DecisionTreeToRules[dTree] transforms a decision tree into rules to be given to GraphPlot and related functions."

CentralizeDataMatrix::usage = "CentralizeDataMatrix[mat,colIndexes] transforms each of the specified columns of mat. The data of each of the specified columns is translated at the median and divided by the quartile distance. The returned result is a two element list of the data matrix with centralized columns and centralizing parameters. The centralizing parameters is a list of median and quartile distance pairs."

DecisionTreeClassificationSuccess::usage = "DecisionTreeClassificationSuccess[dTree, testDataArray, lbls] finds the classification success using dTree over the test data testDataArray for each classification label in lbls. If the last argument, lbls, is omitted then Union[testDataArray[[All,-1]]] is taken as the set of labels. The returned result is a set of rules {{_,True|False}->_?NumberQ..}. The rules {_,True}->_ are for the fractions of correct guesses; the rules {_,False}->_ are for the fractions of incorrect guesses. The rules {_,All}->_ are for the classification success fractions using all records of testDataArray."

DecisionForestClassificationSuccess::usage = "DecisionForestClassificationSuccess[dForest, testDataArray, lbls] finds the classification success using dForest over the test data testDataArray for each classification label in lbls. If the last argument, lbls, is omitted then Union[testDataArray[[All,-1]]] is taken as the set of labels. The returned result is a set of rules {{_,True|False}->_?NumberQ..}. The rules {_,True}->_ are for the fractions of correct guesses; the rules {_,False}->_ are for the fractions of incorrect guesses. The rules {_,All}->_ are for the classification success fractions using all records of testDataArray."

DecisionTreeNumberOfNodesAndLeaves::usage = "DecisionTreeNumberOfNodesAndLeaves[dTree] gives a list of two numbers: the number of internal nodes and the number of leaves of dTree."

DecisionTreeLeafQ::usage = "DecisionTreeLeafQ[rec] tests is rec a decision tree leaf."

DecisionTreeLabels::usage = "DecisionTreeLabels gives the labels used in a decision tree."

DecisionTreeCombinedLeaves::usage = "DecisionTreeCombinedLeaves[dTree] returns all the leaves of dTree into one. The frequencies of the corresponding labels are summed."

PruneDecisionTree::usage = "PruneDecisionTree[dTree] prunes branches of the decision tree dTree using the minimal description length principle."


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
AVCFindBestSplitValueNumerical[avcTally_, 0, impFunc_] :=
  Block[{varVals},
   varVals = Union[avcTally[[All, 1]]];
   First@SortBy[Map[{AVCNumericalImpurity[avcTally, #, impFunc], #} &, varVals], -#[[1]] &]
  ];

Clear[AVCFindBestSplitValue]
AVCFindBestSplitValue[avcTally_, varType_, nStrata_Integer, impFunc_] :=
  Block[{},
   If[varType === Number,
    AVCFindBestSplitValueNumerical[avcTally, nStrata, impFunc],
    AVCFindBestSplitValueCategorical[avcTally, impFunc]
    ]
   ];

Clear[Stratify]
Stratify[data : {_?NumberQ ..}, nStrata_Integer] :=
  Block[{t, min, max, h},
   {min, max} = {Min[data], Max[data]};
   If[min == max,
    data,
    h = (max - min)/nStrata;
    Map[Round[(# - min), h] + min &, data]
    ]
   ];
Stratify[data_, nStrata_Integer] := data;

Clear[AVCSplitSelectionLC]
AVCSplitSelectionLC[dataRecs_?MatrixQ, classLabels_?VectorQ, 
   columnTypes_?VectorQ, axesArg : (All | {_Integer ..}), 
   nStrata_Integer, 
   impFunc_, {linCombMinRecs_Integer, linCombMaxRecs_Integer, svdRank_Integer, 
    cdSVDRank_Integer, svdLabels : (All | Automatic | _List)}, 
   preStratifyQ : (True | False)] :=
  
  Block[{axes = axesArg, numAxes, numAvcs, numDataRecs, U, S, V, cU, cS, cV, numRes = {}, crs, rank, inRules},
   
   (* select linear combination of numerical variables (axes) using thin SVD *)
   
   If[(svdRank > 0 || cdSVDRank > 0) && Dimensions[dataRecs][[1]] > linCombMinRecs && Dimensions[dataRecs][[1]] <= linCombMaxRecs,

    numAxes = Pick[axes, Map[# === Number &, columnTypes[[axes]]]];

    If[Length[numAxes] > 1 && (Length[numAxes] >= svdRank || Length[numAxes] >= cdSVDRank),
     (* find the splitting class label *)
     
     numAvcs = SortBy[Tally[classLabels], -#[[2]] &];
     PRINT["AVCSplitSelection:: splitting class ratio=", N[numAvcs[[1, 2]]/Length[classLabels]]];
     
     Which[
      TrueQ[svdLabels === All],
      numDataRecs = dataRecs,
      TrueQ[svdLabels === Automatic],
      If[numAvcs[[1, 2]]/Length[classLabels] <= 1/2,
       numDataRecs = Pick[dataRecs, Map[# == numAvcs[[1, 1]] &, classLabels]],
       numDataRecs = Pick[dataRecs, Map[# != numAvcs[[1, 1]] &, classLabels]]
      ],
      ListQ[svdLabels],
      inRules = Dispatch[Append[Thread[svdLabels -> True], _?AtomQ -> False]];
      numDataRecs = Pick[dataRecs, classLabels /. inRules],
      True,
      numDataRecs = {}
     ];
     
     (* check is the one-label subset too pure or too small *)
     
     If[Length[numDataRecs] > 0.1*linCombMinRecs && (Length[numDataRecs] > Max[svdRank, cdSVDRank]),
      PRINT["AVCSplitSelection:: Dimensions[numDataRecs] = ", Dimensions[numDataRecs]];
      
      (* find splitting directions using SVD *)      
      PRINT["AVCSplitSelection:: SVD timing",
       AbsoluteTiming[
        (* The union is needed in order to avoid singular matrices. *)
        (* Note that N is applied to the matrix -- I do not see why high precision would be used, *)
        (* this is just a heuristic for finding a direction. *)
        numDataRecs = N[Union[numDataRecs[[All, numAxes]]]];
        {U, S, V} = SingularValueDecomposition[numDataRecs, svdRank, Tolerance -> 0.01];
        If[cdSVDRank > 0,
         {numDataRecs, crs} = MedianCentralizeDataMatrix[numDataRecs, All];
         {cU, cS, cV} = SingularValueDecomposition[numDataRecs, cdSVDRank, Tolerance -> 0.01];
         ];
        ]
       ];
      PRINT["AVCSplitSelection:: Dimensions[V]=", Dimensions[V]];
      PRINT["AVCSplitSelection:: Dimensions[cV]=", Dimensions[cV]];
      If[cdSVDRank > 0,
      V = Transpose[Union[Join[Transpose[V], Transpose[cV]], SameTest -> (Abs[#1.#2] >= 0.98 &)]]];
      PRINT["AVCSplitSelection:: After union of directions Dimensions[V]=", Dimensions[V]];
      rank = Dimensions[V][[2]];
      If[rank == 0, {},
        (* compute the variable columns of the linear combinations *)
        Assert[numAxes == Dimensions[V][[1]]];
        numDataRecs = dataRecs[[All, numAxes]].V;
        If[preStratifyQ,
          numAvcs = Map[AVC[Stratify[numDataRecs[[All, #]], nStrata], classLabels] &, Range[rank]];
          numRes = Table[Append[AVCFindBestSplitValue[numAvcs[[i]], Number, 0, impFunc], {numAxes, V[[All, i]]}], {i, rank}],
          (* ELSE *)
          PRINT["AVCSplitSelection:: Dimensions[numDataRecs]=", Dimensions[numDataRecs]];
          numAvcs = Map[AVC[numDataRecs[[All, #]], classLabels] &, Range[rank]];
          PRINT["AVCSplitSelection:: Length/@numAvcs = ", Length /@ numAvcs];
          numRes = Table[Append[AVCFindBestSplitValue[numAvcs[[i]], Number, nStrata, impFunc], {numAxes, V[[All, i]]}], {i, rank}]
        ]
       ];
      ];
     ];
    ];
    numRes
   ];

Clear[AVCSplitSelection]
AVCSplitSelection[dataRecs_?MatrixQ, classLabels_?VectorQ, 
   columnTypes_?VectorQ, axesArg : (All | {_Integer ..}), 
   nStrata_Integer, 
   impFunc_, {linCombMinRecs_Integer, linCombMaxRecs_Integer, svdRank_Integer, 
    crSVDRank_Integer, svdLabels : (All | Automatic | _List)}, 
   preStratifyQ : (True | False)] :=
  Block[{avcs, res, axes = axesArg, numAxes, numRes = {}, numAvcs, 
     numDataRecs},
    If[axes === All,
     axes = Range[1, Dimensions[dataRecs][[2]]]
     ];
    
    If[preStratifyQ,
     avcs = 
      MapThread[
       If[TrueQ[#2 === Number], 
         AVC[Stratify[dataRecs[[All, #1]], nStrata], classLabels], 
         AVC[dataRecs[[All, #1]], classLabels]] &, {axes, 
        columnTypes[[axes]]}];
     res = Table[Append[AVCFindBestSplitValue[avcs[[i]], columnTypes[[axes[[i]]]], 0, impFunc], axes[[i]]], {i, Length[axes]}],
     (*ELSE*)
     (* should we do the AVC before the stratification of the numerical variables? *)
     
     avcs = Map[AVC[dataRecs[[All, #]], classLabels] &, axes];
     res = 
      Table[Append[
        AVCFindBestSplitValue[avcs[[i]], columnTypes[[axes[[i]]]], 
         nStrata, impFunc], axes[[i]]], {i, Length[axes]}];
     ];
    
    numRes = 
     AVCSplitSelectionLC[dataRecs, classLabels, columnTypes, axes, 
      nStrata, 
      impFunc, {linCombMinRecs, linCombMaxRecs, svdRank, crSVDRank, svdLabels}, 
      preStratifyQ];
    
    res = SortBy[Join[res, numRes], -#[[1]] &];
    First[res]
    ] /; Length[dataRecs[[1]]] == Length[columnTypes] && 
    Length[dataRecs] == 
     Length[classLabels] && (axesArg === All || 
      Min[axesArg] >= 1 && Max[axesArg] <= Length[dataRecs[[1]]]);

Clear[RandomAxes]
RandomAxes[nDimensions_] :=
  Sort[
    Which[
      nDimensions > 10, RandomSample[Range[nDimensions], 5],
      nDimensions > 5, RandomSample[Range[nDimensions], 3],
      nDimensions > 2, RandomSample[Range[nDimensions], 2],
      True, Range[nDimensions]
    ]
  ];

Clear[BuildTreeRecStep]
BuildTreeRecStep[data_, columnTypes_, level_Integer, 
   minSizeTh_Integer, axes : (All | {_Integer ..}), nStrata_Integer, 
   impFunc_, 
   impurityTh_?NumberQ, {linCombMinRecs_Integer, linCombMaxRecs_Integer, svdRank_Integer, 
    cdSVDRank_Integer, svdLabels_}, preStratifyQ : (False | True)] :=

    Block[{res, d1, d2},
    
    (*PRINT["BuildTreeRecStep::",{\[Theta],axes,nStrata,impFunc,
    impurityTh,{linCombMinRecs,svdRank,cdSVDRank,svdLabels},
    preStratifyQ}];
    *)
    If[Length[data] < 1, {{{None, 0}}},
     
     (* Splitting axis and value finding *)
     
     res = AVCSplitSelection[data[[All, 1 ;; -2]], data[[All, -1]], 
       Most[columnTypes], axes, nStrata, 
       impFunc, {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels}, 
       preStratifyQ];
     
     (* Recursive calling *)
     Which[
      (*res\[LeftDoubleBracket]1\[RightDoubleBracket]<=
      impurityTh,{{{Length[data],
      data\[LeftDoubleBracket]1,-1\[RightDoubleBracket]}}},*)
      
      res[[1]] <= impurityTh || Length[data] < minSizeTh,
      {SortBy[Reverse /@ Tally[data[[All, -1]]], -#[[1]] &]},
      True,
      Which[
       MatrixQ[res[[3]], NumberQ],
       PRINT[
        "BuildTree:: res\[LeftDoubleBracket]3\[RightDoubleBracket]=", 
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
        res, {If[MatrixQ[res[[3]], NumberQ], Dot, 
          columnTypes[[res[[3]]]]], Length[data]}],
       BuildTreeRecStep[d1, columnTypes, level + 1, minSizeTh, axes, 
        nStrata, impFunc, 
        impurityTh, {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels}, 
        preStratifyQ],
       BuildTreeRecStep[d2, columnTypes, level + 1, minSizeTh, axes, 
        nStrata, impFunc, 
        impurityTh, {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels}, 
        preStratifyQ]}
      ]
     ]
    ] /; minSizeTh > 0;

BuildDecisionTree::fvalopt = "The value of `1` should have the form `2`.";

BuildDecisionTree::ivalopt = "The value of `1` should be a positive integer.";

BuildDecisionTree::iavalopt = "The value of `1` should be Automatic or an integer between `2` and `3`.";

BuildDecisionTree::rvalopt = "The value of `1` should be a positive real number.";

BuildDecisionTree::lvalopt = "The value of `1` should be Automatic, All, or a subset of the class labels `2`.";

BuildDecisionTree::farg = "The first argument is expected to be a matrix all elements of which are atoms.";

Clear[BuildDecisionTree]
Options[BuildDecisionTree] = {"RandomAxes" -> False, 
   "ImpurityFunction" -> "Gini", "Strata" -> 100, 
   "LinearCombinations" -> {"MinSize" -> Automatic, "Rank" -> 2, 
     "CentralizedDataRank" -> Automatic, "Labels" -> Automatic}, 
   "PreStratify" -> False};
BuildDecisionTree[data_, 
   columnTypes_, {minSizeTh_Integer, impurityTh_?NumberQ}, 
   opts : OptionsPattern[]] :=
  Block[{res, d1, d2, axesArg,
     randomAxes = OptionValue[BuildDecisionTree, "RandomAxes"],
     impFunc = OptionValue[BuildDecisionTree, "ImpurityFunction"],
     nStrata = OptionValue[BuildDecisionTree, "Strata"],
     linComb = OptionValue[BuildDecisionTree, "LinearCombinations"],
     preStratifyQ = OptionValue[BuildDecisionTree, "PreStratify"],
     linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels, nNumVars, lbls},
    
    (* Options handling *)
    nNumVars = Count[columnTypes, Number];
    lbls = Union[data[[All, -1]]];
    Which[
      TrueQ[ linComb === Automatic ],
      {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels} = {Automatic, Automatic, Automatic, Automatic, Automatic},
      ! TrueQ[ linComb==False || linComb==None ],
      {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels} = {"MinSize", "MaxSize", "Rank", "CentralizedDataRank", "Labels"} /. linComb /. {"MinSize" -> Automatic, "MaxSize" -> Automatic, "Rank" -> Automatic, "CentralizedDataRank" -> Automatic, "Labels" -> Automatic},
      True,
      {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels} = {Length[data], Length[data], 0, 0, {}}
    ];
    If[ nNumVars < 2,
      {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels} = {Length[data], Length[data], 0, 0, {}}
    ];
    If[TrueQ[linCombMinRecs === Automatic], linCombMinRecs = Floor[0.1 Dimensions[data][[1]]]];
    If[TrueQ[linCombMaxRecs === Automatic], linCombMaxRecs = Dimensions[data][[1]]];
    If[TrueQ[svdRank === Automatic], svdRank = 2];
    If[TrueQ[cdSVDRank === Automatic], cdSVDRank = svdRank];
    If[! (IntegerQ[linCombMinRecs] && linCombMinRecs > 0), 
      Message[BuildDecisionTree::iavalopt, "MinSize", 1, Length[data]]; 
      Return[{}]
    ];
    If[! (IntegerQ[linCombMaxRecs] && linCombMaxRecs >= linCombMinRecs && linCombMaxRecs <= Dimensions[data][[1]] ), 
      Message[BuildDecisionTree::iavalopt, "MaxSize", linCombMinRecs, Length[data]]; 
      Return[{}]
    ];
    If[! (IntegerQ[svdRank] && 0 <= svdRank <= nNumVars), Message[BuildDecisionTree::iavalopt, "Rank", 0, nNumVars]; Return[{}]];
    If[! (IntegerQ[cdSVDRank] && 0 <= cdSVDRank <= nNumVars), Message[BuildDecisionTree::iavalopt, "CentralizedDataRank", 0, nNumVars]; Return[{}]];
    If[AtomQ[svdLabels] && !TrueQ[svdLabels === Automatic] && !TrueQ[svdLabels === All], svdLabels = {svdLabels}];
    If[! (TrueQ[svdLabels === Automatic] || TrueQ[svdLabels === All] || Apply[And, Map[MemberQ[lbls, #] &, svdLabels]]), Message[BuildDecisionTree::lvalopt, "Labels", lbls]; Return[{}]];
    
    PRINT["BuildTree:: {linCombMinRecs,linCombMaxRecs,svdRank,cdSVDRank,svdLabels}=", {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels}];
    
    {svdRank, cdSVDRank} =
     Map[
      Which[
        TrueQ[# === All], Count[columnTypes, Number],
        ! IntegerQ[#], 0,
        True, #
        ] &,
      {svdRank, cdSVDRank}];
    PRINT["svdRank=", svdRank, " cdSVDRank=", cdSVDRank];
    
    If[! (TrueQ[impFunc == "Entropy"] || TrueQ[impFunc == "Gini"]),
     Message[BuildDecisionTree::fvalopt, "ImpurityFunction", "\"Gini\"|\"Entropy\""];
     Return[{}]
    ];
    impFunc = If[TrueQ[impFunc == "Entropy"], AVCEntropy, AVCGini];
    
    axesArg =
     Which[
      TrueQ[randomAxes] || IntegerQ[randomAxes] && randomAxes >= Length[data[[1]]] - 1,
      RandomAxes[Length[data[[1]]] - 1],
      IntegerQ[randomAxes],
      Sort[RandomSample[Range[Length[data[[1]]] - 1], randomAxes]],
      TrueQ[randomAxes === All] || TrueQ[! randomAxes],
      All,
      True,
      Message[BuildDecisionTree::fvalopt, "RandomAxes", "All|False|True|_Integer"];
      All
      ];
    
    If[! (IntegerQ[nStrata] && nStrata > 0), Message[BuildDecisionTree::ivalopt, "Strata"]; Return[{}]];
    
    If[! MatchQ[preStratifyQ, False | True], Message[BuildDecisionTree::fvalopt, "PreStratify", "False|True"]; Return[{}]];
    
    PRINT[
     "BuildTree:: ", {Max[minSizeTh, 1], axesArg, nStrata, impFunc, 
      impurityTh, {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels}, 
      preStratifyQ}];
    
    (* Recursive call *)
    
    BuildTreeRecStep[data, columnTypes, 0, minSizeTh, axesArg, 
     nStrata, impFunc, 
     impurityTh, {linCombMinRecs, linCombMaxRecs, svdRank, cdSVDRank, svdLabels}, 
     preStratifyQ]
    
    ] /; Length[data[[1]]] == Length[columnTypes];
BuildDecisionTree[data_, minSizeTh_: 1, opts : OptionsPattern[]] :=
  BuildDecisionTree[data, {minSizeTh, 0.}, opts] /; NumberQ[minSizeTh];
BuildDecisionTree[data_, {minSizeTh_Integer, impurityTh_?NumberQ}, opts : OptionsPattern[]] :=
  Block[{columnTypes},
   If[ !MatrixQ[ data, AtomQ],
     Message[BuildDecisionTree::farg];
     Return[{}];
   ];
   columnTypes = 
    Map[ VectorQ[ data[[All, #]], NumericQ ] &, Range[1, Length[ data[[1]]] ] ];
   columnTypes = columnTypes /. {True -> Number, False -> Symbol};
   BuildDecisionTree[data, columnTypes, {Max[minSizeTh, 1], impurityTh}, opts]
  ];

(* Forest *)
Clear[BuildDecisionForest]
BuildDecisionForest[data_, minSizeTh_Integer, n_Integer, opts : OptionsPattern[]] :=
   BuildDecisionForest[data, {minSizeTh, 0.}, n, opts];
BuildDecisionForest[data_, {minSizeTh_Integer, impurityTh_?NumberQ}, n_Integer, opts : OptionsPattern[]] :=
  Table[BuildDecisionTree[data, {minSizeTh, impurityTh}, Sequence @@ Append[{opts}, "RandomAxes" -> True]], {n}];

Clear[ParallelBuildDecisionForest]
ParallelBuildDecisionForest[data_, minSizeTh_Integer, n_Integer, opts : OptionsPattern[]] :=
  ParallelBuildDecisionForest[data, {minSizeTh, 0.}, n, opts];
ParallelBuildDecisionForest[data_, {minSizeTh_Integer, impurityTh_?NumberQ}, n_Integer, opts : OptionsPattern[]] :=
  ParallelTable[BuildDecisionTree[data, {minSizeTh, impurityTh}, Sequence @@ Append[{opts}, "RandomAxes" -> True]], {n}];

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
Options[DecisionForestClassify] = {"Weighted" -> True};
DecisionForestClassify[forest_, record_, opts : OptionsPattern[]] :=
  Block[{res, weightedQ = OptionValue[DecisionForestClassify, "Weighted"]},
   res = DecisionTreeClassify[#, record] & /@ forest;
   res = Flatten[res, 1];
   If[TrueQ[weightedQ],
    res = GatherBy[res, #[[2]] &];
    res = Map[{Total[#[[All, 1]]], #[[1, 2]]} &, res];
    SortBy[res, -#[[1]] &],
    (*ELSE*)
    SortBy[Reverse /@ Tally[res[[All, 2]]], -#[[1]] &]
    ]
   ];

Clear[ParallelDecisionForestClassify]
Options[ParallelDecisionForestClassify] = {"Weighted" -> False};
ParallelDecisionForestClassify[forest_, record_] := 
  Block[{res, weightedQ = OptionValue[ParallelDecisionForestClassify, "Weighted"]},
   res = ParallelMap[DecisionTreeClassify[#, record] &, forest, Method -> "CoarsestGrained", DistributedContexts -> Automatic];
   res = Flatten[res, 1];
   If[TrueQ[weightedQ],
    res = GatherBy[res, #[[2]] &];
    res = Map[{Total[#[[All, 1]]], #[[1, 2]]} &, res];
    SortBy[res, -#[[1]] &],
    (*ELSE*)
    SortBy[Reverse /@ Tally[res[[All, 2]]], -#[[1]] &]
    ]
   ];

(* Convert to rules *)

(* This function transforms a decision tree into rules to be given to GraphPlot and related functions. *)

Clear[MakeIDGenerator]
MakeIDGenerator[] :=
  Module[{i = 0},
   Clear[NewID, ResetID];
   NewID[] := i++;
   ResetID[] := (i = 0);
  ];

Clear[TreeToRulesRecStep, LeafNodeDecoration]
LeafNodeDecoration[node_] := If[MatrixQ[node], Column[{Row[{"leaf ", NewID[]}], Grid[node]}], node];
TreeToRulesRecStep[tree_] :=
  Which[
   tree === {}, {},
   Rest[tree] === {}, {},
   MatrixQ[tree] && Dimensions[tree][[2]] == 2, {},
   Length[Rest[tree]] == 2,
   Join[
    MapThread[
     Which[
       #2 == "True" && Length[tree[[1]]] >= 4 && (tree[[1, 4]] === Number || tree[[1, 4]] === Dot), 
         {tree[[1]] -> LeafNodeDecoration[#1[[1]]], "\[LessEqual] " <> ToString[NumberForm[tree[[1, 2]], 2]]},
       #2 == "False" && Length[tree[[1]]] >= 4 && (tree[[1, 4]] === Number || tree[[1, 4]] === Dot), 
         {tree[[1]] -> LeafNodeDecoration[#1[[1]]], "> " <> ToString[NumberForm[tree[[1, 2]], 2]]},
       #2 == "True" && Length[tree[[1]]] >= 4 && tree[[1, 4]] === Symbol, 
         {tree[[1]] -> LeafNodeDecoration[#1[[1]]], "= " <> ToString[tree[[1, 2]]]},
       #2 == "False" && Length[tree[[1]]] >= 4 && tree[[1, 4]] === Symbol, 
         {tree[[1]] -> LeafNodeDecoration[#1[[1]]], "\[NotEqual] " <> ToString[tree[[1, 2]]]},
       True, 
         {tree[[1]] -> LeafNodeDecoration[#1[[1]]], #2}
       ] &, {Rest[tree], {"True", "False"}}, 1],
    Flatten[TreeToRulesRecStep[#] & /@ Rest[tree], 1]
   ],
   True,
   Join[Map[{tree[[1]] -> LeafNodeDecoration[#1[[1]]]} &, Rest[tree], {1}], Flatten[TreeToRulesRecStep[#] & /@ Rest[tree], 1]]
  ];

Clear[DecisionTreeToRules]
DecisionTreeToRules[tree_] :=
  Block[{dtree, k=0},
    dtree = tree /. ({m_, v_, cInd_Integer, s_, n_} :> {m, v, cInd, s, n, "node "<>ToString[k++]});
    MakeIDGenerator[];
    TreeToRulesRecStep[dtree]
  ] /. {{r_Rule, edge_String} :> {r, Style[StandardForm[edge], Background -> White, FontSlant -> Plain]}};


(* Centralize data *)

(*
This function takes as arguments a data matrix and specified indexes of numerical columns. The data of each of the specified columns is translated at the median and dividied by the quartile distance. The returned result is the data matrix with centralized columns and the centralizing parameters, a list of pairs median and quartile distance.
It should be re-written using Mathematica's built-in function Standardize.
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
    If[qd > 0,
     data[[All, i]] = (data[[All, i]] - m)/qd,
     data[[All, i]] = (data[[All, i]] - m)
    ];
    AppendTo[centralizers, {m, qd}]
    , {i, inds}];
   {data, centralizers}
  ];

Clear[MedianCentralizeDataMatrix]
MedianCentralizeDataMatrix[dataArg_?MatrixQ] := MedianCentralizeDataMatrix[dataArg, All];
MedianCentralizeDataMatrix[dataArg_?MatrixQ, indsArg : ({_Integer ..} | All)] :=
  Block[{data = dataArg, m, qd, inds = indsArg, centralizers = {}},
   If[inds === All, inds = Range[Dimensions[data][[2]]]];
   Do[
    m = Median[data[[All, i]]];
    data[[All, i]] = (data[[All, i]] - m);
    AppendTo[centralizers, {m, 1}]
    , {i, inds}];
   {data, centralizers}
  ];

(* DecisionTreeClassificationSuccess *)

Clear[DecisionTreeOrForestClassificationSuccess, DecisionTreeClassificationSuccess, DecisionForestClassificationSuccess]
DecisionTreeOrForestClassificationSuccess[classFunc : (DecisionTreeClassify | DecisionForestClassify), dTreeOrForest_, dataArr_?MatrixQ] := DecisionTreeOrForestClassificationSuccess[classFunc, dTreeOrForest, dataArr, Union[dataArr[[All, -1]]]];
DecisionTreeOrForestClassificationSuccess[classFunc : (DecisionTreeClassify | DecisionForestClassify), dTreeOrForest_, dataArr_?MatrixQ, labels_?VectorQ, selectionFunc_: First] :=
  Block[{guesses, guessStats, tdata, t, dataLabels=Union[dataArr[[All, -1]]]},
   t =
    Table[
     If[ !MemberQ[ dataLabels, lbl],
     If[ TrueQ[classFunc === DecisionTreeClassify],
      Message[DecisionTreeClassificationSuccess::nlbl, lbl, dataLabels],
      Message[DecisionForestClassificationSuccess::nlbl, lbl, dataLabels]
     ];
     {0,0},
     tdata = Select[dataArr, #[[-1]] == lbl &];
      guesses = selectionFunc[classFunc[dTreeOrForest, Most[#]]][[2]] & /@ tdata;
      guessStats = MapThread[Equal, {guesses, tdata[[All, -1]]}];
      {Count[guessStats, True], Count[guessStats, False]}/Length[tdata] // N
     ]
     , {lbl, labels}];
   t = MapThread[{{#1, True} -> #2[[1]], {#1, False} -> #2[[2]]} &, {labels, t}];
   guesses = classFunc[dTreeOrForest, Most[#]][[1, 2]] & /@ dataArr;
   guessStats = MapThread[Equal, {guesses, dataArr[[All, -1]]}];
   Flatten[#, 1] &@ Join[t, {{All, True} -> (Count[guessStats, True]/Length[dataArr] // N), {All, False} -> (Count[guessStats, False]/Length[dataArr] // N)}]
  ];

DecisionTreeClassificationSuccess::nlbl = "The specified label `1` is not one of the data array labels `2`."
DecisionTreeClassificationSuccess::wsig = "The first two arguments are expected to be a decision tree and a data array.";
DecisionTreeClassificationSuccess[dTreeOrForest_, dataArr_?MatrixQ, x___] := 
  DecisionTreeOrForestClassificationSuccess[DecisionTreeClassify, dTreeOrForest, dataArr, x];
DecisionTreeClassificationSuccess[___]:=Message[DecisionTreeClassificationSuccess::wsig];

DecisionForestClassificationSuccess::nlbl = "The specified label `1` is not one of the data array labels `2`."
DecisionForestClassificationSuccess::wsig = "The first two arguments are expected to be a decision forest and a data array.";
DecisionForestClassificationSuccess[dTreeOrForest_, dataArr_?MatrixQ, x___] := 
  DecisionTreeOrForestClassificationSuccess[DecisionForestClassify, dTreeOrForest, dataArr, x];
DecisionForestClassificationSuccess[___]:=Message[DecisionForestClassificationSuccess::wsig];


(* Pruning *)

Clear[DecisionTreeNumberOfNodesAndLeaves, DecisionTreeNumberOfNodesAndLeavesRec]
DecisionTreeNumberOfNodesAndLeaves[dTree_] :=
  Block[{NNODES = 0, NLEAVES = 0},
   DecisionTreeNumberOfNodesAndLeavesRec[dTree];
   {NNODES, NLEAVES}
  ];
DecisionTreeNumberOfNodesAndLeavesRec[dTree_] :=
  Block[{},
   Which[
    MatrixQ[dTree] && Dimensions[dTree][[2]] == 2, NLEAVES++,
    MatrixQ[dTree[[1]]] && Dimensions[dTree[[1]]][[2]] == 2, NLEAVES++,
    True,
    NNODES++;
    DecisionTreeNumberOfNodesAndLeavesRec[dTree[[2]]];
    DecisionTreeNumberOfNodesAndLeavesRec[dTree[[3]]];
   ]
  ];

Clear[DecisionTreeLeafQ]
DecisionTreeLeafQ[leaf_] := 
  MatrixQ[leaf] && VectorQ[leaf[[All, 1]], NumberQ] && VectorQ[leaf[[All, 2]], Not[NumberQ[#]] &];

Clear[DecisionTreeLabels]
DecisionTreeLabels[dtree_] := 
  Union[Flatten[
    Cases[dtree, s_ /; DecisionTreeLeafQ[s], Infinity][[All, All, 2]]]];

Clear[DecisionTreeCombinedLeaves]
DecisionTreeCombinedLeaves[dtree_] :=
  Block[{leaves},
   leaves = Flatten[Cases[dtree, s_ /; DecisionTreeLeafQ[s], Infinity], 1];
   Map[{Total[#[[All, 1]]], #[[1, 2]]} &, GatherBy[leaves, #[[2]] &]]
  ];

Clear[MDLLeafCost, MDLLeavesCost]
MDLLeafCost[leaf_?DecisionTreeLeafQ, numberOfLabels_Integer] :=
  Block[{n = Total[leaf[[All, 1]]], k = numberOfLabels},
   Total[(#1*Log[n/#1] & ) /@ leaf[[All, 1]]] + ((k - 1)/2.)*Log[n/2.] + Log[Pi^(k/2)/Gamma[k/2]]
  ];
MDLLeafCost[dtree_, numberOfLabels_Integer] := MDLLeafCost[DecisionTreeCombinedLeaves[dtree], numberOfLabels];
MDLLeavesCost[dtree_, numberOfLabels_Integer] :=
  Block[{leaves},
   leaves = Cases[dtree, s_ /; DecisionTreeLeafQ[s], \[Infinity]];
   Total[MDLLeafCost[#, numberOfLabels] & /@ leaves]
  ];

Clear[DecisionTreeNValuesPerIndex];
DecisionTreeNValuesPerIndex[dtree_] :=
  Block[{t, tn, ts, td, res = 0},
   t = Cases[dtree, s : {___, Symbol | Number | Dot, ___} /; Length[s] == 5, Infinity];
   tn = GatherBy[Select[t, #[[4]] === Number &], #[[3]] &];
   ts = GatherBy[Select[t, #[[4]] === Symbol &], #[[3]] &];
   td = GatherBy[Select[t, #[[4]] === Dot &], #[[3, 1]] &];
   Dispatch[Join[
     Map[{#[[1, 3]], Number} -> Length[Union[#[[All, 2]]]] &, tn],
     Map[{#[[1, 3]], Symbol} -> Length[Union[#[[All, 2]]]] &, ts],
     Map[{#[[1, 3, 1]], Dot} -> {Length[Union[Flatten[#[[All, 3, 2]]]]], 
         Length[Union[#[[All, 2]]]]} &, td]
     ]]
  ];

Clear[MDLSplitCost]
MDLSplitCost[dtree_, varIndexToNValuesRules_Dispatch] :=
  Block[{res},
   res =
    Which[
     MatchQ[dtree[[1, 4]], Number | Symbol],
     Log[((dtree[[1, 3 ;; 4]] /. varIndexToNValuesRules) - 1) /. {0 -> 1}] + Log[Length[varIndexToNValuesRules[[1]]] - 1],
     MatchQ[dtree[[1, 4]], Dot],
     Log[Total[{dtree[[1, 3, 1]], Dot} /. varIndexToNValuesRules] - 1] + 
      Log[2^(Length[Select[varIndexToNValuesRules[[1]], #[[1, 2]] === Number &]] - 1)],
     True,
     (* Fail-safe but should not happen*) 
     Print["MDLSplitCost::internal error!"];
     0
    ];
   N[res + 1]
  ];

Clear[PruneDecisionTree, MDLPruneDecisionTreeRec]
Options[PruneDecisionTree] = {Method -> "MDL"};
PruneDecisionTree[dtree_, opts : OptionsPattern[]] :=
  Block[{resTree, cost},
   {resTree, cost} = 
    MDLPruneDecisionTreeRec[dtree, Length[DecisionTreeLabels[dtree]], DecisionTreeNValuesPerIndex[dtree]];
   resTree
  ];
MDLPruneDecisionTreeRec[dtree_, numberOfLabels_Integer, varIndexToNValuesRules_] :=
  Block[{leftSubTree, leftCost, rightSubTree, rightCost, tAsLeafCost, tSplitCost},
   If[
    DecisionTreeLeafQ[dtree[[1]]],
    (* we are at a leaf *)
    {dtree, MDLLeafCost[dtree[[1]], numberOfLabels]},
    (*ELSE*)
    tSplitCost = MDLSplitCost[dtree, varIndexToNValuesRules];
    tAsLeafCost = MDLLeafCost[dtree, numberOfLabels];
    {leftSubTree, leftCost} = 
     MDLPruneDecisionTreeRec[dtree[[2]], numberOfLabels, varIndexToNValuesRules];
    {rightSubTree, rightCost} = 
     MDLPruneDecisionTreeRec[dtree[[3]], numberOfLabels, varIndexToNValuesRules];
    If[tAsLeafCost + 1 <= tSplitCost + 1 + leftCost + rightCost,
     (*Prune child nodes*)
     leftSubTree = DecisionTreeCombinedLeaves[dtree];
     {{leftSubTree}, tAsLeafCost + 1},
     (*ELSE*)
     {{dtree[[1]], leftSubTree, rightSubTree}, tSplitCost + 1 + leftCost + rightCost}
    ]
   ]
  ];

End[]

EndPackage[]