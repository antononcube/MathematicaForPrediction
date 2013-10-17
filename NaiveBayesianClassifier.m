(*
    Implementation of naive Bayesian classifier generation in Mathematica
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
    Mathematica is (C) Copyright 1988-2013 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 0.8 *)
(* This package contains definitions for generation of naive Bayesian classifiers. *)
(* I am not sure that this is best desgin of functionality and signatures. It is in my TODO list to review the design and write functions that are better of handling unexpected arguments. Another TODO task is the ability to specify different distribution approximation ranges for each variable. Right now the ranges are specified for all -- see the range argument in the function definitions below. *)

BeginPackage["NaiveBayesianClassifier`"]

MakeBayesianClassifier::usage = "MakeBayesianClassifier[dataArg_?ArrayQ, labelArg_, rangeArg : (Automatic | {_?NumberQ, _?NumberQ}), nbins_Integer] makes probability function of the label labelArg by approximating the variable distributions using rangeArg and nbins."

MakeBayesianClassifiers::usage = "MakeBayesianClassifiers[dataArg_?ArrayQ, nbins_Integer] makes Bayesian functions for each label in the argument dataArg by approximating the variable distributions using nbins number of bins."

NBCClassify::usage = "NBCClassify[{cf_,cfLabel_}, {ncf_,ncfLabel_}, thA_?NumberQ, thNA_?NumberQ, x_?VectorQ, inds:(All|{_Integer..})] applies two piece-wise functions, cf and ncf, derived from data with two labels {cfLabel,ncfLabel} to x[[inds]]. The values cf[x[[inds]]] and ncf[x[[inds]]] are for the probabilities to get cfLabel and ncfLabel respectively. The tuning parameters thA and thNA are used to decide is the overall classification result -- see the function definition."

NBCClassificationStatistics::usage = "NBCClassificationStatistics[{cf_,cfLabel_}, {ncf_,ncfLabel_}, thA_?NumberQ, thNA_?NumberQ, testData_?ArrayQ, inds:(All|{_Integer..})] computes statistics for the performance of a naive Bayesian classifier made of cf and ncf over test data. The function NBCClassify is used internally."

Begin["`Private`"]


(* This function computes P(X=x/C[i])/P(X=x). *)
Clear[MakeBayesianFunction]
MakeBayesianFunction[data : {_?NumberQ ..}, allData : {_?NumberQ ..}, range : (Automatic | {_?NumberQ, _?NumberQ}), nbins_Integer] :=
  Block[{ds, bcs, allbcs, func},
   If[TrueQ[range === Automatic],
    ds = FindDivisions[{Min[data], Max[data]}, nbins],
    ds = FindDivisions[range, nbins]
   ];
   bcs = BinCounts[data, {ds}];
   allbcs = BinCounts[allData, {ds}];

   (* P(X=x/Ci)*)
   bcs = N[bcs/Length[data]];

   (* P(X=x)*)
   allbcs = N[allbcs/Length[allData]];

   func = 
   Piecewise[
     MapThread[
      Function[{int, c, ac}, {If[ac > 0, c/ac, 0], int[[1]] <= # < int[[2]]}], 
      {Partition[ds, 2, 1], bcs, allbcs}
     ]
   ];
   With[{f = func}, f &]
  ];

MakeBayesianFunction[data : {_String ..}, allData : {_String ..}, dummy___] :=
   MakeBayesianFunction[data, allData];

MakeBayesianFunction[data : {_String ..}, allData : {_String ..}] :=  
  Block[{rules, allRules, func},

   (* P(X=x/C[i])*)   
   rules = Append[Rule @@@ Tally[data], _ -> 0];
   rules[[All, 2]] = rules[[All, 2]]/Length[data];

   (* P(X=x)*)
   allRules = Append[Rule @@@ Tally[allData], _ -> 0];
   allRules[[All, 2]] = allRules[[All, 2]]/Length[allData];

   func = 
   Piecewise[
     Map[
      Function[{v}, {If[(v /. allRules) > 0, N[(v /. rules)/(v /. allRules)], 0], # == v}], 
      Most[rules[[All, 1]]]
     ]
   ];
   With[{f = func}, f &]
  ];

(* This function takes P(X=x/C[i])/P(X=x) and multiplies it with P(C[i]). 
   Therefore, we get
   P(C[i]/X=x)=(P(X=x/C[i])P(C[i]))/P(X=x)=(P(X=x\[Intersection]C[i])/P(X=x)). 
   The last column of the array argument data is made of labels. 
   The argument label is one of the labels in data[[All,-1]] .
   The returned result is a function to be applied to a vector of the same type as data[[1,1;;-2]] .
*)
Clear[MakeBayesianClassifier]
MakeBayesianClassifier[data_?ArrayQ, label_, range : (Automatic | {_?NumberQ, _?NumberQ}), nbins_Integer] :=
  Block[{funcs, ldata},
   ldata = Select[data, #[[-1]] == label &];
   funcs = 
    MapThread[
     MakeBayesianFunction[#1, #2, range, nbins] &, 
     {Most@Transpose[ldata], Most@Transpose[data]}];
   With[{fs = funcs, factor = N[Length[ldata]/Length[data]]}, 
    factor*Apply[Times, MapThread[#1[#2] &, {fs, #}]] &]
  ];

Clear[MakeBayesianClassifiers]
MakeBayesianClassifiers[data_?ArrayQ, nbins_Integer] :=  
  Block[{labels, t, funcs},
   labels = Union[data[[All, -1]]];
   funcs = Map[Function[{l}, MakeBayesianClassifier[data, l, Automatic, nbins]], labels];
   Thread[labels -> funcs]
  ];

(* This function is for NBC classification of over data with two labels {False,True}.
   For using the NBC functions made with MakeBayesianClassifier(s) over data with more labels 
   other classification functions have to made.

   The argument cf is for True, the argument ncf is for False.
   The arguments thA and thNA are threshold parameters.
   The argument x is data record.
   The argument inds is used to take elements of x.
   The functions cf and ncf are applied to x[[inds]]. *)
Clear[NBCClassify]
NBCClassify[cf_, ncf_, thA_?NumberQ, thNA_?NumberQ, x_?VectorQ, inds:(All|{_Integer..}):All] :=
  NBCClassify[{cf,True}, {ncf,False}, thA, thNA, x, inds];
NBCClassify[{cf_,cfLabel_}, {ncf_,ncfLabel_}, thA_?NumberQ, thNA_?NumberQ, x_?VectorQ, inds:(All|{_Integer..}):All] :=
  Block[{rcf, rncf},
   {rcf, rncf} = {cf[x[[inds]]], ncf[x[[inds]]]};
   Which[
    rcf >= thA || (1 - rncf) >= thNA, cfLabel, 
    rncf > 0.5, ncfLabel, 
    rcf > rncf, cfLabel,
    True, ncfLabel
   ]
  ];

Clear[NBCClassificationStatistics]
NBCClassificationStatistics[cf_, ncf_, thA_?NumberQ, thNA_?NumberQ, testData_?ArrayQ, inds:(All|{_Integer..}):All] := 
  NBCClassificationStatistics[{cf,True}, {ncf,False}, thA, thNA, testData, inds];
NBCClassificationStatistics[{cf_,cfLabel_}, {ncf_,ncfLabel_}, thA_?NumberQ, thNA_?NumberQ, testData_?ArrayQ, inds:(All|{_Integer..}):All] :=  
  Block[{res, ncAll, ncTrue, ncFalse, data},
   
   data = testData;
   res = NBCClassify[{cf,cfLabel},{ncf,ncfLabel},thA,thNA,#,inds]& /@ data;
   ncAll = Count[MapThread[Equal,{res,data[[All,-1]]}],True];
   ncAll = ncAll/Length[data];
   
   data = Select[testData,#[[-1]]==cfLabel&];
   res = NBCClassify[{cf,cfLabel},{ncf,ncfLabel},thA,thNA,#,inds]& /@ data;
   ncTrue = Count[MapThread[Equal,{res,data[[All,-1]]}],True];
   ncTrue = ncTrue/Length[data];
   
   data = Select[testData,#[[-1]]==ncfLabel&];
   res = NBCClassify[{cf,cfLabel},{ncf,ncfLabel},thA,thNA,#,inds]& /@ data;
   ncFalse = Count[MapThread[Equal,{res,data[[All,-1]]}],True];
   ncFalse = ncFalse/Length[data];
   
   Transpose[{
    {"all records", Row[{cfLabel, " records"}], Row[{ncfLabel, " records"}]},
    N@{ncAll, ncTrue, ncFalse}}]
  ];

End[]

EndPackage[]