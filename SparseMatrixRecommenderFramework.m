(*
    Sparse matrix recommender framework in Mathematica
    Copyright (C) 2014-2016  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2016 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 1.0 *)
(*
   The framework is described in the presentation

   [1] Anton Antonov, "A fast and agile item-item recommender : design and implementation", October, WTC 2011.
       URL: http://library.wolfram.com/infocenter/Conferences/7964/

   The presentation is also available at MathematicaForPrediction project at GitHub:

   [2] https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Antonov%20WTC-2011%20-%20A%20fast%20and%20agile%20IIR.pdf

   NOTE, that there is a dependency on the package OutlierIdentifiers.m:

   [3] Anton Antonov, "Implementation of one dimensional outlier identifying algorithms in Mathematica", Mathematica package, (2013).
       URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m .

   The package [3] is imported below (using Import[...]).

   *)

(* What is in the framework

   1. Hooks
   1.1. Sparse Matrix creators plug-ins
   1.1.1. Constructors
   1.1.2. Weights calculation
   1.1.3. Weights parameters
   1.2. Data
   1.2.1. Data interpretation
   1.3. Sparse Matrix Column interpretation to be used for ad-hoc profiles (search based, cold-start based)
   1.4. Post-processing
   1.4.1. Proofs
   1.4.2. Distances
   1.4.3. Ratings
   1.4.4. Re-orderings


   2. Algorithms
   2.1. Basic recommendation algorithm
   2.1.1. Parameters: user history, number of results
   2.2. Recommendatons for a list of profile items
   2.2.1. Takes number of results and profile vector or profile items list
   2.3. Post-processing
   2.3.1. Proofs -- common tags
   2.3.2. Ratings --
   2.3.3. Carousels --

   3. Interface
   3.1. Consumed items
   3.2. Recommended items
   3.3. User profile
   3.4. Controls for weghts calculation


   4. Major design decisions
   4.1. Construct M01 and make M as M=M01.W .
   4.2. After initialization, column interpreatation is always the same (because we keep M01).
   4.3. Template method DP for the basic recommendation alorithm.
   4.4. Strategy DP for matrix construction.
   4.5. Observers for the interface parts.
   4.6. Using rules to map tags to indexes and map local indexes to column indexes in M01.
*)

(* ::Section:: *)
(*Item recommender object definitions*)


(* ::Subsection:: *)
(*Data symbols*)


(* ::Subsubsection:: *)
(*Core recommender*)


(* ::Program:: *)
(*M01*)
(*M*)
(*W*)
(*weightsVector*)
(*metaDataNames*)
(*columnIndexToTagRules*)
(*tagToColumnIndexRules*)
(*tagTypes*)


(* ::Subsubsection:: *)
(*Interface*)


(* ::Program:: *)
(*recommendationDataIndexes*)
(*consumedDataIndexes*)
(*automaticWeights*)
(*useOutliers*)
(*panelWidth*)
(*panelHight*)


(* ::Subsection:: *)
(*ItemRecommender*)


Clear[ItemRecommender]

If[Length[DownValues[SSparseMatrix`MakeSSparseMatrix]] == 0,
  Echo["Importing SSparseMatrix.m from GitHub...", "SparseMatrixRecommenderFramework:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/SSparseMatrix.m"]
  Echo["...Done.", "SparseMatrixRecommenderFramework:"]
];

If[Length[DownValues[CrossTabulate`CrossTabulate]] == 0,
  Echo["Importing CrossTabulate.m from GitHub...", "SparseMatrixRecommenderFramework:"];
  Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m"]
  Echo["...Done.", "SparseMatrixRecommenderFramework:"]
];


(*=========================================================*)
(* Creation                                                *)
(*=========================================================*)

Clear[ItemRecommenderCreation]

ItemRecommenderCreation::rneq = "The row names of SSparseMatrix objects are not the same."
ItemRecommenderCreation::niid = "The specified item variable name is not one of the column names of the dataset."

ItemRecommenderCreation[id_String, smats : {_SparseArray ..},
  tagTypeNames : {_String ..}, rowNames : {_String ..},
  columnNames : {{_String ..} ..}] :=
    Module[{objIR},
      objIR = ItemRecommender[id];
      objIR["itemNames"] = rowNames;
      objIR["tagTypes"] = tagTypeNames;
      objIR["M01"] = objIR["SpliceMatrices"][smats];
      objIR["MakeColumnInterpretation"][tagTypeNames, columnNames];
      objIR["UseTagTypeWeights"][ConstantArray[1, Length[tagTypeNames]]];
      objIR
    ] /; Length[smats] == Length[tagTypeNames] == Length[columnNames];


ItemRecommenderCreation[id_String, smats : Association[ (_ -> _SSparseMatrix) ..]] :=
    Block[{tagTypeNames, rowNames, columnNames},

      tagTypeNames = Keys[smats];

      rowNames = RowNames /@ Values[smats];

      If[ !(Equal @@ rowNames),
        Message[ItemRecommenderCreation::rneq];
        Return[$Failed]
      ];

      rowNames = First[rowNames];

      columnNames = ColumnNames /@ Values[smats];

      ItemRecommenderCreation[id, SparseArray /@ Values[smats], tagTypeNames, rowNames, columnNames ]
    ];

ItemRecommenderCreation[id_String, ds_Dataset, itemVarName_String ] :=
    Block[{ncol, varNames, smats, idPos},

      ncol = Dimensions[ds][[2]];

      varNames = Normal[Keys[ds[1]]];

      idPos = Flatten[Position[varNames, itemVarName]];
      If[ Length[idPos]==0,
        Message[ItemRecommenderCreation::niid];
        Return[$Failed]
      ];
      idPos = First[idPos];

      smats = ToSSparseMatrix /@
          Table[CrossTabulate[ds[All, {idPos, i}]], {i, Complement[Range[ncol], {idPos}]}];

      smats = AssociationThread[Flatten[List @@ Complement[varNames,{itemVarName}]] -> smats];

      ItemRecommenderCreation[id, smats]
    ];

Clear[MakeItemRecommender]
MakeItemRecommender = ItemRecommenderCreation;


(* Makes the item-tag sparse martix \[Element] {0,1}^(Subscript[n, items]*Subscript[n, tags]) *)
ItemRecommender[d___]["MakeM01"][args___]:=ItemRecommender[d]["M01"]=SparseArray[RandomInteger[{0,1},{100,20}]];
ItemRecommender[d___]["MakeBasicSparseMatrix"][args___]:=ItemRecommender[d]["MakeM01"][args];


(* Does not check for consistency witth M01.
I.e. make sure that M01.W should give a matrix (the M matrix) *)
ItemRecommender[d___]["MakeW"][weights_?VectorQ]:=
    Block[{},
      ItemRecommender[d]["W"]=SparseArray[DiagonalMatrix[weights]]
    ];
ItemRecommender[d___]["MakeWeightMatrix"][weights:{_?NumberQ...}]:=ItemRecommender[d]["MakeW"];


ItemRecommender[d___]["UseWeights"][weights_?VectorQ]:=
    Block[{},
      ItemRecommender[d]["MakeW"][weights];
      ItemRecommender[d]["M"]=ItemRecommender[d]["M01"].ItemRecommender[d]["W"]
    ]/;Length[weights]==Dimensions[ItemRecommender[d]["M01"]][[2]];


ItemRecommender[d___]["UseTagTypeWeights"][tagTypeWeights:{_?NumberQ...}]:=
    Block[{weights},
      weights=Flatten@
          Table[
            ConstantArray[tagTypeWeights[[i]],
              ItemRecommender[d]["tagTypeIndexOffsets"][[i+1]]-ItemRecommender[d]["tagTypeIndexOffsets"][[i]]
            ],{i,Length[tagTypeWeights]}];
      weights=SparseArray[weights];
      ItemRecommender[d]["UseWeights"][weights];
    ]/;Length[tagTypeWeights]==Length[ItemRecommender[d]["tagTypes"]];


ItemRecommender[d___]["MakeColumnInterpretation"][tagType_String,tags_List]:=
    Block[{},
      ItemRecommender[d]["ColumnInterpretationLocal"][tagType]=tags;
      ItemRecommender[d]["ColumnIndexToTagRulesLocal"][tagType]=Dispatch[Thread[Range[1,Length[tags]]->tags]];
      ItemRecommender[d]["TagToColumnIndexRulesLocal"][tagType]=Dispatch[Thread[tags->Range[1,Length[tags]]]];
    ];


ItemRecommender[d___]["MakeColumnInterpretation"][tagTypes:{_String...},tagsLists:{_List...}]:=
    Block[{len=Total[Length/@tagsLists],offsets,ttlens=Length/@tagsLists},
      ItemRecommender[d]["tagTypes"]=tagTypes;
      (* assign local interpretations *)
      MapThread[ItemRecommender[d]["MakeColumnInterpretation"][#1,#2]&,{tagTypes,tagsLists}];
      (* assign global interpreation*)
      ItemRecommender[d]["ColumnInterpretation"]=Join@@tagsLists;
      ItemRecommender[d]["columnIndexToTagRules"]=Dispatch[Thread[Range[1,len]->Flatten[tagsLists,1]]];
      ItemRecommender[d]["tagToColumnIndexRules"]=Dispatch[Thread[Flatten[tagsLists,1]->Range[1,len]]];
      (* name alias *)
      ItemRecommender[d]["columnIndexInterpretationRules"]:=ItemRecommender[d]["columnIndexToTagRules"];
      (* global-local column indexes rules *)
      offsets=FoldList[Plus,0,Length/@tagsLists];
      ItemRecommender[d]["tagTypeIndexOffsets"]=offsets;
      ItemRecommender[d]["tagTypeRanges"] = AssociationThread[tagTypes, Transpose[{Most[offsets]+1, Rest[offsets]}]];
      ItemRecommender[d]["TypeFunction"]=With[{w=Which@@Flatten[Map[Function[{arg},{#>arg[[1]],arg[[2]]}],Transpose[{Rest[Reverse@offsets],Reverse@tagTypes}]]]},
        Function[w]
      ];
      Do[
        ItemRecommender[d]["localIndexToGlobalIndexRules"][tagTypes[[i]]]=Dispatch[Thread[Range[1,ttlens[[i]]]->Range[offsets[[i]]+1,offsets[[i]]+ttlens[[i]]]]],
        {i,1,Length[tagTypes]}];
    ]/;Length[tagTypes]==Length[tagsLists];


ItemRecommender[d___]["MakeRowInterpretation"][rowIDs:{(_Integer|_String)..}]:=
    Block[{},
      ItemRecommender[d]["RowIDs"]=rowIDs;
      ItemRecommender[d]["RowIDToIndexRules"]=Dispatch[Thread[rowIDs->Range[Dimensions[ItemRecommender[d]["M01"]][[1]]]]];
      ItemRecommender[d]["RowIndexToIDRules"]=Dispatch[Thread[Range[Dimensions[ItemRecommender[d]["M01"]][[1]]]->rowIDs]];
    ]/;Dimensions[ItemRecommender[d]["M01"]][[1]]==Length[rowIDs];


ItemRecommender[d___]["SetFilterMatrix"][fmat_?MatrixQ,indexMappingRules_Dispatch]:=
    Block[{},
      ItemRecommender[d]["F"]=fmat;
      ItemRecommender[d]["globalIndexToFilterIndexRules"]=indexMappingRules;
    ]/;Dimensions[ItemRecommender[d]["M01"]][[1]]==Dimensions[fmat];


ItemRecommender[d___]["SetFilterMatrix"][filterTagType_String]:=
    Block[{indexMappingRules},
      indexMappingRules=ItemRecommender[d]["localIndexToGlobalIndexRules"][filterTagType];
      indexMappingRules=Reverse/@Normal[indexMappingRules];
      ItemRecommender[d]["F"]=Transpose[ItemRecommender[d]["M01"][[All,indexMappingRules[[All,1]]]]];
      ItemRecommender[d]["globalIndexToFilterIndexRules"]=Dispatch[indexMappingRules];
    ]/;MemberQ[ItemRecommender[d]["tagTypes"],filterTagType];


(*=========================================================*)
(* SpliceMatrices                                          *)
(*=========================================================*)

ItemRecommender[d___]["SpliceMatrices"][smats:{_SparseArray...}]:=
    ItemRecommender[d]["SpliceMatrices"][smats,ConstantArray[1,Length[smats]]];
ItemRecommender[d___]["SpliceMatrices"][smats:{_SparseArray...},weights:{_?NumberQ...}]:=
    Block[{},
      SparseArray[
        MapThread[Join,MapThread[#1*#2&,{smats,weights}]]
      ]
    ]/;Length[smats]==Length[weights]==Length[ItemRecommender[d]["tagTypes"]];



(*=========================================================*)
(* Recommendations                                         *)
(*=========================================================*)

(* ::Text:: *)
(*The formulas we use for the version below are:*)

(* ::DisplayFormula:: *)
(*S=M Transpose[M]*)
(*v S=S v=M (Transpose[M] v)=M (v M)*)


(* ::Text:: *)
(*This is a a specification of the algorithm below in Recommendations.*)


(* ::Text:: *)
(*1. Make a vector v\[Element]\[DoubleStruckCapitalR]^Subscript[n, s] that has zeros everywhere except at the coordinates corresponding to showInds. The values at those coordinates are determined by ratings. *)
(*2. Do the two matrix-vector multiplications s:=M (v M), M\[Element]\[DoubleStruckCapitalR]^(Subscript[n, s]* Subscript[n, md]). The vector s is called the score vector for showInds or v. (Also the formula s:=(M M^T)v can be used, but M M^T might be large and dense.)*)
(*3. Make the list of pairs {Subscript[s, i],i},i\[Element][1,...,Subscript[n, s]], and name that list sp.*)
(*4. Sort descendingly sp according to the first value of each pair. *)
(*5. Take the indexes of the first k pairs, i.e. take sp(1:k,2).*)

ItemRecommender[d___]["UserItemsToLove"][args___] := ItemRecommender[d]["Recommendations"][args];

ItemRecommender[d___]["Recommendations"][inputShowInds:{_Integer...},inputRatings:{_?NumberQ...},nRes_Integer, removeHistoryQ_:True]:=
    Block[{inds,vec,maxScores,smat=ItemRecommender[d]["M"]},
    (*userMatrix=MapThread[#1*#2&,{smat\[LeftDoubleBracket]inputShowInds\[RightDoubleBracket],inputRatings},1];*)
    (*1*)
      vec=SparseArray[Thread[inputShowInds->inputRatings],{smat//Length}];
      (*2*)
      vec=smat.(vec.smat);
      (*3 and 4 and 5*)
      inds=Reverse@Ordering[vec//Normal];
      If[TrueQ[removeHistoryQ],
        If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],inds}],#[[1]]>0&&!MemberQ[inputShowInds,#[[2]]]&]],
        If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],inds}],#[[1]]>0&]]
      ]
    ]/;Length[inputShowInds]==Length[inputRatings];
ItemRecommender[d___]["Recommendations"][{},_,nRes_Integer]:=
    Block[{inds=Range[1,nRes]},Transpose[{ConstantArray[1,{Length[inds]}],inds}]];


ItemRecommender[d___]["UserItemsToLoveByProfile"][args___] := ItemRecommender[d]["RecommendationsByProfile"][args];

ItemRecommender[d___]["RecommendationsByProfile"][profileInds:{_Integer...},profileScores:{_?NumberQ...},nRes_Integer]:=
    Block[{inds,vec,smat=ItemRecommender[d]["M"]},
      vec=SparseArray[Thread[profileInds->profileScores],{Dimensions[smat][[2]]}];
      vec=smat.vec;
      inds=Reverse@Ordering[vec//Normal];
      (*If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec\[LeftDoubleBracket]inds\[RightDoubleBracket],inds}],#\[LeftDoubleBracket]1\[RightDoubleBracket]>0&]]*)
      If[Length[#]>nRes,Take[#,nRes],#]&[DeleteCases[Transpose[{vec[[inds]],inds}],{0.,_}]]
    ]/;Length[profileInds]==Length[profileScores];
ItemRecommender[d___]["RecommendationsByProfile"][profileVec_SparseArray,nRes_Integer]:=
    Block[{inds,vec,smat=ItemRecommender[d]["M"]},
      vec=smat.profileVec;
      (*inds=Most[ArrayRules[vec]]\[LeftDoubleBracket]All,1,1\[RightDoubleBracket];
If[Length[#]>nRes,Take[#,nRes],#]&[SortBy[Select[Transpose[{vec\[LeftDoubleBracket]inds\[RightDoubleBracket],inds}],#\[LeftDoubleBracket]1\[RightDoubleBracket]>0&],-#\[LeftDoubleBracket]1\[RightDoubleBracket]&]]*)
      inds=Reverse@Ordering[vec//Normal];
      (*If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],inds}],#[[1]]>0&]];*)
      If[Length[#]>nRes,Take[#,nRes],#]&[DeleteCases[Transpose[{vec[[inds]],inds}],{0.,_}]]
    ]/;Dimensions[ItemRecommender[d]["M"]][[2]]==Dimensions[profileVec][[1]];


(* The code is almost identical to the definition above *)
ItemRecommender[d___]["RecommendationsByProfile"][rowInds:{_Integer..},profileInds:{_Integer...},profileScores:{_?NumberQ...},nRes_Integer]:=
    Block[{inds,vec,smat=ItemRecommender[d]["M"][[rowInds]]},
      vec=SparseArray[Thread[profileInds->profileScores],{Dimensions[smat][[2]]}];
      vec=smat.vec;
      inds=Reverse@Ordering[vec//Normal];
      If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],inds}],#[[1]]>0&]]
    ]/;Length[profileInds]==Length[profileScores];
ItemRecommender[d___]["RecommendationsByProfile"][rowInds:{_Integer..},profileVec_SparseArray,nRes_Integer]:=
    Block[{inds,vec,smat=ItemRecommender[d]["M"][[rowInds]]},
      vec=smat.profileVec;
      inds=Reverse@Ordering[vec//Normal];
      (*If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],inds}],#[[1]]>0&]];*)
      If[Length[#]>nRes,Take[#,nRes],#]&[DeleteCases[Transpose[{vec[[inds]],inds}],{0.,_}]]
    ]/;Dimensions[ItemRecommender[d]["M"]][[2]]==Dimensions[profileVec][[1]];


(*=========================================================*)
(* RecommendationsByProfile with filtering                 *)
(*=========================================================*)

ItemRecommender[d___]["UserItemsToLoveByFilterAndProfile"][args___] := ItemRecommender[d]["RecommendationsByFilterAndProfile"][args];

ItemRecommender[d___]["RecommendationsByFilterAndProfile"][filterInds:{_Integer..},profileInds:{_Integer...},profileScores:{_?NumberQ...},nRes_Integer,strictQ:(True|False)]:=
    Block[{finds,inds,vec,maxScores,smat,fmat=ItemRecommender[d]["F"],indexArray,tf=Length[filterInds]},
      finds=filterInds/.ItemRecommender[d]["globalIndexToFilterIndexRules"];
      vec=fmat.DiagonalMatrix[SparseArray[Thread[finds->1.],{Dimensions[fmat][[1]]}]];
      If[TrueQ[strictQ],vec=SparseArray[Clip[vec,{tf,tf},{0.,1.}]]];
      indexArray=Most[ArrayRules[vec]][[All,1,1]];
      smat=ItemRecommender[d]["M"][[Most[ArrayRules[vec]][[All,1,1]]]];
      vec=SparseArray[Thread[profileInds->profileScores],{Dimensions[smat][[2]]}];
      vec=smat.vec;
      inds=Reverse@Ordering[vec//Normal];
      If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],indexArray[[inds]]}],#[[1]]>0&]]
    ]/;Length[profileInds]==Length[profileScores]&&MatrixQ[ItemRecommender[d]["F"]]&&Dimensions[ItemRecommender[d]["F"]][[2]]==Dimensions[ItemRecommender[d]["M"]][[1]];


ItemRecommender[d___]["RecommendationsByFilterAndProfile"][filterVec_SparseArray,profileVec_SparseArray,nRes_Integer,strictQ:(True|False)]:=
    Block[{finds,inds,vec,maxScores,smat,fmat=ItemRecommender[d]["F"],indexArray,tf=Total[filterVec]},
      finds=Most[ArrayRules[filterVec]][[All,1,1]]/.ItemRecommender[d]["globalIndexToFilterIndexRules"];
      vec=SparseArray[Thread[finds->1.],{Dimensions[fmat][[1]]}].fmat;
      If[TrueQ[strictQ],vec=SparseArray[Clip[vec,{tf,tf},{0.,1.}]]];
      indexArray=Most[ArrayRules[vec]][[All,1,1]];
      smat=ItemRecommender[d]["M"][[indexArray]];
      vec=smat.profileVec;
      inds=Reverse@Ordering[vec//Normal];
      If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],indexArray[[inds]]}],#[[1]]>0&]]
    ]/;Dimensions[filterVec][[1]]==Dimensions[profileVec][[1]]==Dimensions[ItemRecommender[d]["M"]][[2]]&&MatrixQ[ItemRecommender[d]["F"]]&&Dimensions[ItemRecommender[d]["F"]][[2]]==Dimensions[ItemRecommender[d]["M"]][[1]];


(* Not an universal solution and it is only few %'s faster than the above *)
(*ItemRecommender[d___]["RecommendationsByFilterAndProfile"][filterVec_SparseArray,profileVec_SparseArray,nRes_Integer]:=
Block[{finds,inds,vec,maxScores,smat,fmat=ItemRecommender[d]["F"],indexRules,indexArray},
vec=filterVec[[ItemRecommender[d]["tagTypeIndexOffsets"][[2]]+1;;ItemRecommender[d]["tagTypeIndexOffsets"][[2+1]]]];
vec=vec.fmat;
vec=SparseArray[Clip[vec,{tf,tf},{0.,1.}]];
(*indexRules=Dispatch[MapIndexed[#2[[1]]->#[[1,1]]&,Most[ArrayRules[vec]]]];*)
(*indexRules=Dispatch[Thread[Range[Length[#]]->#]]&@Most[ArrayRules[vec]][[All,1,1]];*)
(*indexRules=Dispatch[Rule@@@Transpose[{Range[Length[#]],#}]]&@Most[ArrayRules[vec]][[All,1,1]];*)
indexArray=Most[ArrayRules[vec]][[All,1,1]];
smat=ItemRecommender[d]["M"][[indexArray]];
vec=smat.profileVec;
inds=Reverse@Ordering[vec//Normal];
If[Length[#]>nRes,Take[#,nRes],#]&[Select[Transpose[{vec[[inds]],indexArray[[inds]]}],#[[1]]>0&]]
]/;Dimensions[filterVec][[1]]==Dimensions[profileVec][[1]]==Dimensions[ItemRecommender[d]["M"]][[2]]&&MatrixQ[ItemRecommender[d]["F"]]&&Dimensions[ItemRecommender[d]["F"]][[2]]==Dimensions[ItemRecommender[d]["M"]][[1]];*)

(*=========================================================*)
(* UserProfile                                             *)
(*=========================================================*)

ItemRecommender[d___]["UserProfileVector"][args___] := ItemRecommender[d]["ProfileVector"][args];

ItemRecommender[d___]["ProfileVector"][inputShowInds:{_Integer...}]:=
    ItemRecommender[d]["ProfileVector"][inputShowInds,ConstantArray[1.,Length[inputShowInds]]];
ItemRecommender[d___]["ProfileVector"][inputShowInds:{_Integer...},inputRatings:{_?NumberQ...}]:=
    Block[{vec,smat=ItemRecommender[d]["M"]},
      vec=SparseArray[Thread[inputShowInds->inputRatings],{smat//Length}];
      vec.smat
    ]/;Length[inputShowInds]==Length[inputRatings];


ItemRecommender[d___]["UserProfile"][args___] := ItemRecommender[d]["Profile"][args];


ItemRecommender[d___]["Profile"][inputShowInds:{_Integer...}]:=
    ItemRecommender[d]["Profile"][inputShowInds,ConstantArray[1.,Length[inputShowInds]]];
ItemRecommender[d___]["Profile"][inputShowInds:{_Integer...},inputRatings:{_?NumberQ...}]:=
    Block[{inds,vec},
      vec=ItemRecommender[d]["ProfileVector"][inputShowInds,inputRatings];
      inds=Most[ArrayRules[vec]][[All,1,1]];
      SortBy[Select[Transpose[{vec[[inds]],inds,inds/.ItemRecommender[d]["columnIndexToTagRules"]}],#[[1]]>0&],-#[[1]]&]
    ]/;Length[inputShowInds]==Length[inputRatings];


ItemRecommender[d___]["MakeProfileVector"][tags_List]:=
    ItemRecommender[d]["MakeProfileVector"][tags,ConstantArray[1.,Length[tags]]];
ItemRecommender[d___]["MakeProfileVector"][tags_List,weights_]:=
    ItemRecommender[d]["MakeProfileVector"][Thread[tags->weights]];
ItemRecommender[d___]["MakeProfileVector"][aTagWeights_Association]:=
    ItemRecommender[d]["MakeProfileVector"][Normal[aTagWeights]];
ItemRecommender[d___]["MakeProfileVector"][tagWeightRules:{Rule[_,_?NumberQ]..}]:=
    Block[{t=tagWeightRules},
      t[[All,1]]=t[[All,1]]/.ItemRecommender[d]["tagToColumnIndexRules"];
      SparseArray[t,{Dimensions[ItemRecommender[d]["M01"]][[2]]}]
    ];

ItemRecommender[d___]["UserProfileFromVector"][args___] := ItemRecommender[d]["ProfileFromVector"][args];

ItemRecommender[d___]["ProfileFromVector"][pvec_SparseArray] :=
    Block[{arules,res},
      arules = Most[ArrayRules[pvec]];
	  res = Transpose[{arules[[All, 2]], Flatten[arules[[All, 1]]], ItemRecommender[d]["ColumnInterpretation"][[Flatten[arules[[All, 1]]]]]}];
	  res[[Reverse[Ordering[res[[All, 1]]]]]]	  
    ] /; VectorQ[pvec];


(*=========================================================*)
(* SMR Algebra                                             *)
(*=========================================================*)

(*ItemRecommender[d___]["Join"][smr2_ItemRecommender] :=*)
    (*Block[{},*)

    (*];*)

(*ItemRecommender[d___]["ColumnBind"][smat_SparseArray] :=*)
    (*Block[{},*)

    (*];*)

(*ItemRecommender[d___]["RowBind"][smat_SparseArray] :=*)
    (*Block[{},*)

    (*];*)

(*ItemRecommender[d___]["ColumnBind"][smat_SSparseMatrix] :=*)
    (*Block[{},*)

    (*];*)

(*ItemRecommender[d___]["RowBind"][smat_SSparseMatrix] :=*)
    (*Block[{},*)

    (*];*)

(*=========================================================*)
(* Classify                                                *)
(*=========================================================*)

ItemRecommender[d___]["Classify"][tagType_String, tags:(_List | {_Rule..} | _Association), args___] :=
    ItemRecommender[d]["Classify"][tagType, ItemRecommender[d]["MakeProfileVector"][tags], args];

ItemRecommender[d___]["Classify"][tagType_String, tags_List, weights_List, args___] :=
    ItemRecommender[d]["Classify"][tagType, ItemRecommender[d]["MakeProfileVector"][tags,weights], args] /; Length[tags] == Length[weights];

ItemRecommender[d___]["Classify"][tagType_String, pvec_SparseArray, nTopNNs_Integer,
  voting:(True|False):False, dropZeroScoredLabels:(True|False):True] :=
    Block[{recs, clMat, s, t},
      recs = ItemRecommender[d]["RecommendationsByProfile"][pvec,nTopNNs];

      clMat = ItemRecommender[d]["M"][[All, ItemRecommender[d]["tagTypeRanges"][tagType] ]];

      If[ voting,
        t = Most[ArrayRules[clMat]]; t[[All,2]] = 1;
        clMat = SparseArray[t,Dimensions[clMat]];
        recs[[All,1]] = 1;
      ];

      s = (recs[[All,1]] / Max[recs[[All,1]]] ) . clMat[[ recs[[All,2]], All ]];

      s = AssociationThread[ ItemRecommender[d]["ColumnInterpretation"][[ ItemRecommender[d]["tagTypeRanges"][tagType] ]] -> s ];

      Reverse[Sort[s]]
    ];

(*=========================================================*)
(* RatingPrediction                                        *)
(*=========================================================*)

(* ::Text:: *)
(*Designate the user u viewed show indexes with {Subscript[i, 1],Subscript[i, 2],...,Subscript[i, Subscript[n, u]]} and the items recommended for the user u with {Subscript[j, 1],Subscript[j, 2],...,Subscript[j, k]}. He has the ratings {Subscript[r, u Subscript[i, 1]],...,Subscript[r, u Subscript[i, Subscript[n, u]]]}. We are using the formula:.*)


(* ::DisplayFormula:: *)
(*Subscript[r, u j]=*)
(*\!\(\*UnderscriptBox[\(\[Sum]\), \(i \[Element] {*)
(*\*SubscriptBox[\(i\), \(1\)], ..., *)
(*\*SubscriptBox[\(i\), *)
(*SubscriptBox[\(n\), \(u\)]]}\)]\)Subscript[S, i j]Subscript[r, u i]/*)
(*\!\(\*UnderscriptBox[\(\[Sum]\), \(i \[Element] {*)
(*\*SubscriptBox[\(i\), \(1\)], ..., *)
(*\*SubscriptBox[\(i\), *)
(*SubscriptBox[\(n\), \(u\)]]}\)]\)Subscript[S, i j],j\[Element]{Subscript[j, 1],Subscript[j, 2],...,Subscript[j, k]}.*)


(* ::Text:: *)
(*We can do calculations of this formula with linear algebra operations.*)


(* ::DisplayFormula:: *)
(*A=M({Subscript[j, 1],...,Subscript[j, k]})M({Subscript[i, 1],...,Subscript[i, Subscript[n, u]]})^T*)
(*norms=A.[1,1,...,1]^T,where[1,1,...,1]\[Element]\[DoubleStruckCapitalR]^Subscript[n, u]*)
(*t=A.[Subscript[r, u Subscript[i, 1]],...,Subscript[r, u Subscript[i, Subscript[n, u]]]]^T*)
(*Subscript[r, u]=t/norms*)


ItemRecommender[d___]["RatingPrediction"][inputShowInds:{_Integer...},inputRatings:{_?NumberQ...},recInds:{_Integer...}]:=
    Block[{inds,norms,t,smat=ItemRecommender[d]["M"]},
      t=smat[[recInds]].Transpose[smat[[inputShowInds]]];
      norms=Total/@t;
      norms = If[#==0,1,#] & /@ norms;
      t=t.inputRatings;
      t=t/norms
      Transpose[{t,recInds}]
    ];


(* ::Section:: *)
(*Deteriorate old ratings*)


(* ::Text:: *)
(*Inherit dynamically ItemRecommender*)


ItemRecommender[d___]["UserItemsToLove"][inputShowInds:{_Integer...},inputRatings:{_?NumberQ...},inputTimes:{_?NumberQ...},{minRating_Integer,maxRating_Integer},deteriorationFunction_,nRes_Integer]:=
    Block[{newRatings},
      newRatings=MapThread[deteriorationFunction[#1,#2]&,{inputRatings,inputTimes}];
      ItemRecommender[d]["UserItemsToLove"][inputShowInds,inputRatings,nRes]
    ]/;Length[inputShowInds]==Length[inputRatings]==Length[inputTimes];


ItemRecommender[d___]["RatingPrediction"][inputShowInds:{_Integer...},inputRatings:{_?NumberQ...},inputTimes:{_?NumberQ...},recInds:{_Integer...},deteriorationFunction_]:=
    Block[{newRatings,res},
      newRatings=MapThread[deteriorationFunction[#1,#2]&,{inputRatings,inputTimes}];
      ItemRecommender[d]["RatingPrediction"][inputShowInds,inputRatings,recInds];
    ]/;Length[inputShowInds]==Length[inputRatings]==Length[inputTimes];


(*=========================================================*)
(* Utilities                                               *)
(*=========================================================*)

(* ::Subsection:: *)
(*RowTags from the matrix*)


Clear[RowTags];
RowTags[obj_ItemRecommender,rowInd_Integer]:=
    Block[{arules},
      arules=Most[ArrayRules[obj["M"][[rowInd]]]];
      SortBy[Transpose[{obj["ColumnInterpretation"][[Flatten[arules[[All,1]]]]],arules[[All,2]]}],-#[[2]]&]
    ];
RowTags[smat_SparseArray,columnNames_,rowInd_Integer]:=
    Block[{arules},
      arules=Most[ArrayRules[smat[[rowInd]]]];
      SortBy[Transpose[{columnNames[[Flatten[arules[[All,1]]]]],arules[[All,2]]}],-#[[2]]&]
    ];


Clear[DeleteSpatialTags];
DeleteSpatialTags[res:{{_,_?NumberQ}...}]:=
    Block[{},
    (*DeleteCases[res,{s_,n_}/;StringMatchQ[s,"{"~~NumberString~~__~~NumberString~~"}"]]*)
      DeleteCases[res,{s_List,n_?NumberQ}]
    ];


(* ::Subsection:: *)
(*GetProperties*)


Clear[GetProperties]
GetProperties[obj_ItemRecommender,itemInd_Integer]:=
    GetProperties[obj,obj["M"],itemInd];
GetProperties[obj_ItemRecommender,M:(_SparseArray|{_SparseArray..}),itemInd_Integer]:=
    GetProperties[obj["ColumnInterpretation"],M,itemInd];
GetProperties[properties_List,M:(_SparseArray|{_SparseArray..}),itemInd_Integer]:=
    Block[{t},
      t=Most[ArrayRules[M[[itemInd]]]];
      Transpose[{properties[[Flatten@t[[All,1]]]],t[[All,2]]}]
    ];


(* ::Subsection:: *)
(*GetItems*)


Clear[GetItems,GetItemIndexes]
GetItemIndexes[obj_ItemRecommender,tag:(_String|{_?NumberQ,_?NumberQ})]:=
    Block[{pos},
      pos=Flatten@Position[obj["ColumnInterpretation"],tag];
      If[pos=={},
        {},
        GetItems[Transpose[obj["M01"]],None,pos[[1]]]
      ]
    ];
GetItems[mat_,tagInd_]:=GetItems[mat,None,tagInd]
GetItems[obj_ItemRecommender,tag_String]:=
    Block[{pos},
      pos=Flatten@Position[obj["ColumnInterpretation"],tag];
      If[pos=={},
        {},
        GetItems[Transpose[obj["M01"]],obj["itemNames"],pos[[1]]]
      ]
    ];
GetItems[mat_SparseArray,names:(_List|None),itemInd_Integer]:=
    Block[{t},
      t=Most[ArrayRules[mat[[itemInd]]]];
      If[names===None,
        Transpose[{Flatten@t[[All,1]],t[[All,2]]}],
        Transpose[{names[[Flatten@t[[All,1]]]],t[[All,2]]}]
      ]
    ];


Clear[ItemsForTag]
ItemsForTag[IIR_ItemRecommender,tag:(_String|{_?NumberQ,_?NumberQ})]:=
    Block[{res},
      res=GetItemIndexes[IIR,tag];
      Map[{#[[1]],IIR["itemNames"][[#[[1]]]],IIR["itemCoordinates"][[#[[1]]]],#[[2]]}&,res]
    ];


Clear[ItemsForTags]
ItemsForTags[IIR_ItemRecommender,tags_List]:=
    Intersection@@Map[ItemsForTag[IIR,#]&,tags];


(* ::Subsection:: *)
(*FilterResults*)


Clear[FilterResults];
FilterResults[IIR_ItemRecommender,res_,filterTags_]:=
    Block[{fvec},
      fvec=IIR["MakeProfileVector"][filterTags];
      Pick[res,Map[#>0&,IIR["M01"][[res[[All,2]]]].fvec]]
    ];


(* ::Subsection:: *)
(*FilterResultsWithDistanceInterval*)


Clear[FilterResultsWithDistanceInterval];
FilterResultsWithDistanceInterval::ircoords="The recommender object `1` does not have (correct) item coordinates.";
FilterResultsWithDistanceInterval[IIR_ItemRecommender,res_,geopoint:{_?NumberQ,_?NumberQ},{minDistance_?NumberQ,maxDistance:(Infinity|_?NumberQ)},spaceStep_:1609.]:=
    Block[{dists},
      If[Length[IIR["itemCoordinates"]]!=Dimensions[IIR["M01"]][[1]],
        Message[FilterResultsWithDistanceInterval::ircoords,IIR];
        Return[res];
      ];
      dists=Map[EarthSphereDistance[geopoint,#]/N[spaceStep]&,IIR["itemCoordinates"][[res[[All,2]]]]];
      Pick[res,Map[minDistance<=#<=maxDistance&,dists]]
    ];


(* ::Subsection:: *)
(*TableReport*)


Clear[RowMatrixForIIR]
RowMatrixForIIR[IIR_ItemRecommender]:=RowMatrixForIIR[IIR]=(#&/@IIR["M"]);


Clear[TableReport];
Options[TableReport]={"SortColumn"->1,"SortDirection"->"desc"};
TableReport[IIR_ItemRecommender,geopoint_,res_,spaceStep_,opts:OptionsPattern[]]:=
    Block[{dists,data,sc=OptionValue[TableReport,"SortColumn"],sd=ToLowerCase[OptionValue[TableReport,"SortDirection"]]},
      sd=If[sd=="asc"||sd=="ascending"||sd=="incr"||sd=="increasing",sd=1,sd=-1];
      dists=Map[Floor[100*EarthSphereDistance[geopoint,#]/N[spaceStep]]/100.&,IIR["itemCoordinates"][[res[[All,2]]]]];
      data=SortBy[Flatten[#,1]&/@Transpose[{res[[All,1]],dists,IIR["itemNames"][[res[[All,2]]]]/.{s_String:>If[StringLength[s]>35,StringTake[s,35],s]},Function[{ind},SortBy[Select[DeleteSpatialTags[GetProperties[IIR["ColumnInterpretation"],RowMatrixForIIR[IIR],ind]],(StringMatchQ[#[[1]],__~~":"~~__]&)],-#[[2]]&]]/@res[[All,2]]}],sd*#[[sc]]&];
      TableForm[data,TableDepth->2,TableHeadings->{Automatic,Style[#,Blue,FontFamily->"Times"]&/@{"score","distance","name","tags"}}]
    ];
TableReport[IIR_ItemRecommender,res_,opts:OptionsPattern[]]:=Block[{dists,data,sc=OptionValue[TableReport,"SortColumn"],sd=ToLowerCase[OptionValue[TableReport,"SortDirection"]]},sd=If[sd=="asc"||sd=="ascending"||sd=="incr"||sd=="increasing",sd=1,sd=-1];
data=SortBy[(Flatten[#1,1]&)/@Transpose[{res[[All,1]],res[[All,2]],IIR["itemNames"][[res[[All,2]]]]/. {s_String:>If[StringLength[s]>35,StringTake[s,35],s]},Function[{ind},SortBy[GetProperties[IIR["ColumnInterpretation"],RowMatrixForIIR[IIR],ind],-#1[[2]]&]]/@res[[All,2]]}],sd #1[[sc]]&];
TableForm[data,TableDepth->2,TableHeadings->{Automatic,(Style[#1,Blue,FontFamily->"Times"]&)/@{"score","ids","name","tags"}}]
];


(* ::Section:: *)
(*Interface definitions*)


(* ::Subsection:: *)
(*Recommender interfacing functions*)


Clear[InterfaceUserProfileCalculation]
InterfaceUserProfileCalculation[irecObj_ItemRecommender,userInds_,userRatings_]:=
    Block[{colRules,uprof},
      uprof=irecObj["UserProfile"][userInds,userRatings];
      uprof
    ]/;Length[userInds]==Length[userRatings];


Clear[NormalizeRows];
NormalizeRows[smat_?MatrixQ]:=
    Block[{rowNorms},
      rowNorms=Normal[Map[Total,smat]]/.{0->1,0.->1.};
      DiagonalMatrix[SparseArray[1/rowNorms]].smat
    ];


Clear[InterfaceUserToLoveCalculation]
InterfaceUserToLoveCalculation[irecObj_ItemRecommender,userInds_,data_,userRatings_,metaDataWeights_,nResults_Integer]:=
    Block[{res,loved},
      irecObj["UseTagTypeWeights"][metaDataWeights];
      res=irecObj["UserItemsToLove"][userInds,userRatings,nResults];
      loved=Transpose[{res[[All,1]],res[[All,2]],data[[res[[All,2]]]]}];
      loved
    ]/;(userInds==={}||Length[userInds]==Length[userRatings])&&Length[irecObj["tagTypes"]]==Length[metaDataWeights];
InterfaceUserToLoveCalculation[irecTargetObj_ItemRecommender,irecSourceObj_ItemRecommender,userInds_,data_,userRatings_,metaDataWeights_,nResults_Integer]:=
    Block[{res,loved,pvec},
      irecSourceObj["UseTagTypeWeights"][metaDataWeights];
      irecTargetObj["UseTagTypeWeights"][metaDataWeights];
      pvec=irecSourceObj["ProfileVector"][userInds];
      res=irecTargetObj["RecommendationsByProfile"][pvec,nResults];
      loved=Transpose[{res[[All,1]],res[[All,2]],data[[res[[All,2]]]]}];
      loved
    ]/;(userInds==={}||Length[userInds]==Length[userRatings])&&Length[irecTargetObj["tagTypes"]]==Length[metaDataWeights]&&Length[irecSourceObj["tagTypes"]]==Length[metaDataWeights];


Clear[AutomaticUserToLoveCalculation]
AutomaticUserToLoveCalculation[irecObj_ItemRecommender,userInds_,data_,userRatings_,metaDataWeights_,nResults_Integer]:=
    Block[{res,loved,weights},
      weights=SparseArray[Log[Dimensions[irecObj["M01"]][[1]]/(N[Total[irecObj["M01"]]]/.{0.->10000.})]];
      Do[
        If[metaDataWeights[[i]]==0,
          weights[[irecObj["tagTypeIndexOffsets"][[i]]+1;;irecObj["tagTypeIndexOffsets"][[i+1]]]]=0
        ]
        ,{i,Length[metaDataWeights]}];
      irecObj["UseWeights"][weights];
      res=irecObj["UserItemsToLove"][userInds,userRatings,nResults];
      loved=Transpose[{res[[All,1]],res[[All,2]],data[[res[[All,2]]]]}];
      loved
    ]/;Length[userInds]==Length[userRatings]&&Length[irecObj["tagTypes"]]==Length[metaDataWeights];


Clear[InterfaceUserToLoveFiltered]
InterfaceUserToLoveFiltered[irecObj_ItemRecommender,toBeLoved_,uProf_]:=
    Block[{profileVec,newOrder},
      profileVec=ConstantArray[0,Dimensions[irecObj["M"]][[2]]];
      profileVec[[uProf[[All,2]]]]=1;
      newOrder=Reverse@Ordering[irecObj["M"][[toBeLoved[[All,2]]]].profileVec];
      toBeLoved[[newOrder]]
    ];


(* ::Subsection:: *)
(*Outliers*)


Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]


(* ::Subsection:: *)
(*Outlier detection interfacing functions*)


Clear[ColorProfileOutliers]
ColorProfileOutliers[uProf_]:=
    Block[{maxRank,clRules,cls},
      maxRank=Max[uProf[[All,1]]];
      (*cls={{maxRank},Select[shown[[All,1]],0.9 maxRank<=#1<maxRank&]};*)
      cls=OutlierIdentifier[uProf[[All,1]],TopOutliers[SPLUSQuartileIdentifierParameters[#]]&];
      cls={cls,OutlierIdentifier[Drop[uProf[[All,1]],Length[cls]],TopOutliers[HampelIdentifierParameters[#]]&]};
      clRules=Flatten@MapThread[Function[{gr,c},(#1->Style[#1,c]&)/@gr],{cls,Take[{Red,Blue},Length[cls]]},1];
      MapThread[Prepend,{uProf[[All,2;;-1]],uProf[[All,1]]/. clRules}]
    ];


Clear[ColorSelectedInds];
Options[ColorSelectedInds]=Join[Options[Style],{"Foreground"->Darker[Red]}];
ColorSelectedInds[uProf_,inds:{_Integer...},opts:OptionsPattern[]]:=
    Block[{clRules,fc=OptionValue["Foreground"]},
      Fold[ReplacePart[#1,#2->(Style[#,fc,opts]&/@#1[[#2]])]&,uProf,inds]
    ];


Clear[OutlierIndexes]
OutlierIndexes[data:{_?NumberQ...},outlierIdentifier_:HampelIdentifierParameters]:=
    Block[{cls,t},
      cls=OutlierIdentifier[data,TopOutliers[outlierIdentifier[#]]&];
      t=Select[Transpose[{data,Range[Length[data]]}],MemberQ[cls,#[[1]]]&];
      If[t==={},{},t[[All,2]]]
    ];


(* ::Subsection:: *)
(*Post-processing functions*)


Clear[MetaDataProofs]
Options[MetaDataProofs]={"OutlierIdentifierParameters"->None,"NormalizeScores"->True};
MetaDataProofs[irecObj_ItemRecommender,toBeLovedInd_Integer,userProfile_,opts:OptionsPattern[]]:=
    Block[{shInds,scores,outInds,oiFunc=OptionValue["OutlierIdentifierParameters"],normScores=OptionValue["NormalizeScores"],res},
      shInds=Flatten[(Most@ArrayRules[irecObj["M"][[toBeLovedInd]]])[[All,1]]];
      scores=SortBy[Select[userProfile,MemberQ[shInds,#[[2]]]&],-#[[1]]&];
      res=
          If[TrueQ[oiFunc===None],
            scores,
            scores[[If[#==={},All,#]&@OutlierIndexes[scores[[All,1]],TopOutliers[oiFunc[#]]&]]]
          ];
      If[TrueQ[normScores],
        res[[All,1]]=res[[All,1]]/Max[userProfile[[All,1]]]//N
      ];
      res
    ];


Clear[HistoryProofs]
Options[HistoryProofs]={"OutlierIdentifierParameters"->None,"NormalizeScores"->True};
HistoryProofs[irecObj_ItemRecommender,toBeLovedInd_Integer,userHistoryInds:{_Integer...},opts:OptionsPattern[]]:=
    HistoryProofs[irecObj,toBeLovedInd,Transpose[{Table[1.,{Length[userHistoryInds]}],userHistoryInds,userHistoryInds}],opts];
HistoryProofs[irecObj_ItemRecommender,toBeLovedInd_Integer,userHistory:{{_?NumberQ,_Integer,___}...},opts:OptionsPattern[]]:=
    Block[{userHistoryInds,userHistoryRatings,scores,outInds,oiFunc=OptionValue["OutlierIdentifierParameters"],normScores=OptionValue["NormalizeScores"],res},
      userHistoryInds=userHistory[[All,2]];
      userHistoryRatings=userHistory[[All,1]];
      scores=irecObj["M"][[userHistoryInds]].irecObj["M"][[toBeLovedInd]];
      scores=scores*userHistoryRatings;
      scores=Select[SortBy[Transpose[{scores,userHistory}],-#[[1]]&],#[[1]]>0&];
      res=Flatten/@
          If[TrueQ[oiFunc===None],
            scores,
            scores[[If[#==={},All,#]&@OutlierIndexes[scores[[All,1]],TopOutliers[oiFunc[#]]&]]]
          ];
      res=Map[Drop[#,{2}]&,res];
      If[TrueQ[normScores],
        res[[All,1]]=res[[All,1]]/(Max[userHistoryRatings]*(irecObj["M"][[toBeLovedInd]].irecObj["M"][[toBeLovedInd]]))//N
      ];
      res
    ];


Clear[FancyProofs]
Options[FancyProofs]={"Preamble"->"Recommended","HistoryProofPreamble"->"because of your interest in","MetaDataProofPreamble"->"because you like","SimilarityThreshold"->0.45,"ProofOptions"->Options[HistoryProofs]};
FancyProofs[irecObj_ItemRecommender,toBeLovedInd_Integer,userHistory:{{_?NumberQ,_Integer,_String}...},userProfile_,opts:OptionsPattern[]]:=
    Block[{hpsRes,mdpsRes,hps,mdps,TryToTake,
      prem=OptionValue["Preamble"],
      hpPrem=OptionValue["HistoryProofPreamble"],
      mdpPrem=OptionValue["MetaDataProofPreamble"],
      simTh=OptionValue["SimilarityThreshold"],
      proofOpts=OptionValue["ProofOptions"]},
      TryToTake[l_,n_]:=If[Length[l]>n,Take[l,n],l];
      hpsRes=HistoryProofs[irecObj,toBeLovedInd,userHistory,"NormalizeScores"->True,proofOpts];
      mdpsRes=MetaDataProofs[irecObj,toBeLovedInd,userProfile,"NormalizeScores"->True,proofOpts];
      hps=Style[#,Blue]&/@hpsRes[[All,-1]];
      mdps=Style[#,Purple]&/@mdpsRes[[All,-1]];
      Which[
        Length[hps]==Length[userHistory]||Max[hpsRes[[All,1]]]<simTh,
        Row[{prem," ",mdpPrem,": ",TryToTake[mdps,4]}],
        Length[mdps]==0,
        Row[{prem," ",hpPrem," ",TryToTake[hps,3]}],
        Length[hps]==3,
        Row[{prem," ",hpPrem," ",hps}],
        Length[hps]>3,
        Row[{prem," ",hpPrem," ",TryToTake[hps,3]," and ",mdpPrem," ",TryToTake[mdps,2]}],
        Length[hps]<3,
        Row[{prem," ",hpPrem," ",hps," and ",mdpPrem," ",TryToTake[mdps,4-Length[hps]]}],
        True,
        Row[{prem," ",mdpPrem,": ",TryToTake[mdps,4]}]
      ]
    ];


Clear[ProofHistogram]
Options[ProofHistogram]=Options[BarChart];
ProofHistogram[scores_,nelems_:5,opts:OptionsPattern[]]:=
    Block[{data=If[Length[Abs[scores]]<nelems ||nelems===All,scores,Take[scores,nelems]]},
      BarChart[Reverse@data[[All,1]],BarOrigin->Left,ChartLabels->Placed[Reverse@data[[All,3]],After],opts]
    ];


Clear[FancyTooltipProofs]
Options[FancyTooltipProofs]={"Preamble"->"Recommended","HistoryProofPreamble"->"because of your interest in","MetaDataProofPreamble"->"because you like","SimilarityThreshold"->0.45,"ProofHistogramOptions"->{ImageSize->200},"ProofOptions"->Options[HistoryProofs]};
FancyTooltipProofs[irecObj_ItemRecommender,toBeLovedInd_Integer,userHistory:{{_?NumberQ,_Integer,_String}...},userProfile_,opts:OptionsPattern[]]:=
    Block[{hpsRes,mdpsRes,hps,mdps,TryToTake,NBARS=5,
      prem=OptionValue["Preamble"],
      hpPrem=OptionValue["HistoryProofPreamble"],
      mdpPrem=OptionValue["MetaDataProofPreamble"],
      simTh=OptionValue["SimilarityThreshold"],
      proofOpts=OptionValue["ProofOptions"],
      proofHistOpts=OptionValue["ProofHistogramOptions"]},

      TryToTake[l_,n_]:=If[Length[l]>n,Take[l,n],l];
      hpsRes=HistoryProofs[irecObj,toBeLovedInd,userHistory,"NormalizeScores"->True,proofOpts];
      mdpsRes=MetaDataProofs[irecObj,toBeLovedInd,userProfile,"NormalizeScores"->True,proofOpts];

      hps=Style[#,Blue]&/@hpsRes[[All,-1]];
      mdps=Style[#,Purple]&/@mdpsRes[[All,-1]];

      Which[
        Length[hps]==Length[userHistory]||Max[hpsRes[[All,1]]]<simTh,
        Row[{prem," ",mdpPrem,": ",Tooltip[TryToTake[mdps,4],ProofHistogram[mdpsRes,NBARS,proofHistOpts]]}],
        Length[mdps]==0,
        Row[{prem," ",hpPrem," ",Tooltip[TryToTake[hps,3],ProofHistogram[hpsRes,NBARS,proofHistOpts]]}],
        Length[hps]==3,
        Row[{prem," ",hpPrem," ",Tooltip[hps,ProofHistogram[hpsRes,NBARS,proofHistOpts]]}],
        Length[hps]>3,
        Row[{prem," ",hpPrem," ",Tooltip[TryToTake[hps,3],ProofHistogram[hpsRes,NBARS,proofHistOpts]]," and ",mdpPrem," ",Tooltip[TryToTake[mdps,2],ProofHistogram[mdpsRes,NBARS,proofHistOpts]]}],
        Length[hps]<3,
        Row[{prem," ",hpPrem," ",Tooltip[hps,ProofHistogram[hpsRes,NBARS,proofHistOpts]]," and ",mdpPrem," ",Tooltip[TryToTake[mdps,4-Length[hps]],ProofHistogram[mdpsRes,NBARS,proofHistOpts]]}],
        True,
        Row[{prem," ",mdpPrem,": ",Tooltip[TryToTake[mdps,4],ProofHistogram[mdpsRes,NBARS,proofHistOpts]]}]
      ]
    ];


(* ::Subsection:: *)
(*Observer / MVC functions*)


Clear[AddLikeItButton]
SetAttributes[AddLikeItButton,HoldRest];
AddLikeItButton[searchResult:{{_,_,___}...},observedVar_Symbol,command_]:=
    Block[{},
      Map[Append[#1,Button[Style["Like It!",Italic,Small],(AppendTo[USERRATINGS,3];AppendTo[USERDATETIMES,Append[Most[Date[]],0]];AppendTo[observedVar,#[[2]]]),Appearance->None]]&,searchResult]
    ];

AddLikeItButton[searchResult:{{_,_,___}...},observedVar_Symbol,observedVar2_Symbol,command_]:=
    Block[{},
      Map[Append[#1,Button[Style["Like It!",Italic,Small],(AppendTo[observedVar2,1];AppendTo[observedVar,#[[2]]]),Appearance->None]]&,searchResult]
    ];


Clear[AddUseItButton]
SetAttributes[AddUseItButton,HoldRest];
AddUseItButton[searchResult:{{_,_,___}...},observedVar_Symbol,command_]:=
    Block[{},
      MapIndexed[Prepend[#1,Button[Style["\[FilledCircle]",Bold,Small],(If[MemberQ[observedVar,#2[[1]]],observedVar=Complement[observedVar,{#2[[1]]}],AppendTo[observedVar,#2[[1]]]]),Appearance->None]]&,searchResult]
    ];


Clear[AddRatingButton]
SetAttributes[AddRatingButton,HoldRest];
AddRatingButton[searchResult:{{_,_,___}...},observedVar_Symbol,command_]:=
    Block[{},
      MapIndexed[Append[#1,Button[Style["\[FivePointedStar]",Bold],(USERRATINGS[[#2[[1]]]]=Mod[USERRATINGS[[#2[[1]]]],5]+1),Appearance->None]]&,searchResult]
    ];
AddRatingButton[searchResult:{{_,_,___}...},observedVar_Symbol,maxRating_,command_]:=
    Block[{},
      MapIndexed[Append[#1,Button[Style["\[FivePointedStar]",Bold],(observedVar[[#2[[1]]]]=Mod[observedVar[[#2[[1]]]],maxRating]+1),Appearance->None]]&,searchResult]
    ];


(* ::Section::Closed:: *)
(*Experiments*)

(*
offsets=FoldList[Plus,0,{10,10,10}]


Flatten[MapThread[ConstantArray[#1,#2]&,{{1,20,300},Subtract@@@Partition[Reverse[{0,10,20,30}],2,1]}]]
%//Length
*)