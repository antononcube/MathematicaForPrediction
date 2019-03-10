(*
    SSparseMatrix Mathematica unit tests
    Copyright (C) 2018 Anton Antonov

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
    antononcube @ gmai l . c om,
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: JavaTriesWithFrequencies-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2017-01-19 *)

(* :Package Version: 1.0 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2017 Anton Antonov *)
(* :Keywords: S, R, sparse matrix, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

  In order to use this unit tests file set the correct paths in the test with ID "LoadPackage".

*)

BeginTestSection["SSparseMatrixTests"]

VerificationTest[(* 1 *)
	CompoundExpression[
		Get["~/MathematicaForPrediction/SSparseMatrix.m"],
		Greater[Length[DownValues[SSparseMatrix`RowNames]], 0]]
	,
	True	
	,
	TestID->"LoadPackage"
]

VerificationTest[(* 2 *)
	CompoundExpression[Set[mat, SparseArray[List[Rule[List[1, 1], 1], Rule[List[2, 2], 2], Rule[List[4, 3], 3], Rule[List[1, 4], 4], Rule[List[3, 5], 2]]]], Null]
	,
	Null	
	,
	TestID->"mat"
]

VerificationTest[(* 3 *)
	CompoundExpression[Set[mat2, SparseArray[List[Rule[List[1, 1], 1], Rule[List[2, 2], 2], Rule[List[4, 3], 3], Rule[List[1, 3], 4]]]], Null]
	,
	Null	
	,
	TestID->"mat2"
]

VerificationTest[(* 4 *)
	Set[rmat, MakeSSparseMatrix[List[Rule[List[1, 1], 1], Rule[List[2, 2], 2], Rule[List[4, 3], 3], Rule[List[1, 4], 4], Rule[List[3, 5], 2]], Rule["ColumnNames", List["a", "b", "c", "d", "e"]], Rule["RowNames", List["A", "B", "C", "D"]], Rule["DimensionNames", List["U", "V"]]]]
	,
	SSparseMatrix[Association[Rule["SparseMatrix", SparseArray[Automatic, List[4, 5], 0, List[1, List[List[0, 2, 3, 4, 5], List[List[1], List[4], List[2], List[5], List[3]]], List[1, 4, 2, 2, 3]]]], Rule["RowNames", Association[Rule["A", 1], Rule["B", 2], Rule["C", 3], Rule["D", 4]]], Rule["ColumnNames", Association[Rule["a", 1], Rule["b", 2], Rule["c", 3], Rule["d", 4], Rule["e", 5]]], Rule["DimensionNames", Association[Rule["U", 1], Rule["V", 2]]]]]	
	,
	TestID->"rmat"
]

VerificationTest[(* 5 *)
	Equal[SparseArray[rmat], mat]
	,
	True	
	,
	TestID->"rmat-SparseArray"
]

VerificationTest[(* 6 *)
	DimensionNames[rmat]
	,
	List["U", "V"]	
	,
	TestID->"DimensionNames-1"
]

VerificationTest[(* 7 *)
	RowNames[rmat]
	,
	List["A", "B", "C", "D"]	
	,
	TestID->"RowNames-1"
]

VerificationTest[(* 8 *)
	ColumnNames[rmat]
	,
	List["a", "b", "c", "d", "e"]	
	,
	TestID->"ColumnNames-1"
]

VerificationTest[(* 9 *)
	CompoundExpression[Set[rmat2, rmat], Null]
	,
	Null	
	,
	TestID->"rmat2"
]

VerificationTest[(* 10 *)
	CompoundExpression[
		Set[rmat2, rmat],
		rmat2 = SetColumnNames[rmat2, Map[ToString, Range[ColumnsCount[rmat]]]],
		Equal[ColumnNames[rmat2], Map[ToString, Range[ColumnsCount[rmat]]]]
	]
	,
	True	
	,
	TestID->"SetColumnNames-1"
]

VerificationTest[(* 11 *)
	CompoundExpression[
		Set[rmat2, rmat],
		rmat2 = SetRowNames[rmat2, Map[ToString, Range[RowsCount[rmat]]]],
		Equal[RowNames[rmat2], Map[ToString, Range[RowsCount[rmat]]]]]
	,
	True	
	,
	TestID->"SetRowNames-2"
]

VerificationTest[(* 12 *)
	DimensionNames[Transpose[rmat]]
	,
	List["V", "U"]	
	,
	TestID->"DimensionNames-1"
]

VerificationTest[(* 13 *)
	Total[rmat, 2]
	,
	12	
	,
	TestID->"Total-1"
]

VerificationTest[(* 14 *)
	RowSums[rmat]
	,
	List[5, 2, 2, 3]	
	,
	TestID->"RowSums-1"
]

VerificationTest[(* 15 *)
	ColumnSums[rmat]
	,
	List[1, 2, 3, 4, 2]	
	,
	TestID->"ColumnSums-1"
]

VerificationTest[(* 16 *)
	Transpose[Part[rmat, List[1], All]]
	,
	SSparseMatrix`SSparseMatrix[Association[Rule["SparseMatrix", SparseArray[Automatic, List[5, 1], 0, List[1, List[List[0, 1, 1, 1, 2, 2], List[List[1], List[1]]], List[1, 4]]]], Rule["RowNames", Association[Rule["a", 1], Rule["b", 2], Rule["c", 3], Rule["d", 4], Rule["e", 5]]], Rule["ColumnNames", Association[Rule["A", 1]]], Rule["DimensionNames", Association[Rule["V", 2], Rule["U", 1]]]]]	
	,
	TestID->"Transpose=1"
]

VerificationTest[(* 17 *)
	Dot[rmat, Transpose[Part[rmat, List[1], All]]]
	,
	(* The previous version of this test had "DimensionNames" -> Association["U" -> 2] . *)
	(* I changed it when I introduced the SSparseMatrix::dnsame message. *)
	SSparseMatrix[
		Association["SparseMatrix" -> SparseArray[Automatic, {4, 1}, 0,
			{1, {{0, 1, 1, 1, 1}, {{1}}}, {17}}],
			"RowNames" -> Association["A" -> 1, "B" -> 2, "C" -> 3, "D" -> 4],
			"ColumnNames" -> Association["A" -> 1],
			"DimensionNames" -> <|"1" -> 1, "2" -> 2|>]]
	,
	{SSparseMatrix::dnsame}
	,
	TestID->"Dot-matrix-vector-1"
]

VerificationTest[(* 18 *)
	Normal[SparseArray[Dot[rmat, Transpose[Part[rmat, List[1], All]]]]]
	,
	List[List[17], List[0], List[0], List[0]]
  ,
	{SSparseMatrix::dnsame}
	,
	TestID->"Dot-matrix-vector-2"
]

VerificationTest[(* 19 *)
	Equal[SparseArray[Dot[rmat, Transpose[Part[rmat, List[1], All]]]], Dot[SparseArray[rmat], Transpose[SparseArray[Part[rmat, List[1], All]]]]]
	,
	True
	,
	{SSparseMatrix::dnsame}
	,
	TestID->"Dot-matrix-vector-3"
]

VerificationTest[(* 20 *)
	Equal[SparseArray[Dot[rmat, Part[rmat, 1, All]]], Dot[SparseArray[rmat], Part[rmat, 1, All]]]
	,
	True
	,
	TestID->"Dot-matrix-vector-4"
]

VerificationTest[(* 21 *)
	Equal[SparseArray[Dot[rmat, Transpose[mat]]], Dot[SparseArray[rmat], Transpose[mat]]]
	,
	True	
	,
	TestID->"Dot-matrix-matrix-1"
]

VerificationTest[(* 22 *)
	Equal[SparseArray[Dot[Transpose[mat], rmat]], Dot[Transpose[mat], SparseArray[rmat]]]
	,
	True	
	,
	TestID->"Dot-matrix-matrix-2"
]

VerificationTest[(* 23 *)
	CompoundExpression[Set[rmat2, ToSSparseMatrix[SparseArray[RandomInteger[List[0, 4], List[ColumnsCount[rmat], RowsCount[rmat]]]]]], Equal[SparseArray[Dot[rmat, rmat2, rmat]], Dot[SparseArray[rmat], SparseArray[rmat2], SparseArray[rmat]]]]
	,
	True	
	,
	TestID->"Dot-matrix-matrix-matrix-1"
]

VerificationTest[(* 24 *)
	RowNames[Dot[rmat, rmat2, rmat]]
	,
	RowNames[rmat]	
	,
	TestID->"Dot-matrix-matrix-matrix-2"
]

VerificationTest[(* 25 *)
	ColumnNames[Dot[rmat, rmat2, rmat]]
	,
	ColumnNames[rmat]	
	,
	TestID->"Dot-matrix-matrix-matrix-3"
]

VerificationTest[(* 26 *)
	SparseArray[Plus[rmat, 1]]
	,
	Plus[SparseArray[rmat], 1]	
	,
	TestID->"Plus-1"
]

VerificationTest[(* 27 *)
	SparseArray[Plus[rmat, -2]]
	,
	Plus[SparseArray[rmat], -2]	
	,
	TestID->"Plus-2"
]

VerificationTest[(* 28 *)
	SparseArray[Times[rmat, 10]]
	,
	Times[SparseArray[rmat], 10]	
	,
	TestID->"Times-1"
]

VerificationTest[(* 29 *)
	SparseArray[Plus[Times[10, rmat], Times[2.33`, rmat]]]
	,
	Plus[Times[10, SparseArray[rmat]], Times[2.33`, SparseArray[rmat]]]	
	,
	TestID->"Arithmetic-1"
]

VerificationTest[(* 30 *)
	Plus[rmat, Transpose[rmat2]]
	,
	Plus[ToSSparseMatrix[SparseArray[rmat]], Transpose[rmat2]]	
	,
	TestID->"MatrixSum-1"
]

VerificationTest[(* 31 *)
	List[Part[rmat, "A"], Head[Part[rmat, "A"]]]
	,
	List[Part[SparseArray[rmat], 1], SparseArray]	
	,
	TestID->"Part-1"
]

VerificationTest[(* 32 *)
	List[Part[rmat, All, "a"], Head[Part[rmat, All, "a"]]]
	,
	List[Part[SparseArray[rmat], All, 1], SparseArray]	
	,
	TestID->"Part-2"
]

VerificationTest[(* 33 *)
	CompoundExpression[Set[t1, MakeSSparseMatrix[SparseArray[Part[rmat, List["A"]]], Rule["RowNames", List["A"]], Rule["ColumnNames", ColumnNames[rmat]]]], List[Equal[SparseArray[Part[rmat, List["A"]]], SparseArray[t1]], Equal[RowNames[Part[rmat, List["A"]]], RowNames[t1]], Equal[ColumnNames[Part[rmat, List["A"]]], ColumnNames[t1]]]]
	,
	List[True, True, True]	
	,
	TestID->"Part-3"
]

VerificationTest[(* 34 *)
	Part[rmat, "A", All]
	,
	Part[rmat, "A"]	
	,
	TestID->"Part-4"
]

VerificationTest[(* 35 *)
	Part[rmat, "A", "d"]
	,
	Part[SparseArray[rmat], 1, 4]	
	,
	TestID->"Part-5"
]

VerificationTest[(* 36 *)
	Part[rmat, List["C", "D", "A", "B"]]
	,
	Part[rmat, List[3, 4, 1, 2], All]	
	,
	TestID->"PermutationPart-1"
]

VerificationTest[(* 37 *)
	Part[rmat, List["C", "D", "A", "B"]]
	,
	Part[rmat, List[3, 4, 1, 2]]	
	,
	TestID->"PermutationPart-2"
]

VerificationTest[(* 38 *)
	SparseArray[Part[rmat, List["C", "D", "A", "B"]]]
	,
	Part[SparseArray[rmat], List[3, 4, 1, 2]]	
	,
	TestID->"PermutationPart-3"
]

VerificationTest[(* 39 *)
	SparseArray[Part[rmat, List["C", "D", "A", "B"], List["c", "d", "e", "a", "b"]]]
	,
	Part[SparseArray[rmat], List[3, 4, 1, 2], List[3, 4, 5, 1, 2]]	
	,
	TestID->"PermutationPart-4"
]

VerificationTest[(* 40 *)
	Equal[Part[rmat, List["B", "C"], List["a", "b"]], Part[rmat, Span[2, 3], Span[1, 2]]]
	,
	True	
	,
	TestID->"PartSubset-1"
]

VerificationTest[(* 41 *)
	Equal[rmat, Part[rmat, All, All]]
	,
	True	
	,
	TestID->"PartSubset-2"
]

VerificationTest[(* 42 *)
	CompoundExpression[Set[rmat2, ToSSparseMatrix[rmat, Rule["RowNames", Map[Function[StringJoin["s.", Slot[1]]], RowNames[rmat]]]]], Set[rmat3, ToSSparseMatrix[rmat, Rule["ColumnNames", Map[Function[StringJoin["t.", Slot[1]]], ColumnNames[rmat]]]]], Null]
	,
	Null	
	,
	TestID->"BindingMatrices-0"
]

VerificationTest[(* 43 *)
	CompoundExpression[Set[t, Normal[SparseArray[rmat]]], Set[res, MakeSSparseMatrix[SparseArray[Join[t, t]], Rule["RowNames", Join[Map[Function[StringJoin[Slot[1], ".1"]], RowNames[rmat]], Map[Function[StringJoin[Slot[1], ".2"]], RowNames[rmat]]]], Rule["ColumnNames", ColumnNames[rmat]]]], Set[res2, RowBind[rmat, rmat]], List[Equal[SparseArray[res], SparseArray[res2]], Equal[RowNames[res], RowNames[res2]], Equal[ColumnNames[res], ColumnNames[res2]]]]
	,
	List[True, True, True]	
	,
	TestID->"RowBind-1"
]

VerificationTest[(* 44 *)
	CompoundExpression[Set[t, Normal[SparseArray[rmat]]], Set[res, MakeSSparseMatrix[SparseArray[Join[t, t]], Rule["RowNames", Join[RowNames[rmat], RowNames[rmat2]]], Rule["ColumnNames", ColumnNames[rmat]]]], Set[res2, RowBind[rmat, rmat2]], List[Equal[SparseArray[res], SparseArray[res2]], Equal[RowNames[res], RowNames[res2]], Equal[ColumnNames[res], ColumnNames[res2]]]]
	,
	List[True, True, True]	
	,
	TestID->"RowBind-2"
]

VerificationTest[(* 45 *)
	CompoundExpression[
		Set[t, Transpose[Normal[SparseArray[rmat]]]],
		Set[res, MakeSSparseMatrix[SparseArray[Transpose[Join[t, t]]],
			Rule["RowNames", RowNames[rmat]],
			Rule["ColumnNames", Join[Map[Function[StringJoin[Slot[1], ".1"]], ColumnNames[rmat]], Map[Function[StringJoin[Slot[1], ".2"]], ColumnNames[rmat]]]]]
		],
		Set[res2, ColumnBind[rmat, rmat]],
		List[Equal[SparseArray[res], SparseArray[res2]], Equal[RowNames[res], RowNames[res2]], Equal[ColumnNames[res], ColumnNames[res2]]]]
	,
	List[True, True, True]	
	,
	TestID->"ColumnBind-1"
]

VerificationTest[(* 46 *)
	CompoundExpression[
		Set[t, Transpose[Normal[SparseArray[rmat]]]],
		Set[t2, Transpose[Normal[SparseArray[rmat3]]]],
		Set[res, MakeSSparseMatrix[SparseArray[Transpose[Join[t, t2]]],
			Rule["RowNames", RowNames[rmat]],
			Rule["ColumnNames", Join[ColumnNames[rmat], ColumnNames[rmat3]]]]
		],
		Set[res2, ColumnBind[rmat, rmat3]],
		List[Equal[SparseArray[res], SparseArray[res]], Equal[RowNames[res], RowNames[res2]], Equal[ColumnNames[res], ColumnNames[res2]]]]
	,
	List[True, True, True]	
	,
	TestID->"ColumnBind-2"
]


VerificationTest[(* 47 *)

	newRows = {"D", "A", "A1", "B", "B1", "C"};
	rmat2 = ImposeRowNames[rmat, newRows];

	rs2 = RowSumsAssociation[rmat2];
	rs = RowSumsAssociation[rmat];
	cs2 = ColumnSumsAssociation[rmat2];
	cs = ColumnSumsAssociation[rmat];

	ColumnNames[rmat2] == ColumnNames[rmat] &&

			RowNames[rmat2] == newRows &&

			rs2["A1"] == 0 &&

			rs2["B1"] == 0 &&

			Apply[ And, Map[ rs2[#] == rs[#] &, RowNames[rmat]] ] &&
			Apply[ And, Map[ cs2[#] == cs[#] &, Intersection[ColumnNames[rmat2], ColumnNames[rmat]]]],

	True,

	TestID->"ImposeRowNames-1"
]


VerificationTest[(* 48 *)

	newColumns = {"d", "a", "a1", "b", "b1", "c", "e"};
	rmat3 = ImposeColumnNames[rmat, newColumns];

	rs3 = RowSumsAssociation[rmat3];
	rs = RowSumsAssociation[rmat];
	cs3 = ColumnSumsAssociation[rmat3];
	cs = ColumnSumsAssociation[rmat];

	RowNames[rmat3] == RowNames[rmat] &&

			ColumnNames[rmat3] == newColumns &&

			cs3["a1"] == 0 &&

			cs3["b1"] == 0 &&

			Apply[ And, Map[ rs3[#] == rs[#] &, Intersection[RowNames[rmat3], RowNames[rmat]]] ] &&

			Apply[ And, Map[ cs3[#] == cs[#] &, ColumnNames[rmat]] ],

	True,

	TestID->"ImposeColumnNames-1"
]


VerificationTest[(* 49 *)

	smatProf = SparseArray[RandomReal[{0, 1}, {200, 120}]];

	rmatProf =
			ToSSparseMatrix[smatProf,
				"RowNames" ->
						Map["A" <> ToString[#] &, Range[Dimensions[smatProf][[1]]]],
				"ColumnNames" ->
						Map["b" <> ToString[#] &, Range[Dimensions[smatProf][[2]]]]];

	sres = smatProf.Transpose[smatProf];
	rres = rmatProf.Transpose[rmatProf];

	Norm[sres[[1 ;; 120, 1 ;; 120]] - SparseArray[rres[[1 ;; 120, 1 ;; 120]]]],

	0.,

	{SSparseMatrix::dnsame},

	TestID -> "Dimension-names-after-Dot-1"
]

EndTestSection[]
