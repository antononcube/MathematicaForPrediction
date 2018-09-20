(*
    Data reshaping Mathematica package
    Copyright (C) 2018  Anton Antonov

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
    Mathematica is (C) Copyright 1988-2018 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* :Title: DataReshape *)
(* :Context: DataReshape` *)
(* :Author: Anton Antonov *)
(* :Date: 2018-09-07 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: long form, wide form, dataset, reshape *)
(* :Discussion:

    # In brief

    Functions for conversion of Dataset objects and matrices into long form or wide form.

    # Rationale

    Obviously inspired from R's package "reshape2", [1].


    # Usage examples



    # References

    [1] Hadley Wickham, reshape2: Flexibly Reshape Data: A Reboot of the Reshape Package, (2017), cran.r-project.org.
        URL: https://cran.r-project.org/web/packages/reshape2/index.html .


    Anton Antonov
    Windermere, FL, USA
    2018-09-07

*)

BeginPackage["DataReshape`"];

ToLongForm::usage = "ToLongForm[ds_Dataset, idColumns_, valueColumns_] \
converts the dataset ds into long form. The resulting dataset has the columns idColumns and \
the columns \"Variable\" and \"Value\" derived from valueColumns."

ToWideForm::usage = "ToWideForm[ds_Dataset, idColumns_, variableColumn_, valueColumns_] \
converts the dataset ds into wide form. The resulting dataset has columns that are unique values of variableColumn and \
with values that are the corresponding values of valueColumn."


Begin["`Private`"];

(***********************************************************)
(* Utilities                                               *)
(***********************************************************)

Clear[ColumnSpecQ]

ColumnSpecQ[x_] := IntegerQ[x] || StringQ[x] || MatchQ[x, Key[__]];


(***********************************************************)
(* ToLongForm                                              *)
(***********************************************************)

Clear[ToLongForm]

ToLongForm[ds_Dataset, idColumn_?ColumnSpecQ, valueColumn_?ColumnSpecQ] := ToLongForm[ds, {idColumn}, {valueColumn}];

ToLongForm[ds_Dataset, idColumn_?ColumnSpecQ, valueColumns : {_?ColumnSpecQ ..}] := ToLongForm[ds, {idColumn}, valueColumns];

ToLongForm[ds_Dataset, idColumns : {_?ColumnSpecQ ..}, valueColumn_?ColumnSpecQ] := ToLongForm[ds, idColumns, {valueColumn}];

ToLongForm[ds_Dataset] := ToLongForm[ ds, {0}, Range[ Length[ ds[1] ] ] ] ;

(*ToLongForm[ds_Dataset, valueColumn_Integer] := ToLongForm[ds, {0}, {valueColumn}];*)

(*ToLongForm[ds_Dataset, valueColumns : {_Integer ..}] := ToLongForm[ds, {0}, valueColumns];*)

ToLongForm[ds_Dataset, idColumns : {_Integer ..}, valueColumns : {_Integer ..}] :=
    Block[{records = Normal[ds]},

      records =
          Which[
            TrueQ[idColumns == {0}] && MatchQ[records, Association[(_ -> _Association) ..]],
            Association@
                KeyValueMap[<|"RowKey" -> #1|> -> KeyTake[#2, Keys[#2][[valueColumns]]] &, records],

            ! TrueQ[idColumns == {0}] && MatchQ[records, Association[(_ -> _Association) ..]],
            Association@
                Map[KeyTake[#, Keys[#][[idColumns]]] ->
                    KeyTake[#, Keys[#][[valueColumns]]] &, Values[records]],

            TrueQ[idColumns == {0}] && MatchQ[records, List[_Association ..]],
            Association@
                MapIndexed[<|"RowKey" -> #2[[1]]|> -> KeyTake[#1, Keys[#1][[valueColumns]]] &, records],

            MatchQ[records, List[(_Association) ..]],
            Association@
                Map[KeyTake[#, Keys[#][[idColumns]]] ->
                    KeyTake[#, Keys[#][[valueColumns]]] &, records],

            MatchQ[records, List[(_List) ..]],
            Association@
                Map[AssociationThread[ToString/@idColumns, #[[idColumns]]] ->
                    AssociationThread[ToString/@valueColumns, #[[valueColumns]]] &,
                  records],

            True,
            Return[$Failed]
          ];

      RecordsToLongForm[records]

    ] /; (TrueQ[idColumns == {0}] ||
        Apply[And, Map[1 <= # <= Dimensions[ds][[2]] &, idColumns]]) &&
        Apply[And, Map[1 <= # <= Dimensions[ds][[2]] &, valueColumns]] &&
        Length[Intersection[idColumns, valueColumns]] == 0;


ToLongForm::nocolkeys = "If the second and third arguments are not column indices the dataset should have named columns.";

ToLongForm::colkeys = "If the second and third arguments are not column indices then they are expected to be columns names of the dataset.";

ToLongForm[ds_Dataset, idColumns_List, valueColumns_List] :=
    Block[{keys},
      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[ToLongForm::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[ ! Apply[And, Map[ MemberQ[keys, #]&, idColumns ] ] || ! Apply[And, Map[ MemberQ[keys, #]&, valueColumns ] ],
        Message[ToLongForm::colkeys];
        Return[$Failed]
      ];

      ToLongForm[ds, Flatten[Position[keys,#]& /@ idColumns], Flatten[Position[keys,#]& /@ valueColumns] ]
    ];

ToLongForm[ds_Dataset, "RowID", valueColumns_List] :=
    Block[{keys},
      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[ToLongForm::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[ ! Apply[And, Map[ MemberQ[keys, #]&, valueColumns ] ],
        Message[ToLongForm::colkeys];
        Return[$Failed]
      ];

      ToLongForm[ds, 0, Flatten[Position[keys,#]& /@ valueColumns] ]
    ];

ToLongForm::args = "The first argument is expected to be a dataset; \
the rest of the arguments are expected to be columns specifications."

ToLongForm[___] :=
    Block[{},
      Message[ToLongForm::args];
      $Failed
    ];

(* This an "internal" function. It is assumed that all records have the same keys. *)
(* valueColumns is expected to be a list of keys that is a subset of the records keys. *)
RecordsToLongForm[records: Association[(_ -> _Association) ..]] :=
    Block[{res},
      res =
          KeyValueMap[
            Function[{k, rec}, Map[Join[k, <|"Variable" -> #, "Value" -> rec[#]|>] &, Keys[rec]]],
            records
          ];

      Dataset[Flatten[res]]
    ];


(***********************************************************)
(* ToWideForm                                              *)
(***********************************************************)

(* Essentially a contingency dataset making. *)

Clear[ToWideForm];

Options[ToWideForm] = {"AggregationFunction"->Total};

ToWideForm[ ds_Dataset, idColumn_Integer, variableColumn_Integer, valueColumn_Integer, opts:OptionsPattern[] ] :=
    Block[{records = Normal[ds]},

      records =
          Which[
            TrueQ[idColumn == 0] && MatchQ[records, Association[(_ -> _Association) ..]],
            KeyValueMap[ <| "RowID" -> #1, Values[#2][[variableColumn]] -> Values[#2][[valueColumn]] |> &, records],

            ! TrueQ[idColumn == 0] && MatchQ[records, Association[(_ -> _Association) ..]],
            Map[ <| Keys[#][[idColumn]] -> Values[#][[idColumn]] , Values[#][[variableColumn]] -> Values[#][[valueColumn]] |> &, Values[records]],

            MatchQ[records, List[(_Association) ..]],
            Map[ <| Keys[#][[idColumn]] -> Values[#][[idColumn]] , Values[#][[variableColumn]] -> Values[#][[valueColumn]] |> &, records],

            MatchQ[records, List[(_List) ..]],
            Map[ <| idColumn -> #[[idColumn]], #[[variableColumn]] -> #[[valueColumn]] |> &, records],

            True,
            Return[$Failed]
          ];

      RecordsToWideForm[records, OptionValue[ToWideForm, "AggregationFunction"] ]

    ]/; ( idColumn == 0 || 1 <= idColumn <= Dimensions[ds][[2]] ) &&
        ( 1 <= variableColumn <= Dimensions[ds][[2]] ) &&
        ( 1 <= valueColumn <= Dimensions[ds][[2]] ) &&
        ( Length[Union[{idColumn, variableColumn, valueColumn}]] == 3);


ToWideForm::nocolkeys = "If the second and third arguments are not column indices the dataset should have named columns.";

ToWideForm::colkeys = "If the second, third, and fourth arguments are not column indices then they are expected to be columns names of the dataset.";

ToWideForm[ds_Dataset, idColumn_, variableColumn_, valueColumn_, opts:OptionsPattern[] ] :=
    Block[{keys},
      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[ToWideForm::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[ ! Apply[And, Map[ MemberQ[keys, #]&, {idColumn, variableColumn, valueColumn} ] ],
        Message[ToWideForm::colkeys];
        Return[$Failed]
      ];

      ToWideForm[ds, Sequence @@ Flatten[Position[keys,#]& /@ {idColumn, variableColumn, valueColumn}], opts ]
    ];

ToWideForm[ds_Dataset, "RowID", variableColumn_, valueColumn_, opts:OptionsPattern[] ] :=
    Block[{keys},
      keys = Normal[ds[1]];

      If[!AssociationQ[keys],
        Message[ToWideForm::nocolkeys];
        Return[$Failed]
      ];

      keys = Keys[keys];

      If[ ! Apply[And, Map[ MemberQ[keys, #]&, {variableColumn, valueColumn} ] ],
        Message[ToWideForm::colkeys];
        Return[$Failed]
      ];

      ToWideForm[ds, 0, Sequence @@ Flatten[Position[keys,#]& /@ { variableColumn, valueColumn}], opts ]
    ];

ToWideForm::args = "The first argument is expected to be a dataset; \
the rest of the arguments are expected to be columns specifications."

ToWideForm[___] :=
    Block[{},
      Message[ToWideForm::args];
      $Failed
    ];


RecordsToWideForm[records: { (_Association) ..}, aggrFunc_] :=
    Block[{res, colNames},
      res = GroupBy[records, {Keys[#][[1]] -> #[[1]], Keys[#][[2]]} &, aggrFunc@Map[Function[{r}, r[Keys[r][[2]]]], #] &];
      res = KeyValueMap[<|#1[[1]], #1[[2]] -> #2|> &, res];

      res = Dataset[GroupBy[res, #[[1]] &, Join[Association[#]] &]];

      colNames = Union[Flatten[Values[Normal[res[All, Keys]]]]];

      res[All, Join[AssociationThread[colNames -> Missing[]], #]& ]
    ];


End[]; (* `Private` *)

EndPackage[]