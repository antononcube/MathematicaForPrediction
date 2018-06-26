(*
    Monadic quantile regression Mathematica unit tests
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

(* :Title: MonadicContextualClassification-Unit-Tests *)
(* :Author: Anton Antonov *)
(* :Date: 2018-06-24 *)

(* :Package Version: 0.7 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2018 Anton Antonov *)
(* :Keywords: monad, monadic, quantile regression, workflow, State monad, Mathematica, Wolfram Language, unit test *)
(* :Discussion:

   This file has units tests for the package

     https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m

*)
BeginTestSection["MonadicQuantileRegression-Unit-Tests"]

VerificationTest[(* 1 *)
  CompoundExpression[
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"],
    Greater[Length[SubValues[MonadicQuantileRegression`QRMonQuantileRegression]], 0]
  ]
  ,
  True
  ,
  TestID->"LoadPackage"
]

VerificationTest[(* 2 *)
  data = Table[{x, Exp[-x^2] + RandomVariate[NormalDistribution[0, .15]]}, {x, -3, 3, .2}];
  MatrixQ[data, NumberQ]
  ,
  True
  ,
  TestID->"GenerateData"
]

VerificationTest[(* 3 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegression[ 12, Range[0.2,0.8,0.2] ],
        QRMonTakeContext
      ];
  Keys[res["regressionFunctions"]]
  ,
  Range[0.2,0.8,0.2]
  ,
  TestID->"QuantileRegression-1"
]

VerificationTest[(* 4 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegression[ 12 ],
        QRMonTakeContext
      ];
  Keys[res["regressionFunctions"]]
  ,
  { 0.25, 0.5, 0.75 }
  ,
  TestID->"QuantileRegression-2"
]

VerificationTest[(* 5 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegression[ 12, Range[0.2,0.8,0.2] ],
        QRMonLeastSquaresFit[ {1, x, Exp[-x^2]} ],
        QRMonTakeContext
      ];
  Sort @ Keys[res["regressionFunctions"]]
  ,
  Sort @ Prepend[Range[0.2,0.8,0.2], "mean"]
  ,
  TestID->"QuantileRegression-and-Fit-1"
]

VerificationTest[(* 6 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonLeastSquaresFit[ {1, x, Exp[-x^2]} ],
        QRMonQuantileRegression[ 12, Range[0.2,0.8,0.2] ],
        QRMonTakeContext
      ];
  Sort @ Keys[res["regressionFunctions"]]
  ,
  Sort @ Prepend[Range[0.2,0.8,0.2], "mean"]
  ,
  TestID->"Fit-and-QuantileRegression-1"
]


VerificationTest[(* 7 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegressionFit[ {1, x, Exp[-x^2]}, Range[0.2,0.8,0.2] ],
        QRMonLeastSquaresFit[ {1, x, Exp[-x^2]} ],
        QRMonTakeContext
      ];
  Sort @ Keys[res["regressionFunctions"]]
  ,
  Sort @ Prepend[Range[0.2,0.8,0.2], "mean"]
  ,
  TestID->"QuantileRegressionFit-and-Fit-1"
]


VerificationTest[(* 8 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonLeastSquaresFit[ {1, x, Exp[-x^2]} ],
        QRMonQuantileRegressionFit[ {1, x, Exp[-x^2]}, Range[0.2,0.8,0.2] ],
        QRMonTakeContext
      ];
  Sort @ Keys[res["regressionFunctions"]]
  ,
  Sort @ Prepend[Range[0.2,0.8,0.2], "mean"]
  ,
  TestID->"Fit-and-QuantileRegressionFit-1"
]


VerificationTest[(* 9 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonOutliers,
        QRMonTakeValue
      ];
  MatrixQ[#, NumberQ] & /@ KeyTake[res, {"topOutliers", "bottomOutliers"}]
  ,
  <|"topOutliers" -> True, "bottomOutliers" -> True|>
  ,
  TestID->"Outliers-1"
]


VerificationTest[(* 10 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonOutliers["Knots" -> 6, "TopOutliersQuantile" -> 0.98],
        QRMonTakeValue
      ];
  MatrixQ[#, NumberQ] & /@ KeyTake[res, {"topOutliers", "bottomOutliers"}]
  ,
  <|"topOutliers" -> True, "bottomOutliers" -> True|>
  ,
  TestID->"Outliers-2"
]


VerificationTest[(* 11 *)
  VectorQ[
    DoubleLongRightArrow[
      QRMonUnit[data],
      QRMonQuantileRegression[12, Range[0.1, 0.9, 0.2]],
      QRMonGridSequence,
      QRMonTakeValue
    ],
    IntegerQ]
  ,
  True
  ,
  TestID->"GridSequence-1"
]


VerificationTest[(* 11 *)
  VectorQ[
    DoubleLongRightArrow[
      QRMonUnit[data],
      QRMonQuantileRegression[12, Range[0.1, 0.9, 0.2]],
      QRMonBandsSequence,
      QRMonTakeValue
    ],
    IntegerQ]
  ,
  True
  ,
  TestID->"BandsSequence-1"
]


VerificationTest[(* 12 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegression[6, Range[0.1, 0.9, 0.2]],
        QRMonConditionalCDF[0.2],
        QRMonTakeValue
      ];
  {MatchQ[res, <|0.2 -> _InterpolatingFunction|>], res[0.2]["ValuesOnGrid"] == Range[0.1, 0.9, 0.2] }
  ,
  { True, True }
  ,
  TestID->"ConditionalCDF-1"
]


VerificationTest[(* 13 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegression[12, Range[0.2, 0.8, 0.2]],
        QRMonEvaluate[4],
        QRMonTakeValue
      ];
  MatchQ[res, <|(_?NumberQ -> _?NumberQ) ..|>]
  ,
  True
  ,
  TestID->"Evaluate-1"
]


VerificationTest[(* 14 *)
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegression[12, Range[0.2, 0.8, 0.2]],
        QRMonLeastSquaresFit[{1, x, Exp[-x^2]}],
        QRMonEvaluate[4],
        QRMonTakeValue
      ];
  Sort[Keys[res]]
  ,
  Sort[Prepend[Range[0.2, 0.8, 0.2],"mean"]]
  ,
  TestID->"Evaluate-2"
]


VerificationTest[(* 15 *)
  points = {3, 4, {1, 2}};
  res =
      DoubleLongRightArrow[
        QRMonUnit[data],
        QRMonQuantileRegression[12, Range[0.2, 0.8, 0.2]],
        QRMonEvaluate[points],
        QRMonTakeValue
      ];
  MatchQ[res, <|(_?NumberQ -> {_?NumberQ, _?NumberQ, {_?NumberQ, _?NumberQ}}) ..|>]
  ,
  True
  ,
  TestID->"Evaluate-3"
]


EndTestSection[]
