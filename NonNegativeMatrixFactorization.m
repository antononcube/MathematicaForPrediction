(*
    Non-Negative Matrix Factorization algorithm implementation in Mathematica
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
	antononcube @ gmail . com,
	Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2013 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Version 1.0 *)
(* This package contains definitions for the application of Non-Negative Matrix Factorization (NNMF). *)
(* 
   The implementation follows the description of the hybrid algorithm GD-CLS (Gradient Descent with Constrained Least Squares) in the article:

     Shahnaz, F., Berry, M., Pauca, V., Plemmons, R., 2006.
     Document clustering using nonnegative matrix factorization. Information Processing & Management 42 (2), 373-386.

   In order to use NearestWords a nearest function has to be created over the column indices of the topic matrix.
   For example:

   {W, H} = NormalizeMatrixProduct[W, H];
   HNF = Nearest[Range[Dimensions[H][[2]]], DistanceFunction -> (Norm[H[[All, #1]] - H[[All, #2]]] &)]
   NearestWords[HNF, "agent", termsOfH, stemmingRules, 15]

*)

BeginPackage["NonNegativeMatrixFactorization`"];

GDCLS::usage = "GDCLS[V_?MatrixQ,k_Integer,opts] returns the pair of matrices {W,H} such that V = W H and \
the number of the columns of W and the number of rows of H are k. The method used is called Gradient Descent with Constrained Least Squares.";

GDCLSGlobal::usage = "GDCLSGlobal[V_?MatrixQ,W_?MatrixQ,H_?MatrixQ,opts] continues the GDCLS iterations over the matrices W and H \
in the execution context and returns {W,H} as a result.";

NormalizeMatrixProduct::usage = "NormalizeMatrixProduct[W_?MatrixQ,H_?MatrixQ] returns a pair of matrices {W1,H1} \
such that W1 H1 = W H and the norms of the columns of W1 are 1.";

LeftNormalizeMatrixProduct::usage = "Same as NormalizeMatrixProduct.";

RightNormalizeMatrixProduct::usage = "RightNormalizeMatrixProduct[W_?MatrixQ,H_?MatrixQ] returns a pair of matrices {W1,H1} \
such that W1 H1 = W H and the norms of the rows of H1 are 1.";

BasisVectorInterpretation::usage = "BasisVectorInterpretation[vec_?VectorQ,n_Integer,interpretationItems_List] \
takes the n largest coordinates of vec, finds the corresponding elements in interpretationItems, and returns a list of coordinate-item pairs.";

NearestWords::usage = "NearestWords[HNF, word, terms, stemmingRules, n] calculates a statistical thesaurus entry \
for a specified nearest function over the columns of a matrix of topics and a word.";

Begin["`Private`"];

Clear[GDCLS];

Options[GDCLS] = {"MaxSteps" -> 200, "NonNegative" -> True, "Epsilon" -> 10^-9., "RegularizationParameter" -> 0.01, PrecisionGoal -> Automatic, "PrintProfilingInfo" -> False};

GDCLS[V_?MatrixQ, k_?IntegerQ, opts : OptionsPattern[]] :=
    Block[{t, fls, A, W, H, T, m, n, b, diffNorm, normV, nSteps = 0,
      nonnegQ = OptionValue[GDCLS, "NonNegative"],
      maxSteps = OptionValue[GDCLS, "MaxSteps"],
      eps = OptionValue[GDCLS, "Epsilon"],
      lbd = OptionValue[GDCLS, "RegularizationParameter"],
      pgoal = OptionValue[GDCLS, PrecisionGoal],
      PRINT = If[TrueQ[OptionValue[GDCLS, "PrintProfilingInfo"]], Print, None]},
      {m, n} = Dimensions[V];
      W = RandomReal[{0, 1}, {m, k}];
      H = ConstantArray[0, {k, n}];
      normV = Norm[V, "Frobenius"]; diffNorm = 10 normV;
      If[ pgoal === Automatic, pgoal = 4 ];
      While[nSteps < maxSteps && TrueQ[! NumberQ[pgoal] || NumberQ[pgoal] && (normV > 0) && diffNorm / normV > 10^(-pgoal)],
        nSteps++;
        t =
            Timing[
              A = Transpose[W].W + lbd * IdentityMatrix[k];
              T = Transpose[W];
              fls = LinearSolve[A];
              H = Table[(b = T.V[[All, i]]; fls[b]), {i, 1, n}];
              H = SparseArray[Transpose[H]];
              If[nonnegQ,
                H = Clip[H, {0, Max[H]}]
              ];
              W = W * (V.Transpose[H]) / (W.(H.Transpose[H]) + eps);
            ];
        If[NumberQ[pgoal],
          diffNorm = Norm[V - W.H, "Frobenius"];
          If[nSteps < 100 || Mod[nSteps, 100] == 0, PRINT["step:", nSteps, ", iteration time:", t, " relative error:", diffNorm / normV]],
          If[nSteps < 100 || Mod[nSteps, 100] == 0, PRINT["step:", nSteps, ", iteration time:", t]]
        ];
      ];
      {W, H}
    ];


Clear[GDCLSGlobal];

Options[GDCLSGlobal] = Options[GDCLS];

SetAttributes[GDCLSGlobal, HoldAll];

GDCLSGlobal[V_, W_, H_, opts : OptionsPattern[]] :=
    Block[{t, fls, A, k, T, m, n, b, diffNorm, normV, nSteps = 0,
      nonnegQ = OptionValue[GDCLSGlobal, "NonNegative"],
      maxSteps = OptionValue[GDCLSGlobal, "MaxSteps"],
      eps = OptionValue[GDCLSGlobal, "Epsilon"],
      lbd = OptionValue[GDCLSGlobal, "RegularizationParameter"],
      pgoal = OptionValue[GDCLSGlobal, PrecisionGoal],
      PRINT = If[TrueQ[OptionValue[GDCLSGlobal, "PrintProfilingInfo"]], Print, None]},
      {m, n} = Dimensions[V];
      k = Dimensions[H][[1]];
      normV = Norm[V, "Frobenius"]; diffNorm = 10 normV;
      While[nSteps < maxSteps && TrueQ[! NumberQ[pgoal] || NumberQ[pgoal] && (normV > 0) && diffNorm / normV > 10^(-pgoal)],
        nSteps++;
        t =
            Timing[
              A = Transpose[W].W + lbd * IdentityMatrix[k];
              T = Transpose[W];
              fls = LinearSolve[A];
              H = Table[(b = T.V[[All, i]]; fls[b]), {i, 1, n}];
              H = SparseArray[Transpose[H]];
              If[nonnegQ,
                H = Clip[H, {0, Max[H]}]
              ];
              W = W * (V.Transpose[H]) / (W.(H.Transpose[H]) + eps);
            ];
        If[NumberQ[pgoal],
          diffNorm = Norm[V - W.H, "Frobenius"];
          If[nSteps < 100 || Mod[nSteps, 100] == 0, PRINT[nSteps, " ", t, " relative error=", diffNorm / normV]],
          If[nSteps < 100 || Mod[nSteps, 100] == 0, PRINT[nSteps, " ", t]]
        ];
      ];
      {W, H}
    ] /; MatrixQ[W] && MatrixQ[H] && Dimensions[W][[2]] == Dimensions[H][[1]];

(* ::Subsection:: *)
(*Normalize matrices*)

Clear[NormalizeMatrixProduct];
NormalizeMatrixProduct[W_?MatrixQ, H_?MatrixQ] :=
    Block[{d, S, SI},
      d = Table[Norm[W[[All, i]]], {i, Length[W[[1]]]}];
      S = DiagonalMatrix[d];
      SI = DiagonalMatrix[Map[If[# != 0, 1 / #, 0]&, d]];
      {W.(SI), S.H}
    ];

LeftNormalizeMatrixProduct = NormalizeMatrixProduct;

Clear[RightNormalizeMatrixProduct];
RightNormalizeMatrixProduct[W_?MatrixQ, H_?MatrixQ] :=
    Block[{d, S, SI},
      d = Table[Norm[H[[i]]], {i, Length[H]}];
      S = DiagonalMatrix[d];
      SI = DiagonalMatrix[1 / d];
      {W.S, SI.H}
    ];

Clear[BasisVectorInterpretation];
BasisVectorInterpretation[vec_, n_Integer, terms_] :=
    Block[{t},
      t = Reverse@Ordering[vec, -n];
      Transpose[{vec[[t]], terms[[t]]}]
    ];


Clear[NearestWords];
NearestWords[HNF_NearestFunction, word_String, terms : {_String ..},
  stemmingRules_, n_Integer : 20] :=
    Block[{sword, tpos, inds},
      sword = word /. stemmingRules;
      tpos = Position[terms, sword];
      If[Length[tpos] == 0, {},
        inds = HNF[Flatten[tpos][[1]], n];
        terms[[inds]]
      ]
    ];

End[];

EndPackage[]