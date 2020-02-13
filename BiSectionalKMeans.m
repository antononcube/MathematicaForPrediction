(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: BiSectionalKMeans *)
(* :Context: BiSectionalKMeans` *)
(* :Author: Anton Antonov *)
(* :Date: 2020-02-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 11.3 *)
(* :Copyright: (c) 2020 Anton Antonov *)
(* :Keywords: K-means, Bi-sectional, clustering, hierarchical clustering *)
(* :Discussion:

   # In brief

   # Usage examples

   Make random points:

     pointsPerCluster = 300;
     pnts2D = Flatten[#, 1] &@
     MapThread[
       Transpose[{RandomReal[NormalDistribution[#1, #3], pointsPerCluster],
       RandomReal[NormalDistribution[#2, #3], pointsPerCluster]}] &,
     Transpose[{{10, 20, 4}, {20, 60, 6}, {40, 10, 6}}]];

   Apply K-means:

     clRes = KMeans[pnts2D, 3];
     Show[{ListPlot[clRes["Clusters"], Frame -> True, AspectRatio -> 1, PlotStyle -> PointSize[0.02]],
           ListPlot[clRes["MeanPoints"], PlotStyle -> {PointSize[0.03], Red}]}]

   Apply BiSectionalKMeans:

     clRes = BiSectionalKMeans[pnts2D, 3];
     Show[{ListPlot[clRes["Clusters"], Frame -> True, AspectRatio -> 1, PlotStyle -> PointSize[0.02]],
           ListPlot[clRes["MeanPoints"], PlotStyle -> {PointSize[0.03], Red}]}]

   # Additional comments

   I implemented this algorithm in 2009; it was on of the original algorithms I had in mind for this GitHub repository.

   ---
   Anton Antonov
   Windermere, FL, USA
   2020-02-12

*)

BeginPackage["BiSectionalKMeans`"];

SilhouetteTest::usage = "Compute the Silhouette cluster quality measure";

KMeans::usage = "KMeans[data, k, opts] finds k clusters of data using the K-means clustering algorithm.";

BiSectionalKMeans::usage = "BiSectionalKMeans[data, k, opts] does hierarchical clustering of data.";


Begin["`Private`"];

(************************************************************)
(* Silhouette                                               *)
(************************************************************)

Clear[AverageDistance, Silhouette];

AverageDistance[cluster_, point_, distFunc_ : EuclideanDistance] := Mean[Map[distFunc[#, point] &, cluster]];

Silhouette[clusters_, item_, distFunc_] :=
    Block[{a, b, ds, inCluster, pos, inClusterPos},

      pos = Position[clusters, item][[1]];
      inCluster = clusters[[Sequence @@ Drop[pos, -1]]];
      inClusterPos = pos[[1]];

      a = Mean[Map[distFunc[#, item] &, inCluster]];

      If[Length[clusters] > 1,
        ds =
            Map[
              Mean[Map[distFunc[#, item] &, clusters[[#]]]] &,
              Drop[Range[Length[clusters]], {inClusterPos}]
            ];
        b = Min[ds],
        (*ELSE*)
        b = None
      ];

      If[b === None,
        a,
        (*ELSE*)
        (b - a) / Max[a, b]
      ]
    ];

Silhouette[clusters_, distFunc_] := Mean[Silhouette[clusters, #, distFunc] & /@ Flatten[clusters, 1]];


(************************************************************)
(* SilhouetteTest                                           *)
(************************************************************)

Clear[SilhouetteTest];

SyntaxInformation[SilhouetteTest] = { "ArgumentsPattern" -> { _, OptionsPattern[] } };

Options[SilhouetteTest] = { DistanceFunction -> EuclideanDistance, "NumberOfSamplePoints" -> 60, "Repetitions" -> 4 };

SilhouetteTest[clusters_, opts : OptionsPattern[] ] :=
    Block[{distFunc, nSamplePoints, nReps},

      distFunc = OptionValue[SilhouetteTest, DistanceFunction];
      nSamplePoints = OptionValue[SilhouetteTest, "NumberOfSamplePoints"];
      nReps = OptionValue[SilhouetteTest, "Repetitions"];

      Mean @
          Table[
            Silhouette[If[Length[#] <= nSamplePoints, #, RandomSample[#, nSamplePoints]] & /@ clusters, distFunc],
            nReps
          ]
    ];


(************************************************************)
(* Validity                                                 *)
(************************************************************)

Clear[SumSquaredErrors];
SumSquaredErrors[means_, clusters_, distFunc_] :=
    Block[{},
      Total[MapThread[Function[{c, m}, Total[distFunc[#, m]^2 & /@ c]], {clusters, means}]]
    ] /; Length[clusters] == Length[means];

Clear[Validity];
Validity[means_, clusters_, distFunc_] :=
    Block[{cdMin},
      cdMin = Min[Flatten@Table[distFunc[means[[i]], means[[j]]], {i, 1, Length[means]}, {j, i + 1, Length[means]}]];
      SumSquaredErrors[means, clusters, distFunc] / (Length[means] cdMin^2)
    ] /; Length[clusters] == Length[means];


(************************************************************)
(* KMeans                                                   *)
(************************************************************)

Clear[KMeansDataQ];
KMeansDataQ[ data_ ] := MatrixQ[data, NumericQ];

Clear[KMeans];

SyntaxInformation[KMeans] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

Options[KMeans] = {
  DistanceFunction -> EuclideanDistance, MaxSteps -> 1000, PrecisionGoal -> 6,
  "LearningParameter" -> 0.01, "MinReassignmentsFraction" -> 1/200
};

KMeans::"nargs" = "The first argument is expected to be a numerical matrix, \
the second argument is expected to be a positive integer.";
KMeans::"nfrac" = "The value of the option `1` is expected to be Automatic or a real number in the interval [0, 1).";
KMeans::"npg" = "The value of the option `1` is expected to be Automatic or a positive real number.";
KMeans::"nmrfr" = "The value of the option `1` is expected to be Automatic or a positive real number.";
KMeans::"npi" = "The value of the option `1` is expected to be Automatic or a positive integer.";

KMeans[inputs_SparseArray, nseeds_?IntegerQ, opts : OptionsPattern[]] :=
    KMeans[ Normal[inputs], nseeds, opts ];

KMeans[inputs_?KMeansDataQ, nseeds_?IntegerQ, opts : OptionsPattern[]] :=
    Block[{eta, precGoal, distFunc, maxSteps, clusters, clustersInds,
      j, means, meansOld, meansDiff, nSteps = 0, tol, dMat,
      mvec, minReassignmentFraction, minReassignPoints, clustersIndsOld, newInds,
      indexClusters},

      (* Options *)
      eta = OptionValue[KMeans, "LearningParameter"];
      If[eta === Automatic, eta = 0.01];
      If[! (NumericQ[eta] && 0 <= eta < 1),
        Message[KMeans::"nfrac", "LearningParameter"];
        Return[$Failed];
      ];

      precGoal = OptionValue[KMeans, PrecisionGoal];
      If[precGoal === Automatic, precGoal = 6];
      If[! (NumericQ[precGoal]),
        Message[KMeans::"npg", "PrecisionGoal"];
        Return[$Failed];
      ];

      distFunc = OptionValue[KMeans, DistanceFunction];
      If[distFunc === Automatic, distFunc = EuclideanDistance];

      maxSteps = OptionValue[KMeans, MaxSteps];
      If[maxSteps === Automatic, maxSteps = 100];
      If[ !( IntegerQ[maxSteps] && maxSteps > 0 ),
        Message[KMeans::"npi", "MaxSteps"];
        Return[$Failed];
      ];

      minReassignmentFraction = OptionValue[KMeans, "MinReassignmentsFraction"];
      If[minReassignmentFraction === Automatic, minReassignmentFraction = 0.005];

      If[! (NumericQ[minReassignmentFraction] && 0 <= minReassignmentFraction < 1),
        Message[KMeans::"nfrac", "MinReassignmentsFraction"];
        Return[$Failed];
      ];

      minReassignPoints = Length[inputs] * minReassignmentFraction;

      (* Main algorithm *)

      means = RandomSample[inputs, nseeds];
      meansDiff = Infinity;

      (* newInds is for the number of points that have been re-
      assigned to new cluster centers *)
      newInds = Length[inputs];
      tol = 10^-precGoal;

      While[(meansDiff > tol && newInds >= minReassignPoints) && nSteps < maxSteps,

        nSteps++;

        (* clusterInds[[i]] says to which cluster center input[[i]] is assigned to. *)
        clustersIndsOld = clustersInds;
        meansOld = means;

        dMat = Table[distFunc[means[[i]], inputs[[j]]], {i, 1, Length[means]}, {j, 1, Length[inputs]}];

        If[eta > 0,

          (* standard learning with eta *)
          clustersInds =
              ((mvec = dMat[[All, #1]]; j = Position[mvec, Min[mvec]][[1, 1]];
              means[[j]] = means[[j]] + eta * (inputs[[#1]] - means[[j]]); j) &) /@ Range[Length[inputs]],

          (*ELSE*)
          clustersInds =
              ((mvec = dMat[[All, #1]]; j = Position[mvec, Min[mvec]][[1, 1]];j) &) /@ Range[Length[inputs]];
          means =
              Map[Mean[Pick[inputs, clustersInds /. {# -> True}]] &, Range[Length[means]]];
        ];

        (* find the number of points re-assigned to new cluster centers *)
        If[nSteps > 1,
          newInds = HammingDistance[clustersInds, clustersIndsOld];
        ];

        (* displacement of the cluster centers *)
        meansDiff = Total@MapThread[distFunc[#1, #2] &, {means, meansOld}];
      ];

      clusters = Map[Pick[inputs, clustersInds /. {# -> True}] &, Range[Length[means]]];

      indexClusters = GroupBy[Transpose[{Range[Length[inputs]], clustersInds}], #[[2]] &, #[[All, 1]] &];

      <| "MeanPoints" -> means, "Clusters" -> clusters, "ClusterLabels" -> clustersInds, "IndexClusters" -> indexClusters |>
    ]/; nseeds > 0;

KMeans[___] :=
    Block[{},
      Message[KMeans::"nargs"];
      Return[$Failed]
    ];


(************************************************************)
(* BiSectionalKMeans                                        *)
(************************************************************)

(* Using the following definition of "fold in".
  [fold something in/into something]to combine things that were previously separate so they can be dealt with together.
  https://www.macmillandictionary.com/dictionary/british/fold-in
*)
Clear[BiSectionalKMeans];

SyntaxInformation[BiSectionalKMeans] = { "ArgumentsPattern" -> { _, _, OptionsPattern[] } };

Options[BiSectionalKMeans] =
    Join[
      {"NumberOfTrialBisections" -> 3, "ClusterSelectionFunction" -> "MaxSquaredError", "FoldIn" -> False },
      Options[KMeans]
    ];

BiSectionalKMeans::"nargs" = "The first argument is expected to be a numerical matrix, \
the second argument is expected to be a positive integer.";
BiSectionalKMeans::"npi" = "The value of the option `1` is expected to be Automatic or a positive integer.";
BiSectionalKMeans::"ncls" = "No clusters were obtained; suspecting the specified divisions are too deep. \
Returning last available clusters.";
BiSectionalKMeans::"nclf" = "The value of the option `1` is expected to be one of `2`.";

BiSectionalKMeans[data_SparseArray, k_?IntegerQ, opts : OptionsPattern[]] :=
    BiSectionalKMeans[ Normal[data], k, opts ];

BiSectionalKMeans[data : {{_?NumberQ ...} ...}, k_?IntegerQ, opts : OptionsPattern[]] :=
    Block[{numberOfTrialBisections, distFunc, clusterSelectionMethod, expectedMethodNames,
      clusters, means, sses, sset, s, newMeans,
      newClusters, spos, kInd, clustersToAdd, meansToAdd,
      indexesToDrop = {}, kMeansOpts, kmRes, foldInQ, clustersAcc, meansAcc},

      (* Options *)
      distFunc = OptionValue[BiSectionalKMeans, DistanceFunction];
      If[distFunc === Automatic, distFunc = EuclideanDistance];

      numberOfTrialBisections = OptionValue[BiSectionalKMeans, "NumberOfTrialBisections"];
      If[numberOfTrialBisections === Automatic, numberOfTrialBisections = 3];
      If[ !( IntegerQ[numberOfTrialBisections] && numberOfTrialBisections > 0 ),
        Message[BiSectionalKMeans::"npi", "NumberOfTrialBisections"];
        Return[$Failed];
      ];

      clusterSelectionMethod = OptionValue[BiSectionalKMeans, "ClusterSelectionFunction"];
      If[clusterSelectionMethod === Automatic, clusterSelectionMethod = "MaxSquaredError"];

      expectedMethodNames = {"MaxSize", "MaxLength", "MaxSquaredError"};
      If[ !MemberQ[ ToLowerCase[expectedMethodNames], ToLowerCase[clusterSelectionMethod] ],
        Message[BiSectionalKMeans::nclf, "ClusterSelectionFunction", ToString[expectedMethodNames]];
        Return[$Failed]
      ];

      foldInQ = TrueQ[OptionValue[ BiSectionalKMeans, "FoldIn" ]];

      kMeansOpts = FilterRules[{opts}, Options[KMeans]];

      (* initial cluster *)
      clusters = {data};
      means = Mean /@ clusters;
      If[ foldInQ, clustersAcc = {clusters}; meansAcc = {means}];
      sses = {Total[Map[distFunc[means[[1]], #]^2 &, data]]};

      While[Length[clusters] < k,

        (* Select a cluster to bisect. *)
        Which[
          ToLowerCase[clusterSelectionMethod] == ToLowerCase["MaxSquaredError"],
          spos = First@Flatten@Position[Delete[sses, indexesToDrop], Max[sses]],

          MemberQ[ ToLowerCase[{"MaxSize", "MaxLength"}], ToLowerCase[clusterSelectionMethod] ],
          s = Delete[clusters, indexesToDrop];
          spos = First@Flatten@Position[Length /@ s, Max[Length /@ s]],

          True,
          Message[BiSectionalKMeans::nclf, "ClusterSelectionFunction", ToString[expectedMethodNames]];
          Return[$Failed]
        ];

        If[! NumberQ[spos],
          Message[BiSectionalKMeans::"ncls"];
          If[ foldInQ,
            Return[<| "MeanPoints" -> means, "Clusters" -> clustersAcc |>],
            Return[<| "MeanPoints" -> means, "Clusters" -> clusters |>]
          ]
        ];

        kInd = 0;
        {meansToAdd, clustersToAdd} = {{}, {}};

        Do[
          (* Bisect *)
          kmRes = KMeans[clusters[[spos]], 2, kMeansOpts];
          If[ TrueQ[kmRes === $Failed], Return[$Failed, Block]];

          newMeans = kmRes["MeanPoints"];
          newClusters = kmRes["Clusters"];

          If[Length[newClusters] > 1,

            sset =
                MapThread[
                  Function[{mn, cl}, Total[Map[distFunc[mn, #]^2 &, cl]]],
                  {newMeans, newClusters},
                  1
                ];

            If[kt == 1 || Total[sset] < s,
              s = Total[sset];
              {meansToAdd, clustersToAdd} = {newMeans, newClusters}
            ];

          ],
          {kt, 1, numberOfTrialBisections}
        ];

        If[Length[clustersToAdd] > 0,

          means = Join[Drop[means, {spos}], meansToAdd];
          clusters = Join[Drop[clusters, {spos}], clustersToAdd];
          sses = Join[Drop[sses, {spos}], sset];

          If[ foldInQ,
            AppendTo[clustersAcc, clustersToAdd];
            AppendTo[meansAcc, meansToAdd];
          ],

          (*ELSE*)
          AppendTo[indexesToDrop, {spos}];
        ];
      ];

      If[ foldInQ,
        <| "MeanPoints" -> meansAcc, "Clusters" -> clustersAcc |>,
        (*ELSE*)
        <| "MeanPoints" -> means, "Clusters" -> clusters |>
      ]
    ]/; k > 0;

BiSectionalKMeans[___] :=
    Block[{},
      Message[BiSectionalKMeans::"nargs"];
      Return[$Failed]
    ];

End[]; (* `Private` *)

EndPackage[]