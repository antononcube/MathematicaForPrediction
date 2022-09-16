(*
    Neural network graph Mathematica package
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
    ʇǝu˙oǝʇsod@ǝqnɔuouoʇuɐ
    Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2022 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)

(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: NeuralNetworkGraph *)
(* :Context: NeuralNetworkGraph` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-09-16 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.1 *)
(* :Copyright: (c) 2022 Anton Antonov *)
(* :Keywords: *)
(* :Discussion:

Based on codes in the discussions:

- ["Neural network illustrations"](https://mathematica.stackexchange.com/q/175686/34008)
- ["Please share neural network illustrations"](https://community.wolfram.com/groups/-/m/t/1360660)

*)

BeginPackage["NeuralNetworkGraph`"];

NeuralNetworkGraph::usage = "NeuralNetworkGraph[layerCounts : ( {_Integer...} | _Association )] \
produces a graph that illustrates a neural network.";

Begin["`Private`"];

Clear[NeuralNetworkGraph];

Options[NeuralNetworkGraph] = Options[Graph];

NeuralNetworkGraph[layerCounts : {_Integer ..}, opts:OptionsPattern[]] :=
    NeuralNetworkGraph[AssociationThread[Row[{"layer ", #}] & /@ Range@Length[layerCounts], layerCounts], opts];

NeuralNetworkGraph[namedLayerCounts_Association, opts:OptionsPattern[]] :=
    Block[{graphUnion, graph, vstyle,
      layerCounts = Values[namedLayerCounts],
      layerCountsNames = Keys[namedLayerCounts]},
      graphUnion[g_?GraphQ] := g;
      graphUnion[g__?GraphQ] := GraphUnion[g];
      graph =
          graphUnion @@
              MapThread[
                IndexGraph, {CompleteGraph /@ Partition[layerCounts, 2, 1],
                FoldList[Plus, 0, layerCounts[[;; -3]]]}];
      vstyle =
          Catenate[
            Thread /@
                Thread[TakeList[VertexList[graph], layerCounts] ->
                    ColorData[97] /@ Range@Length[layerCounts]]];
      graph =
          Graph[graph, opts,
            GraphLayout -> {"MultipartiteEmbedding",
              "VertexPartition" -> layerCounts}, GraphStyle -> "BasicBlack",
            VertexSize -> 0.5, VertexStyle -> vstyle];
      Legended[graph,
        Placed[PointLegend[ColorData[97] /@ Range@Length[layerCounts],
          layerCountsNames, LegendMarkerSize -> 30, LegendLayout -> "Row"], Below]]
    ];

End[]; (* `Private` *)

EndPackage[]