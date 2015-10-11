(*
    GitHubPlots Mathematica package
    Copyright (C) 2015  Anton Antonov

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
	antononcube @ gmail.com,
	Windermere, Florida, USA.
*)

(*
    Mathematica is (C) Copyright 1988-2015 Wolfram Research, Inc.

    Protected by copyright law and international treaties.

    Unauthorized reproduction or distribution subject to severe civil
    and criminal penalties.

    Mathematica is a registered trademark of Wolfram Research, Inc.
*)


(* :Title: GitHubPlots *)
(* :Author: Anton Antonov *)
(* :Date: 2015-10-11 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 10.2 *)
(* :Copyright: (c) 2015 Anton Antonov *)
(* :Keywords: GitHub, commit, plot *)
(* :Discussion:

  This package provides the functions GitHubDateListPlot and GitHubBarChart for plotting commits data from GiHub.
  Here is an example:

    GitHubDateListPlot["hadley", "plyr"]

  The functions GitHubDateListPlot and GitHubBarChart take options that can be given to
  the corresponding functions DateListPlot and BarChart. For example,

    GitHubDateListPlot["hadley", "plyr",
      PlotRange -> {{{2015, 04, 01}, {2015, 04, 20}}, {0, 26}},
      ImageSize -> {Automatic, 600}]

    GitHubBarChart["hadley", "plyr", ScalingFunctions -> "Log"]

  To do these kind of data presentation there are several questions / problems to be resolved.

  1. Getting the data.
     GitHub provides a web service API.

  2. Combining text characterizing the commits with graphics of the commits dependencies.
     This is done here with DateListPlot and BarChart: their frame ticks are (some) commit data,
     and their graphic elements are for the commit dependencies.

  3. Parsing the commits data.
     This is done with an ad-hoc parser. I.e. the parser has to be modified if GitHub changes
     the format of the returned results.

  4. Dependencies layout.
     A graph of the commits dependencies is made.
     The roots and the leaves are identified.
     The paths from all roots to all leaves are found.
     The paths a plotted starting with the longests first. In this way the shortest path (e.g. the trunk)
     is going to be most visible, which intuitively makes sense.
     This is not the best approach. The paths are most likely to be many and the layout gets cluttered.
     Note that the function GitHubDateListPlot uses RandomSample to the color of dependency lines.

  TODO
   There are several options that would be nice to be added.
   1. Font size of the ticks, coloring, line thickness, etc.
   2. I am not sure is possible and how to retrieve commits that a password protected.
   3. Hyperlinks to the commits.


  There are probably bugs in the code. I have tested it with only 4-5 repositories.

  This file was created using Mathematica Plugin for IntelliJ IDEA.

  Anton Antonov
  2015-10-11

*)

BeginPackage["GitHubPlots`"]
(* Exported symbols added here with SymbolName::usage *)

GitHubDateListPlot::usage = "Gives a plot of GitHub commits order and dependencies for a specified user and repository."

GitHubBarChart::usage = "Gives a bar chart for the GitHub commits distanses from now for a specified user and repository."

Begin["`Private`"]

(* Parser *)

(* Different entities can be parsed. I have just selected few that seem to be most important. *)

Clear[ParseGitRecord]
ParseGitRecord[recRules_] :=
    Block[{sha, message, committer, date, parentSha, htmlURL},
      sha = "sha" /. recRules;
      message = StringReplace["message" /. ("commit" /. recRules), Whitespace -> " "];
      committer = "name" /. ("committer" /. ("commit" /. recRules));
      date = "date" /. ("committer" /. ("commit" /. recRules));
      parentSha = "sha" /. ("parents" /. recRules);
      htmlURL = "html_url" /. recRules;
      Thread[{"sha", "message", "committer", "date", "parentSha", "htmlURL"} ->
          {sha, message, committer, date, parentSha, htmlURL}]
    ];


Clear[EmptyGitRecord]
EmptyGitRecord[sha_String] := EmptyGitRecord[sha, ""];
EmptyGitRecord[sha_String, date_String] :=
    {"sha" -> sha,
      "commit" -> {"message" -> "none", "committer" -> {"name" -> "unknown", "date" -> date}},
      "parents" -> {"sha" -> "none"}, "htmlURL"->""};

(* Core plot data *)
(* Some parametrizing options would be nice to be added:
   1. should the graph paths be computed or not;
   2. what colors to use for the tick labels;
   3. with what distance to offset the date of the unknown commits.
 *)
CorePlotData[ user_String, repo_String ] :=
    Block[{ url, data, commitRecs,
      graphRules, commitsGraph, unknown, unknownDate,
      roots, leaves, paths, shaInds, pathsByInds, tickLabels, datePoints },

    (* Get data *)
      url = StringTemplate["https://api.github.com/repos/`1`/`2`/commits"];
      data = Import[url[user, repo], "JSON"];

      (* Parse *)
      commitRecs = ParseGitRecord /@ data;
      (*commitRecs = SortBy[commitRecs, "date" /. # &];*) (* This is will be done after adding the unknown. *)

      (* Make a commits graph *)
      graphRules = Flatten@Map[Thread, ("parentSha" -> "sha") /. commitRecs];
      commitsGraph = Graph[DirectedEdge @@@ graphRules];

      (* Find the unknown parents and add empty records for them. *)
      unknown = Complement[Union[Flatten[List @@@ graphRules]], "sha" /. commitRecs];

      unknownDate =
          DateString[
            Min[Map[AbsoluteTime[DateList[#]] &, "date" /. commitRecs]] - 24*3600];

      commitRecs =
          Join[commitRecs,
            Map[{"sha" -> #, "message" -> "none", "committer" -> "unknown",
              "date" -> unknownDate, "parentSha" -> "none"} &, unknown]];

      commitRecs = SortBy[commitRecs, DateList["date" /. #] &];

      (* Find paths *)
      roots = unknown;
      leaves = Complement[graphRules[[All, 2]], graphRules[[All, 1]]];

      paths = Flatten[ Outer[FindPath[commitsGraph, #1, #2, Length[commitRecs], All] &, roots, leaves], 2];

      paths = SortBy[paths, -Length[#] &];

      shaInds = AssociationThread["sha" /. commitRecs, Range[Length[commitRecs]]];

      pathsByInds = Map[shaInds /@ # &, paths];

      (* The plot data *)
      tickLabels = {"message", Style["committer", "Text", GrayLevel[0.7]], Style["date", "Text", GrayLevel[0.8]]} /. commitRecs;
      tickLabels[[All, 1]] =
          StringReplace[#, WhitespaceCharacter -> " "] & /@ tickLabels[[All, 1]];

      tickLabels[[All, 1]] =
          MapThread[
            Function[{mess,url},
              Button[
                Mouseover[Style[mess,"Text"], Style[mess, "HyperlinkActive"]],
                NotebookLocate[{URL[url], None}],
                Appearance -> None]
            ],
            {tickLabels[[All, 1]], "htmlURL" /. commitRecs}];

      datePoints =
          MapThread[{AbsoluteTime[DateList[#1]], #2} &, {"date" /. commitRecs, Range[Length[commitRecs]]}];

      (*Print[tickLabels];*)

      {commitRecs, pathsByInds, datePoints, tickLabels}
    ];


(* DateListPlot based *)
(* It would be nice to be able to specify the commits point sizes and line thickness of the dependencies. *)
GitHubDateListPlot[ user_String, repo_String, opts:OptionsPattern[] ] :=
    Block[{commitRecs, pathsByInds, datePoints, tickLabels, gr},

      {commitRecs, pathsByInds, datePoints, tickLabels} = CorePlotData[user,repo];

      gr = DateListPlot[
        MapThread[Tooltip[#1, #2] &, {datePoints, tickLabels}],
        opts,
        Joined -> False, PlotStyle -> {PointSize[0.03]},
        PlotRange -> All, AspectRatio -> 2, ImageSize -> {Automatic, 800},
        GridLines -> {None, Automatic},
        FrameTicks -> {{Automatic,
          Table[{i, tickLabels[[i]] }, {i, 1, Length[tickLabels]}]}, {Automatic, Automatic}},
        FrameLabel -> {{"commit order", None}, {1, 1} "date"}];

      Show[gr,
        Graphics[{Opacity[0.5], Thickness[0.01],
          MapThread[{ColorData["TemperatureMap"][#2],
            Line[datePoints[[#1]]]} &, {pathsByInds,
            RandomSample[Range[0, 1, 1/(Length[pathsByInds] - 1)]]}],
          Opacity[1], LightBlue, PointSize[0.02],
          MapThread[
            Tooltip[
              Point[#1],
              Row[{#2, " ", DateDifference[#1[[1]], Date[], {"Month", "Day", "Hour", "Minute"}], " ago"}]] &,
            {datePoints, "sha" /. commitRecs}]}]]
    ];


(* BarChart based *)
GitHubBarChart[ user_String, repo_String, opts:OptionsPattern[] ] :=
    Block[{commitRecs, pathsByInds, datePoints, tickLabels, distancesFromNow},

      {commitRecs, pathsByInds, datePoints, tickLabels} = CorePlotData[user,repo];

      distancesFromNow =
          Map[DateDifference[DateList[#], Date[]] &, "date" /. commitRecs];

      BarChart[distancesFromNow, opts,
        BarOrigin -> Right, BarSpacing -> 0.5,
        ChartElementFunction -> "GradientScaleRectangle", Frame -> True,
        GridLines -> Automatic, Method -> {"GridLinesInFront" -> True},
        PlotRangePadding -> {{None, 0.5}, {None, 0.3}},
        FrameLabel -> {{"commit order", None}, {1, 1} "number of days"},
        FrameTicks -> {{Automatic,
          Table[{i, tickLabels[[i]] }, {i, 1, Length[tickLabels]}]}, {Automatic,
          Automatic}},(*ScalingFunctions\[Rule]"Log",*)AspectRatio -> 3,
        ImageSize -> {Automatic, 800}]

    ];

End[] (* `Private` *)

EndPackage[]