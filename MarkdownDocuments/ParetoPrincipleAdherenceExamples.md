# **Pareto principle adherence examples**

Anton Antonov   
[MathematicaForPrediction blog at WordPress](https://mathematicaforprediction.wordpress.com/)   
[MathematicaForPrediction project at GitHub](https://github.com/antononcube/MathematicaForPrediction)   
March 2016   
July 2016   
October 2016   

## Introduction

This document is made to provide examples of the Pareto principle manifestation in different datasets.

The [Pareto principle](https://en.wikipedia.org/wiki/Pareto_principle) is an interesting law that manifests in many contexts. It is also known as "Pareto law", "the law of significant few", "the 80-20 rule".

For example: 

- "80% of the land is owned by 20% of the population",

- "10% of all lakes contain 90% of all lake water."

For extensive discussion and studied examples see the Wikipedia entry ["Pareto principle"](https://en.wikipedia.org/wiki/Pareto_principle), \[[4](https://en.wikipedia.org/wiki/Pareto_principle)\].

It is a good idea to see for which parts of the analyzed data the Pareto principle manifests. Testing for the Pareto principle is usually simple. For example, assume that we have the GDP of all countries:

    countries = CountryData["Countries"];
    gdps = {CountryData[#, "Name"], CountryData[#, "GDP"]} & /@ countries;
    gdps = DeleteCases[gdps, {_, _Missing}] /. Quantity[x_, _] :> x;

    Grid[{RecordsSummary[gdps, {"country", "GDP"}]}, Alignment -> Top, Dividers -> All]

[![GDPUnsorted1][1]][1]

In order to test for the manifestation of the Pareto principle we have to (i) sort the GDP values in descending order, (ii) find the cumulative sums, (iii) normalize the obtained vector by the sum of all values, and (iv) plot the result. These steps are done with the following two commands:

    t = Reverse@Sort@gdps[[All, 2]];
    ListPlot[Accumulate[t]/Total[t], PlotRange -> All, GridLines -> {{0.2} Length[t], {0.8}}, Frame -> True]

[![GDPPlot1][2]][2]

In this document we are going to use the special function `ParetoLawPlot` defined in the next section and the package \[[1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)\]. Most of the examples use data that is internally accessible within *Mathematica*. Several external data examples are considered. 

See the package \[[1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)\] for the function `RecordsSummary`. See the source file \[2\] for R functions that facilitate the plotting of Pareto principle graphs. See the package \[[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m)\] for the outlier detection functions used below.

## Definitions

This simple function makes a list plot that would help assessing the manifestation of the Pareto principle. It takes the same options as `ListPlot`. 

    Clear[ParetoLawPlot]
    Options[ParetoLawPlot] = Options[ListPlot];
    ParetoLawPlot[dataVec : {_?NumberQ ..}, opts : OptionsPattern[]] := ParetoLawPlot[{Tooltip[dataVec, 1]}, opts];
    ParetoLawPlot[dataVecs : {{_?NumberQ ..} ..}, opts : OptionsPattern[]] := ParetoLawPlot[MapThread[Tooltip, {dataVecs, Range[Length[dataVecs]]}], opts];
    ParetoLawPlot[dataVecs : {Tooltip[{_?NumberQ ..}, _] ..}, opts : OptionsPattern[]] :=
      Block[{t, mc = 0.5},
       t = Map[Tooltip[(Accumulate[#]/Total[#] &)[SortBy[#[[1]], -# &]], #[[2]]] &, dataVecs];
       ListPlot[t, opts, PlotRange -> All, GridLines -> {Length[t[[1, 1]]] Range[0.1, mc, 0.1], {0.8}}, Frame -> True, FrameTicks -> {{Automatic, Automatic}, {Automatic, Table[{Length[t[[1, 1]]] c, ToString[Round[100 c]] <> "%"}, {c, Range[0.1, mc, 0.1]}]}}]
      ];

This function is useful for coloring the outliers in the list plots.

    ClearAll[ColorPlotOutliers]
    ColorPlotOutliers[] := # /. {Point[ps_] :> {Point[ps], Red, Point[ps[[OutlierPosition[ps[[All, 2]]]]]]}} &;
    ColorPlotOutliers[oid_] := # /. {Point[ps_] :> {Point[ps], Red, Point[ps[[OutlierPosition[ps[[All, 2]], oid]]]]}} &;

These definitions can be also obtained by loading the packages 
[MathematicaForPredictionUtilities.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)
and 
[OutlierIdentifiers.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m); see [1,3].

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MathematicaForPredictionUtilities.m"]
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/OutlierIdentifiers.m"]

## Units

Below we are going to use the metric system of units. (If preferred we can easily switch to the imperial system.)

    $UnitSystem = "Metric";(*"Imperial"*)

## CountryData

We are going to consider a typical Pareto principle example -- weatlh of income distribution.

### GDP

This code find the Gross Domestic Product (GDP) of different countries:

    gdps = {CountryData[#, "Name"], CountryData[#, "GDP"]} & /@CountryData["Countries"];
    gdps = DeleteCases[gdps, {_, _Missing}] /. Quantity[x_, _] :> x;

The corresponding Pareto plot (note the default grid lines) shows that 10% of countries have 90% of the wealth:

    ParetoLawPlot[gdps[[All, 2]], ImageSize -> 400]

[![GDPPlot2][3]][3]

Here is the log histogram of the GDP values.

    Histogram[Log10@gdps[[All, 2]], 20, PlotRange -> All]

[![GDPHistogram1][4]][4]

The following code shows the log plot of countries GDP values and the found outliers.

    Manipulate[
     DynamicModule[{data = Transpose[{Range[Length[gdps]], Sort[gdps[[All, 2]]]}], pos},
      pos = OutlierPosition[modFunc@data[[All, 2]], tb@*opar];
      If[Length[pos] > 0,
       ListLogPlot[{data, data[[pos]]}, PlotRange -> All, PlotTheme -> "Detailed", FrameLabel -> {"Index", "GDP"}, PlotLegends -> SwatchLegend[{"All", "Outliers"}]],
       ListLogPlot[{data}, PlotRange -> All, PlotTheme -> "Detailed", FrameLabel -> {"Index", "GDP"}, PlotLegends -> SwatchLegend[{"All", "Outliers"}]]
      ]
     ],
     {{opar, SPLUSQuartileIdentifierParameters, "outliers detector"}, {HampelIdentifierParameters, SPLUSQuartileIdentifierParameters}},
     {{tb, TopOutliers, "bottom|top"}, {BottomOutliers, TopOutliers}},
     {{modFunc, Identity, "data modifier function"}, {Identity, Log}}
    ]

[![Outliers1][5]][5]

This table gives the values for countries with highest GDP.

    Block[{data = gdps[[OutlierPosition[gdps[[All, 2]], TopOutliers@*SPLUSQuartileIdentifierParameters]]]},
     Row[Riffle[#, " "]] &@Map[Grid[#, Dividers -> All, Alignment -> {Left, "."}] &, Partition[SortBy[data, -#[[-1]] &], Floor[Length[data]/3]]]
    ]

[![HighestGDP1][6]][6]

### Population

Similar data retrieval and plots can be made for countries populations.

    pops = {CountryData[#, "Name"], CountryData[#, "Population"]} & /@CountryData["Countries"];
    unit = QuantityUnit[pops[[All, 2]]][[1]];
    pops = DeleteCases[pops, {_, _Missing}] /. Quantity[x_, _] :> x;

In the following Pareto plot we can see that 15% of countries have 80% of the total population:

    ParetoLawPlot[pops[[All, 2]], PlotLabel -> Row[{"Population", ", ", unit}]]

[![PopPlot1][7]][7]

 Here are the countries with most people:

    Block[{data = pops[[OutlierPosition[pops[[All, 2]], TopOutliers@*SPLUSQuartileIdentifierParameters]]]},
     Row[Riffle[#, " "]] &@Map[Grid[#, Dividers -> All, Alignment -> {Left, "."}] &, Partition[SortBy[data, -#[[-1]] &], Floor[Length[data]/3]]]
    ]

[![HighestPop1][8]][8]

### Area

We can also see that the Pareto principle holds for the countries areas:

    areas = {CountryData[#, "Name"], CountryData[#, "Area"]} & /@CountryData["Countries"];
    areas = DeleteCases[areas, {_, _Missing}] /. Quantity[x_, _] :> x;
    ParetoLawPlot[areas[[All, 2]]]

[![AreaPlot1][9]][9]

    Block[{data = areas[[OutlierPosition[areas[[All, 2]], TopOutliers@*SPLUSQuartileIdentifierParameters]]]},
     Row[Riffle[#, " "]] &@Map[Grid[#, Dividers -> All, Alignment -> {Left, "."}] &, Partition[SortBy[data, -#[[-1]] &], Floor[Length[data]/3]]]
    ]

[![HighestArea1][10]][10]

### Time series-wise

An interesting diagram is to plot together the curves of GDP changes for different countries. We can see China and Poland have had rapid growth.

    res = Table[
        (t = CountryData[countryName, {{"GDP"}, {1970, 2015}}];
         t = Reverse@Sort[t["Path"][[All, 2]] /. Quantity[x_, _] :> x];
         Tooltip[t, countryName])
        , {countryName, {"USA", "China", "Poland", "Germany", "France", "Denmark"}}];

    ParetoLawPlot[res, PlotRange -> All, Joined -> True, PlotLegends -> res[[All, 2]]]

[![GDPGrowth1][11]][11]

#### Manipulate

This dynamic interface can be used for a given country to compare (i) the GDP evolution in time and (ii) the corresponding Pareto plot.

    Manipulate[
     DynamicModule[{ts, t},
      ts = CountryData[countryName, {{"GDP"}, {1970, 2015}}];
      t = Reverse@Sort[ts["Path"][[All, 2]] /. Quantity[x_, _] :> x];
      Grid[{{"Date list plot of GDP values", "GDP Pareto plot"}, {DateListPlot[ts, ImageSize -> Medium],
         ParetoLawPlot[t, ImageSize -> Medium]}}]
     ], {countryName, {"USA", "China", "Poland", "Germany", "France", "Denmark"}}]

[![GDPGrowth2][12]][12]

## Country flag colors

The following code demonstrates that the colors of the pixels in country flags also adhere to the Pareto principle.

    flags = CountryData[#, "Flag"] & /@ CountryData["Countries"];

    flags[[1 ;; 12]]

[![Flags1][13]][13]

    ids = ImageData /@ flags;

    pixels = Apply[Join, Flatten[ids, 1]];

    Clear[ToBinFunc]
    ToBinFunc[x_] := Evaluate[Piecewise[MapIndexed[{#2[[1]], #1[[1]] < x <= #1[[2]]} &, Partition[Range[0, 1, 0.1], 2, 1]]]];

    pixelsInt = Transpose@Table[Map[ToBinFunc, pixels[[All, i]]], {i, 1, 3}];

    pixelsIntTally = SortBy[Tally[pixelsInt], -#[[-1]] &];

    ParetoLawPlot[pixelsIntTally[[All, 2]]]

[![FlagsPlot1][14]][14]

## TunnelData

Loking at lengths in the tunnel data we can see the manifestation of an exaggerated Pareto principle.

    tunnelLengths = TunnelData[All, {"Name", "Length"}];
    tunnelLengths // Length

    (* 1552 *)

    t = Reverse[Sort[DeleteMissing[tunnelLengths[[All, -1]]] /. Quantity[x_, _] :> x]];

    ParetoLawPlot[t]

[![TunnelsPlot1][15]][15]

Here is the *logarithmic* histogram of the lengths:

    Histogram[Log10@t, PlotRange -> All, PlotTheme -> "Detailed"]

[![TunnelsHist1][16]][16]


## LakeData

The following code gathers the data and make the Pareto plots surface areas, volumes, and fish catch values for lakes. We can that the lakes volumes show exaggerated Pareto principle.

    lakeAreas = LakeData[All, "SurfaceArea"];
    lakeVolumes = LakeData[All, "Volume"];
    lakeFishCatch = LakeData[All, "CommercialFishCatch"];

    data = {lakeAreas, lakeVolumes, lakeFishCatch};
    t = N@Map[DeleteMissing, data] /. Quantity[x_, _] :> x;

    opts = {PlotRange -> All, ImageSize -> Medium}; MapThread[ParetoLawPlot[#1, PlotLabel -> Row[{#2, ", ", #3}], opts] &, {t, {"Lake area", "Lake volume", "Commercial fish catch"}, DeleteMissing[#][[1, 2]] & /@ data}]

[![LakesPlot1][21]][21]

## City data

One of the examples given in \[[5](https://en.wikipedia.org/wiki/Power_law)\] is that the city areas obey 
the [Power Law](https://en.wikipedia.org/wiki/Power_law). Since the Pareto principle is a kind of Power Law we can confirm 
that observation using Pareto principle plots. 

The following grid of Pareto principle plots is for areas and population sizes of cities in selected states of USA.   

    res = Table[
        (cities = CityData[{All, stateName, "USA"}];
         t = Transpose@Outer[CityData, cities, {"Area", "Population"}];
         t = Map[DeleteMissing[#] /. Quantity[x_, _] :> x &, t, {1}];
         ParetoLawPlot[MapThread[Tooltip, {t, {"Area", "Population"}}], PlotLabel -> stateName, ImageSize -> 250])
        , {stateName, {"Alabama", "California", "Florida", "Georgia", "Illinois", "Iowa", "Kentucky", "Ohio", "Tennessee"}}];

    Legended[Grid[ArrayReshape[res, {3, 3}]], SwatchLegend[Cases[res[[1]], _RGBColor, Infinity], {"Area", "Population"}]]

[![CitiesPlot1][22]][22]

## Movie ratings in MovieLens datasets

Looking into the [MovieLens 20M dataset](http://grouplens.org/datasets/movielens/20m/), \[[6](http://grouplens.org/datasets/movielens/20m/)\], 
we can see that the Pareto princple holds for (1) most rated movies and (2) most active users.
We can also see the manifestation of an exaggerated Pareto law -- 90% of all ratings are for 10% of the movies.

!["MovieLens20M-MDensity-and-Pareto-plots"][25]


## Most popular pin codes

The following plot taken from the blog post "PIN analysis", \[[7](http://www.datagenetics.com/blog/september32012/)\], shows that the four digit passwords people use 
adhere to the Pareto principle: the first 20% of (the unique) most frequently used passwords correspond to the 70% of all passwords use.

    ColorNegate[Import["http://www.datagenetics.com/blog/september32012/c.png"]]

[![Cumulative-4-Digit-Password-Usages-ColorNegated][26]][26]

## References

\[1\] Anton Antonov, ["MathematicaForPrediction utilities"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m), (2014), source code [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction),  https://github.com/antononcube/MathematicaForPrediction, package [MathematicaForPredictionUtilities.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m).

\[2\] Anton Antonov, Pareto principle functions in R, source code [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction),  https://github.com/antononcube/MathematicaForPrediction, source code file ParetoLawFunctions.R .

\[3\] Anton Antonov, [Implementation of one dimensional outlier identifying algorithms in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m), (2013), MathematicaForPrediction at GitHub, URL: https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m .

\[4\] Wikipedia entry, ["Pareto principle"](https://en.wikipedia.org/wiki/Pareto_principle), URL: https://en.wikipedia.org/wiki/Pareto_principle .

\[5\] Wikipedia entry, ["Power law"](https://en.wikipedia.org/wiki/Power_law), URL: https://en.wikipedia.org/wiki/Power_law .

\[6\] GroupLens Research, [MovieLens 20M Dataset](http://grouplens.org/datasets/movielens/20m/), (2015). 

\[7\] ["PIN analysis"](http://datagenetics.com/blog/september32012/index.html), (2012), [DataGenetics](http://datagenetics.com).

<!---
[1]:GDPUnsorted1.png
[2]:GDPPlot1.png
[3]:GDPPlot2.png
[4]:GDPHistogram1.png
[5]:Outliers1.png
[6]:HighestGDP1.png
[7]:PopPlot1.png
[8]:HighestPop1.png
[9]:AreaPlot1.png
[10]:HighestArea1.png
[11]:GDPGrowth1.png
[12]:GDPGrowth2.png
[13]:Flags1.png
[14]:FlagsPlot1.png
[15]:TunnelDataLengthsParetoLaw.png
[16]:TunnelDataLengthsHistogram.png
[17]:Bridges1.png
[18]:BridgesPlot1.png
[19]:BridgesHist1.png
[20]:BridgesPlot2.png
[21]:LakesPlot1.png
[22]:CitiesPlot1.png
[23]:PINPlot1.png
[24]:Tunnels1.png
[25]:MovieLens20M-Matrix-Density-and-Pareto-Law-Plots.png
[26]:Cumulative-4-Digit-Password-Usages-ColorNegated.png
-->
[1]:http://i.imgur.com/alFUwa6.png
[2]:http://i.imgur.com/G9qJhUg.png
[3]:http://i.imgur.com/nnGszOx.png
[4]:http://i.imgur.com/k7y9XzN.png
[5]:http://i.imgur.com/kCLY9Qb.png
[6]:http://i.imgur.com/GqNjTKl.png
[7]:http://i.imgur.com/5C0FUV4.png
[8]:http://i.imgur.com/yNZ6Wdg.png
[9]:http://i.imgur.com/4Ibt0se.png
[10]:http://i.imgur.com/vWtm8bj.png
[11]:http://i.imgur.com/KT1L50z.png
[12]:http://i.imgur.com/vNP3icT.png
[13]:http://i.imgur.com/tzO70c3.png
[14]:http://i.imgur.com/k062rlK.png
[15]:http://i.imgur.com/hB1zb5U.png
[16]:http://i.imgur.com/BnkDGXk.png
[17]:http://i.imgur.com/FrhR6hH.png
[18]:http://i.imgur.com/zj3MZf2.png
[19]:http://i.imgur.com/nRKITnK.png
[20]:http://i.imgur.com/5STChgg.png
[21]:http://i.imgur.com/oGWtUzf.png
[22]:http://i.imgur.com/OjVtqWF.png
[23]:http://i.imgur.com/PhMk4sa.png
[24]:http://i.imgur.com/tLBt49y.png
[25]:http://i.imgur.com/teFPbaK.png
[26]:http://i.imgur.com/up4XkPN.png
