# Call graph generation for context functions

Anton Antonov   
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)   
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)   
[MathematicaVsR at GitHub](https://github.com/antononcube/MathematicaVsR)  
January 2019

## In brief

This document describes the package 
[CallGraph.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/CallGraph.m) 
for making call graphs between the functions that belong to specified contexts.

The main function is `CallGraph` that gives a graph with vertices that are functions names and edges that show 
which functions call which other functions. With the default option values the graph vertices are labeled and 
have tooltips with function usage messages.

## General design

The call graphs produced by the main package function `CallGraph` are assumed to be used for studying or refactoring 
of large code bases written with Mathematica / Wolfram Language.

The argument of `CallGraph` is a context string or a list of context strings.

With the default values of its options `CallGraph` produces a graph with labeled nodes and the labels have tooltips 
that show the usage messages of the functions from the specified contexts.
It is assumed that this would be the most useful call graph type for studying the codes of different sets of packages.

We can make simple, non-label, non-tooltip call graph using `CallGraph[ ... , "UsageTooltips" -> False ]`.

The simple call graph can be modified with the functions:

    CallGraphAddUsageMessages, CallGraphAddPrintDefinitionsButtons, CallGraphBiColorCircularEmbedding

Each of those functions is decorating the simple graph in a particular way.

## Package load

This loads the package [CallGraph.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/CallGraph.m) :

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/CallGraph.m"]

The following packages are used in the examples below.

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicQuantileRegression.m"]

    Get["https://raw.githubusercontent.com/szhorvat/IGraphM/master/IGInstaller.m"];
    Needs["IGraphM`"]

## Usage examples

### Generate a call graph with usage tooltips

    CallGraph["IGraphM`", GraphLayout -> "SpringElectricalEmbedding", ImageSize -> Large]

[![IGraphM-call-graph-with-usage-tooltips](https://i.imgur.com/ShOVJMVl.png)](https://i.imgur.com/ShOVJMV.png)

### Generate a call graph by excluding symbols

    gr = CallGraph["IGraphM`", Exclusions -> Map[ToExpression, Names["IG*Q"]], ImageSize -> 900]


[![IGraphM-call-graph-with-exclusions](https://i.imgur.com/n2y0KrKl.png)](https://i.imgur.com/n2y0KrK.png)

### Generate call graph with buttons to print definitions

    gr0 = CallGraph["IGraphM`", "UsageTooltips" -> False];
    gr1 = CallGraphAddPrintDefinitionsButtons[gr0, GraphLayout -> "StarEmbedding", ImageSize -> 900]

[![IGraphM-call-graph-with-definition-buttons](https://i.imgur.com/wbRcNoEl.png)](https://i.imgur.com/wbRcNoE.png)

### Generate circular embedding graph color

    cols = RandomSample[ ColorData["Rainbow"] /@ Rescale[Range[VertexCount[gr1]]]];

    CallGraphBiColorCircularEmbedding[ gr1, "VertexColors" -> cols, ImageSize -> 900 ]

[![IGraphM-call-graph-bicolor-Bezie](https://i.imgur.com/BMgO1rEl.png)](https://i.imgur.com/BMgO1rE.png)

(The core functions used for the implementation of `CallGraphBiColorCircularEmbedding` were taken from kglr's Mathematica Stack Exchange answer: https://mathematica.stackexchange.com/a/188390/34008 . Those functions were modified to take additional arguments.)

## Options

The package functions "CallGraph*" take all of the options of the function Graph.
Below are described the additional options of CallGraph.

- "PrivateContexts"   
Should the functions of the private contexts be included in the call graph.

- "SelfReferencing"   
Should the self referencing edges be excluded or not.

- "AtomicSymbols"   
Should atomic symbols be included in the call graph.

- Exclusions   
Symbols to be excluded from the call graph.

- "UsageTooltips"   
Should vertex labels with the usage tooltips be added.

- "UsageTooltipsStyle"   
The style of the usage tooltips.

## Possible issues

- With large context (e.g. "System`") the call graph generation might take long time. (See the TODOs below.)

- With "PrivateContexts"->False the call graph will be empty if the public functions do not depend on each other.

- For certain packages the scanning of the down values would produce (multiple) error messages or warnings.

## Future plans

The following is my TODO list for this project.

1. Special handling for the "System`" context.

2. Use the symbols up-values to make the call graph.

3. Consider/implement call graph making with specified patterns and list of symbols.

   - Instead of just using contexts and exclusions. (The current approach/implementation.)

4. Provide special functions for "call sequence" tracing for a specified symbol.


