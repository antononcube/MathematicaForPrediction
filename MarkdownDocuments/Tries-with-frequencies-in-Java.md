# Tries with frequencies in Java

Anton Antonov   
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)   
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)   
[MathematicaVsR project at GitHub](https://github.com/antononcube/MathematicaVsR/tree/master/Projects)   
January 2017

## Introduction

This document describes the installation and use in Mathematica of Tries with frequencies [1] implemented in Java [2] through a corresponding Mathematica package [3].

Prefix tree or Trie, [6], is a tree data structure that stores a set of "words" that consist of "characters" -- each element can be seen as a key to itself. The article [1] and packages [2,3,4] extend that data structure to have additional data (frequencies) associated with each key.

The packages [2,3] work with lists of strings only. The package [4] can work with more general data but it is much slower.

The main motivation to create the package [3] was to bring the fast Trie functions implementations of [2] into Mathematica in order to prototype, implement, and experiment with different text processing algorithms. (Like, inductive grammar parsers generation and entity name recognition.) The speed of combining [2] and [3] is evaluated in the section "Performance tests" below.

## Set-up

This following directory path has to have the jar file "TriesWithFrequencies.jar".

    $JavaTriesWithFrequenciesPath = 
      "/Users/antonov/MathFiles/MathematicaForPrediction/Java/\
    TriesWithFrequencies";
    FileExistsQ[
     FileNameJoin[{$JavaTriesWithFrequenciesPath, 
       "TriesWithFrequencies.jar"}]]

    (* True *)

For more details see the explanations in the README file in the [GitHub directory](https://github.com/antononcube/MathematicaForPrediction/tree/master/Java/TriesWithFrequencies) of [2].

The following directory is expected to have the Mathematica package [3].

    dirName = "/Users/antonov/MathFiles/MathematicaForPrediction";
    FileExistsQ[FileNameJoin[{dirName, "JavaTriesWithFrequencies.m"}]]

    (* True *)

    AppendTo[$Path, dirName];
    Needs["JavaTriesWithFrequencies`"]

This commands installs Java (via JLink`) and loads the necessary Java libraries.

    JavaTrieInstall[$JavaTriesWithFrequenciesPath]

## Basic examples

### Creation

Consider this list of words:

    words = {"bar", "bars", "barks", "barkeep", "barn", "balm", "car", 
       "cars", "care", "caress", "card", "cold", "colder"};

We make tries with the lists of their characters. Here is an example:

    JavaTrieForm[
     JavaTrieCreateBySplit[words]](*//Magnify[#,1.2]&*)
    AutoCollapse[]

!["JavaTrieForm"](http://i.imgur.com/WRYWKRE.png)

The nodes of the created trie -- a tree structure -- are pairs of keys and values. Given a node, node's value says how many times node's key appears after the sequence of keys made from the root to the node.

Let us create a trie with the list:

    jTr = JavaTrieCreate[Characters /@ words]

    (* JLink`Objects`vm6`JavaObject17871328887439361 *)

If we have a large list of words the function JavaTrieCreate will be slow since that list is converted to even larger list of lists of characters. For better performance it is better to send the list of words to a Java function that then will do the word splitting (on the Java side). This is done with the function JavaTrieCreateBySplit.

    jTr1 = JavaTrieCreateBySplit[words]

    (* JLink`Objects`vm6`JavaObject10966211349184513 *)

We can convince ourselves that the two Java objects are the same trie:

    JavaTrieEqualQ[jTr, jTr1]

    (* True *)

The function JavaTrieCreateBySplit takes a second argument for the splitting string. For the list of words:

    t = StringRiffle[Characters[#], "~"] & /@ words

    (* {"b~a~r", "b~a~r~s", "b~a~r~k~s", "b~a~r~k~e~e~p", "b~a~r~n", \
    "b~a~l~m", "c~a~r", "c~a~r~s", "c~a~r~e", "c~a~r~e~s~s", "c~a~r~d", \
    "c~o~l~d", "c~o~l~d~e~r"} *)

we can make the following trie with the splitting symbol "~":

    jTr1a = JavaTrieCreateBySplit[t, "~"]

    (* JLink`Objects`vm6`JavaObject28441694324129793 *)

Again we get a trie equivalent to the first one:

    JavaTrieEqualQ[jTr, jTr1a]

    (* True *)

### Data view and visualization

It is important to be able to (i) view the trie data from the Java trie objects and (ii) visualize the Java trie objects.

The packages [2,3] have functions for conversion to JSON.

    JavaTrieToJSON@jTr

    (* {"value" -> 13., "key" -> "", 
     "children" -> {
       {"value" -> 6., "key" -> "b", 
         "children" -> {{"value" -> 6., "key" -> "a", 
            "children" -> {{"value" -> 5., "key" -> "r", 
               "children" -> {{"value" -> 1., "key" -> "s", 
                  "children" -> {}}, {"value" -> 2., "key" -> "k", "children" -> {{"value" -> 1., "key" -> "s", "children" -> {}}, {"value" -> 1., "key" -> "e", 
                     "children" -> {{"value" -> 1., "key" -> "e", "children" -> {{"value" -> 1., "key" -> "p", 
                        "children" -> {}}}}}}}}, {"value" -> 1., "key" -> "n", "children" -> {}}}}, {"value" -> 1., 
              "key" -> "l", "children" -> {{"value" -> 1., "key" -> "m", "children" -> {}}}}}}}}, 
       {"value" -> 7., "key" -> "c", 
         "children" -> {{"value" -> 5., "key" -> "a", 
            "children" -> {{"value" -> 5., "key" -> "r", 
               "children" -> {{"value" -> 1., "key" -> "s", 
                  "children" -> {}}, {"value" -> 1., "key" -> "d", "children" -> {}}, {"value" -> 2., "key" -> "e", 
                    "children" -> {{"value" -> 1., "key" -> "s", "children" -> {{"value" -> 1., "key" -> "s", 
                       "children" -> {}}}}}}}}}}, 
                       {"value" -> 2., "key" -> "o", "children" -> {{"value" -> 2., "key" -> "l", "children" -> {{"value" -> 2., "key" -> "d", 
                 "children" -> {{"value" -> 1., "key" -> "e", "children" -> {{"value" -> 1., "key" -> "r", "children" -> {}}}}}}}}}}}}}} *)

The function JavaTrieForm can be used for visualizing Java tries (and it is similar to TrieForm from [4].)

    JavaTrieForm@jTr

We can also view the trie content by finding all paths from the trie root to its leaves.

    JavaTrieRootToLeafPaths[jTr] // Grid

!["JavaTrieRootToLeafPaths"](http://i.imgur.com/YNvYDAX.png)

### Probabilities

We transform the node frequencies into probabilities using JavaTrieNodeProbabilities.

    JavaTrieComparisonGrid[{jTr, JavaTrieNodeProbabilities[jTr]}, ImageSize -> 350]

!["JavaTrieNodeProbabilities"](http://i.imgur.com/hl5cF34.png)

As it was mentioned above, given a node from the first trie, $\text{jTr}$, node's value says how many times node's key appears after the sequence of keys made from the root to the node.

Given a node of the second trie, $\text{JavaTrieNodeProbabilities}(\text{jTr})$, node's value is the conditional probability of node's key occurrence after the sequence of keys made from the root to the node.

We are going to use the names "frequency trie" and "probabilities trie" to distinguish between the two types of tries.

### Retrieval

The most important functionality for tries is the retrieval. The packages [2,3] provide several retrieval functions that have different outcomes.

    jSubTr = JavaTrieRetrieve[jTr, Characters@"ba"];

    JavaTrieComparisonGrid[{JavaTrieRetrieve[jTr, {"b", "a"}]}, 
     ImageSize -> 250]
    AutoCollapse[]

!["JavaTrieRetrieve-{b,a}"](http://i.imgur.com/rtmfTP3.png)

We can examine the obtained sub-trie by finding the root-to-leaves paths.

    JavaTrieRootToLeafPaths[jSubTr] // Grid

!["JavaTrieRootToLeafPaths-jSubTr"](http://i.imgur.com/wROaWHG.png)

    JavaTrieContains[jTr, Characters@#] & /@ {"ba", "bar", "bard"}

    (* {False, True, False} *)

The function JavaTrieCompleteMatch finds does the prefix of a given word that is in the trie is also a complete word.

    JavaTrieCompleteMatch[jTr, Characters@#] & /@ {"ba", "bar", "bard"}

    (* {False, True, True} *)

To get the words that have a certain prefix we can use the function JavaTrieGetWords.

    ColumnForm@JavaTrieGetWords[jTr, Characters@#] & /@ {"ba", "ca"}

!["JavaTrieGetWords-ba-ca"](http://i.imgur.com/TT3oqDj.png)

### Shrinking

One of the most beneficial and interesting operations over a trie is to shrink it in order to expose its prefixes. This done with the function JavaTrieShrink.

    JavaTrieComparisonGrid[{jTr, JavaTrieShrink[jTr]}, ImageSize -> 350]

!["JavaTrieShrink"](http://i.imgur.com/ObsbDGl.png)

We can do the trie shrinking by inserting a string between the joined node keys.

    JavaTrieComparisonGrid[{jTr, JavaTrieShrink[jTr, "~"]}, ImageSize -> 350]

!["JavaTrieShrink-tilde"](http://i.imgur.com/jwFhPVn.png)

The shrinking can be done based on a threshold specification, given as a third argument to JavaTrieShrink.

    JavaTrieComparisonGrid[{jTr, JavaTrieShrink[jTr, "", 2]}, ImageSize -> 350]

!["JavaTrieShrink-2"](http://i.imgur.com/jwFhPVn.png)

Note that for given frequencies trie the threshold shrinking produces different prefixes for the corresponding probabilities trie.

    JavaTrieComparisonGrid[{JavaTrieNodeProbabilities[jTr], 
      JavaTrieShrink[JavaTrieNodeProbabilities[jTr], "", 0.8]}, 
     ImageSize -> 350]

!["JavaTrieNodeProbabilties-JavaTrieShrink-0.8"](http://i.imgur.com/II3o6f4.png)

### Removal functionalities

There are several functions for removing nodes from tries. 

    funcs = {JavaTrieRegexRemove, JavaTrieThresholdRemove, JavaTrieParetoFractionRemove};
    Grid[Transpose@{funcs,
       Map[Style[#, FontFamily -> "Times"] &,
        {"Removes nodes that have keys adhering to a regex expression.",
         "Removes nodes that have values (or above) a threshold.",
         "Removes nodes that have values below (or above) thresholds from a specified Pareto fraction."}]},
     Alignment -> Left, Dividers -> All, 
     FrameStyle -> LightGray]
    AutoCollapse[]

!["JavaTrie-Remove-functions-table"](http://i.imgur.com/PjXQ33N.png)

Here is an example with JavaTrieRegexRemove:

    JavaTrieComparisonGrid[{JavaTrieShrink[jTr], 
      JavaTrieRegexRemove[JavaTrieShrink[jTr], "s.*"]}, 
     ImageSize -> 250]

!["JavaTrieShrink-JavaTrieRegexRemove-s"](http://i.imgur.com/SXq6isi.png)

Here is an example with JavaTrieThresholdRemove in which the removed branches are replaced with nodes that have the key "®".

    JavaTrieComparisonGrid[{jTr, JavaTrieThresholdRemove[jTr, 2, "®"]}, 
     ImageSize -> 350]

!["JavaTrieThresholdRemove-2r"](http://i.imgur.com/XGsc4Wc.png)

### Node counts

In order to find the number of nodes in a trie we can use the function JavaTrieNodeCounts:

    JavaTrieNodeCounts[jTr]

    (* <|"total" -> 26, "internal" -> 17, "leaves" -> 9|> *)

    JavaTrieNodeCounts[JavaTrieShrink[jTr]]

    (* <|"total" -> 17, "internal" -> 8, "leaves" -> 9|> *)

Further statistics can be obtained through conversion to JSON. Here is an example:

    Quartiles[Cases[JavaTrieToJSON[jTr], ("value" -> v_) :> v, Infinity]]

    (* {1., 1., 5.} *)

## Design issues

### Quick transfer

As it was mentioned above the packages [2,3] work with lists of strings only. 

In order to transfer the lists of strings quickly to the Java Trie objects instead of incremental building of Java (array) lists there is are dedicated functions in [2] that produce a list of lists of strings from a list of strings and or hash-maps. Further [2] has functions to make creation, retrieval, and other functions "listable". These Java functions are used internally in [3] to make the following functions work on lists of strings

    JavaTrieContains[jTr, 
      Characters /@ {"bard", "cat", "car"}] // JavaObjectToExpression

    (* {False, False, True} *)

    JavaTrieCompleteMatch[jTr, 
      Characters /@ {"bard", "cat", "car"}] // JavaObjectToExpression

    (* {True, False, True} *)

    JavaTrieRetrieve[jTr, 
      Characters /@ {"bar", "cat", "car"}] // JavaObjectToExpression

    (* {JLink`Objects`vm6`JavaObject23715158580789249, \
    JLink`Objects`vm6`JavaObject1484934912933889, \
    JLink`Objects`vm6`JavaObject28397044649426945} *)

    With[{res = %}, JavaTrieComparisonGrid[res]]

!["JavaTrieRetrieve-bard-cat-car"](http://i.imgur.com/L2o6mqf.png)

### Trie traversal

The Java tries with frequencies implementation [3] has functions for traversal of tries. (See the signatures of the function map.) Such traversal us a convenient way to implement certain functions. (Like the functions for removal.)

One of the traversal function implementations of [3] changes keys and values, but does not change the shape of the trie. The second traversal function takes function objects that can change the trie shape. These traversal functions cannot be interfaced in the package [4]. (Or only partially.)

Here are the signatures:

    public static Trie map (Trie tr, TrieKeyValueFunction func)

    public static Trie map (Trie tr, TrieNodeFunction preFunc, TrieNodeFunction postFunc)

The second and third arguments are function objects.

## Performance evaluation

Assume we want find the words of "Hamlet" that are not in the book "Origin of Species". This section shows that the Java trie creation and query times for this task are quite small.

### Membership of words

#### Read words

The following code reads the words in the texts. We get $\text{$\$$Failed}$ words from "Hamlet" and $\text{$\$$Failed}$ words from "Origin of Species".

    hWords =
      Block[{words},
       words = 
        StringSplit[
         ExampleData[{"Text", "Hamlet"}], {Whitespace, 
          PunctuationCharacter}];
       words = Select[ToLowerCase[words], StringLength[#] > 0 &]
       ];
    Length[hWords]

    (* 32832 *)

    osWords =
      Block[{words},
       words = 
        StringSplit[
         ExampleData[{"Text", "OriginOfSpecies"}], {Whitespace, 
          PunctuationCharacter}];
       words = Select[ToLowerCase[words], StringLength[#] > 0 &]
       ];
    Length[osWords]

    (* 151205 *)

#### Membership

First we create trie with "Origin of species" words:

    AbsoluteTiming[
     jOStr = JavaTrieCreateBySplit[osWords];
    ]

    (* {0.682531, Null} *)

Sanity check -- the "Origin of species" words are in the trie:

    AbsoluteTiming[
     And @@ JavaObjectToExpression[
       JavaTrieContains[jOStr, Characters /@ osWords]]
    ]

    (* {1.32224, True} *)

Membership of "Hamlet" words into "Origin of Species":

    AbsoluteTiming[
     res = JavaObjectToExpression[
        JavaTrieContains[jOStr, Characters /@ hWords]];
    ]

    (* {0.265307, Null} *)

Tallies of belonging:

    Tally[res]

    (* {{True, 24924}, {False, 7908}} *)

Sample of words from "Hamlet" that do not belong to "Origin of Species":

    RandomSample[Pick[hWords, Not /@ res], 30]

    (* {"rosencrantz", "your", "mar", "airy", "rub", "honesty", \
    "ambassadors", "oph", "returns", "pale", "virtue", "laertes", \
    "villain", "ham", "earnest", "trail", "unhand", "quit", "your", \
    "your", "fishmonger", "groaning", "your", "wake", "thou", "liest", \
    "polonius", "upshot", "drowned", "grosser"} *)

Common words sample:

    RandomSample[Pick[hWords, res], 30]

    (* {"as", "indeed", "it", "with", "wild", "will", "to", "good", "so", \
    "dirt", "the", "come", "not", "or", "but", "the", "why", "my", "to", \
    "he", "and", "you", "it", "to", "potent", "said", "the", "are", \
    "question", "soft"} *)

#### Statistics

The node counts statistics calculation is fast:

    AbsoluteTiming[
     JavaTrieNodeCounts[jOStr]
    ]

    (* {0.002344, <|"total" -> 20723, "internal" -> 15484, "leaves" -> 5239|>} *)

The node counts statistics computation after shrinking is comparably fast :

    AbsoluteTiming[
     JavaTrieNodeCounts[JavaTrieShrink[jOStr]]
    ]

    (* {0.00539, <|"total" -> 8918,  "internal" -> 3679, "leaves" -> 5239|>} *)

The conversion of a large trie to JSON and computing statistics over the obtained tree is reasonably fast:

    AbsoluteTiming[
     res = JavaTrieToJSON[jOStr];
    ]

    (* {0.557221, Null} *)

    AbsoluteTiming[
     Quantile[
      Cases[res, ("value" -> v_) :> v, \[Infinity]], 
      Range[0, 1, 0.1]]
    ]

    (* {0.019644, {1., 1., 1., 1., 2., 3., 5., 9., 17., 42., 151205.}} *)

### Dictionary infixes

Get all words from a dictionary:

    allWords =  DictionaryLookup["*"];
    allWords // Length

    (* 92518 *)

Trie creation and shrinking:

    AbsoluteTiming[
     jDTrie = JavaTrieCreateBySplit[allWords];
     jDShTrie = JavaTrieShrink[jDTrie];
    ]

    (* {0.30508, Null} *)

JSON form extraction:

    AbsoluteTiming[
     jsonRes = JavaTrieToJSON[jDShTrie];
    ]

    (* {3.85955, Null} *)

Here are the node statistics of the original and shrunk tries:

    Grid[{{"Original trie", 
       "Shrunk trie"}, {GridTableForm[
        List @@@ Normal[JavaTrieNodeCounts[jDTrie]], 
        TableHeadings -> {"type", "count"}], 
       GridTableForm[List @@@ Normal[JavaTrieNodeCounts[jDShTrie]], 
        TableHeadings -> {"type", "count"}]}}]
    AutoCollapse[]

!["Orginal-trie-vs-Shrunk-trie-Node-Counts"](http://i.imgur.com/uH0yq5s.png)

Find the infixes that have more than three characters and appear more than 10 times:

    Multicolumn[#, 4] &@
     Select[SortBy[
       Tally[Cases[
         jsonRes, ("key" -> v_) :> v, Infinity]], -#[[-1]] &], StringLength[#[[1]]] > 3 && #[[2]] > 10 &]

!["Long-infixes-in-shrunk-dictionary-trie"](http://i.imgur.com/DtOoGIB.png)

## Unit tests

Many of example shown in this document have corresponding tests in the file [JavaTriesWithFrequencies-Unit-Tests.wlt](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/JavaTriesWithFrequencies-Unit-Tests.wlt) hosted at GitHub.

    tr = TestReport[
      dirName <> "/UnitTests/JavaTriesWithFrequencies-Unit-Tests.wlt"]

!["TestReport"](http://i.imgur.com/kwZvQWo.png)

    tr["TestResultRules"]

!["TestResultRules"](http://i.imgur.com/opwlAaC.png)

## References

[1] Anton Antonov, ["Tries with frequencies for data mining"](https://mathematicaforprediction.wordpress.com/2013/12/06/tries-with-frequencies-for-data-mining/), (2013), [MathematicaForPrediction at WordPress blog](https://mathematicaforprediction.wordpress.com)*. *URL: [https://mathematicaforprediction.wordpress.com/2013/12/06/tries-with-frequencies-for-data-mining/](https://mathematicaforprediction.wordpress.com/2013/12/06/tries-with-frequencies-for-data-mining/) .

[2] Anton Antonov, [Java tries with frequencies Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/JavaTriesWithFrequencies.m), (2017), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), package [JavaTriesWithFrequencies.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/JavaTriesWithFrequencies.m) .

[3] Anton Antonov, [Tries with frequencies in Java](https://github.com/antononcube/MathematicaForPrediction/tree/master/Java/TriesWithFrequencies), (2017), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), project [Java/TriesWithFrequencies](https://github.com/antononcube/MathematicaForPrediction/tree/master/Java/TriesWithFrequencies).

[4] Anton Antonov, [Tries with frequencies Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m), (2013), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), package [TriesWithFrequencies.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/TriesWithFrequencies.m) .

[5] Anton Antonov, [Java tries with frequencies Mathematica unit tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/JavaTriesWithFrequencies-Unit-Tests.wlt), (2017), source code at [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction), unit tests file [JavaTriesWithFrequencies-Unit-Tests.wlt](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/UnitTests/JavaTriesWithFrequencies-Unit-Tests.wlt) .

[6] Wikipedia, [Trie](https://en.wikipedia.org/wiki/Trie), https://en.wikipedia.org/wiki/Trie .

