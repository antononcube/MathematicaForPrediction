# The Great conversation in USA presidential speeches

Anton Antonov   
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction/)   
[MathematicaVsR at GitHub](https://github.com/antononcube/MathematicaVsR)   
[MatheamticaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)   
October 2017   
December 2017   

## Introduction

This document shows a way to chart in Mathematica / WL the evolution of topics in collections of texts. 
The making of this document (and related code) is primarily motivated by the fascinating concept of the [Great Conversation](https://en.wikipedia.org/wiki/Great_Conversation), [[Wk1](https://en.wikipedia.org/wiki/Great_Conversation), MA1]. 
In brief, all western civilization books are based on $103$ great ideas; if we find the great ideas each significant book is based on we can construct a time-line (spanning centuries) of the great conversation between the authors; see [MA1, MA2, MA3].

Instead of finding the great ideas in a text collection we extract topics statistically, using dimension reduction with [Non-Negative Matrix Factorization (NNMF)](https://en.wikipedia.org/wiki/Non-negative_matrix_factorization), [[AAp3](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m), [AA1](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf), [AA2](https://mathematicaforprediction.wordpress.com/2013/10/15/statistical-thesaurus-from-npr-podcasts/)].

The presented computational results are based on the text collections of State of the Union speeches of USA presidents \[D2\].
The code in this document can be easily configured to use the much smaller text collection [[D1](https://resources.wolframcloud.com/DataRepository/resources/Presidential%2BNomination%2BAcceptance%2BSpeeches)] available online and in Mathematica/WL.
(The collection [[D1](https://resources.wolframcloud.com/DataRepository/resources/Presidential%2BNomination%2BAcceptance%2BSpeeches)] is fairly small, $51$ documents; the collection [D2] is much larger, $2453$ documents.) 

The procedures (and code) described in this document, of course, work on other types of text collections. 
For example: movie reviews, podcasts, editorial articles of a magazine, etc.

A secondary objective of this document is to illustrate the use of the monadic programming pipeline as a [Software design pattern](https://en.wikipedia.org/wiki/Software_design_pattern), [[AA3](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)]. 
In order to make the code concise in this document I wrote the package  [MonadicLatentSemanticAnalysis.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m), [[AAp5](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m)]. 
Compare with the code given in [[AA1](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf)].

The very first version of this document was written for the 2017 summer course ["Data Science for the Humanities"](http://www.dhoxss.net/datascienceforhumanities) at the University of Oxford, UK.

## Outline of the procedure applied

The procedure described in this document has the following steps.

   1. Get a collection of documents with known dates of publishing.

      - Or other types of tags associated with the documents. 

   2. Do preliminary analysis of the document collection.

      - Number of documents; number of unique words.

      - Number of words per document; number of documents per word.

      - (Some of the statistics of this step are done easier after the Linear vector space representation step.)

   3. Optionally perform Natural Language Processing (NLP) tasks.

      1. Obtain or derive [stop words](https://en.wikipedia.org/wiki/Stop_words).

      2. Remove stop words from the texts.

      3. Apply [stemming](https://en.wikipedia.org/wiki/Stemming) to the words in the texts.

   4. Linear vector space representation.

      - This means that we represent the collection with a document-word matrix.

      - Each unique word is a basis vector in that space.

      - For each document the corresponding point in that space is derived from the number of appearances of document's words.

   5. Extract topics.

      - In this document [NNMF](https://en.wikipedia.org/wiki/Non-negative_matrix_factorization) is used.

      - In order to obtain better results with NNMF some experimentation and refinements of the topics search have to be done.

   6. Map the documents over the extracted topics.

      - The original matrix of the vector space representation is replaced with a matrix with columns representing topics (instead of words.)

   7. Order the topics according to their presence across the years (or other related tags).

      - This can be done with hierarchical clustering.

      - Alternatively, 
      
        1. for a given topic find the weighted mean of the years of the documents that have that topic, and 
        
        2. order the topics according to those mean values.

   8. Visualize the evolution of the documents according to their topics.

      1. This can be done by simply finding the contingency matrix year vs topic.

      2. For the president speeches we can use the president names for time-line temporal axis instead of years. 

         + Because the corresponding time intervals of president office occupation do not overlap. 

**Remark:** Some of the functions used in this document combine several steps into one function call (with corresponding parameters.)

## Packages

This loads the packages [AAp1-AAp8]:

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m"];
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicTracing.m"]
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/HeatmapPlot.m"];
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/RSparseMatrix.m"];

(Note that some of the packages that are imported automatically by [[AAp5](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m)].)

The functions of the central package in this document, [[AAp5](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m)], have the prefix "LSAMon". Here is a sample of those names:

    Short@Names["LSAMon*"]

    (* {"LSAMon", "LSAMonAddToContext", "LSAMonApplyTermWeightFunctions", <<27>>, "LSAMonUnit", "LSAMonUnitQ", "LSAMonWhen"} *)

## Data load

In this section we load a text collection from a specified source. 

The text collection from ["Presidential Nomination Acceptance Speeches"](https://resources.wolframcloud.com/DataRepository/resources/Presidential%2BNomination%2BAcceptance%2BSpeeches), [D1], is small and can be used for multiple code verifications and re-runnings. The "State of Union addresses of USA presidents" text collection from [D2] was converted to a Mathematica/WL object by Christopher Wolfram (and sent to me in a private communication.) The text collection [D2] provides far more interesting results (and they are shown below.)

    If[True,
      speeches = ResourceData[ResourceObject["Presidential Nomination Acceptance Speeches"]];
      names = StringSplit[Normal[speeches[[All, "Person"]]][[All, 2]], "::"][[All, 1]],

      (*ELSE*)
      (*State of the union addresses provided by Christopher Wolfram. *)      
      Get["~/MathFiles/Digital humanities/Presidential speeches/speeches.mx"];
      names = Normal[speeches[[All, "Name"]]];
    ];

    dates = Normal[speeches[[All, "Date"]]];
    texts = Normal[speeches[[All, "Text"]]];

    Dimensions[speeches]

    (* {2453, 4} *)



## Basic statistics for the texts

Using different [contingency matrices](https://en.wikipedia.org/wiki/Contingency_table) we can derive basic statistical information about the document collection. (The document-word matrix is a contingency matrix.)

First we convert the text data in long-form:

    docWordRecords = 
      Join @@ MapThread[
        Thread[{##}] &, {Range@Length@texts, names, 
         DateString[#, {"Year"}] & /@ dates, 
         DeleteStopwords@*TextWords /@ ToLowerCase[texts]}, 1];

Here is a sample of the rows of the long-form:

    GridTableForm[RandomSample[docWordRecords, 6], 
     TableHeadings -> {"document index", "name", "year", "word"}]

[!["Speeches-words-long-form-rows-sample"](https://imgur.com/3a1hoLzl.png)](https://imgur.com/3a1hoLz.png)

Here is a summary:
  
    Multicolumn[
     RecordsSummary[docWordRecords, {"document index", "name", "year", "word"}, "MaxTallies" -> 8], 4, Dividers -> All, Alignment -> Top]

[!["USA-presidents-speeches-words-long-form-summary"](https://imgur.com/ASBFWQ6l.png)](https://imgur.com/ASBFWQ6.png)

Using the long form we can compute the document-word matrix:

    ctMat = CrossTabulate[docWordRecords[[All, {1, -1}]]];
    MatrixPlot[Transpose@Sort@Map[# &, Transpose[ctMat@"XTABMatrix"]], 
     MaxPlotPoints -> 300, ImageSize -> 800, 
     AspectRatio -> 1/3]

[!["USA-presidents-speeches-doc-vs-term-contingency-matrix"](https://imgur.com/k9vTR1Ml.png)](https://imgur.com/k9vTR1M.png)

Here is the president-word matrix:

    ctMat = CrossTabulate[docWordRecords[[All, {2, -1}]]];
    MatrixPlot[Transpose@Sort@Map[# &, Transpose[ctMat@"XTABMatrix"]], MaxPlotPoints -> 300, ImageSize -> 800, AspectRatio -> 1/3]

[!["USA-presidents-speeches-president-vs-term-contingency-matrix"](https://imgur.com/2MLCq7pl.png)](https://imgur.com/2MLCq7p.png)

Here is an alternative way to compute text collection statistics through the document-word matrix computed within the monad `LSAMon`:

    LSAMonUnit[texts]⟹LSAMonEchoTextCollectionStatistics[];

[!["USA-presidents-speeches-LSAMonEchoTextCollectionStatistics"](https://imgur.com/8c48rxtl.png)](https://imgur.com/8c48rxt)

## Procedure application

### Stop words

Here is one way to obtain [stop words](https://en.wikipedia.org/wiki/Stop_words):

    stopWords = Complement[DictionaryLookup["*"], DeleteStopwords[DictionaryLookup["*"]]];
    Length[stopWords]
    RandomSample[stopWords, 12]

    (* 304 *)

    (* {"has", "almost", "next", "WHO", "seeming", "together", "rather", "runners-up", "there's", "across", "cannot", "me"} *)

We can complete this list with additional stop words derived from the collection itself. (Not done here.)

### Linear vector space representation and dimension reduction

**Remark:** In the rest of the document we use "term" to mean "word" or "stemmed word".

The following code makes a document-term matrix from the document collection, exaggerates the representations of the terms using ["TF-IDF"](https://en.wikipedia.org/wiki/Tfâidf), and then does topic extraction through dimension reduction. The dimension reduction is done with [NNMF](https://en.wikipedia.org/wiki/Non-negative_matrix_factorization); see  [[AAp3](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m), [AA1](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf), [AA2](https://mathematicaforprediction.wordpress.com/2013/10/15/statistical-thesaurus-from-npr-podcasts/)].

    SeedRandom[312]

    mObj =
      LSAMonUnit[texts]⟹
       LSAMonMakeDocumentTermMatrix[{}, stopWords]⟹
       LSAMonApplyTermWeightFunctions[]⟹
       LSAMonTopicExtraction[Max[5, Ceiling[Length[texts]/100]], 60, 12, "MaxSteps" -> 6, "PrintProfilingInfo" -> True];


This table shows the pipeline commands above with comments: 

[!["LSAMon-sample-pineline"](https://imgur.com/cX2S5EF.png)](https://imgur.com/cX2S5EF.png)

#### Detailed description

The monad object `mObj` has a context of named values that is an Association with the following keys:

    Keys[mObj⟹LSAMonTakeContext]

    (* {"texts", "docTermMat", "terms", "wDocTermMat", "W", "H", "topicColumnPositions", "automaticTopicNames"} *)

Let us clarify the values by briefly describing the computational steps.

   1. From texts we derive the document-term matrix $\text{docTermMat}\in \mathbb{R}^{m \times n}$, where $n$ is the number of documents and $m$ is the number of terms.

      - The terms are words or stemmed words.

      - This is done with `LSAMonMakeDocumentTermMatrix`.

   2. From `docTermMat` is derived the (weighted) matrix wDocTermMat using ["TF-IDF"](https://en.wikipedia.org/wiki/Tfâidf).

      - This is done with `LSAMonApplyTermWeightFunctions`.

   3. Using `docTermMat` we find the terms that are present in sufficiently large number of documents and their column indices are assigned to topicColumnPositions.

   4. Matrix factorization.

      1. Assign to $\text{wDocTermMat}[[\text{All},\text{topicsColumnPositions}]]$, $\text{wDocTermMat}[[\text{All},\text{topicsColumnPositions}]]\in \mathbb{R}^{m_1 \times n}$, where $m_1 = |topicsColumnPositions|$.

      2. Compute using NNMF the factorization $\text{wDocTermMat}[[\text{All},\text{topicsColumnPositions}]]\approx H W$, where $W\in \mathbb{R}^{k \times n}$, $H\in \mathbb{R}^{k \times m_1}$, and $k$ is the number of topics.

      3. The values for the keys "W, "H", and "topicColumnPositions" are computed and assigned by `LSAMonTopicExtraction`.

   5. From the top terms of each topic are derived automatic topic names and assigned to the key `automaticTopicNames` in the monad context.

      - Also done by `LSAMonTopicExtraction`.

### Statistical thesaurus

At this point in the object `mObj` we have the factors of NNMF. Using those factors we can find a statistical thesaurus for a given set of words. The following code calculates such a thesaurus, and echoes it in a tabulated form.

    queryWords = {"arms", "banking", "economy", "education", "freedom", 
       "tariff", "welfare", "disarmament", "health", "police"};

    mObj⟹
      LSAMonStatisticalThesaurus[queryWords, 12]⟹
      LSAMonEchoStatisticalThesaurus[];

[!["USA-presidents-speeches-statistical-thesaurus"](https://imgur.com/bTPrbfJ.png)](https://imgur.com/bTPrbfJ.png)

By observing the thesaurus entries we can see that the words in each entry are semantically related. 

Note, that the word "welfare" strongly associates with "[applause]". The rest of the query words do not, which can be seen by examining larger thesaurus entries:

    thRes =
      mObj⟹
       LSAMonStatisticalThesaurus[queryWords, 100]⟹
       LSAMonTakeValue;
    Cases[thRes, "[applause]", Infinity]

    (* {"[applause]", "[applause]"} *)

The second "[applause]" associated word is "education".

#### Detailed description

The statistical thesaurus is computed by using the NNMF's right factor $H$.

For a given term, its corresponding column in $H$ is found and the nearest neighbors of that column are found in the space $\mathbb{R}^{m_1}$ using Euclidean norm.

### Extracted topics

The topics are the rows of the right factor $H$ of the factorization obtained with NNMF .

Let us tabulate the topics found above with `LSAMonTopicExtraction` :

    mObj⟹ LSAMonEchoTopicsTable["NumberOfTerms" -> 6, "MagnificationFactor" -> 0.8, Appearance -> "Horizontal"];

[!["USA-presidents-speeches-topics"](https://imgur.com/SvjWjQol.png)](https://imgur.com/SvjWjQo.png)

### Map documents over the topics

The function `LSAMonTopicsRepresentation` finds the top outliers for each row of NNMF's left factor $W$. (The outliers are found using the package [[AAp4](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m)].) The obtained list of indices gives the topic representation of the collection of texts.

    Short@(mObj⟹LSAMonTopicsRepresentation[]⟹LSAMonTakeContext)["docTopicIndices"]
    
    {{53}, {47, 53}, {25}, {46}, {44}, {15, 42}, {18}, <<2439>>, {30}, {33}, {7, 60}, {22, 25}, {12, 13, 25, 30, 49, 59}, {48, 57}, {14, 41}}

Further we can see that if the documents have tags associated with them -- like author names or dates -- we can make a contingency matrix of tags vs topics. (See [[AAp8](https://github.com/antononcube/MathematicaForPrediction/blob/master/CrossTabulate.m), [AA4](https://mathematicaforprediction.wordpress.com/2016/10/04/contingency-tables-creation-examples/)].)
This is also done by the function `LSAMonTopicsRepresentation` that takes tags as an argument. If the tags argument is `Automatic`, then the tags are simply the document indices.

Here is a an example:

    rsmat = mObj⟹LSAMonTopicsRepresentation[Automatic]⟹LSAMonTakeValue;
    MatrixPlot[rsmat]

[!["USA-presidents-speeches-document-vs-topic-contigency-matrix-plot"](https://imgur.com/y8ezXJzl.png)](https://imgur.com/y8ezXJz.png)

Here is an example of calling the function LSAMonTopicsRepresentation with arbitrary tags.

    rsmat = mObj⟹LSAMonTopicsRepresentation[DateString[#, "MonthName"] & /@ dates]⟹LSAMonTakeValue;
    MatrixPlot[rsmat]

[!["USA-presidents-speeches-dateMonth-vs-topic-contigency-matrix-plot"](https://imgur.com/0TZYBnM.png)](https://imgur.com/0TZYBnM.png)

Note that the matrix plots  above are very close to the charting of the Great conversation that we are looking for. This can be made more obvious by observing the row names and columns names in the tabulation of the transposed matrix `rsmat`:

    Magnify[#, 0.6] &@MatrixForm[Transpose[rsmat]]

[!["USA-presidents-speeches-dateMonth-vs-topic-contigency-matrix-form"](https://imgur.com/iZFngoil.png)](https://imgur.com/iZFngoi.png)



## Charting the great conversation

In this section we show several ways to chart the Great Conversation in the collection of speeches. 

There are several possible ways to make the chart: using a time-line plot, using heat-map plot, and using appropriate tabulation (with `MatrixForm` or `Grid`).

In order to make the code in this section more concise the package [RSparseMatrix.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RSparseMatrix.m), \[[AAp7](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RSparseMatrix.m), [AA5](https://mathematicaforprediction.wordpress.com/2015/10/08/rsparsematrix-for-sparse-matrices-with-named-rows-and-columns/)\], is used.

### Topic name to topic words

This command makes an Association between the topic names and the top topic words.

    aTopicNameToTopicTable = 
      AssociationThread[(mObj⟹LSAMonTakeContext)["automaticTopicNames"], 
       mObj⟹LSAMonTopicsTable["NumberOfTerms" -> 12]⟹LSAMonTakeValue];

Here is a sample:

    Magnify[#, 0.7] &@ aTopicNameToTopicTable[[1 ;; 3]]

[!["USA-presidents-speeches-topic-name-to-topic-words-sample"](https://imgur.com/HRanSAEl.png)](https://imgur.com/HRanSAE.png)

### Time-line plot

This command makes a contingency matrix between the documents and the topics (as described above):

    rsmat = ToRSparseMatrix[mObj⟹LSAMonTopicsRepresentation[Automatic]⟹LSAMonTakeValue]

This time-plot shows great conversation in the USA presidents state of union speeches:

    TimelinePlot[
     Association@
      MapThread[
       Tooltip[#2, aTopicNameToTopicTable[#2]] -> dates[[ToExpression@#1]] &, 
       Transpose[RSparseMatrixToTriplets[rsmat]]], 
     PlotTheme -> "Detailed", ImageSize -> 1000, AspectRatio -> 1/2, PlotLayout -> "Stacked"]

[!["USA-presidents-speeches-great-conversation-time-line-plot-large"](https://imgur.com/RNVLsVIl.png)](https://imgur.com/RNVLsVI.png)

The plot is too cluttered, so it is a good idea to investigate other visualizations.

### By topic vs president

We can use the USA president names instead of years in the Great Conversation chart because the USA presidents terms do not overlap.

This makes a contingency matrix presidents vs topics:

    rsmat2 = ToRSparseMatrix[
       mObj⟹LSAMonTopicsRepresentation[
         names]⟹LSAMonTakeValue];

Here we compute the chronological order of the presidents based on the dates of their speeches:

    nameToMeanYearRules = 
      Map[#[[1, 1]] -> Mean[N@#[[All, 2]]] &, 
       GatherBy[MapThread[List, {names, ToExpression[DateString[#, "Year"]] & /@ dates}], First]];
    ordRowInds = Ordering[RowNames[rsmat2] /. nameToMeanYearRules];

This heat-map plot uses the (experimental) package [HeatmapPlot.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/HeatmapPlot.m), \[[AAp6](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/HeatmapPlot.m)\]:

    Block[{m = rsmat2[[ordRowInds, All]]},
     HeatmapPlot[SparseArray[m], RowNames[m], 
      Thread[Tooltip[ColumnNames[m], aTopicNameToTopicTable /@ ColumnNames[m]]],
      DistanceFunction -> {None, Sort}, ImageSize -> 1000, 
      AspectRatio -> 1/2]
     ]

[!["USA-presidents-speeches-great-conversation-heatmap"](https://imgur.com/xJvddq3l.png)](https://imgur.com/xJvddq3.png)

Note the value of the option `DistanceFunction`: there is not re-ordering of the rows and columns are reordered by sorting. Also, the topics on the horizontal names have tool-tips.

## References

### Text data

[D1] Wolfram Data Repository, ["Presidential Nomination Acceptance Speeches"](https://resources.wolframcloud.com/DataRepository/resources/Presidential%2BNomination%2BAcceptance%2BSpeeches).

[D2] US Presidents, [State of the Union Addresses](https://books.google.com/books?id=eRRYCwAAQBAJ), Trajectory, 2016. ‪ISBN‬1681240009, 9781681240008‬.

[D3] Gerhard Peters, ["Presidential Nomination Acceptance Speeches and Letters, 1880-2016"](http://www.presidency.ucsb.edu/nomination.php), [The American Presidency Project](http://www.presidency.ucsb.edu/index.php).

[D4] Gerhard Peters, ["State of the Union Addresses and Messages"](http://www.presidency.ucsb.edu/sou.php), [The American Presidency Project](http://www.presidency.ucsb.edu/index.php).

### Packages

[AAp1] Anton Antonov, [MathematicaForPrediction utilities ](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)*[Mathematica ](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)*[package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m), (2014), [MathematicaForPrediction at GitHub.](https://github.com/antononcube/MathematicaForPrediction)

[AAp2] Anton Antonov, [Implementation of document-term matrix construction and re-weighting functions in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m)[, ](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)(2013), [MathematicaForPrediction at GitHub.](https://github.com/antononcube/MathematicaForPrediction)

[AAp3] Anton Antonov, [Implementation of the Non-Negative Matrix Factorization algorithm in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m), (2013), [MathematicaForPrediction at GitHub.](https://github.com/antononcube/MathematicaForPrediction)

[AAp4] Anton Antonov, [Implementation of one dimensional outlier identifying algorithms in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m), (2013), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AAp5] Anton Antonov, [Monadic latent semantic analysis Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AAp6] Anton Antonov, [Heatmap plot Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/HeatmapPlot.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AAp7] Anton Antonov, [RSparseMatrix Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RSparseMatrix.m), (2015), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AAp8] Anton Antonov, [Cross tabulation implementation in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/CrossTabulate.m), (2017), [MathematicaForPrediction at GitHub.](https://github.com/antononcube/MathematicaForPrediction)

### Books and articles

[AA1] Anton Antonov, $$ (2013),  [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.*

[AA2] Anton Antonov, ["Statistical thesaurus from NPR podcasts"](https://mathematicaforprediction.wordpress.com/2013/10/15/statistical-thesaurus-from-npr-podcasts/), (2013), [MathematicaForPrediction at WordPress blog](https://mathematicaforprediction.wordpress.com)*.*

[AA3] Anton Antonov, ["Monad code generation and extension](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)", (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.*

[AA4] Anton Antonov, ["Contingency tables creation examples"](https://mathematicaforprediction.wordpress.com/2016/10/04/contingency-tables-creation-examples/), (2016), [MathematicaForPrediction at WordPress blog](https://mathematicaforprediction.wordpress.com)*.*

[AA5] Anton Antonov, ["RSparseMatrix for sparse matrices with named rows and columns"](https://mathematicaforprediction.wordpress.com/2015/10/08/rsparsematrix-for-sparse-matrices-with-named-rows-and-columns/), (2015), [MathematicaForPrediction at WordPress blog](https://mathematicaforprediction.wordpress.com)*.*

[Wk1] Wikipedia entry, [Great Conversation](https://en.wikipedia.org/wiki/Great_Conversation).

[MA1] Mortimer Adler, "The Great Conversation Revisited," in The Great Conversation: A Peoples Guide to Great Books of the Western World, Encyclopædia Britannica, Inc., Chicago,1990, p. 28.

[MA2] Mortimer Adler, ["Great Ideas"](https://www.thegreatideas.org/greatideas1.html).

[MA3] Mortimer Adler, ["How to Think About the Great Ideas: From the Great Books of Western Civilization"](https://www.goodreads.com/book/show/136043.How_to_Think_About_the_Great_Ideas), 2000, Open Court.

