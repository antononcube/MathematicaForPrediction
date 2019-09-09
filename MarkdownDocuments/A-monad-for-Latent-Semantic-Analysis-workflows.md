# A monad for Latent Semantic Analysis workflows

Anton Antonov   
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)  
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)   
[MathematicaVsR at GitHub](https://github.com/antononcube/MathematicaVsR/tree/master/Projects)   
September 2019

## Introduction

In this document we describe the design and implementation of a (software programming) monad, [Wk1], for [Latent Semantic Analysis](https://en.wikipedia.org/wiki/Latent_semantic_analysis) workflows specification and execution. The design and implementation are done with Mathematica / Wolfram Language (WL).

**What is Latent Semantic Analysis (LSA)? :** A statistical method (or a technique) for finding relationships in natural language texts that is based on the so called [Distributional hypothesis](https://en.wikipedia.org/wiki/Distributional_semantics), [Wk2, Wk3]. (The Distributional hypothesis can be simply stated as "linguistic items with similar distributions have similar meanings"; for insightful philosophical and scientific discussion see [MS1].) LSA can be seen as the application of [Dimensionality reduction](https://en.wikipedia.org/wiki/Dimensionality_reduction) techniques over matrices derived with the [Vector space model](https://en.wikipedia.org/wiki/Vector_space_model).

The goal of the monad design is to make the specification of LSA workflows (relatively) easy, straightforward, by following a certain main scenario and specifying variations over that scenario.

The monad is named `LSAMon` and it is based on the State monad package ["StateMonadCodeGenerator.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m), [[AAp1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m), [AA1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)], the document-term matrix making package ["DocumentTermMatrixConstruction.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m), [AAp4, AA2], the Non-Negative Matrix Factorization (NNMF) package ["NonNegativeMatrixFactorization.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m), [AAp5, AA2], and the package "SSparseMatrix.m", [AAp2, AA5], that provides matrix objects with named rows and columns.

The data for this document is obtained from WL's repository and it is manipulated into a certain ready-to-utilize form (and uploaded to GitHub.)

The monadic programming design is used as a [Software Design Pattern](https://en.wikipedia.org/wiki/Software_design_pattern). The LSAMon monad can be also seen as a [Domain Specific Language](https://en.wikipedia.org/wiki/Domain-specific_language) (DSL) for the specification and programming of machine learning classification workflows.  

Here is an example of using the `LSAMon` monad over a collection of documents that consists of 233 US state of union speeches.



The table above is produced with the package ["MonadicTracing.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m), [[AAp2](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m), [AA1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)], and some of the explanations below also utilize that package.

As it was mentioned above the monad `LSAMon` can be seen as a DSL. Because of this the monad pipelines made with `LSAMon` are sometimes called "specifications".

**Remark:** With "*term*" we mean "a word, a word stem, or other type of token". 

**Remark:** LSA and Latent Semantic Indexing (LSI) are considered more or less to be synonyms. I think that "latent semantic analysis" sounds more universal and that "latent semantic indexing" as a name refers to a specific Information Retrieval technique. Below we refer to "LSI functions" like "IDF" and "TF-IDF" that are applied within the generic LSA workflow.

### Contents description

The document has the following structure.

   + The sections "Package load" and "Data load" obtain the needed code and data.

      + (Needed and put upfront from the ["Reproducible research"](https://en.wikipedia.org/wiki/Reproducibility#Reproducible_research) point of view.)

   + The sections "Design consideration" and "Monad design" provide motivation and design decisions rationale.

   + The sections "LSAMon overview" and "Monad elements" provide technical descriptions needed to utilize the `LSAMon` monad .

      + (Using a fair amount of examples.)

   + The section "Unit tests" describes the tests used in the development of the `LSAMon` monad.

      + (The random pipelines unit tests are especially interesting.)

   + The section "Future plans" outlines future directions of development.

      + (The most interesting and important one is the ["conversational agent"](https://github.com/antononcube/ConversationalAgents/tree/master/Projects/TimeSeriesWorkflowsAgent) direction.)

   + The section "Implementation notes" just says that `LSAMon`'s development process and this document follow the ones of the classifications workflows monad ClCon, [[AA6](https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows/)].

**Remark:** One can read only the sections "Introduction", "Design consideration", "Monad design", and "LSAMon overview". That set of sections provide a fairly good, programming language agnostic exposition of the substance and novel ideas of this document.

## Package load

The following commands load the packages 
[[AAp1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m)--AAp7]:

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m"]
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicTracing.m"]

## Data load

In this section we load data that is used in the rest of the document. The text data was obtained through WL's repository, transformed in a certain more convenient form, and uploaded to GitHub.

The text summarization and plots are done through `LSAMon`, which in turn uses the function RecordsSummary from the package ["MathematicaForPredictionUtilities.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m), [[AAp7](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m)].

### Hamlet

    textHamlet = 
      ToString /@ 
       Flatten[Import["https://raw.githubusercontent.com/antononcube/MathematicaVsR/master/Data/MathematicaVsR-Data-Hamlet.csv"]];

    TakeLargestBy[
     Tally[DeleteStopwords[ToLowerCase[Flatten[TextWords /@ textHamlet]]]], #[[2]] &, 20]

    (* {{"ham", 358}, {"lord", 225}, {"king", 196}, {"o", 124}, {"queen", 120}, 
        {"shall", 114}, {"good", 109}, {"hor", 109}, {"come",  107}, {"hamlet", 107}, 
        {"thou", 105}, {"let", 96}, {"thy", 86}, {"pol", 86}, {"like", 81}, {"sir", 75}, 
        {"'t", 75}, {"know", 74}, {"enter", 73}, {"th", 72}} *)
  
    LSAMonUnit[textHamlet]⟹LSAMonMakeDocumentTermMatrix⟹LSAMonEchoDocumentTermMatrixStatistics;

![LSAMon-Data-Load-Hamlet-echo](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Data-Load-Hamlet-echo.png)


### USA state of union speeches 

    url = "https://github.com/antononcube/MathematicaVsR/blob/master/Data/MathematicaVsR-Data-StateOfUnionSpeeches.JSON.zip?raw=true";
    str = Import[url, "String"];
    filename = First@Import[StringToStream[str], "ZIP"];
    aStateOfUnionSpeeches = Association@ImportString[Import[StringToStream[str], {"ZIP", filename, "String"}], "JSON"];

    lsaObj = 
    LSAMonUnit[aStateOfUnionSpeeches]⟹
    LSAMonMakeDocumentTermMatrix⟹
    LSAMonEchoDocumentTermMatrixStatistics["LogBase" -> 10];

![LSAMon-Data-Load-StateOfUnionSpeeches-echo](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Data-Load-StateOfUnionSpeeches-echo.png)

    TakeLargest[ColumnSumsAssociation[lsaObj⟹LSAMonTakeDocumentTermMatrix], 12]

    (* <|"government" -> 7106, "states" -> 6502, "congress" -> 5023, 
         "united" -> 4847, "people" -> 4103, "year" -> 4022, 
         "country" -> 3469, "great" -> 3276, "public" -> 3094, "new" -> 3022, 
         "000" -> 2960, "time" -> 2922|> *)

### Stop words

In some of the examples below we want to explicitly specify the
 [stop words](https://en.wikipedia.org/wiki/Stop_words). 
 Here are stop words derived using the built-in functions `DictionaryLookup` and `DeleteStopwords`.

    stopWords = Complement[DictionaryLookup["*"], DeleteStopwords[DictionaryLookup["*"]]];

    Short[stopWords]
    
    (* {"a", "about", "above", "across", "add-on", "after", "again", <<290>>, 
       "you'll", "your", "you're", "yours", "yourself", "yourselves", "you've" } *)
        
## Design considerations

The steps of the main LSA workflow addressed in this document follow.

   1. Get a collection of documents with associated ID's.

   2. Create a document-term matrix.

      1. Here we apply the [Bag-or-words model](https://en.wikipedia.org/wiki/Bag-of-words_model) and [Vector space model](https://en.wikipedia.org/wiki/Vector_space_model). 

         1. The sequential order of the words is ignored and each document is represented as a point in a multi-dimensional vector space. 

         2. That vector space axes correspond to the unique words found in the whole document collection.

      2. Consider the application of [stemming](https://en.wikipedia.org/wiki/Stemming) rules.

      3. Consider the removal of stop words.

   3. Apply matrix-entries weighting functions.

      1. Those functions come from LSI.

      2. Functions like "IDF", "TF-IDF", "GFIDF".

   4. Extract topics.

      1. One possible statistical way of doing this is with Dimensionality reduction.

      2. We consider using Singular Value Decomposition (SVD) and Non-Negative Matrix Factorization (NNMF).

   5. Make and display the topics table.

   6. Extract and display a statistical thesaurus of selected words.

   7. Map search queries or unseen documents over the extracted topics.

   8. Find the most important documents in the document collection. (Optional.)

The following flow-chart corresponds to the list of steps above.

![LSA-worflows](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSA-workflows.jpg)

In order to address:

   + the introduction of new elements in LSA workflows,

   + workflows elements variability, and

   + workflows iterative changes and refining,

it is beneficial to have a DSL for LSA workflows. We choose to make such a DSL through a 
[functional programming monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)), 
[[Wk1](https://en.wikipedia.org/wiki/Monad_(functional_programming)), 
[AA1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)].

Here is a quote from [[Wk1](https://en.wikipedia.org/wiki/Monad_(functional_programming))] that fairly well describes why we choose to make a classification workflow monad and hints on the desired properties of such a monad.

> [...] The monad represents computations with a sequential structure: a monad defines what it means to chain operations together. This enables the programmer to build pipelines that process data in a series of steps (i.e. a series of actions applied to the data), in which each action is decorated with the additional processing rules provided by the monad. [...]

Monads allow a programming style where programs are written by putting together highly composable parts, combining in flexible ways the possible actions that can work on a particular type of data. [...]

**Remark:** Note that quote from [[Wk1](https://en.wikipedia.org/wiki/Monad_(functional_programming))] 
refers to chained monadic operations as "pipelines". We use the terms "monad pipeline" and "pipeline" below.

## Monad design

The monad we consider is designed to speed-up the programming of LSA workflows outlined in the previous section. 
The monad is named `LSAMon` for "**L**atent **S**emantic **A**nalysis** Mon**ad".

We want to be able to construct monad pipelines of the general form:

![LSAMon-Monad-Design-formula-1](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Monad-Design-formula-1.png)

`LSAMon` is based on the [State monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)#State_monads), 
[[Wk1](https://en.wikipedia.org/wiki/Monad_(functional_programming)), 
[AA1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)], 
so the monad pipeline form (1) has the following more specific form:

![LSAMon-Monad-Design-formula-2](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Monad-Design-formula-2.png)

This means that some monad operations will not just change the pipeline value but they will also change the pipeline context.

In the monad pipelines of `LSAMon` we store different objects in the contexts for at least one of the following two reasons.

   1. The object will be needed later on in the pipeline, or

   2. The object is (relatively) hard to compute. 

Such objects are document-term matrix, Dimensionality reduction factors and the related topics.

Let us list the desired properties of the monad.

   + Rapid specification of non-trivial LSA workflows.

   + The monad works with associations with string values, list of strings.

   + The monad use the Linear vector spaces model .

   + The document-term frequency matrix is can be created after removing stop words and/or word stemming.

   + It is easy to specify and apply different LSI weight functions. (Like "IDF" or "GFIDF".)

   + The monad can do dimension reduction with SVD and NNMF and corresponding matrix factors are retrievable with monad functions.

   + Documents (or query strings) external to the monad a easily mapped into monad's Linear vector space of terms and the Linear vector space of topics.

   + The monad allows of cursory examination and summarization of the data.

   + The pipeline values can be of different types. Most monad functions modify the pipeline value; some modify the context; some just echo results.

   + It is easy to obtain the pipeline value, context, and different context objects for manipulation outside of the monad.

   + It is easy to tabulate extracted topics and related statistical thesauri.

   + It is easy to specify and apply re-weighting functions for the entries of the document-term contingency matrices.

The `LSAMon` components and their interactions are fairly simple.

The main `LSAMon` operations implicitly put in the context or utilize from the context the following objects: 

   + document-term matrix, 

   + the factors obtained by matrix factorization algorithms,

   + extracted topics.

Note the that the monadic set of types of `LSAMon` pipeline values is fairly heterogenous and certain awareness of 
"the current pipeline value" is assumed when composing `LSAMon` pipelines.

Obviously, we can put in the context any object through the generic operations of the State monad of the package ["StateMonadGenerator.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md), [[AAp1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)].

## LSAMon overview

When using a monad we lift certain data into the "monad space", using monad's operations we navigate computations in that space, and at some point we take results from it. 

With the approach taken in this document the "lifting" into the `LSAMon` monad is done with the function LSAMonUnit. 
Results from the monad can be obtained with the functions `LSAMonTakeValue`, `LSAMonContext`, 
or with the other `LSAMon` functions with the prefix "LSAMonTake" (see below.)

Here is a corresponding diagram of a generic computation with the `LSAMon` monad:

![LSAMon-pipeline](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-pipeline.jpg)

**Remark:** It is a good idea to compare the diagram with formulas (1) and (2).

Let us examine a concrete `LSAMon` pipeline that corresponds to the diagram above. In the following table each pipeline operation is combined together with a short explanation and the context keys after its execution.

Here is the output of the pipeline:

The `LSAMon` functions are separated into four groups:

   + operations,

   + setters and droppers,

   + takers,

   + State Monad generic functions.

### Monad functions interaction with the pipeline value and context

An overview of the those functions is given in the tables in next two sub-sections. 
The next section, "Monad elements", gives details and examples for the usage of the `LSAMon` operations.

![LSAMon-Overview-operations-context-interactions-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Overview-operations-context-interactions-table.png)

![LSAMon-Overview-setters-droppers-takers-context-interactions-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Overview-setters-droppers-takers-context-interactions-table.png)

### State monad functions

Here are the `LSAMon` State Monad functions (generated using the prefix "LSAMon", [AAp1, AA1].)

![LSAMon-Overview-StMon-usage-descriptions-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Overview-StMon-usage-descriptions-table.png)

### Main monad functions

Here are the usage descriptions of the main (not monad-supportive) `LSAMon` functions, 
which are explained in detail in the next section.

![LSAMon-Overview-operations-usage-descriptions-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Overview-operations-usage-descriptions-table.png)

## Monad elements

In this section we show that `LSAMon` has all of the properties listed in the previous section.

### The monad head

The monad head is `LSAMon`. Anything wrapped in `LSAMon` can serve as monad's pipeline value. 
It is better though to use the constructor `LSAMonUnit`. (Which adheres to the definition in [Wk1].)

    LSAMon[textHamlet, <||>]⟹LSAMonMakeDocumentTermMatrix[Automatic, Automatic]⟹LSAMonEchoFunctionContext[Short];

### Lifting data to the monad

The function lifting the data into the monad QRMon is QRMonUnit.

The lifting to the monad marks the beginning of the monadic pipeline. It can be done with data or without data. Examples follow.

    LSAMonUnit[textHamlet]⟹LSAMonMakeDocumentTermMatrix⟹LSAMonTakeDocumentTermMatrix

    LSAMonUnit[]⟹LSAMonSetDocuments[textHamlet]⟹LSAMonMakeDocumentTermMatrix⟹LSAMonTakeDocumentTermMatrix

(See the sub-section "Setters, droppers, and takers" for more details of setting and taking values in `LSAMon` contexts.)

Currently the monad can deal with data in the following forms: 

   + vectors of strings,

   + associations with string values.

Generally, WL makes it easy to extract columns datasets order to obtain vectors or matrices, 
so datasets are not currently supported in `LSAMon`.

### Making of the document-term matrix

As it was mentioned above with "term" we mean "a word or a stemmed word". Here is are examples of stemmed words.

    WordData[#, "PorterStem"] & /@ {"consequential", "constitution", "forcing", ""}

The fundamental model of `LSAMon` is the so called [Vector space model](https://en.wikipedia.org/wiki/Vector_space_model) 
(or the closely related [Bag-of-words model.)](https://en.wikipedia.org/wiki/Bag-of-words_model)
The document-term matrix is a linear vector space representation of the documents collection. 
That representation is further used in `LSAMon` to find topics and statistical thesauri.

Here is an example of ad hoc construction of a document-term matrix using a couple of paragraphs from "Hamlet".

    inds = {10, 19};
    aTempText = AssociationThread[inds, textHamlet[[inds]]]

    MatrixForm @ CrossTabulate[Flatten[KeyValueMap[Thread[{#1, #2}] &, TextWords /@ ToLowerCase[aTempText]], 1]]

When we construct the document-term matrix we (often) want to stem the words and (almost always) want to remove [stop words](https://en.wikipedia.org/wiki/Stop_words). 
`LSAMon`'s function LSAMonMakeDocumentTermMatrix makes the document-term matrix and takes specifications for stemming and stop words.

    lsaObj =
      LSAMonUnit[textHamlet]⟹
       LSAMonMakeDocumentTermMatrix["StemmingRules" -> Automatic, "StopWords" -> Automatic]⟹
       LSAMonEchoFunctionContext[ MatrixPlot[#documentTermMatrix] &]⟹
       LSAMonEchoFunctionContext[TakeLargest[ColumnSumsAssociation[#documentTermMatrix], 12] &];

We can retrieve the stop words used in a monad with the function LSAMonTakeStopWords.

    Short[lsaObj⟹LSAMonTakeStopWords]

We can retrieve the stemming rules used in a monad with the function LSAMonTakeStemmingRules.

    Short[lsaObj⟹LSAMonTakeStemmingRules]

The specification Automatic for stemming rules uses `WordData[#,"PorterStem"]&`.

Instead of the options style signature we can use positional signature.

- Options style: `LSAMonMakeDocumentTermMatrix["StemmingRules" -> {}, "StopWords" -> Automatic]` .

- Positional style: `LSAMonMakeDocumentTermMatrix[{}, Automatic]` . 
     
### LSI weight functions

After making the document-term matrix we will most likely apply LSI weight functions, [Wk2], like "GFIDF" and "TF-IDF". 
(This follows the "standard" approach used in search engines for calculating weights for document-term matrices; see [MB1].) 

#### Frequency matrix

We use the following definition of the frequency document-term matrix $F$.

Each entry $f_{i j}$ of the matrix $F$ is the number of occurrences of the term $j$ in the document $i$.

#### Weights

Each entry of the weighted document-term matrix $M$ derived from the frequency document-term matrix $F$ is expressed with the formula

[//]: # (No rules defined for DisplayFormula)

where
$g_j$ -- global term weight;
$l_{i j}$ -- local term weight;
$d_i$ -- normalization weight.

Various formulas exist for these weights and one of the challenges is to find the right combination of them when using different document collections.

Here is a table of weight functions formulas. 

![LSAMon-LSI-weight-functions-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-LSI-weight-functions-table.png)

#### Computation specifications

`LSAMon` function LSAMonApplyTermWeightFunctions delegates the LSI weight functions application to the package ["DocumentTermMatrixConstruction.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m), [[AAp4](https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m)].

Here is an example.

    lsaHamlet = LSAMonUnit[textHamlet]⟹LSAMonMakeDocumentTermMatrix;
    wmat =
      lsaHamlet⟹
       LSAMonApplyTermWeightFunctions["IDF", "TermFrequency", "Cosine"]⟹
       LSAMonTakeWeightedDocumentTermMatrix;

    TakeLargest[ColumnSumsAssociation[wmat], 6]

Instead of using the positional signature of LSAMonApplyTermWeightFunctions we can specify the LSI functions using options.

    wmat2 =
      lsaHamlet⟹
       LSAMonApplyTermWeightFunctions[
        "GlobalWeightFunction" -> "IDF", 
        "LocalWeightFunction" -> "TermFrequency", 
        "NormalizerFunction" -> "Cosine"]⟹
       LSAMonTakeWeightedDocumentTermMatrix;

    TakeLargest[ColumnSumsAssociation[wmat2], 6]

Here we are summaries of the non-zero values of the weighted document-term matrix derived with different combinations 
of global, local, and normalization weight functions.

    Magnify[#, 0.8] &@Multicolumn[Framed /@ #, 6] &@Flatten@
      Table[
       (wmat =
         lsaHamlet⟹
          LSAMonApplyTermWeightFunctions[gw, lw, nf]⟹
          LSAMonTakeWeightedDocumentTermMatrix;
        RecordsSummary[SparseArray[wmat]["NonzeroValues"], 
         List@StringRiffle[{gw, lw, nf}, ", "]]),
       {gw, {"IDF", "GFIDF", "Binary", "None", "ColumnStochastic"}},
       {lw, {"Binary", "Log", "None"}},
       {nf, {"Cosine", "None", "RowStochastic"}}]
    AutoCollapse[]

![LSAMon-LSI-weight-functions-combinations-application-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-LSI-weight-functions-combinations-application-table.png)

### Extracting topics

Streamlining topic extraction is one of the main reasons `LSAMon` was implemented. The topic extraction correspond to the so called "syntagmatic" relationships between the terms, [MS1].

#### Theoretical outline

The original weighed document-term matrix $M$ is decomposed into the matrix factors $W$ and $H$.

$M \approx W.H, W \in {R}^(k \times m), H \in] R^{k \times n} .$

The $i$-th row of $M$ is expressed with the $i$-th row of $W$ multiplied by $H$.

The rows of $H$ are the topics. SVD produces orthogonal topics; NNMF does not. 

The $i$-the document of the collection corresponds to the $i$-th row $W$. 
Finding the Nearest Neighbors (NN's) of the $i$-th document using the rows similarity of the matrix $W$ gives document NN's through topic similarity.

The terms correspond to the columns of $H$. 
Finding NN's based on similarities of $H$'s columns produces statistical thesaurus entries.

The term groups provided by $H$'s rows correspond to "syntagmatic" relationships. 
Using similarities of $H$'s columns we can produce term clusters that correspond to "paradigmatic" relationships.  

#### Computation specifications

Here is an example using the play "Hamlet" in which we specify additional stop words.

    stopWords2 = {"enter", "exit", "[exit", "ham", "hor", "laer", "pol", "oph", "thy", "thee", "act", "scene"};

    SeedRandom[2381]
    lsaHamlet =
      LSAMonUnit[textHamlet]⟹
       LSAMonMakeDocumentTermMatrix["StemmingRules" -> Automatic, "StopWords" -> Join[stopWords, stopWords2]]⟹
       LSAMonApplyTermWeightFunctions["GlobalWeightFunction" -> "IDF", "LocalWeightFunction" -> "None", "NormalizerFunction" -> "Cosine"]⟹
       LSAMonExtractTopics["NumberOfTopics" -> 12, "MinNumberOfDocumentsPerTerm" -> 10, Method -> "NNMF", "MaxSteps" -> 20]⟹
       LSAMonEchoTopicsTable["NumberOfTableColumns" -> 6, "NumberOfTerms" -> 10];

![LSAMon-Extracting-topics-Hamlet-topics-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Extracting-topics-Hamlet-topics-table.png)

Here is an example using the USA presidents "state of union" speeches.

    SeedRandom[7681]
    lsaSpeeches =
      LSAMonUnit[aStateOfUnionSpeeches]⟹
       LSAMonMakeDocumentTermMatrix["StemmingRules" -> Automatic,  "StopWords" -> Automatic]⟹
       LSAMonApplyTermWeightFunctions["GlobalWeightFunction" -> "IDF", "LocalWeightFunction" -> "None", "NormalizerFunction" -> "Cosine"]⟹
       LSAMonExtractTopics["NumberOfTopics" -> 36, "MinNumberOfDocumentsPerTerm" -> 40, Method -> "NNMF", "MaxSteps" -> 12]⟹
       LSAMonEchoTopicsTable["NumberOfTableColumns" -> 6, "NumberOfTerms" -> 10];

![LSAMon-Extracting-topics-StateOfUnionSpeeches-topics-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Extracting-topics-StateOfUnionSpeeches-topics-table.png)

Note that in both examples:

   1. stemming is used when creating the document-term matrix,

   2. the default LSI re-weighting functions are used: "IDF", "None", "Cosine",

   3. the dimension reduction algorithm NNMF is used.

Things to keep in mind.

   4. The interpretability provided by NNMF comes at a price. 

   5. NNMF is prone to get stuck into local minima, so several topic extractions (and corresponding evaluations) have to be done. 

   6. We would get different results with different NNMF runs with the same parameters. (NNMF uses random numbers initialization.)

   7. The NNMF topic vectors are not orthogonal.

   8. SVD is much faster than NNMF, but it topic vectors are hard to interpret.

   9. Generally, the topics derived with SVD are stable, do not change with different runs with the same parameters. 

   10. The SVD topics vectors are orthogonal, which provides for quick to find representations of documents not in the monad's document collection.

### Extracting statistical thesauri

The statistical thesaurus extraction corresponds to the "paradigmatic" relationships between the terms, [MS1].

    lsaSpeeches⟹
      LSAMonExtractStatisticalThesaurus[
       "Words" -> Map[WordData[#, "PorterStem"] &, {"bank", "war", "economy", "school", "port", "health", "enemy", "nuclear"}], 
       "NumberOfNearestNeighbors" -> 12]⟹
      LSAMonEchoStatisticalThesaurus;

![LSAMon-Extracting-statistical-thesauri-echo](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Extracting-statistical-thesauri-echo.png)

### Mapping queries and documents to terms

One of the most natural operations is to find the representation of an arbitrary document (or sentence or a list of words) in monad's Linear vector space of terms. This is done with the function LSAMonRepresentByTerms.

Here is an example in which a sentence is represented as a one-row matrix (in that space.)

    obj =
      lsaHamlet⟹
       LSAMonRepresentByTerms["Hamlet, Prince of Denmark killed the king."]⟹
       LSAMonEchoValue;

Here we display only the non-zero columns of that matrix.

    obj⟹
      LSAMonEchoFunctionValue[MatrixForm[Part[#, All, Keys[Select[SSparseMatrix`ColumnSumsAssociation[#], # > 0& ]]]]& ];

#### Transformation steps

Assume that LSAMonRepresentByTerms is given a list of sentences. Then that function performs the following steps.

**1.** The sentence is split into a list of words.

**2.** If monad's document-term matrix was made by removing stop words the same stop words are removed from the list of words.

**3.** If monad's document-term matrix was made by stemming the same stemming rules are applied to the list of words.

**4.** The LSI global weights and the LSI local weight and normalizer functions are applied to sentence's contingency matrix.

#### Equivalent representation

Let us look convince ourselves that documents used in the monad to built the weighted document-term matrix have the same representation as the corresponding rows of that matrix.

Here is an association of documents from monad's document collection.

    inds = {6, 10};
    queries = Part[lsaHamlet⟹LSAMonTakeDocuments, inds];
    queries
     
     (* <|"id.0006" -> "Getrude, Queen of Denmark, mother to Hamlet. Ophelia, daughter to Polonius.", 
          "id.0010" -> "ACT I. Scene I. Elsinore. A platform before the Castle."|> *)

    lsaHamlet⟹
      LSAMonRepresentByTerms[queries]⟹
      LSAMonEchoFunctionValue[MatrixForm[Part[#, All, Keys[Select[SSparseMatrix`ColumnSumsAssociation[#], # > 0& ]]]]& ];

![LSAMon-Mapping-queries-and-documents-to-topics-query-matrix](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Mapping-queries-and-documents-to-topics-query-matrix.png)

    lsaHamlet⟹
      LSAMonEchoFunctionContext[MatrixForm[Part[Slot["weightedDocumentTermMatrix"], inds, Keys[Select[SSparseMatrix`ColumnSumsAssociation[Part[Slot["weightedDocumentTermMatrix"], inds, All]], # > 0& ]]]]& ];

![LSAMon-Mapping-queries-and-documents-to-topics-context-sub-matrix](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Mapping-queries-and-documents-to-topics-context-sub-matrix.png)


### Mapping queries and documents to topics

Another natural operation is to find the representation of an arbitrary document (or a list of words) in monad's Linear vector space of topics. This is done with the function LSAMonRepresentByTopics.

Here is an example.

    inds = {6, 10};
    queries = Part[lsaHamlet⟹LSAMonTakeDocuments, inds];
    Short /@ queries

    (* <|"id.0006" -> "Getrude, Queen of Denmark, mother to Hamlet. Ophelia, daughter to Polonius.", 
         "id.0010" -> "ACT I. Scene I. Elsinore. A platform before the Castle."|> *)
    
    lsaHamlet⟹
      LSAMonRepresentByTopics[queries]⟹
      LSAMonEchoFunctionValue[MatrixForm[Part[#, All, Keys[Select[SSparseMatrix`ColumnSumsAssociation[#], # > 0& ]]]]& ];

![LSAMon-Mapping-queries-and-documents-to-terms-query-matrix](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Mapping-queries-and-documents-to-terms-query-matrix.png)

    lsaHamlet⟹
      LSAMonEchoFunctionContext[MatrixForm[Part[Slot["W"], inds, Keys[Select[SSparseMatrix`ColumnSumsAssociation[Part[Slot["W"], inds, All]], # > 0& ]]]]& ];

![LSAMon-Mapping-queries-and-documents-to-terms-query-matrix](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Mapping-queries-and-documents-to-terms-query-matrix.png)


#### Theory

In order to clarify what the function LSAMonRepresentByTopics is doing let us go through the formulas it is based on.

The original weighed document-term matrix $M$ is decomposed into the matrix factors $W$ and $H$.

[//]: # (No rules defined for DisplayFormula)

The $i$-th row of $M$ is expressed with the $i$-th row of $W$ multiplied by $H$.

[//]: # (No rules defined for DisplayFormula)

For a query vector $q_0\in \mathbb{R}^m$we want to find its topics representation vector $x\in \mathbb{R}^k$:

[//]: # (No rules defined for DisplayFormula)

Denote with $\frac{1}{H}$ the inverse or pseudo-inverse matrix of $H$. We have:

[//]: # (No rules defined for DisplayFormula)

[//]: # (No rules defined for DisplayFormula)

In `LSAMon` for SVD $H^T$; for NNMF is $\frac{1}{H}$ is the pseudo-inverse of $H$.

The vector $x$ obtained with LSAMonRepresentByTopics.

### Finding the most important documents

There are several algorithms we can apply for finding the most important documents in the collection. 
`LSAMon` utilizes two types algorithms: (1) graph centrality measures based, and (2) matrix factorization based. With certain graph centrality measures the two algorithms are equivalent. In this sub-section we demonstrate the matrix factorization algorithm (that uses SVD.)

**Definition:** The most important sentences have the most important words and the most important words are in the most important sentences.

That definition can be used to derive an iterations-based model that can be expressed with SVD or eigenvector finding algorithms, [LE1].

Here we pick an important part of the play "Hamlet".

    focusText = 
      First@Pick[textHamlet, StringMatchQ[textHamlet, ___ ~~ "to be" ~~ __ ~~ "or not to be" ~~ ___, IgnoreCase -> True]];
    Short[focusText]

    (* "Ham. To be, or not to be- that is the question: Whether 'tis ....y. 
        O, woe is me T' have seen what I have seen, see what I see!" *)

    LSAMonUnit[StringSplit[ToLowerCase[focusText], {",", ".", ";", "!", "?"}]]⟹
      LSAMonMakeDocumentTermMatrix["StemmingRules" -> {}, "StopWords" -> Automatic]⟹
      LSAMonApplyTermWeightFunctions⟹
      LSAMonFindMostImportantDocuments[3]⟹
      LSAMonEchoFunctionValue[GridTableForm];

![LSAMon-Find-most-important-documents-table](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/A-monad-for-Latent-Semantic-Analysis-workflows/LSAMon-Find-most-important-documents-table.png)

### Setters, droppers, and takers

The values from the monad context can be set, obtained, or dropped with the corresponding "setter", "dropper", and "taker" functions as summarized in a previous section.

For example:

    p = LSAMonUnit[textHamlet]⟹LSAMonMakeDocumentTermMatrix[Automatic, Automatic];

    p⟹LSAMonTakeMatrix

If other values are put in the context they can be obtained through the (generic) function LSAMonTakeContext, [AAp1]:

    Short@(p⟹QRMonTakeContext)["documents"]
     
    (* <|"id.0001" -> "1604", "id.0002" -> "THE TRAGEDY OF HAMLET, PRINCE OF DENMARK", <<220>>, "id.0223" -> "THE END"|> *) 

Another generic function from [AAp1] is LSAMonTakeValue (used many times above.)

Here is an example of the "data dropper" LSAMonDropDocuments:

    Keys[p⟹LSAMonDropDocuments⟹QRMonTakeContext]

    (* {"documentTermMatrix", "terms", "stopWords", "stemmingRules"} *)
    
(The "droppers" simply use the state monad function LSAMonDropFromContext, [AAp1]. For example, LSAMonDropDocuments is equivalent to LSAMonDropFromContext["documents"].)

## Unit tests

The development of `LSAMon` was done with two types of unit tests: (i) directly specified tests, [[AAp7](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicQuantileRegression-Unit-Tests.wlt)], and (ii) tests based on randomly generated pipelines, [[AA8](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicQuantileRegressionRandomPipelinesUnitTests.m)].

The unit test package should be further extended in order to provide better coverage of the functionalities and illustrate -- and postulate -- pipeline behavior.

### Directly specified tests

Here we run the unit tests file 
["MonadicLatentSemanticAnalysis-Unit-Tests.wlt"](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysis-Unit-Tests.wlt),
[[AAp8](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysis-Unit-Tests.wlt)].

    AbsoluteTiming[
     testObject = TestReport["~/MathematicaForPrediction/UnitTests/MonadicLatentSemanticAnalysis-Unit-Tests.wlt"]
    ]

The natural language derived test ID's should give a fairly good idea of the functionalities covered in [AAp3].

    Values[Map[#["TestID"] &, testObject["TestResults"]]]

### Random pipelines tests

Since the monad `LSAMon` is a DSL it is natural to test it with a large number of randomly generated "sentences" of that DSL. 
For the `LSAMon` DSL the sentences are `LSAMon` pipelines. The package 
["MonadicLatentSemanticAnalysisRandomPipelinesUnitTests.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysisRandomPipelinesUnitTests.m), 
[[AAp9](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysisRandomPipelinesUnitTests.m)], 
has functions for generation of QRMon random pipelines and running them as verification tests. 
A short example follows.

Generate pipelines:

    SeedRandom[234]
    pipelines = MakeLSAMonRandomPipelines[100];
    Length[pipelines]

    (* 100 *)

Here is a sample of the generated pipelines:

    (*Block[{DoubleLongRightArrow,pipelines=RandomSample[pipelines,6]},\
    Clear[DoubleLongRightArrow];
    pipelines=pipelines/.{_TemporalData->"tsData",_?MatrixQ->\
    "distData"};
    GridTableForm[Map[List@ToString[DoubleLongRightArrow@@#,FormatType\
    ->StandardForm]&,pipelines],TableHeadings->{"pipeline"}]
    ]
    AutoCollapse[]*)

Here we run the pipelines as unit tests:

    AbsoluteTiming[
     res = TestRunLSAMonPipelines[pipelines, "Echo" -> False];
    ]

From the test report results we see that a dozen tests failed with messages, all of the rest passed.

    rpTRObj = TestReport[res]

    (* {"LoadPackage", "USASpeechesData", "HamletData", "StopWords", 
        "Make-document-term-matrix-1", "Make-document-term-matrix-2",
        "Apply-term-weights-1", "Apply-term-weights-2", "Topic-extraction-1",
        "Topic-extraction-2", "Topic-extraction-3", "Topic-extraction-4",
        "Statistical-thesaurus-1", "Topics-representation-1",
        "Take-document-term-matrix-1", "Take-weighted-document-term-matrix-1",
        "Take-document-term-matrix-2", "Take-weighted-document-term-matrix-2",
        "Take-terms-1", "Take-Factors-1", "Take-Factors-2", "Take-Factors-3",
        "Take-Factors-4", "Take-StopWords-1", "Take-StemmingRules-1"} *)


## Implementation notes

The implementation methodology of the `LSAMon` monad packages [AAp3, AAp9] followed the methodology created 
for the `ClCon` monad package [AAp10, AA6]. Similarly, this document closely follows the structure and exposition 
of the `ClCon  monad document 
["A monad for classification workflows"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-classification-workflows.md), 
[AA6].

A lot of the functionalities and signatures of `LSAMon` were designed and programed through considerations 
of natural language commands specifications given to a specialized conversational agent. 


## References

###  Packages

[AAp1] Anton Antonov, [State monad code generator Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.
    *URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m) .

[AAp2] Anton Antonov, [Monadic tracing Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.
    *URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m) .

[AAp3] Anton Antonov, [Monadic Latent Semantic Analysis Mathematica packag](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m)e, (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.*
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m) .

[AAp4] Anton Antonov, [Implementation of document-term matrix construction and re-weighting functions in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m)[, ](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m)(2013), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.*
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/DocumentTermMatrixConstruction.m) .

[AAp5] Anton Antonov, [Non-Negative Matrix Factorization algorithm implementation in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m), (2013), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
  URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m)[ .](https://github.com/antononcube/MathematicaForPrediction/blob/master/QuantileRegression.m)

[AAp6] Anton Antonov, [SSparseMatrix Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/SSparseMatrix.m),[ ](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m)(2018), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
  URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/SSparseMatrix.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/SSparseMatrix.m) [.](https://github.com/antononcube/MathematicaForPrediction/blob/master/NonNegativeMatrixFactorization.m)

[AAp7] Anton Antonov, [MathematicaForPrediction utilities](https://github.com/antononcube/MathematicaForPrediction/raw/master/MathematicaForPredictionUtilities.m), (2014), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
  URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m) .

[AAp8] Anton Antonov, [Monadic Latent Semantic Analysis unit tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysis-Unit-Tests.wlt), (2019), [MathematicaVsR at GitHub](https://github.com/antononcube/MathematicaVsR).
   URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysis-Unit-Tests.wlt](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysis-Unit-Tests.wlt) .

[AAp9] Anton Antonov, [Monadic Latent Semantic Analysis random pipelines Mathematica unit tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysisRandomPipelinesUnitTests.m), (2019), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
  URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysisRandomPipelinesUnitTests.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicLatentSemanticAnalysisRandomPipelinesUnitTests.m) .

[AAp10] Anton Antonov, [Monadic contextual classification Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction). 
  URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m) .

### ConverationalAgents Packages

[AAp10] Anton Antonov, [Time series workflows grammar in EBNF](https://github.com/antononcube/ConversationalAgents/blob/master/EBNF/TimeSeriesWorkflowsGrammar.m), (2018), [ConversationalAgents at GitHub](https://github.com/antononcube/ConversationalAgents), [https://github.com/antononcube/ConversationalAgents](https://github.com/antononcube/ConversationalAgents).

[AAp11] Anton Antonov, [QRMon translator Mathematica package](https://github.com/antononcube/ConversationalAgents/blob/master/Projects/TimeSeriesWorkflowsAgent/Mathematica/QRMonTranslator.m),(2018), [ConversationalAgents at GitHub](https://github.com/antononcube/ConversationalAgents), [https://github.com/antononcube/ConversationalAgents](https://github.com/antononcube/ConversationalAgents).

### MathematicaForPrediction articles

[AA1] Anton Antonov, ["Monad code generation and extension"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md), (2017),  [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*, *[https://github.com/antononcube/MathematicaForPrediction](https://github.com/antononcube/MathematicaForPrediction).

[AA2] Anton Antonov, ["Topic and thesaurus extraction from a document collection"](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Topic%20and%20thesaurus%20extraction%20from%20a%20document%20collection.pdf), (2013), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).

[AA3] Anton Antonov, ["The Great conversation in USA presidential speeches"](https://mathematicaforprediction.wordpress.com/2017/12/24/the-great-conversation-in-usa-presidential-speeches/), (2017), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).
  URL: [https://mathematicaforprediction.wordpress.com/2017/12/24/the-great-conversation-in-usa-presidential-speeches](https://mathematicaforprediction.wordpress.com/2017/12/24/the-great-conversation-in-usa-presidential-speeches/) .

[AA4] Anton Antonov, ["Contingency tables creation examples"](https://mathematicaforprediction.wordpress.com/2016/10/04/contingency-tables-creation-examples/), (2016), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).

[AA5] Anton Antonov, ["RSparseMatrix for sparse matrices with named rows and columns"](https://mathematicaforprediction.wordpress.com/2015/10/08/rsparsematrix-for-sparse-matrices-with-named-rows-and-columns/), (2015), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).

[AA6] Anton Antonov, ["A monad for classification workflows"](https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows/),  (2018), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).
  URL: [https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows/](https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows/) .

### Other

[Wk1] Wikipedia entry, [Monad](https://en.wikipedia.org/wiki/Monad_(functional_programming)), 
   URL: [https://en.wikipedia.org/wiki/Monad_(functional_programming)](https://en.wikipedia.org/wiki/Monad_(functional_programming)) . 

[Wk2] Wikipedia entry, [Latent semantic analysis](https://en.wikipedia.org/wiki/Latent_semantic_analysis),
  URL: [https://en.wikipedia.org/wiki/Latent_semantic_analysis](https://en.wikipedia.org/wiki/Latent_semantic_analysis) .

[Wk3] Wikipedia entry, [Distributional semantics](https://en.wikipedia.org/wiki/Distributional_semantics),
  URL: [https://en.wikipedia.org/wiki/Distributional_semantics](https://en.wikipedia.org/wiki/Distributional_semantics) .

[Wk4] Wikipedia entry, [Non-negative matrix factorization](https://en.wikipedia.org/wiki/Non-negative_matrix_factorization),
  URL: [https://en.wikipedia.org/wiki/Non-negative_matrix _factorization](https://en.wikipedia.org/wiki/Non-negative_matrix_factorization) .

[LE1] Lars Elden, Matrix Methods in Data Mining and Pattern Recognition, 2007, SIAM. ISBN-13: 978-0898716269.

[MB1] Michael W. Berry & Murray Browne, Understanding Search Engines: Mathematical Modeling and Text Retrieval, 2nd. ed., 2005, SIAM. ISBN-13: 978-0898715811.

[MS1] Magnus Sahlgren, ["The Distributional Hypothesis"](http://soda.swedish-ict.se/3941/1/sahlgren.distr-hypo.pdf), (2008), Rivista di Linguistica. 20 (1): 33\[Dash]53.

[PS1] Patrick Scheibe, [Mathematica (Wolfram Language) support for IntelliJ IDE](https://github.com/halirutan/Mathematica-IntelliJ-Plugin)A, (2013-2018), [Mathematica-IntelliJ-Plugin at GitHub](https://github.com/halirutan/Mathematica-IntelliJ-Plugin).
   URL: [https://github.com/halirutan/Mathematica-IntelliJ-Plugin](https://github.com/halirutan/Mathematica-IntelliJ-Plugin) .

