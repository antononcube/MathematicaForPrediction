# Rapid specification of random tabular datasets generation

## Introduction

In this document we discuss the motivation, design, implementation, and extensions of a function that generates
random tabular data.

Here is an example of such function (using [AAr1]):

```mathematica
ResourceFunction["RandomTabularDataset"][4]
```

In this document we discuss an implementation in Mathematica / Wolfram Language (WL) -- 
[`RandomTabularDataset`](https://www.wolframcloud.com/obj/antononcube/DeployedResources/Function/RandomTabularDataset), 
[[AAw1](https://www.wolframcloud.com/obj/antononcube/DeployedResources/Function/RandomTabularDataset)].

(We also point to similar efforts in R.)

## Motivations

Our motivations of making functions or packages that generate random datasets are pretty standard: 

- Rapid prototyping (of proof of concepts)

- Thorough testing of (data wrangling) algorithms

- Making unit tests 

More specifically we want to:

- Be able to quickly produce example datasets for
  [Data Wrangling classes](https://community.wolfram.com/groups/-/m/t/2112820), [AA1]
  
- Have a large corpus of datasets to test the data transformations conversational agents, like
  [DSL::English::DataQueryWorkflows](https://github.com/antononcube/Raku-DSL-English-DataQueryWorkflows), [AAr1]
  
- Have a large corpus of datasets to illustrate data quality verification algorithms or frameworks, like this
  [Data Quality Monitoring Module](https://github.com/antononcube/HowToBeADataScientistImpostor-book/blob/master/Part-5-Software-engineering-skills/Data-Quality-Monitoring-Module.md)
  
  
**Remark:** Using random data and construction of random workflows has been heavily utilized in
the development of the software monads `ERTMon`, [AAr2], and `QRMon`, [AAr3]. 

## Similar work

### R

There several packages in R and related blog posts that implement and discuss similar functionalities.

- The package "wakefield" by Tyler Rinker et al., [TR1], can be used to generate random data frames 
  -- very similar to `RandomTabularDataset`.

- The package "fakeR" by Lily Zhang, Dustin Tingley, [LZ1], can be used to generate "simulated" data frames
  from example data frames.

- The package "ERTMon-R", [AAr2, AA3], has unit test helper functions that generate random (families) of random dataset . 

### WL

There is a dedicated discussion, 
["Generating Random Datasets"](https://community.wolfram.com/groups/-/m/t/2109909), [MB1], 
at 
[Wolfram's Community](https://community.wolfram.com).

## Design principles

We want to have a function that allows the rapid specification of random datasets with particular properties.
We want to have a fairly comprehensive list of particular properties.  
 
Here is a list of the functionalities:

- Column values generators with random and deterministic functions
  
  - Easy to specify generators based on built-in WL distributions

  - Particular columns can have particular generators
    
  - Specifications of overall generators

- Column names generators with random and deterministic functions
    
- Optional addition of row names

- The ability to specify the presence of missing values

- Support for both pointwise and vector-wise random values generators

## Development and implementation

There is a dedicated package 
["RandomTabularDataset.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RandomTabularDataset.m)
and 
[dedicated unit tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/RandomTabularDataset-Unit-Tests.wlt) 
for that package. (The unit tests were also included in WFR's submission, [AA1].)

Of course, it is best to have a
[Wolfram Function Repository (WFR)](https://resources.wolframcloud.com/FunctionRepository/) function that generates random datasets. 
I implemented such function and submitted it to WFR -- see 
[`RandomTabularDataset`](https://www.wolframcloud.com/obj/antononcube/DeployedResources/Function/RandomTabularDataset).

Another, closely related WFR function is 
[`ExampleDataset`](https://resources.wolframcloud.com/FunctionRepository/resources/ExampleDataset).

**Remark:** Note that I prefer the name `RandomTabularDataset` instead of `RandomDataset`. In Mathematica / WL
datasets can be (deeply) hierarchical objects. Tabular datasets are simpler than the general WL datasets,
but tabular data is very common, easier to explain and to reason with.


## Demonstration

The resource function 
[`ExampleDataset`](https://resources.wolframcloud.com/FunctionRepository/resources/ExampleDataset) 
makes datasets from 
[`ExampleData`](https://reference.wolfram.com/language/ref/ExampleData.html). 
Here is an example dataset:


```mathematica
dsAW = ResourceFunction["ExampleDataset"][{"Statistics", "AnimalWeights"}]
```

Here is a similar random dataset:

```mathematica
SeedRandom[23];
dsCW = ResourceFunction["https://www.wolframcloud.com/obj/antononcube/DeployedResources/Function/RandomTabularDataset"][
   {60, {"Creature", "BodyWeight", "BrainWeight"}},
   "Generators" -> <| 
     1 -> (Table[StringJoin[RandomChoice[CharacterRange["a", "z"], 5]], #] &),
     2 -> FindDistribution[Normal@dsAW[All, "BodyWeight"]],
     3 -> FindDistribution[Normal@dsAW[All, "BrainWeight"]]|>];
IQB = Interval[Quartiles[N@Normal[dsAW[All, #BrainWeight/#BodyWeight &]]][[{1, 3}]]];
dsCW[Select[IntervalMemberQ[IQB, #BrainWeight/ #BodyWeight] &]]
```

**Remark:** Instead of quartile boundaries filtering we can filter with
`AnomalyDetection[Normal[dsAW[All, #BrainWeight/#BodyWeight &]]]`,
but the latter is prone to produce results that are "too far off."

## Neat example

A random dataset with values produced by resource functions that generate random objects:

```mathematica
SeedRandom[3];
ResourceFunction[
 "https://www.wolframcloud.com/obj/antononcube/DeployedResources/\
Function/RandomTabularDataset"][{5, {"Mondrian", "Mandala", "Haiku", "Scribble", "Maze", "Fortune"}},
 "Generators" ->
  <|
   1 -> (ResourceFunction["RandomMondrian"][] &),
   2 -> (ResourceFunction["RandomMandala"][] &),
   3 -> (ResourceFunction["RandomEnglishHaiku"][] &),
   4 -> (ResourceFunction["RandomScribble"][] &),
   5 -> (ResourceFunction["RandomMaze"][12] &),
   6 -> (ResourceFunction["RandomFortune"][] &)|>,
 "PointwiseGeneration" -> True]
```

## Future plans

In this section we discuss a few of the most important (or inspiring) directions of extending the work on `RandomTabularDataset`.
The "narrower" extensions are given first, the most general last.

### Multi-dimensional generators specifications

The initial design and implementation of `RandomTabularDataset` allows specification of 
zero- or one-dimensional generators only.
It would be very useful to have ability to specify multi-dimensional distributions for sets of columns. 

Here is an example:

```mathematica
ResourceFunction["RandomTabularDataset"][{Automatic,5}, "Generators"-> <| {1,3,4} -> CopulaDistribution["Product", {GeometricDistribution[.3], 
  PoissonDistribution[4], PoissonDistribution[12]}] |>]
```

### Applications to data obfuscation

A more rigorous and enhanced version of the random dataset generation example in the section "Demonstration" 
above can be utilized to do data obfuscation that:

- Allows easier to specify obfuscation elements and features 

- Produces data that is harder to identify as an obfuscated version of the original data 

See the related R packages 
["fakeR"](https://cran.r-project.org/web/packages/fakeR/index.html), [LZ1], 
and 
["DataObfuscation"](https://github.com/antononcube/R-packages/tree/master/DataObfuscation), [AAp3].

For time series data obfuscation or simulation can be done using the Quantile Regression packages [AAp4, AAr3]
described in [AA2].

### Implementations in Julia, Python, or R

It is natural to consider the implementation of `RandomTabularDataset` in the programming languages
Julia, Python, and R. Those three languages together with WL are the target languages of the 
Domain Specific Language (DSL) Raku module [AAr1] that allows the generation of programming code through
natural language commands.

### Implementing the more general `RandomDataset`

As it was mentioned above, in Mathematica / WL datasets can be (deeply) hierarchical objects. 
Tabular datasets are simpler than the general WL datasets,
but tabular data is very common, easier to explain and to reason with. 
Hence, `RandomTabularDataset` is a 2D specialization of much more ambitious function: `RandomDataset`.

A few immediate observations:

- We can make a `RandomDataset` object making a WL associations of `RandomTabularDataset` objects 

- We have to "scale" the generators to generate not just random points or vectors but also random multi-dimensional arrays.

From the observations follows that `RandomDataset` should have most of the arguments and options of `RandomTabularDataset`,
but there two main questions that require careful design answers:

- How to specify generators that correspond to k-dimensional arrays (with k > 1)?

- How to specify generators that for names of particular arrays in particular dimensions?


### Conversational agent for generating random objects

I consider making a conversational agent that generates programming code (in R, WL, others) that generate random objects.

**Remark:** The conversational agent targets not just the generation of random tabular datasets but any random objects
that can be generated in R and WL.

Here are example commands:

```
make a list of 24 random graphs
```

```
make twenty random datasets with columns that are normally distributed
each dataset should have at least 200 rows and at least 5 columns
```

```
make a random dataset with first column made of random words and second column with Poisson distribution numbers with lambda 3
```

The design of the conversational agent has to use both appropriate grammars and well defined graph of 
Finite State Machine (FSM) transitions. 
(Grammars, of course, themselves can be seen as FSMs. Hence, can be included in the master FSM.)

## References

### Articles, posts

[MB1] Mike Besso, 
["Creating Random Datasets"](https://community.wolfram.com/groups/-/m/t/2109909),
(2020),
[community.wolfram.com](https://community.wolfram.com).


[AA1] Anton Antonov,
["Data transformation workflows - Wolfram U sessions"](https://community.wolfram.com/groups/-/m/t/2112820),
(2020),
[community.wolfram.com](https://community.wolfram.com).

[AA2] Anton Antonov,
["A monad for Quantile Regression workflows"](https://mathematicaforprediction.wordpress.com/2018/08/01/a-monad-for-quantile-regression-workflows/),
(2018),
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).

### Packages, repositories

[LZ1] Lily Zhang and Dustin Tingley,
["fakeR: Simulates Data from a Data Frame of Different Variable Types"](https://cran.r-project.org/web/packages/fakeR/index.html)
(2016),
[CRAN](https://cran.r-project.org).

[TR1] Tyler Rinker et al.,
["wakefield: Generate Random Data Sets"](https://cran.r-project.org/web/packages/wakefield/index.html),
(2020),
[CRAN](https://cran.r-project.org).

[AAp1] Anton Antonov,
[Random Tabular Dataset Mathematica Package](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/RandomTabularDataset.m),
(2020),
[MathematicaForPrediction at GitHub/antononcube](https://github.com/antononcube/MathematicaForPrediction).

[AAp2] Anton Antonov,
[RandomTabularDataset Mathematica unit tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/RandomTabularDataset-Unit-Tests.wlt),
(2020),
[MathematicaForPrediction at GitHub/antononcube](https://github.com/antononcube/MathematicaForPrediction).

[AAp3] Anton Antonov,
["Data obfuscation functions in R"](https://github.com/antononcube/R-packages/tree/master/DataObfuscation),
(2019),
[R-packages at GitHub/antononcube](https://github.com/antononcube/R-packages).

[AAp4] Anton Antonov, 
[Monadic Event Records Transformations Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicEventRecordsTransformations.m),
(2018), 
[MathematicaForPrediction at GitHub/antononcube](https://github.com/antononcube/MathematicaForPrediction).

[AAr1] Anton Antonov,
[DSL::English::DataQueryWorkflows Raku package](https://github.com/antononcube/Raku-DSL-English-DataQueryWorkflows),
(2020),
[Raku-DSL-English-DataQueryWorkflows at GitHub/antononcube](https://github.com/antononcube/Raku-DSL-English-DataQueryWorkflows).

[AAr2] Anton Antonov,
["Event Records Transformations Monad in R"](https://github.com/antononcube/ERTMon-R),
(2018),
[ERTMon-R at GitHub/antononcube](https://github.com/antononcube/ERTMon-R).

[AAr3] Anton Antonov,
["Quantile Regression Monad in R"](https://github.com/antononcube/QRMon-R),
(2019),
[QRMon-R at GitHub/antononcube](https://github.com/antononcube/QRMon-R).

## Wolfram Functions Repository

[AAw1] Anton Antonov
[RandomTabularDataset](https://www.wolframcloud.com/obj/antononcube/DeployedResources/Function/RandomTabularDataset).

[AAw2] Anton Antonov
[ExampleDataset](https://resources.wolframcloud.com/FunctionRepository/resources/ExampleDataset).
