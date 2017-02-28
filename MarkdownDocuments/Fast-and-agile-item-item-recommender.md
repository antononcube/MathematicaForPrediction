# **A fast and agile item-item recommender : design and implementation**

### Anton Antonov

Consultant at Synergis

Talk at Wolfram Technology Conference 2011

([Link to a CDF file with the presentation](https://www.wolfram.com/events/technology-conference/2011/presentations/AFastAndAgileIIR.cdf))

## Abstract

In this talk are presented the design and implementation of an Item-Item Recommender (IIR) based on linear algebra operations. The presented IIR has great performance and scalability properties. Design and algorithmic approaches are discussed for recommendation proofs, tuning, and diversification. Recommendations of movies, houses, and points-of-interest are going to be demonstrated using a common user interface.

The algorithms discussed are from the fields of sparse matrix linear algebra, collaborative filtering, natural language processing, principal component analysis, association rule learning, and outlier detection.

# Structure of the talk

- Demonstration of several recommenders (movies, houses, geo-locations)

- Some philosophy: why Item-Item recommenders should be preferred

- Some theory: using linear algebra algorithms

- Diversification of the recommendations

- Extensions

# Who am I 

M.Sc. in Mathematics (General algebra) (1997)

M.Sc. in Computer Science (Data bases) (1997)

Ph.D. in Applied Mathematics (Large scale air-pollution simulations) (2001)

Kernel Developer at Wolfram Research, Inc. (2001-2008)

Principal Engineer at Sezmi Corporation (2008-2011)

Consultant (mathematical modeling and algorithms) at Synergis (now)

# A movie & TV show recommender

Movie and TV show recommendations [1]

Nearly 6000 movies and TV shows, characterized with actors, directors, writers, genres, and other meta-data.

# A house recommender

House recommendations [2]

## General description

- Nearly 8000 houses over 60 diffrent neighborhoods.

- Neighborhoods are characterzied with occupations breakdown and location.

- Houses are of different types (house, town-house, apartment).

- Houses are characterzied with construction (number of floors, number of bathrooms)

- and additions (garage, basement, pool, attic). 

## The circular city

The center of the city is the most interesting place, houses close to it are more expensive.

The neighborhoods (zones) are separated by roads.

The zones on the out-most circle are industrial zones -- people drive to them on daily basis.

![](ItemItemRecommender-CircularCity1.png)

## Interface

![](ItemItemRecommender-CircularCity2.png)

# A points-of-interest recommender 

Points of interest recommendations[3] (for Champaign-Urbana)

Nearly 4000 different points-of-interest in the Champaign-Urbana area.

## All points of interest

![](ItemItemRecommender-PointOfInterest1.png)

## Screenshot of the recommender

![](ItemItemRecommender-PointOfInterest2.png)

# General recommender design

This is the high-level design of the item-item recommender.

![](ItemItemRecommender-GeneralRecommender1.png)

# General interface design

Using linear algebra algorithms to compute user profile and recommendations from the user history.

Using Model-View-Controllers (MVC's) to tweak the outcomes.

![](ItemItemRecommender-GeneralInterface1.png)

(MVC is also known as the Observer Design Pattern.)

# Some philosophy

When talking about recommendation most people think about collaborative filtering.

Collaborative filtering $\approx$ crowd-sourcing.

Item-item recommenders can be used both

1. at cold start (no consumption data)

2. when large consumption data is available.

Item-item recommenders are better; they are more precise, and allow more tuning.

The presented item-item recommender can be used on different kinds of data. (As demonstrated earlier...)

# Some theory

The Basic Recommendation Algorithm (BRA) of this item-item recommender takes as arguments :
1. a list of item indexes *itemInds*, 
2. star ratings *ratings* for the items in *itemInds*, and 
3. an integer *k* for the desired number of recommendation results.  

BRA has the following steps:

1. Represent the data in a item $\times$ meta-data matrix $M:=M_{01} \in  \{0,1\}^{n\times m}$. ($n$ items, $m$ meta-datums.)

2. To the columns (or entries) of $M_{01}$ are assigned different weights $\{w_{1},...,w_{m}\}$.

3. Form the matrix $M:=M_{01} diag(w_{1},...,w_{m}),M \in R^{n \times m}$. This step can be done for each user separately.

4. The item-item similarity matrix $S:=M M^{T}$  can be formed (but it can be larger and dense).

5. Make a vector $v  \in  R^{n}$ that has zeros everywhere except at the coordinates corresponding to *itemInds*. The values at those coordinates are determined by *ratings*. 

6. Do the two matrix-vector multiplications $s:=M(v M)$. The vector $s$ is called the *score vector* for *itemInds* or $v$. (Also the formula $s:=S v=(M M^{T})v$ can be used, but $M M^{T}$ might be large and dense.)

7. Make the list of pairs $\{s_{i},i\}, i  \in  [1, ... ,n_{s}]$, and name that list $sp$.

8. Sort descendingly $sp$ according to the first value of each pair.

9. Take the indexes of the first *k* pairs, i.e. take $sp(1:k,2)$.

## Some mathematical relations

1. Transposing of a matrix product:

$$ (A B)^{T}= B^{T} (A^{T}). $$

2. Denote with $S$ the show $\times$ show similarity matrix. We have

$$ S=M M^{T}=(M_{01} diag(w))(M_{01} diag(w))^{T}=(M_{01} diag(w))(diag(w)(M_{01})^{T})=M^{01} diag({w_{1}}^{2},...,{w_{n_{m}}}^{2})(M_{01})^{T}.$$

3. For the show score vector of the user $u$ we have:

$$ s_{u}=S v=M (v M)=(M_{01} diag(w))(v M_{01} diag(w)).$$

## Some explanation aids

    MatrixPlot[smat, MaxPlotPoints -> 300]

    MatrixPlot[smat.Transpose[smat], MaxPlotPoints -> 300]

# Predicted ratings

Assume consumption history of user $u$ consists of the items: $\{i_{1},...,i_{n_{u}}\}$.

In order to calculate the predicted ratings for the recommended items, the following formula is used

$$ r_{u j}=\sum_{i \in  \{i_{1},...,i_{n_{u}}\}}S_{i j}r_{u i}/\sum_{i \in  \{i_{1},...,i_{n_{u}}\}}S_{i j},j \in \{j_{1},j_{2},...,j_{k}\},$$

in which $S_{i  j}$ denotes the similarity between the shows $i$ and $j$. (Note that the ratings on the left-hand side are not known -- they are the ones that need to be predicted.)

We can do the calculations of formula (4) using linear algebra operations.

1. Compute the matrix $A \in  R^{k\times n_{u}}$

$$ A=M(\{j_{1},...,j_{k}\})M(\{i_{1},...,i_{n_{u}}\})^{T}.$$

2. This formula finds the sums in the denominator of (4):

$$ norms=A.[1,1,...,1]^{T}, where [1,1,...,1] \in R^{n_{u}},norms  \in  R^{k}.$$

3. This formula finds the sums in the numerator of (4):

$$ t=A.[r_{u i_{1}},...,r_{u i_{n_{u}}}]^{T}, t \in  R^{k}$$

4. Compute the predicted ratings with the formula $\{r_{u j_{1}},r_{u j_{2}},...,r_{u j_{k}}\}=t/norms$, where the division is element by element, i.e.

$$ r_{u j_{h}}=t_{h}/norms_{h},h \in [1,...,k].$$

5. Re-order the indexes $\{j_{1},j_{2},...,j_{k}\}$ according to the descending order of the predicted ratings $\{r_{u j_{1}}, r_{u j_{2}}, ..., r_{u j_{k}}\}$.


# Meta-data characterization

The items can be characterized with several types of meta-data.

For the recommenders demonstrated earlier I harvested meta-data from

- TIGER

- Yelp

- Wikipedia

- Other sources

Using clustering and Latent Semantic Analysis (LSA) to make characterizations. 
For example, the themes in the demonstrated movie recommender are derived by using LSA.

Assume, we have data for millions of users and thousands of items. We can find, say, 1000 clusters of items based on users' consumption history. Each cluster is represented as a column in the item $\times$ meta-data matrix. 

# Diversification of the recommendations

Given the consumption history of a user, we can

**1.** Change weights of characterizing meta-data:

**1.1** Manually.

**1.2** Automatically (similar to search engine presentation[4]).

**2.** Use subsets of items:

**2.1.** Based on recency.

**2.1.1.** Last five shows.

**2.1.2.** Random sample of three elements from the last ten consumed items.

**2.2.** Based on time.

**2.2.1.** Day of the week. (What shows the user watches on Tuesday?)

**2.2.2.** Time interval of the day. (What museums are open right now? Or between 7pm and 9pm?)

**2.2.3.** Time interval from now. (What places and were visited in the last 48 hours?)

**2.3.** Based on meta-data type.

**2.3.1.** Pick all items with a particular tag.

**2.3.2.** Pick recently consumed items with a particular tag.

**3.** Taking subsets of the profile

**3.1.** A sampling or an outlier algorithm that would be applied on profile's list of pairs $\{score_{i}, tag_{i}\}, i \in N.$

# Outlier detection 

## Optional

The purpose of the outlier detection algorithms is to find those items that have scores significantly higher than the rest.

Taking a certain number of items with scores among top $k$ scores is not the same as an outlier detection, but it can be used as a replacement. 

Let us consider the following set of 50 numbers:

    pnts = RandomVariate[GammaDistribution[5, 1], 50]

    {1.57597, 5.03009, 6.18286, 7.44338, 3.3172, 5.55868, 5.47957, 3.07219, 1.51227, 3.36605, 2.62238, 5.25025, 5.57436, 2.0441, 5.31813, 5.01855, 1.711, 6.29888, 3.73988, 3.44081, 6.09885, 6.70532, 4.40424, 5.68977, 6.33919, 3.77062, 2.02377, 5.24491, 7.65962, 5.08227, 3.81159, 8.99772, 2.04538, 4.20143, 4.5597, 2.97527, 5.43555, 6.39612, 3.38575, 3.32693, 6.26912, 5.15415, 6.34743, 2.39711, 2.60531, 4.02664, 4.41152, 4.14645, 9.27038, 3.84409}

If we sort those numbers descendingly and plot them we get:

    nts = pnts // Sort // Reverse;
    ListPlot[pnts, PlotStyle -> {PointSize[0.015]}, Filling -> Axis, PlotRange -> All]

![](ItemItemRecommender-OutlierDetection1.png)

Let use the following outlier detection algorithm: 
1. Find all values in the list that are larger than the mean value multiplied by 1.5;
2. Then find the positions of these values in the list of numbers.

In $Mathematica$ this is implemented in the following way.

    pos = Flatten[Map[Position[pnts, #] &, Select[pnts, # > 1.5 Mean[pnts] &]]]
    
    {1, 2, 3, 4}

Lets plot all points in blue and the so found outliers in red:

    ListPlot[{pnts, Transpose[{pos, pnts[[pos]]}]}, PlotStyle -> {PointSize[0.015], PointSize[0.009]}, Filling -> Axis, PlotRange -> All]

![](ItemItemRecommender-OutlierDetection2.png)

Instead of the mean value we can use another reference point, like the median value. Obviously, we can also use a multiplier different than 1.5.

# Computational complexity and scaling

The recommendation algorithm shown above uses sparse linear algebra, hence it is very fast.

## Data too large for one processor

Making the computations in parallel is (almost) trivial.

![](ItemItemRecommender-ComplexityAndScale1.png)

## Multi recommender structure

One can hook-up several recommenders for items of different types and provide cross-domain recommendations.

![](ItemItemRecommender-ComplexityAndScale2.png)

## Complexity analysis

Too long to explain here, but analysis and experimentation can and should be done for the computational complexity involved of all parts of the algorithm. 

(Extension for large number of users.[5])

# Implementations design

Object-oriented design of the item-item recommender was done that encapsulates:

- matrix representation -- item × meta-data matrix

- column interpretation -- rules, offsets

- recommendation algorithms

Sparse Matrix Recommender Framework (ItemRecommender object, hub of functionality)[6]

# Extensions: carousels

We want to endow our recommendation engine with the feature of finding for each user $u$ sets of items $S$ and $L$, and sets of items $P$, for which the following statement would be true: "because the user $u$ has consumed the items in $S$ and likes items that have all tags in $P$, the user $u$ would like the items in $L$". 

Using carousels is exemplified by Netflix and Bing for iPad. 
Netflix places carousels at the home page of each user in order to provide more tuned and detailed recommendations.

It is beneficial to give rigorous mathematical deviation of the "carousel problem": carousels for recommendations[7].

## **Visual aids**

## Netflix

![](ItemItemRecommender-Carousels1.png)

## Bing for iPad

![](ItemItemRecommender-Carousels2.png)

## Design

![](ItemItemRecommender-Carousels3.png)

# Extensions: Recommendation Query Language

Instead of designing an API to the Item-Item Recommender (IIR) it might be beneficial to design and implement a Recommendation Query Language (RQL) that is an interface to IIR.

The proposed RQL is very similar SQL.

Here is an example:

{ttfamily recommend for history` $historySet$ `order by relevance limit 20}

{ttfamily recommend according to ( select from (find profile` $historySubSet$`) if tag in (take tags of` $profileSubSet$`)) limit 20}

Note that RQL (and SQL) are functional programming languages.

## BNF for RQL
___

    <recommend for profile> ::=
    "recommend" <data fields spec>
           "according to" <profile spec>
           "order by" <order spec>
           "with weights" <weight-tag list>
           "limit" <integer>

    <recommend for history> ::=
    "recommend" <data fields spec>
          "for history" <item history spec>
          "order by" <order spec>
          "with weights" <weight-tag list>
          "limit" integer>
___
___

    <order spec> ::= "similarity" | "predicted ratings" | "absolute ratings" | "relevance"

    <data fields spec> ::= (<tag type name> | <data field name>) ["," <data fields spec>]

    <profile spec> ::= <weight-tag spec> | <profile finder spec> |  <weight-tag list sum> | <profile filtering>

    <profile finder spec> ::= "find profile for" <item history spec> "with weights" <weight-tag list> [<profile limit spec>]

    <weight-tag list sum> ::= [<double>] <weight-tag list> | [<double>] <weight-tag list> "+" <weight-tag list sum>

    <profile limit spec> ::= "limit" <integer> | "limit with" <outlier detector spec>

    <outlier detector spec> ::= <outlier detector names> | <outlier detector function>

    <tag spec> ::= <tag value> | <tag id>

    <tag seq> ::= <tag spec> ["," <tag seq>]

    <tag list> ::= "(" <tag list> ")"

    <tag type spec> ::= <tag type name> | <tag type id>

    <weight-tag seq> ::= "(" <double> "," <tag spec> ")" | "(" <double> "," <tag type spec> ")" ) ["," <weight-tag list> ]

    <weight-tag list> ::= "automatic" | "(" <weight-tag seq> ")"

    <item history spec> ::= "SQL SELECT statement" | <item history list>

    <item history list> ::= "(" <item id> "," <rating> ")" | <item history list>

    <item history list> ::= "(" <item id> "," <location id> "," <time spec> "," <rating> ")" | <item history list>

    <rating> ::= "1"|"2"|"3"|"4"|"5"

    <profile filtering> ::= "select from" <profile spec> ( "if tag in" <tag list> | "if rank >" <double> | "if rank <" <double>)

    <profile tags> ::= "take tags of" <profile spec>

    <global weight setting> ::= "use" <weight-tag list>

# Summary

By representing item-data relationships in a matrix form, we can use sparse linear algebra algorithms to implement an item-item recommender that is fast, very tunable, and scalable both data-wise and hardware-wise.

The presented three recommenders demonstrate the flexibility of the approach taken ...
