# **Classification and association rules for census income data**
Original posted at [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com/) on March 30, 2014 by Anton Antonov

## Introduction

In this blog post I am going to show (some) analysis of census income data — the so called “Adult” data set, \[[1](http://archive.ics.uci.edu/ml/datasets/Census+Income)\] — using three types of algorithms: decision tree classification, naive Bayesian classification, and association rules learning. Mathematica packages for all three algorithms can be found at the project [MathematicaForPrediction](https://github.com/antononcube/MathematicaForPrediction) hosted at GitHub, \[[2](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m),[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/NaiveBayesianClassifier.m),[4](https://github.com/antononcube/MathematicaForPrediction/blob/master/AprioriAlgorithm.m)\].

(The census income data set is also used in the description of the R package “arules”, \[[7](https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf)\].)

In the census data every record represents a person with 14 attributes, the last element of a record is one of the labels `{“>=50K”,”<50K”}`. The relationships between the categorical variables in that data set was described in my previous blog post, “Mosaic plots for data visualization”.

For this data the questions I am most interested in are the following.

- **Question 1:** Which of the variables (age, occupation, sex, etc.) are most decisive for determining the income of a person?

- **Question 2:** Which values for which variables form conditions that would imply high income or low income? (I.e. “>50K” or “<=50K”.)

- **Question 3:** What conclusions or confirmations we can get from answering the previous two questions?

One way to answer Question 1 is to use following steps, \[8\].

1. Build a classifier with the training set.

2. Verify using the test set that good classification results are obtained.

3. If the number of variables (attributes) is k for each i, 1<=i<=k :

    3.1. Shuffle the values of the i-th column of the test data and find the classification success rates.

4. Compare the obtained k classification success rates between each other and with the success rates obtained by the un-shuffled test data.

5. The variables for which the classification success rates are the worst are the most decisive.

Following these steps with a decision tree classifier, \[[2](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m)\], I found that “marital-status” and “education-num” (years of education) are most decisive to give good prediction for the “>50K” label. Using a naive Bayesian classifier, \[[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/NaiveBayesianClassifier.m)\], the most significant variables are “marital-status” and “relationship”. (More details are given in the sections “Application of decision trees” and “Application of naive Bayesian classifier”.)

One way to answer Question 2 is to find which values of the variables (e.g. “Wife”, “Peru”, “HS-grad”, “Exec-managerial”) associate most frequently with “>50K” and “<=50K” respectively and apply different Bayesian probability statistics on them. This is what the application of Associative rules learning gives, \[[9](http://en.wikipedia.org/wiki/Association_rule_learning)\]. Another way is to use mosaic plots, \[[5](https://github.com/antononcube/MathematicaForPrediction/blob/master/MosaicPlot.m),[9](http://en.wikipedia.org/wiki/Association_rule_learning)\], and prefix trees (also known as “tries”) \[[6](https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m),[11](http://en.wikipedia.org/wiki/Trie),[12](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Tries.pdf)\].

In order to apply Association rule learning we need to make the numerical variables categorical — we need to partition them into non-overlapping intervals. (This derived, “all categorical” data is also amenable to be input data for mosaic plots and prefix trees.)

Insights about the data set using Mosaic Plots can be found in my previous blog post [“Mosaic plots for data visualization”](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/), \[[13](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/)\]. The use of Mosaic Plots in \[[13](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/)\] is very similar to the Naive Bayesian Classifiers application discussed below.

## Data set

The data set can be found and taken from http://archive.ics.uci.edu/ml/datasets/Census+Income, \[[1](http://archive.ics.uci.edu/ml/datasets/Census+Income)\].

The description of the data set is given in the file “adult.names” of the data folder. The data folder provides two sets with the same type of data “adult.data” and “adult.test”; the former is used for training, the latter for testing.

The total number of records in the file “adult.data” is 32561; the total number of records in the file “adult.test” is 16281.

Here is how the data looks like:

![Adult census income data sample][1]

Since I did not understand the meaning of the column “fnlwgt” I dropped it from the data.

Here is a summary of the data:

![Adult census income data summary][2]

As it was mentioned in the introduction, only 24% of the labels are “>50K”. Also note that 2/3 of the records are for males.

## Scatter plots and mosaic plots

Often scatter plots and mosaic plots can give a good idea of the general patterns that hold in the data. This sub-section has a couple of examples, but presenting extensive plots is beyond the scope of this blog post. Let me point out that it might be very beneficial to use these kind of plots with *Mathematica*‘s dynamic features ([like Manipulate and Tooltip](https://mathematicaforprediction.files.wordpress.com/2014/03/adult-data-manipulate-and-tooltip-for-mosaic-plots.png)), or make a grid of mosaic plots.

Mosaic plots of the categorical variables of the data can be seen in my previous blog post [“Mosaic plots for data visualization”](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/).

Here is a table of the histograms for “age”, “education-num”, and “hours-per-week”:

![adult-data-scatter-plots-age-education-num-hours-per-week][3]

Here is a table with scatter plots for all numerical variables of the data:

![adult-data-scatter-plots-age-education-num-capital-gain-capital-loss-hours-per-week][4]

## Application of decision trees

The building and classification with decision trees is straightforward. Since the label “>50K” is only a quarter of the records I consider the classification success rates for “>50K” to be more important.

![adult-data-Decision-tree-classification-success-rates][5]

I experimented with several sets of parameters for decision tree building. I did not get a classification success rate for “>50K” better than 0.644 . Using pruning based on the Minimal Description Length (MDL) principle did not give better results. (I have to say I find MDL pruning to be an elegant idea, but I am not convinced that it works that
well. I believe decision tree pruning based on test data would produce much better results. Only the MDL decision tree pruning is implemented in \[[2](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m)\].)

The overall classification success rate is in line with the classification success ratios listed in explanation of the data set; see the file “adult.names” in \[[1](http://archive.ics.uci.edu/ml/datasets/Census+Income)\].

Here is a table with the results of the column shuffling experiments described in the introduction (in red is the name of the data column shuffled):

![adult-data-Decision-tree-classification-shuffled-success-rates-table][6]

Here is a plot of the “>50K” success rates from the table above:

![adult-data-Decision-tree-classification-shuffled-success-rates-plot][7]

We can see from the table and the plot that variables “marital-status”, “education-num”, “capital-gain”, “age”, and “occupation” are very decisive when it comes to determining high income. The variable “marital-status” is significantly more decisive than the others.

While considering the decisiveness of the variable “marital-status” we can bring the following questions:
1. Do people find higher paying jobs after they get married?
2. Are people with high paying jobs more likely to marry and stay married?

Both questions are probably answered with “Yes” and probably that is why “marital-status” is so decisive. It is hard to give quantified answers to these questions just using decision trees on this data — we would need to know the salary and marital status history of the individuals (different data) or to be able to imply it (different algorithm).

We can see the decisiveness of “age”, “education-num”, “occupation”, and “hours-per-week” as natural. Of course one is expected to receive a higher pay if he has studied longer, has a high paying occupation, is older (more experienced), and works more hours per week. Note that this statement explicitly states the direction of the correlation: we do assume that longer years of study bring higher pay. It is certainly a good idea to consider the alternative direction of the correlation, that people first get high paying jobs and that these high paying jobs allow them to get older and study longer.

## Application of naive Bayesian classifiers

The naive Bayesian classifier, \[[3](https://github.com/antononcube/MathematicaForPrediction/blob/master/NaiveBayesianClassifier.m)\], produced better classification results than the decision trees for the label “>50K”:

![adult-data-NBC-classification-success-rates][8]

Here is a table with the results of the column shuffling experiments described in the introduction (in red is the name of the data column shuffled):

![adult-data-NBC-classification-shuffled-success-rates-table][9]

Here is a plot of the “>50K” success rates from the table above:

![adult-data-NBC-classification-shuffled-success-rates-plot][10]

In comparison with the decision tree importance of variables experiments we can notice that:

1. “marital-status” is very decisive and it is the second most decisive variable;

2. the most decisive variable is “relationship” but it correlates with “marital-status”;

3. “age”, “occupation”, “hours-per-week”, “capital-gain”, and “sex” are decisive.

## Shuffled classification rates plots comparison

Here are the two shuffled classification rates plots stacked together for easier comparison:

![adult-data-Decision-tree-and-NBC-classification-shuffled-success-rates-plots][11]

## Data modification

In order to apply the association rules finding algorithm Apriori, \[[4](https://github.com/antononcube/MathematicaForPrediction/blob/master/AprioriAlgorithm.m)\], the data set have to be modified. The modification is to change the numerical variables “age”, “education-num”, and “age” into categorical. I just partitioned them into non-overlapping intervals, labeled the intervals, and assigned the labels according the variable values. Here is the summary of the modified data for just these variables:

![adault-data-numerical-to-categorical-columns-summary][12]

## Finding association rules

Using the modified data I found a large number of association rules with the Apriori algorithm, \[[4](https://github.com/antononcube/MathematicaForPrediction/blob/master/AprioriAlgorithm.m)\]. I used the measure called “confidence” to extract the most significant rules. The confidence of an association rule $A \rightarrow C$ with antecedent $A$ and consequent $C$ is defined to be the ratio P$(A \cap C)/$P$(C)$. The higher the ratio the more confidence we have in the rule. (If the ratio is 1 we have a logical rule, $C \subset A$.)

Here is a table showing the rules with highest confidence for the consequent being “>50K”:

![adult-data-association-rules-more-than-50K][13]

From the table we can see for example that 2.1% of the data records (or 693 records) show that for a married man who has studied 14 years and originally from USA there is a 0.79 probability that he earns more than $50000.

Here is a table showing the rules with highest confidence for the consequent being “<=50K”:

![adult-data-association-rules-less-than-50K][14]

The association rules in these tables confirm the findings with the classifiers: marital status, age, and education are good predictors of income labels “>50K” and “<=50K”.

## Conclusion

The analysis confirmed (and quantified) what is considered common sense:

Age, education, occupation, and marital status (or relationship kind) are good for predicting income (above a certain threshold).

Using the association rules we see for example that

(1) if a person earns more than $50000 he is very likely to be a married man with large number of years of education;

(2) single parents, younger than 25 years, who studied less than 10 years, and were never-married make less than $50000.

## References

\[1\] Kohavi, R. and Becker, B. (1996), [Census Income Data Set](http://archive.ics.uci.edu/ml/datasets/Census+Income), 
at [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml). 

\[2\] Antonov, A., [Decision tree and random forest implementations in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/AVCDecisionTreeForest.m), (2013), 
source code at [MathematicaForPrediction project at GitHub]( https://github.com/antononcube/MathematicaForPrediction), 
package [AVCDecisionTreeForest.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/AVCDecisionTreeForest.m).

\[3\] Antonov, A., [Implementation of naive Bayesian classifier generation in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/NaiveBayesianClassifier.m), (2013),
source code at [MathematicaForPrediction project at GitHub]( https://github.com/antononcube/MathematicaForPrediction), 
package [NaiveBayesianClassifier.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/NaiveBayesianClassifier.m).

\[4\] Antonov, A., [Implementation of the Apriori algorithm in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/AprioriAlgorithm.m), (2013), 
source code at [MathematicaForPrediction project at GitHub]( https://github.com/antononcube/MathematicaForPrediction), 
package [AprioriAlgorithm.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/AprioriAlgorithm.m).

\[5\] Antonov, A., [Mosaic plot for data visualization implementation in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/MosaicPlot.m), (2014), 
source code at [MathematicaForPrediction project at GitHub]( https://github.com/antononcube/MathematicaForPrediction), 
package [MosaicPlot.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MosaicPlot.m).

\[6\] Antonov, A., [Tries with frequencies Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/TriesWithFrequencies.m), (2013), 
source code at [MathematicaForPrediction project at GitHub]( https://github.com/antononcube/MathematicaForPrediction), 
package [TriesWithFrequencies.m](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/TriesWithFrequencies.m).

\[7\] Hahsler, M. et al., [Introduction to arules - A computational environment for mining association rules and frequent item sets](https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf), (2012), 
[CRAN](https://cran.r-project.org).

\[8\] Breiman, L. et al., Classification and regression trees, Chapman & Hall, 1984.

\[9\] Wikipedia entry, [Association rules learning](http://en.wikipedia.org/wiki/Association_rule_learning), http://en.wikipedia.org/wiki/Association_rule_learning .

\[10\] Antonov, A., [Mosaic plots for data visualization](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Mosaic%20plots%20for%20data%20visualization.pdf), (March, 2014), 
documentation at [MathematicaForPrediction project at GitHub](https://github.com/antononcube/MathematicaForPrediction).

\[11\] Wikipedia entry, [Trie](http://en.wikipedia.org/wiki/Trie), URL: http://en.wikipedia.org/wiki/Trie .

\[12\] Antonov, A., [Tries](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Tries.pdf), (December, 2013),
documentation at [MathematicaForPrediction project at GitHub](https://github.com/antononcube/MathematicaForPrediction).

\[13\] Antonov, A., [Mosaic plots for data visualization](https://mathematicaforprediction.wordpress.com/2014/03/17/mosaic-plots-for-data-visualization/), (March, 2014)
blog post at [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com/).


[1]:http://i.imgur.com/5FkKOJ1.png
[2]:http://i.imgur.com/IvcnUJy.png
[3]:http://i.imgur.com/yjNoqQH.png
[4]:http://i.imgur.com/rA6T40I.png
[5]:http://i.imgur.com/mttNgcg.png
[6]:http://i.imgur.com/ukiw8dk.png
[7]:http://i.imgur.com/PaFazqq.png
[8]:http://i.imgur.com/Z5ACa7L.png
[9]:http://i.imgur.com/6H8fEMn.png
[10]:http://i.imgur.com/eETD8hd.png
[11]:http://i.imgur.com/fSFdWEN.png
[12]:http://i.imgur.com/boNtAYx.png
[13]:http://i.imgur.com/tzo3Lm0.png
[14]:http://i.imgur.com/QW0k1H3.png
