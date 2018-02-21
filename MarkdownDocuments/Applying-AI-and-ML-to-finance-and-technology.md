# Applying Artificial Intelligence and Machine Learning to Finance and Technology 

Anton Antonov   
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)   
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)   
February, 2018

## Introduction

In this blog post I try to provide some further context for the panel discussion:

**"Artificial Intelligence and Machine Learning application in finance and technology"**,

which was part of the conference ["Data Science Salon Miami 2018"](https://datascience.salon/miami/) held in Miami on February 8 and 9.

The blog post can be read independently, but my intent is to provide a brief review of the discussion and further context to (some of) the answers. (It is probably better to see [discussion's recording first](https://medium.com/@formulatedby/latest).)

Also, my comments and remarks are saturated with links for reference and further reading. 

You can also read [the post by panel's discussion host, Irma Becerra](https://www.linkedin.com/pulse/data-science-changing-our-lives-better-irma-becerra-fernandezphd/).

This blog post is not written by someone particularly enamored by [Artificial Intelligence (AI)](https://en.wikipedia.org/wiki/Artificial_intelligence), [Data Science (DS)](https://en.wikipedia.org/wiki/Data_science), or [Machine Learning (ML)](https://en.wikipedia.org/wiki/Machine_learning). I do like working in those disciplines, but I do think there are more interesting and fascinating mathematical disciplines than them. Also, I think AI, DS, and ML should be seen through the lens of [Operations Research](https://en.wikipedia.org/wiki/Operations_research), which gives the most fruitful perspectives of their utilization. 


### Briefly about the event

The conference "Data Science Salon Miami 2018" included approximately 300 attendants. (I was told that there were ~60% data scientist and data analysts, and ~40% managers.)

## The panel people

Here is a list of the people participating in the panel (Irma was the host):

- [Irma Becerra-Fernandez](https://datascience.salon/speakers/irma-becerra-fernandez-ph-d/), [in](https://www.linkedin.com/in/irma-becerra-fernandez-ph-d-8753a91/)

    -  Ph.D. Provost & Chief Academic Officer at St Thomas University
  
 - [Colleen Farrelly](https://datascience.salon/speakers/colleen-farrelly/), [in](https://www.linkedin.com/in/colleenmfarrelly/)

    - Data Scientist at Kaplan
 
- [Mauro Damo](https://datascience.salon/speakers/mauro-damo/), [in](https://www.linkedin.com/in/maurodamo/)

    - Sr. Data Scientist at Dell Technologies

- [Anton Antonov](https://datascience.salon/speakers/anton-antonov/), [in](https://www.linkedin.com/in/anton-antonov-89a8797) 

    - Applied Mathematics Consultant at Accendo Data LLC


## The panel questions

Here are the main questions asked during the panel session:

1. What was your path to Data Science? 

2. What are some misconceptions about AI? What do you see as being the future of AI?

3. How is artificial intelligence helping us engage with customers?

4. What techniques can we expect to see in terms of informing our marketing strategy so we can build predictive models to support our businesses? (backup question)

5. What can we do make advancements in Data Science more accessible to the public?

Here are some of the questions from the audience:

1. How to apply AI or Machine Learning to process automation?

2. What is going to happen if an adopted AI technology fails?
   

## On the main panel questions

### What was your path to Data Science? 

All of the people in the panel became data scientists later in their career. Each of us at first was studying or doing something else.

Of course, that is to be expected since the term "data science" and the related occupation came into [fashion relatively recently](https://trends.google.com/trends/explore?date=all&q=data%20science).

(Although apparently [the term itself is fairly old](https://en.wikipedia.org/wiki/Data_science#History).)

### What are some misconceptions about AI? What do you see as being the future of AI?

This is of course a long and fruitful topic.

Here -- and during the panel session -- I want to concentrate of what kind of thinking processes generate those misconceptions.
(Obviously this a less laborious task than enumerating and discussing the different concrete misconceptions.)

#### Weak AI

I side with the so called ["Weak AI"](https://en.wikipedia.org/wiki/Weak_AI). Here are some summarizing points.

- Weak AI has a goal to adequately do certain mental tasks by humans. In general, Weak AI does not try to replicate the human approaches, methods, and algorithms for those tasks.

- All algorithms are based on 0s and 1s, so generally, I find "AI" misleading and "Strong AI" an interesting thing to think about, but far from practical use.

- Classifiers are just a complicated if-statement.

    - Derived with deep learning or other algorithms.

    - Also, classification problems belong to the conceptually simplest ML branch ([Supervised learning](https://en.wikipedia.org/wiki/Supervised_learning).)

Generally, my personal opinions on "AI", come from living through at least one [AI winter](https://en.wikipedia.org/wiki/AI_winter), and being a subject in the [AI effect](https://en.wikipedia.org/wiki/AI_effect).

[!["AI-winters-and-winter-episodes"](https://imgur.com/k48lvD3l.png)](https://imgur.com/k48lvD3.png)

#### The ingredients of coming to wrong ideas about AI

Study the following mind-map, ["AI misconceptions reasons"](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Misc/AI-misconceptions-reasons.pdf).

[!["AI-misconseptions-reasons"](https://imgur.com/NQeIcTw.png)](https://imgur.com/NQeIcTw.png)

I think the major ingredients of getting the wrong AI ideas are two.

1. Confusion of high performance with competence.

    - Often, confusion of high performance in some narrow problem domain with competence in an enclosing more general problem domain.

2. Exponential growth extrapolation of AI's advances.

    - This is probably related to the [Weber-Fechner law](https://en.wikipedia.org/wiki/Weber–Fechner_law).  

My favorite analogy to point 1 above is the following.

There are two principal ways to fish: with bait and a fishing rod, and by draining the lake and picking the fish you want.
Any other way of fishing is something in between. (For example, using a net or a bomb.)

So, the high performance of draining the lake can be confused with the ability of bait selection and fishing-spot picking.
(And yes, not knowing how the AI algorithms work is a big misconception maker too.)

[!["Two-ways-to-fish"](https://imgur.com/ToHBosK.png)](https://imgur.com/ToHBosK.png)

As an illustration of "draining the lake" approach to interesting puzzles humans play with, review (skim over) this article: ["Solving Sudoku as an integer programming problem"](http://community.wolfram.com/groups/-/m/t/974303). 

  - We "drain the lake" by formulating an integer optimization problem with 729 variables and 1089 constraints. By solving the integer programming problem we "pick the fish." 
    
  - Note, that this is not even considered AI now. Granted, we do need a symbolic computations system like [Mathematica](http://www.wolfram.com), which is a very advanced system. (And yes, a good symbolic manipulation system can be seen as AI, especially 4-5 decades ago.)


### How is artificial intelligence helping us engage with customers?

I specialize in making recommenders and conversational agents. 

For items that require significant investment in time (movies, books) or money (houses, automobiles, freelancers) recommenders have to make very good explanations of the recommendations. This is usually based on similarities to items consumed in the past or stated preferences.

For items that can be easily tried on and discarded (songs) we are better off making prediction algorithms that predict the "survival" of the item in customer's mind. (How long a song is going to be in a playlist?)

[The recommenders](http://library.wolfram.com/infocenter/Conferences/7964/) I developed in the past tend to be very agile : fast, easy to tune, with clear "recommendation proofs." Very often the agility of the recommender system is more important that giving good or precise recommendations "out of the box."

Customer engagement through conversational agents is much more about envisioning of the right work-flow and mind-flow of the customer. Although clarity and simplicity are important, I like the idea of using grammar rules and agent names that are challenging or intriguing. (Like, "Will they kill me?" or "I wanna eat!".)

### What techniques can we expect to see in terms of informing our marketing strategy so we can build predictive models to support our businesses?

In many ways the application of AI to Finance is somewhat easier, since financial data tends to be well-curated.

For example,

- in healthcare different patient health data can be messy (and incomplete) and generally speaking human body is inherently complex;

- health insurance financial data, though, is well-curated (since people need to get payed) and fairly simple.
    
To clarify, if we have records with four columns:
        
       claim ID, claim total amount, transaction amount, timestamp

then we already in a position to do fair amount of modeling and prediction.


### What can we do make advancements in Data Science more accessible to the public?

My answer was two-fold:

1. it is important to be able to communicate AI and ML concepts, ideas, and functionalities to stakeholders in business projects and, generally, to curious people, but

2. from the other hand, some of the underlying algorithms require mastery of multiple computer science and mathematical disciplines.

In other words:

1. of course the data scientist knowing his stuff should be able to explain the AI and ML functionalities to laypersons, but

2. from the other hand, "art is for the educated."

I would like to point out that technology itself has already democratized and popularized the AI and ML advancements. Everyone is familiar with, say, content recommendations, search queries completion, and optical character recognition. Few decades ago these functionalities would have been considered science fiction or part of Strong AI.

## Some afterthoughts

- The main panel questions and the questions from the audience made me want to discuss a general classification of the AI application -- study the following mind-map ["Application of AI and ML"](https://github.com/antononcube/MathematicaForPrediction/blob/master/Documentation/Misc/Application-AI-and-ML.pdf).

[!["AI and Machine Learning application in technology"](https://imgur.com/gTt6LIQ.png)](https://imgur.com/gTt6LIQ.png)

- As I mentioned, classification belongs to conceptually the simplest part of ML, Supervised Learning. The hardest part of ML is [Reinforcement Learning](https://en.wikipedia.org/wiki/Reinforcement_learning).

- Descriptions of how to [analyze the pre-election tweets of Donald Trump](https://github.com/antononcube/MathematicaVsR/tree/master/Projects/TextAnalysisOfTrumpTweets) or apply the [Great Conversation idea](https://mathematicaforprediction.wordpress.com/2017/12/24/the-great-conversation-in-usa-presidential-speeches/) to interesting collections of text do help the communication of Data Science ideas to the curious.

- I would be much more interested in developing agriculture recommendation systems than recommenders for digital content consumption (movies, books, songs) or dating. 

- Also, I am a big advocate of utilizing recommenders in [Operations Research](https://en.wikipedia.org/wiki/Operations_research) frameworks. 

    - Those frameworks optimize quantified satisfaction or financial goals of the organizations or customers.

- I am very impressed by the making and utilization of the [Dynamic Analysis and Replanning Tool (DART)](https://en.wikipedia.org/wiki/Dynamic_Analysis_and_Replanning_Tool). In 4-5 years DART completely payed off the money directed into AI by DARPA. 

- It seems that [AI came recently into fashion](https://trends.google.com/trends/explore?date=all&q=AI) somewhat coinciding with the advancements in Neural Networks through the so called ["deep learning" on 2011-2012](https://en.wikipedia.org/wiki/Deep_learning#History).

    - These kind of observations bring articles like ["Is AI Riding a One-Trick Pony?"](https://www.technologyreview.com/s/608911/is-ai-riding-a-one-trick-pony/).








