# Parametrized event records data transformations

Anton Antonov   
[MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com)   
[MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)   
[MathematicaVsR at GitHub](https://github.com/antononcube/MathematicaVsR/tree/master/Projects)   
August-October 2018

#### *Version 1.0*

## Introduction

In this document we describe transformations of events records data in order to make that data more amenable for the application of Machine Learning (ML) algorithms.

Consider the following **problem formulation** (done with the next five bullet points.)

   + From data representing a (most likely very) diverse set of events we want to derive contingency matrices corresponding to each of the variables in that data. 

   + The events are observations of the values of a certain set of variables for a certain set of entities. Not all entities have events for all variables. 

   + The observation times do not form a regular time grid. 

   + Each contingency matrix has rows corresponding to the entities in the data and has columns corresponding to time. 

   + The software component providing the functionality should allow parametrization and repeated execution. (As in ML classifier training and testing scenarios.)

The phrase "event records data" is used instead of "time series" in order to emphasize that (i) some variables have categorical values, and (ii) the data can be given in some general database form, like transactions long-form.

The required transformations of the event records in the problem formulation above are done through the monad `ERTMon`, 
\[[AAp3](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicEventRecordsTransformations.m)\]. 
(The name "ERTMon" comes from "**E**vent **R**ecords **T**ransformations **Mon**ad".) 

The monad code generation and utilization is explained in \[[AA1](https://mathematicaforprediction.wordpress.com/2017/06/23/monad-code-generation-and-extension/)\] 
and implemented with 
\[[AAp1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m)\].

It is assumed that the event records data is put in a form that makes it (relatively) easy to extract time series for the set of entity-variable pairs present in that data. 

In brief `ERTMon` performs the following sequence of transformations.

   1. The event records of each entity-variable pair are shifted to adhere to a specified start or end point,

   2. The event records for each entity-variable pair are aggregated and normalized with specified functions over a specified regular grid,

   3. Entity vs. time interval contingency matrices are made for each combination of variable and aggregation function.

The transformations are specified with a "computation specification" dataset. 

Here is an example of an `ERTMon` pipeline over event records:

![ERTMon-small-pipeline-example](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-small-pipeline-example.png)

The rest of the document describes in detail:

   + the structure, format, and interpretation of the event records data and computations specifications,

   + the transformations of time series aligning, aggregation, and normalization,

   + the software pattern design -- a monad -- that allows sequential specifications of desired transformations.

Concrete examples are given using weather data. See 
\[[AAp9](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/WeatherEventRecords.m)\].

## Package load

The following commands load the packages \[AAp1-AAp9\].

    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicEventRecordsTransformations.m"]
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/MonadicProgramming/MonadicTracing.m"]
    Import["https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/Misc/WeatherEventRecords.m"]

## Data load

The data we use is weather data from meteorological stations close to certain major cities. 
We retrieve the data with the function `WeatherEventRecords` from the package
\[[AAp9](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/WeatherEventRecords.m)\].


    ?WeatherEventRecords

> WeatherEventRecords[ citiesSpec_: {{_String, _String}..}, dateRange:{{_Integer, _Integer, _Integer}, {_Integer, _Integer, _Integer}}, wProps:{_String..} : {"Temperature"},  nStations_Integer : 1 ] gives an association with event records data.

    citiesSpec = {{"Miami", "USA"}, {"Chicago", "USA"}, {"London",  "UK"}};
    dateRange = {{2017, 7, 1}, {2018, 6, 31}};
    wProps = {"Temperature", "MaxTemperature", "Pressure", "Humidity", "WindSpeed"};
    res1 = WeatherEventRecords[citiesSpec, dateRange, wProps, 1];

    citiesSpec = {{"Jacksonville", "USA"}, {"Peoria", "USA"}, {"Melbourne", "Australia"}};
    dateRange = {{2016, 12, 1}, {2017, 12, 31}};
    res2 = WeatherEventRecords[citiesSpec, dateRange, wProps, 1];

Here we assign the obtained datasets to variables we use below:

    eventRecords = Join[res1["eventRecords"], res2["eventRecords"]];
    entityAttributes = Join[res1["entityAttributes"], res2["entityAttributes"]];

Here are the summaries of the datasets `eventRecords` and `entityAttributes`:

    RecordsSummary[eventRecords]

![ERTMon-RecordsSummary-eventRecord](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-RecordsSummary-eventRecords.png)


    RecordsSummary[entityAttributes]
    
![ERTMon-RecordsSummary-entityAttributes](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-RecordsSummary-entityAttributes.png)

## Design considerations

### Workflow

The steps of the main event records transformations workflow addressed in this document follow.

   1. Ingest event records and entity attributes given in the [Star schema](https://en.wikipedia.org/wiki/Star_schema) style.

   2. Ingest a computation specification.

      1. Specified are aggregation time intervals, aggregation functions, normalization types and functions.

   3. Group event records based on unique entity ID and variable pairs.

      1. Additional filtering can be applied using the entity attributes.

   4. For each variable find descriptive statistics properties.

      1. This is to facilitate normalization procedures.

      2. Optionally, for each variable find outlier boundaries.

   5. Align each group of records to start or finish at some specified point.

      1. For each variable we want to impose a regular time grid.

   6. From each group of records produce a time series.

   7. For each time series do prescribed aggregation and normalization.

      1. The variable that corresponds to each group of records has at least one (possibly several) computation specifications.

   8. Make a contingency matrix for each time series obtained in the previous step.

      1. The contingency matrices have entity ID's as rows, and time intervals enumerating values of time intervals.

The following flow-chart corresponds to the list of steps above.

![ERTMon-workflows](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-workflows.jpg)

A corresponding monadic pipeline is given in the section "Larger example pipeline".

### Feature engineering perspective

The workflow above describes a way to do feature engineering over a collection of event records data. For a given entity ID and a variable we derive several different time series. 

Couple of examples follow.

   + One possible derived feature (times series) is for each entity-variable pair we make time series of the hourly mean value in each of the eight most recent hours for that entity. The mean values are normalized by the average values of the records corresponding to that entity-variable pair. 

   + Another possible derived feature (time series) is for each entity-variable pair to make a time series with the number of outliers in the each half-hour interval, considering the most recent 20 half-hour intervals. The outliers are found by using outlier boundaries derived by analyzing all values of the corresponding variable, across all entities. 

From the examples above -- and some others -- we conclude that for each feature we want to be able to specify:

   + maximum history length (say from the most recent observation),

   + aggregation interval length,

   + aggregation function (to be applied in each interval),

   + normalization function (per entity, per cohort of entities, per variable),

   + conversion of categorical values into numerical ones.

### Repeated execution

We want to be able to do repeated executions of the specified workflow steps. 

Consider the following scenario. After the event records data is converted to a entity-vs-feature contingency matrix, we use that matrix to train and test a classifier. 
We want to find the combination of features that gives the best classifier results. 
For that reason we want to be able to easily and systematically change the computation specifications (interval size, aggregation and normalization functions, etc.) 
With different computation specifications we obtain different entity-vs-feature contingency matrices, that would have different performance with different classifiers.

Using the classifier training and testing scenario we see that there is another repeated execution perspective: after the feature engineering is done over the training data, we want to be able to execute exactly the same steps over the test data. 
Note that with the training data we find certain global or cohort normalization values and outlier boundaries that have to be used over the test data. 
(Not derived from the test data.)

The following diagram further describes the repeated execution workflow.

![ERTMon-repeated-execution-workflow](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-repeated-execution-workflow.jpg)

Further discussion of making and using ML classification workflows through the monad software design pattern can be found in 
[[AA2](https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows/)].

## Event records data design

The data is structured to follow the style of [Star schema](https://en.wikipedia.org/wiki/Star_schema). 
We have event records dataset (table) and entity attributes dataset (table).

The structure datasets (tables) proposed satisfy a wide range of modeling data requirements. 
(Medical and financial modeling included.)

### Entity event data

The entity event data has the columns "EntityID", "LocationID", "ObservationTime", "Variable", "Value".

    RandomSample[eventRecords, 6]
    
![ERTMon-eventRecords-sample](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-eventRecords-sample.png)

Most events can be described through "Entity event data". 
The entities can be anything that produces a set of event data: financial transactions, vital sign monitors, wind speed sensors, chemical concentrations sensors.

The locations can be anything that gives the events certain "spatial" attributes: medical units in hospitals, sensors geo-locations, tiers of financial transactions.

### Entity attributes data

The entity attributes dataset (table) has attributes (immutable properties) of the entities. (Like, gender and race for people, longitude and latitude for wind speed sensors.)

    entityAttributes[[1 ;; 6]]
    
![ERTMon-entityAttributes-sample](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-entityAttributes-sample.png)

### Example

For example, here we take all weather stations in USA:

    ws = Normal[entityAttributes[Select[#Attribute == "Country" && #Value == "USA" &], "EntityID"]]

    (* {"KMFL", "KMDW", "KNIP", "KGEU"} *)
    
Here we take all temperature event records for those weather stations:

    srecs = eventRecords[Select[#Variable == "Temperature" && MemberQ[ws, #EntityID] &]];

And here plot the corresponding time series obtained by grouping the records by station (entity ID's) and taking the columns "ObservationTime" and "Value":

    grecs = Normal @ GroupBy[srecs, #EntityID &][All, All, {"ObservationTime", "Value"}];
    DateListPlot[grecs, ImageSize -> Large, PlotTheme -> "Detailed"]

![ERTMon-DateListPlot-USA-Temperature](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-DateListPlot-USA-Temperature.png)


## Monad elements

This section goes through the steps of the general `ERTMon` workflow.
For didactic purposes each sub-section changes the pipeline assigned to the variable `p`. 
Of course all functions can be chained into one big pipeline as shown in the section "Larger example pipeline".

### Monad unit

The monad is initialized with ERTMonUnit.

    ERTMonUnit[]

    (* ERTMon[None, <||>] *)
    
### Ingesting event records and entity attributes

The event records dataset (table) and entity attributes dataset (table) are set with corresponding setter functions. 
Alternatively, they can be read from files in a specified directory.

    p =
      ERTMonUnit[]⟹
       ERTMonSetEventRecords[eventRecords]⟹
       ERTMonSetEntityAttributes[entityAttributes]⟹
       ERTMonEchoDataSummary;
       
![ERTMon-echo-data-summary](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-echo-data-summary.png)

### Computation specification

Using the package \[AAp3\] we can create computation specification dataset. 
Below is given an example of constructing a fairly complicated computation specification. 

The package function `EmptyComputationSpecificationRow` can be used to construct the rows of the specification.

    EmptyComputationSpecificationRow[]

    (* <|"Variable" -> Missing[], "Explanation" -> "", 
        "MaxHistoryLength" -> 3600, "AggregationIntervalLength" -> 60, 
        "AggregationFunction" -> "Mean", "NormalizationScope" -> "Entity", 
        "NormalizationFunction" -> "None"|> *)
        
        
    compSpecRows = 
      Join[EmptyComputationSpecificationRow[], <|"Variable" -> #, 
          "MaxHistoryLength" -> 60*24*3600, 
          "AggregationIntervalLength" -> 2*24*3600, 
          "AggregationFunction" -> "Mean", 
          "NormalizationScope" -> "Entity", 
          "NormalizationFunction" -> "Mean"|>] & /@ 
       Union[Normal[eventRecords[All, "Variable"]]];
    compSpecRows =
      Join[
       compSpecRows, 
       Join[EmptyComputationSpecificationRow[], <|"Variable" -> #, 
           "MaxHistoryLength" -> 60*24*3600, 
           "AggregationIntervalLength" -> 2*24*3600, 
           "AggregationFunction" -> "Range", 
           "NormalizationScope" -> "Country", 
           "NormalizationFunction" -> "Mean"|>] & /@ 
        Union[Normal[eventRecords[All, "Variable"]]],
       Join[EmptyComputationSpecificationRow[], <|"Variable" -> #, 
           "MaxHistoryLength" -> 60*24*3600, 
           "AggregationIntervalLength" -> 2*24*3600, 
           "AggregationFunction" -> "OutliersCount", 
           "NormalizationScope" -> "Variable"|>] & /@ 
        Union[Normal[eventRecords[All, "Variable"]]]
       ];

The constructed rows are assembled into a dataset (with `Dataset`). 
The function `ProcessComputationSpecification` is used to convert a user-made specification dataset into a form used by `ERTMon`.

    wCompSpec = 
     ProcessComputationSpecification[Dataset[compSpecRows]][SortBy[#Variable &]]
     
![ERTMon-wCompSpec](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-wCompSpec.png)


The computation specification is set to the monad with the function `ERTMonSetComputationSpecification`.

Alternatively, a computation specification can be created and filled-in as a CSV file and read into the monad. 
(Not described here.)

### Grouping event records by entity-variable pairs

With the function `ERTMonGroupEntityVariableRecords` we group the event records by the found unique entity-variable pairs. 
Note that in the pipeline below we set the computation specification first.

    p =
      p⟹
       ERTMonSetComputationSpecification[wCompSpec]⟹
       ERTMonGroupEntityVariableRecords;

### Descriptive statistics (per variable)

After the data is ingested into the monad and the event records are grouped per entity-variable pairs we can find certain descriptive statistics for the data. This is done with the general function ERTMonComputeVariableStatistic and the specialized function  ERTMonFindVariableOutlierBoundaries.

    p⟹ERTMonComputeVariableStatistic[RecordsSummary]⟹ERTMonEchoValue;

![ERTMon-compute-variable-statistic-1](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-compute-variable-statistic-1.png)


    p⟹ERTMonComputeVariableStatistic⟹ERTMonEchoValue;

![ERTMon-compute-variable-statistic-2](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-compute-variable-statistic-2.png)


    p⟹ERTMonComputeVariableStatistic[TakeLargest[#, 3] &]⟹ERTMonEchoValue;

    (* value: <|Humidity->{1.,1.,0.993}, MaxTemperature->{48,48,48},
                Pressure->{1043.1,1042.8,1041.1}, Temperature->{42.28,41.94,41.89},
                WindSpeed->{54.82,44.63,44.08}|> *)
                
                
### Finding the variables outlier boundaries

The finding of outliers counts and fractions can be specified in the computation specification. 
Because of this there is a specialized function for outlier finding `ERTMonFindVariableOutlierBoundaries`. 
That function makes the association of the found variable outlier boundaries (i) to be the pipeline value and (ii) to be the value of context key "variableOutlierBoundaries". 
The outlier boundaries are found using the functions of the package 
\[[AAp6](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m)\].

If no argument is specified ERTMonFindVariableOutlierBoundaries uses the Hampel identifier (`HampelIdentifierParameters`).

    p⟹ERTMonFindVariableOutlierBoundaries⟹ERTMonEchoValue;

    (* value: <|Humidity->{0.522536,0.869464}, MaxTemperature->{14.2106,31.3494},
                Pressure->{1012.36,1022.44}, Temperature->{9.88823,28.3318},
                WindSpeed->{5.96141,19.4086}|> *)
                
    Keys[p⟹ERTMonFindVariableOutlierBoundaries⟹ERTMonTakeContext]

    (* {"eventRecords", "entityAttributes", "computationSpecification",
        "entityVariableRecordGroups", "variableOutlierBoundaries"} *)
       
In the rest of document we use the outlier boundaries found with the more conservative identifier `SPLUSQuartileIdentifierParameters`.

    p =
      p⟹
       ERTMonFindVariableOutlierBoundaries[SPLUSQuartileIdentifierParameters]⟹
       ERTMonEchoValue;

    (* value: <|Humidity->{0.176,1.168}, MaxTemperature->{-1.67,45.45},
                Pressure->{1003.75,1031.35}, Temperature->{-5.805,43.755},
                WindSpeed->{-5.005,30.555}|> *)

### Conversion of event records to time series

The grouped event records are converted into time series with the function `ERTMonEntityVariableGroupsToTimeSeries`. 
The time series are aligned to a time point specification given as an argument. 
The argument can be: a date object, "MinTime", "MaxTime", or "None". ("MaxTime" is the default.)

    p⟹
      ERTMonEntityVariableGroupsToTimeSeries["MinTime"]⟹
      ERTMonEchoFunctionContext[#timeSeries[[{1, 3, 5}]] &];
      
![ERTMon-records-groups-minTime](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-records-groups-minTime.png)

Compare the last output with the output of the following command.

    p =
      p⟹
       ERTMonEntityVariableGroupsToTimeSeries["MaxTime"]⟹
       ERTMonEchoFunctionContext[#timeSeries[[{1, 3, 5}]] &];

![ERTMon-records-groups-maxTime](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-records-groups-maxTime.png)

### Time series restriction and aggregation.

The main goal of `ERTMon` is to convert a diverse, general collection of event records into a collection of aligned time series over specified regular time grids.

The regular time grids are specified with the columns "MaxHistoryLength" and "AggregationIntervalLength" of the computation specification. 
The time series of the variables in the computation specification are restricted to the corresponding maximum history lengths and are aggregated using the corresponding aggregation lengths and functions. 

    p =
      p⟹
       ERTMonAggregateTimeSeries⟹
       ERTMonEchoFunctionContext[DateListPlot /@ #timeSeries[[{1, 3, 5}]] &];
       
![ERTMon-restriction-and-aggregation](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-restriction-and-aggregation.png)


### Application of time series functions

At this point we can apply time series modifying functions. An often used such function is moving average.

    p⟹
      ERTMonApplyTimeSeriesFunction[MovingAverage[#, 6] &]⟹
      ERTMonEchoFunctionValue[DateListPlot /@ #[[{1, 3, 5}]] &];

![ERTMon-moving-average-application](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-moving-average-application.png)

Note that the result is given as a pipeline value, the value of the context key "timeSeries" is not changed.

(In the future, the computation specification and its handling might be extended to handle moving average or other time series function specifications.)

### Normalization

With "normalization" we mean that the values of a given time series values are divided (normalized) with a descriptive statistic derived from a specified set of values. 
The specified set of values is given with the parameter "NormalizationScope" in computation specification.

At the normalization stage each time series is associated with an entity ID and a variable.

Normalization is done at three different scopes: "entity", "attribute", and "variable".  

Given a time series $T(i,var)$ corresponding to entity ID $i$ and a variable $var$ we define the normalization values for the different scopes in the following ways.

   + Normalization with scope "entity" means that the descriptive statistic is derived from the values of $T(i,var)$ only.

   + Normalization with scope attribute means that 

      + from the entity attributes dataset we find attribute value that corresponds to $i$, 

      + next we find all entity ID's that are associated with the same attribute value,

      + next we find value of normalization descriptive statistic using the time series that correspond to the variable $var$ and the entity ID's found in the previous step.

   + Normalization with scope "variable" means that the descriptive statistic is derived from the values of all time series corresponding to $var$.

Note that the scope "entity" is the most granular, and the scope "variable" is the coarsest.

The following command demonstrates the normalization effect -- compare the $y$-axes scales of the time series corresponding to the same entity-variable pair.

    p =
      p⟹
       ERTMonEchoFunctionContext[DateListPlot /@ #timeSeries[[{1, 3, 5}]] &]⟹
       ERTMonNormalize⟹
       ERTMonEchoFunctionContext[DateListPlot /@ #timeSeries[[{1, 3, 5}]] &];
       
![ERTMon-normalization](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-normalization.png)

Here are the normalization values that should be used when normalizing "unseen data."

    p⟹ERTMonTakeNormalizationValues
    
    (* <|{"Humidity.Range", "Country", "USA"} -> 0.0864597, 
      {"Humidity.Range", "Country", "UK"} -> 0.066, 
      {"Humidity.Range", "Country", "Australia"} -> 0.145968, 
      {"MaxTemperature.Range", "Country", "USA"} -> 2.85468, 
      {"MaxTemperature.Range", "Country", "UK"} -> 78/31, 
      {"MaxTemperature.Range", "Country", "Australia"} -> 3.28871, 
      {"Pressure.Range", "Country", "USA"} -> 2.08222, 
      {"Pressure.Range", "Country", "Australia"} -> 3.33871, 
      {"Temperature.Range", "Country", "USA"} -> 2.14411, 
      {"Temperature.Range", "Country", "UK"} -> 1.25806, 
      {"Temperature.Range", "Country", "Australia"} -> 2.73032, 
      {"WindSpeed.Range", "Country", "USA"} -> 4.13532, 
      {"WindSpeed.Range", "Country", "UK"} -> 3.62097, 
      {"WindSpeed.Range", "Country", "Australia"} -> 3.17226|> *)

### Making contingency matrices

One of the main goals of `ERTMon` is to produce contingency matrices corresponding to the event records data.

The contingency matrices are created and stored as `SSparseMatrix` objects, \[AAp7\].

    p =
      p⟹ERTMonMakeContingencyMatrices;

We can obtain an association of the contingency matrices for each variable-and-aggregation-function pair, or obtain the overall contingency matrix.

    p⟹ERTMonTakeContingencyMatrices
    Dimensions /@ %

![ERTMon-contingency-matrices](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-contingency-matrices.png)


    smat = p⟹ERTMonTakeContingencyMatrix;
    MatrixPlot[smat, ImageSize -> 700]

![ERTMon-contingency-matrix](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-contingency-matrix.png)


    RowNames[smat]
    
    (* {"EGLC", "KGEU", "KMDW", "KMFL", "KNIP", "WMO95866"} *)

## Larger example pipeline

The pipeline shown in this section utilizes all main workflow functions of `ERTMon`. 
The used weather data and computation specification are described above.

![ERTMon-large-pipeline-example](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-large-pipeline-example.png)

![ERTMon-large-pipeline-example-output](https://github.com/antononcube/MathematicaForPrediction/raw/master/MarkdownDocuments/Diagrams/Parametrized-event-records-data-transformations/ERTMon-large-pipeline-example-output.png)

## References

### Packages

\[AAp1\] Anton Antonov, [State monad code generator Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m) .

\[AAp2\] Anton Antonov, [Monadic tracing Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m) .

\[AAp3\] Anton Antonov, [Monadic Event Records Transformations Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicEventRecordsTransformations.m), (2018), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicEventRecordsTransformations.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicEventRecordsTransformations.m) .

\[AAp4\] Anton Antonov, [MathematicaForPrediction utilities](https://github.com/antononcube/MathematicaForPrediction/raw/master/MathematicaForPredictionUtilities.m), (2014), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MathematicaForPredictionUtilities.m) .

\[AAp5\] Anton Antonov, [Cross tabulation implementation in Mathematica](https://raw.githubusercontent.com/antononcube/MathematicaForPrediction/master/CrossTabulate.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/CrossTabulate.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/CrossTabulate.m) .

\[AAp6\] Anton Antonov, [Implementation of one dimensional outlier identifying algorithms in Mathematica](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m), (2013), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction).
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m ](https://github.com/antononcube/MathematicaForPrediction/blob/master/OutlierIdentifiers.m).

\[AAp7\] Anton Antonov, [SSparseMatrix Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/SSparseMatrix.m), (2018), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/SSparseMatrix.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/SSparseMatrix.m) .

\[AAp8\] Anton Antonov, [Monadic contextual classification Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.*
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m) .

\[AAp9\] Anton Antonov, [Weather event records data Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/WeatherEventRecords.m), (2018), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.*
    URL: [https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/WeatherEventRecords.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/Misc/WeatherEventRecords.m) .

### Documents

\[AA1\] Anton Antonov, [Monad code generation and extension](https://mathematicaforprediction.wordpress.com/2017/06/23/monad-code-generation-and-extension/), (2017), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).
    URL: [https://mathematicaforprediction.wordpress.com/2017/06/23/monad-code-generation-and-extension](https://mathematicaforprediction.wordpress.com/2017/06/23/monad-code-generation-and-extension/) .

\[AA1a\] Anton Antonov, [Monad code generation and extension](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md), (2017),  [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction)*.*

\[AA2\] Anton Antonov, [A monad for classification workflows](https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows/), (2018), [MathematicaForPrediction at WordPress](https://mathematicaforprediction.wordpress.com).
    URL: [https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows](https://mathematicaforprediction.wordpress.com/2018/05/15/a-monad-for-classification-workflows/) .


