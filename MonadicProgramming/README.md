# Monadic programming


This folder has two kinds of packages:

1. for implementing, code generation of monads,

2. utilization of the monad pipeline design pattern for different tasks.


## Monad code generation

The approach taken here treats the Monadic programming pipeline as a [Software design pattern](https://en.wikipedia.org/wiki/Software_design_pattern). 

The monads are obtained through code generation -- see the packages:
[MaybeMonadCodeGenerator](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MaybeMonadCodeGenerator.m),
[StateMonadCodeGenerator](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m), 
and the article \[[1](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md)\]. 


## Applications

Using the [State Monad package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m)
several packages are developed for different tasks in Machine Learning and Natural Language Processing.

- Classifier creation and testing, [MonadicContextualClassification.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m). 

- Text analysis, [MonadicTextAnalyzer.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTextAnalyzer.m).

- Latent semantic analysis, [MonadicLatentSemanticAnalysis.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicLatentSemanticAnalysis.m).

- Phrase completion, [MonadicPhraseCompletion.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicPhraseCompletion.m).

- Quantile regression, [MonadicQuantileRegression.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicQuantileRegression.m).

The monad tracing package [MonadicTracing.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m)
demonstrates how a monad can act as Decorator in the [Decorator design pattern](https://en.wikipedia.org/wiki/Decorator_pattern).
 

## Presentations

The WTC 2017 presentation 
["Monadic Programming: With Application to Data Analysis, Machine Learning and Language Processing"](https://www.wolfram.com/broadcast/video.php?v=2050), 
\[2\], gives a good introduction and overview of the approach taken. 
  
## References

\[1\] Anton Antonov, 
["Monad code generation and extension"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md), 
(2017), 
[MathematicaForPrediction at GitHub](https://github.com/antononcube/).

\[2\] Anton Antonov, 
["Monadic Programming: With Application to Data Analysis, Machine Learning and Language Processing"](http://wac.36f4.edgecastcdn.net/0036F4/pub/www.wolfram.com/technology-conference/2017/Antonov.zip),
[Wolfram Technology Conference 2017](https://www.wolfram.com/events/technology-conference/2017/presentations/#wednesday).
([YouTube video](https://m.youtube.com/watch?v=_cIFA5GHF58).)

\[3\] Anton Antonov, 
["A monad for classification workflows"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-classification-workflows.md),
(2018),
[MathematicaForPrediction at GitHub](https://github.com/antononcube/).
