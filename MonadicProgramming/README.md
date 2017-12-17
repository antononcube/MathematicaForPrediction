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


The monad tracing package [MonadicTracing.m](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicTracing.m)
demonstrates how a monad can act as Decorator in the [Decorator design pattern](https://en.wikipedia.org/wiki/Decorator_pattern).
 
  
## References

\[1\] Anton Antonov, ["Monad code generation and extension"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/Monad-code-generation-and-extension.md), (2017), 
[MathematicaForPrediction at GitHub](https://github.com/antononcube/).
