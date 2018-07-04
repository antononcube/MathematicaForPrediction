# Package structure and develoment workflow

## Introduction

Because the development of some the monad packages can be large and complex, 
a more flexible and powerful structure documented Mathematica/WL is required. 

For the monad package implementation we take a minimalistic approach. 
The basic idea is to have:
 
 - **different modules of project-independent full-fledged packages**,
 
 - a **custom loader**, and
 
 - an **interface section.**

Below is (briefly) described the multi-package dependency, loading, and code generation of the package 
["MonadicContextualClassification.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m), \[1\].

The goal of the package \[1\] is to provide a Domain Specific Language (DSL) for rapid specification of machine learning classification workflows. 

## Package file structure

1. Packages import code section.

   - Here packages are imported from GitHub with `Import`. 

     + (If the corresponding definitions are not already in the context.)
 
   - These are the mentioned above **"project-independent full-fledged packages"**.

     + (They were developed before the package \[1\].)

     + (The package \[1\] provides a DSL that combines those "full-fledged packages.")

2. Package interface definitions code section.

   - Here after `BeginPackage` the `::usage` definitions are given.

   - This is the **"interface section"** mentioned above.

3. The private context declaration.

4. The declaration of the package contexts with `Needs` code section.

   - (For the contexts of the packages in the imports code section 1.)

5. Code generation section.

   - In this case with    
`` GenerateStateMonadCode["MonadicContextualClassification`ClCon",...]`` 

   - This code generation code section together with the imports code section 1, 
      correspond to the **"custom loader"** mentioned above. 

     + (The package \[2\] generates the definitions of some of the functions of the package \[1\].)

6. Function implementations code section.

## Testing

The development of the package \[1\] was/is done with two unit test files. 
(One with [hand-written unit tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicContextualClassification-Unit-Tests.wlt), the other with [randomly generated tests](https://github.com/antononcube/MathematicaForPrediction/blob/master/UnitTests/MonadicContextualClassificationRandomPipelinesUnitTests.m). The latter is specific to the methodology behind the package functionality.) 

The point is that unit tests are crucial when dealing with this kind of complex package dependencies. 
(And packages that deal with complex subjects.)


## Summary diagram

Below is given a diagram that summarizes the development of ["MonadicContextualClassification.m"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m). For more details see the end sections of the document ["A monad for classification workflows"](https://github.com/antononcube/MathematicaForPrediction/blob/master/MarkdownDocuments/A-monad-for-classification-workflows.md).

[![ClCon-development-cycle](https://i.imgur.com/hmMPfCrl.png)](https://i.imgur.com/hmMPfCr.png)

## Example run 

The following screenshot is of an example run that demonstrates the package import prompts and the utilization of 
the loaded packages in a classification pipeline. 
(The packages have functionalities for training classifier ensembles, making ROC plots, 
and finding importance of variables.)

[![ClCon-example-run-with-Import](https://imgur.com/X2Nephgh.png)](https://imgur.com/X2Nephg.png)

## References

\[1\] Anton Antonov, [Monadic contextual classification Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/MonadicContextualClassification.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction/).

\[2\] Anton Antonov, [State monad code generator Mathematica package](https://github.com/antononcube/MathematicaForPrediction/blob/master/MonadicProgramming/StateMonadCodeGenerator.m), (2017), [MathematicaForPrediction at GitHub](https://github.com/antononcube/MathematicaForPrediction/).
