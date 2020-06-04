# Introduction
- Introduce functional correspondence.
- Emphasize the mechanical nature of transformation.
- Reference multiple papers where it has been used.
- Emphasize the need for usage of different semantic formats.
- Emphasize that deriving one format from another offers advantages.
- Emphasize that there is no tool which allows for automatic transformation.
- State assumptions about reader's knowledge
  + I assume familiarity with semantics formats
  + I assume familiarity with functional programming
- Summarize the contents of the following text.

## Interpreter Definition Language
- Introduce the language (in general)
- Example interpreter for big-step _IMP_ with detailed description
- Link to Users's Manual

## Semantic Formats
### Denotational Semantics
- Most abstract formulation.
- Requires complex mathematical machinery.
- Example of denotation for _IMP_ (or maybe λ-calculus).
- Example definitional interpreter embodying these semantics.

### Big-step Operational Semantics
- The most natural operational semantics
- What does it mean for it to be operational
- Relation between programs and final values
- Not suitable for reasoning about divergent computations
- Cannot distinguish between divergent and stuck term
- Example semantics for c-b-n λ-calculus with substitution
- Interpreter for these semantics

### Abstract Machines
- The most explicit operational semantics.
  Pins down details like argument evaluation order, redex search etc.  
- First order transition system
- Show Krivine machine
- Interpreter for this machine

# The Functional Correspondence
- Inputs, outputs of the transformation
- Introduce running example -- λ-calculus meta-circular interpreter with environment, corresponding to denotational semantics.
## Continuation Passing Style
- Introduce CPS, what are the goals of the transformation
- Serious/trivial term classification
- Independence of evaluation orders between meta and object languages
- Simple translation algorithm for c-b-v λ-calculus
- Resulting interpreter with only `eval` translated
- Stress that it produces administrative redexes
- Hint that partial CPS translation is required

## Defunctionalization
- Introduce defunctionalization
- Define the function spaces, dispatch on tags, top-level apply functions

# Semantics Transformer
## Administrative Normal Form
## Control Flow Analysis
## Selective CPS
## Selective Defunctionalization
## Let Inlining

# Evaluation

# User’s Manual

# Developer’s Manual
