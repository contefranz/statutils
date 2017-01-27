---
# statutils v1.1

**statutils** is a collection of utilities regarding both modelling, 
data management and wrangling. Below is a list
of the current available functions grouped by topic/usage. 

### Linear Models

Here you can find two functions: 

* `reduction()`: a procedure to iteratively reduce a linear model by selecting 
predictors according to a given significance threshold. 

* `summary()`: the related method to print results out of `reduction()`.

* `conf_int()`: method to compute confidence intervals when either a linear 
model or a reduce model computed by `lm()` and `reduction()`, respectively, are 
passed.

### General Utilities

Here you can find general purpose utilities: 

* `count_na()`: print the number (and the proportion) of missing data in a 
`data.frame` like object.

* `def_complete()`: after `mice()` and `complete()` functions in the package `mice`
have been run, the resulting dataset is, based on th `action` argument, a repeated
version of the original input. This is done to track all the simulation runs carried
out with `mice()`. `def_complete()` defines a dataset with the same row number of 
the original one, but with no missing data.