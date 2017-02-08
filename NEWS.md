# statutils v1.2

### Major changes

* `reduction()` is now called `reduce_model()`. The function returns an object
of class `lm` so that all the related methods work correctly.

* `conf_int()` and `summary.list()` have been deleted due to the 
upgrade of `reduce_model()`.

### Minor changes

* Upgraded documentation

---

# statutils v1.1

### Major changes

* `conf_int()`: method to compute confidence intervals when either a linear 
model or a reduce model computed by `lm()` and `reduction()`, respectively, are 
passed.

### Minor changes

* Few reviews of the documentation.
