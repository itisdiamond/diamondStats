# You can install:
install.packages("remotes")

remotes::install_github("itisdiamond/diamondStats")

If necessary, you will be prompted to install additional required packages.
It is recommended to accept and confirm the installation of dependencies to ensure that the package works correctly without any further manual intervention.

# diamondStats: Usage Guide

This guide explains all possible input forms for each main function in the package

# diamondStats: Usage Guide

This guide explains all possible input forms for each main function in the package, clarifying accepted argument types, typical usage, and options. The goal is to make the syntax clear both to expert R users and to those less familiar with function calling conventions.

---

## sdmean\_diamond

**Compute mean and standard deviation of a numeric vector.**

```r
# Typical usage: input is a numeric vector
sdmean_diamond(x)
# x: numeric vector (e.g., x = c(1, 2, 3, 4, NA))

# With a column from a data frame
sdmean_diamond(data$column)
```

**Returns:** Named numeric vector: Mean and SD.

---

## barplot\_error

**Draw barplot of group means with error bars.**

```r
# Formula interface (response ~ group) with data framearplot_error(y ~ group, data = mydata)

# Specify type of error bars and graphical options
barplot_error(y ~ group, data = mydata, error.bars = "se", col = "blue")

# Confidence intervals, custom y limits
barplot_error(y ~ group, data = mydata, error.bars = "conf.int", conf.level = 0.99, ylim = c(0, 10))
```

* `y`: numeric response
* `group`: factor or character

**Returns:** Barplot to current device. Invisibly returns bar midpoints.

---

## mean\_diamond

**Compare mean and standard deviation using classical, trimmed, and winsorized methods.**

```r
mean_diamond(x)
mean_diamond(x, trim = 0.1)                 # Trimming
mean_diamond(x, probs = c(0.01, 0.99))      # Winsorizing
mean_diamond(x, verbose = TRUE)             # Prints and returns data frame
# x: numeric vector
```

**Returns:** Data frame with method, n, mean, sd, etc.

---

## SkewKurt\_diamond

**Compute skewness and kurtosis, with interpretation.**

```r
# Basic usage with a numeric vector
SkewKurt_diamond(x)
SkewKurt_diamond(x, na.rm = TRUE)
# x: numeric vector (at least 8 valid values recommended)

# Formula interface (per-group calculation)
SkewKurt_diamond(y ~ group, data = mydata)
```

**Returns**: If given a numeric vector: Named list with skewness, kurtosis, and their classification.
If used with a formula (y ~ group): Data frame with skewness and kurtosis (and interpretation) for each group.

---

## correlation\_diamond

**Flexible correlation with CI, strength, subgrouping.**

```r
# Numeric vectors\correlation_diamond(x, y)

# Formula interface with data frame
correlation_diamond(y ~ x, data = mydata)

# Choose method
correlation_diamond(y ~ x, data = mydata, method = "pearson")
correlation_diamond(y ~ x, data = mydata, method = c("pearson", "spearman"))

# Grouped (subset as grouping variable or logical)
correlation_diamond(y ~ x, data = mydata, subset = group)
correlation_diamond(y ~ x, data = mydata, subset = logical_vector)
```

* `x, y`: numeric vectors of same length
* `group`: factor/character/numeric grouping variable

**Returns:** Data frame with method, coefficient, CI, interpretation, etc.

---

## cor\_influence\_diamond

**Assess leave-one-out influence on correlation.**

```r
cor_influence_diamond(x, y)
cor_influence_diamond(x, y, method = "spearman")
cor_influence_diamond(x, y, plot = TRUE)
cor_influence_diamond(x, y, improve = 2, worsen = 2)
cor_influence_diamond(x, y, subset = logical_vector)
```

* `x, y`: numeric vectors (same length)
* `subset`: logical vector (optional)

**Returns:** Data frame with influence for each observation; can plot highlights.

---

## pcorrelation\_diamond

**Partial correlation, flexible control variables and grouping.**

```r
pcorrelation_diamond(x, y, z)
pcorrelation_diamond(x, y, z, method = "pearson")
pcorrelation_diamond(x, y, z, method = c("spearman", "kendall"))
pcorrelation_diamond(x, y, z, subset = group)
pcorrelation_diamond(x, y, z, data = mydata)
```

* `x, y`: numeric vectors
* `z`: numeric vector or data.frame (controls)
* `group`: optional grouping variable

**Returns:** Data frame with method, coefficient, interpretation, n, etc.

---

## glass.delta\_1s

**Glass's delta (single sample vs. population) with bootstrap CI.**

```r
glass.delta_1s(x, mu = value, sd = value)
glass.delta_1s(x, mu = value, sd = value, R = 5000, conf.level = 0.99, seed = 1234)
print(glass.delta_1s(x, mu = value, sd = value))
```

* `x`: numeric vector
* `mu`, `sd`: reference mean and SD

**Returns:** S3 object; print method displays summary with CI.

---

## mvnorm

**Energy test of multivariate normality.**

```r
mvnorm(x, y)
mvnorm(x, y, z)
mvnorm(x, y, subset = group)
mvnorm(x, y, subset = logical_vector)
```

* At least two numeric vectors, equal length
* Subset: logical or grouping variable (factor/character/numeric)

**Returns:** Prints result; invisibly returns test object or list by group.

---

## lm\_summary

**Regression diagnostics: formula or lm object.**

```r
lm_summary(lm_object)
lm_summary(response ~ predictor, data = mydata)
lm_summary(lm_object, best_res = 5, worst_res = 5)
lm_summary(lm_object, plot = TRUE)
lm_summary(lm_object, order = "asc")
```

* Input: lm object, or formula with data frame
* Options: show best/worst residuals, plotting, order

**Returns:** Data frame with regression stats per observation.

---

## Dependencies

Some functions require the following packages:

* `DescTools`
* `ppcor`
* `energy`

Install if necessary:

```r
install.packages(c("DescTools", "ppcor", "energy"))
```

---

## General Notes

* Functions accept both plain vectors and data frame columns as input.
* `subset` arguments accept logical, factor, character, or numeric vectors (see each function).
* For full details, use R help: `?function_name`.

---
