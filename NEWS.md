# Change of package `aides`

## aides 1.1.0. (release version)
### Date: August 11, 2023

**Add function:** `PlotDisparity()` to illustrate disparity plot.

\


## aides 1.0.5 (development version)
### Date: July 29, 2023

**Modify function:** improve function `TestDisparity()` with parameters for sorting data and building a data frame for user-defined disparity plot.

> 1. parameter `sort` with options of reference for data sorting (i.e. time, size, and excessive cases).
>
> 2. returning a data frame for user-defined disparity plot by argument `TRUE` for parameter `plot`.
>

\


## aides 1.0.4 (development version)
### Date: July 17, 2023

**Modify function:** improve function `TestDisparity()` with summary information.

> 1. displaying the results of outlier detection.
>
> 2. displaying information of outlier detection method.
>
> 3. removing results of normal distribution test from the summary infomation.
>
>

\

**Modify function:** improve function `TestDisparity()` with parameters for data input and outlier detection.

> 1. parameter `data` can be used for importing a data frame for disparity test.
>
> 2. parameter `time` is a vector of study year.
>
> 3. calculation of outlier detection (default with GESD and MAD for normal and non-normal distribution respectively).
>
> 4. parameter `outlier` consists of four outlier detection methods (i.e. IQR, Z, GESD, and MAD).
>
>

\


## aides 1.0.3 (development version)
### Date: July 01, 2023

**Modify function:** improve function `DoSA()` with parameters for sequential analysis.

> 1. parameter `PV` for presumed variance (default with post-hoc analysis mode).
>
> 2. parameter `ref` with options of reference group (i.e. 1 and 2; default with 2).
>
> 3. parameter `pooling` with options of pooling method (e.g. inverse variance, Mantel-Haensze, and Peto; default with IV).
>
> 4. parameter `adjust` with options of adjustment factor (including D-squared, I-squared, and conceptual heterogeneity; default none).
>
>

\


## aides 1.0.2 (development version)
### Date: June 30, 2023

**Modify function:** improve function `DoSA()` with information and displaying appearance on plot.

> 1. information on model of the meta-analysis and method for heterogeneity estimetor.
>
> 2. displaying spending boundaries with continuous curve rather lines between observed fraction points.
>
> 3. displaying cumulative z score with weighted points according to weights of each data source in the meta-analysis.
>
> 4. displaying required information size with red dash line.
>
> 5. displaying labels of each data source using parameter `id = TURE`.
>
> 6. displaying y axis title with inverted favorable direction using parameter `invert = TRUE`.
>
> 7. displaying beta-spending boundaries using parameter `BSB = TRUE`.
>
>

\


## aides 1.0.1 (development version)
### Date: June 25, 2023

**Modify function:** improve function `DoSA()` with structured  summary information as follows:

> *1. information on main outputs of sequential analysis*
>
> 1.1. acquired sample size
>
> 1.2. required sample size
>
> 1.3. cumulative z score
>
> 1.4. alpha-spending boundary
>
> 1.5. adjusted confidence interval (based on alpha-spending boundary)
>
>
> *2. presumed parameters for sequential analysis*
>
> 2.1. probability of type I error.
>
> 2.2. probability of type II error.
>
> 2.3. statistic power.
>
> 2.4. effect size (including risk in each group for outcome with binary data).
>
> 2.5. variance.
>
>
> *3. meta-analysis*
> 
> 3.1. settings for the meta-analysis.
>
> 3.2. result of the meta-analysis.
>
>
> *4. adjustment factor*
>
> 4.1. diversity (D-squared).
>
> 4.2. heterogeneity (I-squared). 
>
> 4.3. value of the adjustment.
>
>

\


## aides 1.0.0 (release version)
### Date: June 20, 2023

**Release:** *aides* 1.0.0 on the CRAN.
\

### Date: June 15, 2023

**Initiation:** Build package *aides* with first three functions including `TestDisparity()`, `TestDiscordance()`, and `DoSA()`.


> 1. `TestDisparity()` is to test assumption of disparity in sample size.
>
> 2. `TestDiscordance()` is to test assumption of discordance between theoretical and observed study scale.
>
> 3. `DoSA()` is to conduct sequential analysis.
>
>

\


### Writing style of *aides*

This package is written according to Google's R style. For readers, details of naming rules are listed as follows:
> 1. **.R file** is named using lower case with underscore "_" between words (*e.g. test_disparity.R*). 
>
> 2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `TestDiscordance()`).
>
> 3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `dataCases`).
>
> 4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `dataDiSS$w.normality`).
>
>

\


### Version numbering rule of *aides* (June 20, 2023)

version number consists of three integers with a period between them (eg. version 1.0.0).

> 1. Updating the first integer refers to a modification with new methodological impact.
>
> 2. Changing the second integer refers to an update with a new function without new methodological impact.
>
> 3. Updating the third integer refers to a modification in a function.
>
>
