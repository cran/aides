# Change of package `aides`

## aides 1.0.0

### Date: June 15, 2023

Build package *aides* with first three functions including `TestDiscordance()`, `TestDisparity()`, and `DoSA()`.


> 1. `TestDiscordance()` is to test assumption of discordance between theoretical and observed study scale.
> 2. `TestDisparity()` is to test assumption of disparity in sample size.
> 3. `DoSA()` is to conduct sequential analysis.


### Writing style of *aides*

This package is written according to Google's R style. For readers, details of naming rules are listed as follows:
> 1. **.R file** is named using lower case with underscore "_" between words (*e.g. test_disparity.R*). 
> 2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `TestDiscordance()`).
> 3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `dataCases`).
> 4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `dataDiSS$w.normality`).

- Added a `NEWS.md` file to track changes to the package.
