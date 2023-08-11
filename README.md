
<!-- readme: start -->
<!-- title: start -->
\

<img src = "https://github.com/EnochKang/RES/blob/main/aides/aides_logo.png?raw=true" align = "left" width = "100" />



# &nbsp; **aides:** 
### &nbsp; *Additive Information & Details of Evidence Synthesis*
    
&nbsp; &nbsp; [Enoch Kang](https://orcid.org/0000-0002-4903-942X)

<!-- title: end -->
<!-- badges: start -->

[![CRAN](https://img.shields.io/cran/v/aides?color=blue&label=CRAN&logo=r)](https://cran.r-project.org/package=aides)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg?color=grassgreen)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Licence](https://img.shields.io/badge/licence-GPL--3-brightgreen.svg?color=grassgreen)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![Monthly Downloads](https://cranlogs.r-pkg.org:443/badges/aides?color=orange)](https://cranlogs.r-pkg.org:443/badges/aides)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/aides?color=orange)](https://cran.r-project.org/package=aides)

<!-- badges: end -->
<!-- content: start -->

\

## About package *aides*

*aides* is an **R** package which is planned to support users to do additional analysis or graphics of evidence synthesis. Users can use functions in *aides* by calling the library with following syntax:


```{r}
install.packages("aides")
```



-   [Feature](#features)
-   [Flow and functions](#flow-and-functions)
-   [Usage and examples](#usage-and-examples)
-   [To do list](#to-do-list)


## Feature

Briefly, *aides* consists of three tasks as follows:

- **Disparity:** a newly proposed assumption regarding disparities in sample size analysis.

- **Discordance:** a newly proposed assumption regarding discordance in rank of study size analysis.

- **Sequential analysis:** a method to examine the sufficiency of information size.


    
## Flow and functions


Users can import their data and do relevant tests or graphics using functions in package *aides*. The present package consists of four functions listed as follows. 

- **Disparity:**  `TestDisparity()` and `PlotDisparity()`.

- **Discordance:** `TestDiscordance()`.  
  
- **Sequential analysis:**  `DoSA()`.
\

#### Disparity:

- **Step 1.** Build or load data.

- **Step 2.** Do disparity test using function `TestDisparity()`.

- **Optional** Illustrate user-defined disparity plot using function `PlotDisparity()`.


#### Discordance:

- **Step 1.** Build or load data.

- **Step 2.** Do discordance test using function `TestDiscordance()`.


#### Sequential analysis:

- **Step 1.** Build or load data.

- **Step 2.** Do sequential analysis using function `DoSA()`.


## Usage and examples

The following steps and syntax demonstrate how user can carry out discordance test.

> **STEP 1.** Import data (example of the study by Fleiss 1993)
>
> ``` {r}
library(meta)
data("Fleiss1993bin")
data <- Fleiss1993bin
> ```
>
>
> **STEP 2.** Process data
>
> ```{r}
data$n  <- data$n.asp + data$n.plac
data$se <- sqrt((1 / data$d.asp) - (1 / data$n.asp) + (1 / data$d.plac) - (1 / data$n.plac))
> ```
>
>
>
> **STEP 3.** Test assumption of discordance in study size
>
> ```{r}
output <- TestDiscordance(n = n, 
                          se = se, 
                          study = study,
                          data = data)
> ```
>
> **STEP 4.** Illustrate discordance plot
> ```{r}
TestDiscordance(n = n, 
                se = se, 
                study = study, 
                data = data, 
                plot = TRUE)
> ```
>
> Output:
>
> ```{r, eval = TRUE, echo = FALSE,  fig.cap = "Figure 1. an example of discordance plot", fig.height = 4, fig.width = 8, fig.align = "center", out.width = "80%"}
library(meta)
data("Fleiss1993bin")
data <- Fleiss1993bin
data$n  <- data$n.asp + data$n.plac
data$se <- sqrt((1 / data$d.asp) - (1 / data$n.asp) + (1 / data$d.plac) - (1 / data$n.plac))
TestDiscordance(n = n, 
                se = se, 
                study = study, 
                data = data, 
                plot = TRUE)
> ```



## To do list

Task force will keep update package *aides* for relevant issues.
