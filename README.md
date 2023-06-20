<img src="../aides/vignettes/aides_logo.png" width="15%" />

[Enoch Kang](https://orcid.org/0000-0002-4903-942X)

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/aides)](https://cran.r-project.org/package=aides)
[![Monthly Downloads](https://cranlogs.r-pkg.org:443/badges/aides)](https://cranlogs.r-pkg.org:443/badges/aides)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/aides)](https://cran.r-project.org/package=aides)

Package *aides* is planned to support users to do additional analysis or graphics. Users can use functions in *aides* by calling the library with following syntax:


    install.packages("aides")

-   [Feature](#features)
-   [Flow and functions](#flow-and-functions)
-   [Usage and examples](#usage-and-examples)
-   [To do list](#to-do-list)


## Feature

Briefly, *aides* consists of three functions.

- **Discordance:** a newly proposed assumption regarding discordance in rank of study size analysis.

- **Disparity:** a newly proposed assumption regarding disparities in sample size analysis.

- **Sequential analysis:** a method to examine the sufficiency of information size.


    
## Flow and functions


Users can import their data and do relevant tests or graphics using functions in package *aides*. The present package consists of three functions listed as follows. 

**Discordance:** `TestDiscordance()`.  
  
**Disparity:**  `TestDisparity()`.

**Sequential analysis:**  `DoSA()`.
\

#### Discordance:

**Step 1.** Build or load data.

**Step 2.** Do discordance test using function `TestDiscordance()`.


#### Disparity:

**Step 1.** Build or load data.

**Step 2.** Do disparity test using function `TestDisparity()`.


#### Sequential analysis:

**Step 1.** Build or load data.

**Step 2.** Do sequential analysis using function `DoSA()`.


## Usage and examples

The following steps and syntax demonstrate how user can carry out discordance test.

> **STEP 1.** Import data (example of the study by Fleiss 1993)
> ```{r, eval = FALSE}
library(meta)
data("Fleiss1993bin")
data <- Fleiss1993bin
> ```
>
> **STEP 2.** Process data
> ```{r, eval = FALSE}
data$n  <- data$n.asp + data$n.plac
data$se <- sqrt((1 / data$d.asp) - (1 / data$n.asp) + (1 / data$d.plac) - (1 / data$n.plac))
> ```
>
> **STEP 3.** Test assumption of discordance in study size
> ```{r, eval = FALSE}
output <- TestDiscordance(n = n, 
                          se = se, 
                          study = study,
                          data = data)
> ```
>
> **STEP 4.** Illustrate discordance plot
> ```{r, eval = FALSE}
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
