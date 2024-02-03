<!-- readme: start -->
<!-- title: start -->
<br>

<img src = "https://github.com/EnochKang/RES/blob/main/aides/aides_logo.png?raw=true" align = "left" width = "120" />
<br>

# &nbsp; **aides** 

#### &nbsp; &nbsp; *Additive Information & Details of Evidence Synthesis*

&nbsp; &nbsp; &nbsp; [Enoch Kang](https://orcid.org/0000-0002-4903-942X)
<br>
<br>

<!-- title: end -->
<!-- badges: start -->

<table border = "0">
  
  <tr>
    <td> [![CRAN](https://img.shields.io/cran/v/aides?color=blue&label=CRAN&logo=r&logoColor=skyblue)](https://cran.r-project.org/package=aides) </td>
    <td> [![Date](https://img.shields.io/badge/Date-Dec.20.2023-blue.svg?logo=r&logoColor=skyblue)](https://github.com/EnochKang/RES/blob/main/aides/NEWS.md) </td>
    <td> [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-blue.svg?color=blue&label=Lifecycle&logo=r&logoColor=skyblue)](https://lifecycle.r-lib.org/articles/stages.html#stable) </td>
    <td> [![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg?color=blue&label=Licence&logo=gnu&logoColor=skyblue)](https://www.gnu.org/licenses/gpl-3.0.en.html) </td>
  </tr>
  <tr>
    <td> [![Dependencis](https://tinyverse.netlify.com/badge/aides)](https://cran.r-project.org/package=aides) </td>
    <td> [![Functions](https://img.shields.io/badge/Functions-8-green.svg?logo=r&logoColor=green)](https://drive.google.com/file/d/1gxw_mhdxThBs28MyEf8W5Oq6PcYfJYJW/view?usp=sharing) </td>
    <td> [![Monthly Downloads](https://cranlogs.r-pkg.org:443/badges/aides?color=orange)](https://cranlogs.r-pkg.org:443/badges/aides) </td>
    <td> [![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/aides?color=orange)](https://cran.r-project.org/package=aides) </td>
  </tr>
  <tr>
    <td> </td>
    <td> </td>
  </tr>
</table>


<!-- badges: end -->
<!-- content: start -->
<br>

-   [About aides](#about-aides)
-   [Features](#features)
-   [Dependencies & installation](#dependencies-and-installation)
-   [Flow and functions](#flow-and-functions)
-   [Examples](#examples)
-   [Coding conventions](#coding-conventions)
-   [License](#license)
-   [To do list](#to-do-list)

<!-- content: end -->
<!-- about: start -->
<br>


## About *aides*

*aides* is an **R** package which is planned to support users to do additional analysis or graphics of evidence synthesis. Essentially, package *aides* serves as an aiding toolkit for pooled analysis of aggregated data, crafted with a vision to support a more inclusive and informed approach to evidence-based decision-making; and it is developed with values of flexibility, ease of use, and comprehensibility. Package *aides* will be updated with advances of methodology of data synthesis and evidence evaluation. 

The initial goals of package *aides* are to simplify analysis process for both professionals and public users, and to support them in navigating the complexities of synthesized evidence. Long-term goals of package aides are to support knowledge translation and decision-making based on the obtained information with comprehensive understanding of the evidence.

<!-- content: end -->
<!-- features: start -->
<br>


## Features

Briefly, *aides* currently consists of three tasks as follows:

- **Disparity:** a newly proposed assumption regarding disparities in sample size analysis.

- **Discordance:** a newly proposed assumption regarding discordance in rank of study size analysis.

- **Sequential analysis:** a method to examine the sufficiency of information size.

<!-- features: end -->
<!-- dependencies and installation: start -->
<br>


## Dependencies and installation

Package *aides* depends on various packages, and is developed using [**R (version 4.2.2)**](https://cran.r-project.org/bin/windows/base/old/4.2.2/). Therefore, those packages are concurrently installed with package *aides*. The dependencies are listed as follows:

-  [*boot*](https://cran.r-project.org/package=boot) (**note:** package *aides* is developed using [*boot* **version 1.3-28**](https://cran.r-project.org/src/contrib/Archive/boot/boot_1.3-28.tar.gz))

- [*metafor*](https://cran.r-project.org/package=metafor) (**note:** package *aides* is developed using [*metafor* **version 3.8-1**](https://cran.r-project.org/src/contrib/Archive/metafor/metafor_3.8-1.tar.gz))

- [*meta*](https://cran.r-project.org/package=meta) (**note:** package *aides* is developed using [*meta* **version 6.2-1**](https://cran.r-project.org/src/contrib/Archive/meta/meta_6.2-1.tar.gz))
<br>
<br>

Formal released package *aides* can be installed from [CRAN](https://cran.r-project.org/package=aides) via R with following syntax:

```{r}
install.packages("aides")
```

<!-- dependencies and installation: end -->
<!-- flow and functions: start -->
<br>


## Flow and functions

Users can import their data and do relevant tests or graphics using functions in package *aides*. The present package consists of seven functions listed as follows. 

- **Disparity:**  `PlotDistrSS()`, `TestDisparity()`, and `PlotDisparity()`.

- **Discordance:** `TestDiscordance()`.  
  
- **Sequential analysis:**  `DoSA()`. `DoOSA()`, `PlotOSA()`, and `PlotPower()`.
<br>

#### Disparity:

- **Step 1.** Build or load data.

- **Step 2.** Do disparity test using function `TestDisparity()`.

- **Optional** Illustrate user-defined disparity plot using function `PlotDisparity()`.


#### Discordance:

- **Step 1.** Build or load data.

- **Step 2.** Do discordance test using function `TestDiscordance()`.


#### Sequential analysis:

- **Step 1.** Build or load data.

- **Step 2.** Do sequential analysis using function `DoSA()` or function `DoOSA()`.

- **Step 3.** Illustrate user-defined observed sequential analysis plot using function `PlotOSA()`.


<!-- flow and functions: end -->
<!-- examples: start -->
<br>


## Examples

#### 1. Disparity test

The following steps and syntax demonstrate how user can carry out disparity test.

**STEP 1.** Import data (example of the study by Olkin 1995)

``` {r}
library(meta)
data("Olkin1995")
data <- Olkin1995
```


**STEP 2.** Process data

```{r}
data$n  <- data$n.exp + data$n.cont
```


**STEP 3.** Test assumption of discordance in study size

```{r}
output <- output <- aides::TestDisparity(n = n, 
                                         data = data, 
                                         study = author, 
                                         time = year,
                                         outlier = "MAD", 
                                         rblty = "MAD", 
                                         plot = TRUE)
```


**STEP 4.** Illustrate discordance plot

```{r}
TestDisparity(n = n, 
              data = data, 
              study = author, 
              time = year, 
              outlier = "MAD", 
              plot = TRUE)
```

<p align = "center" width = "25%">
<img src = "https://github.com/EnochKang/RES/blob/main/aides/figure/Figure%20Disparity%20(outlier%20MAD).png?raw=true">
</p>
<br>


```{r}
PlotDisparity(object = output, 
              which = "CV")
```

<p align = "center" width = "25%">
<img src = "https://github.com/EnochKang/RES/blob/main/aides/figure/Figure%20Disparity%20(variability%20robust).png?raw=true">
</p>
<br>


#### 2. Discordance test

The following steps and syntax demonstrate how user can carry out discordance test.

**STEP 1.** Import data (example of the study by Fleiss 1993)

``` {r}
library(meta)
data("Fleiss1993bin")
data <- Fleiss1993bin
```


**STEP 2.** Process data

```{r}
data$n  <- data$n.asp + data$n.plac
data$se <- sqrt((1 / data$d.asp) - (1 / data$n.asp) + (1 / data$d.plac) - (1 / data$n.plac))
```


**STEP 3.** Test assumption of discordance in study size

```{r}
output <- TestDiscordance(n = n, 
                          se = se, 
                          study = study,
                          data = data)
```


**STEP 4.** Illustrate discordance plot

```{r}
TestDiscordance(n = n, 
                se = se, 
                study = study, 
                data = data, 
                plot = TRUE)
```

<p align = "center" width = "25%">
<img src = "https://github.com/EnochKang/RES/blob/main/aides/figure/Figure%20Discordance%20(basic).png?raw=true">
</p>
<br>


#### 3. Sequential analysis

The following steps and syntax demonstrate how user can carry out sequential analysis.

**STEP 1.** Import data (example of the study by Fleiss 1993)

``` {r}
library(meta)
data("Fleiss1993bin")
data <- Fleiss1993bin
```


**STEP 2.** Perform sequentail analysis

```{r}
DoSA(Fleiss1993cont, study, year,
     r1 = d.asp, n1 = n.asp,
     r2 = d.plac, n2 = n.plac,
     measure = "RR", PES = 0.5,
     group = c("Aspirin", "Control"), 
     plot = TRUE)
```

<p align = "center" width = "25%">
<img src = "https://github.com/EnochKang/RES/blob/main/aides/figure/Figure%20DoSA%20(basic).png?raw=true">
</p>
<br>


<!-- examples: end -->
<!-- coding conventions: start -->
<br>


## Coding conventions

There are some rules for version numbering in package *aides* (June 20, 2023). Basically, version number consists of three integers with a period between them (eg. version 1.0.0).

1. Updating the first integer refers to package update(s) with new methodological impact.

2. Changing the second integer refers to package update(s) with new function(s) without new methodological impact.

3. Updating the third integer refers to formal modification(s) of existed function(s).

<br>

This package is mainly written according to [Google's R style](https://web.stanford.edu/class/cs109l/unrestricted/resources/google-style.html). For readers, details of naming rules are listed as follows:

1. **.R file** is named using lower case with underscore "_" between words (*e.g. test_disparity.R*). 

2. **function** is named using verb or verb with noun, and the first character of each word is written in capital letter (e.g. `TestDiscordance()`).

3. **object** is named using noun with the first word in lower case, but the first character of rest words is written using capital letter (e.g. `dataCases`).

4. **variable** is named using noun written in lower case. Words of variable name are separated by "." if a variable name consists of more than two words (e.g. `dataDiSS$w.normality`).

<br>

Common-used prefix in package *aides* are listed as follows:

1. **`angl...`**   refers angle of text.

2. **`clr...`**    refers to color.

3. **`lgc...`**    refers to logic value.

4. **`szFnt...`**  refers to font size.

5. **`szLn...`**   refers to line width.

6. **`szPnt...`**  refers to point size.

7. **`txt...`**    refers text (string).

8. **`typLn...`**  refers type of line.

9. **`typPnt...`** refers type of point.

<!-- coding conventions: end -->
<!-- license: start -->
<br>


## License

This package is licensed under the [GPL-3 License](https://cran.r-project.org/web/licenses/GPL-3).

<!-- license: end -->
<!-- to do list: start -->
<br>


## To do list

Task force will keep update package *aides* for relevant issues.

<!-- to do list: end -->
