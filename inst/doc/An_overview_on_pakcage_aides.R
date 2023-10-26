## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, fig.align = "left", out.width = "30%"----------------------
knitr::include_graphics("aides_logo.png")

## -----------------------------------------------------------------------------
library(aides)

## ----setup, echo = FALSE, warning = FALSE, message = FALSE--------------------
library(aides)
library(meta)

## ----eval = FALSE-------------------------------------------------------------
#  library(meta)
#  data("Olkin1995")
#  dataOlkin1995 <- Olkin1995

## ----eval = FALSE-------------------------------------------------------------
#  dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont

## ----eval = FALSE-------------------------------------------------------------
#  shapiro.test(dataOlkin1995$n)
#  PlotDistrSS(dataOlkin1995$n)

## ----result-distrSS, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
shapiro.test(dataOlkin1995$n)

## ----plot-distrSS, fig.cap = "An example for visualization of distribution of study sizes", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 5, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
PlotDistrSS(n = n,
            data = dataOlkin1995, 
            study = author, 
            time = year)

## ----eval = FALSE-------------------------------------------------------------
#  TestDisparity(n = n,
#                data = dataOlkin1995,
#                study = author,
#                time = year)

## ----result-disparity, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
TestDisparity(n = n, 
              data = dataOlkin1995, 
              study = author, 
              time = year)

## ----eval = FALSE-------------------------------------------------------------
#  TestDisparity(n = n,
#                data = dataOlkin1995,
#                study = author,
#                time = year,
#                plot = TRUE)

## ----plot-disparity-outlier, eval = TRUE, fig.cap = "An example for disparity-outlier plot", echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 4, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Olkin1995")
dataOlkin1995   <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
TestDisparity(n = n,
              data = dataOlkin1995, 
              study = author, 
              time = year, 
              plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  rsltDisparity <- TestDisparity(n = n,
#                                 data = dataOlkin1995,
#                                 study = author,
#                                 time = year,
#                                 vrblty = "MAD")

## ----result-disparity-vrblty-MAD, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Olkin1995")
dataOlkin1995 <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
rsltDisparity <- TestDisparity(n = n, 
                               data = dataOlkin1995, 
                               study = author, 
                               time = year,
                               vrblty = "MAD")

## ----eval = FALSE-------------------------------------------------------------
#  PlotDisparity(rsltDisparity,
#                which = "CV",
#                szFntAxsX = 1)

## ----plot-disparity-variability-MAD, eval = TRUE, fig.cap = "An example for disparity-variability (robust) plot", echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Olkin1995")
dataOlkin1995   <- Olkin1995
dataOlkin1995$n <- dataOlkin1995$n.exp + dataOlkin1995$n.cont
rsltDisparity <- TestDisparity(n = n,
                               data = dataOlkin1995, 
                               study = author, 
                               time = year,
                               vrblty = "MAD", 
                               plot = FALSE)

PlotDisparity(rsltDisparity, 
              which = "CV",
              szFntAxsX = 1)

## ----eval = FALSE-------------------------------------------------------------
#  library(meta)
#  data("Fleiss1993bin")
#  dataFleiss1993bin <- Fleiss1993bin

## ----eval = FALSE-------------------------------------------------------------
#  dataFleiss1993bin$n  <- dataFleiss1993bin$n.asp + dataFleiss1993bin$n.plac
#  dataFleiss1993bin$se <- sqrt((1 / dataFleiss1993bin$d.asp) - (1 / dataFleiss1993bin$n.asp) + (1 / dataFleiss1993bin$d.plac) - (1 / dataFleiss1993bin$n.plac))

## ----eval = FALSE-------------------------------------------------------------
#  TestDiscordance(n = n,
#                  se = se,
#                  study = study,
#                  data = dataFleiss1993bin)

## ----result-discordance, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Fleiss1993bin")
dataFleiss1993bin    <- Fleiss1993bin
dataFleiss1993bin$n  <- dataFleiss1993bin$n.asp + dataFleiss1993bin$n.plac
dataFleiss1993bin$se <- sqrt((1 / dataFleiss1993bin$d.asp) - (1 / dataFleiss1993bin$n.asp) + (1 / dataFleiss1993bin$d.plac) - (1 / dataFleiss1993bin$n.plac))
TestDiscordance(n = n, 
                se = se, 
                study = study,
                data = dataFleiss1993bin)

## ----eval = FALSE-------------------------------------------------------------
#  TestDiscordance(n = n,
#                  se = se,
#                  study = study,
#                  data = dataFleiss1993bin,
#                  plot = TRUE)

## ----plot-discordance, fig.cap = "An example for discordance plot", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 12, fig.align = "center", out.width = "100%"----
data("Fleiss1993bin")
dataFleiss1993bin <- Fleiss1993bin
dataFleiss1993bin$n  <- dataFleiss1993bin$n.asp + dataFleiss1993bin$n.plac
dataFleiss1993bin$se <- sqrt((1 / dataFleiss1993bin$d.asp) - (1 / dataFleiss1993bin$n.asp) + (1 / dataFleiss1993bin$d.plac) - (1 / dataFleiss1993bin$n.plac))
TestDiscordance(n = n, 
                se = se, 
                study = study, 
                data = dataFleiss1993bin, 
                plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  library(meta)
#  data("Fleiss1993cont")
#  dataFleiss1993cont <- Fleiss1993cont

## ----eval = FALSE-------------------------------------------------------------
#  DoSA(Fleiss1993cont,
#       source = study,
#       time = year,
#       m1 = mean.psyc,
#       sd1 = sd.psyc,
#       n1 = n.psyc,
#       m2 = mean.cont,
#       sd2 = sd.cont,
#       n2 = n.cont,
#       measure = "SMD",
#       PES = 0.2,
#       group = c("Psychotherapy", "Control"),
#       plot = FALSE)

## ----result-DoSA, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Fleiss1993cont")
dataFleiss1993cont <- Fleiss1993cont
DoSA(Fleiss1993cont, 
     source = study, 
     time = year,
     m1 = mean.psyc, 
     sd1 = sd.psyc, 
     n1 = n.psyc,
     m2 = mean.cont, 
     sd2 = sd.cont, 
     n2 = n.cont,
     measure = "SMD", 
     PES = 0.2,
     group = c("Psychotherapy", "Control"))

## ----eval = FALSE-------------------------------------------------------------
#  DoSA(Fleiss1993cont,
#       source = study,
#       time = year,
#       m1 = mean.psyc,
#       sd1 = sd.psyc,
#       n1 = n.psyc,
#       m2 = mean.cont,
#       sd2 = sd.cont,
#       n2 = n.cont,
#       measure = "SMD",
#       PES = 0.2,
#       group = c("Psychotherapy", "Control"))

## ----plot-sequential, fig.cap = "An example for sequential analysis", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993cont")
dataFleiss1993cont <- Fleiss1993cont
DoSA(Fleiss1993cont, 
     source = study, 
     time = year,
     m1 = mean.psyc, 
     sd1 = sd.psyc, 
     n1 = n.psyc,
     m2 = mean.cont, 
     sd2 = sd.cont, 
     n2 = n.cont,
     measure = "SMD", 
     PES = 0.2,
     group = c("Psychotherapy", "Control"), 
     plot = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  DoOSA(Fleiss1993cont,
#       source = study,
#       time = year,
#       m1 = mean.psyc,
#       sd1 = sd.psyc,
#       n1 = n.psyc,
#       m2 = mean.cont,
#       sd2 = sd.cont,
#       n2 = n.cont,
#       measure = "SMD",
#       group = c("Psychotherapy", "Control"))

## ----result-OSA, eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE----
data("Fleiss1993cont")
dataFleiss1993cont <- Fleiss1993cont
DoOSA(Fleiss1993cont, 
     source = study, 
     time = year,
     m1 = mean.psyc, 
     sd1 = sd.psyc, 
     n1 = n.psyc,
     m2 = mean.cont, 
     sd2 = sd.cont, 
     n2 = n.cont,
     measure = "SMD", 
     group = c("Psychotherapy", "Control"))

## ----eval = FALSE-------------------------------------------------------------
#  DoOSA(Fleiss1993cont,
#       source = study,
#       time = year,
#       m1 = mean.psyc,
#       sd1 = sd.psyc,
#       n1 = n.psyc,
#       m2 = mean.cont,
#       sd2 = sd.cont,
#       n2 = n.cont,
#       measure = "SMD",
#       group = c("Psychotherapy", "Control"),
#       plot = TRUE)

## ----plot-OSA, fig.cap = "An example for observed sequential analysis plot", eval = TRUE, echo = FALSE, warning = FALSE, message = FALSE, results = "hide", fig.height = 6, fig.width = 8, fig.align = "center", out.width = "100%"----
data("Fleiss1993cont")
dataFleiss1993cont <- Fleiss1993cont
DoOSA(Fleiss1993cont, 
     source = study, 
     time = year,
     m1 = mean.psyc, 
     sd1 = sd.psyc, 
     n1 = n.psyc,
     m2 = mean.cont, 
     sd2 = sd.cont, 
     n2 = n.cont,
     measure = "SMD", 
     group = c("Psychotherapy", "Control"),
     plot = TRUE)

