## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, out.width = "10%"------------------------------------------
knitr::include_graphics("aides_logo.png")

## ----setup--------------------------------------------------------------------
library(aides)

## ---- eval = FALSE------------------------------------------------------------
#  library(meta)
#  data("Fleiss1993bin")
#  data <- Fleiss1993bin

## ---- eval = FALSE------------------------------------------------------------
#  data$n  <- data$n.asp + data$n.plac
#  data$se <- sqrt((1 / data$d.asp) - (1 / data$n.asp) + (1 / data$d.plac) - (1 / data$n.plac))

## ---- eval = FALSE------------------------------------------------------------
#  output <- TestDiscordance(n = n,
#                            se = se,
#                            study = study,
#                            data = data)

## ---- eval = FALSE------------------------------------------------------------
#  TestDiscordance(n = n,
#                  se = se,
#                  study = study,
#                  data = data,
#                  plot = TRUE)

## ---- eval = TRUE, echo = FALSE,  fig.cap = "Figure 1. an example of discordance plot", fig.height = 4, fig.width = 8, fig.align = "center", out.width = "80%"----
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

