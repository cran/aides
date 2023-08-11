#' @title Sequential analysis.
#'
#' @author Enoch Kang
#'
#' @description
#' **DoSA()** is a function for conducting sequential analysis.
#'
#' @param data    DATAFRAME consists of relevant information.
#' @param source  CHARACTER for labeling the included data sets.
#' @param time    NUMERIC values of time sequence.
#' @param n       INTEGER values of sample sizes.
#' @param es      NUMERIC values of effect sizes.
#' @param se      NUMERIC values of standard errors for the effect sizes.
#' @param r1      INTEGER values of observed events in group 1 in the included data.
#' @param m1      NUMERIC values of estimated means in group 1 in the included data.
#' @param sd1     NUMERIC values of standard deviations in group 1 in the
#'                included data.
#' @param n1      INTEGER values of sample sizes in group 1 in the included data.
#' @param r2      INTEGER values of observed events in group 2 in the included data.
#' @param m2      NUMERIC values of estimated means in group 2 in the included data.
#' @param sd2     NUMERIC values of standard deviations in group 2 in the
#'                included data.
#' @param n2      INTEGER values of sample sizes in group 2 in the included data.
#' @param group   CHARACTER for labeling two groups.
#' @param ref     NUMERIC values of 1 or 2 for indicating group 1 or 2 as reference.
#' @param prefer  CHARACTER of "small" and "large" for indicating which direction
#'                is beneficial effect in statistic test.
#' @param measure CHARACTER for indicating which statistic measure should be used.
#' @param model   CHARACTER of "random" and "fixed" for indicating whether
#'                to use random-effects model or fixed-effect model.
#' @param method  CHARACTER for indicating which estimator should be used in
#'                random-effects model. In addition to the default "DL" method,
#'                the current version also supports "REML" and "PM" methods for
#'                calculating heterogeneity estimator.
#' @param pooling CHARACTER for indicating which method has to be used for pooling
#'                binary data. Current version consists of "IV" and "MH" for
#'                binary data pooling.
#' @param alpha   NUMERIC value between 0 to 1 for indicating the assumed type I
#'                error.
#' @param beta    NUMERIC value between 0 to 1 for indicating the assumed type II
#'                error.
#' @param PES     NUMERIC value for indicating the presumed meaningful effect size.
#' @param PV      NUMERIC value for indicating the presumed variance of the
#'                meaningful effect size.
#' @param adjust  CHARACTER for indicating how to adjust expected information size.
#'                Current version consists of "none" and "D2" for the adjustment.
#' @param plot    LOGIC value for indicating whether to illustrate alpha-spending
#'                monitoring plot.
#' @param id      LOGIC value for indicating whether to label each data source.
#' @param invert  LOGIC value for indicating whether to invert plot.
#' @param BSB     LOGIC value for indicating whether to illustrate beta-spending
#'                boundaries.
#'
#'
#' @return
#' **DoSA()** returns a summary on the result of sequential analysis.
#' \item{studies}{Numbers of studies included in the sequential analysis.}
#' \item{AIS}{Acquired information size refers to the total sample size in the
#'       sequential analysis.}
#' \item{alpha}{A numeric value of type I error for the sequential analysis.}
#' \item{beta}{A numeric value of type II error for the sequential analysis.}
#' \item{PES}{A numeric value of presumed meaningful effect size for the
#'       sequential analysis.}
#' \item{Varian}{A numeric value of presumed variance of the meaningful effect
#'       size for the sequential analysis.}
#' \item{Divers}{A numeric value to show diversity in the pooled analysis.}
#' \item{AF}{A numeric value of adjustment factor.}
#' \item{RIS.org}{A numeric value for required information size without adjustment.}
#' \item{RIS.adj}{A numeric value for adjusted required information size.}
#' \item{frag}{A vector of fraction of each study included in the sequential
#'       analysis.}
#' \item{weight}{A vector of weight of each study included in the sequential
#'       analysis.}
#' \item{es.cum}{A vector of cumulative effect size in the sequential analysis.}
#' \item{se.cum}{A vector of standard error for the cumulative effect size in the
#'       sequential analysis.}
#' \item{zval.cum}{A vector of cumulative z-value in the sequential analysis.}
#' \item{asb}{A data frame of alpha-spending values for each study.}
#' \item{aslb}{A numeric value for lower alpha-spending boundary.}
#' \item{asub}{A numeric value for upper alpha-spending boundary.}
#'
#'
#' @references
#' Jennison, C., & Turnbull, B. W. (2005). Meta-analyses and adaptive group
#' sequential designs in the clinical development process.
#' **Journal of biopharmaceutical statistics**, *15(4)*, 537–558.
#' https://doi.org/10.1081/BIP-200062273.
#'
#' Wetterslev, J., Jakobsen, J. C., & Gluud, C. (2017). Trial sequential analysis
#' in systematic reviews with meta-analysis. **BMC medical research methodology**,
#' *17(1)*, 1-18.
#'
#' NCSS Statistical Software (2023). **Group-sequential analysis for two proportions**.
#' In *PASS Documentation*. Available online:
#' https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Group-Sequential_Analysis_for_Two_Proportions.pdf
#'
#'
#' @seealso \code{\link{TestDisparity}}, \code{\link{TestDiscordance}}
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Fleiss (1993)
#' library(meta)
#' data("Fleiss1993cont")
#'
#' # 2. Perform sequential analysis
#'  output <- DoSA(Fleiss1993cont, study, year,
#'                 m1 = mean.psyc, sd1 = sd.psyc, n1 = n.psyc,
#'                 m2 = mean.cont, sd2 = sd.cont, n2 = n.cont,
#'                 measure = "SMD", PES = 0.5,
#'                 group = c("Psychotherapy", "Control"), plot = TRUE)
#'
#' ## End(Not run)
#'
#' @export DoSA



DoSA <- function(data = NULL,
                 source = NULL,
                 time = NULL,
                 n = NULL,
                 es = NULL,
                 se = NULL,
                 r1 = NULL,
                 m1 = NULL,
                 sd1 = NULL,
                 n1 = NULL,
                 r2 = NULL,
                 m2 = NULL,
                 sd2 = NULL,
                 n2 = NULL,
                 group = c("Group 1", "Group 2"),
                 ref = 2,
                 prefer = "small",
                 measure = "ES",
                 model = "random",
                 method = "DL",
                 pooling = "IV",
                 alpha = 0.05,
                 beta = 0.2,
                 PES = "post-hoc",
                 PV = "post-hoc",
                 adjust = "none",
                 plot  = FALSE,
                 id = FALSE,
                 invert = FALSE,
                 BSB = FALSE) {

  # 01. CHECK arguments -----
  lgcInData   <- ifelse(is.null(data), FALSE, TRUE)
  lgcInSource <- ifelse(is.null(substitute(source)), FALSE, TRUE)
  lgcInN      <- ifelse(is.null(substitute(n)), FALSE, TRUE)
  lgcInES     <- ifelse(is.null(substitute(es)), FALSE, TRUE)
  lgcInSE     <- ifelse(is.null(substitute(se)), FALSE, TRUE)
  lgcInTime   <- ifelse(is.null(substitute(time)), FALSE, TRUE)
  lgcInR1     <- ifelse(is.null(substitute(r1)), FALSE, TRUE)
  lgcInM1     <- ifelse(is.null(substitute(m1)), FALSE, TRUE)
  lgcInSD1    <- ifelse(is.null(substitute(sd1)), FALSE, TRUE)
  lgcInN1     <- ifelse(is.null(substitute(n1)), FALSE, TRUE)
  lgcInR2     <- ifelse(is.null(substitute(r2)), FALSE, TRUE)
  lgcInM2     <- ifelse(is.null(substitute(m2)), FALSE, TRUE)
  lgcInSD2    <- ifelse(is.null(substitute(sd2)), FALSE, TRUE)
  lgcInN2     <- ifelse(is.null(substitute(n2)), FALSE, TRUE)
  lgcInPES    <- ifelse(is.null(substitute(PES)), FALSE, TRUE)
  lgcInPV     <- ifelse(is.null(substitute(PV)), FALSE, TRUE)

  lgcReq1     <- ifelse(lgcInData == TRUE, TRUE, FALSE)
  lgcReq2     <- ifelse(FALSE %in% c(lgcInN, lgcInES, lgcInSE), FALSE, TRUE)
  lgcReq3     <- ifelse(FALSE %in% c(lgcInR1, lgcInN1, lgcInR2, lgcInN2), FALSE, TRUE)
  lgcReq4     <- ifelse(FALSE %in% c(lgcInM1, lgcInSD1, lgcInN1, lgcInM2, lgcInSD2, lgcInN2), FALSE, TRUE)
  lgcReq5     <- ifelse(FALSE %in% c(lgcInSource, lgcInTime, lgcInPES, lgcInPV), FALSE, TRUE)

  lgcStop1     <- ifelse(lgcReq1 == TRUE, FALSE, TRUE)
  infoLgcStop1 <- ifelse(lgcStop1 == TRUE,
                        'Argument "data" should be used for assigning a data set.',
                        "")
  lgcStop2     <- ifelse(TRUE %in% c(lgcReq2, lgcReq3, lgcReq4), FALSE, TRUE)
  infoLgcStop2 <- ifelse(lgcStop2 == TRUE, 'Arguments "n", "es", and "se" should be defined for the analysis based on study-level data.
                        Arguments "n1" and "n2" should be defined with "r1" and "r2" for dichotomous outcome based on arm-level data.
                        Or arguments "n1" and "n2" should be defined with "m1", "sd1", "m2", "sd2" for continuous outcome based on arm-level data.',
                        "")
  lgcStop3     <- ifelse(lgcReq5 == TRUE, FALSE, TRUE)
  infoLgcStop3 <- ifelse(lgcStop3 == FALSE,
                        'Argument "source", "time", "PES", and "PV" are required',
                        "")

  if (lgcStop1 | lgcStop2 | lgcStop3)
    stop(paste(ifelse(lgcStop1, paste(infoLgcStop1, "\n", "")),
               ifelse(lgcStop2, paste(infoLgcStop2, "\n", "")),
               ifelse(lgcStop3, paste(infoLgcStop3, "\n", "")),
               sep = "")
         )



  # 02. DEFINE core data -----
  dataIn <- data

  source <- deparse(substitute(source))
  time   <- deparse(substitute(time))
  colnames(dataIn)[which(colnames(dataIn) == source)] <- "source"
  colnames(dataIn)[which(colnames(dataIn) == time)]   <- "time"

  if (lgcReq2) {
    n      <- deparse(substitute(n))
    es     <- deparse(substitute(es))
    se     <- deparse(substitute(se))
    colnames(dataIn)[which(colnames(dataIn) == n)]  <- "n"
    colnames(dataIn)[which(colnames(dataIn) == es)] <- "es"
    colnames(dataIn)[which(colnames(dataIn) == se)] <- "se"
  }

  if (lgcReq3) {
    r1 <- deparse(substitute(r1))
    r2 <- deparse(substitute(r2))
    colnames(dataIn)[which(colnames(dataIn) == r1)] <- "r1"
    colnames(dataIn)[which(colnames(dataIn) == r2)] <- "r2"
    if (measure == "ES") {
      measure <- "RR"
    }
  }

  if (lgcReq4) {
    m1  <- deparse(substitute(m1))
    sd1 <- deparse(substitute(sd1))
    m2  <- deparse(substitute(m2))
    sd2 <- deparse(substitute(sd2))
    colnames(dataIn)[which(colnames(dataIn) == m1)]  <- "m1"
    colnames(dataIn)[which(colnames(dataIn) == sd1)] <- "sd1"
    colnames(dataIn)[which(colnames(dataIn) == m2)]  <- "m2"
    colnames(dataIn)[which(colnames(dataIn) == sd2)] <- "sd2"
    if (measure == "ES") {
      measure <- "MD"
    }
  }

  if (TRUE %in% c(lgcReq3, lgcReq4)) {
    n1 <- deparse(substitute(n1))
    n2 <- deparse(substitute(n2))
    colnames(dataIn)[which(colnames(dataIn) == n1)] <- "n1"
    colnames(dataIn)[which(colnames(dataIn) == n2)] <- "n2"
    dataIn$n <- dataIn$n1 + dataIn$n2
  }

  dataIn <- dataIn[order(dataIn$time), ]

  infoGroup    <- group
  infoRef      <- ref
  infoPrefer   <- prefer
  infoMeasure  <- measure
  infoModel    <- model
  infoMethod   <- ifelse(infoModel == "random", method, "DL")
  infoPooling  <- ifelse(pooling == "IV", "Inverse",
                         ifelse(infoMeasure %in% c("RR", "OR"),
                                pooling, "Inverse"))
  infoAlpha    <- alpha
  infoBeta     <- beta
  infoPES      <- PES
  infoPV       <- PV
  infoAdjust   <- adjust
  infoPlot     <- plot
  infoID       <- id
  infoInvert   <- invert
  infoBSB      <- BSB

  if (infoRef == 1) {
    infoGroup[c(1, 2)] <- infoGroup[c(2, 1)]
    infoPrefer <- ifelse(infoPrefer == "small", "large", "small")

    if (lgcReq3) {
      colnames(dataIn)[which(colnames(dataIn) == "r1")] <- "rGroup1"
      colnames(dataIn)[which(colnames(dataIn) == "r2")] <- "rGroup2"
      colnames(dataIn)[which(colnames(dataIn) == "rGroup1")] <- "r2"
      colnames(dataIn)[which(colnames(dataIn) == "rGroup2")] <- "r1"
    }

    if (lgcReq4) {
      colnames(dataIn)[which(colnames(dataIn) == "m1")]  <- "mGroup1"
      colnames(dataIn)[which(colnames(dataIn) == "sd1")] <- "sdGroup1"
      colnames(dataIn)[which(colnames(dataIn) == "m2")]  <- "mGroup2"
      colnames(dataIn)[which(colnames(dataIn) == "sd2")] <- "sdGroup2"
      colnames(dataIn)[which(colnames(dataIn) == "mGroup1")]  <- "m2"
      colnames(dataIn)[which(colnames(dataIn) == "sdGroup1")] <- "sd2"
      colnames(dataIn)[which(colnames(dataIn) == "mGroup2")]  <- "m1"
      colnames(dataIn)[which(colnames(dataIn) == "sdGroup2")] <- "sd1"
    }

    colnames(dataIn)[which(colnames(dataIn) == "n1")]  <- "nGroup1"
    colnames(dataIn)[which(colnames(dataIn) == "n2")]  <- "nGroup2"
    colnames(dataIn)[which(colnames(dataIn) == "nGroup1")]  <- "n2"
    colnames(dataIn)[which(colnames(dataIn) == "nGroup2")]  <- "n1"

  }




  # 03. PREPARE data before sequential analysis -----
  if (lgcReq2) {
    outMA <- meta::metagen(data = dataIn,
                           TE = es,
                           seTE = se,
                           studlab = source,
                           method.tau = infoMethod)
  }

  if (lgcReq3) {
    outMA <- meta::metabin(data = dataIn,
                           event.e = r1,
                           n.e = n1,
                           event.c = r2,
                           n.c = n2,
                           sm = infoMeasure,
                           studlab = source,
                           method = infoPooling,
                           method.tau = infoMethod)
  }

  if (lgcReq4) {
    outMA <- meta::metacont(data = dataIn,
                            mean.e = m1,
                            sd.e = sd1,
                            n.e = n1,
                            mean.c = m2,
                            sd.c = sd2,
                            n.c = n2,
                            sm = infoMeasure,
                            studlab = source,
                            method.tau = infoMethod)
  }

  outCMA <- meta::metacum(outMA, pooled = infoModel)

  infoNumStud  <- outCMA$k.study
  infoCases    <- sum(dataIn$n)

  if (lgcReq3) {
    if (infoModel == "random") {
      infoProp1 <- boot::inv.logit(meta::metaprop(event = r1, n = n1, method = "GLMM", data = dataIn)$TE.random)
      infoProp2 <- boot::inv.logit(meta::metaprop(event = r2, n = n2, method = "GLMM", data = dataIn)$TE.random)

      # Hajian-Tilaki K. Sample size estimation in epidemiologic studies. Caspian J Intern Med. 2011 Fall;2(4):289-98. PMID: 24551434; PMCID: PMC3895825.
      if (infoProp2 < 0.001) {
        if (infoProp2 < 0.000001) {
          infoProp2 <- 0.000001
        } else {
          infoProp1 <- (infoProp2 * exp(outMA$TE.random)) / (1 + infoProp2 * (exp(outMA$TE.random) - 1))
        }
      }

    } else {
      infoProp1 <- boot::inv.logit(meta::metaprop(event = r1, n = n1, method = "GLMM", data = dataIn)$TE.fixed)
      infoProp2 <- boot::inv.logit(meta::metaprop(event = r2, n = n2, method = "GLMM", data = dataIn)$TE.fixed)

      if (infoProp2 < 0.001) {
        if (infoProp2 < 0.000001) {
          infoProp2 <- 0.000001
        } else {
          infoProp1 <- (infoProp2 * exp(outMA$TE.fixed)) / (1 + infoProp2 * (exp(outMA$TE.fixed) - 1))
        }
      }
    }

    infoESMA  <- infoProp2 - infoProp1
    infoVarMA <- (infoProp2 + infoProp1) / 2 * (1 - (infoProp2 + infoProp1) / 2)
    #infoSEMA  <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
  } else if (isFALSE(lgcReq3)) {
    infoESMA  <- ifelse(infoModel == "random", outMA$TE.random, outMA$TE.fixed)
    infoSEMA  <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
    #infoVarMA <- ifelse(infoModel == "random", 1 / sum(outMA$w.random), 1 / sum(outMA$w.fixed))
    #infoVarMA    <- ifelse(infoModel == "random", 1 / sum(1 / (outMA$seTE + outMA$tau2)), 1 / sum(1 / outMA$seTE))
    infoVarMA  <- (infoSEMA * sqrt(infoCases - 2))^2
    #infoVarMA    <- infoSEMA^2
  }

  #infoESMA  <- ifelse(infoModel == "random", outMA$TE.random, outMA$TE.fixed)
  #infoSEMA  <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)
  #infoVarMA <- (infoSEMA * sqrt(infoCases - 2))^2
  infoPES   <- ifelse(infoPES == "post-hoc", infoESMA, infoPES)
  infoPV    <- ifelse("post-hoc" %in% c(infoPES, infoPV), infoVarMA, infoPV)
  infoI2    <- outMA$I2

  # 04. DO sequential analysis -----
  # alpha spending boundary (NCSS 710 Group-Sequential Analysis for Two Proportions 710-17)
  # https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Group-Sequential_Analysis_for_Two_Proportions.pdf
  # 04.01 Generate observed cumulative information

  if (infoModel == "random") {
    dataIn$weight <- outMA$w.random / sum(outMA$w.random)
  } else {
    dataIn$weight <- outMA$w.fixed / sum(outMA$w.fixed)
  }

  dataIn$esCum  <- outCMA$TE[c(1:infoNumStud)]
  dataIn$seCum  <- outCMA$seTE[c(1:infoNumStud)]
  dataIn$zCum   <- outCMA$statistic[c(1:infoNumStud)]

  ## 04.02 Calculate diversity (D-square, D2)
  infoDivers   <- (((outMA$seTE.random * sqrt(infoCases - 2))^2) - ((outMA$seTE.fixed * sqrt(infoCases - 2))^2)) / ((outMA$seTE.random * sqrt(infoCases - 2))^2)
    #infoDivers   <- 1 / sum(1 / (outMA$seTE)) / 1 / sum(1 / (outMA$seTE + outMA$tau2))
    #infoDivers   <- (((outMA$seTE.random * sqrt(infoCases))^2) - ((outMA$seTE.fixed * sqrt(infoCases))^2)) / ((outMA$seTE.random * sqrt(infoCases))^2)
    #1-(((MA.RE$seTE.fixed*sqrt(sum(DATA.TSA$n)-2))^2)/((MA.RE$seTE.random*sqrt(sum(DATA.TSA$n)-2))^2))
    #1 - (sum(outMA$w.random) / sum(outMA$w.fixed))
    #sum(((outMA$w.fixed^-1)+MA.RE$tau2)^-1)/sum(MA.RE$w.fixed)

  ## 04.03 Determinate adjustement factor
  if (infoAdjust == "D2") {
    infoAF <- 1 / (1 - infoDivers)
    #infoAF <- (outMA$seTE.random * sqrt(infoCases - 2))^2 / (outMA$seTE.fixed * sqrt(infoCases - 2))^2
  }

  if (infoAdjust == "I2") {
    infoAF <- 1 / (1 - infoI2)
  }

  if (infoAdjust == "CHL") {
    infoAF <- 1.33
  }

  if (infoAdjust == "CHM") {
    infoAF <- 2
  }

  if (infoAdjust == "CHH") {
    infoAF <- 4
  }

  ## 04.04 Calculate required information size (RIS)
  #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * (infoSEMA * sqrt(infoCases - 2))^2 / infoPES^2
  #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
  #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(1 - infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
  #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
  # X infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * (infoSEMA * sqrt(infoCases))^2 / (infoPES)
  #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) / (infoPES^2)
  # X infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * infoSEMA / infoPES^2
  #infoRIS      <- (1 / (1 - infoDivers)) * 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoSEMA * sqrt(infoCases))^2 / (infoPES^2))
  #infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * infoPV / (infoPES^2)
  infoRISOrg <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(1 - infoBeta)))^2) * infoPV / infoPES^2

  if (infoAdjust == "none") {
    infoRIS    <- infoRISOrg
  } else {
    infoRISAdj <- infoRISOrg * infoAF
    infoRIS    <- infoRISAdj
  }

  dataPlotSA <- as.data.frame(cbind(sample = c(ceiling(infoRIS/20):infoRIS),
                                    frag   = c(ceiling(infoRIS/20):infoRIS) / infoRIS)
                              )
  dataPlotSA$aslb <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotSA$frag))) / 2); dataPlotSA$aslb <- ifelse(dataPlotSA$aslb == "-Inf", -10, dataPlotSA$aslb)
  dataPlotSA$asub <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotSA$frag))) / 2); dataPlotSA$asub <- ifelse(dataPlotSA$asub == "Inf", 10, dataPlotSA$asub)
  dataPlotSA$bsub <-  (qnorm(pnorm(qnorm((infoBeta)) / dataPlotSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataPlotSA$bsub <- ifelse(dataPlotSA$bsub < 0, 0, dataPlotSA$bsub)
  dataPlotSA$bslb <- -(qnorm(pnorm(qnorm((infoBeta)) / dataPlotSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataPlotSA$bslb <- ifelse(dataPlotSA$bslb > 0, 0, dataPlotSA$bslb)
  infoFragBSB     <- dataPlotSA[max(which(dataPlotSA$bsub == 0)), "frag"]


  dataSA <- dataIn[, c("source", "time", "n", "weight", "esCum", "seCum", "zCum")]
  dataSA <- dataSA[order(dataSA$time), ]

  if (infoInvert) {
    dataSA$zCum <- dataSA$zCum * -1
    infoPrefer  <- ifelse(infoPrefer == "small", "large", "small")
  }

  dataSA$nCum  <- 0

  for (study.i in c(1:infoNumStud)) {
    if (study.i == 1) {
      dataSA[study.i, "nCum"] <- dataSA[study.i, "n"]
    } else {
      dataSA[study.i, "nCum"] <- dataSA[study.i, "n"] + dataSA[study.i - 1, "nCum"]
    }
  }

  dataSA$frag  <- dataSA$nCum / infoRIS
  dataSA$asub  <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataSA$frag))) / 2); dataSA$asub <- ifelse(dataSA$asub == "Inf", 10, dataSA$asub)
  dataSA$aslb  <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataSA$frag))) / 2); dataSA$aslb <- ifelse(dataSA$aslb == "-Inf", -10, dataSA$aslb)
  dataSA$bsub  <-  (qnorm(pnorm(qnorm((infoBeta)) / dataSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataSA$bsub <- ifelse(dataSA$bsub < 0, 0, dataSA$bsub)
  dataSA$bslb  <- -(qnorm(pnorm(qnorm((infoBeta)) / dataSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataSA$bslb <- ifelse(dataSA$bslb > 0, 0, dataSA$bslb)
  #dataSA$power <- 1 - pnorm(qnorm(1 - infoAlpha / 2) - dataSA$zCum) + pnorm(-qnorm(1 - infoAlpha/2) - dataSA$zCum); dataSA$power
  #dataSA$power<-1-pnorm(qnorm(1-alpha/2*dataSA$frag)-dataSA$zCum)+pnorm(-qnorm(1-alpha/2*dataSA$frag)-dataSA$zCum);dataSA$power
  #dataSA$power<-1-pnorm(qnorm(1-alpha/2)/sqrt(dataSA$frag)/2-dataSA$zCum)+pnorm(-qnorm(1-alpha/2)/sqrt(dataSA$frag)/2-dataSA$zCum);dataSA$power


  dataSA <- as.data.frame(dataSA)

  dataSA <- dataSA[order(dataSA$frag), ]

  infoColorASB <- ifelse(dataSA$nCum > infoRIS,
                            rgb(1, 1, 1, 1),
                            "gray25")
  infoPosLabel <- ifelse(dataSA$zCum > 0, 4, 2)

  if (max(dataSA$frag) < infoFragBSB) {
    dataSA[nrow(dataSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA, NA,
                                    round(infoRIS * infoFragBSB, 0), infoFragBSB,
                                    qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(infoFragBSB))) / 2),
                                    -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(infoFragBSB))) / 2),
                                    0, 0#,(1 - infoBeta) * infoFragBSB
                                    )
    }

  dataSA[nrow(dataSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA, NA,
                                  round(infoRIS, 0), 1,
                                  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2)
                                  #,1 - infoBeta
                                  )



  ## 04.05 Adjusted MA -----
  lgcMAAdj <- abs(dataSA$zCum[infoNumStud]) > abs(dataSA$asub[infoNumStud])
  infoLgcMAAdj  <- ifelse(lgcMAAdj,
                         "Adjusted confidence interval is suggested to be performed.",
                         "Adjusted confidence interval is not necessary to be performed.")
  infoPValMAAdj <- pnorm(ifelse(dataSA$asub[infoNumStud] > 5.3,
                                5.3,
                                dataSA$asub[infoNumStud]))

  if (lgcMAAdj) {
    if (lgcReq2) {
      outAdjMA <- meta::metagen(data = dataIn,
                                TE = es,
                                seTE = se,
                                studlab = source,
                                method.tau = infoMethod,
                                level.ma = infoPValMAAdj)
    }

    if (lgcReq3) {
      outAdjMA <- meta::metabin(data = dataIn,
                                event.e = r1,
                                n.e = n1,
                                event.c = r2,
                                n.c = n2,
                                sm = infoMeasure,
                                studlab = source,
                                method.tau = infoMethod,
                                method = infoPooling,
                                level.ma = infoPValMAAdj)
    }

    if (lgcReq4) {
      outAdjMA <- meta::metacont(data = dataIn,
                                 mean.e = m1,
                                 sd.e = sd1,
                                 n.e = n1,
                                 mean.c = m2,
                                 sd.c = sd2,
                                 n.c = n2,
                                 sm = infoMeasure,
                                 studlab = source,
                                 method.tau = infoMethod,
                                 level.ma = infoPValMAAdj)
    }

    if (infoModel == "random") {
      infoLCIMAAdj <- outAdjMA$lower.random
      infoUCIMAAdj <- outAdjMA$upper.random
    } else {
      infoLCIMAAdj <- outAdjMA$lower.fixed
      infoUCIMAAdj <- outAdjMA$upper.fixed
    }
  }



  # 05. BUILD an DoSA object -----
  lsDoSA <- list(name    = "Sequential analysis",
                 studies = infoNumStud,
                 AIS     = infoCases,
                 alpha   = infoAlpha,
                 beta    = infoBeta,
                 PES     = infoPES,
                 Pooing  = infoPooling,
                 Varian  = infoPV,
                 Divers  = infoDivers,
                 AF      = ifelse(infoAdjust == "none",
                                  "Undefined",
                                  infoAF),
                 RIS.org = ceiling(infoRISOrg),
                 RIS.adj = ifelse(infoAdjust == "none",
                                  "No adjusted",
                                  ceiling(infoRISAdj))
                 )
  class(lsDoSA)    <- c("aides", "DoSA")
  lsDoSA$frag      <- paste(round(dataSA$frag[infoNumStud], 4) * 100,
                            "%",
                            sep = "")
  lsDoSA$weight    <- paste(round(dataSA$weight[c(1:infoNumStud)], 4) * 100,
                            "%",
                            sep = "")
  lsDoSA$es.cum    <- round(dataSA$esCum[c(1:infoNumStud)], 3)
  lsDoSA$se.cum    <- round(dataSA$seCum[c(1:infoNumStud)], 3)
  lsDoSA$zval.cum  <- round(dataSA$zCum[c(1:infoNumStud)], 3)
  lsDoSA$asb       <- round(dataSA[c(1:infoNumStud), c("aslb", "asub")], 3)
  lsDoSA$aslb      <- round(dataSA$aslb[infoNumStud], 3)
  lsDoSA$asub      <- round(dataSA$asub[infoNumStud], 3)

  if (infoPlot == TRUE) {
    lsDoSA$data    <- dataSA
    }
  #lsDoSA$data.plot <- dataPlotSA



  # 06. RETURN summary of function `DoSA()` -----
  cat(paste("\n"), fill = TRUE, sep = "")
  cat(paste("Summary of sequential analysis (main information)\n",
            " Acquired sample size: ",
            infoCases,
            "\n Required sample size",
            ifelse(infoAdjust != "none",
                   " (heterogeneity adjusted): ",
                   " (without adjusted): "
                   ),
            ifelse(infoAdjust == "none",
                   ceiling(infoRISOrg),
                   ceiling(infoRISAdj)
                   ),
            "\n Cumulative z score: ",
            round(dataSA$zCum[infoNumStud], 3),
            "\n Alpha-spending boundary: ",
            round(dataSA$asub[infoNumStud], 3),
            " and ",
            round(dataSA$aslb[infoNumStud], 3),
            "\n ",
            infoLgcMAAdj,
            sep = ""),
      fill = TRUE, sep = "")

  if (lgcMAAdj) {
    cat(paste("\n",
              "Adjusted confidence interval based on type I error ",
              ifelse(dataSA$asub[infoNumStud] > 5.3,
                     "< 0.000001",
                     1 - pnorm(dataSA$asub[infoNumStud])
                     ),
              ": \n",
              round(infoLCIMAAdj, 3),
              " to ",
              round(infoUCIMAAdj, 3),
              sep = ""),
      fill = TRUE, sep = "")
  }

  cat(paste("\n",
            "Summary of sequential analysis (additional information)",
            "\n 1. Assumed information",
            "\n 1.1. Defined type I error: ",
            infoAlpha,
            "\n 1.2. Defined type II error: ",
            infoBeta,
            "\n 1.3. Defined power: ",
            1 - infoBeta,
            "\n 1.4. Presumed effect: ",
            round(infoPES, 3),
            ifelse(lgcReq3,
                   paste("\n      (risks in group 1 and 2 were ",
                         round(infoProp1, 10) * 100,
                         "%, and ",
                         round(infoProp2, 10) * 100,
                         "% respectively)",
                         sep = ""),
                   ""
                   ),
            "\n 1.5. Presumed variance: ",
            round(infoPV, 3),

            "\n",
            "\n 2. Meta-analysis",
            "\n 2.1. Setting of the meta-analysis",
            ifelse(infoMeasure %in% c("RR", "OR"),
                   paste("\n Data were pooled using ",
                         ifelse(infoPooling == "Inverse",
                                "inverse variance",
                                ifelse(infoPooling == "MH",
                                       "Mantel-Haensze",
                                       infoPooling
                                       )
                                ),
                         " approach in ",
                         ifelse(infoModel == "random",
                                paste("random-effects model with ",
                                      infoMethod, " method.",
                                      sep = ""),
                                "fixed-effect model."),
                         sep = ""),
                   paste("\n Data were pooled using inverse variance in ",
                         ifelse(infoModel == "random",
                                paste("random-effects model with ",
                                      infoMethod, " method.",
                                      sep = ""),
                                "fixed-effect model."),
                         sep = "")
                   ),

            "\n 2.2. Result of the meta-analysis \n ",
            paste(ifelse(infoMeasure %in% c("RR", "OR"),
                         paste("Log ", infoMeasure,
                               sep = ""),
                         infoMeasure
                         ),
                  ": ",
                  ifelse(infoModel == "random",
                         round(outMA$TE.random, 3),
                         round(outMA$TE.fixed, 3)
                         ),
                  " (95% CI: ",
                  ifelse(infoModel == "random",
                         round(outMA$lower.random, 3),
                         round(outMA$lower.fixed, 3)
                         ),
                  " to ",
                  ifelse(infoModel == "random",
                         round(outMA$upper.random, 3),
                         round(outMA$upper.fixed, 3)
                         ),
                  ")",
                  sep = ""),

            "\n ",
            "\n 3. Adjustment factor \n ",
            paste("The required information size is calculated ",
                  ifelse(infoAdjust == "none",
                         "without adjustment factor.",
                         paste("with adjustment factor based on ",
                               ifelse(infoAdjust == "D2",
                                      "diversity (D-squared).",
                                      ifelse(infoAdjust == "I2",
                                             "heterogeneity (I-squared).",
                                             ifelse(infoAdjust == "CHL",
                                                    "low conceptual heterogeneity (around 25%).",
                                                    ifelse(infoAdjust == "CHM",
                                                           "moderate conceptual heterogeneity (around 50%).",
                                                           ifelse(infoAdjust == "CHH",
                                                                  "high conceptual heterogeneity (around 75%).",
                                                                  "?")
                                                           )
                                                    )
                                             )
                                      ),
                               sep = "")
                         ),
                  " Relevant parameters are listed as follows.",
                  sep = ""),
            "\n 3.1. Heterogeneity (I-squared): ",
            paste(round(outMA$I2, 3) * 100, "%",
                  sep = ""),
            "\n 3.2. Diversity (D-squared): ",
            paste(round(infoDivers, 2) * 100, "%",
                  sep = ""),
            "\n 3.3. Adjustement factor: ",
            ifelse(infoAdjust == "none",
                   "Undefined",
                   round(infoAF, 3)),
            sep = ""),
      fill = TRUE, sep = "")



  # 07. ILLUSTRATE proportion of alpha-spending monitoring plot -----

  if (plot == TRUE) {

    plot(dataSA$nCum * 1.1, dataSA$asub,
         type = "l", frame = F,
         xlim = c(0, max(dataSA$nCum) * 1.2),
         ylim = c(ceiling(min(dataSA$aslb)) * (-10) / ceiling(min(dataSA$aslb)),
                ceiling(max(dataSA$asub)) * 10 / ceiling(max(dataSA$asub)) + 1),
         col = rgb(1, 1, 1, 0),
         xlab = "Sample size",
         yaxt = "n", #xaxt="n", "darkred"
         #ylab=paste("Favors", Txs[1], "   (Z score)   Favors",Txs[2]),
         ylab = "",
         main = "Sequential analysis")
    mtext(paste("(Note: the meta-analysis was conducted in ",
                ifelse(infoModel == "random",
                       paste("random-effects model based on ",
                             infoMethod,
                             " method)",
                             sep = ""),
                       "fixed-effect model)"
                       ),
                sep = ""),
          side = 1, line = 4, cex = 0.6)

    axis(side = 2, at = c(seq(ceiling(min(dataSA$aslb)) * (-10) / ceiling(min(dataSA$aslb)),
                          ceiling(max(dataSA$asub)) * 10 / ceiling(max(dataSA$asub)), 2)),
         padj = 0, hadj = 1, las = 1)
    mtext("Cumulative\n z score", side = 3, line = 0, at = -infoRIS * 0.05)
    mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[2], infoGroup[1])),
          side = 2, line = 2, at = 5,
          cex = ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1)) #(1/sqrt(seq(11,100,by=1)))^2*10
    mtext(paste("Favors\n", ifelse(infoPrefer == "small", infoGroup[1], infoGroup[2])),
          side = 2, line = 2, at = -5,
          cex = ifelse(max(nchar(infoGroup[2]), nchar(infoGroup[1])) > 10, (1 / sqrt(max(nchar(infoGroup[2]), nchar(infoGroup[1]))))^2 * 10, 1)) #(1/sqrt(seq(11,100,by=1)))^2*10
    #lines(dataSA$nCum,
    #      dataSA$asub,
    #      lwd = 1, col = "darkred", lty = 1)
    lines(dataPlotSA$sample,
          dataPlotSA$asub,
          lwd = 1, col = "darkred", lty = 1)
    points(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum,
           dataSA[which(!is.na(dataSA[, "source"])), ]$asub,
           col = infoColorASB, pch = 15, cex = 0.8)
    #lines(dataSA$nCum,
    #      dataSA$aslb,
    #      lwd = 1, col = "darkred", lty = 1)
    lines(dataPlotSA$sample,
          dataPlotSA$aslb,
          lwd = 1, col = "darkred", lty = 1)
    points(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum,
           dataSA[which(!is.na(dataSA[, "source"])), ]$aslb,
           col = infoColorASB, pch = 15, cex = 0.8)
    #lines(dataSA$nCum,
    #      dataSA$bsub,
    #      lwd = 1, col = "darkred", lty = 1)
    #lines(dataSA$nCum,
    #      dataSA$bslb,
    #      lwd = 1, col = "darkred", lty = 1)

    if (infoBSB) {
      lines(dataPlotSA$sample,
            dataPlotSA$bsub,
            lwd = 1, col = "darkred", lty = 1)
      lines(dataPlotSA$sample,
            dataPlotSA$bslb,
            lwd = 1, col = "darkred", lty = 1)
    }

    segments(c(0),
             c(-2, 0, 2),
             c(max(dataSA$nCum) * 1.1),
             c(-2, 0, 2),
             lty = c(2, 1, 2), lwd = 1, col = "gray25")
    lines(dataSA$nCum,
          dataSA$zCum,
          col = "blue3", lwd = 2)
    segments(c(0), c(0), dataSA[1, "nCum"], dataSA[1, "zCum"],
             lty = c(1), lwd = 2, col = 'blue3')
    points(dataSA$nCum,
           dataSA$zCum,
           col = "gray25", cex = 1 + dataSA$weight^2, pch = 15)

    arrows(max(dataSA$nCum), 0,
           max(dataSA$nCum) * 1.1, 0,
           lty = 1, length = 0.1)

    if (infoID) {
      points(dataSA$nCum,
             dataSA$zCum - dataSA$zCum,
             col = "gray25", cex = 0.6, pch = "|")
      text(dataSA$nCum,
           #ifelse(dataSA$zCum > 0,
           #      dataSA$zCum + 0.5,
           #      dataSA$zCum - 0.5),
           ifelse(dataSA$zCum > 0, -0.5, 0.5),
           dataSA$source,
           srt = 45,
           #pos = infoPosLabel,
           pos = 6 - infoPosLabel,
           cex = 0.8,
           col = c("gray20"))
    }

    #text(dataSA$time, dataSA$zCum - 0.5,
    #     c(round(dataSA$zCum, 2)),
    #     col = c("gray20"))

    rect(0, -10, infoRIS * 0.8, -7.5,
         lty = 0, col = rgb(1, 1, 1, 0.5))
    points(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum,
           dataSA[which(!is.na(dataSA[, "source"])), ]$aslb,
           col = infoColorASB, pch = 15, cex = 0.8)
    segments(c(0.05), c(-8), c(infoRIS / 20), c(-8),
             lty = c(1), lwd = 2, col = 'blue3')
    #text(0.1, -8.5,
          #paste("Observed z score; observed power:",
                #round(dataSA$power[length(dataSA$power) - 1], 2)),
          #pos = 4, cex = 0.8)
    text(infoRIS / 15, -8,
         paste("Cumulative z score", sep = ""),
         pos = 4, cex = 0.8)
    segments(c(0.05), c(-9), c(infoRIS / 20), c(-9),
             lty = c(1), lwd = 1.5, col = "darkred")
    text(infoRIS / 15, -9,
         paste("Parameters for alpha-spending boundary:"),
         pos = 4, cex = 0.8)
    text(infoRIS / 15, -9.7,
         paste(ifelse(infoMeasure %in% c("MD", "SMD"),
                      infoMeasure,
                      "Assumed effect: "
                      ),
               " = ",
               round(infoPES, 3),
               "; assumed power: ", round(1 - infoBeta, 2),
               "; assumed alpha: ", infoAlpha, sep = ""
               ),
         pos = 4, cex = 0.8)
    segments(c(infoRIS), c(-9), c(infoRIS), c(9),
             lty = c(2), col = "darkred")
    text(infoRIS, 10,
         paste("Required sample size:", ceiling(infoRIS)),
         pos = ifelse(infoRIS > infoCases, 2, 4),
         cex = 0.8)
    text(infoRIS, 9,
         paste("Acquired sample size:",
               ceiling(max(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum))),
         pos = ifelse(infoRIS > infoCases, 2, 4),
         cex = 0.8)
  }

  output <- lsDoSA

}
