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
#' @param prefer  CHARACTER of "small" and "large" for indicating which direction
#'                is beneficial effect in statistic test.
#' @param measure CHARACTER for indicating which statistic measure should be used.
#' @param model   CHARACTER of "random" and "fixed" for indicating whether
#'                to use random-effects model or fixed-effect model.
#' @param method  CHARACTER for indicating which estimator should be used in
#'                random-effects model.
#' @param alpha   NUMERIC value between 0 to 1 for indicating the assumed type I
#'                error.
#' @param beta    NUMERIC value between 0 to 1 for indicating the assumed type II
#'                error.
#' @param AES     NUMERIC value for indicating the assumed meaningful effect size.
#' @param plot    LOGIC value for indicating whether to illustrate proportion of
#'                excessive cases plot.
#'
#'
#' @return
#' **DoSA()** returns a summary on the result of sequential analysis.
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
#'                 measure = "SMD", AES = 0.5,
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
                 prefer = "small",
                 measure = "ES",
                 model = "random",
                 method = "DL",
                 alpha = 0.05,
                 beta = 0.2,
                 AES = NULL,
                 plot  = FALSE) {

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
  lgcInAES    <- ifelse(is.null(substitute(AES)), FALSE, TRUE)

  lgcReq1     <- ifelse(lgcInData == TRUE, TRUE, FALSE)
  lgcReq2     <- ifelse(FALSE %in% c(lgcInN, lgcInES, lgcInSE), FALSE, TRUE)
  lgcReq3     <- ifelse(FALSE %in% c(lgcInR1, lgcInN1, lgcInR2, lgcInN2), FALSE, TRUE)
  lgcReq4     <- ifelse(FALSE %in% c(lgcInM1, lgcInSD1, lgcInN1, lgcInM2, lgcInSD2, lgcInN2), FALSE, TRUE)
  lgcReq5     <- ifelse(FALSE %in% c(lgcInSource, lgcInTime, lgcInAES), FALSE, TRUE)

  lgcStop1     <- ifelse(lgcReq1 == TRUE, FALSE, TRUE)
  lgcStop1Info <- ifelse(lgcStop1 == TRUE,
                        'Argument "data" should be used for assigning a data set.',
                        "")
  lgcStop2     <- ifelse(TRUE %in% c(lgcReq2, lgcReq3, lgcReq4), FALSE, TRUE)
  lgcStop2Info <- ifelse(lgcStop2 == TRUE, 'Arguments "n", "es", and "se" should be defined for the analysis based on study-level data.
                        Arguments "n1" and "n2" should be defined with "r1" and "r2" for dichotomous outcome based on arm-level data.
                        Or arguments "n1" and "n2" should be defined with "m1", "sd1", "m2", "sd2" for continuous outcome based on arm-level data.',
                        "")
  lgcStop3     <- ifelse(lgcReq5 == TRUE, FALSE, TRUE)
  lgcStop3Info <- ifelse(lgcStop3 == FALSE,
                        'Argument "source", "time", and "AES" are required',
                        "")

  if (lgcStop1 | lgcStop2 | lgcStop3)
    stop(paste(ifelse(lgcStop1, paste(lgcStop1Info, "\n", "")),
               ifelse(lgcStop2, paste(lgcStop2Info, "\n", "")),
               ifelse(lgcStop3, paste(lgcStop3Info, "\n", "")),
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
  infoPrefer   <- prefer
  infoMeasure  <- measure
  infoModel    <- model
  infoMethod   <- method
  infoAlpha    <- alpha
  infoBeta     <- beta
  infoAES      <- AES
  infoPlot     <- plot



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
  infoESMA     <- ifelse(infoModel == "random", outMA$seTE.random, outMA$seTE.fixed)

  dataIn$esCum <- outCMA$TE[c(1:infoNumStud)]
  dataIn$seCum <- outCMA$seTE[c(1:infoNumStud)]
  dataIn$zCum  <- outCMA$statistic[c(1:infoNumStud)]

  infoRIS      <- 4 * ((qnorm(1 - (infoAlpha / 2)) + abs(qnorm(infoBeta)))^2) * ((infoESMA * sqrt(infoCases))^2 / (infoAES^2))

  dataSA <- dataIn[, c("source", "time", "n", "esCum", "seCum", "zCum")]
  dataSA <- dataSA[order(dataSA$time), ]

  dataSA$nCum  <- 0

  for (study.i in c(1:infoNumStud)) {
    if (study.i == 1) {
      dataSA[study.i, "nCum"] <- dataSA[study.i, "n"]
    } else {
      dataSA[study.i, "nCum"] <- dataSA[study.i, "n"] + dataSA[study.i - 1, "nCum"]
    }
  }

  dataSA$frag <- dataSA$nCum / infoRIS



  # 04. DO sequential analysis
  # alpha spending boundary (NCSS 710 Group-Sequential Analysis for Two Proportions 710-17)
  # https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Group-Sequential_Analysis_for_Two_Proportions.pdf
  dataSA$asub <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataSA$frag))) / 2); dataSA$asub <- ifelse(dataSA$asub == "Inf", 10, dataSA$asub)
  dataSA$aslb <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataSA$frag))) / 2); dataSA$aslb <- ifelse(dataSA$aslb == "-Inf", -10, dataSA$aslb)
  dataSA$bsub <-  (qnorm(pnorm(qnorm((infoBeta)) / dataSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataSA$bsub <- ifelse(dataSA$bsub < 0, 0, dataSA$bsub)
  dataSA$bslb <- -(qnorm(pnorm(qnorm((infoBeta)) / dataSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataSA$bslb <- ifelse(dataSA$bslb > 0, 0, dataSA$bslb)
  dataSA$power<- 1 - pnorm(qnorm(1 - infoAlpha / 2) - dataSA$zCum) + pnorm(-qnorm(1 - infoAlpha/2) - dataSA$zCum); dataSA$power
  #dataSA$power<-1-pnorm(qnorm(1-alpha/2*dataSA$frag)-dataSA$zCum)+pnorm(-qnorm(1-alpha/2*dataSA$frag)-dataSA$zCum);dataSA$power
  #dataSA$power<-1-pnorm(qnorm(1-alpha/2)/sqrt(dataSA$frag)/2-dataSA$zCum)+pnorm(-qnorm(1-alpha/2)/sqrt(dataSA$frag)/2-dataSA$zCum);dataSA$power

  dataSA <- as.data.frame(dataSA)

  if (max(dataSA$frag) < 0.08) {
    dataSA[nrow(dataSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA,
                                    round(infoRIS*0.08, 0), 0.08,
                                    qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(0.08))) / 2),
                                    -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(0.08))) / 2),
                                    0, 0, (1 - infoBeta) * 0.08)
    }

  dataSA[nrow(dataSA) + 1, ] <- c(NA, NA, NA, NA, NA, NA,
                                  round(infoRIS, 0), 1,
                                  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) / 2),
                                  1 - infoBeta)

  dataSA <- dataSA[order(dataSA$frag), ]

  dataPlotSA <- as.data.frame(cbind(sample = c(ceiling(infoRIS/20):infoRIS),
                                    frag   = c(ceiling(infoRIS/20):infoRIS) / infoRIS)
                              )
  dataPlotSA$aslb <- -qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotSA$frag))) / 2); dataPlotSA$aslb <- ifelse(dataPlotSA$aslb == "-Inf", -10, dataPlotSA$aslb)
  dataPlotSA$asub <-  qnorm(1 - (2 - 2 * pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(dataPlotSA$frag))) / 2); dataPlotSA$asub <- ifelse(dataPlotSA$asub == "Inf", 10, dataPlotSA$asub)
  dataPlotSA$bsub <-  (qnorm(pnorm(qnorm((infoBeta)) / dataPlotSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataPlotSA$bsub <- ifelse(dataPlotSA$bsub < 0, 0, dataPlotSA$bsub)
  dataPlotSA$bslb <- -(qnorm(pnorm(qnorm((infoBeta)) / dataPlotSA$frag)) + (qnorm(pnorm(qnorm((1 - infoAlpha / 2)) / sqrt(1))) - qnorm(pnorm(qnorm((infoBeta)) / sqrt(1))))); dataPlotSA$bslb <- ifelse(dataPlotSA$bslb > 0, 0, dataPlotSA$bslb)



  # 05. BUILD an DoSA object -----
  lsDoSA <- list(name    = "Sequential analysis",
                   studies = infoNumStud,
                   AIS     = infoCases,
                   alpha   = infoAlpha,
                   beta    = infoBeta,
                   AES     = infoAES,
                   RIS     = ceiling(infoRIS)
                   )
  class(lsDoSA)    <- "DoSA"
  lsDoSA$frag      <- dataSA$frag
  lsDoSA$es.cum    <- dataSA$esCum
  lsDoSA$se.cum    <- dataSA$seCum
  lsDoSA$zval.cum  <- round(dataSA$zCum[-c(length(dataSA$zCum))], 3)
  lsDoSA$asb       <- round(dataSA[, c("aslb", "asub")], 3)
  lsDoSA$aslb      <- round(dataSA$aslb[infoNumStud], 3)
  lsDoSA$asub      <- round(dataSA$asub[infoNumStud], 3)
  lsDoSA$data      <- dataSA
  #lsDoSA$data.plot <- dataPlotSA



  # 06. RETURN summary of function `DoSA()` -----
  cat(paste("\n"), fill = TRUE, sep = "")
  cat(paste("Summary of sequential analysis\n",
            " Acquired sample size: ",
            infoCases,
            "\n Required sample size: ",
            ceiling(infoRIS),
            "\n Cumulative z score: ",
            round(dataSA$zCum[infoNumStud], 3),
            "\n Alpha-spending boundary: ",
            round(dataSA$asub[infoNumStud], 3),
            " and ",
            round(dataSA$aslb[infoNumStud], 3),
            sep = ""),
      fill = TRUE, sep = "")



  # 07. ILLUSTRATE proportion of excessive cases plot -----

  if (plot == TRUE) {

    plot(dataSA$nCum, dataSA$asub,
         type = "l", frame = F,
         xlim = c(0, infoRIS * 1.2),
         ylim = c(ceiling(min(dataSA$aslb)) * (-10) / ceiling(min(dataSA$aslb)),
                ceiling(max(dataSA$asub)) * 10 / ceiling(max(dataSA$asub)) + 1),
         col = rgb(1, 1, 1, 0),
         xlab = "Sample size",
         yaxt = "n", #xaxt="n", "darkred"
         #ylab=paste("Favors", Txs[1], "   (Z score)   Favors",Txs[2]),
         ylab = "",
         main = "Sequential analysis")

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
           col = "gray25", pch = 15, cex = 0.8)
    #lines(dataSA$nCum,
    #      dataSA$aslb,
    #      lwd = 1, col = "darkred", lty = 1)
    lines(dataPlotSA$sample,
          dataPlotSA$aslb,
          lwd = 1, col = "darkred", lty = 1)
    points(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum,
           dataSA[which(!is.na(dataSA[, "source"])), ]$aslb,
           col = "gray25", pch = 15, cex = 0.8)
    lines(dataSA$nCum,
          dataSA$bsub,
          lwd = 1, col = "darkred", lty = 1)
    lines(dataSA$nCum,
          dataSA$bslb,
          lwd = 1, col = "darkred", lty = 1)
    segments(c(0),
             c(-2, 0, 2),
             c(max(dataSA$nCum)),
             c(-2, 0, 2),
             lty = c(2, 1, 2), lwd = 1, col = "gray25")
    lines(dataSA$nCum,
          dataSA$zCum,
          col = "blue3", lwd = 2)
    points(dataSA$nCum,
           dataSA$zCum,
           col = "gray25", cex = 1, pch = 15)

    #text(dataSA$time, dataSA$zCum - 0.5,
    #     c(round(dataSA$zCum, 2)),
    #     col = c("gray20"))

    rect(0, -10, infoRIS * 0.8, -7.5,
         lty = 0, col = rgb(1, 1, 1, 0.5))
    points(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum,
           dataSA[which(!is.na(dataSA[, "source"])), ]$aslb,
           col = "gray25", pch = 15, cex = 0.8)
    segments(c(0.05), c(-8), c(infoRIS / 20), c(-8),
             lty = c(1), lwd = 2, col = 'blue4')
    #text(0.1, -8.5,
          #paste("Observed z score; observed power:",
                #round(dataSA$power[length(dataSA$power) - 1], 2)),
          #pos = 4, cex = 0.8)
    text(infoRIS / 15, -8,
         paste("Observed z score", sep = ""),
         pos = 4, cex = 0.8)
    segments(c(0.05), c(-9), c(infoRIS / 20), c(-9),
             lty = c(1), lwd = 1.5, col = "darkred")
    text(infoRIS / 15, -9,
         paste("Parameters for alpha-spending boundary:"),
         pos = 4, cex = 0.8)
    text(infoRIS / 15, -9.7,
         paste(infoMeasure, " = ", infoAES,
               "; assumed power: ", round(1 - infoBeta, 2),
               "; assumed alpha: ", infoAlpha, sep = ""),
         pos = 4, cex = 0.8)
    segments(c(infoRIS), c(-9), c(infoRIS), c(9),
             lty = c(2), col = "gray")
    text(infoRIS, 10,
         paste("Required sample size:", ceiling(infoRIS)),
         pos = 2, cex = 0.8)
    text(infoRIS, 9,
         paste("Acquired sample size:",
               ceiling(max(dataSA[which(!is.na(dataSA[, "source"])), ]$nCum))),
         pos = 2, cex = 0.8)
  }

  output <- lsDoSA

}
