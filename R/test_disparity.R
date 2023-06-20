#' @title Test assumption of disparities in sample size.
#'
#' @author Enoch Kang
#'
#' @description
#' **TestDisparity()** is a function for disparities in sample size analysis.
#'
#' @param n     NUMERIC values for sample size (n) of each study.
#' @param study STRINGS for study label of each study.
#' @param coval NUMERIC value of cutting-off value for variability that should be
#'              smaller than zero.
#' @param plot  LOGIC value for indicating whether to illustrate proportion of
#'              excessive cases plot.
#' @param color STRING of a color name for emphasizing the significant
#'              disparities in sample size.
#'
#'
#' @return
#' **TestDisparity()** returns a summary of result regarding disparities in sample size.
#'
#'
#' @references
#' Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for
#' normality (complete samples). **Biometrika**, *52(3)*, 591-611.
#'
#' Hendricks, W. A., & Robey, K. W. (1936). The sampling distribution of the
#' coefficient of variation. **The Annals of Mathematical Statistics**, *7(3)*,
#' 129-132.
#'
#' Sokal, R. R., & Braumann, C. A. (1980). Significance tests for coefficients
#' of variation and variability profiles. **Systematic Biology**, *29(1)*, 50-66.
#'
#'
#' @seealso \code{\link{TestDiscordance}}
#'
#' @examples
#' ## Not run:
#' # 1. Import a dataset of study by Fleiss (1993)
#' library(meta)
#' data("Fleiss1993bin")
#' data <- Fleiss1993bin
#'
#' # 2. Calculate total sample size and standard error of each study
#' data$n  <- data$n.asp + data$n.plac
#' data$se <- sqrt((1 / data$d.asp) - (1 / data$n.asp) + (1 / data$d.plac) - (1 / data$n.plac))
#'
#' # 3. Test disparities in sample sizes
#' output <- TestDisparity(data$n, data$study)
#'
#' # 4. Illustrate disparity plot
#' TestDisparity(data$n, data$study, plot = TRUE)
#'
#' ## End(Not run)
#'
#' @export TestDisparity



TestDisparity <- function(n,
                    study = NULL,
                    coval = 0.10,
                    plot  = FALSE,
                    color = "firebrick3") {

  # 01. DEFINE core data -----
  dataCases <- n

  # 02. CHECK arguments -----
  lgcN      <- ifelse(is.null(dataCases),
                      TRUE,
                      ifelse(isFALSE(is.numeric(dataCases)),
                             TRUE,
                             ifelse(isFALSE(min(dataCases) > 0),
                                    TRUE,
                                    ifelse("FALSE" %in% names(table(dataCases %% 1 == 0)),
                                    TRUE, FALSE)
                                    )
                             )
                      )

  lgcStudy  <- ifelse(is.null(study), FALSE,
                      ifelse(length(study) == length(dataCases),
                             FALSE, TRUE)
                      )

  lgcCOVal  <- ifelse(is.null(coval), FALSE,
                      ifelse(is.numeric(coval),
                             ifelse((coval >= 0) == TRUE,
                                    FALSE, TRUE),
                             TRUE)
                      )

  lgcPlot   <- ifelse(is.null(plot), FALSE,
                      ifelse(is.logical(plot), FALSE, TRUE))

  lgcColor  <- ifelse(is.null(color), FALSE,
                      ifelse(length(color) == 1,
                             ifelse(color %in% colors(), FALSE, TRUE),
                             TRUE))

  if (lgcN) {
    infoStopN <- 'Argument "n" must be a integer vector for sample size of each study.'
    }

  if (lgcStudy) {
    infoStopStudy <- 'Argument "study" must be a vector for study label of each study, and length of the vector should be the same with length of the vector for sample size.'
    }

  if (lgcCOVal) {
    infoStopCOVal <- 'Argument "coval" must be a numeric value between 0 and 1 in order to determining a cutting-off value for variability.'
    }

  if (lgcPlot) {
    infoStopPlot  <- 'Argument "plot" must be a logical value in terms of "TRUE" or "FALSE" for indicating whether to illustrate disparity plot.'
    }

  if (lgcColor) {
    infoStopColor <- 'Argument "color" must be a color name in R for emphasizing the significant disparities in sample size.'
    }


  # 03. RETURN results of argument checking  -----
  if (lgcN | lgcStudy | lgcCOVal | lgcPlot | lgcColor)
    stop(paste(ifelse(lgcN, paste(infoStopN, "\n", "")),
               ifelse(lgcStudy, paste(infoStopStudy, "\n", "")),
               ifelse(lgcCOVal, paste(infoStopCOVal, "\n", "")),
               ifelse(lgcPlot, paste(infoStopPlot, "\n", "")),
               ifelse(lgcColor, paste(infoStopColor, "\n", "")),
               sep = "")
         )


  # 04. ANALYZE
  ## 04.01. PREPARE data for testing disparities in sample size -----
  infoNumStud  <- length(dataCases)
  infoAlpha    <- 0.05
  infoMCases   <- mean(dataCases)
  infoSDCases  <- sd(dataCases)

  if (isFALSE(is.null(study))) {
    dataStudy <- study
  }

  ## 04.02. TEST normality -----
  outptNorm    <- shapiro.test(dataCases)

  ## 04.03. TEST variability -----
  infoOrgnCV   <- infoSDCases / infoMCases  # original coefficient of variance
  infoUnbsCV   <- infoOrgnCV * (1 + 1 / (4 * infoNumStud))  # unbiased coefficient of variance
  infoSECV     <- infoUnbsCV / sqrt(2 * infoNumStud)
  infoDiffCV   <- infoOrgnCV - coval
  infoStatsT   <- infoDiffCV / infoSECV
  infoDFCV     <- infoNumStud - 1
  infoCrtTCV   <- qt(infoAlpha, infoNumStud)
  infoPValCV   <- pt(infoStatsT,
                     df = infoDFCV,
                     lower.tail = FALSE)
  infoLCICV    <- infoDiffCV - infoSECV * infoCrtTCV
  infoUCICV    <- infoDiffCV + infoSECV * infoCrtTCV
  infoCutCases <- c(floor(infoMCases * (1 - coval)), ceiling(infoMCases * (1 + coval)))


  # 05. BUILD a data frame of the disparity -----
  dataCasesPlot <- as.data.frame(cbind(seq   = c(1:length(dataCases)),
                                       obs   = dataCases,
                                       dsprt = dataCases - mean(dataCases),
                                       # Number of excessive cases
                                       excN  = ifelse(dataCases < infoCutCases[1],
                                                      dataCases - infoCutCases[1],
                                                      ifelse(dataCases > infoCutCases[2],
                                                             dataCases - infoCutCases[2],
                                                             0)))
                                 )

  dataCasesPlot$seq   <- as.numeric(dataCasesPlot$seq)
  dataCasesPlot$obs   <- as.numeric(dataCasesPlot$obs)
  dataCasesPlot$dsprt <- as.numeric(dataCasesPlot$dsprt)
  dataCasesPlot$excN  <- as.numeric(dataCasesPlot$excN)

  ### Proportion of excessive cases
  dataCasesPlot$excP  <- ifelse(dataCasesPlot$excN == 0,
                                0,
                                dataCasesPlot$excN / sum(abs(dataCasesPlot$excN)))
  dataCasesPlot$excP  <- as.numeric(dataCasesPlot$excP)


  if (is.null(study)) {
    for (i.study in c(1:infoNumStud)) {
      dataCasesPlot[i.study, "study"] <- paste("Study ",
                                               dataCasesPlot[i.study, "seq"],
                                               sep = "")
    }
  } else {
    dataCasesPlot$study <- dataStudy
  }

  ### Position of study labels
  dataCasesPlot$pos   <- ifelse(dataCasesPlot$excP < 0, 3, 1)
  dataCasesPlot$color <- ifelse(dataCasesPlot$excP == 0,
                                "skyblue", color)
  dataCasesPlot <- dataCasesPlot[, c("seq", "study", "pos", "obs", "dsprt", "excN", "excP", "color")]


  # 06. BUILD an disparity object -----
  dataDiSS <- list(name      = "Disparities in sample size test",
                   disparity = ifelse(outptNorm$p.value < 0.05,
                                      "Suspected",
                                      ifelse(infoPValCV < 0.05,
                                             "Suspected",
                                             "Unsuspected")))
  class(dataDiSS)      <- "disparity"
  dataDiSS$w.normality <- outptNorm$statistic
  dataDiSS$p.normality <- outptNorm$p.value
  dataDiSS$cv          <- infoOrgnCV
  dataDiSS$diff.cv     <- infoDiffCV
  dataDiSS$se.cv       <- infoSECV
  dataDiSS$crt.t.cv    <- infoCrtTCV
  dataDiSS$t.cv        <- ifelse(outptNorm$p.value >= 0.05,
                                 infoStatsT, NA)
  dataDiSS$p.cv        <- ifelse(outptNorm$p.value >= 0.05,
                                 infoPValCV, NA)
  dataDiSS$lci.cv      <- infoLCICV
  dataDiSS$uci.cv      <- infoUCICV
  dataDiSS$cov.cases   <- infoCutCases
  dataDiSS$exc.ttl     <- sum(abs(dataCasesPlot$excN))
  dataDiSS$exc.prop    <- dataCasesPlot$excP
  #dataDiSS$data.plot   <- dataCasesPlot
  dataDiSS$sample.size <- dataCases



  # 07. RETURN summary of function `TestDisparity()` -----
  cat(paste("\n"), fill = TRUE, sep = "")
  cat(paste("Summary of disparities in sample size test:\n",
            " Statistics (",
            ifelse(outptNorm$p.value < 0.05, "W", "t"),
            "): ",
            ifelse(outptNorm$p.value < 0.05,
                   round(outptNorm$statistic, 3),
                   round(infoStatsT, 3)),
            "\n P-value: ",
            ifelse(outptNorm$p.value < 0.05,
                   ifelse(outptNorm$p.value < 0.001,
                          "< 0.001",
                          round(outptNorm$p.value, 3)),
                   ifelse(dataDiSS$p.cv < 0.001,
                          "< 0.001",
                          round(dataDiSS$p.cv, 3))
                   ),
            "\n Note: ",
            ifelse(outptNorm$p.value < 0.05,
                   "Suspected disparities in sample size due to non-normality",
                   ifelse(dataDiSS$p.cv < 0.05,
                          paste("Suspected disparities in sample size due to large variability",
                                " that is based on cutting-off value ",
                                coval * 100, "% variability.",
                                sep = ""),
                   "No significant finding in the stepwised tests of disparities in sample size.")),
            sep = ""),
      fill = TRUE, sep = "")



  # 08. ILLUSTRATE proportion of excessive cases plot -----

  if (plot == TRUE) {

    dataCasesPlot <- dataCasesPlot[order(dataCasesPlot$excN, decreasing = FALSE), ]
    dataCasesPlot$seq <- c(1:nrow(dataCasesPlot))

    plot(dataCasesPlot$seq,
         dataCasesPlot$excP,
         type = "n", frame = FALSE,
         xaxt = "n", yaxt = "n",
         ylim = c(ifelse(min(dataCasesPlot$excP) > -0.5,
                         -0.5,
                         ifelse(min(dataCasesPlot$excP) > -1,
                                -1,
                                min(dataCasesPlot$excP))
                         ),
                  ifelse(max(dataCasesPlot$excP) < 0.5,
                         0.5,
                         ifelse(max(dataCasesPlot$excP) < 1,
                                1,
                                max(dataCasesPlot$excP))
                         )
                  ),
         xlab = "", ylab = "")
    segments(0, 0,
             nrow(dataCasesPlot), 0,
             col = "blue4")
    segments(dataCasesPlot$seq, 0,
             dataCasesPlot$seq, dataCasesPlot$excP,
             col = "gray")
    points(dataCasesPlot$seq,
           dataCasesPlot$excP,
           pch = 21, bg = dataCasesPlot$color, col = "gray")
    text(1,
         par("usr")[4] * 0.6,
         paste("Summary of disparities in sample size test:\n",
               " Statistics (",
               ifelse(outptNorm$p.value < 0.05, "W", "t"),
               "): ",
               ifelse(outptNorm$p.value < 0.05,
                      round(outptNorm$statistic, 3),
                      round(infoStatsT, 3)),
               "\n P-value: ",
               ifelse(outptNorm$p.value < 0.05,
                      ifelse(outptNorm$p.value < 0.001,
                             "< 0.001",
                             round(outptNorm$p.value, 3)),
                      paste(ifelse(dataDiSS$p.cv < 0.001,
                                   "< 0.001",
                                   round(dataDiSS$p.cv, 3)),
                            " (one-tailed test)",
                            sep = "")
                      ),
               sep = ""),
         pos = 4, cex = 1.2)
    axis(2, las = 2)
    text(dataCasesPlot$seq,
         par("usr")[3],
         dataCasesPlot$study,
         cex = ifelse(infoNumStud < 11, 1,
                      1 / sqrt(infoNumStud / 10)),
         xpd = TRUE, pos = 1, srt = 45)
    mtext("Disparity plot \n (Proportion of excessive cases)", side = 3, cex = 1.5)
    mtext("Observed study", side = 1, cex = 1.2, line = 4)
    mtext("Disparity", side = 2, cex = 1.2, line = 3)

  }

  output <- dataDiSS

}
