% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_observed_sequential_analysis.R
\name{PlotOSA}
\alias{PlotOSA}
\title{Illustrate observed sequential plot.}
\usage{
PlotOSA(
  object = NULL,
  sclAxsX = "sample",
  txtTtl = NULL,
  group = NULL,
  lgcZone = FALSE,
  lgcLblStdy = FALSE,
  lgcSAP = FALSE,
  lgcInvert = FALSE,
  lgcSmooth = FALSE,
  szFntTtl = 1.8,
  szFntTtlX = 1.2,
  szFntTtlY = NULL,
  szFntAxsX = 0.8,
  szFntAxsY = 0.8,
  szFntLgnd = 0.7,
  szFntLblY = 1.2,
  szFntStdy = 0.8,
  szFntOIS = 0.8,
  szFntAIS = 0.8,
  szPntStdy = 1,
  szPntASB = 0.8,
  szLn0 = 1,
  szLnSig = 1,
  szLnZCum = 2,
  szLnASB = 1,
  szLnOIS = 1,
  typPntStdy = NULL,
  typPntASB = NULL,
  typLn0 = 1,
  typLnSig = 2,
  typLnZCum = 1,
  typLnASB = 3,
  typLnOIS = 2,
  clrTtl = "black",
  clrTtlX = "black",
  clrTtlY = "black",
  clrAxsX = "black",
  clrAxsY = "black",
  clrLgnd = "black",
  clrLblY = "black",
  clrLblStdy = "black",
  clrLblOIS = "black",
  clrLblAIS = "black",
  clrPntStdy = "gray25",
  clrPntASB = "none",
  clrLn0 = "gray25",
  clrLnSig = "gray",
  clrLnZCum = "blue4",
  clrLnASB = "red4",
  clrLnOIS = "red4",
  anglStdy = 30,
  BSB = FALSE
)
}
\arguments{
\item{object}{OBJECT in \strong{DoOSA} class that is an output of observed
sequential analysis using function \code{DoOSA()}.}

\item{sclAxsX}{CHARACTER for indicating unit of scale on axis X.}

\item{txtTtl}{CHARACTER for user-defined main title on the observed sequential
analysis plot.}

\item{group}{CHARACTER for labeling two groups.}

\item{lgcZone}{LOGIC value for indicating whether to show zones.}

\item{lgcLblStdy}{LOGIC value for indicating whether to label each data source.}

\item{lgcSAP}{LOGIC value for indicating whether to show sequential-adjusted
power.}

\item{lgcInvert}{LOGIC value for indicating whether to invert plot.}

\item{lgcSmooth}{LOGIC value for indicating whether to smooth error boundaries.}

\item{szFntTtl}{NUMERIC value for indicating font size of main title.}

\item{szFntTtlX}{NUMERIC value for indicating font size of title on axis X.}

\item{szFntTtlY}{NUMERIC value for indicating font size of title on axis Y.}

\item{szFntAxsX}{NUMERIC value for indicating font size of scale on axis X.}

\item{szFntAxsY}{NUMERIC value for indicating font size of scale on axis Y.}

\item{szFntLgnd}{NUMERIC value for indicating font size of legend.}

\item{szFntLblY}{NUMERIC value for indicating font size of the label of
"Cumulative z-score" on axis Y.}

\item{szFntStdy}{NUMERIC value(s) for indicating font size(s) of the label(s)
of each data source.}

\item{szFntOIS}{NUMERIC value for indicating font size of the label of optimal
information size.}

\item{szFntAIS}{NUMERIC value for indicating font size of the label of acquired
information size.}

\item{szPntStdy}{NUMERIC value(s) for indicating size(s) of observed point(s).}

\item{szPntASB}{NUMERIC value for indicating size of point(s) on alpha-spending
boundaries.}

\item{szLn0}{NUMERIC value for indicating width of null line.}

\item{szLnSig}{NUMERIC value for indicating width of line for statistical
significance.}

\item{szLnZCum}{NUMERIC value for indicating width of line for cumulative
z-score.}

\item{szLnASB}{NUMERIC value for indicating width of line for alpha-spending
boundaries.}

\item{szLnOIS}{NUMERIC value for indicating width of line for optimal
information size.}

\item{typPntStdy}{NUMERIC value(s) between 1 to 5 for indicating type(s) of
observed point(s). Symbols in the current version includes
circle, square, diamond, triangle point-up, and triangle
point down.}

\item{typPntASB}{NUMERIC value between 1 to 5 for indicating type of point(s)
on alpha-spending boundaries. Symbols in the current version
includes circle, square, diamond, triangle point-up, and
triangle point down.}

\item{typLn0}{NUMERIC value for indicating type of null line.}

\item{typLnSig}{NUMERIC value for indicating type of line for statistical
significance.}

\item{typLnZCum}{NUMERIC value for indicating type of line for cumulative
z-score.}

\item{typLnASB}{NUMERIC value for indicating type of line for alpha-spending
boundaries.}

\item{typLnOIS}{NUMERIC value for indicating type of line for optimal
information size.}

\item{clrTtl}{CHARACTER of a color name for main title.}

\item{clrTtlX}{CHARACTER of a color name for title on axis X.}

\item{clrTtlY}{CHARACTER of a color name for title on axis Y.}

\item{clrAxsX}{CHARACTER of a color name for scale on axis X.}

\item{clrAxsY}{CHARACTER of a color name for scale on axis Y.}

\item{clrLgnd}{CHARACTER of a color name for legend.}

\item{clrLblY}{CHARACTER of a color name for the label "Cumulative z-score"
on axis Y.}

\item{clrLblStdy}{CHARACTER of color name(s) for the label(s) of each data source.}

\item{clrLblOIS}{CHARACTER of a color name for the label of optimal information
size.}

\item{clrLblAIS}{CHARACTER of a color name for the label of acquired information
size.}

\item{clrPntStdy}{CHARACTER of color name(s) for observed point(s) of data source.}

\item{clrPntASB}{CHARACTER of a color name for point(s) on the alpha-spending
boundaries.}

\item{clrLn0}{CHARACTER of a color name for null line.}

\item{clrLnSig}{CHARACTER of a color name for line of statistical significance.}

\item{clrLnZCum}{CHARACTER of a color name for line of cumulative z-score.}

\item{clrLnASB}{CHARACTER of a color name for line of alpha-spending boundaries.}

\item{clrLnOIS}{CHARACTER of a color name for line of optimal information size.}

\item{anglStdy}{NUMERIC value between 0 and 360 for indicating angle of data
source.}

\item{BSB}{LOGIC value for indicating whether to illustrate beta-spending
boundaries.}
}
\value{
\strong{PlotOSA()} returns a plot of observed sequential analysis.
}
\description{
\strong{PlotOSA()} is a function for plotting observed sequential analysis.
}
\examples{
## Not run:
# 1. Import a dataset of study by Fleiss (1993)
library(meta)
data("Fleiss1993bin")

# 2. Perform observed sequential analysis
 output <- DoOSA(Fleiss1993bin, study, year,
                 r1 = d.asp, n1 = n.asp,
                 r2 = d.plac, n2 = n.plac,
                 measure = "RR",
                 group = c("Aspirin", "Control"),
                 plot = TRUE)

# 3. Illustrate plot of observed sequential analysis
 PlotOSA(output)

## End(Not run)

}
\references{
Jennison, C., & Turnbull, B. W. (2005). Meta-analyses and adaptive group
sequential designs in the clinical development process.
\strong{Journal of biopharmaceutical statistics}, \emph{15(4)}, 537–558.
https://doi.org/10.1081/BIP-200062273.

Wetterslev, J., Jakobsen, J. C., & Gluud, C. (2017). Trial sequential analysis
in systematic reviews with meta-analysis. \strong{BMC medical research methodology},
\emph{17(1)}, 1-18.

NCSS Statistical Software (2023). \strong{Group-sequential analysis for two proportions}.
In \emph{PASS Documentation}. Available online:
https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/NCSS/Group-Sequential_Analysis_for_Two_Proportions.pdf
}
\seealso{
\code{\link{DoSA}}, \code{\link{DoOSA}}, \code{\link{PlotPower}}
}
\author{
Enoch Kang
}
