\name{tiger-package}
\alias{tiger-package}
\docType{package}
\title{
TIme series of Grouped ERrors
}
\description{
 About fifty performance measures are calculated for a gliding window,
comparing two time series. The resulting matrix is clustered, such
that each time window can be assigned to an error type cluster. The
mean performance measures for each cluster can be used to give meaning
to each cluster. Additionally, synthetic peaks are used to better
characterize the clusters. The package provides functions to calculate
and visualize these results.
}
\details{

Use tiger to perform the calculations. See the package
vignette for an example, how to perform calculations and how to
evaluate information
}
\author{
Dominik Reusser

Maintainer: Dominik Reusser <dreusser@uni-potsdam.de>
}
\references{ Reusser, D. E., Blume, T., Schaefli, B., and Zehe, E.: Analysing the temporal dynamics of model performance for hydrological models, Hydrol. Earth Syst. Sci. Discuss., 5, 3169-3211, 2008. }
\keyword{ package }
\examples{
data(tiger.example)
modelled <- tiger.single$modelled
measured <- tiger.single$measured
peaks <- synth.peak.error(rise.factor=2, recession.const=0.02, rise.factor2=1.5)
\dontrun{result2 <- tiger(modelled=modelled, measured=measured, window.size=240, synthetic.errors=peaks)
errors.in.time(d.dates, result2, solution=6, show.months=TRUE)}

}
