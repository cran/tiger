\name{tiger}
\alias{tiger}
\alias{tiger.peaks}
\title{ Calculate temporal dynamics of model performance }
\description{
 About fifty performance measures are calculated for a gliding window,
comparing two time series. The resulting matrix is clustered, such
that each time window can be assigned to an error type cluster. The
mean performance measures for each cluster can be used to give meaning
to each cluster. Additionally, synthetic peaks are used to better
characterize the clusters.
}
\usage{
tiger(modelled, measured, window.size,  maxc = 15, synthetic.errors=NA)
tiger.peaks(result, synthetic.errors)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelled}{ Time series of modelled data }
  \item{measured}{ Time series of measured data }
  \item{window.size}{ Size of the moving window }
  \item{maxc}{ Maximum number of clusters to be tested }
  \item{synthetic.errors}{ Matrix returned from \code{\link{synth.peak.error}}  }
  \item{result}{ object returned from tiger}
}
\details{
   See the package vignette.
}
\value{
 \item{maxc}{see input parameter}
 \item{window.size}{see input parameter}
 \item{modelled}{see input parameter}
 \item{measured}{see input parameter}
 \item{synthetic.errors}{see input parameter}
 \item{measures.synthetic.peaks}{matrix of performance measures for synthetic errors}
 \item{measures}{matrix of performance measures for the gliding time window}
 \item{na.rows}{vector of boolean, indicating which time windows contain NA values}
 \item{names}{names of the perfomance measures}
 \item{measures.uniform}{measures, transformed to uniform distribution}
 \item{measures.uniform.synthetic.peaks}{measures for synthetic errors, transformed with the corresponding transformation from previous item}
 \item{error.names}{names of the synthetic error types}
 \item{best.value.location}{list, indicating what the value for "no
error" for each performance measure is}
 \item{validityMeasure}{vector with validty index for solutions with 2:maxc clusters}
 \item{cluster.assignment}{list of 2:maxc objects returned from \code{\link[e1071]{cmeans}}}
}
\references{ Reusser, D. E., Blume, T., Schaefli, B., and Zehe, E.: Analysing the temporal dynamics of model performance for hydrological models, Hydrol. Earth Syst. Sci. Discuss., 5, 3169-3211, 2008. }
\author{ Dominik Reusser }
\seealso{ The package vignette}
\examples{
data(tiger.example)
modelled <- tiger.res$modelled
measured <- tiger.res$measured
peaks <- synth.peak.error(rise.factor=2, recession.const=0.02, rise.factor2=1.5)
result2 <- tiger(modelled=modelled[1000:3000], measured=measured[1000:3000], window.size=240, synthetic.errors=peaks)
errors.in.time(d.dates[1000:3000], result2, solution=6, show.months=TRUE)

peaks2 <- synth.peak.error(rise.factor=2, recession.const=0.02,
     rise.factor2=1.5, err1.factor=c(1.3,1.5,2.0),
     err2.factor = c(0.02,0.03,0.06), 
     err3.factor=c(2,4,10), 
     err4.factor = c(9,22,40), 
     err5.factor = c(0.2,0.3,0.5),
     err6.factor =c(2,3,5),
     err9.factor=c(1.5,3,6)
   )

result3 <- tiger.peaks(result2, peaks2)

   peaks.in.clusters(result2, solution=6)
   x11()
   peaks.in.clusters(result3, solution=6)

}
\keyword{ utilities }
