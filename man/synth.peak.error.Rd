\name{synth.peak.error}
\alias{synth.peak.error}
\alias{synth.peak}
\alias{p.synth.peak.error}
\title{ Generate synthetic peaks and peak errors }
\description{
  These functions allow generation of synthetic hydrologic peaks
generated from a combination of exponetial functions. Also, synthetic
errors for the reproduction of a reference peak can be generated in
order to subsequentially test the behaviour of performance measures
with respect to these errors.
}
\usage{
synth.peak(base = 0.07, base.time = 6, rise.time = 5, rise.factor,
recession.const = 0.2, length.out = 240, rez.time)
synth.peak.error(base = 0.07, base.time = 6, rise.time = 5,
rise.factor, rise.factor2, recession.const = 0.2, length.out = 240,
rez.time = length.out - base.time - rise.time, err1.factor = c(1.2,
1.4, 1.6), err2.factor = base/c(10, 5, 2), err3.factor = c(2, 4,
8), err4.factor = c(9, 18, 27), err5.factor = c(0.1, 0.2, 0.4),
err6.factor = c(0.7, 0.8, 0.9), 
err9.factor = c(2, 3, 4.5))
p.synth.peak.error(peaks, y.max = (max(peaks, na.rm = TRUE)),
                 peak.cluster = NULL, peak.palette = grey(c(0, 0.6,
                 0.8)), use.layout = TRUE, show.errors = 1:n.errors,
                 peak.lty = rep(1, n.errors), mfrow = c(2,
                 ceiling(length(show.errors)/2)), plot.legend = TRUE,
                 print.error.nr = TRUE)
}
\arguments{
  \item{base}{ level of the base flow component }
  \item{base.time}{ number of time steps before the rise phase starts.
May be negative, such that the peak starts outside the window. }
  \item{rise.time}{ Number of time steps for the rise phase}
  \item{rise.factor}{ The peak maximum is about \code{rise.factor}
higher than the base flow. }
  \item{rise.factor2}{ \code{rise.factor} for the "peaks" which only
show a recession phase. }
  \item{recession.const}{ Recession constant for the peak. }
  \item{length.out}{ Total length of the time series to be returned. }
  \item{rez.time}{ Length of the recession phase }
  \item{err1.factor}{ Factors to use for the first error type: Over-
and underestimation of the peak }
  \item{err2.factor}{ Factors to use for the second error type:  shifting of the entire time series}
  \item{err3.factor}{ Factors to use for the third error type:
Recession too fast/too slow }
  \item{err4.factor}{ Factors to use for the fourth error type: Lag time  }
  \item{err5.factor}{ Factors to use for the fifth error type: Correct
total volume, but peak over/underestimated }
  \item{err6.factor}{ Factors to use for the sixth error type: Peak
too wide/too narrow }
  \item{err9.factor}{ Factors to use for the ninth error type: Shift
during the recession phase }
  \item{peaks}{ object returned from synth.peak.error() }
  \item{y.max}{ upper limit for the y-axis }
  \item{peak.cluster}{object returned from \code{\link{peaks.in.clusters}} used for
coloring the cluster assignment of synthetic peaks (see examples)}
  \item{peak.palette}{Colors to use if peak.cluster is NULL: first
color for reference, second and third for peaks over- and
underestimating the reference}
  \item{use.layout}{Boolean, indicating whether to use the predefined
layout}
  \item{plot.legend}{Boolean, indicating whether to show the legend}
  \item{print.error.nr}{Boolean, indicating whether to label each subplot with a number}
  \item{mfrow}{mfrow plot parameter (only used, if use.layout=FALSE)}
  \item{show.errors}{Vector with indices indicating which errors to display}
  \item{peak.lty}{Line types for either clusters as defined in
peak.cluster or as in peak.palette}
}
\value{
   synth.peak returns a vector with the synthetic peak according to
the provided parameters
   synth.peak.error returns an array with dimension 3. The first
dimension corresponds to the error type. The second dimension to the
level of the corresponding error type. The third dimension corresponds
to the time.
}
\references{ Reusser, D. E., Blume, T., Schaefli, B., and Zehe, E.: Analysing the temporal dynamics of model performance for hydrological models, Hydrol. Earth Syst. Sci. Discuss., 5, 3169-3211, 2008. }
\author{ Dominik Reusser }
\seealso{ The package vignette }
\examples{
     ref.peak <- synth.peak(rise.factor=2, recession.const=0.02)
peaks <- synth.peak.error(rise.factor=2, recession.const=0.02, rise.factor2=1.5)
peaks2 <- synth.peak.error(rise.factor=2, recession.const=0.02,
     rise.factor2=1.5, err1.factor=c(1.3,1.5,2.0),
     err2.factor = c(0.02,0.03,0.06), 
     err3.factor=c(2,4,10), 
     err4.factor = c(9,22,40), 
     err5.factor = c(0.2,0.3,0.5),
     err6.factor =c(2,3,5),
     err9.factor=c(1.5,3,6)
   )

    p.synth.peak.error(peaks)
    p.synth.peak.error(peaks2)

    data(tiger.example)
    peak.cluster <- peaks.in.clusters(result=tiger.single,  solution=5, new.order=c(2,3,5,1,4))
    p.synth.peak.error(peaks=tiger.single$synthetic.errors, peak.cluster=peak.cluster, peak.palette=rainbow(5))

}
\keyword{ utilities }
\keyword{ hplot }
