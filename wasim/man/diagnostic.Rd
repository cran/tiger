\name{diagnostic}
\alias{diagnostic}
\alias{diagnostic_dawson}
\alias{diagnostic_window}
\alias{diagnostic_series}
\title{ Calculate a number of objectives to compare time series }
\description{
   \code{diagnostic} and \code{diagnostic_dawson} take two vectors 
   (assumed to be time series) and
   calculates the following objective functions to compare them:
   correalation, Nash Sutcliffe efficiency, ratio of the integral,
   lagtime (maximum of the cross correlation), the number of timesteps
   with opposite sign of the derivative, the highest ratio between
   recession coefficients and the root mean square error, as well as
   the ones listed in Dawson 2007.

   \code{diagnostic_window} calcualtes these measures for a
   part of the time series only. It is used internally by
   \code{diagnostic_series} ToDo: Merge with diagnostic.
   
   \code{diagnostic_series} takes this a step further by
   calculating the above measures for a gliding window along the time
   series and calculating additional measures.
   Similar to \code{\link{diagnostic}}, the function
   takes two vectors (assumed to be time series) and
   calculates a number of objectives compare them.
   In contrast to the more simple \code{diagnostic}, the same objectives
    are applied to a gliding window and a few additional objectives
    are calcualated: the ratio of the derivatives, the ratio of the
    recession coefficients for each time step and the current quantile
    of the residuals.
}
\usage{
diagnostic(modelled, measured, use_qualV = FALSE)
diagnostic_window(position, duration, measured, modelled, use_qualV = FALSE, diff.ecdf=NA)
diagnostic_series(measured, modelled, duration, integral_correction = FALSE, use_qualV = FALSE)
diagnostic_dawson(modelled, measured, p=NA, m=NA, additional=TRUE, use_qualV=FALSE, diff.ecdf=NA )
}
\arguments{
  \item{modelled}{ Modelled time series or array with dimension \code{c(number_series, dim(measured))}}
  \item{measured}{ Measured time series }
  \item{position}{ Index from where to start the calculation }
  \item{duration}{ Number of elements to include }
  \item{integral_correction}{ Boolean. If true, the ratio of the
  integrals is divided by the total ratio of the entire integral. This
  way, relative integral errors can be detected.}
  \item{p}{ The number of free parameters in each model - required to
  calculate AIC and BIC}
  \item{m}{ The number of data points that were used in the model
  calibration - required to calculate AIC and BIC}
  \item{additional}{Boolean, indicating whether to calculate
  additional measures to the ones defined in Dawson 2007}
  \item{use_qualV}{Boolean, indicating whether to calculate
  the additional measures defined in Jachner 2007}
  \item{diff.ecdf}{\code{\link{ecdf}}-function of the bias (measured-modelled)}
}
\details{
 For more details on the objectives, see the see-also-section
}
\value{
 A data frame with the described objectives
}
\references{ Dawson, C. W.; Abrahart, R. J. & See, L. M. HydroTest: A web-based toolbox of evaluation metrics for the standardised assessment of hydrological forecasts Environmental Modelling & Software, 2007 , 22 , 1034-1052

Jachner, S.; van den Boogaart, K. G. & Petzoldt, T. Statistical Methods for the Qualitative Assessment of Dynamic Models with Time Delay (R Package qualV) Journal of Statistical Software, 2007 , 22 , 1-30}
\author{ Dominik Reusser }
\note{
}
\seealso{ \code{\link{cor}},
\code{\link{nashS}},\code{\link{lagtime}},\code{\link{count.diff.direction.error}},\code{\link{k_rel}},\code{\link{rmse}} }
\examples{
   data(example.peaks,package="wasim")

   plot(reference.peak, type="l")
   lines(example.peaks[1,], lty=2)

   diagnostic(measured = reference.peak, modelled = example.peaks[1,])

   diagnostic(measured = reference.peak, modelled = example.peaks)

   #first half only
   diagnostic_window(measured=reference.peak, modelled=example.peaks[1,], position = 1, duration = 45 )
   
   #gliding window for 10 time steps
   diagnostic_series(measured=reference.peak, modelled=example.peaks[1,], duration = 10 )

}
\keyword{ utilities }
