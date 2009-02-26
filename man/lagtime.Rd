\name{lagtime}
\alias{lagtime}
\title{ Lagtime between two time series }
\description{
 This function calculates the lagtime between x and y, defined as the shift resulting in the maximum cross correlation. 
}
\usage{
lagtime(x, y)
}
\arguments{
  \item{x}{ Time series }
  \item{y}{ Time series }
}
\value{
 The lagtime as scalar. Positive if x is shifted towards later times.
}
\author{Dominik Reusser }
\seealso{ \code{\link{ccf}},\code{\link{diagnostic_dawson}} }
\examples{
   data(example.peaks,package="tiger")
   plot(reference.peak, type="l")
   lines(example.peaks[7,], lty=2)
   lagtime(reference.peak, example.peaks[7,])

}
\keyword{ utilities }
