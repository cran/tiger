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
\details{
}
\value{
 The lagtime as scalar. Positive if x is shifted towards later times.
}
\references{  }
\author{Dominik Reusser }
\note{ }
\seealso{ \code{\link{ccf}},\code{\link{diagnostic}} }
\examples{
   data(example.peaks,package="wasim")
   plot(reference.peak, type="l")
   lines(example.peaks[7,], lty=2)
   lagtime(reference.peak, example.peaks[7,])

}
\keyword{ utilities }
