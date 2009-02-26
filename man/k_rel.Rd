\name{k_rel}
\alias{k_rel}
\title{ Mean ratio of hydrological recession constants of two discharge time series }
\description{
  This function calculates the mean ratio between local hydrological recession constant for each point in two discharge time series. 
}
\usage{
k_rel(x, y)
}
\arguments{
  \item{x}{ discharge time serie  }
  \item{y}{ discharge time serie  }
}
\value{
 A scalar with the mean ratio
}
\author{ Dominik Reusser }
\seealso{ \code{\link{k_hyd}}, \code{\link{diagnostic_dawson}} }
\examples{

   data(example.peaks,package="tiger")

   k_rel(reference.peak, example.peaks[1,])

}
\keyword{ utilities }
