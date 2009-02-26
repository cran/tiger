\name{k_hyd}
\alias{k_hyd}
\title{ Hydrological recession constant }
\description{
  This function calculates the local hydrological recession constant for each point in a time series. The function returns \code{NA} for periods with increasing discharge.
}
\usage{
k_hyd(x)
}
\arguments{
  \item{x}{ discharge time serie  }
}
\value{
Vector of recession constants.
}
\references{ Blume Recession Paper }
\author{ Dominik Reusser}
\seealso{  \code{\link{diagnostic_dawson}} }
\examples{

   data(example.peaks,package="tiger")

   k_hyd(reference.peak)

}
\keyword{ utilities }
