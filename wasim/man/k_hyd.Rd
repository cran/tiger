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
\details{
}
\value{
Vector of recession constants.
}
\references{ Blume Recession Paper }
\author{ Dominik Reusser}
\note{}
\seealso{  \code{\link{diagnostic}} }
\examples{

   data(example.peaks,package="wasim")

   k_hyd(reference.peak)

}
\keyword{ utilities }
