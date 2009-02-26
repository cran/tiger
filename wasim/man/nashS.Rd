\name{nashS}
\alias{nashS}
\alias{nashS_HF}
\title{ Calculates the (weighted) Nash Suttcliffe Efficiency coefficient }
\description{
Calculates the Nash Suttcliffe Efficiency coefficient. 
}
\usage{
nashS(modelled, measured, weigth = NA)
nashS_HF(modelled, measured, weigth = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelled}{ Vector with modeled data }
  \item{measured}{ Vector with measured data }
  \item{weigth}{ If this vector is supplied, each data point is
  weighted accordingly }
}
\details{
The weighting corresponds to the value in the empirical cumulative
distribution function.
}
\value{
   Returns a scalar between -Inf and 1 corresponding to the agreement
   between measured and modelled data. 0 means the model agrees
   equally well as the mean value.
}
\references{ \url{http://en.wikipedia.org/wiki/Nash-Sutcliffe_efficiency_coefficient} }
\author{ Dominik Reusser}
\note{ }
\seealso{   }
\examples{

}
\keyword{ utilities }
\keyword{ univar }
