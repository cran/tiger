\name{span}
\alias{span}
\title{ Calculate the span/range of a time series}
\description{
This function has been implemented for a simpler call of apply.
}
\usage{
span(x)
}
\arguments{
  \item{x}{ A vector}
}
\details{
}
\value{
\code{ diff(range(x,na.rm=TRUE)) }
}
\references{  }
\author{ Dominik Reusser}
\seealso{ \code{\link{diagnostic}} }
\examples{
   data(models_dawson,package="wasim")
   span(models_dawson[,1])

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
