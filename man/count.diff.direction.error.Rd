\name{count.diff.direction.error}
\alias{count.diff.direction.error}
\title{ Compare sign of derivatives }
\description{
Counts the number of elements for which two vectors show
different signs in the derivative.
}
\usage{
count.diff.direction.error(x, y)
}
\arguments{
  \item{x}{ First vector }
  \item{y}{ Second vector }
}
\value{
  \code{sum((diff(x) / diff(y))<0, na.rm=T)} 
}
\author{ Dominik Reusser }
\seealso{ \code{\link{diagnostic_dawson}} }
\examples{
#All different
count.diff.direction.error(1:10,10:1)

#One different
count.diff.direction.error(1:10,c(1:9, 8))

}
\keyword{ utilities }
