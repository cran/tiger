\name{to.uniform}
\alias{to.uniform}
\title{ Transform data to uniform distribution }
\description{
Transform data to uniform distribution. Optionally, a set of values
can be transformed against a reference set of data.
}
\usage{
to.uniform(ref, val = NA)
}
\arguments{
  \item{ref}{ Set of values that determince the transformation }
  \item{val}{ Values to be transformed }
}
\details{
   If values is NA, the reference set itself will be transformed.
}
\value{
  Vector with transformed values.
}
\author{ Dominik Reusser }
\examples{
   a <- rnorm(100)
   hist(a)
   b <- to.uniform(a)
   hist(b)
   c <- to.uniform(ref=a, val=c(-0.5,0,0.5))
}
\keyword{ utilities }
