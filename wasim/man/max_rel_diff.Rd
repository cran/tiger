\name{max_rel_diff}
\alias{max_rel_diff}
\title{ Calculate the maximum ration of the derivatives of two time series }
\description{
Calculate the maximum ration of the derivatives of two time series 
}
\usage{
max_rel_diff(x, y)
}
\arguments{
  \item{x}{ Modelled time series or array with dimension \code{c(number_series, dim(y))}}
  \item{y}{ Measured time series }
}
\details{
}
\value{
  \code{max(diff(x) / diff(y), na.rm=TRUE)}
}
\references{  }
\author{ Dominik Reusser}
\seealso{ \code{\link{diagnostic}} }
\examples{
   data(models_dawson,package="wasim")
   max_rel_diff(models_dawson[,1],models_dawson[,2])

   do.call(rbind,lapply(models_dawson, FUN =max_rel_diff, y=models_dawson[,1]))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
