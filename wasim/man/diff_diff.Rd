\name{diff_diff}
\alias{diff_diff}
\title{ Count time steps with wrong direction in dynamics }
\description{
\code{diff_diff} counts  the time steps for two time series during
which the first shows an increase and the second shows a decrease.
}
\usage{
diff_diff(x, y)
}
\arguments{
  \item{x}{ Modelled time series or array with dimension \code{c(number_series, dim(y))}}
  \item{y}{ Measured time series }
}
\details{
}
\value{
 The count
}
\references{  }
\author{ Dominik Reusser}
\seealso{ \code{\link{diagnostic}} }
\examples{
   data(models_dawson,package="wasim")
   diff_diff(models_dawson[,1],models_dawson[,2])

   do.call(rbind,lapply(models_dawson, FUN =diff_diff, y=models_dawson[,1]))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
