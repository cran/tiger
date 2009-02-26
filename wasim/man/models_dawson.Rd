\name{models_dawson}
\alias{models_dawson}
\docType{data}
\title{ Synthetic peak errors from Dawson 2007}
\description{
   A number of synthetic peak errors used for testing performance
   measures and similar
}
\usage{data(models_dawson)}
\format{
  A data frame with 160 observations on the following 5 variables.
  \describe{
    \item{\code{Observed}}{a numeric vector}
    \item{\code{Model.A...Naive}}{a numeric vector}
    \item{\code{Model.B...Low.Flow}}{a numeric vector}
    \item{\code{Model.C...Noisy}}{a numeric vector}
    \item{\code{Model.D...High.Flow}}{a numeric vector}
  }
}
\details{
}
\source{
\url{https://co-public.lboro.ac.uk/cocwd/HydroTest/index.html}
}
\references{
Dawson, C. W.; Abrahart, R. J. & See, L. M. HydroTest: A web-based toolbox of evaluation metrics for the standardised assessment of hydrological forecasts Environmental Modelling & Software, 2007 , 22 , 1034-1052
}
\examples{
data(models_dawson)
plot(models_dawson[,1])
lines(models_dawson[,2])
diagnostic_dawson(modelled=models_dawson[,2], measured=models_dawson[,1], use_qualV=TRUE)
do.call(rbind,lapply(models_dawson,FUN=diagnostic_dawson, measured=models_dawson[,1]))
}
\keyword{datasets}
