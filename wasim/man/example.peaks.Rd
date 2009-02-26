\name{example.peaks}
\alias{example.peaks}
\alias{reference.peak}
\docType{data}
\title{Synthetic peak errors}
\description{
   A number of synthetic peak errors used for testing performance
   measures and similar
}
\usage{data(example.peaks)}
\format{
  The format for example.peaks is:
 num [1:12, 1:91] 0.1346 0.1346 0.1846 0.0846 0.1346 ...

  The format for the reference.peak is:
 num [1:91] 0.135 0.134 0.134 0.134 0.134 ...
}
\details{
}
\source{
}
\references{
}
\examples{
data(example.peaks)
str(example.peaks)
str(reference.peak)
plot(reference.peak,type="line")
lines(example.peaks[,1], lty=2)
diagnostic(measured = reference.peak, modelled = example.peaks[1,])
## maybe str(peaks) ; plot(peaks) ...
}
\keyword{datasets}
