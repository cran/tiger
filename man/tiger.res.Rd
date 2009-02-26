\name{tiger.res}
\alias{tiger.example}
\alias{tiger.res}
\alias{d.dates}
\docType{data}
\title{ Example data for tempperform package}
\description{
Example data for tempperform package 
}
\usage{data(tiger.example)}
\format{
   Object returned from \code{\link{tiger}}.
}
\source{
Reusser, D. E., Blume, T., Schaefli, B., and Zehe, E.: Analysing the temporal dynamics of model performance for hydrological models, Hydrol. Earth Syst. Sci. Discuss., 5, 3169-3211, 2008. 
}
\examples{
data(tiger.example)
errors.in.time(d.dates, tiger.res, solution=6, show.months=TRUE)
}
\keyword{datasets}
