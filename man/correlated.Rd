\name{correlated}
\alias{correlated}
\alias{correl}
\title{ Calculate correlation structure }
\description{
   Calculate the correlation structure between multiple performance
measures
}
\usage{
correlated(result, limit = 0.85, plot.scatter = FALSE, keep = NA)
correl(measures, limit = 0.85, plot.scatter = FALSE, keep = NA)
}
\arguments{
  \item{result}{ object returned from \code{\link{tiger}} }
  \item{measures}{ data.frame for which to determine correlation structure }
  \item{limit}{ Limit for absolute correlation, above which data is considered to be correlated }
  \item{plot.scatter}{ Boolean, indicating whether to show pairwise plots for correlated measures }
  \item{keep}{ Vector with names of measures that must not be excluded because of correlation with other measures }
}
\value{
  correl returns:
  \item{pairs}{Matrix with indices of pairwise correlated measures}
  \item{pairs.by.name}{Matrix with measure names of pairwise correlated measures}
  \item{possible.exclusion}{List indicating which measures might be removed to end up with no strongly correlated measures. The list also indicates, which measure is correlated to the removed measures}
  \item{to.drop}{List of indices for measures to drop (according to previous list)}
  \item{to.drop.by.name}{List of measure names (of the previous list)}
 
  
  correlated returns a list of two correl results, one for the
original performance measures and one for the transformed measures
from a result from \code{\link{tiger}}.
}
\author{ Dominik Reusser }
\seealso{ This method helps to reduce the amount of data to be analyzed from an evaluation using \code{\link{tiger}}}
\examples{
data(tiger.example)
correlated <- correlated(tiger.single, keep=c("CE","RMSE" ))
}
\keyword{ utilities }
