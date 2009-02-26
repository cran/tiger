\name{eD}
\alias{eD}
\title{ Euclidean Distance }
\description{
  Calculate the euclidean distance for multiple vectors
}
\usage{
eD(x, y)
}
\arguments{
  \item{x}{ matrix with first set of vectors. }
  \item{y}{ matrix with second set of vectors. }
}
\details{
  x and y need the following structure to compare multiple vectors at
once:
  
   rows contain the k vectors

   columns the n coordinates in the n-space

   \code{str(x) ==  matrix [1:k, 1:n]}
}
\value{
  vector with the euclidean distance for each pair
}
\author{ Dominik Reusser }
\examples{
eD(1:3, 2:4)

## The function is currently defined as
function(x,y){
   return(sqrt(sum((x-y)^2)))
  }
}
\keyword{ utilities }
