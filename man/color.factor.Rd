\name{color.factor}
\alias{color.factor}
\title{ Create colors with intensity according to the magnitude of a value }
\description{
Create colors with intensity according to the magnitude of a value
}
\usage{
color.factor(color, value, max)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{color}{ The base color(s) to use }
  \item{value}{ A vector of values }
  \item{max}{ The maximum value represented by full intensity }
}
\value{
 A vector of colors, one entry for each value
}
\author{ Dominik Reusser }
\examples{
   data <- 1:10
   cols=color.factor("red", data, max=10)
   plot(data, col=cols)


   cols=color.factor(c("red","green","blue"), data, max=10)
   plot(data, col=cols)
}
\keyword{ color }
