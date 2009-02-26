\name{line_sumrain}
\alias{line_sumrain}
\title{ Draw line of cumulative rainfall }
\description{
 Add a line with cumulative rainfall data to a plot. The maximum value of the plot has to be passed. If the cumulative rainfall exceeds this maximum, it will start again at 0.
}
\usage{
line_sumrain(xdata, cum_sum_rain, theMax, ...)
}
\arguments{
  \item{xdata}{ Values ploted on the x axis, usually read with \code{\link{read.dates} }}
  \item{cum_sum_rain}{ Vector with the cumulative rain }
  \item{theMax}{ The maximum of the y-axis}
  \item{\dots}{ additional arguments passed to \code{lines} }
}
\details{
}
\value{
 Used for its side effect
}
\references{ }
\author{ Dominik Reusser }
\note{ 
}
\seealso{  See also \code{\link{p.flow_comp}} }
\examples{
  #Uniform rain
  plot(1:100, rep(1, 100),ylim=c(10,0),type="h", xlab="time", ylab="rainfall")
  line_sumrain(xdata=1:100, cum_sum_rain=(1:100), theMax=10)
}
\keyword{ utilities }
\keyword{ hplot }
