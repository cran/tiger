\name{include.others}
\alias{include.others}
\title{ Internal Function: evaluate box plot}
\description{
Find clusters with a comparable position on
the box plot with respect to the best value. 
Comparable position means the median of one set of values falls within
the interquartile range of the reference set of values
}
\usage{
include.others(selected, center, stats, best = FALSE)
}
\arguments{
  \item{selected}{ index of the best value set. }
  \item{center}{ where is the best value within the  }
  \item{stats}{ stats element of a boxplot result }
  \item{best}{ are we comparing against the best set? }
}
\value{
   Vector of indizes for which elements are comparable
}
\author{ Dominik Reusser }
\seealso{  \code{\link{box.plots}} }
\keyword{ utilities }
