\name{plots}
\alias{box.plots}
\alias{errors.in.time}
\alias{peaks.in.clusters}
\alias{peaks.measures}
\alias{scatterplot}
\title{ Evaluation plots for temporal dynamics of model performance }
\description{
   Create various plot to understand the temporal dynamics of model performance 
}
\usage{
box.plots(result, solution, show.measures = 1:num.measures,
             new.order = 1:solution, show.synthetic.peaks = FALSE,
             synthetic.peaks.col = c(2:8, 3), show.timestep = NA,
                 ref = NULL, ref.new.order = new.order, ref.solutions =
                 solution)
errors.in.time(xval, result, solution, show.months = FALSE, new.order = 1:solution )
peaks.in.clusters(result, solution,  new.order = 1:solution)
peaks.measures(result, show.measures = 1:num.measures, synthetic.peaks.col = c(2:8, 2:8), mfrow = c(2, 3), do.out = rep(TRUE, length(show.measures)))
scatterplot(measures, show.measures=1:num.measures)
}
\arguments{
  \item{result}{ object returned from \code{\link{tiger}}}
  \item{measures}{ data.frame from which to create a scatter plot.  e.g. result\$measures.uniform }
  \item{solution}{ number of clusters to use for further evaluations (see also \code{\link{validityIndex}})}
  \item{show.measures}{ vector of indices indicating for which performance measures to show the plots}
  \item{new.order}{New numbering to assign to clusters. See also \code{\link{change.order.clusters}}}
  \item{show.synthetic.peaks}{Show values of the synthetic peaks on top of the box plots.}
  \item{synthetic.peaks.col}{Colors to use for synthetic peaks.}
  \item{do.out}{vector of booleans indicating whether to exclude outliers when showing the plot}
  \item{show.timestep}{timestep for which
the values for the performance measures are to be plotted as black lines in the box plot }
  \item{xval}{ Values to be plotted on the x-axis (e.g. POSIX-date) }
  \item{show.months}{ Boolean indicating whether to add month ticks to x axis }
  \item{mfrow}{ see \code{\link{par}}}
  \item{ref}{Reference solution to be ploted in grey on the box plot}
  \item{ref.new.order}{New numbering to assign to clusters for reference solution on the box plot}
  \item{ref.solutions}{Number of clusters for reference solution for which to plot the box plot}
}
\details{
box.plots: for each performance measure, a box plot is created
showing the values for each cluster

errors.in.time: occurence of the errors cluster along the time
dimension

peaks.in.clusters: table of the position of the synthetic peak
errors in the clusters.

peaks.measures: responce of the performance measures to the
synthetic peak errors.


scatterplot: scatter plot of the performance measures


  See package vignette for further details about which plot does what.
}
\value{
 used for the side effect of plotting results
}
\references{ Reusser, D. E., Blume, T., Schaefli, B., and Zehe, E.: Analysing the temporal dynamics of model performance for hydrological models, Hydrol. Earth Syst. Sci. Discuss., 5, 3169-3211, 2008. }
\author{ Dominik Reusser }
\seealso{ The package vignette}
\examples{
data(tiger.example)

new.order <- c(6,3,2,5,4,1)
correlated <- correlated(tiger.res, keep=c("CE","RMSE" ))

opar <- par(mfrow=c(3,5))
box.plots(tiger.res, solution=6, new.order=new.order, show.synthetic.peaks=TRUE)
par(opar)
errors.in.time(d.dates, tiger.res, solution=6, show.months=TRUE, new.order=new.order)
peaks.in.clusters(tiger.res, solution=6, new.order=new.order)
peaks.measures(tiger.res, show.measures=correlated$measures.uniform$to.keep)
scatterplot(tiger.res$measures.uniform, show.measures=correlated$measures.uniform$to.keep)


}
\keyword{ utilities }
\keyword{ hplot }
