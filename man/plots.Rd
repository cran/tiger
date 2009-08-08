\name{plots}
\alias{box.plots}
\alias{errors.in.time}
\alias{peaks.in.clusters}
\alias{peaks.on.som}
\alias{peaks.measures}
\alias{scatterplot}
\alias{p.validityIndex}
\title{ Evaluation plots for temporal dynamics of model performance }
\description{
   Create various plot to understand the temporal dynamics of model performance 
}
\usage{
box.plots(result, solution, show.measures = 1:num.measures,
             new.order = 1:solution, show.synthetic.peaks = FALSE,
             synthetic.peaks.col = c(2:8, 2:8), show.timestep = NA,
	     show.cell = NA,
                 ref = NULL, ref.new.order = new.order, ref.solutions =
                 solution, col.best.match = "black",
                 clusterPalette = rainbow(solution))
errors.in.time(xval, result, solution, show.months = FALSE, new.order
                 = 1:solution, x.range = 1:length(xval), pmax =
                 max(c(result$measured, result$modelled), na.rm =
                 TRUE), data.colors = data.frame(measured = c("grey"),
                 modelled = c("black"), rain = c("black")),
                 clusterPalette = rainbow(solution), color.cut.off = 0,
                 frac.max = 0.7, frac.min = 0.4, legend.pos =
                 "topleft", ...)
peaks.in.clusters(result, solution,  new.order = 1:solution)
peaks.on.som(result, solution, clusterPalette=rainbow(solution), cell.size = 0.9, mfrow=c(2,ceiling(n.errors/2)), new.order=1:solution)
peaks.measures(result, show.measures = 1:num.measures,
                 synthetic.peaks.col = c(2:8, 2:8), mfrow = c(2, 3),
                 col.best.match = "black", do.out = rep(TRUE,
                 length(show.measures)))
scatterplot(measures, show.measures=1:num.measures)
p.validityIndex(result, validity.max)
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
  \item{cell.size}{fraction of the cell square to be filled with color}
  \item{show.cell}{the scores for a certain cell on the SOM can be
ploted as blue line on the box plot (see examples)}
  \item{x.range}{Indizes of x-values to be plotted}
  \item{pmax}{maximum discharge for definition of the plot range}
  \item{frac.min}{minimum of the y-range covered by color bars for cluster occurence}
  \item{frac.max}{maximum of the y-range covered by color bars for cluster occurence}
  \item{clusterPalette}{colors to use for the clusters}
  \item{color.cut.off}{Value of cluster occurence below which the color bar is set to transparent (for better readability)}
  \item{legend.pos}{Position of the legend}
  \item{data.colors}{Color definition for rainfall and runoff}
  \item{show.timestep}{timestep for which
the values for the performance measures are to be plotted as black lines in the box plot }
  \item{xval}{ Values to be plotted on the x-axis (e.g. POSIX-date) }
  \item{show.months}{ Boolean indicating whether to add month ticks to x axis }
  \item{mfrow}{ see \code{\link{par}}}
  \item{ref}{Reference solution to be ploted in grey on the box plot}
  \item{ref.new.order}{New numbering to assign to clusters for reference solution on the box plot}
  \item{ref.solutions}{Number of clusters for reference solution for which to plot the box plot}
  \item{validity.max}{Do not plot solutions with cluster numbers resulting above in a validty index above validity.max}
  \item{col.best.match}{Color to use for plotting the line indicating
                        the position of the best match}
  \item{...}{additional parameters passed to plot}
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
correlated <- correlated(tiger.single, keep=c("CE","RMSE" ))

opar <- par(mfrow=c(3,5))
box.plots(tiger.single, solution=6, new.order=new.order, show.synthetic.peaks=TRUE)
box.plots(tiger.single, solution=6, new.order=new.order, show.cell=data.frame(x=1,y=1))
par(opar)
errors.in.time(xval=d.dates, result= tiger.single, solution=6, show.months=TRUE, new.order=new.order)
peaks.in.clusters(tiger.single, solution=6, new.order=new.order)
peaks.measures(tiger.single, show.measures=correlated$measures.uniform$to.keep)
scatterplot(tiger.single$measures.uniform, show.measures=correlated$measures.uniform$to.keep)


}
\keyword{ utilities }
\keyword{ hplot }
