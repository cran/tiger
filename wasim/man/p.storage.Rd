\name{p.storage}
\alias{p.storage}
\alias{p.flow_comp}
\alias{p.balance}
\title{ Plot flow components / storage fillage / balance (integrals) of WaSiM model run }
\description{
For each component listed in storage, the time series for this
component is plotted in a separate window together with the measured
discharge.
}
\usage{
p.flow_comp(data, xdata = 1:NROW(data), measured = NULL,
                 data.names = data.types$beschreibung_en[csubset],
                 csubset = c(10, 29, 6, 8, 9, 5, 11), crain = 7, ylab =
                 "flow [mm/h]", c.flow_com = NULL, l.flow_com = NULL,
                 interflow_correction = TRUE, interflow_row = 11,
                 baseflow_row = 8, legend.position = "right", ...)
p.storage(data, xdata, measured, data.names =
                 data.types$beschreibung_en, storage = c(18, 20, 22,
                 24, 26), catchment = 1, mfrow = c(2, 3), ...)
p.balance(data, xdata, measured, plot = TRUE, flows = c(7, 10,
                 3, 1), storage = c(18, 20, 22, 24, 26), catchment = 1,
                 xlab = "time", ...)
}
\arguments{
  \item{data}{ Data as read with \code{\link{read.results} }}
  \item{xdata}{ Values ploted on the x axis, usually read with \code{\link{read.dates} }}
  \item{measured}{ Observed flow data, ploted as reference. Can be read with \code{\link{read.observations}}  }
  \item{data.names}{Names of data series used for legends and y-axes}
  \item{storage}{Vector or integers, indicating which data series to use as storages  }
  \item{plot}{ Boolean, indicating whether to plot the integrated flow components }
  \item{csubset}{vector of indices indicating which components to plot}
  \item{crain}{index of the column in \code{data} which carries rain information}
  \item{interflow_correction}{Boolean indicating whether baseflow should be subtracted from interflow (because WaSiM reports the sum of the two as interflow)}
  \item{interflow_row}{index of the column in \code{data} which contains interflow information}
  \item{baseflow_row}{index of the column in \code{data} which contains baseflow information}
  \item{ylab}{label to use for y-axis}
  \item{xlab}{label to use for x-axis}
  \item{mfrow}{mfrow parameter for plots}
  \item{legend.position}{position parameter for legend command}
  \item{c.flow_com}{vector of colors to use for the different flow components}
  \item{l.flow_com}{vector of line widths to use for the different flow components}
  \item{catchment}{Which subcatchment should be ploted (column index from data)}
  \item{flows}{numeric vector indicating which flow components to plot (indices from data)}
  \item{\dots}{ Additional arguments passed to plot }

}
\details{
}
\value{
 Used for its function of creating a plot
}
\references{  }
\author{ Dominik Reusser}
\note{ 

}
\seealso{  }
\examples{
d.datum <- read.dates(file= system.file("weisseritz/qgesww100.txt", package="wasim"))  # Datumsreihe heisst jetzt d.datum
d.wasim.out <- read.results("", system.file("weisseritz", package="wasim"),ts.length=21937,subcatchments=2, ending="ww100.txt")    # Datensatz heisst jetzt d.wasim.out 
d.meas.all<-read.table(system.file("weisseritz/ww100-rh.txt", package="wasim"),header=TRUE, na.strings ="999", skip=4)  # gemessene Daten einlesen
d.meas<-d.meas.all$Ammelsdorf[69716:91652]
p.storage(d.wasim.out, xdata=d.datum, measured=d.meas)
}
\keyword{ utilities }
\keyword{ hplot }
