\name{p.grid}
\alias{p.grid}
\alias{read.grid}
\title{ Read and plot a grid-file output by WaSiM }
\description{
   These two functions help to read and plot grid-files created by
   WaSiM. You need the gridasci-executable if to convert binary grid
   data to ascii grid data.
}
\usage{
p.grid(grid, ...)
read.grid(file, record_size=4)
}
\arguments{
  \item{grid}{ A grid read with the read.grid function }
  \item{\dots}{ Additional arguments passed to plot }
  \item{file}{ WASIM grid output file in ASCII or binary format.}
  \item{record_size}{ number of bytes used to represent a float value in the binary grid. Usually 4, but may be platform-dependent.}
  
}
\details{
  The file is opened as a binary grid unless the infile has one of the following extensions: *.asc, *.ascii, *.txt.
  Nodata values are represented by NAs.
}
\value{
  read.grid returns a list:
  \item{head }{dataframe of header information containing the fields
  ncols, nrows, xllcorner, yllcorner, cellsize, nodata\_value.}
  \item{tab }{The actual grid}
}
\references{ WaSiM-ETH manual }
\author{ Dominik Reusser, Till Francke}
\seealso{  \code{\link{read.results}}, \code{\link{p.flow_comp}} for more WaSiM related function.
  \code{\link{filled.contour}} for plotting options}
\examples{
  sd.grid <- read.grid( file= system.file("weisseritz/sd__ww100.grd", package="wasim"))  # Datumsreihe heisst jetzt d.datum
  p.grid(sd.grid, 
        color.palette=topo.colors,
	      zlim=c(-20,20),
        main="Saturation deficit at the Weisseritz")
}
\keyword{ hplot }
\keyword{ utilities }
