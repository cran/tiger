\name{convert_q_units}
\alias{convert_q_units}
\title{ Convert discharge units }
\description{
  converts discharge data of a given file (infile) in WASIM-format into output file (outfile)
 converting discharge from [m3/s] to [mm] (convert\_to="specific") or vice versa (convert\_to="absolute")
 using the file gauges\_area\_file containing the catchment areas and
 a timestep of timestep\_sec seconds
}
\usage{
convert_q_units (infile, outfile, gauges_area_file, convert_to, timestep_sec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{infile}{ name of file holding discharge data in WASIM format}
  \item{outfile}{ name of file to produce containing the converted discharge}
  \item{gauges_area_file}{ name of file containing tab-separated data
  (GAUGE	FOREIGN\_ID	SUBBAS\_ID	AREA\_SUBBAS\_KM2	AREA\_UPSTREAM\_KM2)}
  \item{convert_to}{ "specific": convert m3/s to mm; "absolute": convert mm to m3/s}
  \item{timestep_sec}{ length of timestep in inout file in seconds}
}
\details{
}
\value{
  no return value
}
\references{  }
\author{ Till Francke }
\seealso{  \code{\link{read.results}} }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}


\keyword{ manip }
\keyword{ utilities }
