\name{myRead.table}
\alias{myRead.table}
\title{ Read a wasim result file into a table with given length }
\description{
  This function reads an output-txt File from WaSiM. It is usually called from other functions. Standard colum names are generated. Date and time are removed from the data object (see \link{read.dates}). Finaly, the time series are expanded with NA to fill a certain length (If simulations stoped to early).
}
\usage{
myRead.table(name, subcatchments, has_stat, ts.length, na.values)
}
\arguments{
  \item{name}{ File name to read}
  \item{ts.length}{ Number of expected data points for the time series. The resulting table is guaranteed to have this length }
  \item{subcatchments}{ Number of subcatchments simulated }
  \item{has_stat}{ boolean indicating wheter statistics column exists}
  \item{na.values}{ String which identifies NA }
}
\details{
}
\value{
  A table of dimension dim=c(subcatchments+1,ts.length). The last column contains the statistics column if existing.
}
\references{  }
\author{ Dominik Reusser}
\note{ 

}
\seealso{  }
\examples{
}
\keyword{ file }
\keyword{ utilities }
