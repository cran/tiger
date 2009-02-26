\name{f.score}
\alias{f.score}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
f.score(data, sens, measured, parameter, criterion, limit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ ~~Describe \code{data} here~~ }
  \item{sens}{ ~~Describe \code{sens} here~~ }
  \item{measured}{ ~~Describe \code{measured} here~~ }
  \item{parameter}{ ~~Describe \code{parameter} here~~ }
  \item{criterion}{ ~~Describe \code{criterion} here~~ }
  \item{limit}{ ~~Describe \code{limit} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, sens, measured, parameter, criterion, limit) 
{
	weigth<-sens[parameter,]>limit
	#functions <- array(dim=(max(c.krit)))
	#functions[[c.krit.rmse]] <- function(){return(0)}
	functions <- c(
		#rmse 
		function (x) {
		return(f.rmse(measured, as.numeric(x), weigth))
		},
		function(x) {
			return(f.nashS(measured,as.numeric(x), weigth))
		},
		function(x) {
			return(f.nashS_HF(measured,as.numeric(x), weigth))
		}
	)

	func <- functions[[criterion]]

	return(apply(data, 1, func))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ univar }% __ONLY ONE__ keyword per line
