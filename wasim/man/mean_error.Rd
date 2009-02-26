\name{mean_error}
\alias{mean_error}
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mean_error(a, b, cond = rep(TRUE, NROW(a)))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ ~~Describe \code{a} here~~ }
  \item{b}{ ~~Describe \code{b} here~~ }
  \item{cond}{ ~~Describe \code{cond} here~~ }
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
function (a, b, cond = rep(TRUE, NROW(a))){

cond[is.na(a)] <- FALSE

cond[is.na(b)] <- FALSE

a <- a[cond]

b <- b[cond]

return(sum(a-b)/NROW(a))

  }
}
\keyword{ utilities }
