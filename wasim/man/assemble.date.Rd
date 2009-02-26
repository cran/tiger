\name{assemble.date}
\alias{assemble.date}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
assemble.date(table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{table}{ ~~Describe \code{table} here~~ }
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
function (table) 
{
    t.midnight <- table$Hour==24
    t.next <- c(FALSE,t.midnight[-NROW(t.midnight)])
    table$Day[t.midnight] <- table$Day[t.next]
    table$Month[t.midnight] <- table$Month[t.next]
    table$Year[t.midnight] <- table$Year[t.next]
    table$Hour[t.midnight] <- 0
    return( strptime(paste(sep="",table$Year, "-", table$Month, "-", table$Day, " ", table$Hour\%\%24), "\%Y-\%m-\%d \%H"))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
\keyword{ ts }
