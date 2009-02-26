\name{validityIndex}
\alias{validityIndex}
\title{ Validity Index for fuzzy clustering }
\description{
 Calculate the validity index for fuzzy clusters. A validity index
below 1 indicates, that in between clusters is larger than within
clusters. Evaluating the validity index for various numbers of desired
clusters may help to find the minimum.
}
\usage{
validityIndex(cclust, values, verbose = FALSE)
}
\arguments{
  \item{cclust}{ object returned from \code{\link[e1071]{cmeans}} }
  \item{values}{ data provided as \code{x} to \code{\link[e1071]{cmeans}}  }
  \item{verbose}{ boolean. If true, values for numerator and denominater are printed. }
}
\value{
  A single number, the validity index.
}
\references{ Reusser, D. E., Blume, T., Schaefli, B., and Zehe, E.: Analysing the temporal dynamics of model performance for hydrological models, Hydrol. Earth Syst. Sci. Discuss., 5, 3169-3211, 2008. 

Xie, X. and Beni, G.: A validity measure for fuzzy clustering, IEEE T. Pattern Anal., 13, 841-847, 1991. 3181}
\author{ Dominik Reusser }
\seealso{\code{\link[e1071]{cmeans}} for the fuzzy clustering itself}
\examples{
myOrig <- matrix(c(c(1,0,0,1),
         c(0,0,1,2),
         c(1,1,0,3)), nrow=3, ncol=4, byrow=TRUE)


myData <- rbind(
     matrix(myOrig[1,], nrow=50, ncol=4, byrow=TRUE),
     matrix(myOrig[2,], nrow=50, ncol=4, byrow=TRUE),
     matrix(myOrig[3,], nrow=50, ncol=4, byrow=TRUE)
     )
str(myData)
myData[,1:3] <- myData[,1:3] + rnorm(3*150)*0.3
myData

maxc <- 10

require(e1071)
    validity <- rep(NA, maxc)
    all.cluster.rer <- list()
    for(centers in 2:maxc){
        cluster.rer<-cmeans(x=myData, centers=centers, method="cmeans", m=2)
        validity[centers] <- validityIndex(cluster.rer , myData)
        all.cluster.rer[[centers]] <- cluster.rer
    }

  plot(validity, type="l")
 
}
\keyword{ utilities }
