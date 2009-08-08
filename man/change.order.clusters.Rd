\name{change.order.clusters}
\alias{change.order.clusters}
\title{ Change numbering of clusters }
\description{
   Changes the cluster numbering in an fuzzy clustering object.
}
\usage{
change.order.clusters(clustering, new.order)
}
\arguments{
  \item{clustering}{ Object returned from \code{\link[e1071]{cmeans}} }
  \item{new.order}{ Vector with new cluster numbering.}
}
\details{
   Cluster 1 from the old object is assigned the number stored in the frist position in new.order, Cluster 2 the number on the second position and so on.
}
\value{
   Identical object as \code{clustering} except the cluster numbering is changed
}
\references{ Reusser, D. E., Blume, T., Schaefli, B., and Zehe, E.: Analysing the temporal dynamics of model performance for hydrological models, Hydrol. Earth Syst. Sci. Discuss., 5, 3169-3211, 2008. }
\author{ Dominik Reusser }
\seealso{\code{\link[e1071]{cmeans}} for the fuzzy clustering }
\examples{
data(tiger.example)

new.order <- c(6,3,2,5,4,1)

cmeans.result <- tiger.single$cluster.assignment[[6]]
str(cmeans.result)
cmeans.result2 <- change.order.clusters(cmeans.result, new.order)
}
\keyword{ utilities }
