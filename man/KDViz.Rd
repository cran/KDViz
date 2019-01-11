\name{KDViz}
\alias{KDViz}
\title{
Knowledge Domain Visualization
}
\description{
Knowledge domain visualization using mpa co-words method as the word clustering method and network graphs with D3.js library as visualization tool.
}
\usage{
KDViz(groupDTM, group, graph = FALSE, export = TRUE)
}
\arguments{
  \item{groupDTM}{
a list of matrices from grouped terms and the documents in which they appear, obtained through \code{\link{GroupDTM}}.
}
  \item{group}{
an integer specifying the group of words that is want to be visualized.
}
  \item{graph}{
logical. If \code{TRUE}, a preview of the network graph from the chosen group will be plotted in R internal viewer.
}
  \item{export}{
logical. If \code{TRUE}, an html document will be saved withe the graph network using D3.js library.
}
}
\details{
This function returns a \code{forceNetwork} from \code{networkD3} package only if \code{graph} argument is \code{TRUE}. By default, it will export an html document with a network graph ready for navigate in a web browser.
}
\value{
If \code{graph} argument is \code{TRUE}, a \code{forceNetwork} will be returned.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\examples{
data("KDCorpus")
mpaWords <- matrix.corpus.mpa(KDCorpus, fmin = 10, cmin = 1)
myKDSummary <- KDSummary(matrix.mpa = mpaWords, groupSize = 10)
data("KDDTM")
myWordGroupDTM <- GroupDTM(dtm = KDDTM, kdSummary = myKDSummary)

KDViz(groupDTM = myWordGroupDTM, group = 1, export = TRUE)
}
\keyword{ kdviz }
\keyword{ knowledge domain visualization }