\name{ArticleSearch}
\alias{ArticleSearch}
\title{
Search articles from web
}
\description{
Search articles by specifying a list of key terms and a journal database.
}
\usage{
ArticleSearch(keywords, webSite = "ScienceDirect", maxSize,
  saveCSV = FALSE, exportName = "BibDataOnline")
}
\arguments{
  \item{keywords}{
a vector containing the key terms to search.
}
  \item{webSite}{
a string specifying the website where you want to search the articles. The default website is "ScienceDirect".
}
  \item{maxSize}{
the max number of article you want to get.
}
  \item{saveCSV}{
logical. If \code{TRUE}, a \code{CSV} file will be saved with the downloaded information from the bibliographic database.
}
  \item{exportName}{
a character string naming the \code{CSV} file that is going to be saved.
}
}
\details{
The amount of time spent in the execution of this function depends on the internet connection and the number of articles found and the number of articles to download.
}
\value{
A dataframe object containing the information obtained from the search of articles.
}
\author{
Andres Palacios \email{anfpalacioscl@unal.edu.co}
}
\note{
Currently, SciencDirect.com is the only website from which articles information can be downloaded.
}
\examples{
myBibData <- ArticleSearch(c("knowledge domain visualization"),
  webSite = "ScienceDirect", maxSize = 7, saveCSV = FALSE)

head(myBibData, 3)
}
\keyword{ articles }
\keyword{ web scrapping }