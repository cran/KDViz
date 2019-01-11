ArticleSearch <-
function(keywords, webSite = "ScienceDirect", maxSize, saveCSV = FALSE, exportName = "BibDataOnline") {
  if (missing(keywords)){
    stop("Keywords have not been specified")
  } else {
    if (length(keywords) == 0) {
      stop("Keywords vector can not be empty")
    }
  }
  webList <- c("ScienceDirect")
  if(webSite %in% webList) {
    if(webSite == "ScienceDirect") {
      HomeUrl <- function(searchKeywords, offset, show = 100) {
        paste0("https://www.sciencedirect.com/search?qs=",searchKeywords,"&show=",show,"&sortBy=date&offset=",offset)
      }
      searchKeywords <- paste0(gsub(pattern = " ", replacement = "%20", x = keywords), collapse = "%2C")
      show <- 100 
      if(!missing(maxSize)) {
        if(maxSize <= 25) {
          show <- 25
        }
      }
      currentSession <- xml2::read_html(HomeUrl(searchKeywords,0,show))
      
      resultsFound <- as.numeric(gsub(x = rvest::html_text(rvest::html_node(currentSession, ".search-body-results-text")), pattern = "[A-z]|\\s+|[:punct:]|[.]|[,]", replacement = ""))
      if(missing(maxSize)) {
        maxSize <- resultsFound
      }
      if(maxSize < resultsFound) {
        resultsFound <- maxSize
      }
      if(resultsFound > 999) {
        resultsFound <- 999
        cat("Only can get a maximum of 999 articles\n"); flush.console()
      }
      
      offset <- (ceiling(resultsFound/100)-1)*100
      cat("Retrieving",resultsFound,"articles...\n"); flush.console()
      
      artNodes <- rvest::html_nodes(currentSession, ".result-list-title-link")
      artTitle <- rvest::html_text(artNodes)
      artLink <- rvest::html_attr(artNodes, "href")

      if(offset > 0) {
        for (currentOffset in seq(from = 100, to = offset, by = 100)) {
          currentSession <- xml2::read_html(HomeUrl(searchKeywords,currentOffset))
          artNodes <- rvest::html_nodes(currentSession, ".result-list-title-link")
          artTitle <- c(artTitle, rvest::html_text(artNodes))
          artLink <- c(artLink, rvest::html_attr(artNodes, "href"))
        }
      }
      cat("Main information obtained. Obtaining bibliometric extra info...\n"); flush.console()
      artData <- data.frame("Title" = artTitle, "URL" = paste0("https://www.sciencedirect.com",artLink), stringsAsFactors = FALSE)
      artData <- artData[1:maxSize, ]
      
      tryCatch({
        artInfo <- NULL
        repeat{
          if (is.null(nrow(artInfo))) {
            artPage <- xml2::read_html(artData$URL[1])
          } else {
            if(nrow(artInfo) == nrow(artData)) {
              cat("\nThe entire information of", nrow(artData), "articles has been obtained\n"); flush.console()
              break
            }
            artPage <- xml2::read_html(artData$URL[nrow(artInfo)+1])
            cat("\r",(nrow(artInfo) + 1),"of",nrow(artData)); flush.console()
          }
          artAbstract <- paste0(rvest::html_text(rvest::html_children(rvest::html_nodes(artPage, ".abstract.author"))[-1]), collapse = ",")
          artKeywords <- paste0(rvest::html_text(rvest::html_children(rvest::html_nodes(artPage, ".keywords-section"))[-1]), collapse = ",")
          artAuthors <- paste0(rvest::html_text(rvest::html_children(rvest::html_nodes(artPage, ".author-group"))[-1]), collapse = ",")
          artJournal <- paste0(rvest::html_text(rvest::html_nodes(artPage, ".publication-title-link")), collapse = ",")
          artDOI <- paste0(rvest::html_attr(rvest::html_nodes(artPage, ".doi"), "href"), collapse = ",")
          
          if(length(artAbstract) == 0) artAbstract <- ""
          if(length(artKeywords) == 0) artKeywords <- ""
          if(length(artAuthors) == 0) artAuthors <- ""
          if(length(artJournal) == 0) artJournal <- ""
          if(length(artDOI) == 0) artDOI <- ""
          
          artInfo <- rbind(artInfo, c(artAbstract, artKeywords, artAuthors, artJournal, artDOI))
        }
        artData <- data.frame(cbind(artData$Title, artInfo, artData$URL), stringsAsFactors = FALSE)
        names(artData) <- c("Title", "Abstract", "Keywords", "Authors", "Journal", "DOI", "URL")
      }, error = function(e) cat("\nCouldn't finish to get the entire information"))
      
      row.names(artData) <- paste0("A",1:nrow(artData))
      
      if(saveCSV) {
        cat("Saving data in a CSV file"); flush.console()
        write.table(artData, file = paste0(exportName, ".csv"), col.names = TRUE, row.names = TRUE, sep = ",")
        cat(". Done"); flush.console()
      }
      
      return(artData)
    }
  } else {
    stop("Only 'ScienceDirect' database is available, soon there will be more...")
  }
}
