LoadArticle <-
function(articleData, articleName, browser = TRUE) {
  cat("Showing article ", articleName, ":\n\n", sep = "")
  index <- which(row.names(articleData) %in% articleName)
  print(as.matrix(articleData[index, ]))
  
  if (browser) {
    browseURL(url = as.character(articleData$URL[index]))
  }
}
