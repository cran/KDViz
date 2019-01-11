BibToCorpus <-
function(bibData, bibUnits = "Keywords", controlList, stopWords = TRUE, wordsToRemove, replaceWords) {
  cat("Processing Corpus from bibliometric data...\n\n"); flush.console()
  
  dataCorpus <- data.frame(doc_id = rownames(bibData), text = bibData[, colnames(bibData) %in% bibUnits])
  
  if(missing(stopWords)) {
    stopWords = FALSE
  }
  
  corpusTime <- Sys.time()
  
  corpus <- tm::VCorpus(tm::DataframeSource(dataCorpus))
  
  if("stripWhitespace" %in% controlList) {
    cat("Collapsing multiple whitespace characters to a single one...\n"); flush.console()
    
    corpus <- tm::tm_map(corpus, tm::stripWhitespace)
  }
  
  corpus <- tm::tm_map(corpus, tm::content_transformer(
    function(x) stringr::str_replace_all(pattern = "[-]", replacement = "_", x)
  ))
  
  corpus <- tm::tm_map(corpus, tm::content_transformer(
    function(x) stringr::str_replace_all(pattern = "[^[:alnum:][_]]", replacement = " ", x)
  ))
  
  corpus <- tm::tm_map(corpus, tm::content_transformer(tolower))
  
  corpus <- tm::tm_map(corpus, tm::PlainTextDocument)
  
  if(stopWords == TRUE) {
    cat("Removing stop words...\n"); flush.console()
    
    # Kevin Bouge stop words list (https://sites.google.com/site/kevinbouge/stopwords-lists)
    kbStopWords <- as.character(read.delim(system.file("extdata", "stopwords_en.txt", package = "KDViz"), header = FALSE)[,1])
    
    corpus <- tm::tm_map(corpus, tm::removeWords, kbStopWords)
  }
  
  if(!missing(wordsToRemove)) {
    cat("Removing words from custom list...\n"); flush.console()
    corpus <- tm::tm_map(corpus, tm::removeWords, wordsToRemove)
  }
  
  if("removeNumbers" %in% controlList) {
    cat("Removing numbers...\n"); flush.console()
    
    corpus <- tm::tm_map(corpus, tm::removeNumbers)
  }
  
  corpus <- tm::tm_map(corpus, tm::content_transformer(
    function(x) stringr::str_replace_all(pattern = "[_]+", replacement = "_", x)
  ))
  
  if(!missing(replaceWords)) {
    corpus <- ReplaceByList(corpus, wordsFile = replaceWords)
  }
  
  corpusTime <- Sys.time() - corpusTime
  cat("\nCorpus process finished\n\n"); flush.console()
  
  return(corpus)
}
