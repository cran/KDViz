ReplaceByList <-
function(corpus, wordsFile) {
  
  termFreq <- TermFrequency(corpus)
  
  wordSet <- read.delim(wordsFile, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  colnames(wordSet) <- c("Replace", "Word")
  wordSubset <- wordSet[which(wordSet$Word %in% termFreq$Term), ]
  
  replaceTime <- Sys.time()
  
  nWords <- dim(wordSubset)[1]
  cat(nWords,"words to replace:\n"); flush.console()
  for(l in 1:nWords) {
    corpus <- tm::tm_map(corpus, tm::content_transformer(
      function(x) gsub(pattern = paste0("\\b", wordSubset[l, 2], "\\b"), replacement = wordSubset[l, 1], x)
    ))
    cat(" ", round((l/nWords)*100, 1), "% of words replaced\r", sep = ""); flush.console()
  }
  
  replaceTime <- Sys.time() - replaceTime
  
  return(corpus)
}
