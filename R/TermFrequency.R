TermFrequency <-
function(x) {
  if (sum(class(x)%in%c("VCorpus", "Corpus")) > 0) {
    dtm <- tm::DocumentTermMatrix(x, control = list(weighting = tm::weightBin))
  } else if (sum(class(x) %in% c("DocumentTermMatrix", "simple_triplet_matrix")) > 0) {
    dtm <- tm::weightBin( x )
  }
  dtm <- as.matrix(dtm)
  
  termCount <- sort(colSums(dtm), decreasing = TRUE)
  termFreq <- data.frame(
    "Term" = names( termCount ),
    "Freq" = termCount
  )
  rownames(termFreq) <- 1:dim(termFreq)[1]
  
  return(termFreq)
}
