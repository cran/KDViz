DTMFromCorpus <-
function(corpus, rowNames) {
  dtm <- tm::DocumentTermMatrix(corpus, control = list(weighting = tm::weightBin, removePunctuation = FALSE))
  dtm <- as.matrix(dtm)
  if(missing(rowNames)) {
    row.names(dtm) <- paste0("A", 1:nrow(dtm))
  } else {
    row.names(dtm) <- rowNames
  }
  dtm <- dtm[which(rowSums(dtm) != 0), ]
  
  return(dtm)
}
