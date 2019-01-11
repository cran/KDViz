TermFreqByWord <-
function(termFreqTable, word) {
  TFByWord <- termFreqTable[grep(word, termFreqTable$Term), ]
  
  return(TFByWord)
}
