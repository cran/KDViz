KDSummary <-
function(matrix.mpa, groupSize = 10, graph = TRUE) {
  mpa <- mpa::mpa(matrix.mpa$MatrixA, groupSize, matrix.mpa$Words)
  
  clustTable <- data.frame("Cluster" = mpa$Nombres, "Size" = mpa$Resumen[, 2], "Density" = mpa$Resumen[, 3], "Centrality" = mpa$Resumen[, 4])
  if(graph) {
    mpa::diagram.mpa(mpa)
  }
  
  wordClasses <- data.frame("Word" = matrix.mpa$Words, "Class" = mpa$Clases)
  wordClasses$Word <- stringr::str_replace_all(pattern = " ", replacement = "_", wordClasses$Word)
  
  KDSummary <- list(clustTable = clustTable, wordClasses = wordClasses)
  
  return(KDSummary)
}