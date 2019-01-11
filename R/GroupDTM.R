GroupDTM <-
function(dtm, kdSummary) {
  groupDTM <- list()
  wordClasses <- kdSummary$wordClasses
  for(class in sort(unique(wordClasses$Class))) {
    if(class != 0) {
      #cat('Class ',class,'\n'); flush.console()
      wordGroup <- wordClasses[which(wordClasses[, 2] == class), 1]
      if(length(wordGroup) != 0) {
        dtmGroup <- as.data.frame(dtm[, colnames(dtm) %in% wordGroup])
        dtmGroup <- dtmGroup[which(rowSums(dtmGroup) != 0), ]
        groupDTM[[class]] <- dtmGroup
        #cat(' Dimensions:',dim(dtmGroup),'\n'); flush.console()
      }
    }
  }
  return(groupDTM)
}
