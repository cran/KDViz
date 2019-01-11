matrix.corpus.mpa <-
function(corpus, fmin = 3, cmin = 3) {
  corpusMPA <- read.corpus.mpa(corpus)
  
  mpaWords <- mpa::matriz.mpa(corpusMPA, sep.ind="ind0", sep.pal="/", fmin = fmin, cmin = cmin)
  names(mpaWords) <- c("MatrixA","MatrixC","Words","lt")
  
  return(mpaWords)
}