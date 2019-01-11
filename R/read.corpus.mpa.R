read.corpus.mpa <-
function(corpus) {
  corpusMPA <- sapply(corpus, function(x) x[1])
  corpusMPA <- corpusMPA[corpusMPA != ""]
  
  corpusMPA <- stringr::str_replace_all(pattern="\\s+", replacement=" ", corpusMPA)
  corpusMPA <- stringr::str_replace_all(pattern=" ", replacement="/", corpusMPA)
  corpusMPA <- stringr::str_replace_all(pattern="_", replacement=" ", corpusMPA)
  
  corpusMPA <- paste0(c("/ind0/"), corpusMPA, "/")
  
  return(corpusMPA)
}
