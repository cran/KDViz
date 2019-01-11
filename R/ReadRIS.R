ReadRIS <-
function(risFile, saveCSV = FALSE, exportName) {
  if(missing(exportName)) {
    exportName = "myBibData"
  }
  GetRISElement <- function(x, pattern, replacement = "", collapse = ";") {
    text <- gsub(x[grep(pattern, x)], pattern = pattern, replacement = replacement)
    if(length(text) > 1) {
      text <- paste0(text, collapse = collapse)
    }
    if(length(text) == 0) {
      text <- c("")
    }
    
    return(text)
  }
  GetRISList <- function(bibUnit, pattern, replacement = "", collapse = ";") {
    sapply(bibUnit, function(x) GetRISElement(x, pattern = pattern, replacement = replacement, collapse = collapse))
  }
  risData <- readLines(risFile, encoding = "UTF-8")
  
  artFrom <- grep("TY  - ", risData)
  artTo <- grep("ER  - ", risData)
  
  artData <- list()
  repeat {
    if(length(artData) == length(artFrom)) {
      break
    }
    artData[[length(artData)+1]] <- risData[artFrom[length(artData)+1]:artTo[length(artData)+1]]
  }
  
  artData <- lapply(artData, function(x) x[x != c("")])
  
  bibData <- data.frame(
    "Title" = GetRISList(artData, pattern = "T1  - "),
    "Abstract" = GetRISList(artData, pattern="AB  - "),
    "Keywords" = gsub(" ", "-", gsub(" ;", ";", gsub("; ", ";", GetRISList(artData, pattern = "KW  - ")))),
    "Authors" = GetRISList(artData, pattern = "AU  - "),
    "Journal" = GetRISList(artData, pattern = "JO  - "),
    "Volume" = GetRISList(artData, pattern = "VL  - "),
    "Issue" = GetRISList(artData, pattern = "IS  - "),
    "StartPage" = GetRISList(artData, pattern = "SP  - "),
    "EndPage" = GetRISList(artData, pattern = "EP  - "),
    "PubYear" = GetRISList(artData, pattern = "PY  - "),
    "ISBN.ISSN" = GetRISList(artData, pattern = "SN  - "),
    "DOI" = GetRISList(artData, pattern="DO  - "),
    "URL" = GetRISList(artData, pattern="UR  - "),
    stringsAsFactors = FALSE
  )
  
  bibData[bibData$Abstract == "Abstract", ]$Abstract <- c("")
  
  row.names(bibData) <- paste0("A", 1:nrow(bibData))
  
  if(saveCSV) {
    write.table(bibData, file = paste0(exportName, ".csv"))
  }
  
  return(bibData)
}
