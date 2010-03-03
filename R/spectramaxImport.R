parseSpectramaxText <- function(txt) {
  body <- grep("^\tTemperature",txt) + c(1,-2)
  sp  = strsplit(txt[seq(from=body[1], to=body[2])], "\t")
  rawMatrix <- do.call("rbind", sp)
  valueMatrix <- rawMatrix[,-c(1,2,ncol(rawMatrix))]
  ## comma
  if (length(grep(",", valueMatrix)) > 1) {
    valueMatrix <- gsub(",",".", valueMatrix)
  }
  nrow <- nrow(valueMatrix)
  ncol <- ncol(valueMatrix)
  
  cns <- sprintf("%02d", 1:ncol)
  rns <- LETTERS[1:nrow]

  ## by column
  well <- paste(rns, rep(cns,each=length(rns)),sep="")
  val <- as.numeric(valueMatrix)

  ## order to by row
  wellOrder <- order(well)
  well <- well[wellOrder]
  val <- val[wellOrder]
  
  out <- list(data.frame(well = I(well),
                         val = val),
              txt = I(txt))
  return(out)
}


spectramaxImport <- function(file, encoding="latin1") {
  txt <- lapply(file, readLines, encoding=encoding)
  outs <- lapply(txt, parseSpectramaxText)
  if(length(file)==1) {
    outs <- outs[[1]]
  }
  return(outs)
}
