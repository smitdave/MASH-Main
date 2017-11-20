library(R.utils)
sourceEntireFolder=function(folderName, verbose=FALSE, showWarnings=TRUE){
  files=list.files(folderName, full.names=TRUE)
  files=files[ grepl("\\.[rR]$", files) ]
  if(!length(files) && showWarnings){warning("No R files in ", folderName)}
  for(f in files){
    if (verbose){cat("sourcing: ", f, "\n")}
    try(source(f, local=FALSE, echo=FALSE), silent=!verbose)
  }
  return(invisible(NULL))
}
