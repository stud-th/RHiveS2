# non-exported function copied from RJDBC package https://github.com/s-u/RJDBC
.sql.qescape <- function(s, identifier=FALSE, quote="\"") {
  s <- as.character(s)
  if (identifier) {
    vid <- grep("^[A-Za-z]+([A-Za-z0-9_]*)$",s)
    if (length(s[-vid])) {
      if (is.na(quote)) stop("The JDBC connection doesn't support quoted identifiers, but table/column name contains characters that must be quoted (",paste(s[-vid],collapse=','),")")
      s[-vid] <- .sql.qescape(s[-vid], FALSE, quote)
    }
    return(s)
  }
  if (is.na(quote)) quote <- ''
  s <- gsub("\\\\","\\\\\\\\",s)
  if (nchar(quote)) s <- gsub(paste("\\",quote,sep=''),paste("\\\\\\",quote,sep=''),s,perl=TRUE)
  paste(quote,s,quote,sep='')
}
