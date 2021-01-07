#' HiveS2Connection class connection class.
#' inherits from JDBCConnection (RJDBC)
#' @import rJava  dplyr DBI
#'
#' @export
#' @keywords internal
setClass("HiveS2Connection",
         contains = "JDBCConnection",
         slots = list(
           host = "character",
           port = "character",
           schema_name = "character",
           username = "character"
         ))

#' function HiveS2 copied from RJDBC
#' create jdbcHiveDriver dbObj
#' @export
#' @rdname HiveS2Connection-class
HiveS2 <- function(driverClass='', classPath='', identifier.quote=NA) {
  classPath <- path.expand(unlist(strsplit(classPath, .Platform$path.sep)))
  .jinit(classPath) ## this is benign in that it's equivalent to .jaddClassPath if a JVM is running
  .jaddClassPath(system.file("java", "RJDBC.jar", package="RJDBC"))
  if (nchar(driverClass) && is.jnull(.jfindClass(as.character(driverClass)[1])))
    stop("Cannot find jdbc.HiveDriver driver class ",driverClass)
  jdrv <- .jnew(driverClass, check=FALSE)
  .jcheck(TRUE)
  if (is.jnull(jdrv)) jdrv <- .jnull()
  new("jdbcHiveDriver", identifier.quote=as.character(identifier.quote), jdrv=jdrv)
}


#' @export
#' @rdname HiveS2Connection-class
setMethod("dbQuoteIdentifier", c("HiveS2Connection", "character"), function(conn, x, ...) {
  if (any(is.na(x))) {
    stop("Cannot pass NA to dbQuoteIdentifier()", call. = FALSE)
  }
  x <- gsub(conn@identifier.quote, paste0(conn@identifier.quote,conn@identifier.quote), enc2utf8(x))
  if (length(x) == 0L) {
    SQL(character(), names = names(x))
  } else {
    SQL(paste(conn@identifier.quote, x, conn@identifier.quote, sep = ""), names = names(x))
  }
})


#' @export
#' @rdname HiveS2Connection-class
setMethod("dbQuoteIdentifier", signature("HiveS2Connection", "SQL"), function(conn, x, ...) {
  x
})

#' jdbcHiveS2Connection info
#' @export
#' @rdname HiveS2Connection-class
setMethod("dbGetInfo", "HiveS2Connection", function(dbObj, ...) {
    list(
      dbObj,
      dbname = dbObj@schema_name,
      username = dbObj@username,
      host = dbObj@host,
      port = dbObj@port
    )
})


#' @export
#' @rdname HiveS2Connection-class
setMethod("show",  "HiveS2Connection",  function(object) {
  cat(
    "<HiveS2Connection: ", object@host, ":", object@port, ">\n",
    "Schema: ", object@schema_name, "\n",
    "User: ", object@username, "\n",
    sep=""
  )
})

#' TODO: dbGetTables not working
#' @export
setMethod("dbGetTables", "HiveS2Connection", function(conn, table="%", schema=conn@schema_name, ...) {
  FALSE
})
#' TODO: dbGetFields not working
#' @export
setMethod("dbGetFields", "HiveS2Connection", function(conn, table="%", schema=conn@schema_name, ...) {
  FALSE
})

setMethod("dbRemoveTable", "HiveS2Connection", def=function(conn, name, silent=FALSE, ...)
  if (silent) tryCatch(dbRemoveTable(conn, name, silent=FALSE), error=function(e) FALSE) else dbSendUpdate(conn, paste("DROP TABLE", name)))

#' @export
#' @rdname HiveS2Connection-class
setMethod("dbDisconnect", "HiveS2Connection", function(conn, ...){
  .jcall(conn@jc, "V", "close")
  invisible(TRUE)
})

#' @export
#' @rdname HiveS2Connection-class
setMethod("dbWriteTable", "HiveS2Connection", def=function(conn, name, value, overwrite=FALSE, append=FALSE, force=FALSE, ...) {
#  ac <- .jcall(conn@jc, "Z", "getAutoCommit")
  overwrite <- isTRUE(as.logical(overwrite))
  append <- isTRUE(as.logical(append))
  if (overwrite && append) stop("overwrite=TRUE and append=TRUE are mutually exclusive")
  if (is.vector(value) && !is.list(value)) value <- data.frame(x=value)
  if (length(value)<1) stop("value must have at least one column")
  if (is.null(names(value))) names(value) <- paste("V",1:length(value),sep='')
  if (length(value[[1]])>0) {
    if (!is.data.frame(value)) value <- as.data.frame(value, row.names=1:length(value[[1]]))
  } else {
    if (!is.data.frame(value)) value <- as.data.frame(value)
  }
  fts <- sapply(value, dbDataType, dbObj=conn)
  if (isTRUE(as.logical(force))) {
    if (overwrite) dbRemoveTable(conn, name, silent=TRUE)
  } else if (dbExistsTable(conn, name)) {
    if (overwrite) dbRemoveTable(conn, name)
    else if (!append) stop("Table `",name,"' already exists")
  } else append <- FALSE ## if the table doesn't exist, append has no meaning
  fdef <- paste(.sql.qescape(names(value), TRUE, conn@identifier.quote),fts,collapse=',')
  qname <- .sql.qescape(name, TRUE, conn@identifier.quote)
  if (!append) {
    sql_format <-
      "ROW FORMAT SERDE 'org.apache.hadoop.hive.serde2.OpenCSVSerde'
    WITH SERDEPROPERTIES (
      \"separatorChar\" = \";\",
      \"quoteChar\"     = \"\\\"\")
    STORED AS TEXTFILE"
    ct <- paste("CREATE TABLE ",qname," (",fdef,") ",sql_format,sep= '')
    dbSendQuery(conn, ct)
  }
  if (nrow(value) == 0)  invisible(TRUE)
#fwrite
  ## Save file to disk, then use LOAD DATA command
  fn <- normalizePath(tempfile("rsdbi"), winslash = "/", mustWork = FALSE)
  write.table(value, file = fn, sep=";", col.names=FALSE, row.names=FALSE)
  on.exit(unlink(fn), add = TRUE)

  sql <- paste0(
    "LOAD DATA INPATH ", dbQuoteString(conn, fn),
    "  OVERWRITE INTO TABLE ", dbQuoteIdentifier(conn, name)
  )
  dbSendQuery(conn, sql)
  invisible(TRUE)
})

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
