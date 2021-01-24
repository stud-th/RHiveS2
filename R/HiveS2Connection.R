#' @include helper-extr-pcgs.R
#' HiveS2Connection class connection class.
#' inherits from JDBCConnection (RJDBC)
#' @import rJava  dplyr
#' @import RJDBC DBI
#' @export
#' @keywords internal
setClass("HiveS2Connection",
         contains = "JDBCConnection",
         slots = list(
           host = "character",
           port = "character",
           schema_name = "character",
           username = "character",
           password = "character"
         ))

#' function HiveS2 copied from RJDBC
#' creates HiveS2Driver dbObj
#' @export
#' @rdname HiveS2Connection-class
HiveS2 <- function(driverClass='', classPath='', identifier.quote="`") {
  classPath <- path.expand(unlist(strsplit(classPath, .Platform$path.sep)))
  .jinit(classPath) ## this is benign in that it's equivalent to .jaddClassPath if a JVM is running
  .jaddClassPath(system.file("java", "RJDBC.jar", package="RJDBC"))
  if (nchar(driverClass) && is.jnull(.jfindClass(as.character(driverClass)[1])))
    stop("Cannot find jdbc.HiveDriver driver class ",driverClass)
  jdrv <- .jnew(driverClass, check=FALSE)
  .jcheck(TRUE)
  if (is.jnull(jdrv)) jdrv <- .jnull()
  new("HiveS2Driver", identifier.quote=as.character(identifier.quote), jdrv=jdrv)
}


#' @export
#' @import DBI
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
#' @import DBI
#' @rdname HiveS2Connection-class
setMethod("dbQuoteIdentifier", signature("HiveS2Connection", "SQL"), function(conn, x, ...) {
  x
})


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


#' @export
#' @rdname HiveS2Connection-class
setMethod("dbDisconnect", "HiveS2Connection", function(conn, ...){
  .jcall(conn@jc, "V", "close")
  invisible(TRUE)
})

#' dbWriteTable writtes table into local temporary dile and than it is laded into Hive with 'LOAD DATA' statment
#' Sceleton of the fuction adapted from RJDBC packade
#' @param overwrite allows to specify weather existing table with the same name should be removed
#' @export
#' @rdname HiveS2Connection-class
setMethod("dbWriteTable", "HiveS2Connection", def=function(conn, name, value, overwrite=FALSE, append=FALSE, force=FALSE,...) {
# getAutoCommit deleated as it's not supported
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

  ## Save file to disk, then use LOAD DATA command
  fn <- normalizePath(tempfile("rsdbi", tmpdir = "/Docker/csv-data"), winslash = "/", mustWork = FALSE)
  #fwrite used as it is  but much faster that write.csv so more suitable for Big Data
  data.table::fwrite(value, file = fn, sep=";", col.names=FALSE, row.names=FALSE)
  on.exit(unlink(fn), add = TRUE)
  fn_docker <- paste0("/files/csv-data/",basename(fn))
  fn<-fn_docker
  sql <- paste0(
    "LOAD DATA INPATH ", dbQuoteString(conn, fn),
    "  OVERWRITE INTO TABLE ", dbQuoteIdentifier(conn, name)
  )
  dbSendQuery(conn, sql)
  invisible(TRUE)
})
