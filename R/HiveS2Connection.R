#' HiveS2Connection class connection class.
#' inherits from JDBCConnection (RJDBC)
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

setMethod("dbDisconnect", "HiveS2Connection", getMethod("dbDisconnect", "JDBCConnection"))

#' function HiveS2 copied from RJDBC
#' create jdbcHiveDriver dbObj
#' @export
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


#' #' @export
#' setMethod("dbDataType", "HiveS2Connection", function(conn, obj, ...) {
#'   switch_type(obj,
#'               factor = "STRING",
#'               datetime = "TIMESTAMP",
#'               date = "DATE",
#'               binary = "BINARY",
#'               integer = "INT",
#'               double = "DOUBLE",
#'               character = "STRING",
#'               logical = "BOOLEAN",
#'               list = "STRING",
#'               time = ,
#'               stop("Unsupported type", call. = FALSE)
#'   )
#' })


#' method "dbQuoteIdentifier" based on RSQLite
#' @export
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
setMethod("dbQuoteIdentifier", c("HiveS2Connection", "SQL"), function(conn, x, ...) {
  x
})

#' jdbcHiveS2Connection info
#' @export
setMethod("dbGetInfo", "HiveS2Connection", function(dbObj, ...) {
            list(
              dbObj,
              dbname = dbObj@schema_name,
              username = dbObj@username,
              host = dbObj@host,
              port = dbObj@port
            )
})

#' @rdname HiveS2Connection-class
#' @export
setMethod("show",  "HiveS2Connection",  function(dbObj,...) {
            cat(
              "<HiveS2Connection: ", dbObj@host, ":", dbObj@port, ">\n",
              "Schema: ", dbObj@schema_name, "\n",
              "User: ", dbObj@username, "\n",
              "Source: ", dbObj@source, "\n",
              sep=""
            )
            parameters <- dbObj@session$parameters()
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
