#' @import RJDBC
#'
#' @export
setClass("jdbcHiveDriver",
          contains = "DBIDriver",
         slots = c(identifier.quote="character",
                   jdrv="jobjRef"))

#' method "dbConnect" copied from RJDBC with JDBCConnection replaced with jdbcHiveDriver
#'
#' @export
setMethod("dbConnect", "jdbcHiveDriver",
  function(
    drv,
    url,
    schema=NULL,
    user='',
    password='',
    ...
    ) {
  jc <- .jcall("java/sql/DriverManager","Ljava/sql/Connection;","getConnection", as.character(paste(url,"/",schema,sep = ""))[1], as.character(user)[1], as.character(password)[1], check=FALSE)
  if (is.jnull(jc) && !is.jnull(drv@jdrv)) {
    oex <- .jgetEx(TRUE)
    p <- .jnew("java/util/Properties")
    if (length(user)==1 && nzchar(user)) .jcall(p, "Ljava/lang/Object;", "setProperty", "user", as.character(user))
    if (length(password)==1 && nzchar(password)) .jcall(p, "Ljava/lang/Object;", "setProperty", "password", as.character(password))
    l <- list(...)
    if (length(names(l))) for (n in names(l)) .jcall(p, "Ljava/lang/Object;", "setProperty", n, as.character(l[[n]]))
    jc <- .jcall(drv@jdrv, "Ljava/sql/Connection;", "connect", as.character(as.character(paste(url,"/",schema,sep = "")))[1], p, check=FALSE)
    print(as.character(paste(url,"/",schema,sep = "")))
  }
  .verify.JDBC.result(jc, "Unable to connect JDBC to ",url)
  new("HiveS2Connection", jc=jc, identifier.quote=drv@identifier.quote)},
  valueClass="HiveS2Connection")

#' function HiveS2 copied from RJDBC
#' create jdbcHiveDriver object
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
#' .verify.JDBC.result copied from RJDBC
.verify.JDBC.result <- function (result, ...) {
  if (is.jnull(result)) {
    x <- .jgetEx(TRUE)
    if (is.jnull(x))
      stop(...)
    else
      stop(...," (",.jcall(x, "S", "getMessage"),")")
  }
}


#' HiveS2Connection class inherits from JDBCConnection (RJDBC)
#' @rdname HiveS2Connection-class
#' @export
setClass("HiveS2Connection",
         contains = "JDBCConnection")

#' method "dbQuoteIdentifier" based on RSQLite
#' @export
setMethod("dbQuoteIdentifier", c("HiveS2Connection", "character"), function(conn, x, ...) {
  if (any(is.na(x))) {
    stop("Cannot pass NA to dbQuoteIdentifier()", call. = FALSE)
  }
  x <- gsub("`", "``", enc2utf8(x))
  if (length(x) == 0L) {
    SQL(character(), names = names(x))
  } else {
    SQL(paste("`", x, "`", sep = ""), names = names(x))
  }
})

#' @export
setMethod("dbQuoteIdentifier", c("HiveS2Connection", "SQL"), function(conn, x, ...) {
  x
})




