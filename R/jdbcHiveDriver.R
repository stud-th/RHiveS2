#' Driver for HiveServer2
#' @import methods
#' @import RJDBC
#' @keywords internal
#' @export
setClass("jdbcHiveDriver",
         contains = "DBIDriver",
         slots = c(identifier.quote="character",
                   jdrv="jobjRef"))
#' @export
#' @rdname HiveS2-class
setMethod("dbUnloadDriver", "jdbcHiveDriver", function(drv, ...) {
  FALSE
})

#' @param drv An object created by \code{HiveS2()}
#' method "dbConnect" copied from RJDBC with JDBCConnection replaced with jdbcHiveDriver
#' @export
setMethod("dbConnect", "jdbcHiveDriver",
          function(
            drv,
            url,
            schema=NULL,
            user='',
            password='',
            ...
          )
  {
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
  new("HiveS2Connection", jc=jc,
      identifier.quote=drv@identifier.quote,
      host_url = url,
      schema_name = schema,
      username = as.character(user))},
valueClass="HiveS2Connection")

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

#' jdbcHiveDriver info
#' @export
setMethod("dbGetInfo", "jdbcHiveDriver", function(dbObj, ...) {
  list(
    name = "jdbcHiveDriver",
    driver.version = dbObj@jdrv@jclass,
    identifier.quote = dbObj@identifier.quote
  )
})

