#' Driver for HiveServer2
#' @import methods
#' @import RJDBC
#' @keywords internal
#' jdbcHiveDriver driver class.
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



#' jdbcHiveDriver info
#' @export
setMethod("dbGetInfo", "jdbcHiveDriver", function(dbObj, ...) {
  list(
    name = "jdbcHiveDriver",
    driver.version = dbObj@jdrv@jclass,
    identifier.quote = dbObj@identifier.quote
  )
})
