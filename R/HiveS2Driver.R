#' Driver for HiveServer2
#' @import methods
#' @import RJDBC
#' @keywords internal
#' HiveS2Driver driver class.
#' @export
setClass("HiveS2Driver",
         contains = "DBIDriver",
         slots = c(identifier.quote="character",
                   jdrv="jobjRef"))
#' @export
setMethod("dbUnloadDriver", "HiveS2Driver", function(drv, ...) {
  FALSE
})



#' HiveS2Driver info
#' @export
setMethod("dbGetInfo", "HiveS2Driver", function(dbObj, ...) {
  list(
    name = "HiveS2Driver",
    driver.version = dbObj@jdrv@jclass,
    identifier.quote = dbObj@identifier.quote
  )
})
