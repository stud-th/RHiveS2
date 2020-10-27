#  to be inproved, more fields should be added?
#' jdbcHiveDriver info
#' @export
setMethod("dbGetInfo", "jdbcHiveDriver", function(dbObj, ...) {
  list(
    name = "jdbcHiveDriver",
    driver.version = dbObj@jdrv@jclass,
    identifier.quote = dbObj@identifier.quote
  )
})
