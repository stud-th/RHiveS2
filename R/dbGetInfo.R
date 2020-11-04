#' jdbcHiveDriver info
#' @export
setMethod("dbGetInfo", "jdbcHiveDriver", function(dbObj, ...) {
  list(
    name = "jdbcHiveDriver",
    driver.version = dbObj@jdrv@jclass,
    identifier.quote = dbObj@identifier.quote
  )
})

#' jdbcHiveS2Connection info
#' @export
setMethod("dbGetInfo", "HiveS2Connection", function(dbObj, ...) {
  list(
    dbObj,
    dbname = dbObj@schema_name,
    username = dbObj@username,
    host = dbObj@host_url
      )
})
