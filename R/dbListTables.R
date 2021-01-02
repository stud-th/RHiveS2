#' when specifying schema all queries that follow will be executed on that schema (warning message)
#' @include HiveS2Connection.R
#' @export
setMethod("dbListTables", "HiveS2Connection", function(conn, pattern=NULL, schema=NULL, ...) {
  if (!is.null(pattern)) {
    statement <- paste('SHOW TABLES LIKE', dbQuoteString(conn, pattern))
  }
  else statement <- "show tables"
  if(!(is.null(schema))){
    d <- dbGetQuery(conn, paste("use ",schema))
    warning(paste("switched to a  schema: ",schema))
  }
  d <- dbGetQuery(conn, statement)
  tnames <- d["tableName"]
  if(length(t(tnames))){
  as.vector(t(tnames))
  } else {
    character(0)
  }
})
