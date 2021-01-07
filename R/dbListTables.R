#' when specifying schema all queries that follow will be executed on that schema (warning message)
#' @include HiveS2Connection.R
#' @rdname HiveS2Connection-class
#' @param pattern - specify pattern for SQL 'SHOW TABLES LIKE' statment
#' @param schema - database which tables will be listed from
#' @export
setMethod("dbListTables", "HiveS2Connection", function(conn, pattern=NULL, schema=NULL, ...) {
  if (!is.null(pattern)) {
    if(!is.character(pattern))noquote(pattern)
    statement <- paste('SHOW TABLES LIKE', dbQuoteString(conn, paste(pattern)))
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
