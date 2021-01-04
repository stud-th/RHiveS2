#'@import dplyr db_save_query
#' @export
db_save_query.HiveS2Connection <- function(con, sql, name, temporary = TRUE, ...) {
  sql <- sql_query_save(con, sql, name, temporary = temporary, ...)
  dbSendQuery(con, sql)
  name
}

sql_query_save<- function(con, sql, name, temporary = TRUE, ...) {
  build_sql(
    "CREATE ", if (temporary) sql("TEMPORARY "), "VIEW \n",
    as.sql(name), " AS ", sql,
    con = con
  )
}

#' @export
#' @importFrom dplyr db_analyze
#' @importFrom dbplyr build_sql
#' @importFrom DBI dbExecute
db_analyze.HiveS2Connection <- function(con, table, ...) {
  info <- dbGetQuery(con, build_sql("SHOW TABLES LIKE ", dbQuoteString(con, table), con = con))
  if (nrow(info) > 0 && identical(info$isTemporary, FALSE)) {
    sql<- build_sql("ANALYZE TABLE", as.sql(table), "COMPUTE STATISTICS", con = con)
    dbExecute(con, sql)
  }
}
