#' @export
db_save_query.HiveS2Connection <- function(con, sql, name, temporary = TRUE, ...) {
  sql <- sql_query_save(con, sql, name, temporary = temporary, ...)
  dbSendQuery(con, sql)
  name
}

sql_query_save<- function(con, sql, name, temporary = TRUE, ...) {
  dbplyr::build_sql(
    "CREATE ", if (temporary) sql("TEMPORARY "), "VIEW \n",
    dbplyr::as.sql(name), " AS ", sql,
    con = con
  )
}

#' @export
db_analyze.HiveS2Connection <- function(con, table, ...) {
  info <- dbGetQuery(con, dbplyr::build_sql("SHOW TABLES LIKE ", dbQuoteString(con, table), con = con))
  if (nrow(info) > 0 && identical(info$isTemporary, FALSE)) {
    sql<- dbplyr::build_sql("ANALYZE TABLE", dbplyr::as.sql(table), "COMPUTE STATISTICS", con = con)
    dbExecute(con, sql)
  }
}

#' @export
db_drop_table.HiveS2Connection <- function(con, table, ...) {
  sql <- dbplyr::build_sql(
    "DROP TABLE ", sql("IF EXISTS "), dbplyr::as.sql(table),
    con = con
  )
  dbSendQuery(con, sql)
}


#adopted from  dbplyr version 2.0.0
#' @importFrom dbplyr sql_variant sql_translator sql_prefix base_scalar base_agg base_win sql_aggregate win_aggregate sql_expr
#' @export
sql_translate_env.HiveS2Connection <- function(con) {
  sql_variant(

    sql_translator(.parent = base_scalar,
                   bitwShiftL    = sql_prefix("SHIFTLEFT", 2),
                   bitwShiftR    = sql_prefix("SHIFTRIGHT", 2),

                   cot = function(x){
                     sql_expr(1 / tan(!!x))
                   },

                   str_replace_all = function(string, pattern, replacement) {
                     sql_expr(regexp_replace(!!string, !!pattern, !!replacement))
                   }
    ),
    sql_translator(.parent = base_agg,
                   n  = function() sql("COUNT(*)"), # issue in dbplyr 1.4.4  #343
                   var = sql_aggregate("VARIANCE", "var"),
    ),
    sql_translator(.parent = base_win,
                   var = win_aggregate("VARIANCE"),
    )
  )
}
