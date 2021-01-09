#' @include helper-extr-pcgs.R
#' @importFrom dbplyr build_sql as.sql
#'
#' @export
db_save_query.HiveS2Connection <- function(con, sql, name, temporary = TRUE, ...) {
  sql <-   build_sql(
    #saves result in temporoary view as temporary table is not supported
    "CREATE ", if (temporary) sql("TEMPORARY "), "VIEW \n",
    as.sql(name), " AS ", sql,
    con = con
  )
  dbSendQuery(con, sql)
  name
}

#' @export
db_analyze.HiveS2Connection <- function(con, table, ...) {
  info <- dbGetQuery(con, build_sql("SHOW TABLES LIKE ", dbQuoteString(con, table), con = con))
  if (nrow(info) > 0 && identical(info$isTemporary, FALSE)) {
    sql<- build_sql("ANALYZE TABLE", as.sql(table), "COMPUTE STATISTICS", con = con)
    dbExecute(con, sql)
  }
}

#' @export
db_drop_table.HiveS2Connection <- function(con, table, ...) {
  sql <- build_sql(
    "DROP TABLE ", sql("IF EXISTS "), as.sql(table),
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
                   quantile = sql_quantile("PERCENTILE"),
                   median = sql_median("PERCENTILE")
    ),
    sql_translator(.parent = base_win,
                   var = win_aggregate("VARIANCE"),
                   quantile = sql_quantile("PERCENTILE", window = TRUE),
                   median = sql_median("PERCENTILE", window = TRUE)
    )
  )
}
