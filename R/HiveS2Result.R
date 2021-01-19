#' @include HiveS2Connection.R
#' @export
setClass(
  "HiveS2Result",
  contains = "JDBCResult",
  slots = list(
    connection = "HiveS2Connection"
  )
)
