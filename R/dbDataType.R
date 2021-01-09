#' @include HiveS2Connection.R
#' @export
setMethod("dbDataType", "HiveS2Connection", function(dbObj, obj) {
  data_type(dbObj, obj)
})

data_type <- function(dbObj,x) {
  if (is.factor(x)) return("STRING")
  if (inherits(x, "hms")) return("TIME")

  switch(
    typeof(x),
    character = "STRING",
    logical = "BOOLEAN",
    double = "FLOAT",
    raw = "BINARY",
    integer = "INTEGER",
    POSIXt ="TIMESTAMP",
    Date = "DATE",
    NULL = "varchar",
    stop("Unsupported type: ", typeof(x), call. = FALSE)
# unsupported conversion to Hive Complex Types
# ARRAY<data_type>,  MAP<primitive_type, data_type>, STRUCT...
  )}
