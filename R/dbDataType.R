#' @include HiveS2Connection
#' @export
setMethod("dbDataType", "HiveS2Connection", function(dbObj, obj) {
  data_type(obj)
})

data_type <- function(x) {
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
    # #TODO: map and array needs further implementation
    # list_named = "MAP",
    # list_unnamed = "ARRAY"
    stop("Unsupported type: ", typeof(x), call. = FALSE)
  )
}
