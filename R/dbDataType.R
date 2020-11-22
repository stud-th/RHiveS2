#' @include HiveS2Connection
#' @export
setMethod("dbDataType", "HiveS2Connection", function(dbObj, obj) {
  data_type(obj)
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
    # #TODO: map and array needs further implementation
    #list = list_to_map_array,
    stop("Unsupported type: ", typeof(x), call. = FALSE)
  )
}

# list_to_map_array <- function(obj){
#   if (data.class(obj) == 'list') {
#     if (length(obj) == 0) {
#       inner.type <- data_type(dbObj, NULL)
#     } else {
#       inner.types <- vapply(obj, function(x) data_type(dbObj, x), '')
#       inner.type <- inner.types[1]
#       if (!all(inner.types == inner.type)) {
#         inner.type <- NA
#       }
#     }
#     if (is.na(inner.type)) {
#       rv <- 'varchar'
#     } else {
#       if (!is.null(names(obj))) {
#         rv <- paste('map<varchar, ', inner.type, '>', sep='')
#       } else {
#         rv <- paste('array<', inner.type, '>', sep='')
#       }
#     }
#   }
#   return(stringi::stri_trans_toupper(rv, 'en_US.UTF-8'))
# }
