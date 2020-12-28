#sparklyr/R/dbi_spark_transactions.R
setMethod("dbBegin", "HiveS2Connection", function(conn) {
  TRUE
})

setMethod("dbCommit", "HiveS2Connection", function(conn) {
  TRUE
})

setMethod("dbRollback", "HiveS2Connection", function(conn) {
  TRUE
})
