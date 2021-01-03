setMethod("dbBegin", "HiveS2Connection", function(conn) {
  TRUE
})

setMethod("dbCommit", "HiveS2Connection", function(conn) {
  FALSE
})

setMethod("dbRollback", "HiveS2Connection", function(conn) {
  TRUE
})
