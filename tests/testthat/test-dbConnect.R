source('utilities.R')
test_that("querying closed connection throws error", {
  conn <- HiveS2_TestConnection()
  dbDisconnect(conn)
  expect_error(
    dbGetQuery(conn, "select * from foo"),
    "java.sql.SQLException: Can't create Statement, connection is closed",
    fixed = TRUE
  )
  conn <- HiveS2_TestConnection()

})
