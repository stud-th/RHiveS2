source('utilities.R')
conn <- HiveS2_TestConnection()

test_that("Test Connection is not null", {
  expect_true(!is.null(conn))
})
test_that("querying closed connection throws error", {
  dbDisconnect(conn)
  expect_error(
    dbGetQuery(conn, "select * from foo"),
    "java.sql.SQLException: Can't create Statement, connection is closed",
    fixed = TRUE
  )
})
