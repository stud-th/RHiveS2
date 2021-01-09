source('utilities.R')

conn <- HiveS2_TestConnection()

dbSendQuery(conn, "create table foo (test int)")
test_that('dbListTables returns character(0) when no match found', {
  expect_equal(
    dbListTables(conn, pattern='no_match'),
    character(0)
  )
})
test_that('dbListTables returns list of tables when  match found', {
  expect_equal(
    dbListTables(conn, pattern='*oo'),
    "foo"
  )
})

test_that('dbListTables returns list of tables', {
  x<-length(dbListTables(conn))
  expect_gt(x, 0)
})

dbRemoveTable(conn, "foo")

test_that('dbListTables returns character(0) when empty database', {

  expect_warning(n<-length(dbListTables(conn, schema = "empty_database")))
  expect_equal(n, 0)

})


dbDisconnect(conn)
