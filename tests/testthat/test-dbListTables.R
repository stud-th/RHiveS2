source('utilities.R')

conn <- HiveS2_TestConnection()

test_that('dbListTables returns character(0) when empty database', {
  expect_equal(length(dbListTables(conn)), 0)
})

dbSendQuery(conn, "create table foo (test int)")
test_that('dbListTables returns character(0) when no match found', {
  expect_equal(
    dbListTables(conn, pattern='no_match'),
    character(0)
  )
})
dbSendQuery(conn, "drop table foo")

test_that('dbListTables returns list of tables', {
  expect_warning(x<-length(dbListTables(conn, schema = "so_survey_2019_hive")))
  expect_gt(x, 0)
})

dbDisconnect(conn)
