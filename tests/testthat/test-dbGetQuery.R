source('utilities.R')

conn <- HiveS2_TestConnection()
#copy_to
test_that("dbGetQuery works with parameterized queries", {
  boeing_param <- dbGetQuery(conn, "SELECT * FROM nycflights13.planes WHERE manufacturer = ?", "BOEING")
  boeing <- dbGetQuery(conn, "select count(*) from (SELECT * FROM nycflights13.planes WHERE manufacturer = 'BOEING')")
  expect_equal(nrow(boeing_param), boeing[1,1])
})
