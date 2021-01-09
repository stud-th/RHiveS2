source('utilities.R')

conn <- HiveS2_TestConnection()
plant_tbl <- copy_to(conn, PlantGrowth, "PlantGrowth",  overwrite = TRUE )

test_that("dbGetQuery works with parameterized queries", {
  boeing_param <- dbGetQuery(conn, "SELECT * FROM PlantGrowth WHERE group = ?", "ctrl")
  boeing <- dbGetQuery(conn, "select count(*) from (SELECT * FROM PlantGrowth WHERE group = 'ctrl')")
  expect_equal(nrow(boeing_param), boeing[1,1])
})
