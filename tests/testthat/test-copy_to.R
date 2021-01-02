source('/Users/zukow/Documents/GitHub/RHiveS2/tests/testthat/utilities.R')

test_that("dbWriteTable not implemented yet", {
  name = "iris"
  data = iris
  repartition = 0L
  sc <- HiveS2_TestConnection()
  expect_error(
    tbl <- dplyr::copy_to(sc, data, name = name, repartition = repartition),
    "dbWriteTable not implemented.",
    fixed = TRUE
  )
})






