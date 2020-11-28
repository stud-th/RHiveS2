test_that("querying closed connection throws error", {
db <- dbConnect(HiveS2("org.apache.hive.jdbc.HiveDriver","/Users/zukow/spark-2.2.1-bin-hadoop2.7/jars/hive-jdbc-1.2.1.spark2.jar",identifier.quote='`'),
                  host ="jdbc:hive2://localhost:",
                  port = "10000",
                  schema = "so_survey_2019_hive"
  )
  dbDisconnect(db)
  expect_error(
    dbGetQuery(db, "select * from foo"),
    "Invalid or closed connection",
    fixed = TRUE
  )
})
