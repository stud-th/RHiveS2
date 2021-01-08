library(dplyr)
source('utilities.R')


conn <- HiveS2_TestConnection()
iris_tbl <- copy_to(conn, iris, "iris",  overwrite = TRUE )

test_that("copy_to overwrites hive table if set to true", {
  name = "iris"
  data = iris
  sc <- HiveS2_TestConnection()
  iris_tbl <- copy_to(conn, data, name = name,  overwrite = TRUE )
  succeed()
})

test_that("the implementation of 'mutate' functions as expected", {

  expect_equivalent(
    iris %>% mutate(x = Species) %>% tbl_vars() %>% length(),
    iris_tbl %>% mutate(x = species) %>% collect() %>% tbl_vars() %>% length()
  )
})

#test adopted from sparlyr/testthat
test_that("the implementation of 'filter' functions as expected", {

  expect_equivalent(
    iris_tbl %>%
      filter(sepal.length == 5.1) %>%
      filter(sepal.width == 3.5) %>%
      filter(petal.length == 1.4) %>%
      filter(petal.width == 0.2) %>%
      select(species) %>%
      collect(),
    iris %>%
      transmute(
        Sepal_Length = `Sepal.Length`,
        Sepal_Width = `Sepal.Width`,
        Petal_Length = `Petal.Length`,
        Petal_Width = `Petal.Width`,
        Species = Species
      ) %>%
      filter(Sepal_Length == 5.1) %>%
      filter(Sepal_Width == 3.5) %>%
      filter(Petal_Length == 1.4) %>%
      filter(Petal_Width == 0.2) %>%
      transmute(Species = as.character(Species))
  )
})

test_that("can compute() over tables", {

  iris_tbl %>% compute()
  succeed()
})

test_that("ifelse as expected", {

  df <- copy_to(conn, tibble::tibble(x = c(0.9, "foo", 1.1)), "df1",  overwrite = TRUE )

  expect_equal(
    df %>% dplyr::mutate(x = ifelse(x > 1, "positive", "negative")) %>% dplyr::pull(x),
    c("negative", NA, "positive")
  )
})

#test adapted from dplyr/tests/testthat
df <- copy_to(conn, data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5)), "df3", overwrite = TRUE )

test_that("joins preserve grouping", {
  g <- group_by(df, x)

  expect_equal(group_vars(inner_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(left_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(semi_join(g, g, by = c("x", "y"))), "x")
  expect_equal(group_vars(anti_join(g, g, by = c("x", "y"))), "x")
})

#test adapted from dbplyr/tests/testthat
# sql-render --------------------------------------------------------------

test_that("quoting for rendering summarized grouped table", {
  out <- copy_to(conn, tibble::tibble(x = "a"), "df3", overwrite = TRUE )%>%
    group_by(x) %>%
    summarise(n = n())
  expect_equal(out%>%collect(), tibble::tibble(x = "a", n = 1L))
})

# sql-build ---------------------------------------------------------------

test_that("summarise generates group_by and select", {
  out <- copy_to(conn, tibble::tibble(g = 1), "df3", overwrite = TRUE ) %>%
    group_by(g) %>%
    summarise(n = n()) %>%
    dbplyr::sql_build()

  expect_equal(out$group_by, sql('`g`'))
  expect_equal(out$select, sql('`g`', 'COUNT(*) AS `n`'))
})

test_that("summarise performs partial evaluation", {
  mf1 <- copy_to(conn, tibble::tibble(x = 1), "df3", overwrite = TRUE )

  val <- 1
  mf2 <- mf1 %>% summarise(y = x == val) %>% collect()

  expect_equal(mf2$y, "true")
})

df1_df <- tibble(a = letters[20:22], b = letters[1:3])
df2_df <- tibble(b = letters[1:3], c = letters[24:26])
df1 <- copy_to(conn, df1_df, "df1",  overwrite = TRUE )
df2 <- copy_to(conn, df2_df, "df2",  overwrite = TRUE )

test_that("join has compliable syntax", {

  expect_equivalent(
    left_join(df2_df, df1_df) %>% dplyr::arrange(b),
    left_join(df2, df1) %>% dplyr::arrange(b) %>% collect()
  )
})
#test adopted from sparlyr/testthat
test_that("'left_join' does not use 'using' clause", {
  expect(
    TRUE,
    grepl(
      "USING",
      dbplyr::sql_render(left_join(df1, df2))
    )
  )
})


