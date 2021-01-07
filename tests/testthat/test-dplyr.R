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

iris_tbl <- copy_to(conn, iris, "iris",  overwrite = TRUE )

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

# test_that("summarise peels off a single layer of grouping", {
#   mf1 <- copy_to(conn, tibble::tibble(x = 1, y = 1, z = 2) %>% group_by(x, y), "df3", overwrite = TRUE )
#   mf2 <- mf1 %>% summarise(n = n())
#   expect_equal(group_vars(mf2), "x")
#
#   mf3 <- mf2 %>% summarise(n = n())
#   expect_equal(group_vars(mf3), character())
# })

test_that("summarise performs partial evaluation", {
  mf1 <- copy_to(conn, tibble::tibble(x = 1), "df3", overwrite = TRUE )

  val <- 1
  mf2 <- mf1 %>% summarise(y = x == val) %>% collect()

  expect_equal(mf2$y, "true")
})

# test_that("grepl works as expected", {
#
#   regexes <- c(
#     "a|c", ".", "b", "x|z", "", "y", "e", "^", "$", "^$", "[0-9]", "[a-z]", "[b-z]"
#   )
#   verify_equivalent <- function(actual, expected) {
#     # handle an edge case for arrow-enabled Spark connection
#     for (col in colnames(df2)) {
#       expect_equivalent(
#         as.character(actual[[col]]),
#         as.character(expected[[col]])
#       )
#     }
#   }
#   for (regex in regexes) {
#     verify_equivalent(
#       df2 %>% dplyr::filter(grepl(regex, b)),
#       df2_tbl %>% dplyr::filter(grepl(regex, b)) %>% collect()
#     )
#     verify_equivalent(
#       df2 %>% dplyr::filter(grepl(regex, c)),
#       df2_tbl %>% dplyr::filter(grepl(regex, c)) %>% collect()
#     )
#   }
# })
#
# test_that("rowSums works as expected", {
#   df <- do.call(
#     tibble::tibble,
#     lapply(
#       seq(6L),
#       function(x) {
#         column <- list(runif(100))
#         names(column) <- paste0("col", x)
#         column
#       }
#     ) %>%
#       unlist(recursive = FALSE)
#   ) %>%
#     dplyr::mutate(na = NA_real_)
#   sdf <- copy_to(sc, df, overwrite = TRUE)
#   expect_row_sums_eq <- function(x, na.rm) {
#     expected <- df %>% dplyr::mutate(row_sum = rowSums(.[x], na.rm = na.rm))
#
#     expect_equivalent(
#       expected,
#       sdf %>%
#         dplyr::mutate(row_sum = rowSums(.[x], na.rm = na.rm)) %>%
#         collect()
#     )
#     expect_equivalent(
#       expected,
#       sdf %>%
#         dplyr::mutate(row_sum = rowSums(sdf[x], na.rm = na.rm)) %>%
#         collect()
#     )
#   }
#   test_cases <- list(
#     4L, 2:4, 4:2, -3L, -2:-4, c(2L, 4L),
#     "col5", c("col1", "col3", "col6"),
#     "na", c("col1", "na"), c("col1", "na", "col3", "col6"),
#     NULL
#   )
#
#   for (x in test_cases) {
#     for (na.rm in c(FALSE, TRUE)) {
#       expect_row_sums_eq(x, na.rm = na.rm)
#     }
#   }
# })
#
# test_that("weighted.mean works as expected", {
#   df <- tibble::tibble(
#     x = c(NA_real_, 3.1, 2.2, NA_real_, 3.3, 4),
#     w = c(NA_real_, 1, 0.5, 1, 0.75, NA_real_)
#   )
#   sdf <- copy_to(sc, df, overwrite = TRUE)
#
#   expect_equal(
#     sdf %>% dplyr::summarize(wm = weighted.mean(x, w)) %>% dplyr::pull(wm),
#     df %>%
#       dplyr::summarize(
#         wm = sum(w * x, na.rm = TRUE) /
#           sum(w * as.numeric(!is.na(x)), na.rm = TRUE)
#       ) %>%
#       dplyr::pull(wm)
#   )
#
#   df <- tibble::tibble(
#     x = rep(c(NA_real_, 3.1, 2.2, NA_real_, 3.3, 4), 3L),
#     w = rep(c(NA_real_, 1, 0.5, 1, 0.75, NA_real_), 3L),
#     grp = c(rep(1L, 6L), rep(2L, 6L), rep(3L, 6L))
#   )
#   sdf <- copy_to(sc, df, overwrite = TRUE)
#
#   expect_equal(
#     sdf %>% dplyr::summarize(wm = weighted.mean(x, w)) %>% dplyr::pull(wm),
#     df %>%
#       dplyr::summarize(
#         wm = sum(w * x, na.rm = TRUE) /
#           sum(w * as.numeric(!is.na(x)), na.rm = TRUE)
#       ) %>%
#       dplyr::pull(wm)
#   )
#   expect_equal(
#     sdf %>% dplyr::summarize(wm = weighted.mean(x ^ 3, w ^ 2)) %>% dplyr::pull(wm),
#     df %>%
#       dplyr::summarize(
#         wm = sum(w ^ 2 * x ^ 3, na.rm = TRUE) /
#           sum(w ^ 2 * as.numeric(!is.na(x)), na.rm = TRUE)
#       ) %>%
#       dplyr::pull(wm)
#   )
# })
# test_that("'left_join' does not use 'using' clause", {
#   expect_equal(
#     spark_version(sc) >= "2.0.0" && packageVersion("dplyr") < "0.5.0.90",
#     grepl(
#       "USING",
#       dbplyr::sql_render(left_join(df1_tbl, df2_tbl))
#     )
#   )
# })
#
# test_that("the implementation of 'left_join' functions as expected", {
#
#   expect_equivalent(
#     left_join(df1, df2) %>% dplyr::arrange(b),
#     left_join(df1_tbl, df2_tbl) %>% dplyr::arrange(b) %>% collect()
#   )
# })

