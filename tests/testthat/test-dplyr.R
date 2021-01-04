library(dplyr)
source('utilities.R')
conn <- HiveS2_TestConnection()


iris_tbl <- tbl(conn, "iris")
test_requires("dplyr")


test_that("the implementation of 'mutate' functions as expected", {
  test_requires("dplyr")

  expect_equivalent(
    iris %>% mutate(x = Species) %>% tbl_vars() %>% length(),
    iris_tbl %>% mutate(x = species) %>% collect() %>% tbl_vars() %>% length()
  )
})

test_that("the implementation of 'filter' functions as expected", {
  test_requires("dplyr")

  expect_equivalent(
    iris_tbl %>%
      filter(sepallength == 5.1) %>%
      filter(sepalwidth == 3.5) %>%
      filter(petallength == 1.4) %>%
      filter(petalwidth == 0.2) %>%
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

# test_that("if_else works as expected", {
#   sdf <- copy_to(sc, tibble::tibble(x = c(0.9, NA_real_, 1.1)))
#
#   expect_equal(
#     sdf %>% dplyr::mutate(x = ifelse(x > 1, "good", "bad")) %>% dplyr::pull(x),
#     c("bad", NA, "good")
#   )
#   expect_equal(
#     sdf %>% dplyr::mutate(x = ifelse(x > 1, "good", "bad", "unknown")) %>%
#       dplyr::pull(x),
#     c("bad", "unknown", "good")
#   )
# })
#
# test_that("grepl works as expected", {
#   test_requires("dplyr")
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

test_that("'head' uses 'limit' clause", {
  test_requires("dplyr")
  test_requires("dbplyr")

  expect_true(
    grepl(
      "LIMIT",
      sql_render(head(iris_tbl))
    )
  )
})

# test_that("'left_join' does not use 'using' clause", {
#   test_requires("dplyr")
#   test_requires("dbplyr")
#
#   expect_equal(
#     spark_version(sc) >= "2.0.0" && packageVersion("dplyr") < "0.5.0.90",
#     grepl(
#       "USING",
#       sql_render(left_join(df1_tbl, df2_tbl))
#     )
#   )
# })
#
# test_that("the implementation of 'left_join' functions as expected", {
#   test_requires("dplyr")
#
#   expect_equivalent(
#     left_join(df1, df2) %>% dplyr::arrange(b),
#     left_join(df1_tbl, df2_tbl) %>% dplyr::arrange(b) %>% collect()
#   )
# })
#
# test_that("'sdf_broadcast' forces broadcast hash join", {
#   query_plan <- df1_tbl %>%
#     sdf_broadcast() %>%
#     left_join(df2_tbl, by = "b") %>%
#     spark_dataframe() %>%
#     invoke("queryExecution") %>%
#     invoke("analyzed") %>%
#     invoke("toString")
#   expect_match(query_plan, "B|broadcast")
# })

test_that("can compute() over tables", {
  test_requires("dplyr")

  iris_tbl %>% compute()
  succeed()
})

# test_that("mutate creates NA_real_ column correctly", {
#   sdf <- sdf_len(sc, 5L) %>% dplyr::mutate(z = NA_real_, sq = id * id)
#
#   expect_equivalent(
#     sdf %>% collect(),
#     tibble::tibble(id = seq(5), z = NA_real_, sq = id * id)
#   )
# })
#
# test_that("transmute creates NA_real_ column correctly", {
#   sdf <- sdf_len(sc, 5L) %>% dplyr::transmute(z = NA_real_, sq = id * id)
#
#   expect_equivalent(
#     sdf %>% collect(),
#     tibble::tibble(z = NA_real_, sq = seq(5) * seq(5))
#   )
# })
#
# test_that("process_tbl_name works as expected", {
#   expect_equal(sparklyr:::process_tbl_name("a"), "a")
#   expect_equal(sparklyr:::process_tbl_name("xyz"), "xyz")
#   expect_equal(sparklyr:::process_tbl_name("x.y"), dbplyr::in_schema("x", "y"))
# })