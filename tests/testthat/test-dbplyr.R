source('utilities.R')
conn <- HiveS2_TestConnection()

#basic sql syntax tests adapted from dbplyr/tests/testthat/test-backend-.R
test_that("basic arithmetic is correct", {
  expect_equal(dbplyr::translate_sql(1 + 2), sql("1.0 + 2.0"))
  expect_equal(dbplyr::translate_sql(2 * 4), sql("2.0 * 4.0"))
  expect_equal(dbplyr::translate_sql(5 ^ 2), sql("POWER(5.0, 2.0)"))
  expect_equal(dbplyr::translate_sql(100L %% 3L), sql("100 % 3"))
})

test_that("small numbers aren't converted to 0", {
  expect_equal(dbplyr::translate_sql(1e-9), sql("1e-09"))
})

test_that("unary minus flips sign of number", {
  expect_equal(dbplyr::translate_sql(-10L), sql("-10"))
  expect_equal(dbplyr::translate_sql(x == -10), sql('`x` = -10.0'))
  expect_equal(dbplyr::translate_sql(x %in% c(-1L, 0L)), sql('`x` IN (-1, 0)'))
})

test_that("unary minus wraps non-numeric expressions", {
  expect_equal(dbplyr::translate_sql(-(1L + 2L)), sql("-(1 + 2)"))
  expect_equal(dbplyr::translate_sql(-mean(x, na.rm = TRUE), window = FALSE), sql('-AVG(`x`)'))
})

test_that("binary minus subtracts", {
  expect_equal(dbplyr::translate_sql(1L - 10L), sql("1 - 10"))
})

test_that("log base comes first", {
  expect_equal(dbplyr::translate_sql(log(x, 10)), sql('LOG(10.0, `x`)'))
})

test_that("log becomes ln", {
  expect_equal(dbplyr::translate_sql(log(x)), sql('LN(`x`)'))
})

test_that("can translate subsetting", {
  expect_equal(dbplyr::translate_sql(a$b), sql("`a`.`b`"))
  expect_equal(dbplyr::translate_sql(a[["b"]]), sql("`a`.`b`"))
})

# custom scalar & string functions fro Hive: adapted from dbplyr\tests\tesstthat---------------------------------------------------------------

test_that("custom scalar & string functions translated correctly", {

  expect_equal(dbplyr::translate_sql(con = conn, bitwShiftL(x, 2L)),                sql("SHIFTLEFT(`x`, 2)"))
  expect_equal(dbplyr::translate_sql(con = conn, bitwShiftR(x, 2L)),                sql("SHIFTRIGHT(`x`, 2)"))
  expect_equal(dbplyr::translate_sql(con = conn, cot(x)),                           sql("1.0 / TAN(`x`)"))
  expect_equal(dbplyr::translate_sql(con = conn, str_replace_all(x, "old", "new")), sql("REGEXP_REPLACE(`x`, 'old', 'new')"))
  expect_equal(dbplyr::translate_sql(con = conn, median(x)),                        sql("PERCENTILE(`x`, 0.5) OVER ()"))
})


test_that("names_to_as() doesn't alias when ident name and value are identical", {
  x <- ident(name = "name")
  y <- sql("`name`")

  expect_equal(names_to_as(y, rlang::names2(x),  conn),  "`name`")
})

test_that("names_to_as() doesn't alias when ident name is missing", {
  x <- ident("*")
  y <- sql("`*`")

  expect_equal(names_to_as(y, rlang::names2(x), conn),  "`*`")
})
