source('utilities.R')

conn <- HiveS2_TestConnection()

test_that("quote identifier", {
    expect_equal(dbQuoteIdentifier(conn, character()), SQL(character()))
    expect_equal(dbQuoteIdentifier(conn, "a"), SQL('`a`'))
    expect_equal(dbQuoteIdentifier(conn, "a b"), SQL('`a b`'))

    expect_equal(dbQuoteIdentifier(conn, SQL('"a"')), SQL('"a"'))
    expect_equal(dbQuoteIdentifier(conn, SQL('"a b"')), SQL('"a b"'))
  })

