# non-exported function copied from RJDBC package https://github.com/s-u/RJDBC
.sql.qescape <- function(s, identifier=FALSE, quote="\"") {
  s <- as.character(s)
  if (identifier) {
    vid <- grep("^[A-Za-z]+([A-Za-z0-9_]*)$",s)
    if (length(s[-vid])) {
      if (is.na(quote)) stop("The JDBC connection doesn't support quoted identifiers, but table/column name contains characters that must be quoted (",paste(s[-vid],collapse=','),")")
      s[-vid] <- .sql.qescape(s[-vid], FALSE, quote)
    }
    return(s)
  }
  if (is.na(quote)) quote <- ''
  s <- gsub("\\\\","\\\\\\\\",s)
  if (nchar(quote)) s <- gsub(paste("\\",quote,sep=''),paste("\\\\\\",quote,sep=''),s,perl=TRUE)
  paste(quote,s,quote,sep='')
}

#' .verify.JDBC.result copied from RJDBC
.verify.JDBC.result <- function (result, ...) {
  if (is.jnull(result)) {
    x <- .jgetEx(TRUE)
    if (is.jnull(x))
      stop(...)
    else
      stop(...," (",.jcall(x, "S", "getMessage"),")")
  }
}

#' @importFrom dbplyr sql_call2 win_over win_current_group win_current_frame
#quantile and median verbs imported from dbplyr/R/translate-sql-quantile.R
sql_quantile <- function(f,
                         style = c("infix", "ordered"),
                         window = FALSE) {
  force(f)
  style <- match.arg(style)
  force(window)

  function(x, probs) {
    check_probs(probs)

    sql <- switch(style,
                  infix = sql_call2(f, x, probs),
                  ordered = build_sql(
                    sql_call2(f, probs), " WITHIN GROUP (ORDER BY ", x, ")"
                  )
    )

    if (window) {
      sql <- win_over(sql,
                      partition = win_current_group(),
                      frame = win_current_frame()
      )
    }
    sql
  }
}

sql_median <- function(f,
                       style = c("infix", "ordered"),
                       window = FALSE) {

  warned <- FALSE
  quantile <- sql_quantile(f, style = style, window = window)
  function(x, na.rm = FALSE) {
    warned <<- check_na_rm("median", na.rm, warned)
    quantile(x, 0.5)
  }
}

check_probs <- function(probs) {
  if (!is.numeric(probs)) {
    stop("`probs` must be numeric", call. = FALSE)
  }

  if (length(probs) > 1) {
    stop("SQL translation only supports single value for `probs`.", call. = FALSE)
  }
}

check_na_rm <- function(f, na.rm, warned) {
  if (warned || identical(na.rm, TRUE)) {
    return(warned)
  }
  warning(
    "Missing values are always removed in SQL.\n",
    "Use `", f, "(x, na.rm = TRUE)` to silence this warning\n",
    "This warning is displayed only once per session.",
    call. = FALSE
  )
  TRUE
}

