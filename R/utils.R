get_pg_type = function(x)
{
  switch(class(x),
         integer = "integer",
         double = "double precision",
         logical = "boolean",
         Date = "date",
         "text")
}

#' @export
.Last.lib = function(libpath)
{
  disconnect()
}

#' @export
print.conn.info = function(x)
{
  print(as.matrix(unclass(x)))
}

#' @export
print.pq.error.message = function(x)
{
  cat(x)
  invisible(x)
}

#' @export
print.pq.status = function(x)
{
  if ( getOption("verbose") ) cat(x, "\n")
  cat(attr(x, "error.message"))
  invisible(x)
}

as.csv = function(...)
{
  paste0(..., collapse = ", ")
}

dquote_esc = function(...)
{
  paste0(paste0("\"", ...), "\"")
}

squote_esc = function(x)
{
  qs = paste0("\\", "\'")
  paste0(paste0(qs, x), qs)
}

format_dates = function(x)
{
  f = function(a) inherits(a, "Date")
  i = sapply(x, f)
  x[, i] = format(x[, i])
  x
}
