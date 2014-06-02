#' @useDynLib ezpg
NULL

#' @export
fetch = function(sql = "", pars = NULL)
{
  res = query(sql, pars)
  if ( res == "PGRES_TUPLES_OK" )
    fetch_dataframe()
  else res
}

#' @export
.Last.lib = function(libpath)
{
  disconnect()
}

#' @export
print.connInfo = function(x)
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
  cat(x, "\n")
  cat(attr(x, "error.message"))
  invisible(x)
}
