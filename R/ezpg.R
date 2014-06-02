#' @useDynLib ezpg
NULL

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
