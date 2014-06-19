get_pg_type = function(x)
{
  switch(typeof(x),
         integer = "integer",
         double = "double precision",
         logical = "boolean",
         Date = "date",
         "text")
}

#' @export
.Last.lib = function(libpath)
{
  clean_up_all()
}

#' @export
print.conn.info = function(x, ...)
{
  print(as.matrix(unclass(x)), ...)
}

#' @export
print.pq.error.message = function(x, ...)
{
  cat(x)
  invisible(x)
}

print.message = function(x, terminate = "\n")
{
  if ( !is.null(x) && nchar(x) > 0 )
    cat(x, terminate)
  invisible(x)
}

is_non_empty_string = function(x)
{
  if ( is.null(x) ) return(FALSE)
  if ( nchar(x) < 1 ) return(FALSE)
  return(TRUE)
}

#' @export
print.pq.status = function(x, ...)
{
  if ( getOption("verbose") ) print.message(x)
  error.message = attr(x, "error.message")
  if ( is_non_empty_string(error.message) )
    print.message(error.message)
  else
  {
    command.status = attr(x, "command.status")
    if ( is_non_empty_string(command.status) )
      print.message(command.status)
    else if ( x == "PGRES_FATAL_ERROR" )
      print.message(x)
  }
  invisible(x)
}

as.csv = function(...)
{
  paste0(..., collapse = ", ")
}

dquote_esc = function(...)
{
  gsub("\"+", "\"",  paste0(paste0("\"", ...), "\""))
}

format_dates = function(x)
{
  f = function(a) inherits(a, "Date")
  i = sapply(x, f)
  x[, i] = format(x[, i])
  x
}

set_schema = function(a, b)
{
  if ( !is.null(b) )
    a = paste(dquote_esc(b), a, sep = ".")
  return(a)
}

handle_row_names = function(a, b)
{
  if ( !is.null(b) )
  {
    a = data.frame(row.names(a), a, stringsAsFactors = FALSE)
    names(a)[1] = b
  }
  return(a)
}

get_primary_key_name = function(tablename)
{
  unlist(fetch("SELECT               
                  pg_attribute.attname as pkey
                FROM
                  pg_index, pg_class, pg_attribute 
                WHERE
                  pg_class.oid = $1::regclass
                AND
                  indrelid = pg_class.oid
                AND
                  pg_attribute.attrelid = pg_class.oid
                AND 
                  pg_attribute.attnum = any(pg_index.indkey)
                AND
                  indisprimary", tablename))
}

get_unique_name = function()
{
  dquote_esc(uuid::UUIDgenerate())
}

table_exists = function(table, schema = NULL)
{
  sql = "select count(*) > 0 from pg_catalog.pg_tables"
  if ( is.null(schema) )
    sql = paste(sql, "where tablename = $1")
  else
    sql = paste(sql, "where tablename = $1 and schemaname = $2")
  fetch(sql, strip_quotes(c(table, schema)))[[1]]
}

strip_quotes = function(x)
{
  gsub("\"", "", x)
}
