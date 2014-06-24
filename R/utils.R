pg_type = function(x)
{
  switch(class(x),
         numeric = switch(typeof(x),
                          double = "double precision",
                          integer = "integer",
                          "text"),
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

primary_key_name = function(tablename)
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

unique_name = function()
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

proc_psql_opts = function(psql_opts = "")
{
  if ( nchar(psql_opts) == 0 && get_conn_info("status.ok") )
  {
    host = get_conn_info("host")
    dbnm = get_conn_info("dbname")
    port = get_conn_info("port")
    user = get_conn_info("user")
    if ( !is.null(host) ) psql_opts = paste(psql_opts, "-h", host)
    if ( !is.null(dbnm) ) psql_opts = paste(psql_opts, "-d", dbnm)
    if ( !is.null(port) ) psql_opts = paste(psql_opts, "-p", port)
    if ( !is.null(user) ) psql_opts = paste(psql_opts, "-U", user)
    psql_opts = paste(psql_opts, "-w")
  }
  return(psql_opts)
}

run_examples = function()
{
  eval(example(ping, run = T), globalenv())
  eval(example(connect, run = T), globalenv())
  eval(example(query, run = T), globalenv())
  eval(example(trace_conn, run = T), globalenv())
  eval(example(libpq_version, run = T), globalenv())
  eval(example(prepare, run = T), globalenv())
  eval(example(push_conn, run = T), globalenv())
  eval(example(async_query, run = T), globalenv())
  eval(example(list_tables, run = T), globalenv())
  eval(example(write_table, run = T), globalenv())
  eval(example(cursor, run = T), globalenv())
}

copy_from = function(what, psql_opts = "")
{
  psql_path = Sys.which("psql")
  if ( nchar(psql_path) == 0 ) stop("psql not found")
  psql_opts = proc_psql_opts(psql_opts)
  if ( grepl("select", what) ) what = paste("(", what, ")")
  sql = paste("copy", what, "to stdout csv header")
  con = pipe(paste(psql_path, psql_opts, "-c", dquote_esc(sql)))
  read.csv(con, header = TRUE, na.string = "\\N", as.is = TRUE)
}

copy_to = function(x, tablename = deparse(substitute(x)), psql_opts = "")
{
  psql_path = Sys.which("psql")
  if ( nchar(psql_path) == 0 ) stop("psql not found")
  psql_opts = proc_psql_opts(psql_opts)
  sql = paste("copy", tablename, "from stdout csv header")
  con = pipe(paste(psql_path, psql_opts, "-c", dquote_esc(sql)))
  write.csv(x, con, header = TRUE, na.string = "\\N", as.is = TRUE)
}
