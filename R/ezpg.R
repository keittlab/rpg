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

#' @export
list_tables = function(only.names = TRUE, no.postgres = TRUE)
{
  query = paste("select", ifelse(only.names, "tablename", "*"))
  query = paste(query, "from pg_tables where tableowner not in ($1)")
  res = fetch(query, ifelse(no.postgres, "postgres", ""))
  if ( only.names ) return(res[[1]])
  res
}

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
write_table = function(x,
                       tablename,
                       pkey = "id",
                       row_names = FALSE,
                       schema = NULL)
{
  x = as.data.frame(x)
  if ( nrow(x) < 1 ) stop("Empty input")
  tablename = as.character(tablename)
  if ( !is.null(schema) )
    tablename = paste(schema, tablename, sep = ".")
  colnames = make.unique(names(x), "")
  write.colnames = colnames
  types = sapply(x, get_pg_type)
  i = which(colnames == pkey)
  if ( length(i) > 0 )
  {
    types[i] = paste(types[i], "primary key")
  }
  else
  {
    if ( row_names && .row_names_info(x) > 0 )
    {
      rnvals = attr(x, "row.names")
      types = c(paste(get_pg_type(rnvals), "primary key"), types)
      colnames = c(pkey, colnames)
      write.colnames = colnames
      x = data.frame(rnvals, x)
    }
    else
    {
      colnames = c(pkey, colnames)
      types = c("serial primary key", types)
    } 
  }
  types = paste(paste(colnames, types), collapse = ", ")
  sql = paste0("create table ", tablename, " (", types, ")")
  query("begin")
  on.exit(query("commit"))
  status = query(sql)
  if ( status == "PGRES_COMMAND_OK" )
  {
    i = sapply(x, function(a) inherits(a, "Date"))
    x[, i] = format(x[, i])
    sqlpars = paste0("$", seq(along = write.colnames))
    sqlpars = paste(sqlpars, collapse = ", ")
    write.colnames = paste(write.colnames, collapse = ", ")
    sql = paste("insert into", tablename, "(", write.colnames, ")")
    sql = paste(sql, "values (", sqlpars, ")")
    for ( i in 1:nrow(x) ) status = query(sql, x[i,])
  }
  return(status)
}
