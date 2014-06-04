#' @import Rcpp
#' @useDynLib ezpg
NULL

#' @details \code{fetch} returns the result of the query as a data frame.
#' @return \code{fetch} returns a data frame or a query status object on failure.
#' @rdname query
#' @export
fetch = function(sql = "", pars = NULL)
{
  res = query(sql, pars)
  if ( res == "PGRES_TUPLES_OK" )
    fetch_dataframe()
  else res
}

#' @export
list_tables = function(only.names = TRUE, no.postgres = TRUE)
{
  query = paste("select", ifelse(only.names, "tablename", "*"))
  query = paste(query, "from pg_tables where tableowner not in ($1)")
  res = fetch(query, ifelse(no.postgres, "postgres", ""))
  if ( only.names && length(res) > 0 ) return(res[[1]])
  res
}

#' @export
write_table = function(x,
                       tablename,
                       pkey = NULL,
                       row_names = NULL,
                       schema = NULL,
                       overwrite = FALSE)
{
  if ( overwrite )
    query(paste("drop table", tablename))
  x = as.data.frame(x)
  if ( prod(dim(x)) < 1 ) stop("Empty input")
  tablename = dquote_esc(tablename)
  if ( !is.null(schema) )
    tablename = paste(dquote_esc(schema), tablename, sep = ".")
  if ( !is.null(row_names) && is.character(row_names) )
  {
    x = data.frame(row.names(x), x)
    names(x)[1] = row_names
  }
  colnames = make.unique(names(x), "")
  types = sapply(x, get_pg_type)
  if ( !is.null(pkey) )
    if ( pkey %in% colnames )
    {
      i = which(colnames == pkey)
      types[i] = paste(types[i], "primary key")
      colnames = dquote_esc(colnames)
      types = paste(colnames, types)
    }
    else
    {
      types = c("serial primary key", types)
      colnames = dquote_esc(colnames)
      tabcols = c(dquote_esc(pkey), colnames)
      types = paste(tabcols, types)
    }
  else
  {
    colnames = dquote_esc(colnames)
    types = paste(colnames, types)    
  }
  types = as.csv(types)
  colnames = as.csv(colnames)
  sql = paste("create table", tablename, "(", types, ")")
  query("begin")
  on.exit(query("commit"))
  status = query(sql)
  if ( status == "PGRES_COMMAND_OK" )
  {
    x = format_dates(x)
    sqlpars = paste0("$", 1:ncol(x))
    sqlpars = as.csv(sqlpars)
    sql = paste("insert into", tablename, "(", colnames, ")")
    sql = paste(sql, "values (", sqlpars, ")")
    for ( i in 1:nrow(x) ) status = query(sql, x[i,])
  }
  return(status)
}

#' @export
read_table = function(tablename, what = "*", row_names = NULL)
{
  tablename = paste0(paste0("\"", tablename), "\"")
  res = fetch(paste("select", what, "from", tablename))
  if ( !is.null(row_names) )
  {
    row.names(res) = res[[row_names]]
    res[[row_names]] = NULL
  }
  res
}

#' @export
dump_conn_trace = function(...)
{
  readLines(get_trace_filename(), ...)
}