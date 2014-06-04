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

#' PostgreSQL database information
#' 
#' Get information about tables in a database
#' 
#' @param only.name if true, just list the table names
#' @param no.postgres if true, exclude tables owned by the postgres role
#' 
#' @return \code{list_tables} a list of table names
#' 
#' @author Timothy H. Keitt
#' @rdname table-info
#' @export
list_tables = function(only.names = TRUE, no.postgres = TRUE)
{
  query = paste("select", ifelse(only.names, "tablename", "*"))
  query = paste(query, "from pg_tables where tableowner not in ($1)")
  res = fetch(query, ifelse(no.postgres, "postgres", ""))
  if ( only.names && length(res) > 0 ) return(res[[1]])
  res
}

#' @return \code{describe_table}: a data frame with column information
#' @rdname table-info
#' @export
describe_table = function(tablename)
{
  fetch("select
          table_schema as schema,
          table_name as table,
          column_name as column,
          ordinal_position as position,
          data_type as type,
          column_default as default
        from
          information_schema.columns
        where
          table_name = $1
        order by
          ordinal_position", tablename)
}

#' PostgreSQL data frame IO
#' 
#' Reads and writes table to and from database
#' 
#' @param x a data frame or something convertible to a data frame
#' @param tablename the name of the table to read from or write to
#' @param pkey a column name to use as primary key
#' @param row_names a column name to write row names
#' @param schema the schema name
#' @param types a list of valid PostgreSQL type names
#' @param overwrite if true, destroy existing table with the same name
#' 
#' @details
#' A table is created using the current connection. If \code{pkey} does not
#' match any column name, then a new column is created with the name given by
#' \code{pkey}. Its type will be \code{serial} and it will be set as the primary
#' key. If \code{pkey} does match an existing column name, then that column will
#' be used as the primary key. Note that \code{\link{make.unique}} will be
#' called on the column names before this matching is done. If \code{row_names}
#' is a character string, the data frame row names will be stored in a column
#' with the column name given by \code{row_names}. The \code{row_names} column
#' can also be the primary key if \code{pkey} is the same as \code{row_names}.
#' 
#' If \code{row_names} is specified when calling \code{read_table}, then the
#' resulting data frame will have row names installed from the column named
#' in \code{row_names}. Note that the column named in \code{row_names} must
#' match a column specified by \code{what}. The matching column will be removed
#' from the data frame.
#' 
#' If \code{types} is not supplied, they will be computed from the classes and
#' types of the columns of input.
#' 
#' @return \code{write_table} the final query status
#' 
#' @author Timothy H. Keitt
#' 
#' @examples
#' \dontrun{
#' data(mtcars)
#' 
#' # default connection
#' connect()
#' 
#' # just the columns
#' write_table(mtcars, overwrite = T)
#' 
#' # make "cyl" primary key (will fail unique constraint)
#' write_table(mtcars, pkey = "cyl", overwrite = T)
#' 
#' # also write row names to "id"
#' write_table(mtcars, row_names = "id", overwrite = T)
#' 
#' # row names as primary key
#' write_table(mtcars, row_names = "id", pkey = "id", overwrite = T)
#' 
#' # default R row names and only first 3 columns
#' read_table("mtcars", what = "mpg, cyl, disp", limit = 3)
#' 
#' # row names from column "id"
#' read_table("mtcars", row_names = "id", limit = 3)
#' 
#' # get row names from primary key
#' read_table("mtcars", pkey_to_row_names = T, limit = 3)}
#' @rdname table-io
#' @export
write_table = function(x,
                       tablename,
                       pkey = NULL,
                       row_names = NULL,
                       schema = NULL,
                       types = NULL,
                       overwrite = FALSE)
{
  if ( missing(tablename) )
    tablename = deparse(substitute(x))
  x = as.data.frame(x, stringsAsFactors = FALSE)
  if ( prod(dim(x)) < 1 ) stop("Empty input")
  tablename = dquote_esc(tablename)
  tablename = set_schema(tablename, schema)
  x = handle_row_names(x, row_names)
  colnames = make.unique(names(x), "")
  if ( is.null(types) )
    types = sapply(x, get_pg_type)
  else
    types = rep(types, ncol(x))
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
  query("begin")
  on.exit(query("commit"))
  if ( overwrite )
    query(paste("drop table", tablename))
  sql = paste("create table", tablename, "(", types, ")")
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

#' @param what a vector of column names
#' @param limit only return this many rows
#' @param pkey_to_row_names if true and row_names not given, use primary key column
#' 
#' @return \code{read_table} a data frame
#' @rdname table-io
#' @export
read_table = function(tablename,
                      what = "*",
                      limit = NULL,
                      row_names = NULL,
                      schema = NULL,
                      pkey_to_row_names = FALSE)
{
  tablename = dquote_esc(tablename)
  tablename = set_schema(tablename, schema)
  sql = paste("select", what, "from", tablename)
  if ( !is.null(limit) ) sql = paste(sql, "limit", limit)
  res = fetch(sql)
  if ( pkey_to_row_names && is.null(row_names) )
    row_names = get_primary_key_name(tablename)
  if ( !is.null(row_names) )
  {
    row.names(res) = res[[row_names]]
    res[[row_names]] = NULL
  }
  res
}

#' @rdname tracing
#' @export
#' 
#' @param ... passed to \code{\link{readLines}}
#' 
#' @details \code{dump_conn_trace} invokes \code{\link{readLines}} on
#' the trace file.
#' 
#' @author Timothy H. Keitt
dump_conn_trace = function(...)
{
  con = get_trace_filename()
  if ( is.null(con) || file.access(con, 4) == -1 ) return(NULL)
  readLines(con, ...)
}