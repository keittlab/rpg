#' @name pqr-package
#' @aliases pqr
#' @docType package
#' @title Easy access to a PostgreSQL database
#' @description
#' Provides functions for connecting to, reading from and writing to a PostgreSQL
#' database. Facilities for tracing the communication between R and PostgreSQL are
#' provided, as are function to retieve detailed session metadata.
#' @details
#' \tabular{ll}{
#' Package: \tab pqr\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2013-6-4\cr
#' License: \tab GPL \cr
#' }
#' The main functions are \code{connect}, which establishes a connection,
#' \code{query}, which issues queries and \code{fetch}, which retieves results.
#' Intelligent defaults are used throughout. Functions that require a connection
#' will automatically attempt to establish a valid connection based on a previous
#' connection or from defaults. The defaults can be overriden in a variety of
#' ways.
#' @author
#' Timothy H. Keitt \cr \url{http://www.keittlab.org/} \cr \cr
#' Maintainer: Timothy H. Keitt \email{tkeitt@@gmail.com} \cr
#' @references \url{http://github.com/thk686/pqr}, \url{http://www.postgresql.org/}
#' @keywords package
#' @import Rcpp
#' @useDynLib pqr
NULL

#' PostgreSQL connection
#' 
#' Manage database connection
#' 
#' @param dbname name of the database or a valid \code{libpq} connection string
#' @param ... named optional connection parameters
#' 
#' @details If no connection parameters are supplied, the
#' connection will fallback to default parameters. Usually
#' this establishes a connection on the localhost to a database,
#' if it exists, with the same name as the user.
#'
#' Valid keywords and their defaults can be obtained by calling
#' \code{get_conn_defaults(all = TRUE)}. A valid \code{libpq}
#' connection string is composed of \code{keyword = value} pairs
#' separated by whitespace. You can either pass the entire string
#' or use named arguments. The names of the arguments will be used
#' as keywords and their values as values.
#' 
#' @note Do not open a connection and then fork the R
#' process. The behavior will be unpredictable. It is perfectly
#' acceptable however to call \code{connect} within each
#' forked instance.
#' 
#' Be careful with \code{ping} as it will attempt to connect to
#' the database server on a remote host, which might not be
#' appreciated by a remote administator. Also, \code{ping} may
#' seem to hang for a long time. It is just polling the connection
#' until it times out.
#' 
#' @return
#' \code{connect} returns one of:
#' \tabular{ll}{
#' \code{CONNECTION_OK} \tab Succesful connection \cr
#' \code{CONNECTION_BAD} \tab Connection failed \cr}
#' 
#' @author Timothy H. Keitt
#' 
#' @examples
#' \dontrun{
#' fetch("show search_path") # default connection
#' connect("test")
#' connect(dbname = "test")
#' connect(dbname = "test", host = "localhost")
#' connect("dbname = test host = localhost")
#' disconnect()}
#' 
#' @export connect
#' @rdname connection
connect = function(dbname, ...)
{
  if ( missing(dbname) )
    values = list(...)
  else
    values = list(dbname = dbname, ...)
  if ( length(values) == 0 )
    return(connect_(character(0), character(0)))
  keywords = names(values)
  if ( is.null(keywords) || "" %in% keywords )
    stop("all arguments must be named")
  connect_(keywords, as.character(values))
}

#' @details \code{fetch} returns the result of a query as a data frame. If
#' \code{sql} is \code{NULL} or empty, then an attempt will be made to retrieve
#' any pending resutls from previous queries. Note that query results are not
#' cleared until the next query is issued so \code{fetch} will continue to
#' return results until a new query is issued.
#' 
#' @return \code{fetch} returns a data frame or a query status object on failure.
#' @rdname query
#' @export
fetch = function(sql = "", pars = NULL)
{
  if ( is.null(sql) || nchar(sql) < 1 )
    return(fetch_dataframe())
  res = query(sql, pars)
  if ( res == "PGRES_TUPLES_OK" )
    fetch_dataframe()
  else res
}

#' PostgreSQL shell
#' 
#' Run PostgreSQL's psql shell interactively
#' 
#' @param psql_opts a character string passed to the psql command
#' 
#' @details
#' The \code{psql} function repeatedly queries for input and pipes it to
#' PostgreSQL's psql command. It will terminate on \code{\\q} or empty
#' input.
#' 
#' If \code{psql_opts} is an empty string, then an attempt will be made to
#' supply suitable options based on the current connection. If there is no
#' active connection, psql will fallback to complied in defaults. If
#' \code{psql_opts} is not an empty string, then it will be passed as is to
#' psql.
#' 
#' You can type psql's escape commands as usual. Try \code{\?}. You
#' cannot use \code{\\e} or \code{\\ef} to evoke an editor. Doing strange
#' things with \code{\!} will likely hang the R session.
#' 
#' There is no way to direclty enter a database password. If one is required,
#' you can use a \href{http://www.postgresql.org/docs/9.1/static/libpq-pgpass.html}{password file}
#'or \code{\link{set_conn_defaults}}. Note that neither solution
#' is very secure, especially \code{\link{set_conn_defaults}} which will
#' assign your password to an environment variable.
#' 
#' Unfortunately it is probably impossible to enable GNU readline support
#' so for example up-arrow will recall your R commands, not the psql
#' commands entered. You can always call psql from a terminal.
#' 
#' @author Timothy H. Keitt
#' 
#' @export
psql = function(psql_opts = "")
{
  psql_path = Sys.which("psql")
  if ( nchar(psql_path) == 0 ) stop("psql not found")
  psql_opts = proc_psql_opts(psql_opts)
  con = pipe(paste(psql_path, psql_opts))
  on.exit(close(con))
  repeat
  {
    inp = readline("psql:> ")
    if ( nchar(inp) && inp != "\\q" )
    {
      if ( inp %in% c("\\e", "\\ef") )
        cat("Cannot evoke editor\n")
      else
        writeLines(inp, con)
    }
    else break
  }
}

#' PostgreSQL database information
#' 
#' Get information about tables in a database
#' 
#' @param only.names if true, just list the table names
#' 
#' @return \code{list_tables}: a vector of table names or a data frame
#' 
#' @author Timothy H. Keitt
#' 
#' @seealso \code{\link{psql}}
#' 
#' @examples
#' \dontrun{
#' # helper util to setup a db for the example
#' pqr:::setup_example_db()
#' 
#' # write data frame contents
#' data(mtcars)
#' write_table(mtcars)
#' list_tables()
#' describe_table("mtcars", "pqrtesting") 
#' 
#' # cleanup and disconnect
#' query("rollback")
#' disconnect()}
#' 
#' @rdname table-info
#' @export
list_tables = function(only.names = TRUE)
{
  res = fetch("SELECT n.nspname as \"Schema\", c.relname as \"Name\",
                CASE c.relkind
                  WHEN \'r\' THEN \'table\'
                  WHEN \'v\' THEN \'view\'
                  WHEN \'i\' THEN \'index\'
                  WHEN \'s\' THEN \'special\'
                  WHEN \'f\' THEN \'foreign table\'
                END as \"Type\",
                pg_catalog.pg_get_userbyid(c.relowner) as \"Owner\"
               FROM pg_catalog.pg_class c
               LEFT JOIN pg_catalog.pg_namespace n
               ON n.oid = c.relnamespace
               WHERE c.relkind IN (\'r\',\'v\',\'f\',\'\')
               AND n.nspname <> \'pg_catalog\'
               AND n.nspname <> \'information_schema\'
               AND n.nspname !~ \'^pg_toast\'
               AND pg_catalog.pg_table_is_visible(c.oid)
               ORDER BY 1,2")
  if ( length(res) < 1 ) return(res)
  if ( inherits(res, "pq.status") ) return(res)
  if ( only.names ) return(res[[2]])
  return(res)
}

#' @param tablename the name of a PostgreSQL table
#' @param schemaname if not null, look only in this schema
#' @return \code{describe_table}: a data frame with column information
#' @rdname table-info
#' @export
describe_table = function(tablename, schemaname = NULL)
{
  sql = "SELECT
          table_schema as schema,
          table_name as table,
          column_name as column,
          ordinal_position as position,
          data_type as type,
          column_default as default
        FROM
          information_schema.columns"
  if ( is.null(schemaname) )
  {
    where = "WHERE
              table_name = $1"
    order = "ORDER BY
              table_schema, ordinal_position"
    fetch(paste(sql, where, order), tablename)
  }
  else
  {
    where = "WHERE
              table_schema = $1
             AND
              table_name = $2"
    order = "ORDER BY
              ordinal_position"
    fetch(paste(sql, where, order), c(schemaname, tablename))
  }
}

#' PostgreSQL data frame IO
#' 
#' Reads and writes table to and from database
#' 
#' @param x a data frame or something convertible to a data frame
#' @param tablename the name of the table to read from or write to
#' @param pkey a column name to use as primary key
#' @param row_names a column name to write row names
#' @param schemaname the schema name
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
#' @note The entire process is wrapped within a transcation. On failure
#' at any point, the transaction will be rolled back and the database
#' unaffected.
#' 
#' Also, \code{write_table} uses SQL \code{INSERT} statements and as such
#' will be slow for large tables. You are much better off bulk loading data
#' using the \code{COPY} command outside of \code{R}.
#' 
#' @author Timothy H. Keitt
#' 
#' @examples
#' \dontrun{
#' # helper util to setup a db for the example
#' pqr:::setup_example_db()
#' 
#' # write data frame contents
#' data(mtcars)
#' write_table(mtcars)
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
#' read_table("mtcars", pkey_to_row_names = T, limit = 3)
#' 
#' # cleanup and disconnect
#' query("rollback")
#' disconnect()}
#' 
#' @rdname table-io
#' @export
write_table = function(x,
                       tablename,
                       pkey = NULL,
                       row_names = NULL,
                       schemaname = NULL,
                       types = NULL,
                       overwrite = FALSE)
{
  if ( missing(tablename) )
    tablename = deparse(substitute(x))
  x = as.data.frame(x, stringsAsFactors = FALSE)
  if ( prod(dim(x)) < 1 ) stop("Empty input")
  tablename = dquote_esc(tablename)
  tablename = set_schema(tablename, schemaname)
  x = handle_row_names(x, row_names)
  colnames = make.unique(names(x), "")
  if ( is.null(types) )
    types = sapply(x, pg_type)
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
  if ( !is.na(check_transaction()) ) on.exit(query("end"))
  spname = unique_name();
  query(paste("savepoint", spname))
  if ( overwrite )
    query(paste("drop table if exists", tablename))
  sql = paste("create table", tablename, "(", types, ")")
  status = query(sql)
  if ( status == "PGRES_COMMAND_OK" )
  {
    x = format_dates(x)
    sqlpars = paste0("$", 1:ncol(x))
    sqlpars = as.csv(sqlpars)
    sql = paste("insert into", tablename, "(", colnames, ")")
    sql = paste(sql, "values (", sqlpars, ")")
    ssname = unique_name()
    pstatus = prepare(sql, ssname)
    if ( pstatus == "PGRES_FATAL_ERROR" )
    {
      query(paste("rollback to", spname))
      return(pstatus)
    }
    estatus = execute_prepared(x, ssname)
    if ( estatus == "PGRES_FATAL_ERROR" )
    {
      query(paste("rollback to", spname))
      return(estatus)
    }
    query(paste("release", spname))
  }
  else
  {
    query(paste("rollback to", spname))
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
                      schemaname = NULL,
                      pkey_to_row_names = FALSE)
{
  tablename = deparse(substitute(tablename))
  tablename = dquote_esc(tablename)
  tablename = set_schema(tablename, schemaname)
  sql = paste("select", what, "from", tablename)
  if ( !is.null(limit) ) sql = paste(sql, "limit", limit)
  res = fetch(sql)
  if ( inherits(res, "pq.status" ) ) return(res) 
  if ( pkey_to_row_names && is.null(row_names) )
    row_names = primary_key_name(tablename)
  if ( !is.null(row_names) )
  {
    row.names(res) = res[[row_names]]
    res[[row_names]] = NULL
  }
  res
}

#' @param warn if true, \code{\link{readLines}} will issue warnings
#' @param ... passed to \code{\link{readLines}}
#' 
#' @details \code{dump_conn_trace} invokes \code{\link{readLines}} on
#' the trace file.
#' 
#' @rdname tracing
#' @export 
dump_conn_trace = function(warn = FALSE, ...)
{
  con = trace_filename()
  if ( is.null(con) || file.access(con, 4) == -1 ) return(NULL)
  out = readLines(con, warn = warn, ...)
  class(out) = "pg.trace.dump"
  return(out)
}

#' @export
print.pg.trace.dump = function(x, ...)
{
  cat(x, sep = "\n", ...)
  invisible(x)
}

#' Iterator support
#' 
#' Construct a row iterator
#' 
#' @param sql any valid query returning rows
#' @param by how many rows to return each iteration
#' 
#' @details This function generates an interator object that can be used with
#' the \code{foreach-package}. It is possible to use the
#' \code{\%dopar\%} operator as shown in the example below. You must
#' establish a connection to the database on each node and in your current
#' session because the call to \code{cursor} requires it. Note that the
#' cursor's lifetime is the current transaction block, so if anything happens
#' to the transaction or you call \code{END} or \code{ROLLBACK}, then the
#' cursor will no longer function. Apparently a named SQL cursor is visible
#' to any database session, as evidenced by the example below,
#' even though it is declared within a transaction. This is not stated
#' explicitely in the PostgreSQL documentation.
#' 
#' @note There are some reports of issues using multicore (forking) with
#' RStudio.
#'  
#' @examples
#' \dontrun{
#' # example requires foreach
#' if ( ! require(foreach, quietly = TRUE) )
#'  stop("This example requires the \'foreach\' package")
#'
#' # helper util to setup a db for the example
#' pqr:::setup_example_db()
#'  
#' # write data frame contents
#' data(mtcars)
#' write_table(mtcars, row_names = "id", pkey = "id")
#' 
#' # expand rows to columns 8 rows at a time
#' x = foreach(i = cursor("select * from mtcars", 8),
#'             .combine = rbind) %do% { i$mpg }
#' print(x, digits = 2)
#'         
#' # parallel example
#' if ( require(doParallel, quietly = TRUE) )
#' {
#'  # make the cluster
#'  cl = makeCluster(2)
#'  
#'  # must connect to database on each node
#'  clusterEvalQ(cl, library(pqr))
#'  clusterEvalQ(cl, connect())
#'  clusterEvalQ(cl, query("set search_path to pqrtesting"))
#'  
#'  # setup the dopar call
#'  registerDoParallel(cl)
#'  
#'  # take column averages 4 rows at a time
#'  curs1 = cursor("select * from mtcars", by = 4)
#'  x = foreach(i = curs1, .combine = rbind, .inorder = FALSE) %dopar%
#'  {
#'    rr = paste0(range(abbreviate(i$id)), collapse = "-")
#'    pid = get_conn_info("server.pid")
#'    j = names(i) != "id"
#'    mn = signif(apply(i[, j], 2, mean), 2)
#'    c(rows = rr, backend = pid, mn)
#'  }
#'  x = as.data.frame(x)
#'  row.names(x) = x$rows
#'  x$rows = NULL
#'  print(noquote(x))
#' }
#'         
#' # cleanup and disconnect
#' query("rollback")
#' disconnect()}
#' 
#' @seealso \code{foreach}
#' 
#' @author Timothy H. Keitt
#' @export
cursor = function(sql, by = 1)
{
  check_transaction();
  cname = unique_name();
  status = query(paste("declare", cname, "cursor for", sql))
  if ( status != "PGRES_COMMAND_OK" ) stop(status)
  f = function()
  {
    res = fetch(paste("fetch", by, "from", cname))
    if ( inherits(res, "pq.status") ) stop(res)
    if ( length(res) < 1 ) stop("StopIteration")
    return(res)
  }
  structure(list(nextElem = f), class = c('cursor', 'abstractiter', 'iter'))
}

#' @param x parameter values
#' @rdname prepare
#' @export
execute_prepared = function(x, name = "")
{
  x = as.matrix(x)
  cols = num_prepared_params(name)
  rows = ceiling(length(x) / cols)
  dim(x) = c(rows, cols)
  storage.mode(x) = "character"
  execute_prepared_(x, name)
}

#' @param what the fields to return or all if NULL
#' @details \code{get_conn_info} returns a list containing
#' information about the current connection. For
#' readability, it will print as though it is a matrix. If
#' you want to see it as a list, try \code{unclass(get_conn_info())}.
#' 
#' If \code{length(what) == 1} then \code{get_conn_info} returns
#' a scalar
#' 
#' @return get_conn_info: a list of values
#' @export get_conn_info
#' @rdname connection-utils
get_conn_info = function(what = NULL)
{
  res = get_conn_info_()
  if ( is.null(what) ) return(res)
  if ( length(what) == 1 ) return(res[[what]])
  return(res[what])
}

#' @param ... a named list of arguments giving new defaults
#' 
#' @details \code{set_conn_defaults} sets the connection defaults by calling
#' \code{\link{Sys.setenv}} and setting the environment variable associated
#' with the connection keywords returned by \code{get_conn_defaults(all = TRUE)}.
#' These settings will only last as long as the current shell session and will
#' reset after a new login.
#' 
#' @rdname connection-utils
#' @export
set_conn_defaults = function(...)
{
  # copied from Sys.setenv
  x = list(...)
  nm = names(x)
  if (is.null(nm) || "" %in% nm) 
    stop("all arguments must be named")
  # end copy
  defs = get_conn_defaults(all = TRUE)
  for ( i in seq(along = nm) )
  {
    envvar = defs$envvar[defs$keyword == nm[i]]
    if ( length(envvar) && nchar(envvar) )
      names(x)[i] = envvar
    else
      x[i] = NULL
  }
  if ( length(x) ) do.call("Sys.setenv", x)
}

#' @details \code{reset_conn_defaults} unsets all environment variables returned
#' by \code{get_conn_defaults(all = TRUE)}.
#' 
#' @rdname connection-utils
#' @export
reset_conn_defaults = function()
{
  var = get_conn_defaults(all = TRUE)$envvar
  for ( v in var )
    if ( length(v) && nchar(v) )
      Sys.unsetenv(v)
}

#' Bulk read and write
#' 
#' Read from and write to a database using COPY
#' 
#' @param what a table name or sql query string
#' @param psql_opt passed directly to the psql command line
#' 
#' @details
#' These functions use the SQL COPY command and therefore are much
#' faster than \code{\link{write_table}} and possibly
#' \code{\link{read_table}}. These functions also call PostgreSQL's
#' psql command from the command line and will fail if it is not
#' found on the search path.
#' 
#' Because these functions shell out to psql you do not need an
#' active connection. By specifying \code{psql_opts} you can connect
#' to any database without affecting the active connection. If you
#' do not specify \code{psql_opts} an attempt will be made to use
#' the active connection information. If that fails,
#' psql will be called without any options.
#' 
#' @note These functions call \code{\link{read.csv}} and
#' \code{\link{write.csv}} and so will suffer the same bandwidth
#' limitations as those functions. I argue that is good enough.
#' There is little point in reading and writing datasets too large
#' for those functions in R. Better to bulk load using psql on
#' the command line and then use \code{\link{cursor}} to read the
#' data in small bits.
#' 
#' @author Timothy H. Keitt
#' 
#' @examples
#' \dontrun{
#' # example requires hflights
#' if ( ! require(hflights, quietly = TRUE) )
#'  stop("This example requires the \'hflights\' package")
#'
#' # big dataset
#' data(hflights)
#' dim(hflights)
#' 
#' dbname = basename(tempfile())
#' system(paste("createdb", dbname))
#' opts = paste("-d", dbname)
#' f = function(x) print(system.time(x));
#' tryCatch(
#' {
#'  f({ copy_to(hflights, psql_opts = opts) }); 
#'  f({ invisible(copy_from("hflights", psql_opts = opts)) })
#' },
#' finally = system(paste("dropdb", dbname)))}
#' 
#' @rdname copy
#' @export
copy_from = function(what, psql_opts = "")
{
  psql_path = Sys.which("psql")
  if ( nchar(psql_path) == 0 ) stop("psql not found")
  psql_opts = proc_psql_opts(psql_opts)
  if ( grepl("select", what) ) what = paste("(", what, ")")
  sql = paste("copy", what, "to stdout csv null \'NA\' header")
  con = pipe(paste(psql_path, psql_opts, "-w -1 -c", dquote_esc(sql)))
  read.csv(con, header = TRUE, as.is = TRUE)
}

#' @param x a data frame
#' @param tablename name of table to create
#' @param schemaname create table in this schema
#' @param overwrite if true drop tablename before creating
#' @param append if true, do not create any table
#' 
#' @rdname copy
#' @export
copy_to = function(x, tablename,
                   schemaname = NULL,
                   overwrite = FALSE,
                   append = FALSE,
                   psql_opts = "")
{
  psql_path = Sys.which("psql")
  if ( nchar(psql_path) == 0 ) stop("psql not found")
  if ( missing(tablename) )
    tablename = deparse(substitute(x))
  tablename = dquote_esc(tablename)
  tablename = set_schema(tablename, schemaname)
  sql = paste("copy", tablename, "from stdin csv null \'NA\' header")
  if ( ! append )
  {
    colnames = make.unique(names(x), "")
    types = sapply(x, pg_type)
    colspec = paste(dquote_esc(colnames), types, collapse = ", ")
    sql = paste("create table", tablename, "(", colspec, ");", sql)
  }
  if ( overwrite )
    sql = paste("drop table if exists", tablename, ";", sql)
  psql_opts = proc_psql_opts(psql_opts)
  con = pipe(paste(psql_path, psql_opts, "-w -1 -c", dquote_esc(sql)))
  write.csv(x, con, row.names = FALSE)
}


