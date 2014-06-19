#include "pqr.h"

// [[Rcpp::export]]
CharacterVector connect_(CharacterVector keywords, CharacterVector values)
{
  setup_connection(keywords, values);
  CharacterVector out(connection_status_string());
  out.attr("error.message") = connection_error_string();
  out.attr("class") = "pq.status";
  return out;
}

//' Database connection utilities
//' 
//' Conection reporting and defaults
//' 
//' @param opts a libpq connection string
//' 
//' @details \code{ping} will ignore any keywords not directly
//' related to the database host (e.g., username, dbname) as it
//' does not connect; it only detect the server port is responding.
//' 
//' @return \code{ping} returns one of the following:
//' \tabular{ll}{
//' \code{PQPING_OK}  \tab Server reachable \cr
//' \code{PQPING_REJECT } \tab Server reachable but not accepting
//' connections \cr
//' \code{PQPING_NO_RESPONSE} \tab Server unreachable \cr
//' \code{PQPING_NO_ATTEMPT} \tab Connection string is nonsense \cr}
//' 
//' @author Timothy H. Keitt
//' 
//' @examples
//' \dontrun{
//' ping("connect_timeout = 3, host = www.keittlab.org")
//' connect()
//' get_conn_defaults()
//' set_conn_defaults(dbname = "test")
//' get_conn_defaults()
//' reset_conn_defaults()
//' get_conn_defaults()
//' get_conn_defaults(all = TRUE)
//' get_conn_info()
//' get_conn_error()
//' disconnect()}
//' 
//' @export ping
//' @rdname connection-utils
// [[Rcpp::export]]
CharacterVector ping(const char* opts = "")
{
  return ping_status_string(opts);
}

//' @details \code{disconnect} will free any query results as well
//' as clean up the connection data. It is called in the pakcage
//' \code{\link{.Last.lib}} function when exiting \code{R}.
//' 
//' @export disconnect
//' @rdname connection
// [[Rcpp::export]]
void disconnect()
{
  clear_conn();
}

// [[Rcpp::export]]
void clean_up_all()
{
  clear_all();
}

//' @return get_conn_error: an error string
//' @export get_conn_error
//' @rdname connection-utils
// [[Rcpp::export]]
CharacterVector get_conn_error()
{
  CharacterVector err(PQerrorMessage(conn));
  err.attr("class") = "pq.error.message";
  return err;
}

// [[Rcpp::export]]
SEXP get_conn_info_()
{
  List info;
  info["dbname"] = wrap_string(PQdb(conn));
  info["user"] = wrap_string(PQuser(conn));
  info["host"] = wrap_string(PQhost(conn));
  info["status.ok"] = PQstatus(conn) == CONNECTION_OK;
  info["port"] = wrap_string(PQport(conn));
  info["options"] = wrap_string(PQoptions(conn));
  info["transaction.status"] = trasaction_status_string();
  info["protocol.version"] = PQprotocolVersion(conn);
  info["server.version"] = PQserverVersion(conn);
  info["socket"] = PQsocket(conn);
  info["server.pid"] = PQbackendPID(conn);
  info["password.needed"] = PQconnectionNeedsPassword(conn) == 1;
  info["password.supplied"] = PQconnectionUsedPassword(conn) == 1;  
  info["encrypted"] = PQgetssl(conn) != NULL;
  info["server.encoding"] = fetch_par("server_encoding");
  info["client.encoding"] = fetch_par("client_encoding");
  info["application.name"] = fetch_par("application_name");
  info["is.superuser"] = fetch_par("is_superuser");
  info["session.authorization"] = fetch_par("session_authorization");
  info["date.style"] = fetch_par("DateStyle");
  info["interval.style"] = fetch_par("IntervalStyle");
  info["time.zone"] = fetch_par("TimeZone");
  info["integer.datetimes"] = fetch_par("integer_datetimes");
  info["standard.conforming.strings"] = fetch_par("standard_conforming_strings");
  info.attr("class") = "conn.info";
  return wrap(info);
}

//' PostgreSQL query
//' 
//' Issue a query to the current database connection
//' 
//' @param sql a query string
//' @param pars a character vector of substitution values
//' 
//' @return \code{query} returns:
//' \tabular{ll}{
//' PGRES_EMPTY_QUERY \tab The string sent to the server was empty \cr
//' PGRES_COMMAND_OK \tab Successful completion of a command returning no data \cr
//' PGRES_TUPLES_OK \tab Successful completion of a command returning data (such as a SELECT or SHOW) \cr
//' PGRES_COPY_OUT \tab Copy Out (from server) data transfer started \cr
//' PGRES_COPY_IN \tab Copy In (to server) data transfer started \cr
//' PGRES_BAD_RESPONSE \tab The server's response was not understood. \cr
//' PGRES_NONFATAL_ERROR \tab A nonfatal error (a notice or warning) occurred \cr
//' PGRES_FATAL_ERROR \tab A fatal error occurred \cr
//' PGRES_COPY_BOTH \tab Copy In/Out (to and from server) data transfer started. This is currently used only for streaming replication \cr}
//' 
//' @author Timothy H. Keitt
//' 
//' @examples
//' \dontrun{
//' query("begin")
//' query("create table test (id integer, field text)")
//' query("insert into test values ($1, $2)", c(1, "test"))
//' fetch("select * from test")
//' query("rollback")}
//' 
//' @rdname query
//' @export query
// [[Rcpp::export]]
CharacterVector query(const char* sql = "", SEXP pars = R_NilValue)
{
  check_conn();
  if ( Rf_isNull(pars) )
    set_res(PQexec(conn, sql));
  else
    exec_params(sql, pars);
  return get_result_status();
}

//' @return \code{get_query_error} returns an error string
//' @export get_query_error
//' @rdname query
// [[Rcpp::export]]
CharacterVector get_query_error()
{
  CharacterVector err(PQresultErrorMessage(res));
  err.attr("class") = "pq.error.message";
  return err;
}

// [[Rcpp::export]]
CharacterMatrix fetch_matrix()
{
  int nc = PQnfields(res),
      nr = PQntuples(res);
  CharacterMatrix out(nr, nc);
  for ( int i = 0; i < nr; ++i )
    for ( int j = 0; j < nc; ++ j )
      out(i, j) = fetch_string(i, j);
  return out;
}

// [[Rcpp::export]]
List fetch_dataframe()
{
  List out;
  int nrow = PQntuples(res),
      ncol = PQnfields(res);
  if ( nrow == 0 || ncol == 0 ) return out;
  CharacterVector names(ncol);
  for ( int col = 0; col < ncol; ++col )
  {
    std::string colname = PQfname(res, col);
    out[colname] = fetch_column(col);
    names[col] = colname;
  }
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrow);
  out.attr("class") = "data.frame";
  out.attr("names") = names;
  return out;
}

//' PostgeSQL connection tracing
//' 
//' Functions to manage connection tracing
//' 
//' @param filename where to send the tracing
//' @param append if true, append to existing file
//' @param remove if true, unlink the tracing file
//' 
//' @details
//' PostgeSQL tracing lets you observe all information
//' passing between \code{pqr} and the database server.
//' 
//' \code{trace_conn} begins tracing and \code{untrace_conn}
//' stops tracing.
//' 
//' @author Timothy H. Keitt
//' 
//' @examples
//' \dontrun{
//' connect()
//' trace_conn()
//' list_tables()
//' dump_conn_trace(n = 40)
//' untrace_conn(remove = TRUE)}
//' @rdname tracing
//' @export
// [[Rcpp::export]]
void trace_conn(const char* filename = "", bool append = false)
{
  check_conn();
  if ( !strlen(filename) ) filename = tempfile();
  if ( tracef ) fclose(tracef);
  tracef = fopen(filename, append ? "a" : "w");
  if ( tracef )
    tracefname = filename;
  else
    Rf_warning("Unable to open tracefile");
  PQtrace(conn, tracef);
}

//' @rdname tracing
//' @export
// [[Rcpp::export]]
void untrace_conn(bool remove = false)
{
  if ( remove && tracefname ) unlink(tracefname);
  clear_tracef();
}

//' @return \code{get_trace_filename}: the name of the file containing
//' trace information.
//' 
//' @rdname tracing
//' @export
// [[Rcpp::export]]
const char* get_trace_filename()
{
  // I figure asking for the file name
  // is a good bet it will soon be read
  if ( tracef ) fflush(tracef);
  return tracefname;
}

//' @param all if false return only defaults with settings
//' @details \code{get_conn_defaults} returns a data frame containing
//' all of the possible connection string keywords, the names of environment
//' variables used to override the defaults, the compiled in default value
//' and the current value of the keyword.
//' @return \code{get_conn_defaults}: a data frame with defaults listed
//' @rdname connection-utils
//' @export
// [[Rcpp::export]]
List get_conn_defaults(const bool all = false)
{
  std::vector<std::string> kw, ev, cp, va;
  PQconninfoOption *defs = PQconndefaults(), *i = defs;
  while ( i && i->keyword )
  {
    if ( all || i->val && strlen(i->val) )
    {
      kw.push_back(i->keyword);
      ev.push_back(i->envvar ? i->envvar : "");
      cp.push_back(i->compiled ? i->compiled : "");
      va.push_back(i->val ? i->val : "");
    }
    ++i;
  }
  PQconninfoFree(defs);
  List out = List::create(Named("keyword") = kw,
                          Named("envvar") = ev,
                          Named("compiled") = cp,
                          Named("value") = va);
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -kw.size());
  out.attr("class") = "data.frame";
  return out;
}

//' Miscelaneous functions
//' 
//' Various utility functions
//' 
//' @author Timothy H. Keitt
//' @rdname misc
//' @export
// [[Rcpp::export]]
int libpq_version()
{
  return PQlibVersion();
}

//' @param passwd the password
//' @param user the user name
//' @rdname misc
//' @export
// [[Rcpp::export]]
const char* encrypt_password(const char* passwd, const char* user)
{
  return PQencryptPassword(passwd, user);
}

//' @rdname misc
//' @export
// [[Rcpp::export]]
const char* get_encoding()
{
  check_conn();
  return pg_encoding_to_char(PQclientEncoding(conn));
}

//' @param encoding the character encoding
//' @rdname misc
//' @export
// [[Rcpp::export]]
bool set_encoding(const char* encoding)
{
  check_conn();
  return PQsetClientEncoding(conn, encoding) == 0;
}

//' @param verbosity one of "terse", "default", "verbose"
//' @rdname misc
//' @export
// [[Rcpp::export]]
void set_error_verbosity(std::string verbosity)
{
  check_conn();
  if ( verbosity.compare("terse") == 0 )
    PQsetErrorVerbosity(conn, PQERRORS_TERSE);
  if ( verbosity.compare("default") == 0 )
    PQsetErrorVerbosity(conn, PQERRORS_DEFAULT);
  if ( verbosity.compare("verbose") == 0 )
    PQsetErrorVerbosity(conn, PQERRORS_VERBOSE);
}

// [[Rcpp::export]]
CharacterVector check_transaction()
{
  check_conn();
  switch ( PQtransactionStatus(conn)  )
  {
    case PQTRANS_INERROR:
      Rcout << "Error in current transaction: rolling back... done." << std::endl;
      query("rollback");
    case PQTRANS_IDLE:
      Rcout << "Initiating new transaction block... done." << std::endl;
      return query("begin");
    default:
      return NA_STRING;
  }
  return NA_STRING; // compiler warning
}

//' Prepared queries
//' 
//' Prepare and execute queries
//' 
//' @param sql a valid query string
//' @param name an optional statement name
//' 
//' @details These functions allow the preparation of an incomplete
//' SQL statement and then application of that statement to a arrays
//' of input parameters.
//' 
//' If the number of parameters supplied to \code{execute_prepared} is a
//' multiple of the number of open parameters in query prepared
//' using \code{prepare}, then the prepared query will be executed
//' repeatedly for each successive set of parameters. This repeated
//' execution loop is evaluted in C++ and so is quite fast. The
//' supplied parameter values will be coerced to a matrix of the
//' appropriate dimensions. If the number of supplied parameter
//' values is not an even multiple of the number of parameters
//' specified in the prepared statement, an error will occur.
//' The passed parameters will be coerced to character strings.
//' 
//' If you supply a \code{name}, then you must use the same name
//' when calling \code{execute_prepared}. This allows multiple prepared queries.
//' If you do not supply a name, each call to \code{prepare} will
//' overwrite the previous prepared statement. The lifetime of
//' a prepared statement is the lifetime of the current connection
//' or transaction.
//' 
//' @note One can use pure SQL to achieve the same result and
//' \code{execute_prepared} will work on any prepared statement regardless
//' of whether it was defined using \code{prepare} or directly with
//' \code{\link{query}}.
//' 
//' It is generally a good idea to wrap \code{prepare}--\code{execute_prepared}
//' in a transaction. If not in a transaction, you cannot rollback any updates
//' and it will be much slower as PostgreSQL initiates a transaction-per-query
//' by default.
//' 
//' @return A status string.
//' 
//' In the case of \code{execute_prepared} the status of the last command
//' executed. In case of error, the loop will terminate prematurely and
//' the status string will contain the error message.
//' 
//' @author Timothy H. Keitt
//' 
//' @examples
//' \dontrun{
//' # try connecting to default database
//' if ( connect() == "CONNECTION_BAD" )
//' {
//'  system("createdb -w -e")
//'  if ( connect() == "CONNECTION_BAD" )
//'    stop("Cannot connect to database")
//' }
//' 
//' # we'll rollback at the end
//' query("begin")
//' 
//' # for kicks work in a schema
//' query("drop schema if exists pqrtesting cascade")
//' query("create schema pqrtesting")
//' query("set search_path to pqrtesting")
//' 
//' # write data frame contents
//' data(mtcars)
//' write_table(mtcars)
//' 
//' # delete the rows
//' query("truncate mtcars")
//' read_table(mtcars)
//' 
//' # use prepare-execute to write rows
//' pars = paste0("$", 1:11, collapse = ", ")
//' sql = paste0("insert into mtcars values (", pars, ")", collapse = " ")
//' prepare(sql, "test_statement")
//' execute_prepared(mtcars, "test_statement")
//' read_table(mtcars, limit = 5)
//' 
//' # cleanup and disconnect
//' query("rollback")
//' disconnect()}
//' 
//' @rdname prepare
//' @export
// [[Rcpp::export]]
CharacterVector prepare(const char* sql, const char* name = "")
{
  check_conn();
  set_res(PQprepare(conn, name, sql, 0, NULL));
  return get_result_status();  
}

// [[Rcpp::export]]
SEXP execute_prepared_(CharacterMatrix pars, const char* name = "")
{
  exec_prepared_rows(pars, name);
  return get_result_status();
}

// [[Rcpp::export]]
int num_prepared_params(const char* name = "")
{
  set_res(PQdescribePrepared(conn, name));
  return PQnparams(res);
}

//' Multiple PostgreSQL connections
//' 
//' Manage multiple connections on a stack
//' 
//' @details
//' These functions allow you to store multiple connections on a stack. They are
//' only used for their side-effects. \code{\link{pqr}} stores an active connection
//' pointer internally. This pointer can be moved onto the stack and manipulated.
//' Once on the stack, the pointer is no longer active. You must use
//' \code{swap_conn} or \code{pop_conn} to reactive a pushed connection, or call
//' \code{\link{connect}} to create a new active connection.
//' 
//' \code{push_conn} pushes the current connection onto the connection stack
//' leaving the active connection null.
//' 
//' @examples
//' \dontrun{
//' # try connecting to default database
//' if ( connect() == "CONNECTION_BAD" )
//' {
//'  system("createdb -w -e")
//'  if ( connect() == "CONNECTION_BAD" )
//'    stop("Cannot connect to database")
//' }
//' 
//' # make some databases
//' dbs = basename(c(tempfile(), tempfile(), tempfile()))
//' sql = paste("create database", dbs)
//' if ( any(sapply(sql, query) == "PGRES_FATAL_ERROR") )
//' {
//'   lapply(paste("drop database", dbs), query)
//'   stop("Cannot create databases")
//' }
//' 
//' # connect
//' connect(paste("dbname", dbs[1], sep = "=")); push_conn()
//' connect(paste("dbname", dbs[2], sep = "=")); push_conn()
//' connect(paste("dbname", dbs[3], sep = "=")); push_conn()
//' 
//' show_conn_stack()
//' rotate_stack()
//' show_conn_stack()
//' rotate_stack(2)
//' show_conn_stack()
//' pop_conn()
//' show_conn_stack()
//' get_conn_info("dbname")
//' swap_conn()
//' show_conn_stack()
//' get_conn_info("dbname")
//' pop_conn()
//' show_conn_stack()
//' pop_conn()
//' show_conn_stack()
//' disconnect()
//' connect()
//' 
//' # cleanup
//' lapply(paste("drop database", dbs), query)}
//' 
//' @rdname stack
//' @export
// [[Rcpp::export]]
void push_conn()
{
  conn_stack.push_back(conn);
  conn = NULL;
}

//' @details
//' \code{pop_conn} pops a connection off the stack and makes it active. Whatever
//' connection was active when \code{pop_conn} is called will be disconnected and
//' cleared. Use \code{swap_conn} to preserve the active connection.
//' 
//' @rdname stack
//' @export
// [[Rcpp::export]]
void pop_conn()
{
  set_conn(conn_stack.back());
  conn_stack.pop_back();
}

//' @details
//' \code{swap_conn} swaps the active connection with the connection on the top
//' of the stack. If the stack is empty, the connection is swapped with a null
//' connection.
//' 
//' @rdname stack
//' @export
// [[Rcpp::export]]
void swap_conn()
{
  if ( conn_stack.empty() )
    push_conn();
  else
    std::swap(conn, conn_stack.back());
}

//' @param n number of shifts
//' @details
//' \code{rotate_stack} moves the bottom of the stack to the top.
//' 
//' @rdname stack
//' @export
// [[Rcpp::export]]
void rotate_stack(const unsigned n = 1)
{
  int ss = conn_stack.size();
  if ( ss > 1 )
    std::rotate(conn_stack.begin(),
                conn_stack.begin() + n % ss,
                conn_stack.end());
}

//' @details
//' \code{show_conn_stack} returns a data frame with information about the
//' connections on the stack.
//' 
//' @rdname stack
//' @export
// [[Rcpp::export]]
List show_conn_stack()
{
  int n = conn_stack.size();
  if ( n == 0 ) return List();
  CharacterVector host(n), dbname(n);
  LogicalVector conn_status(n);
  for ( int i = 0; i != n; ++i )
  {
    const char *hn = PQhost(conn_stack[i]),
               *db = PQdb(conn_stack[i]);
    bool is_ok = PQstatus(conn_stack[i]) == CONNECTION_OK;
    if ( hn ) host[i] = hn; else host[i] = NA_STRING;
    if ( db ) dbname[i] = db; else dbname[i] = NA_STRING;
    conn_status[i] = is_ok;
  }
  List out = List::create(Named("host") = host,
                          Named("dbname") = dbname,
                          Named("status.ok") = conn_status);
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -host.size());
  out.attr("class") = "data.frame";
  return out;
}

//' Asynchronous query processing
//' 
//' Manage an asynchronous query
//' 
//' @param sql a query string
//' @param pars a vector of parameters
//' 
//' @details
//' These functions expose the asynchronous query interface from
//' \code{libpq}. The function \code{async_query} issues a query. Its
//' call is identical to \code{\link{query}} except that it will return
//' immediately. When the issued command is ready, the function
//' \code{async_status} will return a query status object exactly
//' as \code{\link{query}}. Otherwise it will return \code{"BUSY"} to
//' indicate the server has not finished or \code{"DONE"} to indicate
//' there is nothing more to fetch.
//' 
//' If \code{async_status} does not return \code{"DONE"}, then
//' \code{finish_async} must be called to release all results. Otherwise some 
//' result objects will leak, at least until the connection is closed. Note
//' that a call to \code{finish_async} may block until the server is finished
//' processing the command. It calls \code{cancel} internally but there is
//' no guarantee the command will abort.
//' 
//' @return
//' \code{async_query}: true if query was successfully sent (an invalid query
//' will still return true)
//' \code{async_status}: a results status object, possibly indicating an
//' invalid query
//' 
//' @note In practice, you will be much better off using \code{\link{cursor}}
//' as that will usually return very quickly even for large queries, and has
//' the advantage of retrieving the results in chunks. You can call \code{cancel}
//' while a cursor is active. It will simply return \code{PGRES_FATAL_ERROR} is
//' the \code{cancel} is effective.
//' 
//' @author Timothy H. Keitt
//'  
//' @examples
//' \dontrun{
//' # try connecting to default database
//' if ( connect() == "CONNECTION_BAD" )
//' {
//'  system("createdb -w -e")
//'  if ( connect() == "CONNECTION_BAD" )
//'    stop("Cannot connect to database")
//' }
//' 
//' # for kicks work in a schema
//' query("drop schema if exists pqrtesting cascade")
//' query("create schema pqrtesting")
//' query("set search_path to pqrtesting")
//' 
//' # write data frame contents
//' data(mtcars)
//' write_table(mtcars)
//' 
//' # async processing on smallish result
//' # this wont be interesting if your machine is very fast
//' query("begin")
//' async_query("select a.* from mtcars a, mtcars b")
//' repeat
//' {
//'   status = async_status()
//'   if ( status != "BUSY" ) break
//'   cat("busy...\n")
//'   Sys.sleep(1)
//' }
//' print(status)
//' print(head(fetch()))
//' finish_async()
//' Sys.sleep(5)
//' query("rollback")
//' 
//' # async processing on larger result
//' query("begin")
//' async_query("select a.* from mtcars a, mtcars b, mtcars c")
//' count = 0
//' repeat
//' {
//'   status = async_status()
//'   if ( status == "BUSY" )
//'   {
//'     if ( count > 2 )
//'     {
//'       cat("calling cancel...\n")
//'       cancel()
//'     }
//'   }
//'   else break
//'   cat("busy... \n")
//'   Sys.sleep(1)
//'   count = count + 1
//' }
//' print(status)
//' finish_async()
//' query("rollback")
//' 
//' # you can multiplex queries with async_query
//' query("begin")
//' sql1 = "select mpg from mtcars limit 3"
//' sql2 = "select cyl from mtcars limit 4"
//' async_query(paste(sql1, sql2, sep = "; "))
//' while ( async_status() == "BUSY" ) NULL
//' print(fetch())
//' while ( async_status() == "BUSY" ) NULL
//' print(fetch())
//' finish_async()
//' query("rollback")
//' 
//' # finish up
//' disconnect()} 
//' 
//' @export
//' @rdname async
// [[Rcpp::export]]
bool async_query(const char* sql = "", SEXP pars = R_NilValue)
{
  check_conn();
  if ( Rf_isNull(pars) )
    return PQsendQuery(conn, sql) == 1;
  else
    return send_exec_params(sql, pars) == 1;
}

//' @export
//' @rdname async
// [[Rcpp::export]]
CharacterVector async_status()
{
  if ( PQconsumeInput(conn) == 0 )
    stop(PQerrorMessage(conn));
  if ( PQisBusy(conn) == 1 ) return "BUSY";
  PGresult *r = PQgetResult(conn);
  if ( r )
  {
    set_res(r);
    return get_result_status();
  }
  else return "DONE";
}

//' @param stop_on_error call \code{\link{stop}} if cancel request cannot be issued
//' @export
//' @rdname async
// [[Rcpp::export]]
void cancel(const bool stop_on_error = true)
{
  cancel_(stop_on_error);
}

//' @export
//' @rdname async
// [[Rcpp::export]]
void finish_async()
{
  clear_res();
}

