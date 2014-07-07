#include "rpg.h"

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
  info["password.used"] = wrap_string(PQpass(conn));
  #ifdef USE_SSL
  info["encrypted"] = PQgetssl(conn) != NULL;
  #else
  info["encrypted"] = false;
  #endif
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

//' @return \code{result_dim} returns the number of tuples and fields
//' @rdname query
//' @export
// [[Rcpp::export]]
IntegerVector result_dim()
{
  IntegerVector out(2);
  out[0] = PQntuples(res);
  out[1] = PQnfields(res);
  return out;
}

// [[Rcpp::export]]
IntegerMatrix get_tuple_info()
{
  int nrow = PQntuples(res),
      ncol = PQnfields(res);
  IntegerMatrix out(6, ncol);
  CharacterVector coln(ncol), rown(6);
  for ( int i = 0; i != ncol; ++i )
  {
    coln[i] = PQfname(res, i);
    out(0, i) = PQftable(res, i);
    out(1, i) = PQftablecol(res, i);
    out(2, i) = PQfformat(res, i);
    out(3, i) = PQftype(res, i);
    out(4, i) = PQfmod(res, i);
    out(5, i) = PQfsize(res, i);
  }
  rown[0] = "Table OID";
  rown[1] = "Table column OID";
  rown[2] = "Field format";
  rown[3] = "Field type";
  rown[4] = "Field type modifier";
  rown[5] = "Field server storage size";
  out.attr("dimnames") = List::create(rown, coln);
  return out;
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
//' @seealso \code{\link{psql}}
//' 
//' @examples
//' \dontrun{
//' system("createdb rpgtesting")
//' connect("rpgtesting")
//' begin()
//' execute("DROP SCHEMA IF EXISTS rpgtesting CASCADE")
//' execute("CREATE SCHEMA rpgtesting")
//' execute("SET search_path TO rpgtesting")
//' execute("DROP TABLE IF EXISTS test")
//' execute("CREATE TABLE test (id integer, field text)")
//' query("INSERT INTO test VALUES ($1, $2)", c(1, "test"))
//' fetch("SELECT * FROM test")
//' result_dim()
//' fetch("SELECT * FROM testing")
//' query_error()
//' rollback()
//' disconnect()
//' system("dropdb rpgtesting")}
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
  return result_status();
}

//' @return \code{query_error} returns an error string
//' @export query_error
//' @rdname query
// [[Rcpp::export]]
CharacterVector query_error()
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
  int nrow = PQntuples(res),
      ncol = PQnfields(res);
  if ( nrow == 0 ||
       ncol == 0 ) return List();
  List out(ncol);
  CharacterVector names(ncol);
  for ( int col = 0; col < ncol; ++col )
  {
    std::string colname = PQfname(res, col);
    out[col] = fetch_column(col);
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
//' passing between \code{rpg} and the database server.
//' 
//' \code{trace_conn} begins tracing and \code{untrace_conn}
//' stops tracing.
//' 
//' @author Timothy H. Keitt
//' 
//' @examples
//' \dontrun{
//' system("createdb rpgtesting")
//' connect("rpgtesting")
//' trace_conn()
//' list_tables()
//' dump_conn_trace(n = 40)
//' untrace_conn(remove = TRUE)
//' disconnect()
//' system("dropdb rpgtesting")}
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
  if ( remove && tracefname ) unlink_file(tracefname);
  clear_tracef();
}

//' @return \code{trace_filename}: the name of the file containing
//' trace information.
//' 
//' @rdname tracing
//' @export
// [[Rcpp::export]]
const char* trace_filename()
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
    if ( all || (i->val && strlen(i->val)) )
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
//' 
//' @examples
//' \dontrun{
//' # try connecting to default database
//' system("createdb rpgtesting")
//' connect("rpgtesting")
//' begin()
//' 
//' libpq_version()
//' encrypt_password("test", "tester")
//' get_encoding()
//' set_encoding("UTF8")
//' set_error_verbosity("terse")
//' set_error_verbosity("verbose")
//' set_error_verbosity("default")
//'
//' # cleanup
//' rollback()
//' disconnect()
//' system("dropdb rpgtesting")}
//' 
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
bool check_transaction()
{
  check_conn();
  switch ( PQtransactionStatus(conn)  )
  {
    case PQTRANS_INERROR:
      Rcout << "Error in current transaction: rolling back...";
      query("ROLLBACK");
      Rcout << " done." << std::endl;
    case PQTRANS_IDLE:
      Rcout << "Initiating new transaction block...";
      query("BEGIN");
      Rcout << " done." << std::endl;
      return false;
    case PQTRANS_UNKNOWN:
      stop("No active connection");
    default:
      return true;
  }
  return false; // compiler warning
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
//' specified in the prepared statement, an error condition is raised.
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
//' system("createdb rpgtesting")
//' connect("rpgtesting")
//' begin()
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
//' sql = paste0("INSERT INTO mtcars VALUES (", pars, ")", collapse = " ")
//' prepare(sql, "test_statement")
//' execute_prepared(mtcars, "test_statement")
//' read_table(mtcars, limit = 5)
//' 
//' # cleanup
//' rollback()
//' disconnect()
//' system("dropdb rpgtesting")}
//' 
//' @rdname prepare
//' @export
// [[Rcpp::export]]
CharacterVector prepare(const char* sql, const char* name = "")
{
  check_conn();
  set_res(PQprepare(conn, name, sql, 0, NULL));
  return result_status();  
}

// [[Rcpp::export]]
SEXP execute_prepared_(CharacterMatrix pars, const char* name = "")
{
  exec_prepared_rows(pars, name);
  return result_status();
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
//' only used for their side-effects. \code{\link{rpg}} stores an active connection
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
//' # make some databases
//' dbs = paste0("rpgdb", 1:3)
//' lapply(paste("createdb", dbs), system)
//'
//' # connect
//' connect(dbname = dbs[1]); push_conn()
//' connect(dbname = dbs[2]); push_conn()
//' connect(dbname = dbs[3]); push_conn()
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
//' lapply(paste("dropdb", dbs), system)}
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
void rotate_stack(const int n = 1)
{
  int ss = conn_stack.size();
  if ( ss > 1 && n > 0 )
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
//' you should call \code{finish_async} to free pending results. Note
//' that a call to \code{finish_async} may block until the server is finished
//' processing the command. It calls \code{cancel} internally but there is
//' no guarantee the command will abort.
//' 
//' @return
//' \code{async_query}: true if query was successfully sent (an invalid query
//' will still return true)
//' 
//' \code{async_status}: a results status object, possibly indicating an
//' invalid query
//' 
//' \code{is_busy}: a boolean
//' 
//' @note In practice, you will be much better off using \code{\link{cursor}}
//' as that will usually return very quickly even for large queries, and has
//' the advantage of retrieving the results in chunks. You can call \code{cancel}
//' while a cursor is active. The cursor will return \code{PGRES_FATAL_ERROR} if
//' the \code{cancel} is effective. Alternately, issuing any query that sets the
//' result status will have the same effect as \code{finish_async}.
//' 
//' @author Timothy H. Keitt
//'  
//' @examples
//' \dontrun{
//' # create a database
//' system("createdb rpgtesting")
//' connect("rpgtesting")
//' begin()
//' 
//' # write data frame contents
//' data(mtcars)
//' write_table(mtcars)
//' 
//' # async processing on smallish result
//' # this wont be interesting if your machine is very fast
//' async_query("SELECT a.* FROM mtcars a, mtcars b")
//' repeat
//' {
//'   status = async_status()
//'   if ( status != "BUSY" ) break
//'   cat("busy...\n")
//'   Sys.sleep(1)
//' }
//' print(status)
//' head(fetch())
//' finish_async()
//' Sys.sleep(1)
//' 
//' # async processing on larger result
//' async_query("SELECT a.* FROM mtcars a, mtcars b, mtcars c")
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
//' 
//' # you can run multiple queries with async_query
//' rollback(); begin()
//' write_table(mtcars)
//' sql1 = "SELECT mpg FROM mtcars LIMIT 3"
//' sql2 = "SELECT cyl FROM mtcars LIMIT 4"
//' async_query(paste(sql1, sql2, sep = "; "))
//' while ( async_status() == "BUSY" ) NULL
//' fetch()
//' while ( is_busy() ) NULL
//' async_status()
//' fetch()
//' finish_async()
//' 
//' # issue an async query and come back later
//' async_query(sql1)
//' push_conn()
//' connect("rpgtesting")
//' 
//' # fails because of transaction isolation
//' fetch(sql2)
//' pop_conn()
//' async_status()
//' 
//' # results from sql1
//' fetch()
//' 
//' # this is automatic if you issue new queries
//' finish_async()
//' 
//' # cleanup
//' rollback()
//' disconnect()
//' system("dropdb rpgtesting")} 
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

//' @details Any pending results will be lost if you call \code{\link{query}},
//' \code{\link{execute}} or \code{\link{fetch}} with a \code{sql} string prior
//' to \code{async_query} returning \code{DONE}. If you need to issue queries
//' while waiting on an async call, then use \code{\link{push_conn}} to save
//' the query state, \code{\link{connect}} to make a new connetion, and then
//' \code{\link{pop_conn}} followed by \code{async_status}.
//' @export
//' @rdname async
// [[Rcpp::export]]
CharacterVector async_status()
{
  if ( PQconsumeInput(conn) == 0 ) stop(PQerrorMessage(conn));
  if ( PQisBusy(conn) == 1 ) return make_status("BUSY");
  PGresult *r = PQgetResult(conn);
  if ( r )
  {
    PQclear(res);
    res = r;
    return result_status();
  }
  else return make_status("DONE");
}

//' @details \code{is_busy} is a slightly faster shortcut to check whether the
//' server has completed the query. You must still call \code{async_status} to
//' fetch the results.
//' @export
//' @rdname async
// [[Rcpp::export]]
bool is_busy()
{
  if ( PQconsumeInput(conn) == 0 )
    stop(PQerrorMessage(conn));
  return PQisBusy(conn) == 1;
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

// [[Rcpp::export]]
CharacterVector exec_param_serialize(const char* sql, SEXP obj)
{
  SEXP bin = serializeToRaw(obj);
  int binfmt = 1, len = Rf_length(bin);
  const char *binvals = (const char*) RAW(bin);
  set_res(PQexecParams(conn, sql, 1, NULL, &binvals, &len, &binfmt, 0));
  return result_status();
}

// [[Rcpp::export]]
List fetch_stowed(const char* sql, const char* par)
{
  check_conn();
  set_res(PQexecParams(conn, sql, 1, NULL, &par, NULL, NULL, 1));
  int nrow = PQntuples(res),
      ncol = PQnfields(res);
  if ( nrow == 0 ) return List();
  if ( ncol != 2 ) stop("Invalid query");
  List out(nrow);
  for ( int i = 0; i != nrow; ++i )
  {
    const char *byteadat = PQgetvalue(res, i, 1);
    int bdlen = PQgetlength(res, i, 1);
    RawVector bd(byteadat, byteadat + bdlen);
    out[i] = unserializeFromRaw(wrap(bd));
  }
  CharacterVector names(nrow);
  for ( int i = 0; i != nrow; ++i )
    names[i] = fetch_binary_text(i, 0);
  out.attr("names") = names;
  return out;
}

