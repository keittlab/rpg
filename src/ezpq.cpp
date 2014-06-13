#include "ezpg.h"

//' PostgreSQL connection
//' 
//' Manage database connection
//' 
//' @param opts connection parameters
//' 
//' @details If no connection parameters are supplied, the
//' connection will fallback to default parameters. Usually
//' this establishes a connection on the localhost to a database,
//' if it exists, with the same name as the user.
//'
//' Valid keywords and their defaults can be obtained by calling
//' \code{get_conn_defaults()}. 
//' 
//' @note Do not open a connection and then fork the R
//' process. The behavior will be unpredictable. It is perfectly
//' acceptable however to call \code{connect} within each
//' forked instance.
//' 
//' Be careful with \code{ping} as it will attemp to connect to
//' the database server on a remote host, which might not be
//' appreciated by a remote administator. Also, \code{ping} may
//' seem to hang for a long time. It is just polling the connection
//' until it times out.
//' 
//' @return
//' \code{connect} returns one of:
//' \tabular{ll}{
//' \code{CONNECTION_OK} \tab Succesful connection \cr
//' \code{CONNECTION_BAD} \tab Connection failed \cr}
//' 
//' @author Timothy H. Keitt
//' 
//' @examples
//' \dontrun{
//' ping("connect_timeout = 5, host = www.r-project.org")
//' connect("host = localhost")
//' fetch("show search_path")}
//' get_conn_defaults()
//' get_conn_info()
//' get_conn_error()
//' \dontrun{
//' disconnect()}
//' 
//' @export connect
//' @rdname connection
// [[Rcpp::export]]
CharacterVector connect(const char* opts = "")
{
  clear_conn();
  setup_connection(opts);
  CharacterVector out(connection_status_string());
  out.attr("error.message") = connection_error_string();
  out.attr("class") = "pq.status";
  return out;
}

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
//' @export ping
//' @rdname connection
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

//' @return get_conn_error: an error string
//' @export get_conn_error
//' @rdname connection
// [[Rcpp::export]]
CharacterVector get_conn_error()
{
  CharacterVector err(PQerrorMessage(conn));
  err.attr("class") = "pq.error.message";
  return err;
}

//' @details \code{get_conn_info} returns a list containing
//' information about the current connection. For
//' readability, it will print as though it is a matrix. If
//' you want to see it as a list, try \code{unclass(get_conn_info())}.
//' @return get_conn_info: a list of values
//' @export get_conn_info
//' @rdname connection
// [[Rcpp::export]]
SEXP get_conn_info()
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
//' @rdname query
//' @export query
// [[Rcpp::export]]
CharacterVector query(const char* sql = "", SEXP pars = R_NilValue)
{
  check_conn();
  if ( PQprotocolVersion(conn) > 2 && ! Rf_isNull(pars) )
    exec_params(sql, pars);
  else set_res(PQexec(conn, sql));
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
//' passing between \code{ezpg} and the database server.
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
//' dump_conn_trace(n = 10)
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

//' @details \code{get_conn_defaults} returns a data frame containing
//' all of the possible connection string keywords, the names of environment
//' variables used to override the defaults, the compiled in default value
//' and the current value of the keyword.
//' @return \code{get_conn_defaults}: a data frame with defaults listed
//' @rdname connection
//' @export
// [[Rcpp::export]]
DataFrame get_conn_defaults()
{
  std::vector<std::string> kw, ev, cp, va;
  PQconninfoOption *defs = PQconndefaults(), *i = defs;
  while ( i && i->keyword )
  {
        kw.push_back(i->keyword);
        ev.push_back(i->envvar ? i->envvar : "");
        cp.push_back(i->compiled ? i->compiled : "");
        va.push_back(i->val ? i->val : "");
        ++i;
  }
  PQconninfoFree(defs);
  return DataFrame::create(Named("keyword") = kw,
                           Named("envvar") = ev,
                           Named("compiled") = cp,
                           Named("value") = va);
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
//' If the number of parameters supplied to \code{execute} is a
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
//' when calling \code{execute}. This allows multiple prepared queries.
//' If you do not supply a name, each call to \code{prepare} will
//' overwrite the previous prepared statement. The lifetime of
//' a prepared statement is the lifetime of the current connection
//' or transaction.
//' 
//' @author Timothy H. Keitt
//' 
//' @examples
//' \dontrun{
//' system("createdb -w -e ezpgtestingdb")
//' connect("dbname = ezpgtestingdb")
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
//' execute(mtcars, "test_statement")
//' read_table(mtcars, limit = 5)
//' 
//' # drop the database
//' disconnect()
//' system("dropdb -w -e ezpgtestingdb")}
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
SEXP execute_(CharacterMatrix pars, const char* name = "")
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