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
  clear_res(); check_conn();
  if ( PQprotocolVersion(conn) > 2 && ! Rf_isNull(pars) )
    exec_params(sql, pars);
  else res = PQexec(conn, sql);
  CharacterVector out(PQresStatus(PQresultStatus(res)));
  out.attr("error.message") = wrap_string(PQresultErrorMessage(res));
  out.attr("class") = "pq.status";
  return out;
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