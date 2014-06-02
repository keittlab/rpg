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
//' See the PostgreSQL documentation (\url{http://www.postgresql.org})
//' for default and valid connection parameters.
//' 
//' Warning: do not open a connection and then fork the R
//' process. The behavior will be unpredictable. It is perfectly
//' acceptable however to call \code{connect} within each
//' forked instance.
//' 
//' @return
//' \code{connect} returns one of:
//' \tabular{ll}{
//' \code{CONNECTION_OK} \tab Succesful connection \cr
//' \code{CONNECTION_BAD} \tab Connection failed \cr}
//' 
//' @examples
//' \dontrun{
//' if ( connect("dbname=testing") )
//' {
//'   query("select * from testtab")
//'   disconnect()
//' }
//' }
//' 
//' @export connect
//' @rdname connection
// [[Rcpp::export]]
CharacterVector connect(const char* opts = "")
{
  clear_conn();
  conn = PQconnectdb(opts);
  CharacterVector out(connection_status_string());
  out.attr("error.message") = connection_error_string();
  out.attr("class") = "pq.status";
  return out;
}

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
  CharacterVector out(ping_status_string(opts));
  out.attr("error.message") = connection_error_string();
  out.attr("class") = "pq.status";
  return out;
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

//' @return connErr: an error string
//' @export connErr
//' @rdname connection
// [[Rcpp::export]]
CharacterVector connErr()
{
  CharacterVector err(PQerrorMessage(conn));
  err.attr("class") = "pq.error.message";
  return err;
}

//' @details \code{connInfo} returns a list containing
//' information about the current connection. For
//' readability, it will print as though it is a matrix. If
//' you want to see it as a list, try \code{unclass(connInfo())}.
//' @return connInfo: a list of values
//' @export connInfo
//' @rdname connection
// [[Rcpp::export]]
SEXP connInfo()
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
  info.attr("class") = "connInfo";
  return wrap(info);
}

//' PostgreSQL query
//' 
//' Issue a query to the current database connection
//' 
//' @param sql a query string
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
CharacterVector query(const char* sql = "")
{
  clear_res();
  res = PQexec(conn, sql);
  CharacterVector out(PQresStatus(PQresultStatus(res)));
  out.attr("error.message") = wrap_string(PQresultErrorMessage(res));
  out.attr("class") = "pq.status";
  return out;
}

//' @return \code{queryErr} returns an error string
//' @export queryErr
//' @rdname query
// [[Rcpp::export]]
CharacterVector queryErr()
{
  CharacterVector err(PQresultErrorMessage(res));
  err.attr("class") = "pq.error.message";
  return err;
}

