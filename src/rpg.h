#ifndef __RPG_H__
#define __RPG_H__

#include <libpq-fe.h>

#include <Rcpp.h>
using namespace Rcpp;

#include <RApiSerializeAPI.h>

#include <cstring>

static PGconn* conn = NULL;
static PGresult* res = NULL;
static const char* tracefname = NULL;
static std::FILE* tracef = NULL;

static std::vector<PGconn*> conn_stack;

static void cancel_(const bool stop_on_error = false)
{
  char buff[256];
  memset(buff, '\0', 256);
  PGcancel *obj = PQgetCancel(conn);
  int i = PQcancel(obj, buff, 256);
  PQfreeCancel(obj);
  if ( !i && stop_on_error ) stop(buff);
}

static void clear_res()
{
  // Avoid blocking on PQgetResult
  cancel_();
  do
  {
    PQclear(res);
    res = PQgetResult(conn);
  } while ( res );
}

static void set_res(PGresult* x)
{
  clear_res();
  res = x;
}

static void clear_tracef()
{
  PQuntrace(conn);
  if ( tracef ) fclose(tracef);
  tracef = NULL;
}

static void clear_conn()
{
  clear_res();
  clear_tracef();
  PQfinish(conn);
  conn = NULL;
}

static void set_conn(PGconn* x)
{
  clear_conn();
  conn = x;
}

static void clear_stack()
{
  if ( conn_stack.empty() ) return;
  if ( conn )
  {
    conn_stack.insert(conn_stack.begin(), conn);
    conn = NULL;
  }
  while ( ! conn_stack.empty() )
  {
    set_conn(conn_stack.back());
    conn_stack.pop_back();
  }
}

static void clear_all()
{
  clear_stack();
  clear_conn();
}

static std::vector<const char*>
charvec_to_vec_cstr(CharacterVector x, const bool null_terminate = false)
{
  std::vector<const char*> out(x.size());
  for ( int i = 0; i != x.size(); ++i )
    out[i] = x[i] == NA_STRING ? NULL : as<const char*>(x[i]);
  if ( null_terminate ) out.push_back(NULL);
  return out;
}

static void
rpg_notice_processor(void *arg, const char *message)
{
  Rf_warning(message);
}

static void setup_connection(CharacterVector keywords, CharacterVector values)
{
  std::vector<const char*> kw = charvec_to_vec_cstr(keywords, true),
                           vals = charvec_to_vec_cstr(values, true);
  set_conn(PQconnectdbParams(&kw[0], &vals[0], 1));
  if ( PQstatus(conn) == CONNECTION_OK )
  {
    if ( PQprotocolVersion(conn) < 3 )
      stop("PostgreSQL messaging protocol version < 3 not supported");
    PQsetNoticeProcessor(conn, rpg_notice_processor, NULL);
    PQexec(conn, "set client_min_messages to warning");
  }
}

static void check_conn()
{
  if ( PQstatus(conn) == CONNECTION_BAD )
  {
    Rcout << "No connection... attempting reset... ";
    PQreset(conn);
    if ( PQstatus(conn) == CONNECTION_BAD )
    {
      Rcout << "nope... trying default... ";
      setup_connection(CharacterVector(), CharacterVector());
    }
    if ( PQstatus(conn) == CONNECTION_BAD )
      Rcout << "connection failed (try ping)." << std::endl;
    else
      Rcout << "that worked." << std::endl;
  }
}

static SEXP wrap_string(const char* s)
{
  return s ? wrap(std::string(s)) : R_NilValue;
}

static SEXP connection_status_string()
{
  switch ( PQstatus(conn) )
  {
    case CONNECTION_OK: return wrap("CONNECTION_OK");
    default:            return wrap("CONNECTION_BAD");
  }
}

static SEXP ping_status_string(const char* opts)
{
  switch ( PQping(opts) )
  {
    case PQPING_OK:          return wrap("PQPING_OK");
    case PQPING_REJECT:      return wrap("PQPING_REJECT");
    case PQPING_NO_RESPONSE: return wrap("PQPING_NO_RESPONSE");
    default:                 return wrap("PQPING_NO_ATTEMPT");
  }
}

static CharacterVector connection_error_string()
{
  return PQerrorMessage(conn);
}

static SEXP trasaction_status_string()
{
  switch ( PQtransactionStatus(conn) )
  {
    case PQTRANS_IDLE:    return wrap("PQTRANS_IDLE");
    case PQTRANS_ACTIVE:  return wrap("PQTRANS_ACTIVE");
    case PQTRANS_INTRANS: return wrap("PQTRANS_INTRANS");
    case PQTRANS_INERROR: return wrap("PQTRANS_INERROR");
    default:              return wrap("PQTRANS_UNKNOWN");
  }
}

static SEXP fetch_par(const char* par)
{
  const char* status = PQparameterStatus(conn, par);
  return status ? wrap(CharacterVector(status)) : R_NilValue;
}

static std::vector<const char*> c_str_vec_from_sexp(SEXP x)
{
  
  std::vector<const char*> out;
  x = PROTECT(Rf_coerceVector(x, STRSXP));
  for ( int i = 0; i != Rf_length(x); ++i )
  {
    SEXP csxp = STRING_ELT(x, i);
    out.push_back(csxp == NA_STRING ? '\0' : CHAR(csxp));
  }
  UNPROTECT(1);
  return out;
}

static int send_exec_params(const char* sql, SEXP pars)
{
  std::vector<const char*> vals = c_str_vec_from_sexp(pars);
  return PQsendQueryParams(conn, sql, vals.size(), NULL, &vals[0], NULL, NULL, 0);  
}

static void exec_params(const char* sql, SEXP pars)
{
  std::vector<const char*> vals = c_str_vec_from_sexp(pars);
  set_res(PQexecParams(conn, sql, vals.size(), NULL, &vals[0], NULL, NULL, 0));  
}

static void exec_prepared(CharacterVector pars, const char* name = "")
{
  std::vector<const char*> vals = charvec_to_vec_cstr(pars);
  set_res(PQexecPrepared(conn, name, vals.size(), &vals[0], NULL, NULL, 0)); 
}

static void exec_prepared_rows(CharacterMatrix pars, const char* name = "")
{
  for ( int i = 0; i != pars.nrow(); ++i )
  {
    CharacterVector row = pars.row(i);
    std::vector<const char*> vals = charvec_to_vec_cstr(row);
    set_res(PQexecPrepared(conn, name, vals.size(), &vals[0], NULL, NULL, 0));
    if ( PQresultStatus(res) == PGRES_FATAL_ERROR ) return;
  }
}

static SEXP fetch_string(const int row = 0, const int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_STRING;
  std::string val = PQgetvalue(res, row, col);
  return Rf_mkChar(val.c_str());
}

static SEXP fetch_binary_text(const int row = 0, const int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_STRING;
  char* val = PQgetvalue(res, row, col) + 1;
  int i = PQgetlength(res, row, col);
  val[i - 2] = '\0';
  return Rf_mkChar(val);
}

static int fetch_int(const int row = 0, const int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_INTEGER;
  std::string val = PQgetvalue(res, row, col);
  return atoi(val.c_str());
}

static double fetch_double(const int row = 0, const int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_REAL;
  std::string val = PQgetvalue(res, row, col);
  return atof(val.c_str());
}

static int fetch_bool(const int row = 0, const int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_LOGICAL;
  std::string val = PQgetvalue(res, row, col);
  return val.compare("t") ? 0 : 1; // weird
}

static Date fetch_date(const int row = 0, const int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_INTEGER;
  std::string val = PQgetvalue(res, row, col);
  return Date(val);
}

static SEXP fetch_column(const int col = 0)
{
  int nrow = PQntuples(res);
  switch ( PQftype(res, col) )
  {
    case 16:
    {
      LogicalVector out(nrow);
      for ( int row = 0; row != nrow; ++row )
        out[row] = fetch_bool(row, col);
      return wrap(out);
    }
    case 20:
    case 21:
    case 23:
    {
      IntegerVector out(nrow);
      for ( int row = 0; row != nrow; ++row )
        out[row] = fetch_int(row, col);
      return wrap(out);
    }
    case 700:
    case 701:
    {
      NumericVector out(nrow);
      for ( int row = 0; row != nrow; ++row )
        out[row] = fetch_double(row, col);
      return wrap(out);
    }
    case 1082:
    {
      DateVector out(nrow);
      for ( int row = 0; row != nrow; ++row )
        out[row] = fetch_date(row, col);
      return wrap(out);
    }
    default:
    {
      CharacterVector out(nrow);
      for ( int row = 0; row != nrow; ++row )
        out[row] = fetch_string(row, col);
      return wrap(out);
    }
  }
  return R_NilValue;
}

static const char* tempfile()
{
  Function tf("tempfile");
  return as<const char*>(tf());
}

static void unlink_file(const char* filename)
{
  Function ul("unlink");
  ul(filename);
}

static CharacterVector make_status(const char* status,
                                   const bool with_err = false,
                                   const bool with_cmd = false)
{
  CharacterVector out(status);
  if ( with_err ) out.attr("error.message") = wrap_string(PQresultErrorMessage(res));
  if ( with_cmd ) out.attr("command.status") = wrap_string(PQcmdStatus(res));
  out.attr("class") = "pq.status";
  return out;
}

static CharacterVector result_status(const bool with_err = true,
                                     const bool with_cmd = true)
{
  return make_status(PQresStatus(PQresultStatus(res)), with_err, with_cmd);
}

#endif // __RPG_H__
