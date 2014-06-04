#ifndef __EZPG_H__
#define __EZPG_H__

#include <libpq-fe.h>

#include <Rcpp.h>
using namespace Rcpp;

static PGconn* conn = NULL;
static PGresult* res = NULL;
static const char* tracefname = NULL;
static std::FILE* tracef = NULL;

static void clear_res()
{
  PQclear(res);
  res = NULL;
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

static void
ezpg_notice_processor(void *arg, const char *message)
{
    // Rcout << message;
}

static void setup_connection(const char* opts = "")
{
  conn = PQconnectdb(opts);
  if ( PQstatus(conn) == CONNECTION_OK )
    PQsetNoticeProcessor(conn, ezpg_notice_processor, NULL);
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
      setup_connection("");
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
  for ( int i = 0; i < Rf_length(x); ++i )
  {
    SEXP csxp = STRING_ELT(x, i);
    out.push_back(csxp == NA_STRING ? '\0' : CHAR(csxp));
  }
  UNPROTECT(1);
  return out;
}

static void exec_params(const char* sql = "", SEXP pars = R_NilValue)
{
  std::vector<const char*> vals = c_str_vec_from_sexp(pars);
  res = PQexecParams(conn, sql, vals.size(), NULL, &vals[0], NULL, NULL, 0);  
}

static SEXP fetch_string(int row = 0, int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_STRING;
  std::string val = PQgetvalue(res, row, col);
  return Rf_mkChar(val.c_str());
}

static int fetch_int(int row = 0, int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_INTEGER;
  std::string val = PQgetvalue(res, row, col);
  return atoi(val.c_str());
}

static double fetch_double(int row = 0, int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_REAL;
  std::string val = PQgetvalue(res, row, col);
  return atof(val.c_str());
}

static int fetch_bool(int row = 0, int col = 0)
{
  if ( PQgetisnull(res, row, col) ) return NA_LOGICAL;
  std::string val = PQgetvalue(res, row, col);
  return val.compare("t") ? 0 : 1; // weird
}

static Date fetch_date(int row = 0, int col = 0)
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
      for ( int row = 0; row < nrow; ++row )
        out[row] = fetch_bool(row, col);
      return wrap(out);
    }
    case 20:
    case 21:
    case 23:
    {
      IntegerVector out(nrow);
      for ( int row = 0; row < nrow; ++row )
        out[row] = fetch_int(row, col);
      return wrap(out);
    }
    case 700:
    case 701:
    {
      NumericVector out(nrow);
      for ( int row = 0; row < nrow; ++row )
        out[row] = fetch_double(row, col);
      return wrap(out);
    }
    case 1082:
    {
      DateVector out(nrow);
      for ( int row = 0; row < nrow; ++row )
        out[row] = fetch_date(row, col);
      return wrap(out);
    }
    default:
    {
      CharacterVector out(nrow);
      for ( int row = 0; row < nrow; ++row )
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

#endif // __EZPG_H__