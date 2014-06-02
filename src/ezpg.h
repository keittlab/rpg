#ifndef __EZPG_H__
#define __EZPG_H__

#include <libpq-fe.h>

#include <Rcpp.h>
using namespace Rcpp;

static PGconn* conn = NULL;
static PGresult* res = NULL;

static void clear_res()
{
  if ( res ) PQclear(res);
  res = NULL;
}

static void clear_conn()
{
  clear_res();
  if ( conn ) PQfinish(conn);
  conn = NULL;
}

static void check_conn(const char* opts = "")
{
  if ( ! conn )
    conn = PQconnectdb(opts);
}

static SEXP wrap_string(const char* s)
{
  return s ? wrap(CharacterVector(s)) : R_NilValue;
}

static CharacterVector connection_status_string()
{
  ConnStatusType status = PQstatus(conn);
  if ( status == CONNECTION_OK ) return "CONNECTION_OK";
  return "CONNECTION_BAD";
}

static CharacterVector ping_status_string(const char* opts)
{
  PGPing status = PQping(opts);
  if ( status == PQPING_OK ) return "PQPING_OK";
  if ( status == PQPING_REJECT) return "PQPING_REJECT";
  if ( status == PQPING_NO_RESPONSE ) return "PQPING_NO_RESPONSE";
  if ( status == PQPING_NO_ATTEMPT ) return "PQPING_NO_ATTEMPT";
  return "Ping returned an unknown status code";
}
static CharacterVector connection_error_string()
{
  return PQerrorMessage(conn);
}

static SEXP trasaction_status_string()
{
  if ( PQstatus(conn) == CONNECTION_BAD ) return R_NilValue;
  PGTransactionStatusType status = PQtransactionStatus(conn);
  std::string out("");
  if ( status == PQTRANS_IDLE ) out = "PQTRANS_IDLE";
  if ( status == PQTRANS_ACTIVE )  out = "PQTRANS_ACTIVE";
  if ( PQTRANS_INTRANS ) out = "PQTRANS_INTRANS";
  if ( PQTRANS_INERROR ) out = "PQTRANS_INERROR";
  if ( PQTRANS_UNKNOWN ) out =  "PQTRANS_UNKNOWN";
  return wrap(out);
}

static SEXP fetch_par(const char* par)
{
  const char* status = PQparameterStatus(conn, par);
  return status ? wrap(CharacterVector(status)) : R_NilValue;
}

static std::vector<const char*> c_str_vec_from_sexp(SEXP x)
{
  CharacterVector y(x);
  std::vector<const char*> out;
  for ( int i = 0; i < y.size(); ++i )
    if ( Rf_isNull(y[i]) ) out.push_back(0);
    else out.push_back(std::string(y[i]).c_str());
  return out;
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

static SEXP fetch_column(const int col = 0)
{
  int nrow = PQntuples(res);
  switch ( PQftype(res, col) )
  {
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

#endif // __EZPG_H__
