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

static DataFrame retrieveRows()
{
  int nc = PQnfields(res),
      nr = PQntuples(res);
  List df(nc);
  CharacterVector cnames(nc),
                  rnames(nr);
  for ( int i = 0; i < nc; ++i )
  {
    cnames[i] = PQfname(res, i);
    df[i] = CharacterVector(nr);
  }
  for ( int i = 0; i < nr; ++i )
  {
    rnames[i] = i;
    for ( int j = 0; j < nc; ++j )
    {
      CharacterVector col = df[j];
      col[i] = PQgetvalue(res, i, j);
    }
  }
  df.attr("names") = cnames;
  df.attr("row.names") = rnames;
  df.attr("class") = "data.frame";
  return df;
}

#endif // __EZPG_H__
