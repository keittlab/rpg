// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// connect_
CharacterVector connect_(CharacterVector keywords, CharacterVector values);
RcppExport SEXP rpg_connect_(SEXP keywordsSEXP, SEXP valuesSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< CharacterVector >::type keywords(keywordsSEXP );
        Rcpp::traits::input_parameter< CharacterVector >::type values(valuesSEXP );
        CharacterVector __result = connect_(keywords, values);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// ping
CharacterVector ping(const char* opts = "");
RcppExport SEXP rpg_ping(SEXP optsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type opts(optsSEXP );
        CharacterVector __result = ping(opts);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// disconnect
void disconnect();
RcppExport SEXP rpg_disconnect() {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        disconnect();
    }
    return R_NilValue;
END_RCPP
}
// clean_up_all
void clean_up_all();
RcppExport SEXP rpg_clean_up_all() {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        clean_up_all();
    }
    return R_NilValue;
END_RCPP
}
// get_conn_error
CharacterVector get_conn_error();
RcppExport SEXP rpg_get_conn_error() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        CharacterVector __result = get_conn_error();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// get_conn_info_
SEXP get_conn_info_();
RcppExport SEXP rpg_get_conn_info_() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        SEXP __result = get_conn_info_();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// result_dim
IntegerVector result_dim();
RcppExport SEXP rpg_result_dim() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        IntegerVector __result = result_dim();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// get_tuple_info
IntegerMatrix get_tuple_info();
RcppExport SEXP rpg_get_tuple_info() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        IntegerMatrix __result = get_tuple_info();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// query
CharacterVector query(const char* sql = "", SEXP pars = R_NilValue);
RcppExport SEXP rpg_query(SEXP sqlSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type sql(sqlSEXP );
        Rcpp::traits::input_parameter< SEXP >::type pars(parsSEXP );
        CharacterVector __result = query(sql, pars);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// query_error
CharacterVector query_error();
RcppExport SEXP rpg_query_error() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        CharacterVector __result = query_error();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// fetch_matrix
CharacterMatrix fetch_matrix();
RcppExport SEXP rpg_fetch_matrix() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        CharacterMatrix __result = fetch_matrix();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// fetch_dataframe
List fetch_dataframe();
RcppExport SEXP rpg_fetch_dataframe() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        List __result = fetch_dataframe();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// trace_conn
void trace_conn(const char* filename = "", bool append = false);
RcppExport SEXP rpg_trace_conn(SEXP filenameSEXP, SEXP appendSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type filename(filenameSEXP );
        Rcpp::traits::input_parameter< bool >::type append(appendSEXP );
        trace_conn(filename, append);
    }
    return R_NilValue;
END_RCPP
}
// untrace_conn
void untrace_conn(bool remove = false);
RcppExport SEXP rpg_untrace_conn(SEXP removeSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< bool >::type remove(removeSEXP );
        untrace_conn(remove);
    }
    return R_NilValue;
END_RCPP
}
// trace_filename
const char* trace_filename();
RcppExport SEXP rpg_trace_filename() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        const char* __result = trace_filename();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// get_conn_defaults
List get_conn_defaults(const bool all = false);
RcppExport SEXP rpg_get_conn_defaults(SEXP allSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const bool >::type all(allSEXP );
        List __result = get_conn_defaults(all);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// libpq_version
int libpq_version();
RcppExport SEXP rpg_libpq_version() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        int __result = libpq_version();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// encrypt_password
const char* encrypt_password(const char* passwd, const char* user);
RcppExport SEXP rpg_encrypt_password(SEXP passwdSEXP, SEXP userSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type passwd(passwdSEXP );
        Rcpp::traits::input_parameter< const char* >::type user(userSEXP );
        const char* __result = encrypt_password(passwd, user);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// get_encoding
const char* get_encoding();
RcppExport SEXP rpg_get_encoding() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        const char* __result = get_encoding();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// set_encoding
bool set_encoding(const char* encoding);
RcppExport SEXP rpg_set_encoding(SEXP encodingSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type encoding(encodingSEXP );
        bool __result = set_encoding(encoding);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// set_error_verbosity
void set_error_verbosity(std::string verbosity);
RcppExport SEXP rpg_set_error_verbosity(SEXP verbositySEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< std::string >::type verbosity(verbositySEXP );
        set_error_verbosity(verbosity);
    }
    return R_NilValue;
END_RCPP
}
// check_transaction
bool check_transaction();
RcppExport SEXP rpg_check_transaction() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        bool __result = check_transaction();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// prepare_
CharacterVector prepare_(const char* sql, const char* name = "");
RcppExport SEXP rpg_prepare_(SEXP sqlSEXP, SEXP nameSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type sql(sqlSEXP );
        Rcpp::traits::input_parameter< const char* >::type name(nameSEXP );
        CharacterVector __result = prepare_(sql, name);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// execute_prepared_
SEXP execute_prepared_(CharacterMatrix pars, const char* name = "");
RcppExport SEXP rpg_execute_prepared_(SEXP parsSEXP, SEXP nameSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< CharacterMatrix >::type pars(parsSEXP );
        Rcpp::traits::input_parameter< const char* >::type name(nameSEXP );
        SEXP __result = execute_prepared_(pars, name);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// num_prepared_params
int num_prepared_params(const char* name = "");
RcppExport SEXP rpg_num_prepared_params(SEXP nameSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type name(nameSEXP );
        int __result = num_prepared_params(name);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// push_conn
void push_conn();
RcppExport SEXP rpg_push_conn() {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        push_conn();
    }
    return R_NilValue;
END_RCPP
}
// pop_conn
void pop_conn();
RcppExport SEXP rpg_pop_conn() {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        pop_conn();
    }
    return R_NilValue;
END_RCPP
}
// swap_conn
void swap_conn();
RcppExport SEXP rpg_swap_conn() {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        swap_conn();
    }
    return R_NilValue;
END_RCPP
}
// rotate_stack
void rotate_stack(const int n = 1);
RcppExport SEXP rpg_rotate_stack(SEXP nSEXP) {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const int >::type n(nSEXP );
        rotate_stack(n);
    }
    return R_NilValue;
END_RCPP
}
// show_conn_stack
List show_conn_stack();
RcppExport SEXP rpg_show_conn_stack() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        List __result = show_conn_stack();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// async_query
bool async_query(const char* sql = "", SEXP pars = R_NilValue);
RcppExport SEXP rpg_async_query(SEXP sqlSEXP, SEXP parsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type sql(sqlSEXP );
        Rcpp::traits::input_parameter< SEXP >::type pars(parsSEXP );
        bool __result = async_query(sql, pars);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// async_status
CharacterVector async_status();
RcppExport SEXP rpg_async_status() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        CharacterVector __result = async_status();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// is_busy
bool is_busy();
RcppExport SEXP rpg_is_busy() {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        bool __result = is_busy();
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// cancel
void cancel();
RcppExport SEXP rpg_cancel() {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        cancel();
    }
    return R_NilValue;
END_RCPP
}
// finish_async
void finish_async();
RcppExport SEXP rpg_finish_async() {
BEGIN_RCPP
    {
        Rcpp::RNGScope __rngScope;
        finish_async();
    }
    return R_NilValue;
END_RCPP
}
// exec_param_serialize
CharacterVector exec_param_serialize(const char* sql, SEXP obj);
RcppExport SEXP rpg_exec_param_serialize(SEXP sqlSEXP, SEXP objSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type sql(sqlSEXP );
        Rcpp::traits::input_parameter< SEXP >::type obj(objSEXP );
        CharacterVector __result = exec_param_serialize(sql, obj);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// fetch_stowed
List fetch_stowed(const char* sql, const char* par);
RcppExport SEXP rpg_fetch_stowed(SEXP sqlSEXP, SEXP parSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        Rcpp::traits::input_parameter< const char* >::type sql(sqlSEXP );
        Rcpp::traits::input_parameter< const char* >::type par(parSEXP );
        List __result = fetch_stowed(sql, par);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
