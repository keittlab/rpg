#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rpg_connect_(SEXP keywordsSEXP, SEXP valuesSEXP);
extern SEXP rpg_ping(SEXP optsSEXP);
extern SEXP rpg_disconnect_();
extern SEXP rpg_clean_up_all();
extern SEXP rpg_get_conn_error();
extern SEXP rpg_get_conn_info_();
extern SEXP rpg_result_dim();
extern SEXP rpg_get_tuple_info();
extern SEXP rpg_query(SEXP sqlSEXP, SEXP parsSEXP);
extern SEXP rpg_query_error();
extern SEXP rpg_fetch_matrix();
extern SEXP rpg_fetch_dataframe();
extern SEXP rpg_trace_conn(SEXP filenameSEXP, SEXP appendSEXP);
extern SEXP rpg_untrace_conn(SEXP removeSEXP);
extern SEXP rpg_trace_filename();
extern SEXP rpg_get_conn_defaults(SEXP allSEXP);
extern SEXP rpg_libpq_version();
extern SEXP rpg_encrypt_password(SEXP passwdSEXP, SEXP userSEXP);
extern SEXP rpg_get_encoding();
extern SEXP rpg_set_encoding(SEXP encodingSEXP);
extern SEXP rpg_set_error_verbosity(SEXP verbositySEXP);
extern SEXP rpg_check_transaction();
extern SEXP rpg_prepare_(SEXP sqlSEXP, SEXP nameSEXP);
extern SEXP rpg_execute_prepared_(SEXP parsSEXP, SEXP nameSEXP);
extern SEXP rpg_num_prepared_params(SEXP nameSEXP);
extern SEXP rpg_push_conn();
extern SEXP rpg_pop_conn();
extern SEXP rpg_swap_conn();
extern SEXP rpg_rotate_stack(SEXP nSEXP);
extern SEXP rpg_show_conn_stack();
extern SEXP rpg_async_query(SEXP sqlSEXP, SEXP parsSEXP);
extern SEXP rpg_async_status();
extern SEXP rpg_is_busy();
extern SEXP rpg_cancel();
extern SEXP rpg_finish_async();
extern SEXP rpg_exec_param_serialize(SEXP sqlSEXP, SEXP objSEXP);
extern SEXP rpg_fetch_stowed(SEXP sqlSEXP, SEXP parSEXP);
extern SEXP rpg_register_return_formatter(SEXP pgoidSEXP, SEXP fSEXP);
extern SEXP rpg_toggle_echo();

R_CallMethodDef callMethods[]  = {
	{"rpg_connect_", (DL_FUNC) &rpg_connect_, 2},
	{"rpg_ping", (DL_FUNC) &rpg_ping, 1},
	{"rpg_disconnect_", (DL_FUNC) &rpg_disconnect_, 0},
	{"rpg_clean_up_all", (DL_FUNC) &rpg_clean_up_all, 0},
	{"rpg_get_conn_error", (DL_FUNC) &rpg_get_conn_error, 0},
	{"rpg_get_conn_info_", (DL_FUNC) &rpg_get_conn_info_, 0},
	{"rpg_result_dim", (DL_FUNC) &rpg_result_dim, 0},
	{"rpg_get_tuple_info", (DL_FUNC) &rpg_get_tuple_info, 0},
	{"rpg_query", (DL_FUNC) &rpg_query, 2},
	{"rpg_query_error", (DL_FUNC) &rpg_query_error, 0},
	{"rpg_fetch_matrix", (DL_FUNC) &rpg_fetch_matrix, 0},
	{"rpg_fetch_dataframe", (DL_FUNC) &rpg_fetch_dataframe, 0},
	{"rpg_trace_conn", (DL_FUNC) &rpg_trace_conn, 2},
	{"rpg_untrace_conn", (DL_FUNC) &rpg_untrace_conn, 1},
	{"rpg_trace_filename", (DL_FUNC) &rpg_trace_filename, 0},
	{"rpg_get_conn_defaults", (DL_FUNC) &rpg_get_conn_defaults, 1},
	{"rpg_libpq_version", (DL_FUNC) &rpg_libpq_version, 0},
	{"rpg_encrypt_password", (DL_FUNC) &rpg_encrypt_password, 2},
	{"rpg_get_encoding", (DL_FUNC) &rpg_get_encoding, 0},
	{"rpg_set_encoding", (DL_FUNC) &rpg_set_encoding, 1},
	{"rpg_set_error_verbosity", (DL_FUNC) &rpg_set_error_verbosity, 1},
	{"rpg_check_transaction", (DL_FUNC) &rpg_check_transaction, 0},
	{"rpg_prepare_", (DL_FUNC) &rpg_prepare_, 2},
	{"rpg_execute_prepared_", (DL_FUNC) &rpg_execute_prepared_, 2},
	{"rpg_num_prepared_params", (DL_FUNC) &rpg_num_prepared_params, 1},
	{"rpg_push_conn", (DL_FUNC) &rpg_push_conn, 0},
	{"rpg_pop_conn", (DL_FUNC) &rpg_pop_conn, 0},
	{"rpg_swap_conn", (DL_FUNC) &rpg_swap_conn, 0},
	{"rpg_rotate_stack", (DL_FUNC) &rpg_rotate_stack, 1},
	{"rpg_show_conn_stack", (DL_FUNC) &rpg_show_conn_stack, 0},
	{"rpg_async_query", (DL_FUNC) &rpg_async_query, 2},
	{"rpg_async_status", (DL_FUNC) &rpg_async_status, 0},
	{"rpg_is_busy", (DL_FUNC) &rpg_is_busy, 0},
	{"rpg_cancel", (DL_FUNC) &rpg_cancel, 0},
	{"rpg_finish_async", (DL_FUNC) &rpg_finish_async, 0},
	{"rpg_exec_param_serialize", (DL_FUNC) &rpg_exec_param_serialize, 2},
	{"rpg_fetch_stowed", (DL_FUNC) &rpg_fetch_stowed, 2},
	{"rpg_register_return_formatter", (DL_FUNC) &rpg_register_return_formatter, 2},
	{"rpg_toggle_echo", (DL_FUNC) &rpg_toggle_echo, 0},
	{NULL, NULL, 0}};

void
R_init_myLib(DllInfo *info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
	R_useDynamicSymbols(info, FALSE);
	R_forceSymbols(info, TRUE);
}