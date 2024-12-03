// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// pre_filter_cpp
DataFrame pre_filter_cpp(DataFrame df, DataFrame df_population, double threshold, double max_T, CharacterVector var_type_override);
RcppExport SEXP _phs7045midterm_pre_filter_cpp(SEXP dfSEXP, SEXP df_populationSEXP, SEXP thresholdSEXP, SEXP max_TSEXP, SEXP var_type_overrideSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type df(dfSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type df_population(df_populationSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< double >::type max_T(max_TSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type var_type_override(var_type_overrideSEXP);
    rcpp_result_gen = Rcpp::wrap(pre_filter_cpp(df, df_population, threshold, max_T, var_type_override));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_phs7045midterm_pre_filter_cpp", (DL_FUNC) &_phs7045midterm_pre_filter_cpp, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_phs7045midterm(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
