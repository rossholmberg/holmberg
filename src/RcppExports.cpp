// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// idDub_allRows
NumericMatrix idDub_allRows(NumericVector inputlat, NumericVector inputlon, NumericMatrix inputdata, NumericVector outputlat, NumericVector outputlon, NumericVector landmask);
RcppExport SEXP _holmberg_idDub_allRows(SEXP inputlatSEXP, SEXP inputlonSEXP, SEXP inputdataSEXP, SEXP outputlatSEXP, SEXP outputlonSEXP, SEXP landmaskSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type inputlat(inputlatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type inputlon(inputlonSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type inputdata(inputdataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outputlat(outputlatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outputlon(outputlonSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type landmask(landmaskSEXP);
    rcpp_result_gen = Rcpp::wrap(idDub_allRows(inputlat, inputlon, inputdata, outputlat, outputlon, landmask));
    return rcpp_result_gen;
END_RCPP
}
// idDub
NumericVector idDub(int i, NumericVector inputlat, NumericVector inputlon, NumericMatrix inputdata, NumericVector outputlat, NumericVector outputlon, NumericVector landmask);
RcppExport SEXP _holmberg_idDub(SEXP iSEXP, SEXP inputlatSEXP, SEXP inputlonSEXP, SEXP inputdataSEXP, SEXP outputlatSEXP, SEXP outputlonSEXP, SEXP landmaskSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type inputlat(inputlatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type inputlon(inputlonSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type inputdata(inputdataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outputlat(outputlatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outputlon(outputlonSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type landmask(landmaskSEXP);
    rcpp_result_gen = Rcpp::wrap(idDub(i, inputlat, inputlon, inputdata, outputlat, outputlon, landmask));
    return rcpp_result_gen;
END_RCPP
}
// interpolate
NumericVector interpolate(NumericVector x_in, NumericVector y_in, NumericVector x_out);
RcppExport SEXP _holmberg_interpolate(SEXP x_inSEXP, SEXP y_inSEXP, SEXP x_outSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x_in(x_inSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y_in(y_inSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x_out(x_outSEXP);
    rcpp_result_gen = Rcpp::wrap(interpolate(x_in, y_in, x_out));
    return rcpp_result_gen;
END_RCPP
}
// invDistWInt
NumericVector invDistWInt(NumericVector inputlat, NumericVector inputlon, NumericVector inputdata, NumericVector outputlat, NumericVector outputlon);
RcppExport SEXP _holmberg_invDistWInt(SEXP inputlatSEXP, SEXP inputlonSEXP, SEXP inputdataSEXP, SEXP outputlatSEXP, SEXP outputlonSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type inputlat(inputlatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type inputlon(inputlonSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type inputdata(inputdataSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outputlat(outputlatSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type outputlon(outputlonSEXP);
    rcpp_result_gen = Rcpp::wrap(invDistWInt(inputlat, inputlon, inputdata, outputlat, outputlon));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_holmberg_idDub_allRows", (DL_FUNC) &_holmberg_idDub_allRows, 6},
    {"_holmberg_idDub", (DL_FUNC) &_holmberg_idDub, 7},
    {"_holmberg_interpolate", (DL_FUNC) &_holmberg_interpolate, 3},
    {"_holmberg_invDistWInt", (DL_FUNC) &_holmberg_invDistWInt, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_holmberg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
