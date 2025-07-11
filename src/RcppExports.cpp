// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cubical_2dim
Rcpp::NumericMatrix cubical_2dim(const Rcpp::NumericMatrix& image, double threshold, int method);
RcppExport SEXP _ripserr_cubical_2dim(SEXP imageSEXP, SEXP thresholdSEXP, SEXP methodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< int >::type method(methodSEXP);
    rcpp_result_gen = Rcpp::wrap(cubical_2dim(image, threshold, method));
    return rcpp_result_gen;
END_RCPP
}
// cubical_3dim
Rcpp::NumericMatrix cubical_3dim(Rcpp::NumericVector& image, double threshold, int method, int nx, int ny, int nz);
RcppExport SEXP _ripserr_cubical_3dim(SEXP imageSEXP, SEXP thresholdSEXP, SEXP methodSEXP, SEXP nxSEXP, SEXP nySEXP, SEXP nzSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< int >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type nx(nxSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    Rcpp::traits::input_parameter< int >::type nz(nzSEXP);
    rcpp_result_gen = Rcpp::wrap(cubical_3dim(image, threshold, method, nx, ny, nz));
    return rcpp_result_gen;
END_RCPP
}
// cubical_4dim
Rcpp::NumericMatrix cubical_4dim(Rcpp::NumericVector& image, double threshold, int method, int nx, int ny, int nz, int nt);
RcppExport SEXP _ripserr_cubical_4dim(SEXP imageSEXP, SEXP thresholdSEXP, SEXP methodSEXP, SEXP nxSEXP, SEXP nySEXP, SEXP nzSEXP, SEXP ntSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector& >::type image(imageSEXP);
    Rcpp::traits::input_parameter< double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< int >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type nx(nxSEXP);
    Rcpp::traits::input_parameter< int >::type ny(nySEXP);
    Rcpp::traits::input_parameter< int >::type nz(nzSEXP);
    Rcpp::traits::input_parameter< int >::type nt(ntSEXP);
    rcpp_result_gen = Rcpp::wrap(cubical_4dim(image, threshold, method, nx, ny, nz, nt));
    return rcpp_result_gen;
END_RCPP
}
// ripser_cpp_dist
Rcpp::List ripser_cpp_dist(const Rcpp::NumericVector& dataset, int dim, double thresh, float ratio, int p);
RcppExport SEXP _ripserr_ripser_cpp_dist(SEXP datasetSEXP, SEXP dimSEXP, SEXP threshSEXP, SEXP ratioSEXP, SEXP pSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type dataset(datasetSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    Rcpp::traits::input_parameter< double >::type thresh(threshSEXP);
    Rcpp::traits::input_parameter< float >::type ratio(ratioSEXP);
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    rcpp_result_gen = Rcpp::wrap(ripser_cpp_dist(dataset, dim, thresh, ratio, p));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ripserr_cubical_2dim", (DL_FUNC) &_ripserr_cubical_2dim, 3},
    {"_ripserr_cubical_3dim", (DL_FUNC) &_ripserr_cubical_3dim, 6},
    {"_ripserr_cubical_4dim", (DL_FUNC) &_ripserr_cubical_4dim, 7},
    {"_ripserr_ripser_cpp_dist", (DL_FUNC) &_ripserr_ripser_cpp_dist, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_ripserr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
