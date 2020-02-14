// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// eigen_colsums
Eigen::VectorXi eigen_colsums(const Eigen::Map<Eigen::MatrixXi> M);
RcppExport SEXP _rainette_eigen_colsums(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXi> >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(eigen_colsums(M));
    return rcpp_result_gen;
END_RCPP
}
// eigen_rowsums
Eigen::VectorXi eigen_rowsums(const Eigen::Map<Eigen::MatrixXi> M);
RcppExport SEXP _rainette_eigen_rowsums(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXi> >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(eigen_rowsums(M));
    return rcpp_result_gen;
END_RCPP
}
// eigen_chisq
double eigen_chisq(Eigen::Map<Eigen::VectorXi> T1, Eigen::Map<Eigen::VectorXi> T2, Eigen::Map<Eigen::VectorXi> RowSum, int n);
RcppExport SEXP _rainette_eigen_chisq(SEXP T1SEXP, SEXP T2SEXP, SEXP RowSumSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXi> >::type T1(T1SEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXi> >::type T2(T2SEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::VectorXi> >::type RowSum(RowSumSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(eigen_chisq(T1, T2, RowSum, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rainette_eigen_colsums", (DL_FUNC) &_rainette_eigen_colsums, 1},
    {"_rainette_eigen_rowsums", (DL_FUNC) &_rainette_eigen_rowsums, 1},
    {"_rainette_eigen_chisq", (DL_FUNC) &_rainette_eigen_chisq, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_rainette(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}