#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

// [[Rcpp::export]]
Eigen::VectorXi eigen_colsums(const Eigen::Map<Eigen::MatrixXi> M) {
  const Eigen::MatrixXi S(M.colwise().sum());
  const Eigen::VectorXi B(Eigen::Map<const Eigen::VectorXi>(S.data(), S.cols()*S.rows()));
  return B;
}

// [[Rcpp::export]]
Eigen::VectorXi eigen_rowsums(const Eigen::Map<Eigen::MatrixXi> M) {
  const Eigen::MatrixXi S(M.rowwise().sum());
  const Eigen::VectorXi B(Eigen::Map<const Eigen::VectorXi>(S.data(), S.cols()*S.rows()));
  return B;
}


// [[Rcpp::export]]
double eigen_chisq(
    Eigen::Map<Eigen::VectorXi> T1, 
    Eigen::Map<Eigen::VectorXi> T2, 
    Eigen::Map<Eigen::VectorXi> RowSum, 
    int n) {
  int vecLen = T1.size();
  Eigen::MatrixXi m;
  m.resize(vecLen, 2);
  m.col(0) = T1;
  m.col(1) = T2;
  const Eigen::RowVector2i ColSum(m.colwise().sum());
  const Eigen::ArrayX2d E = (RowSum * ColSum).array().cast<double>() / n;
  const Eigen::ArrayX2d diff = (m.array().cast<double>() - E).pow(2) / E;
  return diff.sum();
}


