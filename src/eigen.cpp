#include <RcppEigen.h>
#include <Eigen/Dense>

// [[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;


// Fast chi-square computation
// [[Rcpp::export]]
float eigen_chisq(
    Eigen::VectorXi T1, 
    Eigen::VectorXi T2, 
    Eigen::VectorXi RowSum, 
    int n) {
  int vecLen = T1.size();
  Eigen::MatrixXi m;
  m.resize(vecLen, 2);
  m.col(0) = T1;
  m.col(1) = T2;
  const Eigen::RowVector2i ColSum(m.colwise().sum());
  const Eigen::ArrayX2f E = (RowSum * ColSum).array().cast<float>() / n;
  const Eigen::ArrayX2f diff = (m.array().cast<float>() - E).pow(2) / E;
  return diff.sum();
}


// Split a Matrix at max chi-squared 
// [[Rcpp::export]]
List eigen_split_tab(
  Eigen::MatrixXi tab,
  Eigen::VectorXi indices) {
  
  Eigen::VectorXi colSum = tab.colwise().sum();
  int total = tab.sum();
  indices = indices.array() - 1;
  
  // Initialisation
  int index = indices(0);
  Eigen::Matrix<int, 1, Eigen::Dynamic> tab1 = tab.row(index);
  Eigen::Matrix<int, 1, Eigen::Dynamic> tab2_1 = (tab.topRows(index)).colwise().sum();
  Eigen::Matrix<int, 1, Eigen::Dynamic> tab2_2 = (tab.bottomRows(tab.rows() - (index + 1))).colwise().sum();
  Eigen::Matrix<int, 1, Eigen::Dynamic> tab2 = tab2_1 + tab2_2;
  
  float chisq = eigen_chisq(tab1, tab2, colSum, total);
  float max_index = index + 1;
  float max_chisq = chisq;

  // Compute chi-squared values for each index and keep the maxima
  for (int i = 1; i < indices.size(); i = i + 1) {
    index = indices(i);
    tab1 = tab1 + tab.row(index);
    tab2 = tab2 - tab.row(index);
    chisq = eigen_chisq(tab1, tab2, colSum, total);

    if (chisq > max_chisq) {
      max_chisq = chisq;
      max_index = index + 1;
    }
  }
  
  return List::create(Named("max_index") = max_index,
		      Named("max_chisq") = max_chisq);
}




// Pas utilisé : pas utile
// Eigen::VectorXi eigen_colsums(const Eigen::MatrixXi M) {
//   const Eigen::MatrixXi S(M.colwise().sum());
//   const Eigen::VectorXi B(Eigen::Map<const Eigen::VectorXi>(S.data(), S.cols()*S.rows()));
//   return B;
// }
// Pas utilisé : pas utile
// Eigen::VectorXi eigen_rowsums(const Eigen::MatrixXi M) {
//   const Eigen::MatrixXi S(M.rowwise().sum());
//   const Eigen::VectorXi B(Eigen::Map<const Eigen::VectorXi>(S.data(), S.cols()*S.rows()));
//   return B;
// }


// Pas utilisé : trop lent
// Eigen::MatrixXf eigen_u_coords(Eigen::MatrixXf m) {
//   Eigen::MatrixXf P = m / m.sum();
//   Eigen::MatrixXf rm = P.rowwise().sum();
//   Eigen::MatrixXf cm = P.colwise().sum();
//   Eigen::MatrixXf eP = (rm * cm);
//   Eigen::MatrixXf S = (P.array() - eP.array()) / eP.array().sqrt();
//   Eigen::BDCSVD<Eigen::MatrixXf> svd(S, Eigen::ComputeThinU);
//   Eigen::MatrixXf u = svd.matrixU().col(0);
//   Eigen::MatrixXf coord = u.array() / rm.array().sqrt();
//   return coord;
// }





