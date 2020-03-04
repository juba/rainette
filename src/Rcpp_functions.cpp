/******************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

/******************************************************************************/

// [[Rcpp::export]]
double cpp_chisq(const IntegerVector& T1,
                   const IntegerVector& T2,
                   const IntegerVector& Tsum) {

  double sum_T1 = Rcpp::sum(T1);
  double sum_T2 = Rcpp::sum(T2);
  double n = sum_T1 + sum_T2;
  double p1 = sum_T1 / n;
  double p2 = sum_T2 / n;

  int vecLen = T1.size();
  double res = 0;
  for (int i = 0; i < vecLen; i++) {
    double e1 = Tsum[i] * p1;
    double e2 = Tsum[i] * p2;
    double diff1 = T1[i] - e1;
    double diff2 = T2[i] - e2;
    res += diff1 * diff1 / e1 + diff2 * diff2 / e2;
  }

  return res;
}

/******************************************************************************/

// Split a Matrix at max chi-squared
// [[Rcpp::export]]
List cpp_split_tab(const IntegerMatrix& tab,
                     const IntegerVector& indices) {

  IntegerVector colSum = Rcpp::colSums(tab);

  // Initialisation
  int index = indices(0) - 1;
  IntegerVector tab1 = tab.row(index);
  IntegerVector tab2 = colSum - tab1;
  double chisq = cpp_chisq(tab1, tab2, colSum);

  int max_index = index + 1;
  double max_chisq = chisq;

  // Compute chi-squared values for each index and keep the maxima
  int n = indices.size();
  int p = tab.ncol();
  for (int i = 1; i < n; i++) {
    index = indices(i) - 1;
    for (int j = 0; j < p; j++) {
      tab1[j] += tab(index, j);
      tab2[j] -= tab(index, j);
    }
    chisq = cpp_chisq(tab1, tab2, colSum);

    if (chisq > max_chisq) {
      max_chisq = chisq;
      max_index = index + 1;
    }
  }

  return List::create(Named("max_index") = max_index,
                      Named("max_chisq") = max_chisq);
}

/******************************************************************************/

// Switch rows to maximize chi-square
// [[Rcpp::export]]
NumericVector cpp_switch_docs(const IntegerMatrix& tab1,
                                const IntegerMatrix& tab2) {

  int n1 = tab1.nrow();
  int n2 = tab2.nrow();
  IntegerVector tab1Sum = Rcpp::colSums(tab1);
  IntegerVector tab2Sum = Rcpp::colSums(tab2);
  IntegerVector colSum = tab1Sum + tab2Sum;

  NumericVector chisq_values(n1 + n2);
  int p = tab1Sum.size();
  IntegerVector tab1Sum_new(p), tab2Sum_new(p);

  for (int i1 = 0; i1 < n1; i1++) {
    for (int j = 0; j < p; j++) {
      tab1Sum_new[j] = tab1Sum[j] - tab1(i1, j);
      tab2Sum_new[j] = tab2Sum[j] + tab1(i1, j);
    }
    chisq_values[i1] = cpp_chisq(tab1Sum_new, tab2Sum_new, colSum);
  }
  for (int i2 = 0; i2 < n2; i2++) {
    for (int j = 0; j < p; j++) {
      tab1Sum_new[j] = tab1Sum[j] + tab2(i2, j);
      tab2Sum_new[j] = tab2Sum[j] - tab2(i2, j);
    }
    chisq_values[n1 + i2] = cpp_chisq(tab1Sum_new, tab2Sum_new, colSum);
  }

  return chisq_values;
}

/******************************************************************************/
