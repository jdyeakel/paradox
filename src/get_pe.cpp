#include <Rcpp.h>
using namespace Rcpp;

//' Calculate the portfolio effect and other metrics from a simulation
//' system
//'
//' This function takes the output from \code{\link{paradox_sim}}
//' (minus the row containing the effort time series) and
//' calculates various portfolio effect metrics. It is implemented in
//' C++ for speed.
//' 
//' @param x The matrix minus the effort row
//' @param num_pop Number of populations
//' @param t_end Number of years (after removing the burn-in period
//' yourself).
//' @export
//' @return
//' A named list with the elements: \code{pe} (portfolio effect), \code{sd_ts}
//' (vector of standard deviations for the populations), \code{mean_ts} (vector
//' of means for the subpopulations), \code{sd_total} (standard deviation of
//' the total biomass), \code{mean_total} (mean of the total biomass), \code{sync}
//' (Loreau and de Mazancourt synchrony index).
//' @examples
//' d <- paradox_sim(alpha = rep(0.5, 10))
//' get_pe(d[-11, -c(1:500)], 10, 5)
//' @useDynLib paradox
// [[Rcpp::export]]

Rcpp::List get_pe(NumericMatrix x, int num_pop, int t_end) {

  NumericVector sd_ts(num_pop);
  NumericVector mean_ts(num_pop);
  NumericVector total(t_end);

  for (int i = 0; i < num_pop; ++i) {
    sd_ts(i) = sd(x(i,_));
    mean_ts(i) = mean(x(i,_));
  }

  for (int j = 0; j < t_end; ++j) {
    total(j) = sum(x(_,j));
  }

  double sd_total = sd(total);
  double mean_total = mean(total);

	NumericVector CV_ts = sd_ts / mean_ts;
	double CV_total = sd_total / mean_total;

  double PE = mean(CV_ts / CV_total);

  double sync = pow(sd(total), 2)/ pow(sum(sd_ts), 2);

  return Rcpp::List::create(Rcpp::Named("pe") = PE,
                            Rcpp::Named("sd_ts") = sd_ts,
                            Rcpp::Named("mean_ts") = mean_ts,
                            Rcpp::Named("sd_total") = sd_total,
                            Rcpp::Named("mean_total") = sd_total,
                            Rcpp::Named("sync") = sync);
}
