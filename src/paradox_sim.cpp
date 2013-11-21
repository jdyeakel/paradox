#include <Rcpp.h>
using namespace Rcpp;

//' Simulate biomass and effort trajectories
//'
//' @useDynLib paradox
//' @param t_end Number of time steps to simulate over
//' @param num_pop Number of populations
//' @param alpha alpha parameter in the Shepherd stock-recruit
//' function (density-independent growth). Entered as a numeric vector
//' with one alpha per population.
//' @param beta beta parameter in the Shepherd stock-recruit function
//' (capacity parameter)
//' @param m Natural mortality
//' @param n n parameter in the Shepherd stock-recruit
//' function. Controls compensation level.
//' @param sigma Standard deviation on the simulated alpha values
//' @param q Effectiveness parameter in the effort function
//' @param cpar Operational costs parameter in the effort function
//' @param p Intrinsic value of the fishery parameter in the effort function
//' @param effort_init Initial effort.
//' @param biomas_init Initial population biomass. Recycled across
//' all populations.
//' @param vuln_threshold The vulnerability threshold. A proportion (between 0
//' and 1) indicating the fraction of the mean biomass of a subpopulation before
//' that subpopulation is declared "vulnerable".
//' @param burnin The number of years to discard as burnin.
//' @param return_ts Logical indicating whether the time series should be
//' returned as part of the output. \code{FALSE} by default to save memory if
//' running many repetitions.
//' @param print_diagnostics Logical indicating whether some print
//' statements should be enabled to help debug.
//' @author Original model developed by Justin Yeakel. C++ version
//' originally ported by Sean Anderson.
//' @return A list object. \code{$performance} contains the performance
//' attributes. If \code{return_ts = TRUE}: \code{$biomass} contains the biomass
//' matrix (time is incremented along the columns and populations down the rows;
//' the burnin period has been removed); \code{$effort} contains the effort in a
//' numeric vector with burnin removed.
//' 
//' The \code{performance} data.frame contains (in order of columns)
//' the average-CV portfolio effect, the mean standard deviation of
//' the subpopulation biomasses, the mean mean of the subpopulation
//' biomasses, the standard deviation of the total biomass, the mean of
//' the total biomass, and the Loreau and de Mazancourt synchrony index.
//' @examples
//' out <- paradox_sim(alpha = rep(0.5, 10), return_ts = TRUE)
//' names(out)
//' print(out$performance)
//' op <- par(mfrow = c(2, 1), mar = c(4, 4, .5, .5))
//' matplot(t(out$biomass[, -c(1:500)]), type = "l", lty = 1, xlab = "Year",
//'   ylab = "Biomass")
//' plot(out$effort[-c(1:500)], type = "l", ylab = "Effort", xlab = "Year")
//' par(op)
//' @export
// [[Rcpp::export]]

Rcpp::List paradox_sim(
    int t_end = 1000, 
    int num_pop = 10, 
    NumericVector alpha = NumericVector::create(), 
    double beta = 1/150,
    double m = 0.01, 
    double n = 1, 
    double sigma = 0.05,
    double q = 0.01, 
    double cpar = 1.4, 
    double p = 0.5,
    double effort_init = 10,
    double biomass_init = 50,
    double vuln_thresh = 0.1,
    int burnin = 500,
    bool return_ts = false,
    bool print_diagnostics = false
    ) {
  NumericMatrix biomass(num_pop, t_end);
  NumericVector effort(t_end);
  NumericMatrix ts(num_pop + 1, t_end);
  NumericMatrix stoch_m(num_pop, t_end);

  NumericVector sd_ts(num_pop);
  NumericVector mean_ts(num_pop);
  NumericVector total(t_end - burnin);
  NumericVector CV_ts(num_pop);

  Rcpp::DataFrame performance;
  Rcpp::List out;

  // initialize simulation values in first year
  for (int i = 0; i < num_pop; ++i) {
    biomass(i, 0) = biomass_init;
  }
  effort(0) = effort_init;

  // stochastic component
  RNGScope scope; // ensure seed gets set
  for(int i = 0; i < num_pop; ++i) {
    stoch_m(i, _) = exp(rnorm(t_end, -pow(sigma, 2)/2, sigma));
  }

  // loop over time and populations
  // to fill biomass and effort data
  for (int j = 1; j < t_end; ++j) { // cycle over time
    for(int i = 0; i < num_pop; ++i) { // cycle over populations
      biomass(i, j) = (biomass(i, j-1) * (alpha[i] * stoch_m(i, j-1))) /
        (1 + beta * pow(biomass(i, j-1), 1/n)) + biomass(i, j-1) *
        exp(-m-q * effort(j-1));
    }
    double total_biomass = sum(biomass(_, j-1));
    effort(j) = effort(j-1) * exp(-cpar) + p * total_biomass * 
      ((q * effort(j-1))/(m + q * effort(j-1))) * (1-exp(-m-q*effort(j-1)));
  }

  // remove burn-in period:
  NumericMatrix biomass_burned(num_pop, t_end - burnin);
  NumericVector effort_burned(t_end - burnin);
  for (int j = 0; j < (t_end - burnin); ++j) { // cycle over time
    effort_burned(j) = effort(j + burnin); 
    for (int i = 0; i < num_pop; ++i) { // cycle over populations
      biomass_burned(i, j) = biomass(i, j + burnin); 
    }
  }

  for (int i = 0; i < num_pop; ++i) {
    sd_ts(i) = sd(biomass_burned(i,_));
    mean_ts(i) = mean(biomass_burned(i,_));
  }

  double mean_sd_ts = mean(sd_ts);
  double mean_mean_ts = mean(mean_ts);

  for (int j = 0; j < (t_end - burnin); ++j) {
    total(j) = sum(biomass_burned(_,j));
  }
  
  double sd_total = sd(total);
  double mean_total = mean(total);

	CV_ts = sd_ts / mean_ts;
	double CV_total = sd_total / mean_total;


  double mean_cv_ts = mean(CV_ts);

  double PE = mean_cv_ts / CV_total;

  if (print_diagnostics) {
    Rcout << "subpop 1 cv" << CV_ts(1) <<std::endl;
    Rcout << "cv total" << CV_total <<std::endl;
    Rcout << "mean cv subpops" << mean_cv_ts <<std::endl;
    Rcout << "pe" << PE << std::endl;
  }

  double sync = pow(sd(total), 2)/ pow(sum(sd_ts), 2);
  
  // Calculate Vulnerability Metric
  int vuln_cnt = 0;
  for (int i = 0; i < num_pop; ++i) {
    int vuln_TF = 0;
    for (int j = 0; j < (t_end - burnin); ++j) {
      if (biomass_burned(i,j) < vuln_thresh * mean_ts(i)) {
        vuln_TF += 1;
        }
    }
    if (vuln_TF > 0) {
      vuln_cnt += 1;
    }
  }
  double vuln = vuln_cnt / num_pop;
  
  performance = Rcpp::DataFrame::create(Rcpp::Named("pe") = PE,
                Rcpp::Named("mean_sd_ts") = mean_sd_ts,
                Rcpp::Named("mean_mean_ts") = mean_mean_ts,
                Rcpp::Named("sd_total") = sd_total,
                Rcpp::Named("mean_total") = sd_total,
                Rcpp::Named("sync") = sync,
                Rcpp::Named("vuln") = vuln);
  
  if (return_ts == true) {
  out = Rcpp::List::create(Rcpp::Named("biomass") = biomass_burned,
                           Rcpp::Named("effort") = effort_burned,
                           Rcpp::Named("performance") = performance);
  } else {
  out = Rcpp::List::create(Rcpp::Named("performance") = performance);
  }

  return out;
}
