#include <Rcpp.h>
using namespace Rcpp;

//' Paradox simulation with PE output for speed YEA
//'
//' @export
// [[Rcpp::export]]

Rcpp::List paradox_pe_sim(
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
    double biomass_init = 50
    int burnin = 500,
    ) {
  NumericMatrix biomass(num_pop, t_end);
  NumericVector effort(t_end);
  NumericMatrix ts(num_pop + 1, t_end);
  NumericMatrix stoch_m(num_pop, t_end);

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

  // combine biomass and effort into one matrix
    //for(int i = 0; i < num_pop; ++i) {
       //ts(i, _) = biomass(i, _);
    //}
    //ts(num_pop + 1 - 1, _) = effort;
    
  // remove burn-in period:
  NumericMatrix biomass_burned(num_pop, t_end - burnin);
  for (int j = 0; j < (t_end - burnin); ++j) { // cycle over time
    for (int i = 0; i < num_pop; ++i) { // cycle over populations
      biomass_burned(i, j) = biomass(i, j + burnin); 
    }
  }

  NumericVector sd_ts(num_pop);
  NumericVector mean_ts(num_pop);
  NumericVector total(t_end - burnin);
  NumericVector CV_ts(num_pop);

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

  double PE = mean(CV_ts / CV_total);

  double sync = pow(sd(total), 2)/ pow(sum(sd_ts), 2);
  
  // Calculate Vulnerability Metric
  double vuln_cnt = 0;
  for (int i = 0; i < num_pop; ++i) {
    double vuln_TF = 0;
    for (int j = 0; j < (t_end - burnin); ++j) {
      if (biomass_burned(i,j) < 0.10*mean_ts(i)) {
        vuln_TF += 1;
        }
    }
    if (vuln_TF > 0) {
      vuln_cnt += 1;
    }
  }
  double vuln = vuln_cnt / num_pop;
  
  return Rcpp::List::create(Rcpp::Named("pe") = PE,
                            Rcpp::Named("mean_sd_ts") = mean_sd_ts,
                            Rcpp::Named("mean_mean_ts") = mean_mean_ts,
                            Rcpp::Named("sd_total") = sd_total,
                            Rcpp::Named("mean_total") = sd_total,
                            Rcpp::Named("sync") = sync,
                            Rcpp::Named("vuln") = vuln);
}
