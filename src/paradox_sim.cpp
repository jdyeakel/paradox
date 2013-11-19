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
//' @param n Depensation parameter in the Shepherd stock-recruit
//' function
//' @param sigma Standard deviation on the simulated alpha values
//' @param q Effectiveness parameter in the effort function
//' @param cpar Operational costs parameter in the effort function
//' @param p Intrinsic value of the fishery parameter in the effort function
//' @param biomas_init Initial population biomass. Recycled across
//' all populations.
//' @param effort_init Initial effort.
//' @author Original model developed by Justin Yeakel. C++ version
//' implemented by Sean Anderson.
//' @return A matrix. Time is incremented along the columns and
//' populations down the rows. The last row represents the effort.
//'
//' @examples
//' d <- paradox_sim(alpha = rep(0.5, 10))
//' matplot(t(d[-11, -c(1:100)]), type = "l", lty = 1)
//' @export
// [[Rcpp::export]]
NumericMatrix paradox_sim(
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
    //effort(j) = effort(j-1) * exp(-cpar * effort(j-1)) + p * total_biomass * (1-exp(-q*effort(j-1)));
    effort(j) = effort(j-1) * exp(-cpar) + p * total_biomass * 
      ((q * effort(j-1))/(m + q * effort(j-1))) * (1-exp(-m-q*effort(j-1)));
  }

  // combine biomass and effort into one matrix
    for(int i = 0; i < num_pop; ++i) {
       ts(i, _) = biomass(i, _);
    }
    ts(num_pop + 1 - 1, _) = effort;

  return ts;
}
