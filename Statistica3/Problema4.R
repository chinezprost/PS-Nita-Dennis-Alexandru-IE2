binomial_probability = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q = (k - 0.5 - expectation)/standard_deviation;
  return(pnorm(q));
}
