MC_probability_all_infected = function(N) {
  count_all_infected = 0
  for (idx in 1:N) {
    num_infected = 1
    num_days = 2
    last_errs = c(18, 22, 28)
    num_errs = 18
    while (num_errs > 0) {
      lambda = min(last_errs)
      num_errs = rpois(1, lambda)
      last_errs = c(num_errs, last_errs[1:2])
      num_infected = num_infected + num_errs
      num_days = num_days + 1
    }
    if (num_infected == 40) {
      count_all_infected = count_all_infected + 1
    }
  }
  return(count_all_infected / N)
}

MC_probability_at_least_15_infected = function(N) {
  count_at_least_15_infected = 0
  for (idx in 1:N) {
    num_infected = 1
    num_days = 2
    last_errs = c(18, 22, 28)
    num_errs = 18
    while (num_errs > 0) {
      lambda = min(last_errs)
      num_errs = rpois(1, lambda)
      last_errs = c(num_errs, last_errs[1:2])
      num_infected = num_infected + num_errs
      num_days = num_days + 1
    }
    if (num_infected >= 15) {
      count_at_least_15_infected = count_at_least_15_infected + 1
    }
  }
  return(count_at_least_15_infected / N)
}

MC_probability_at_least_15_infected_error = function(target_error, confidence) {
  N = 10000
  p_hat = MC_probability_at_least_15_infected(N)
  error = 1
  while (error > target_error) {
    N = N * 2
    p_hat_prev = p_hat
    p_hat = MC_probability_at_least_15_infected(N)
    error = qnorm(1 - (1 - confidence) / 2) * sqrt(p_hat_prev * (1 - p_hat_prev) / N)
  }
  return(list(probability = p_hat, error = error))
}

# Punctul a) Estimarea probabilității ca într-o anumită zi toate computerele să fie infectate
prob_all_infected = MC_probability_all_infected(10000)

cat("The probability that all computers are infected on a certain day:", prob_all_infected, "\n")

# Punctul b) Estimarea probabilității ca într-o anumită zi toate computerele să fie infectate
prob_at_least_15_infected = MC_probability_at_least_15_infected(10000)

cat("The probability that at least 15 computers are infected on a certain day:", prob_at_least_15_infected, "\n")

# Punctul c) Estimarea probabilității ca într-o anumită zi toate computerele să fie infectate
target_error = 0.01
confidence = 0.95

result = MC_probability_at_least_15_infected_error(target_error, confidence)

cat("The probability that at least 15 computers are infected on a certain day:", result$probability, "\n")
cat("The error of the estimation:", result$error, "\n")