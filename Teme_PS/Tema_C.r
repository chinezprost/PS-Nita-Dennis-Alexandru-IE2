# Exercitiul I)
paraboloid_dish_volume = function(N, r) {
  N_Counter = 0
  for (iter in 1:N) {
    x1 = runif(1, -sqrt(r), sqrt(r))
    x2 = runif(1, -sqrt(r), sqrt(r))
    x3 = runif(1, 0, r)
    if (x1 * x1 + x2 * x2 <= x3) {
      N_Counter = N_Counter + 1
    }
  }
  return ((2 * sqrt(r)) ^ 2 * r * N_Counter / N)
}
radius = 2
paraboloid_dish_volume(10000, radius)
MC_volume = paraboloid_dish_volume(100000, radius)
volume = pi * radius ^ 2 / 2
absolute_err = abs(MC_volume - volume)
relative_err = absolute_err / volume
cat("Estimated volume is:", MC_volume, "\n")
cat("Absolute error is:", absolute_err, "\n")
cat("Relative error is:", relative_err, "\n")

# Exercitiul II)
quadrilateral_area = function(N) {
  N_Counter = 0
  for (iter in 1:N) {
    x = runif(1, 0, 4)
    y = runif(1, 0, 3)
    if (3 * y <= x + 6 && y <= 12 - 3 * x) {
      N_Counter = N_Counter + 1
    }
  }
  return (4 * 3 * N_Counter / N)
}
quadrilateral_area(100000)
MC_area = quadrilateral_area(100000)
area = 9
absolute_err = abs(MC_area - area)
relative_err = absolute_err / area
cat("Estimated area is:", MC_area, "\n")
cat("Absolute error is:", absolute_err, "\n")
cat("Relative error is:", relative_err, "\n")

# Exercitiul III.a)
MC_integration = function(N, a, b) {
  summation = 0
  for (iter in 1:N) {
    u = runif(1, a, b)
    summation = summation + (u + 1) / sqrt(4 - u)
  }
  return ((b - a) * summation / N)
}
MC_integral = MC_integration(10000, -1, 1)
integral = integrate(function(x) (x + 1) / sqrt(4 - x), -1, 1)$value
absolute_err = abs(MC_integral - integral)
relative_err = absolute_err / integral
cat("Estimated integral is:", MC_integral, "\n")
cat("Absolute error is:", absolute_err, "\n")
cat("Relative error is:", relative_err, "\n")

# Exercitiul III.b)
MC_integration = function(N, a, b) {
  summation = 0
  for (iter in 1:N) {
    u = runif(1, a, b)
    summation = summation + 1 / (u ^ 2 + 4)
  }
  return ((b - a) * summation / N)
}
MC_integral = MC_integration(10000, -50, 0)
integral = integrate(function(x) 1 / (x ^ 2 + 4), -Inf, 0)$value
absolute_err = abs(MC_integral - integral)
relative_err = absolute_err / integral
cat("Estimated integral is:", MC_integral, "\n")
cat("Absolute error is:", absolute_err, "\n")
cat("Relative error is:", relative_err, "\n")

# Exercitiul III.c)
MC_integration = function(N, a, b) {
  summation = 0
  for (iter in 1:N) {
    u = runif(1, a, b)
    summation = summation + u * exp(u)
  }
  return ((b - a) * summation / N)
}
MC_integral = MC_integration(10000, -50, 0)
integral = integrate(function(x) x * exp(x), -Inf, 0)$value
absolute_err = abs(MC_integral - integral)
relative_err = absolute_err / integral
cat("Estimated integral is:", MC_integral, "\n")
cat("Absolute error is:", absolute_err, "\n")
cat("Relative error is:", relative_err, "\n")

# Exercitiul IV. a)
MC_fake = function(m, n, p, q) {
  fake_users_count = m
  days = 0
  while (fake_users_count > 0) {
    fake_users_count = fake_users_count + rbinom(1, n, p)
    cat("fa: ", fake_users_count, "\n");
    i = fake_users_count
    while (i > 0) {
        fake_users_count = fake_users_count - 1
      i = i - 1;
    }
    days = days + 1
  }
  return (days)
}
cat("MC_fake days: ", MC_fake(100000, 500, 0.5, 0.01), "\n")

m = 100000
n = 500
p = 0.5
q = 0.01
fake_users_count = m
days = 0
while (fake_users_count > 0) {
  cat("Before:", fake_users_count, "\n")
  fake_users_count = fake_users_count - rbinom(1, n, p)
  i = fake_users_count
  while (i > 0) {
      fake_users_count = fake_users_count - 1
    i = i - 1;
  }
  days = days + 1
  cat("After:", fake_users_count, "\n")
}

MC_fake_days = function(m, n, p, q, d) {
  fake_users_count = m
  days = 0
  while (fake_users_count > 0 && days < d) {
    fake_users_count = fake_users_count + rbinom(1, n, p)
    i = fake_users_count
    while (i > 0) {
        fake_users_count = fake_users_count - 1
      i = i - 1;
    }
    days = days + 1
  }
  return (max(fake_users_count, 0))
}
cat("MC_fake_days:", MC_fake_days(100000, 500, 0.5, 0.01, 40), "\n");

MC_prob = function(N, m, n, p, q, d) {
  s = 0;
  for (iter in 1:N) {
    s = s + MC_fake_days(m, n, p, q, d)
  }
  return (s / N)
}
cat("MC_prob:",MC_prob(100, 100000, 500, 0.5, 0.01, 40), "\n");
# Exercitiul IV. c)
alpha = 1 - 0.99
z = qnorm(alpha / 2)
epsilon = 0.01
p = 0.5
N_min = p * (1 - p) * (z / epsilon) ^ 2
N_min
#aici trebuie 10.000 de iteratii dar am impresia ca dureaza 4-5 ore
MC_prob(N_min + 1, 10, 500, 0.5, 0.01, 40)