# Exercitiul I)
# Funcția paraboloid_dish_volume calculează volumul unei suprafețe parabolice utilizând metoda Montecarlo.
# N (numărul total de puncte generate) 
# și r (rădăcina pătrată a razei suprafeței parabolice).
paraboloid_dish_volume = function(N, r) {
  N_Counter = 0
  # Se utilizează o buclă for pentru a genera puncte aleatorii și 
  # a verifica dacă acestea se află în interiorul paraboloidului.
  for (iter in 1:N) {
    x1 = runif(1, -sqrt(r), sqrt(r))
    x2 = runif(1, -sqrt(r), sqrt(r))
    x3 = runif(1, 0, r)
    if (x1 * x1 + x2 * x2 <= x3) {
      # Dacă punctul se află în interiorul paraboloidului, se incrementează N_Counter.
      N_Counter = N_Counter + 1
    }
  }
  # Se calculează și se returnează volumul estimat al suprafeței parabolice utilizând formula specifică și proporția de puncte în interiorul paraboloidului.
  return ((2 * sqrt(r)) ^ 2 * r * N_Counter / N)
}
# Se apelează funcția paraboloid_dish_volume cu un număr de puncte 
# și o rază specificate și se calculează volumul estimat și erorile absolute și relative în comparație cu volumul real.
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
# Se definește funcția quadrilateral_area cu un parametru N (numărul total de puncte generate).
quadrilateral_area = function(N) {
  N_Counter = 0
  for (iter in 1:N) {
    x = runif(1, 0, 4)
    y = runif(1, 0, 3)
    # Se utilizează o buclă for pentru a genera puncte aleatorii și a verifica dacă acestea se află în interiorul patrulaterului.
    if (3 * y <= x + 6 && y <= 12 - 3 * x) {
      # Dacă punctul se află în interiorul patrulaterului, se incrementează N_Counter.
      N_Counter = N_Counter + 1
    }
  }
  # Se calculează și se returnează aria estimată a patrulaterului utilizând formula specifică și proporția de puncte în interiorul patrulaterului.
  return (4 * 3 * N_Counter / N)
}
# Se apelează funcția quadrilateral_area cu un număr de puncte și se calculează 
# aria estimată și erorile absolute și relative în comparație cu aria reală.
quadrilateral_area(100000)
MC_area = quadrilateral_area(100000)
area = 9
absolute_err = abs(MC_area - area)
relative_err = absolute_err / area
cat("Estimated area is:", MC_area, "\n")
cat("Absolute error is:", absolute_err, "\n")
cat("Relative error is:", relative_err, "\n")

# Exercitiul III.a)
# N (numărul total de puncte generate),
# a și b (limitele intervalului pentru variabila aleatoare).
MC_integration = function(N, a, b) {
  summation = 0
  # Se utilizează o buclă for pentru a genera puncte aleatorii
  # și a adăuga contribuția fiecărui punct la suma totală.
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
# Se definește funcția MC_fake cu patru parametri: 
# m (numărul inițial de utilizatori falși), 
# n (numărul de utilizatori reali), 
# p (probabilitatea ca un utilizator real să genereze un alt utilizator real) și 
# q (probabilitatea ca un utilizator fals să fie detectat și eliminat într-o zi).
MC_fake = function(m, n, p, q) {
  # Se inițializează variabilele fake_users_count și days.
  fake_users_count = m
  days = 0
  # Se utilizează o buclă while pentru a simula zilele necesare pentru a detecta
  # și elimina utilizatorii falși.
  while (fake_users_count > 0) {
    # În fiecare zi, se adaugă utilizatorii noi generați de utilizatorii reali și 
    # se elimină utilizatorii falși detectați.
    fake_users_count = fake_users_count + rbinom(1, n, p)
    cat("fa: ", fake_users_count, "\n");
    i = fake_users_count
    while (i > 0) {
      if (runif(1, 0, 1) < q) {
        fake_users_count = fake_users_count - 1
      }
      i = i - 1;
    }
    # Se incrementează days pentru a ține evidența numărului de zile trecute.

    days = days + 1
  }
  # Se returnează numărul total de zile necesare pentru a detecta și elimina toți utilizatorii falși.
  return (days)
}
cat("MC_fake days: ", MC_fake(100000, 500, 0.5, 0.01), "\n")

MC_fake_days = function(m, n, p, q, d) {
  fake_users_count = m
  days = 0
  while (fake_users_count > 0 && days < d) {
    fake_users_count = fake_users_count + rbinom(1, n, p)
    i = fake_users_count
    while (i > 0) {
      if (runif(1, 0, 1) < q) {
        fake_users_count = fake_users_count - 1
      }
      i = i - 1;
    }
    days = days + 1
  }
  return (max(fake_users_count, 0))
}
cat("MC_fake_days:", MC_fake_days(100000, 500, 0.5, 0.01, 40), "\n");

# Funcția MC_prob calculează probabilitatea ca simularea să detecteze și să elimine toți utilizatorii falși.
MC_prob = function(N, m, n, p, q, d) {
  s = 0;
  for (iter in 1:N) {
    s = s + MC_fake_days(m, n, p, q, d)
  }
  return (s / N)
}
cat("MC_prob:",MC_prob(100, 100000, 500, 0.5, 0.01, 40), "\n");
# Exercitiul IV. c)
# Se definește variabila alpha ca diferența dintre 1 și nivelul de încredere specificat (0.99).
alpha = 1 - 0.99
# Se utilizează funcția qnorm pentru a calcula valoarea critică z pentru distribuția normală standard în funcție de alpha divizat la 2 (pentru o distribuție bilaterală).
z = qnorm(alpha / 2)
# Se definește epsilon ca valoarea erorii relative acceptate.
epsilon = 0.01
# Se definește p ca probabilitatea ca un utilizator real să genereze un alt utilizator real.
p = 0.5
# Se calculează N_min ca numărul minim de iterații necesare pentru a atinge precizia dorită utilizând formula specifică.
N_min = p * (1 - p) * (z / epsilon) ^ 2
# Se apelează funcția MC_prob cu un număr mai mare de iterații pentru a calcula probabilitatea cu precizia specificată.
MC_prob(N_min + 1, 10, 500, 0.5, 0.01, 40)
