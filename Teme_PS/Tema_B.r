# Exercitiul I)
# Funcția LLN_Geom calculează estimarea mediei și inversul probabilității
# de succes pentru o distribuție geometrică utilizând Legea Numerelor Mari.

# Se definește funcția LLN_Geom cu doi parametri: n (numărul de eșantioane) 
# și p (probabilitatea de succes într-un singur eșantion).
LLN_Geom = function(n, p) {
  return (c(mean(rgeom(n, p)), 1 / p));
}
# Se apelează funcția LLN_Geom cu diferite valori pentru n și p.
LLN_Geom(5000, 0.2)
LLN_Geom(10000, 0.6)
LLN_Geom(100000, 0.6)
LLN_Geom(500000, 0.8)

# Exercitiul II)
# Funcția CLT_Student calculează probabilitatea că media eșantioanelor
# dintr-o distribuție Student cu parametrul r să fie mai mică sau egală cu un anumit bound, 
# utilizând Teorema Limită Centrală.

# r (parametrul distribuției Student), 
# n (numărul de eșantioane utilizate pentru calculul mediei), 
# N (numărul total de simulări) și 
# z (bound-ul înmulțit cu deviația standard).

CLT_Student = function(r, n, N, z) {
  expectation = 0
  st_dev = sqrt(r / (r - 2))
  upper_bound = z * st_dev / sqrt(n) + expectation
  sum = 0
  # Se utilizează o buclă for pentru a simula media eșantioanelor din distribuția Student
  # și a calcula probabilitatea că aceasta să fie mai mică sau egală cu upper_bound.
  for (i in 1:N) {
    x_n = mean(rt(n, r))
    if (x_n <= upper_bound) {
      sum = sum + 1
    }
  }
  # Se returnează probabilitatea estimată,
  # care reprezintă proporția de eșantioane care respectă condiția.
  return (sum / N)
}
estimated_prob = CLT_Student(3, 50, 5000, -1.5)
true_prob = pnorm(-1.5)
relative_error = abs(estimated_prob - true_prob) / true_prob
relative_error
estimated_prob = CLT_Student(4, 50, 10000, 0)
true_prob = pnorm(0)
relative_error = abs(estimated_prob - true_prob) / true_prob
relative_error
estimated_prob = CLT_Student(5, 50, 20000, 1.5)
true_prob = pnorm(1.5)
relative_error = abs(estimated_prob - true_prob) / true_prob
relative_error

# Se afișează eroarea relativă pentru fiecare set de parametri de apel al funcției CLT_Student.
# Exercitiul III)

#  n (numărul de încercări),
#  p (probabilitatea de succes într-o singură încercare),
#  h (limita inferioară a intervalului)
#  și k (limita superioară a intervalului).

ML_Binom = function(n, p, h, k) {
  # Se calculează așteptarea matematică (expectation).
  expectation = n * p
  # Și deviația standard (standard_deviation) pentru distribuția binomială.
  standard_deviation = sqrt(n * p * (1 - p))
  # Se calculează valoarea q1 și q2 pe baza limitelor intervalului și parametrilor distribuției.
  q1 = (h - expectation) / standard_deviation
  q2 = (k - expectation) / standard_deviation
  # Se aproximează probabilitatea că variabila aleatoare se află în intervalul specificat utilizând funcția pnorm.
  approx_prob = pnorm(q2) - pnorm(q1)
  # Se returnează probabilitatea aproximată
  return (approx_prob)
}
ML_Binom(100, 0.3, 20, 40)
#P(X<40)
pbinom(40, 100, 0.3) - dbinom(40, 100, 0.3)
pbinom(39, 100, 0.3)
#P(X<20)
pbinom(20, 100, 0.3) - dbinom(20, 100, 0.3)
pbinom(19, 100, 0.3)
cat(pbinom(39, 100, 0.3) - pbinom(19, 100, 0.3))

# Se afișează diferența dintre P(X<39) și P(X<19). ((pbinom și dbinom) pentru a verifica corectitudinea aproximării.)