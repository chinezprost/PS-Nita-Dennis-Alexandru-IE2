# Exercitiul I)

# alpha (nivelul de încredere),
# n (mărimea eșantionului),
# media_amostra (media eșantionului) și
# sigma (deviația standard a populației).
interval_confianta_z = function(alpha, n, media_amostra, sigma) {
  # Se calculează valoarea critică z utilizând funcția qnorm.
  # Valoarea 1 - alpha / 2 reprezintă probabilitatea din coada superioară a distribuției normale standard pentru un nivel de încredere bilateral.
  
  z_crit = qnorm(1 - alpha / 2, 0, 1)
  # Se calculează limitele intervalului de încredere. 

  # Limita inferioară a este calculată prin scăderea produsului dintre z_crit, sigma și sqrt(n) din media_amostra. 
  a = media_amostra - z_crit * sigma / sqrt(n)

  # Limita superioară b este calculată prin adunarea produsului dintre z_crit, sigma și sqrt(n) la media_amostra.
  b = media_amostra + z_crit * sigma / sqrt(n)

  # Intervalul de încredere este stocat într-un vector și returnat.
  interval = c(a, b)
  return (interval)
}
interval_confianta_z(0.1, 8, 138, 11)
interval_confianta_z(0.05, 8, 138, 11)
interval_confianta_z(0.01, 8, 138, 11)

# Exercitiul II)
# Apelul calculează un interval de încredere pentru media populației folosind un deviația standard cunoscută (sqrt(1.44)) și un eșantion de mărime 256.
interval_confianta_z(0.05, 256, 18, sqrt(1.44))

# Exercitiul III)
# alpha (nivelul de semnificație), 
# n (mărimea eșantionului), 
# succese (numărul de succese din eșantion), 
# p0 (valoarea proporției sub ipoteză) și 
# hyp (tipul de ipoteză: "r" pentru ipoteza alternativă dreaptă, "l" pentru ipoteza alternativă stângă și orice altceva pentru ipoteza bilaterala).
test_proportie = function(alpha, n, succese, p0, hyp) {
  # Se calculează proporția estimată p_prim ca raport între numărul de succese și mărimea eșantionului.
  p_prim = succese / n
  # Se calculează scorul z utilizând formula (p_prim - p0) / sqrt(p0 * (1 - p0) / n).
  scor_z = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  # Se afișează scorul z.
  cat(scor_z, "\n")

  # În funcție de tipul de ipoteză (hyp), se calculează valoarea critică z corespunzătoare nivelului de semnificație alpha. 
  
  
  # Pentru ipoteza alternativă dreaptă sau stângă, se utilizează qnorm(1 - alpha, 0, 1),
  if (hyp == "r") {
    z_crit = qnorm(1 - alpha, 0, 1)
    cat(z_crit, "\n")
    if (scor_z > z_crit) {
      cat("Respingem H0.\n")
    } else {
      cat("Nu avem suficiente dovezi pentru a respinge H0\n")
    }
    
  } else if (hyp == "l") {
    z_crit = qnorm(1 - alpha, 0, 1)
    cat(z_crit, "\n")
    if (scor_z < z_crit) {
      cat("Respingem H0.\n")
    } else {
      cat("Nu avem suficiente dovezi pentru a respinge H0.\n")
    }
  # iar pentru ipoteza bilaterală se utilizează qnorm(1 - alpha / 2, 0, 1).
  } else {
    z_crit = qnorm(1 - alpha / 2, 0, 1)
    cat(z_crit, "\n")
    if (abs(scor_z) > z_crit) {
      cat("Respingem H0.\n")
    } else {
      cat("Nu avem suficiente dovezi pentru a respinge H0.\n")
    }
  }
}
# Apelurile test_proportie(0.01, 153, 17, 0.12, "r") și test_proportie(0.05, 153, 17, 0.12, "r") efectuează teste de ipoteză pentru o proporție cu ipoteza alternativă dreaptă, utilizând diferite nivele de semnificație și parametrii specificați.
test_proportie(0.01, 153, 17, 0.12, "r")
test_proportie(0.05, 153, 17, 0.12, "r")