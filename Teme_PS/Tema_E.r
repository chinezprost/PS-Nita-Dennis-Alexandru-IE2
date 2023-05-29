# Exercitiul I)
interval_confianta_z = function(alpha, n, media_amostra, sigma) {
  z_crit = qnorm(1 - alpha / 2, 0, 1)
  a = media_amostra - z_crit * sigma / sqrt(n)
  b = media_amostra + z_crit * sigma / sqrt(n)
  interval = c(a, b)
  return (interval)
}
interval_confianta_z(0.1, 8, 138, 11)
interval_confianta_z(0.05, 8, 138, 11)
interval_confianta_z(0.01, 8, 138, 11)

# Exercitiul II)
interval_confianta_z(0.05, 256, 18, sqrt(1.44))

# Exercitiul III)
test_proportie = function(alpha, n, succese, p0, hyp) {
  p_prim = succese / n
  scor_z = (p_prim - p0) / sqrt(p0 * (1 - p0) / n)
  cat(scor_z, "\n")
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
test_proportie(0.01, 153, 17, 0.12, "r")
test_proportie(0.05, 153, 17, 0.12, "r")