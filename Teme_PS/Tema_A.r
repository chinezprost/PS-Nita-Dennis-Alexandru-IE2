# Exercitiul I. a)
three_plots <- function(k, n, p, lambda) {
  # Se definește vectorul x care conține valorile de la k la n.
  x <- k:n
  # Se calculează valorile funcției de probabilitate pentru distribuția Poisson (y_poisson), 
  y_poisson <- dpois(x, lambda)

  # distribuția geometrică (y_geometric)
  y_geometric <- dgeom(x, p)

  # și distribuția binomială (y_binomial) pe baza valorilor din vectorul x.
  y_binomial <- dbinom(x, n, p)
  
  # Se afișează un grafic cu valorile din x pe axa orizontală și valorile corespunzătoare din y_poisson pe axa verticală. 
  # Linia graficului este de tipul "l" (linie) și de culoare albastră. Eticheta graficului este "Mass Probability Functions". 
  # Limitele axelor sunt setate pentru a afișa complet graficul. De asemenea, se afișează etichetele pentru axele x și y.
  plot(x, y_poisson, type = "l", main = "Mass Probability Functions", lwd = 2, lty = 1, 
       col = "blue", ylim = c(0, max(y_poisson, y_geometric, y_binomial)), xlab = "k:n", 
       ylab = "Probability") 
  lines(x, y_geometric, type = "l", lwd = 2, lty = 2, col = "red")
  lines(x, y_binomial, type = "l", lwd = 2, lty = 3, col = "green")

  # Se adaugă o legendă în colțul din dreapta sus a graficului, care indică culorile și 
  # tipurile liniilor corespunzătoare fiecărei funcții de probabilitate.
  legend("topright", c("Poisson", "Geometric", "Binomial"), lty = c(1, 2, 3), #(1 - solid, 2 - dashed, 3 - dotted)
         lwd = 2, col = c("blue", "red", "green")) 
}

# Funcția three_plots primește patru argumente: k, n, p, lambda. 
# Aceasta generează trei grafice pentru funcțiile de probabilitate: Poisson, geometrică și binomială.
three_plots(3, 10, 0.4, 1)

# Exercitiul I. b)
geometric_stuff <- function(p) {
  # Se calculează probabilitatea ca un eveniment să aibă valoare impară (odd_probability).
  odd_probability <- 1 / (2 - p)
  # Se calculează probabilitatea de a obține o valoare mai mare sau egală cu 4 (p4).
  p4 <- pgeom(4, p, lower.tail = FALSE) + (1 - p) ^ 3 * p
  # Se calculează probabilitatea de a obține o valoare mai mică sau egală cu 20 (p20).
  p20 <- pgeom(20, p)
  # Se afișează rezultatele calculate.
  cat("P(x = impar) =", odd_probability, "\n")
  cat("P(x >= 4) =", p4, "\n")
  cat("P(x <= 20) =", p20, "\n")
}
geometric_stuff(0.5)

# Exercitiul I. c)
poisson_stuff <- function(lambda) {
  # Se inițializează variabilele k și limit.
  k <- 0
  limit <- 10 ^ (-7)
  # Se calculează valoarea inițială a lui x_eq_a.
  x_eq_a <- exp(-lambda)
  # Se folosește o buclă while pentru a crește valoarea lui k și a recalcula x_eq_a 
  # până când condiția ppois(k, lambda, lower.tail = FALSE) + x_eq_a < limit nu mai este îndeplinită.
  while (ppois(k, lambda, lower.tail = FALSE) + x_eq_a < limit) {
    k <- k + 1
    x_eq_a <- x_eq_a * lambda / k
  }
  # Se afișează rezultatul calculat.
  cat("Cea mai mica valoare a lui k0 astfel încât: P(Y >= k0) < 10 ^ (-7) este:", k, "\n")
}
poisson_stuff(10000000)

# Exercitiul II. a)
calculate_statistics <- function(file_name) {
  # Se citesc datele din fișierul CSV specificat în data.
  data <- read.csv(file_name)
  # Se extrage coloana P în variabila sample_P și coloana S în variabila sample_S.
  sample_P <- data[["P"]]
  sample_S <- data[["S"]]
  
  # Se afișează statistici pentru sample_P: mediana, media, deviația standard, quartile (Q1, Q2, Q3).
  cat("Statistici pentru sample-ul P:\n")
  cat("Mediana:", median(sample_P), "\n")
  cat("Media:", mean(sample_P), "\n")
  cat("Deviata standard:", sd(sample_P), "\n")
  cat("Quartile-urile:\n")
  cat("Q[1]:", quantile(sample_P, 0.25), "\n")
  cat("Q[2]:", quantile(sample_P, 0.5), "\n")
  cat("Q[3]:", quantile(sample_P, 0.75), "\n\n")

  # Se afișează statistici pentru sample_S: mediana, media, deviația standard, quartile (Q1, Q2, Q3)
  cat("Statistici pentru sample-ul S:\n")
  cat("Mediana:", median(sample_S), "\n")
  cat("Media:", mean(sample_S), "\n")
  cat("Deviatia standard:", sd(sample_S), "\n")
  cat("Quartile-urile:\n")
  cat("Q[1]:", quantile(sample_S, 0.25), "\n")
  cat("Q[2]:", quantile(sample_S, 0.5), "\n")
  cat("Q[3]:", quantile(sample_S, 0.75), "\n")
}
calculate_statistics("note.csv")

# Exercitiul II. b)
remove_outliers <- function(file_name, sample_name) {
  # Se citesc datele din fișierul CSV specificat în data.
  data <- read.csv(file_name)
  # Se extrage coloana specificată în variabila sample.
  sample <- data[[sample_name]]
  # Se calculează quartile (Q1 și Q3) și amplitudinea interquartilică (IQR).
  Q1 <- quantile(sample, 0.25)
  Q3 <- quantile(sample, 0.75)
  IQR <- Q3 - Q1
  # Se calculează limitele inferioare și superioare ale valorilor acceptabile 
  # (lower_bound și upper_bound).
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  cat(lower_bound, " : ", upper_bound, "\n")
  # Se filtrează setul de date pentru a elimina valorile care depășesc limitele calculate.
  trimmed_sample <- sample[sample >= lower_bound & sample <= upper_bound]
  # Se returnează setul de date rezultat.
  return(trimmed_sample)
}
remove_outliers("note.csv", "P")
remove_outliers("note.csv", "S")

# Exercitiul II. c)
# Funcția primește un singur argument file_name. 
# Aceasta afișează histograma distribuției frecvențelor pentru două seturi de date: P și S.
plot_frequency_distribution <- function(file_name) {
  # Se configurează divizarea spațiului grafic într-o singură linie și două coloane (par(mfrow = c(1, 2))).
  par(mfrow = c(1, 2))
  # Se afișează histograma distribuției frecvențelor pentru setul de date P, eliminând valorile extreme folosind funcția remove_outliers. 
  # Sunt specificate intervalurile binurilor (breaks), etichetele graficului, axele x și y, culoarea și conturul.
  hist(remove_outliers(file_name, "P"), breaks = seq(1, 10, by = 1),
       main = "Frecventa distributiei sample-ului P.",xlab = "Valori.", ylab = "Frecventa.",
       col = "red", border = "brown")
  # La fel și pentru setul de date S.
  hist(remove_outliers(file_name, "S"), breaks = seq(1, 10, by = 1), 
       main = "Frecventa distributiei sample-ului P.", xlab = "Valori.", ylab = "Frecventa.",
       col = "red", border = "brown")
}
plot_frequency_distribution("note.csv")