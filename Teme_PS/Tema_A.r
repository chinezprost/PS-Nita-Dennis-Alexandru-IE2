# Exercitiul I. a)
three_plots <- function(k, n, p, lambda) {
  x <- k:n
  y_poisson <- dpois(x, lambda)
  y_geometric <- dgeom(x, p)
  y_binomial <- dbinom(x, n, p)
  
  plot(x, y_poisson, type = "l", main = "Mass Probability Functions", lwd = 2, lty = 1, 
       col = "blue", ylim = c(0, max(y_poisson, y_geometric, y_binomial)), xlab = "k:n", 
       ylab = "Probability") 
  lines(x, y_geometric, type = "l", lwd = 2, lty = 2, col = "red")
  lines(x, y_binomial, type = "l", lwd = 2, lty = 3, col = "green")
  legend("topright", c("Poisson", "Geometric", "Binomial"), lty = c(1, 2, 3), #(1 - solid, 2 - dashed, 3 - dotted)
         lwd = 2, col = c("blue", "red", "green")) 
}
three_plots(3, 10, 0.4, 1)

# Exercitiul I. b)
geometric_stuff <- function(p) {
  odd_probability <- 1 / (2 - p)
  p4 <- pgeom(4, p, lower.tail = FALSE) + (1 - p) ^ 3 * p
  p20 <- pgeom(20, p)
  cat("P(x = odd) =", odd_probability, "\n")
  cat("P(x >= 4) =", p4, "\n")
  cat("P(x <= 20) =", p20, "\n")
}
geometric_stuff(0.5)

# Exercitiul I. c)
poisson_stuff <- function(lambda) {
  k <- 0
  limit <- 10 ^ (-7)
  x_eq_a <- exp(-lambda)
  while (ppois(k, lambda, lower.tail = FALSE) + x_eq_a < limit) {
    k <- k + 1
    x_eq_a <- x_eq_a * lambda / k
  }
  cat("Cea mai mica valoare a lui k0 astfel încât: P(Y >= k0) < 10 ^ (-7) este:", k, "\n")
}
poisson_stuff(10000000)

# Exercitiul II. a)
calculate_statistics <- function(file_name) {
  data <- read.csv(file_name)
  sample_P <- data[["P"]]
  sample_S <- data[["S"]]
  cat("Statistici pentru sample-ul P:\n")
  cat("Mediana:", median(sample_P), "\n")
  cat("Media:", mean(sample_P), "\n")
  cat("Deviata standard:", sd(sample_P), "\n")
  cat("Quartile-urile:\n")
  cat("Q[1]:", quantile(sample_P, 0.25), "\n")
  cat("Q[2]:", quantile(sample_P, 0.5), "\n")
  cat("Q[3]:", quantile(sample_P, 0.75), "\n\n")
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
  data <- read.csv(file_name)
  sample <- data[[sample_name]]
  Q1 <- quantile(sample, 0.25)
  Q3 <- quantile(sample, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  cat(lower_bound, " : ", upper_bound, "\n")
  trimmed_sample <- sample[sample >= lower_bound & sample <= upper_bound]
  return(trimmed_sample)
}
remove_outliers("note.csv", "P")
remove_outliers("note.csv", "S")

# Exercitiul II. c)
plot_frequency_distribution <- function(file_name) {
  par(mfrow = c(1, 2))
  hist(remove_outliers(file_name, "P"), breaks = seq(1, 10, by = 1),
       main = "Frecventa distributiei sample-ului P.",xlab = "Valori.", ylab = "Frecventa.",
       col = "red", border = "brown")
  hist(remove_outliers(file_name, "S"), breaks = seq(1, 10, by = 1), 
       main = "Frecventa distributiei sample-ului P.", xlab = "Valori.", ylab = "Frecventa.",
       col = "red", border = "brown")
}
plot_frequency_distribution("note.csv")