poisson_prob = function(a, n) {
  probs = dpois(0:n, lambda = x) # calculez probabilitatiile
  names(probs) = 0:n # salvez probabilitatiile in variabila (le dau nume)
  barplot(probs, main = "Functia lui Poisson de probabilitate.", 
          xlab = "Numarul de evenimente", ylab = "Probabilitatea", 
          col = "blue", border = NA) # afisez probabilitatile (le dam plot)
}