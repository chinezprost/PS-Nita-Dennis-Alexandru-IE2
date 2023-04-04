geometric_plot = function(p, n) {
  # calculez probabilitatea primei valori n in functie de p
  probabilitate = dgeom(1:n, prob = p)
  
  # desenez un grafic de tip bară
  barplot(probabilitate, names.arg = 1:n, xlab = "Valoare", ylab = "Probabilitate", 
          main = paste("Distribuita geometrica:", p, "Valoarea n:", n))
}


