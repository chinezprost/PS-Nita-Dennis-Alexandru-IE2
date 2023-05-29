
# Exercițiul I)
găsește_element_MC = function(x, k) {
  i = 0
  while (i < k) {
    număr = sample(x, 1)
    număr_apariții = 0
    j = 1
    while (j <= length(x)) {
      if (x[j] == număr) {
        număr_apariții = număr_apariții + 1
      }
      j = j + 1
    }
    if (număr_apariții >= length(x) / 2 + 1) {
      return (număr)
    }
    i = i + 1
  }
  return ("x nu deține un element m.")
}
x = sample(1:2, 100, replace = TRUE)
găsește_element_MC(x, 24)
# Se apelează funcția găsește_element_MC cu x și k=24.

# Exercițiul II)
element_ith = function(i, A) {
  z = sample(A, 1)
  Alt = A[A < z]
  Agt = A[A > z]
  if (length(Alt) > i) {
    return (element_ith(i, Alt))
  }
  else {
    if (length(A) > i + length(Agt)) {
      return (z);
    }
    else {
      return (element_ith(i - length(A) + length(Agt), Agt))
    }
  }
}
element_ith(54, x)
sort(x)[55]
element_ith(55, x)
sort(x)[56]



# Exercițiul 3)
găsește_mediană_MC = function(S, a) {
  if (a < 0) {
    a = -a
  }
  m = floor(a * log(length(S)))
  SS = sample(S, m)
  SS_sortat = sort(SS)
  index_mediană = ceiling(m / 2)
  return (SS_sortat[index_mediană])
}

x = runif(4473, 0, 100)
median(x)
găsește_mediană_MC(x, 532)

