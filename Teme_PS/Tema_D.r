
# Exercițiul I)
# Funcția găsește_element_MC caută un element într-un vector x utilizând metoda Montecarlo.
#  x (vectorul de căutat) 
#  și k (numărul maxim de iterații).

găsește_element_MC = function(x, k) {
  # Se inițializează variabila i cu 0 pentru a ține evidența numărului de iterații efectuate.
  i = 0
  # Se utilizează o buclă while care se execută până când numărul de iterații i devine mai mare sau egal cu k.
  while (i < k) {
    # În fiecare iterație, se extrage un element aleatoriu din vectorul x utilizând funcția sample.
    număr = sample(x, 1)
    # Se inițializează variabila număr_apariții cu 0 pentru a număra de câte ori apare elementul extrage în vectorul x.
    număr_apariții = 0
    j = 1
    # Se utilizează o buclă while pentru a parcurge fiecare element din vectorul x și se incrementează număr_apariții de fiecare dată când se găsește o potrivire.
    while (j <= length(x)) {
      if (x[j] == număr) {
        număr_apariții = număr_apariții + 1
      }
      j = j + 1
    }
    # Dacă număr_apariții devine mai mare sau egal cu jumătatea lungimii vectorului x plus 1, înseamnă că elementul 
    # a fost găsit suficient de des în vector și se returnează elementul găsit.
    if (număr_apariții >= length(x) / 2 + 1) {
      return (număr)
    }
    i = i + 1
  }
  # Dacă nu se găsește elementul în cele k iterații, se returnează mesajul "x nu deține un element m.".
  return ("x nu deține un element m.")
}

x = sample(1:2, 100, replace = TRUE)
găsește_element_MC(x, 24)
# Apelul funcției găsește_element_MC(x, 24) caută un element în vectorul x folosind metoda Montecarlo, cu un număr maxim de 24 de iterații.


# Exercițiul II)
# Se definește funcția element_ith cu doi parametri: i (poziția elementului) și A (vectorul de căutat).
element_ith = function(i, A) {
  # Se extrage un element aleatoriu din vectorul A și se stochează în variabila z utilizând funcția sample.
  z = sample(A, 1)
  # Se formează doi noi vectori: Alt care conține elementele mai mici decât z,
  Alt = A[A < z]
  # și Agt care conține elementele mai mari decât z.
  Agt = A[A > z]
  # Dacă lungimea vectorului Alt este mai mare decât i, înseamnă că elementul căutat se află în Alt, deci se apelează recursiv funcția element_ith cu i și Alt.
  if (length(Alt) > i) {
    return (element_ith(i, Alt))
  }
  # În caz contrar, se verifică dacă A conține elemente suficiente pentru a conține elementul căutat. Dacă da, înseamnă că elementul căutat este z și se returnează acesta.
  else {
    if (length(A) > i + length(Agt)) {
      return (z);
    }
    else {
      return (element_ith(i - length(A) + length(Agt), Agt))
    }
  }
}
# Apelurile element_ith(54, x) și element_ith(55, x) găsesc al 54-lea, respectiv al 55-lea element cel mai mic în vectorul x.
# Apelurile sort(x)[55] și sort(x)[56] afișează al 55-lea, respectiv al 56-lea element cel mai mic în vectorul x folosind sortarea.

element_ith(54, x)
sort(x)[55]
element_ith(55, x)
sort(x)[56]



# Exercițiul 3)
# S (setul de date) și a (un parametru care controlează precizia estimării).
găsește_mediană_MC = function(S, a) {
  # Dacă a este mai mic decât 0, se transformă în valoarea absolută a acestuia.
  if (a < 0) {
    a = -a
  }
  # Se calculează un număr m aproximativ de elemente care trebuie extrase din setul de date pentru a estima mediana. 
  # Valoarea m este determinată de a și logaritmul în baza 2 al lungimii setului de date S.
  m = floor(a * log(length(S)))
  # Se extrage un eșantion SS de m elemente din setul de date S utilizând funcția sample.
  SS = sample(S, m)
  # Eșantionul SS este sortat și rezultatul este stocat în variabila SS_sortat.
  SS_sortat = sort(SS)
  # Se calculează indexul medianei în eșantionul sortat ca fiind ceil(m / 2).
  index_mediană = ceiling(m / 2)
  # Se returnează elementul de la indexul medianei în eșantionul sortat.
  return (SS_sortat[index_mediană])
}

x = runif(4473, 0, 100)
median(x)
# Apelul găsește_mediană_MC(x, 532) estimează mediana setului de date x utilizând metoda Montecarlo, 
# cu un parametru de precizie a de 532.
găsește_mediană_MC(x, 532)

