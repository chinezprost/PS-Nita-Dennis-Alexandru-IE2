# A doua problemă pentru laboratorul 3 (2 Mai)
# Function to verify the LLN for the sequence of random variables Xi: Student(r)
LNM_Function <- function(r, n) {
  # Generate a sequence of n random variables from Student(r) distribution
  # Generăm o secvență de n variabile aleatorii din distribuția Student(r)
  X <- rt(n, df = r)
  # Calculăm mediana.
  mediana <- mean(X)
  # Afișăm mediana și valoarea așteptată (pentru Student(r) avem 0)
  cat("Mediana:", mediana, "\n")
  cat("Valoare asteptată:", 0, "\n")
}

#Funcția "rt" generează o secvență de "n" variabile aleatoare cu distribuția Student(r) folosind parametrul "df" care specifică numărul de grade de libertate ale distribuției Student.
#Funcția "mean" calculează media aritmetică a variabilelor aleatoare generate de la pasul anterior și o stochează în variabila "mediana".
#Funcția "cat" afișează pe ecran mediana și valoarea așteptată. În acest caz, valoarea așteptată este 0, deoarece distribuția Student are media 0.
#Astfel, atunci când apelăm funcția "LNM_Function(r, n)", aceasta va genera o secvență de "n" variabile aleatoare cu distribuția Student(r), va calcula media aritmetică a acestor variabile aleatoare și va afișa pe ecran mediana și valoarea așteptată (care în acest caz este 0). Prin urmare, putem utiliza această funcție pentru a verifica dacă LLN se aplică pentru această secvență de variabile aleatoare.

