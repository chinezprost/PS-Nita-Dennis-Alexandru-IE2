# Prima problemă de la laborator 3 (2 Mai)
graphic_distribution = function(mu, sigma)
{
# Creăm secvența de valori.
mySequence = seq(-5, 5, length.out = 1000)

# Calculăm PDF-ul în funcția de secvența de mai sus și parametrii de la tastatură.
pdf = dnorm(mySequence, mean = mu, sd = sigma)

# Creăm graphic-ul în funcița de PDF-ul calculat mai sus.
plot(mySequence, pdf, type = "l", xlab = "Secvența", ylab = "Densitate",
     main = paste0("(µ=", mu, ", σ=", sigma^2, ") (Funcția de Densitate)"))
}

#Funcția "seq" creează o secvență de valori de la -5 la 5 cu o lungime de 1000.
#Funcția "dnorm" calculează funcția de densitate a probabilității (PDF) pentru distribuția normală cu media "mu" și deviația standard "sigma" pentru secvența de valori create anterior.
#Funcția "plot" trasează graficul funcției de densitate a probabilității pentru secvența de valori și PDF-ul calculat mai devreme. Parametrul "type = "l"" specifică tipul de grafic ca fiind o linie și xlab și ylab specifică denumirea axelor x și y. "main" adaugă titlul graficului și utilizează valori specifice ale parametrilor mu și sigma.
#Astfel, atunci când apelăm funcția "graphic_distribution(mu, sigma)", se vor crea valorile pentru secvența de valori, se va calcula PDF-ul pentru distribuția normală și se va trasa graficul. Graficul va afișa densitatea probabilității pentru distribuția normală cu media "mu" și deviația standard "sigma" specificată de utilizator.