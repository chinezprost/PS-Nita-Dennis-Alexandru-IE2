simulate_random_value<- function(prob, values) 
{
    if (any(prob < 0) || sum(prob) != 1) 
    {
        stop("Probabilitatile nu sunt pozitive sau suma lor nu este 1.");
    }
    if (length(values) != length(prob)) 
    {
        stop("Trebuie sa fie la fel de multe values ca si probabilitati. Nr lor trebuie sa fie egal!")
    }
    interval = c(0, cumsum(prob))
    r = runif(1)
    for (i in 1:(length(interval) - 1)) {
        if (interval[i] < r && r <= interval[i + 1]) {
        return(values[i])
        }
    }
  
    stop("Eroare: Valoarea aleatorie nu a putut fi generata!")
}

values = c(1, 2, 3, 4, 5)
prob = c(0.1, 0.2, 0.3, 0.1, 0.3)

result = simulate_random_value(prob, values)
print(result)