mecanici = function()
{
  # Definim rata pentru primul mecanic.
  valoarea_lambda1 <- 4
  # Definim rata pentru al doilea mecanic.
  valoarea_lambda2 <- 12
  # Probabilitatea de servire pentru cel mai rapid mecanic.
  probabilitatea_maiRapid <- 3/4
  # Generăm timpul X.
  if(runif(1) < probabilitatea_maiRapid) 
  { 
    X <- rexp(1, valoarea_lambda2)
  } 
  else 
  {
    X <- rexp(1, valoarea_lambda1)
  }
  #Afisăm timpul X.
  cat("Timpul de servire:", X, "ore\n")
  # Definim numărul de simulări.
  nr_simulari <- 10000
  # Calculăm timpul mediu.
  X_vals <- numeric(nr_simulari)
  for(i in 1:nr_simulari) {
    if(runif(1) < probabilitatea_maiRapid) 
    {
      X_vals[i] <- rexp(1, valoarea_lambda2)
    } 
    else 
    {
      X_vals[i] <- rexp(1, valoarea_lambda1)
    }
  }
  # Afișăm timpul mediu.
  cat("Expected service time:", mean(X_vals), "hours\n")
}