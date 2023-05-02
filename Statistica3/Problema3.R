TLC <- function(alpha, lambda, n, N, standardized_mean) {
  
  # Generăm N sample-uri pt. distribuția Gamma
  X <- matrix(rgamma(n*N, alpha, lambda), ncol=N)
  
  # Calculăm mediana pt fiecare sample
  meanTemp <- apply(X, 2, mean)
  
  # Calculăm mediana teoretică și varianța medianei
  mu <- alpha/lambda
  sigma <- sqrt(alpha/(n*lambda^2))
  
  # Calculăm mediana
  standardized_mean <- (meanTemp - mu)/sigma
  
  # Calculăm CDF concret și teoretic
  ecdf_standardized_mean <- ecdf(standardized_mean)
  tcdf_standardized_mean <- function(x) pnorm(x)
  
  # Calculăm Kolmogorov-Smirnov
  KolmogorovS <- ks.test(standardized_mean, tcdf_standardized_mean)$statistic
  
  # Afișăm în tabel CDF teoretic și concret
  plot(ecdf_standardized_mean, main="TLC folosing distribuția Gamma", 
       xlab="Sample Mean", ylab="Probabilitate cumulativă", ylim=c(0,1))
  curve(tcdf_standardized_mean, add=TRUE, col="red")
  
  
}

#Funcția "rgamma" generează un set de "N" de eșantioane aleatorii de dimensiune "n" dintr-o distribuție Gamma cu parametrii "alpha" și "lambda".
#Funcția "apply" calculează media aritmetică pentru fiecare set de eșantioane generate în matricea "X", pe baza axei 2 (adică pe baza coloanelor).
#În continuare, sunt calculate mediana teoretică și varianța medianei utilizând formula specifică pentru distribuția Gamma.
#Aceste valori sunt utilizate pentru a standardiza media aritmetică a seturilor de eșantioane.
#Funcția "ecdf" calculează funcția distribuției cumulative empirice (ECDF) pentru mediile standardizate.
#Funcția "pnorm" calculează funcția de distribuție normală standard pentru a obține CDF teoretic.
#Funcția "ks.test" calculează testul Kolmogorov-Smirnov între CDF-ul empiric și CDF-ul teoretic și returnează statisticile testului.
#Funcția "plot" afișează CDF empiric pe grafic, iar funcția "curve" afișează CDF teoretic în aceeași grafică.