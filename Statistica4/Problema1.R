parabolasEstimation = function()
{
  # Definim functia data.
  f = {-2*x^2 + 5*x - 2}
  # Definim limitele integralei pe care trebuie să o calculăm.
  a = 0
  b = 2
  # Definim intervalul.
  n = 10000
  # Generăm endpoints-urile pentru interval.
  x = seq(a, b, length.out=n+1)
  # Evaluăm funcția la endpoint-uri.
  y = f(x)
  # Calculăm area folosing regula trapezoidală.
  aria_estimata = sum(diff(x)*(y[-1]+y[-n])/2)
  cat("Aria estimată:", aria_estimata, "\n")
  # Definim integrala.
  integrand = {-2*x^2 + 5*x - 2}
  # Integrăm.
  aria_exacta = integrate(integrand, a, b)$value
  cat("Aria exacta:", aria_exacta, "\n")
  # Calculăm eroarea relativă.
  eroare_relativa <- abs(aria_estimata - aria_exacta) / aria_exacta
  cat("Eroare relativa:", eroare_relativa, "\n")
}