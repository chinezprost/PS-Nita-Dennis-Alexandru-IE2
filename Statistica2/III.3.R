# exercițiul III.1 (necesar pentru ex III.3)
outliers_mean = function(file) {
  
  inputfile = scan(file)
  # salvăm informațiile din fișier in inputfile
  
  mean_value = mean(inputfile)
  # calculăm valoarea mediană
  sd_value = sd(inputfile)
  
  # calculăm deviația standard
  
  # calculăm limita inferioară și superioară pentru a aplica formula
  lower_bound = mean_value - 2 * sd_value
  upper_bound = mean_value + 2 * sd_value
  
  # obținem toate valorile care NU aparține limitelor de mai sus
  result = inputfile[inputfile < lower_bound | inputfile > upper_bound]
  return(result)
}

# exercițiul III.2) (la fel necesar)
outliers_iqr = function(file) {
  
  inputfile = scan(file)
  # salvăm informațiile necesare
  
  q1 = quantile(inputfile, 0.25)
  # calculăm Q1
  q3 = quantile(inputfile, 0.75)
  # calculăm Q3
  
  iqr_value = q3 - q1
  # calculăm IQR
  
  lower_bound = q1 - 1.5 * iqr_value
  upper_bound = q3 + 1.5 * iqr_value
  # calculăm limita inferioară și superioară, la fel ca mai sus
  result = inputfile[inputfile < lower_bound | inputfile > upper_bound]
  return(result)
}

outliers_mean("sample2.txt")
outliers_iqr("sample2.txt")
summary(c1, c2)

