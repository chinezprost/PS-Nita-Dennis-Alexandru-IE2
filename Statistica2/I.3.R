inputfile = read.csv("life_expect.csv")
# citim din CSV-ul dat informațiile date
intervals = seq(min(inputfile$female, inputfile$male), max(inputfile$female, inputfile$male), length.out = 8)

par(mfrow = c(1, 2)) 

# poziționăm histogramele pe aceeași linie
hist(inputfile$female, breaks = intervals, main = "Speranța de viață, la naștere, a femeielor.", xlab = "(SP)")
# compunem histograma care reprezintă speranța de viață, la naștere, a femeielor.
hist(inputfile$male, breaks = intervals, main = "Speranța de viață, la naștere, a bărbațior.", xlab = "(SP)")
# de asemenea, construim și histograma care reprezintă speranța de viață, la naștere, a bărbaților.
