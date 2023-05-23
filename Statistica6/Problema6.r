#S1.1) function
selection_mean = function(file) {
  x = scan(file);
  m = mean(x)
  return(m)
}

#S2.1) function
zconfidence_interval = function(n,sample_mean,alpha,sigma){
  critical_z=qnorm(1-alpha/2,0,1)
  a=sample_mean-critical_z*sigma/sqrt(n)
  b=sample_mean+critical_z*sigma/sqrt(n)
  interval=c(a,b)
  return(interval)
}

#S2.6) function
zconfidence_interval_file = function(file_path, alpha) {
  data <- scan(file_path)
  sample_mean <- mean(data)  
  n <- length(data)  
  sigma <- sd(data)
  critical_z <- qnorm(1 - alpha / 2, 0, 1)
  a <- sample_mean - critical_z * sigma / sqrt(n)
  b <- sample_mean + critical_z * sigma / sqrt(n)
  interval <- c(a, b)
  return(interval)
}

#S3.1) function
t_conf_interval=function(n,sample_mean,s,alpha){
  se=s/sqrt(n)
  critical_t=qt(1-alpha/2,n-1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  interval=c(a,b)
  return(interval)
}

#S3.4) function
t_conf_interval_file <- function(file_path, alpha) {
  data <- scan(file_path) 
  n <- length(data)  
  sample_mean <- mean(data) 
  s <- sd(data)  
  se <- s / sqrt(n)
  critical_t <- qt(1 - alpha / 2, n - 1)
  a <- sample_mean - critical_t * se
  b <- sample_mean + critical_t * se
  interval <- c(a, b)
  return(interval)
}

#S4.1) function
test_proportion = function(alpha,n,succese,p0,tip_ip){
  p_prim = succese/n
  z_score = (p_prim - p0)/sqrt(p0*(1 - p0)/n)
  #cat("z_score=",z_score)
  if(tip_ip=="r"){
    critical_z = qnorm(1 - alpha, 0, 1)
   cat("Z Critical -> ",critical_z)
    if(z_score<=critical_z)
      print("H[0] nu se poate respinge!")
    else
      print("H[0] se respinge! (continuu)")
  }
  if(tip_ip=="l"){
    critical_z = qnorm(alpha, 0, 1)
   cat("Z Critical -> ",critical_z)
    if(z_score>=critical_z)
      print("H[0] nu se poate respinge!")
    else
      print("H[0] se respinge! (continuu)")
  }
  if(tip_ip=="s"){
    critical_z = qnorm(1-alpha/2, 0, 1)
   cat("critical_z=",critical_z)
    if(abs(z_score)<=critical_z)
      print("H[0] nu se poate respinge!")
    else
      print("H[0] se respinge! (continuu)")
  }
}

selection_mean("history.txt")
#S2)
  alpha=0.1
  sample_mean=20
  n=100
  sigma=sqrt(9)
  critical_z=qnorm(1-alpha/2,0,1)
  a=sample_mean-critical_z*sigma/sqrt(n)
  b=sample_mean+critical_z*sigma/sqrt(n)
  interval=c(a,b)
  interval

#S2.1) ->
#S2.2)...
zconfidence_interval(25,67.53,0.1,10)
#S2.3)
zconfidence_interval(50,5,0.05,0.5)
#S2.6) ->

  file_path <- "history.txt" 
  alpha <- 0.05  # nivelul de semnificație (1 - nivelul de încredere)
  interval <- zconfidence_interval_file(file_path, alpha)
  interval

#S3.0)
  alpha=0.05
  sample_mean=3.3
  n=60
  s=0.4
  se=s/sqrt(n)
  critical_t=qt(1-alpha/2,n-1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  interval=c(a,b)
  interval
#S3.1)->
#S3.2)...
  t_conf_interval(196,44.65,sqrt(2.25),0.01)
#S3.3)
#SA)
  t_conf_interval(49,12,1.75,0.01)
  t_conf_interval(49,12,1.75,0.05)
#SB)
  t_conf_interval(49,13.5,1.25,0.07)
#S3.4) ->
  file_path <- "history.txt"  
  alpha <- 0.05  # nivelul de semnificație (1 - nivelul de încredere)

  interval <- t_conf_interval_file(file_path, alpha)
  interval
#S3.5)
  file_path <- "sample.txt" 
  alpha <- 0.1  # nivelul de semnificație (1 - nivelul de încredere) - pentru intervalul de 90%
  interval_90 <- t_conf_interval_file(file_path, alpha)
  interval_90
  alpha <- 0.05  # nivelul de semnificație (1 - nivelul de încredere) - pentru intervalul de 95%
  interval_95 <- t_conf_interval_file(file_path, alpha)
  interval_95
  alpha <- 0.01  # nivelul de semnificație (1 - nivelul de încredere) - pentru intervalul de 99%
  interval_99 <- t_conf_interval_file(file_path, alpha)
  interval_99

#S4)
  alpha=0.01
  n=100
  succese=63
  p_prim=succese/n
  p0=0.6
  z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n)
  critical_z=qnorm(1-alpha,0,1)
  z_score
  critical_z

#S4.1)->

#S4.2)...
test_proportion(alpha = 0.05, n = 150, succese = 20, p0 = 0.10, tip_ip = "s")
