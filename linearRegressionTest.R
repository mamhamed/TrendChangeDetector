linearRegressionTest <- function(c0, c, x){
  ## input: 
  ## c0: a linear regression model (output of lm)
  ## c: a linear regression model (output of lm)
  ## x: observation values
  ##
  ## output: the t-score value for linear model change
  ## reference: http://en.wikipedia.org/wiki/Student's_t-test
  n = length(x)
  SSR <- sum(c$residuals^2)
  SSRx <- sum((x-mean(x))^2)
  
  beta0 <- c0$coefficients[2] 
  beta <- c$coefficients[2]
  tscore_beta <- (beta-beta0)*sqrt(n-2)/sqrt(SSR/SSRx)
  
  tscore <- tscore_beta
  
  cat(paste("p value: " , pt(tscore, df=n-2), "\n=====\n"))
  
  return (pt(tscore, df=n-2))
}
  