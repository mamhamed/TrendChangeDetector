locateRegimeChange <- function(a, initialWindowSize, incrementalWindowSize, plotScore=FALSE, pvalue_thr=0.01){
  ## input: 
  ## a: data has in the form of [X, Y] X, Y are vectors
  ## initialWindowSize: size of the first window to fit the model
  ## incrementalWindowSize: the size of sliding window (test window)
  ## plotScore: if TRUE it will plot the t-score value
  ## pvalue_thr: p-value threshold
  ##
  ## output: the index number at which the linear model changes
  
  
  x0 <- a[ 1: initialWindowSize ,1] 
  y0 <- a[ 1: initialWindowSize ,2] 
  c0 <- lm(y0~x0)
  
  regime_change_point = NULL
  while (initialWindowSize < len){
    x1 <- a[ (initialWindowSize + 1): min(initialWindowSize + incrementalWindowSize, len) ,1] 
    y1 <- a[ (initialWindowSize + 1): min(initialWindowSize + incrementalWindowSize, len) ,2] 
    
    c <- lm(y1~x1)
    
    cat(paste("@ Point ", initialWindowSize, "\n"))
    cat(paste("slope up to now: ", c0$coefficients[2], "\n"))
    cat(paste("new slope: ", c$coefficients[2], "\n"))
    tscore <- linearRegressionTest(c0,c,x1)
    
    
    if(tscore < pvalue_thr){
      regime_change_point <- initialWindowSize
      break
    }
    else{
      initialWindowSize <- initialWindowSize + incrementalWindowSize
      x0 <- a[ 1: initialWindowSize ,1] 
      y0 <- a[ 1: initialWindowSize ,2] 
      c0 <- lm(y0~x0)
    }
  }
  
  return (regime_change_point)
  
  
}
