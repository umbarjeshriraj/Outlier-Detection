q90 = c(0.941, 0.765, 0.642, 0.56, 0.507, 0.468, 0.437,
       0.412, 0.392, 0.376, 0.361, 0.349, 0.338, 0.329,
       0.32, 0.313, 0.306, 0.3, 0.295, 0.29, 0.285, 0.281,
       0.277, 0.273, 0.269, 0.266, 0.263, 0.26)

q95 = c(0.97, 0.829, 0.71, 0.625, 0.568, 0.526, 0.493, 0.466,
       0.444, 0.426, 0.41, 0.396, 0.384, 0.374, 0.365, 0.356,
       0.349, 0.342, 0.337, 0.331, 0.326, 0.321, 0.317, 0.312,
       0.308, 0.305, 0.301, 0.29)

q99 = c(0.994, 0.926, 0.821, 0.74, 0.68, 0.634, 0.598, 0.568,
       0.542, 0.522, 0.503, 0.488, 0.475, 0.463, 0.452, 0.442,
       0.433, 0.425, 0.418, 0.411, 0.404, 0.399, 0.393, 0.388,
       0.384, 0.38, 0.376, 0.372)

DixonsCriticalValue <- array(c(q90,q95,q99),dim = c(28,3,1))
print(DixonsCriticalValue)

column.names <- c("Q90%","Q95%","Q99%")
row.names <- c("3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
matrix.names <- c("Dixons Critical Value")

DixonsCriticalValue <- array(c(q90,q95,q99),dim = c(28,3,1),dimnames = list(row.names,column.names,matrix.names))
print(DixonsCriticalValue)

X <- c(0.142, 0.153, 0.135, 0.002, 0.175)
X <- sort(X)
print(X)
X_len <- length(X)



Q_cal <- function(Input_val){
  if(Input_val == 1)
  {
    r10 <- (X[2]-X[1])/(X[length(X)]-X[1])
    return(as.double(r10))
  }else if(Input_val == 2)
  {
    r11 <-(X[2]-X[1])/(X[length(X)-1]-X[1])
    return(as.double(r11))
  }else if(Input_val == 3)
  {
    r21 <- (X[3]-X[1])/(X[length(X)-1]-X[1])
    return(as.double(r21))
  }else if(Input_val == 4)
  {
    r22 <- (X[3]-X[1])/(X[length(X)-2]-X[1])
    return(as.double(r22))
  }
}

ReadInput <- function(X_len){
  if(X_len < 3){
    print("Input length should be 3 or greater")
  }else if(X_len >=3 && X_len <=7){
    Input_val <- 1
    return(as.integer(Input_val))
  }else if(X_len >= 8 && Input_val <= 10){
    Input_val <- 2
    return(as.integer(Input_val))
  }else if(Input_val >= 11 && Input_val <= 13){
    Input_val <- 3
    return(as.integer(Input_val))
  }else if (Input_val >= 14){
    Input_val <- 4
    return(as.integer(Input_val))
  }
}

ReadConfidenceInterval <- function()
{ 
  n <- readline(prompt="Enter Confidence interval : ")
  if(grepl("^[0-9]+$",n))
  {
    if(n == 90 || n == 95 || n == 99 || n == 0.10 || n == 0.05 || n == 0.01){
      return(as.integer(n))
    }else{
      return(ReadConfidenceInterval())
    }
  }
}

ReadCriticalValue <- function(alpha,X_len){
  if(alpha == 90 || alpha == 0.10){
    return(q90[X_len - 2])
  }else if(alpha == 95 || alpha == 0.05){
    return(q95[X_len - 2])
  }else if(alpha == 99 || alpha == 0.01){
    return(q99[X_len - 2])
  }
}

display <- function(Q_value, Q_critical_value){
  if(Q_value > Q_critical_value){
    sprintf("Outlier : %f", X[1])
  } else {
    print("No outlier")
  }
}

Input_val <- ReadInput(X_len)
print(Input_val)

Q_value <- signif(Q_cal(Input_val),digits = 4)

alpha <- ReadConfidenceInterval()

Q_critical_value <- ReadCriticalValue(alpha,X_len)

display(Q_value,Q_critical_value)





