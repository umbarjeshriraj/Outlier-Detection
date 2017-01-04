# Inter Quartlie method to detect Outlier 

X <- c(152.36,130.38,101.54,96.26,88.03,85.66,83.62,76.53,
       74.36,73.87,73.36,73.35,68.26,65.25,63.68,63.05,57.53)

x_sort <- sort(X)
print(x_sort)

x_length <- length(x_sort)
print(x_length)

flag <- vector(mode="numeric", length=x_length)
print(flag)


num = as.integer(x_length)
if((num %% 2) == 0){
  
  lower_num <- (num/2)
  upper_num <- (lower_num+1)
  
  Q1 <- median(x_sort[1:lower_num])
  Q3 <- median(x_sort[upper_num:num])
  
  IQR_val <- IQR_cal(Q1,Q3)
  print(paste0("Inter Quartile value is : ", IQR_val))
  
  lower_limit <- LowerRange_cal(IQR_val,Q1)
  print(paste0("Lower limit value is : ",lower_limit ))
  
  upper_limit <- UpperRange_cal(IQR_val,Q3)
  print(paste0("Upper limit value is : ",upper_limit ))
  
  display(lower_limit,upper_limit)

}else{
  
  x_q2 <- num/2 
  x_q2 <- floor(0.5 + x_q2)
  print(x_sort[x_q2])
  
  lower_num <- x_q2 - 1
  upper_num <- x_q2 + 1
  
  Q1 <- median(x_sort[1:lower_num])
  Q3 <- median(x_sort[upper_num:num])
  
  IQR_val <- IQR_cal(Q1,Q3)
  print(paste0("Inter Quartile value is : ", IQR_val))
  
  lower_limit <- LowerRange_cal(IQR_val,Q1)
  print(paste0("Lower limit value is : ",lower_limit ))
  
  upper_limit <- UpperRange_cal(IQR_val,Q3)
  print(paste0("Upper limit value is : ",upper_limit ))
  
  display(lower_limit,upper_limit)
}


IQR_cal <- function(Q1,Q3){
  IQR_val = Q3-Q1
  return(as.integer(IQR_val))
}

LowerRange_cal <- function(IQR_val,Q1){
  lower_limit <- (Q1-(1.5*IQR_val))
  return(lower_limit)
}

UpperRange_cal <- function(IQR_val,Q3){
  upper_limit <- (Q3+(1.5*IQR_val))
  return(upper_limit)
}

display <- function(lower_limit,upper_limit)
{
  X <- sort(X)
  X_len <- length(X)
  Outliers <- vector(mode="numeric", length=x_length)
  for(i in 1:X_len){
    if(( X[i] < lower_limit) | ( X[i] > upper_limit)){
      print(paste0("Outlier is :", X[i]))
      Outliers[i] <- 1
      print(Outliers)
      flag <- Outliers
    }else{
      print(paste0("Not an outlierr : ", X[i]))
      Outliers[i] <- 0
      print(Outliers)
      flag <- Outliers
    }
  }
  print(Outliers)
}


