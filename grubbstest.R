library(outliers)
library(ggplot2)

X <- c(152.36,130.38,101.54,96.26,88.03,85.66,83.62,76.53,
       74.36,73.87,73.36,73.35,68.26,65.25,63.68,63.05,57.53)

grubbs.flag <- function(x) {
  outliers <- NULL
  test <- X
  grubbs.result <- grubbs.test(test)
  print(grubbs.result)
  pv <- grubbs.result$p.value
  print(pv)
  if (length(test) < 3 ) stop("Grubb's test requires > 2 input values")
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    print(outliers)
    test <- X[!X %in% outliers]
    print(test)
    grubbs.result <- grubbs.test(test)
    print(grubbs.result)
    pv <- grubbs.result$p.value
    print(pv)
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}

flag <- grubbs.flag(X)
print(flag)



# Plot the outliers highlighted in colour:

ggplot(grubbs.flag(X),aes(x=X,color=Outlier,fill=Outlier))+
  geom_histogram(binwidth=diff(range(X))/30)+
  theme_bw()
