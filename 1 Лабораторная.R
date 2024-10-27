#1 lab ЭММ
n = 500
t1 = 0.3
t2 = 1
t3 = 3
tetta <- c(t1,t2,t3) 
AR <- function (tetta,n)
{
  x <- numeric(n)
  x[1] <- rnorm(1)
  for (k in 2:n) 
  {
    x[k] <- tetta * x[k-1] + rnorm(1)
  }
  return(x)
}
AR1 = AR(t1,n)
AR2 = AR(t2,n)
AR3 = AR(t3,n)
printFunc <- function (data)
{
  plot(data, type="l", col = "green", main = "Линейный график", xlab = "Наблюдение", ylab = "Значения") 
}
printFunc(AR1)
printFunc(AR2)
printFunc(AR3)