#1 lab ЭММ
theta <- 
AR <- function (tetta)
{
  x <- numeric
  x[1] <- rnorm(1)
  for (k in 2:n) 
  {
    x[k] <- theta * x[k-1] + rnorm(1)
  }
  return(x)
}