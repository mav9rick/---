# Установка и загрузка пакетов
# install.packages("dplyr")
# library(dplyr)
# install.packages("Deriv")
# library(Deriv)
n = 1000
t1 = 0.3
t2 = 1
t3 = 3
tetta <- c(t1, t2, t3)

# AR процесс
AR <- function(theta, n) 
{
  x <- numeric(n)
  x[1] <- rnorm(1)
  for (k in 2:n) 
  {
    x[k] <- theta * x[k-1] + rnorm(1)
  }
  return(x)
}

# Генерация данных для разных значений theta
AR1 = AR(t1, n)
AR2 = AR(t2, n)
AR3 = AR(t3, n)

# Функция для построения графика
printFunc <- function(data) 
{
  plot(data, type="l", col = "green", main = "Линейный график", xlab = "Наблюдение", ylab = "Значения")
}
printFunc(AR1)
printFunc(AR2)
printFunc(AR3)

# 2 задание 
# Функция вычисления по МНК
MNK <-function(t,AR,k,n)
{
  # Функция суммы квадратов ошибок
  sum_of_squares <- function(t, AR,k,n) 
  {
    r <- 0 
    for (i in k:n)
    {
      r <- r + (AR[i] - t * AR[i-1])^2
    }
    return(r)
  }
  # Функция для численного вычисления производной
  numerical_derivative <- function(func, x, AR,k,n) 
  {
    h <- 1e-5  # Маленькое значение для приближения
    return((func(x + h, AR,k,n) - func(x - h, AR,k,n)) / (2 * h))
  }
  find_roots <- function(t) 
  {
    numerical_derivative(sum_of_squares, t, AR1,k,n)
  }
  root <- uniroot(find_roots, c(-10, 10))$root
  return(root)
}
MNK1 = MNK(t1,AR1,2,1000)
cat("Корень производной:", MNK1, "\n")
#МП 3 задание
MMP <- function(func,AR)
{
  result <- optimize(sum_of_squares, interval = c(-10, 10), AR)
  tettaRes <- result$minimum
  print(paste("Вычисленное значение tetta:", tettaRes))
}
MMP1 = MMP(MNK1,AR1)
# 4 задание
ntheta = 0.8
nAR = AR(ntheta,1000)

vector <- function(t,AR,kmin,kmax)
{
  k = kmin
  n = kmax
  v <- numeric(kmax-kmin)
  for (i in 1:length(v))
  {
    v[i] <- MNK(t,AR,k+i,n)
  }
  return (v)
}
MNK3 = vector(ntheta,nAR,10,1000)
MMP2 = MMP(MNK3,nAR)

printFunc(MNK3)
MNK3
# 5 задание


