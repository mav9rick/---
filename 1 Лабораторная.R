# Установка и загрузка пакетов
# install.packages("dplyr")
# library(dplyr)
# install.packages("Deriv")
# library(Deriv)
# install.packages("stats")
# library(stats)
# install.packages("forecast")
# library(forecast)
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
cat("Корень производной:", MNK3[1], "\n")
MMP2 = MMP(MNK3,nAR)

printFunc(MNK3)

# 5 задание
AR2 <- function(theta1,theta2, n) 
{
  x <- numeric(n)
  x[1:2] <- rnorm(2)
  for (k in 3:n) 
  {
    x[k] <- theta1 * x[k-1] + theta2 * x[k-2] + rnorm(1)
  }
  return(x)
}

ntheta2 = 0.4
ntheta3 = 0.1
nAR2 = AR2(ntheta2,ntheta3,1000)

stationarity <- function(theta1, theta2) 
  {
  discrim <- theta1^2 - 4 * theta2
  # Если дискриминант отрицательный то корни будут комплексными
  if (discrim < 0) 
    {
    # Для комплексных корней 
    lambda_r <- theta1 / 2
    lambda_im <- sqrt(abs(discrim)) / 2
    m <- sqrt(lambda_r^2 + lambda_im^2)
    # условие стационарности: модуль корня должен быть меньше 1
    st <- m < 1
    } 
  else 
    {
    # корни действительные
    lambda1 <- (theta1 + sqrt(discrim)) / 2  # 1-ый корень
    lambda2 <- (theta1 - sqrt(discrim)) / 2  # 2-ой корень
    #условие стационарности
    st <- abs(lambda1) < 1 && abs(lambda2) < 1
    }
  return(st)
}
# Построение графика после проверок на стационарность
if (stationarity(ntheta2, ntheta3)) 
{
cat("Процесс стационарен\n")
printFunc(nAR2)
} else 
{
cat("Процесс не стационарен \n")
}

# Задание 6 

ntheta1 <- 0.6
ntheta2 <- -0.4
n <- 1000
x <- AR2(ntheta2, ntheta3,n)

arima_model <- arima(x, order = c(2, 0, 0), include.mean = FALSE)

forecasted_values <- forecast(arima_model, h = 10)

plot(forecasted_values)
