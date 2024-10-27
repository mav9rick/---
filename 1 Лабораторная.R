# Установка и загрузка пакетов
# install.packages("dplyr")  # Убедитесь, что пакет установлен
# library(dplyr)
# install.packages("Deriv")   # Убедитесь, что пакет установлен
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

# Функция суммы квадратов ошибок
sum_of_squares <- function(t, AR) 
{
  r <- 0 
  for (i in 2:n)
  {
    r <- r + (AR[i] - t * AR[i-1])^2
  }
  return(r)
}

# Функция для численного вычисления производной
numerical_derivative <- function(func, x, AR) 
{
  h <- 1e-5  # Маленькое значение для приближения
  return((func(x + h, AR) - func(x - h, AR)) / (2 * h))
}

find_roots <- function(t) 
{
  numerical_derivative(sum_of_squares, t, AR1)
}

root <- uniroot(find_roots, c(-10, 10))$root
cat("Корень производной:", root, "\n")

#МП
result <- optimize(sum_of_squares, interval = c(-10, 10), AR = AR1)
tettaRes <- result$minimum
print(paste("Вычисленное значение tetta:", tettaRes))
