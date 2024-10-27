n = 500
t1 = 0.3
t2 = 1
t3 = 3
tetta <- c(t1, t2, t3)

# AR процесс
AR <- function(theta, n) {
  x <- numeric(n)
  x[1] <- rnorm(1)
  for (k in 2:n) {
    x[k] <- theta * x[k-1] + rnorm(1)
  }
  return(x)
}

# Генерация данных для разных значений theta
AR1 = AR(t1, n)
AR2 = AR(t2, n)
AR3 = AR(t3, n)

# Функция для построения графика
printFunc <- function(data) {
  plot(data, type="l", col = "green", main = "Линейный график", xlab = "Наблюдение", ylab = "Значения")
}
printFunc(AR1)
printFunc(AR2)
printFunc(AR3)

# Функция суммы квадратов ошибок
sum_of_squares <- function(t, AR) {
  r <- 0 
  for (i in 2:length(AR)) {
    r <- r + (AR[i] - t * AR[i-1])^2
  }
  return(r)
}

# Оптимизация для нахождения theta
result <- optimize(sum_of_squares, interval = c(-1, 1), AR = AR1)
theta_hat <- result$minimum

print(paste("Вычисленное значение theta:", theta_hat))
