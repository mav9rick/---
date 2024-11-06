# Проверка наличия библиотеки tseries
if (!require(tseries)) 
{
  install.packages("tseries")
}
library(tseries)
# Задание 1 ----
# Моделирование процесса AR(2)ARCH(3)

n <- 2100 
theta <- c(-0.3, 0.4)
A <- c(1, 0.2, 0.1, 0.2)

# Генерируем шум для модели - независимые нормальные случайные величины
epsilon <- rnorm(n)

# Создаем векторы для хранения значений
x <- rep(0, n)
sigma2 <- rep(0, n)

# Инициализация начальных значений для sigma^2 и x
sigma2[1:3] <- var(epsilon)  # начальная дисперсия
x[1:3] <- rnorm(3)  # случайные начальные значения x

# Генерация процесса AR(2)ARCH(3)
for (i in 4:n) 
{
  sigma2[i] <- A[1] + A[2] * x[i-1]^2 + A[3] * x[i-2]^2 + A[4] * x[i-3]^2
  x[i] <- theta[1] * x[i-1] + theta[2] * x[i-2] + sqrt(sigma2[i]) * epsilon[i]
}

# Построение графика полученного процесса
plot(x, type = "l", col = "blue", main = "AR(2)ARCH(3) процесс", xlab = "Время", ylab = "x_n")

# Задание 2 ----
# Разделение на обучающую и тестовую выборки в соотношении 20:1
train_size <- floor(20/21 * n)  # Размер обучающей выборки
test_size <- n - train_size  # Размер тестовой выборки

train_data <- x[1:train_size]
test_data <- x[(train_size + 1):n]

plot(train_data, type = "l", col = "green", main = "Обучающая выборка", xlab = "Время", ylab = "x_train")
plot(test_data, type = "l", col = "red", main = "Тестовая выборка", xlab = "Время", ylab = "x_test")

# Задание 3 ----
# Шаг 3a: Оценка параметров AR(2) модели с помощью arima()
ar_model <- arima(train_data, order = c(2, 0, 0))  # AR(2) модель
theta_hat <- coef(ar_model)[1:2]  # Оцененные параметры theta

cat("Оцененные параметры theta через arima():", theta_hat, "\n")

# Шаг 3b: Оценка параметров ARCH(3) модели для остатков AR(2)
residuals <- residuals(ar_model)
garch_model <- garch(residuals, order = c(0, 3))  # ARCH(3) модель
A_hat <- coef(garch_model)  # Оцененные параметры A

cat("Оцененные параметры A через garch():", A_hat, "\n")

sigma_hat <- fitted(garch_model)[,1]^2
plot(sigma_hat, type = "l", col = "orange", main = "Оцененная дисперсия ARCH(3)", xlab = "Время", ylab = "Sigma^2")

# Задание 4 ----
# Прогнозирование на один шаг вперед
x_forecast <- rep(NA, test_size)
sigma_forecast <- rep(NA, test_size)

for (i in 1:test_size) 
{
  idx <- train_size + i
  Xn <- c(x[idx-1], x[idx-2])
  x_forecast[i] <- sum(theta_hat * Xn)
  sigma_forecast[i] <- sqrt(A_hat[1] + A_hat[2] * x[idx-1]^2 + A_hat[3] * x[idx-2]^2 + A_hat[4] * x[idx-3]^2)
}

upper_bound <- x_forecast + sigma_forecast
lower_bound <- x_forecast - sigma_forecast

plot(test_data, type = "l", col = "turquoise", main = "Прогноз на 1 шаг вперед", 
     xlab = "Наблюдения", ylab = "Значение процесса", ylim = range(c(lower_bound, upper_bound, test_data)))
points(x_forecast, col = "black", pch = 1)
lines(upper_bound, col = "red", lty = 2)
lines(lower_bound, col = "red", lty = 2)

legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("turquoise", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))

# Задание 5-7 ----
data <- read.csv2("C:\\Users\\Николай\\Downloads\\ЛУКОЙЛ_1min_06112024_06112024.txt", header = TRUE, stringsAsFactors = FALSE)
data$X.DATE. <- as.Date(paste0(substr(data$X.DATE., 1, 4), "-", substr(data$X.DATE., 5, 6), "-01"), format = "%Y-%m-%d")
data$X.HIGH. <- as.numeric(data$X.HIGH.)

plot(data$X.HIGH., type="l", col="blue", main = "Динамика актива", xlab = "День", ylab = "Цена")

# Задание 8 ----
log_returns <- log(data$X.HIGH.[-1] / data$X.HIGH.[-nrow(data)])
cat("Логарифмическая доходность (первые 5 значений):", head(log_returns, 5), "\n")

# Задание 9 ----
plot(log_returns, type = "l", col = "green", main = "График логарифмической доходности", xlab = "Наблюдения", ylab = "Доходность z_k")

# Задание 10 ----
train_size_z <- floor(20/21 * length(log_returns))
test_size_z <- length(log_returns) - train_size_z

train_data_z <- log_returns[1:train_size_z]
test_data_z <- log_returns[(train_size_z + 1):length(log_returns)]

ar_model_z <- arima(train_data_z, order = c(2, 0, 0))
theta_hat_z <- coef(ar_model_z)[1:2]

cat("Оценка параметров theta для {z_n} через arima():", theta_hat_z, "\n")

residuals_z <- residuals(ar_model_z)
garch_model_z <- garch(residuals_z, order = c(0, 3))
A_hat_z <- coef(garch_model_z)

cat("Оценка параметров A для {z_n} через garch():", A_hat_z, "\n")

sigma_hat_z <- fitted(garch_model_z)[,1]^2
plot(sigma_hat_z, type = "l", col = "orange", main = "Оцененная дисперсия ARCH(3) для {z_n}", xlab = "Время", ylab = "Sigma^2")

x_forecast_z <- rep(NA, test_size_z)
sigma_forecast_z <- rep(NA, test_size_z)

for (i in 1:test_size_z) 
{
  idx_z <- train_size_z + i
  Xn_z <- c(log_returns[idx_z-1], log_returns[idx_z-2])
  x_forecast_z[i] <- sum(theta_hat_z * Xn_z)
  sigma_forecast_z[i] <- sqrt(A_hat_z[1] + A_hat_z[2] * log_returns[idx_z-1]^2 + A_hat_z[3] * log_returns[idx_z-2]^2 + A_hat_z[4] * log_returns[idx_z-3]^2)
}

upper_bound_z <- x_forecast_z + sigma_forecast_z
lower_bound_z <- x_forecast_z - sigma_forecast_z

plot(test_data_z, type = "l", col = "turquoise", main = "Прогноз на 1 шаг вперед для {z_n}", 
     xlab = "Наблюдения", ylab = "Доходность", ylim = range(c(lower_bound_z, upper_bound_z, test_data_z)))
points(x_forecast_z, col = "black", pch = 1)
lines(upper_bound_z, col = "red", lty = 2)
lines(lower_bound_z, col = "red", lty = 2)

legend("topleft", legend = c("Реальные значения", "Прогнозы", "Границы волатильности"),
       col = c("turquoise", "black", "red"), lty = c(1, NA, 2), pch = c(NA, 1, NA))
