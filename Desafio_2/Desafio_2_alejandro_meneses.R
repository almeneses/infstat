# -----------------------------------------------------------
# Situación 2: Simulación y Potencia de Pruebas de Hipótesis
# -----------------------------------------------------------

# 1. Configuración inicial
set.seed(42) # Para reproducibilidad de los resultados
n <- 40 # Tamaño de la muestra
alpha <- 0.05 # Nivel de significancia
B <- 5000 # Número de iteraciones de la simulación

# Rango de valores de mu a evaluar
mu_range <- seq(9.5, 10.5, by = 0.05)
n_mu <- length(mu_range)

# Matrices para almacenar los resultados de potencia
potencia_normal <- matrix(0, nrow = n_mu, ncol = 3, dimnames = list(NULL, c("Z-test", "t-test", "Wilcoxon")))
potencia_contaminado <- matrix(0, nrow = n_mu, ncol = 3, dimnames = list(NULL, c("Z-test", "t-test", "Wilcoxon")))

# -----------------------------------------------------------
# Escenario 1: Distribución Normal
# -----------------------------------------------------------

for (i in 1:n_mu) {
  mu <- mu_range[i]
  rejects_z <- 0
  rejects_t <- 0
  rejects_wilcox <- 0

  for (j in 1:B) {
    # Generar muestra normal
    muestra <- rnorm(n, mean = mu, sd = 0.5)

    # Z-test (asumiendo sigma conocida)
    z_statistic <- (mean(muestra) - 10) / (0.5 / sqrt(n))
    p_value_z <- 2 * pnorm(-abs(z_statistic))
    if (p_value_z < alpha) {
      rejects_z <- rejects_z + 1
    }

    # t-test (sigma desconocida)
    t_test_result <- t.test(muestra, mu = 10, alternative = "two.sided")
    if (t_test_result$p.value < alpha) {
      rejects_t <- rejects_t + 1
    }

    # Prueba de Wilcoxon
    wilcox_test_result <- wilcox.test(muestra, mu = 10, alternative = "two.sided")
    if (wilcox_test_result$p.value < alpha) {
      rejects_wilcox <- rejects_wilcox + 1
    }
  }

  potencia_normal[i, "Z-test"] <- rejects_z / B
  potencia_normal[i, "t-test"] <- rejects_t / B
  potencia_normal[i, "Wilcoxon"] <- rejects_wilcox / B
}

# -----------------------------------------------------------
# Escenario 2: Distribución Contaminada
# -----------------------------------------------------------

# Función para generar datos del escenario contaminado
generar_muestra_contaminada <- function(n, mu) {
  # 95% de los datos de una normal "limpia"
  n_limpios <- round(n * 0.95)
  limpios <- rnorm(n_limpios, mean = mu, sd = 0.5)

  # 5% de los datos de una normal con mayor varianza (outliers)
  n_contaminados <- n - n_limpios
  contaminados <- rnorm(n_contaminados, mean = mu, sd = 3 * 0.5)

  c(limpios, contaminados)
}

for (i in 1:n_mu) {
  mu <- mu_range[i]
  rejects_z <- 0
  rejects_t <- 0
  rejects_wilcox <- 0

  for (j in 1:B) {
    # Generar muestra contaminada
    muestra <- generar_muestra_contaminada(n, mu)

    # Z-test (asumiendo sigma conocida)
    z_statistic <- (mean(muestra) - 10) / (0.5 / sqrt(n))
    p_value_z <- 2 * pnorm(-abs(z_statistic))
    if (p_value_z < alpha) {
      rejects_z <- rejects_z + 1
    }

    # t-test (sigma desconocida)
    t_test_result <- t.test(muestra, mu = 10, alternative = "two.sided")
    if (t_test_result$p.value < alpha) {
      rejects_t <- rejects_t + 1
    }

    # Prueba de Wilcoxon
    wilcox_test_result <- wilcox.test(muestra, mu = 10, alternative = "two.sided")
    if (wilcox_test_result$p.value < alpha) {
      rejects_wilcox <- rejects_wilcox + 1
    }
  }

  potencia_contaminado[i, "Z-test"] <- rejects_z / B
  potencia_contaminado[i, "t-test"] <- rejects_t / B
  potencia_contaminado[i, "Wilcoxon"] <- rejects_wilcox / B
}


print(mean(potencia_normal[, "Z-test"]))
print(mean(potencia_normal[, "t-test"]))
print(mean(potencia_normal[, "Wilcoxon"]))


# -----------------------------------------------------------
# Análisis y Visualización
# -----------------------------------------------------------

# Gráfico para el Escenario Normal
matplot(mu_range, potencia_normal,
  type = "l", lty = 1, lwd = 2,
  main = "Función de Potencia - Escenario Normal",
  xlab = "Verdadera Media (µ)",
  ylab = "Potencia",
  col = c("blue", "red", "green"),
  ylim = c(0, 1)
)
abline(h = alpha, lty = 2, col = "gray")
legend("right", legend = colnames(potencia_normal), col = c("blue", "red", "green"), lty = 1, lwd = 2)

# Gráfico para el Escenario Contaminado
matplot(mu_range, potencia_contaminado,
  type = "l", lty = 1, lwd = 2,
  main = "Función de Potencia - Escenario Contaminado",
  xlab = "Verdadera Media (µ)",
  ylab = "Potencia",
  col = c("blue", "red", "green"),
  ylim = c(0, 1)
)
abline(h = alpha, lty = 2, col = "gray")
legend("right", legend = colnames(potencia_contaminado), col = c("blue", "red", "green"), lty = 1, lwd = 2)
