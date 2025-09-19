situacion_2 <- function(n = 40, B = 5000, alpha = 0.05) {
  mu_rango <- seq(9.5, 10.5, by = 0.05)
  n_mu <- length(mu_rango)

  potencia_normal <- matrix(0, nrow = n_mu, ncol = 3, dimnames = list(NULL, c("Z-test", "t-test", "Wilcoxon")))
  potencia_contaminado <- matrix(0, nrow = n_mu, ncol = 3, dimnames = list(NULL, c("Z-test", "t-test", "Wilcoxon")))

  # Escenario 1: Distribución Normal

  for (i in 1:n_mu) {
    mu <- mu_rango[i]
    rejects_z <- 0
    rejects_t <- 0
    rejects_wilcox <- 0

    for (j in 1:B) {
      muestra <- rnorm(n, mean = mu, sd = 0.5)

      # Z-test
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

  # Escenario 2: Distribución Contaminada

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
    mu <- mu_rango[i]
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

  # Gráfico para el Escenario Normal
  matplot(mu_rango, potencia_normal,
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
  matplot(mu_rango, potencia_contaminado,
    type = "l", lty = 1, lwd = 2,
    main = "Función de Potencia - Escenario Contaminado",
    xlab = "Verdadera Media (µ)",
    ylab = "Potencia",
    col = c("blue", "red", "green"),
    ylim = c(0, 1)
  )
  abline(h = alpha, lty = 2, col = "gray")
  legend("right", legend = colnames(potencia_contaminado), col = c("blue", "red", "green"), lty = 1, lwd = 2)
}

situacion_5 <- function() {
  n_A <- 1500
  n_B <- 1400

  # 1: Prueba de Shapiro-Wilk y valor P
  sim_A <- rgamma(n_A, shape = 3, scale = 10)
  sim_B <- rlnorm(n_B, meanlog = 2.5, sdlog = 0.5)

  shapiro_A <- shapiro.test(sim_A)
  shapiro_B <- shapiro.test(sim_B)

  print("Resultado de prueba Shapiro-Wilk para Algoritmo A:")
  print(shapiro_A)
  print("Resultado de prueba Shapiro-Wilk para Algoritmo B:")
  print(shapiro_B)

  # 2.1: Diferencia entre percentiles
  p95_A <- quantile(sim_A, 0.95)
  p95_B <- quantile(sim_B, 0.95)
  delta_obs <- p95_A - p95_B

  print("Percentiles 95 y Diferencia:")
  cat("P95(A) =", p95_A, "\n")
  cat("P95(B) =", p95_B, "\n")
  cat("Delta =", delta_obs, "\n\n")

  # 2.2: Bootstrap No Paramétrico
  B <- 10000
  deltas_bootstrap <- numeric(B)

  for (i in 1:B) {
    muestra_A_prima <- sample(sim_A, size = n_A, replace = TRUE)
    muestra_B_prima <- sample(sim_B, size = n_B, replace = TRUE)
    p95A_prima <- quantile(muestra_A_prima, 0.95)
    p95B_prima <- quantile(muestra_B_prima, 0.95)
    deltas_bootstrap[i] <- p95A_prima - p95B_prima
  }

  # 2.3: Intervalo de Confianza por método de percentiles
  ic <- quantile(deltas_bootstrap, c(0.025, 0.975))

  print("IC por método Bootstrap:")
  cat(" IC 95% (percentil): (", ic[1], ",", ic[2], ")\n\n")

  # 3: Prueba de Hipótesis basada en Bootstrap
  p_valor <- sum(deltas_bootstrap <= 0) / B

  print("Valor P de la prueba de hipótesis basada en Bootstrap:")
  print(p_valor)
}
