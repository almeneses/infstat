s1_punto_b <- function(n_simulaciones = 1000, n_casos = 100, prob_ana = 0.03, prob_luis = 0.05) {
  # Simular X e Y
  X <- rbinom(n_simulaciones, size = n_casos, prob = prob_ana)
  Y <- rbinom(n_simulaciones, size = n_casos, prob = prob_luis)

  print("Simulaciones de X (ana):")
  print(X)
  print("Simulaciones de Y (Luis):")
  print(Y)

  # Probabilidades
  probs_X <- dbinom(0:n_simulaciones, size = n_casos, prob = prob_ana)
  probs_Y <- dbinom(0:n_simulaciones, size = n_casos, prob = prob_luis)

  total <- rep(0, n_casos + 1) # índices 0..100 -> posiciones 1..101

  # Suma de todas las combinaciones
  for (i in 0:n_casos) {
    for (j in 0:n_casos) {
      total[i + j + 1] <- total[i + j + 1] + (probs_X[i + 1] * probs_Y[j + 1]) # Propiedad de independencia de variables aleatorias
    }
  }

  # Probabilidad total P(total < 10) -> sum total[1]..total[10] (0..9)
  prob_total_menor_10 <- sum(total[1:10])

  print("Probabilidad de que el Total Manzanas dañadas sea menor que 10:")
  print(p_total_menor_10)
}

s1_punto_c <- function(n_simulaciones = 1000, n_casos = 100, prob_ana = 0.03, prob_luis = 0.05) {
  probs_X <- dbinom(0:n_simulaciones, size = n_casos, prob = prob_ana)
  probs_Y <- dbinom(0:n_simulaciones, size = n_casos, prob = prob_luis)

  # calcular P(X>Y)
  prob_total <- 0
  for (i in 0:n_casos) {
    if (i >= 1) {
      prob_total <- prob_total + probs_X[i + 1] * sum(probs_Y[1:i]) # probs_Y[1:i] corresponde a y=0..i-1
    }
  }

  print("Probabilidad de que Ana tenga más Manzanas dañadas que Luis:")
  prob_total
}

s1_punto_d <- function(n_simulaciones = 1000, n_casos = 100, prob_ana = 0.03, prob_luis = 0.05) {
  X <- rbinom(n_simulaciones, size = n_casos, prob = prob_ana)
  Y <- rbinom(n_simulaciones, size = n_casos, prob = prob_luis)
  Z <- X + Y

  hist(Z, main = "Distribución del total de manzanas dañadas")
  # qqnorm(Z, main="Distribución del total de manzana dañadas")
}

s1_punto_e <- function(n_simulaciones = 1000, n_casos = 100, prob_ana = 0.03, prob_luis = 0.05, n_rechazo = 8) {
  probs_X <- dbinom(0:n_simulaciones, size = n_casos, prob = prob_ana)
  probs_Y <- dbinom(0:n_simulaciones, size = n_casos, prob = prob_luis)
  casos_totales <- (n_casos * 2) + 1

  total <- rep(0, length = casos_totales) # índices 1..201 => t = 0..200
  for (i in 0:n_casos) {
    for (j in 0:n_casos) {
      total[i + j + 1] <- total[i + j + 1] + (probs_X[i + 1] * probs_Y[j + 1])
    }
  }

  # Probabilidad de rechazo: P(T > 8)
  prob_rechazo <- 1 - sum(total[0:n_rechazo + 1]) # índice 9 corresponde a t = 8

  cat("Probabilidad de que los lotes combinados sean rechazados (Manzanas dañadas >", n_rechazo, ")\n")
  prob_rechazo
}
