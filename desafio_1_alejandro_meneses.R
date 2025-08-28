rendimiento <- function(muestra, prob_real) {
  print(mean(muestra))
  sesgo <- mean(muestra) - prob_real
  varianza <- var(muestra)
  ecm <- sesgo^2 + varianza

  return(list(sesgo = sesgo, varianza = varianza, ecm = ecm))
}


situacion_1 <- function(n_simulaciones = 1000, n_barcos = 5, k_exitos = 2, prob_especie = 0.25) {
  prob_estimadas <- numeric(n_simulaciones)
  for (i in 0:n_simulaciones) {
    muestra <- rnbinom(n_barcos, size = k_exitos, prob = prob_especie) + k_exitos
    prob_estimadas[i] <- (n_barcos * k_exitos) / sum(muestra)
  }
  rendimientos <- rendimiento(prob_estimadas, prob_real = prob_especie)
  print(rendimientos)
  # prob_especie_est <-
  # prob_especie_est
}
