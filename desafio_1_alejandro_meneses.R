situacion_1 <- function(n_simulaciones = 1000, n_barcos = 20, k_exitos = 1, prob_especie = 0.25) {
  prob_estimadas <- numeric(n_simulaciones)
  
  for (i in 1:n_simulaciones) {
    # Generar muestras de los barcos
    muestra <- rnbinom(n_barcos, size = k_exitos, prob = prob_especie) + k_exitos
    prob_estimadas[i] <- (k_exitos) / sum(muestra)
  }
  
  # Rendimientos
  sesgo <- mean(prob_estimadas) - prob_especie
  varianza <- ((prob_estimadas - mean(prob_estimadas))^2) / (length(prob_estimadas) - 1)
  ecm <- sesgo^2 + varianza
  
  df <- data.frame(
    sesgo = sesgo,
    varianza = varianza,
    ecm = ecm
  )
  
  # Graficar Rendimientos
  par(mfrow=c(1,3))
  hist(df$sesgo, main="Sesgo", xlab="Sesgo", col="blue")
  hist(df$varianza, main="Varianza", xlab="Varianza", col="green")
  hist(df$ecm, main="ECM", xlab="ECM", col="red")
  par(mfrow=c(1,1))
  
  return(df)
}

