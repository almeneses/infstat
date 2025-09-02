situacion_1 <- function(n_simulaciones = 1000, n_barcos = 20, k_exitos = 1, prob_especie = 0.25) {
  prob_estimadas <- numeric(n_simulaciones)
  
  for (i in 1:n_simulaciones) {
    # Generar muestras de los barcos
    muestra <- rnbinom(n_barcos, size = k_exitos, prob = prob_especie) + k_exitos
    prob_estimadas[i] <- (k_exitos) / sum(muestra)
  }
  
  # Rendimientos
  sesgo <- mean(prob_estimadas) - prob_especie
  varianza <- var(prob_estimadas)
  ecm <- sesgo^2 + varianza
  
  df <- data.frame(
    sesgo = sesgo,
    varianza = varianza,
    ecm = ecm
  )
  
  # Graficar Rendimientos
  #par(mfrow=c(1,3))
  #hist(df$sesgo, main="Sesgo", xlab="Sesgo", col="blue")
  #hist(df$varianza, main="Varianza", xlab="Varianza", col="green")
  #hist(df$ecm, main="ECM", xlab="ECM", col="red")
  #par(mfrow=c(1,1))
  
  return(df)
}


situacion_2 <- function(simulaciones = 1000, n = 200, media = 5, sd = 2){
  x1 <- numeric(simulaciones)
  x2 <- numeric(simulaciones)
  
  for(i in 1:simulaciones){
    muestra <- rnorm(n= 2*n, mean = media, sd = sd)
    x1[i] <- mean(muestra)
    x2[i] <- mean(muestra[1:n])
  }
  
  # 4. Visualizar los resultados con un histograma para evidenciar la diferencia en la varianza
  par(mfrow = c(1, 2)) # Esto divide la ventana gráfica en 1 fila y 2 columnas
  
  # Histograma para el primer estimador
  hist(x1,
       main = "Histograma de Estimador X1 (n=500)",
       xlab = "Valor Estimado",
       col = "blue",
       freq = FALSE # Muestra la densidad para una mejor comparación
      ) # Mismo rango para ambos gráficos
  
  # Histograma para el segundo estimador
  hist(x2,
       main = "Histograma de Estimador X2 (n=500)",
       xlab = "Valor Estimado",
       col = "red",
       freq = FALSE)
  return(c(x1, x2))
}

situacion_3 <- function(simulaciones = 1000, theta_real = 50, n_muestra = 5){

  # Resultados de los 5 estimadores
  resultados <- data.frame(
    theta1 = numeric(simulaciones),
    theta2 = numeric(simulaciones),
    theta3 = numeric(simulaciones),
    theta4 = numeric(simulaciones),
    theta5 = numeric(simulaciones)
  )
  
  for (i in 1:simulaciones) {
    # Genera números enteros de forma aleatoria
    muestra <- sort(sample(1:theta_real, n_muestra, replace = FALSE))
    
    # Calcular los estimadores
    # Estimador 1: 2*media - 1
    resultados$theta1[i] <- 2 * mean(muestra) - 1
    
    # Estimador 2: max + min - 1
    resultados$theta2[i] <- max(muestra) + min(muestra) - 1
    
    # Estimador 3: max + d_barra
    # d_barra = media de los "saltos"
    saltos <- diff(muestra)
    d_barra <- mean(saltos)
    resultados$theta3[i] <- max(muestra) + d_barra
    
    # Estimador 4: 2*mediana - 1
    resultados$theta4[i] <- 2 * median(muestra) - 1
    
    # Estimador 5: media + 3*desviación estándar
    resultados$theta5[i] <- mean(muestra) + 3 * sd(muestra)
  }
  
  # 4. Analizar los resultados (cálculo de Sesgo, Varianza y ECM)
  # Para cada estimador, calculamos la media, varianza y ECM
  analisis <- data.frame(
    Estimador = character(5),
    Media = numeric(5),
    Sesgo = numeric(5),
    Varianza = numeric(5),
    ECM = numeric(5)
  )
  
  # Rellenar la tabla de análisis
  analisis[1,] <- c("theta1", mean(resultados$theta1), mean(resultados$theta1) - theta_real, var(resultados$theta1), var(resultados$theta1) + (mean(resultados$theta1) - theta_real)^2)
  analisis[2,] <- c("theta2", mean(resultados$theta2), mean(resultados$theta2) - theta_real, var(resultados$theta2), var(resultados$theta2) + (mean(resultados$theta2) - theta_real)^2)
  analisis[3,] <- c("theta3", mean(resultados$theta3), mean(resultados$theta3) - theta_real, var(resultados$theta3), var(resultados$theta3) + (mean(resultados$theta3) - theta_real)^2)
  analisis[4,] <- c("theta4", mean(resultados$theta4), mean(resultados$theta4) - theta_real, var(resultados$theta4), var(resultados$theta4) + (mean(resultados$theta4) - theta_real)^2)
  analisis[5,] <- c("theta5", mean(resultados$theta5), mean(resultados$theta5) - theta_real, var(resultados$theta5), var(resultados$theta5) + (mean(resultados$theta5) - theta_real)^2)
  
  # Mostrar la tabla de resultados
  print(analisis)
  
}

