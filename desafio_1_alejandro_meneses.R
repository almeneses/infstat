# Para cada estimador, se calcula: media, sesgo, varianza y ECM
crear_estimadores <- function(nombre = "", datos_estimador, theta_real) {
  media <- mean(datos_estimador)
  varianza <- var(datos_estimador)
  sesgo <- media - theta_real
  ecm <- (sesgo^2) + varianza

  return(c(nombre, media, sesgo, varianza, ecm))
}

situacion_1 <- function(simulaciones = 1000, n_barcos = 20, k_exitos = 2, prob_especie = 0.25) {
  proporciones <- numeric(simulaciones)

  # Resultados de los 4 estimadores
  resultados <- data.frame(
    estimador1 = numeric(simulaciones), # Maxima Verosimilitud
    estimador2 = numeric(simulaciones), # Media + sd
    estimador3 = numeric(simulaciones), # Tmax - Tmin + 1
    estimador4 = numeric(simulaciones) # k_exitos/mediana(tota_intentos)
  )

  for (i in 1:simulaciones) {
    # Generar muestras de los barcos
    fracasos <- rnbinom(n_barcos, size = k_exitos, prob = prob_especie)
    muestra <- fracasos + k_exitos
    muestra_ordenada <- sort(muestra)
    proporciones[i] <- (k_exitos) / sum(fracasos)

    resultados$estimador1[i] <- (n_barcos * k_exitos) / sum(fracasos)
    resultados$estimador2[i] <- (k_exitos * (1 - prob_especie)) / prob_especie + sqrt((k_exitos * (1 - prob_especie)) / prob_especie^2)
    resultados$estimador3[i] <- (max(muestra_ordenada) - min(muestra_ordenada) + 1) / sum(fracasos)
    resultados$estimador4[i] <- k_exitos / median(fracasos + k_exitos)
  }

  analisis <- data.frame(
    Estimador = character(4),
    Media = numeric(4),
    Sesgo = numeric(4),
    Varianza = numeric(4),
    ECM = numeric(4)
  )

  # Crea tabla de rendimientos
  analisis[1, ] <- crear_estimadores("EMV", resultados$estimador1, prob_especie)
  analisis[2, ] <- crear_estimadores("Media + sd", resultados$estimador2, prob_especie)
  analisis[3, ] <- crear_estimadores("Rango", resultados$estimador3, prob_especie)
  analisis[4, ] <- crear_estimadores("Razon Mediana", resultados$estimador4, prob_especie)

  print(analisis)
}


situacion_2 <- function(simulaciones = 1000, n = 200, media = 5, sd = 2) {
  x1 <- numeric(simulaciones)
  x2 <- numeric(simulaciones)

  for (i in 1:simulaciones) {
    muestra <- rnorm(n = 2 * n, mean = media, sd = sd)
    x1[i] <- mean(muestra)
    x2[i] <- mean(muestra[1:n])
  }

  par(mfrow = c(1, 2)) # Esto divide la ventana gráfica en 1 fila y 2 columnas

  # Histograma para el primer estimador
  hist(x1,
    main = paste("Histograma de Estimador X1 ( n =", n, ")"),
    xlab = "Valor Estimado",
    col = "blue",
    freq = FALSE # Muestra la densidad para una mejor comparación
  ) # Mismo rango para ambos gráficos

  # Histograma para el segundo estimador
  hist(x2,
    main = paste("Histograma de Estimador X2 ( n =", n, ")"),
    xlab = "Valor Estimado",
    col = "red",
    freq = FALSE
  )
  print(c(x1, x2))
}

situacion_3 <- function(simulaciones = 1000, theta_real = 50, n_muestra = 5) {
  # Resultados de los 5 estimadores
  resultados <- data.frame(
    estimador1 = numeric(simulaciones),
    estimador2 = numeric(simulaciones),
    estimador3 = numeric(simulaciones),
    estimador4 = numeric(simulaciones),
    estimador5 = numeric(simulaciones)
  )

  for (i in 1:simulaciones) {
    muestra <- sample(1:theta_real, n_muestra, replace = FALSE)
    muestra_ordenada <- sort(muestra)

    # Estimador 1: 2*media - 1
    resultados$estimador1[i] <- 2 * mean(muestra) - 1

    # Estimador 2: max + min - 1
    resultados$estimador2[i] <- muestra[length(muestra)] + muestra[1] - 1

    # Estimador 3: max + d_barra -> d_barra = media de los saltos
    d_barra <- (muestra_ordenada[n_muestra] - muestra_ordenada[1]) / (n_muestra - 1)
    resultados$estimador3[i] <- max(muestra) + d_barra

    # Estimador 4: 2*mediana - 1
    resultados$estimador4[i] <- (2 * median(muestra)) - 1

    # Estimador 5: media + 3*desviación estándar
    resultados$estimador5[i] <- mean(muestra) + (3 * sd(muestra))
  }

  analisis <- data.frame(
    Estimador = character(5),
    Media = numeric(5),
    Sesgo = numeric(5),
    Varianza = numeric(5),
    ECM = numeric(5)
  )

  # Crea tabla de rendimientos
  analisis[1, ] <- crear_estimadores("Estimador 1", resultados$estimador1, theta_real)
  analisis[2, ] <- crear_estimadores("Estimador 2", resultados$estimador2, theta_real)
  analisis[3, ] <- crear_estimadores("Estimador 3", resultados$estimador3, theta_real)
  analisis[4, ] <- crear_estimadores("Estimador 4", resultados$estimador4, theta_real)
  analisis[5, ] <- crear_estimadores("Estimador 5", resultados$estimador5, theta_real)

  print(analisis)
  print(max(muestra_ordenada) - min(muestra_ordenada))
}
