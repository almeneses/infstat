# Librerías necesarias
library(tidyr)
library(nnet)
library(tidyverse)
library(lubridate)
library(speedglm)
library(arrow)

set.seed(123)

# situacion_3_modelo_1, situacion_3_modelo_2 y situacion_3_modelo_3
# son funciones AUXILIARES de situacion_3
situacion_3_modelo_1 <- function(df_taxis) {
  mod_glm <- glm(
    recibio_propina ~ trip_distance + passenger_count + payment_type +
      factor(pickup_hour) + RatecodeID + store_and_fwd_flag,
    family = binomial(link = "logit"),
    data = df_taxis
  )

  print("===========================================")
  print("  Summary Modelo 1")
  print(summary(mod_glm))

  print("===========================================")
  print("  Coeficientes Modelo 1")
  print(exp(coef(mod_glm)))

  muestra_10pct <- df_taxis %>% slice_sample(prop = 0.10)
  muestra_50pct <- df_taxis %>% slice_sample(prop = 0.50)

  print(nrow(df_taxis))
  print(nrow(muestra_10pct))

  # Ajustar mod_glm en submuestras y comparar coeficientes (ejemplo)
  mod_muestra_10pct <- glm(
    recibio_propina ~ trip_distance + passenger_count + payment_type +
      factor(pickup_hour) + RatecodeID + store_and_fwd_flag,
    family = binomial(link = "logit"), data = muestra_10pct
  )
  print("===========================================")
  print("  Summary Modelo 1 (con muestra de 10%)")
  print(summary(mod_muestra_10pct))

  mod_muestra_50pct <- glm(
    recibio_propina ~ trip_distance + passenger_count + payment_type +
      factor(pickup_hour) + RatecodeID + store_and_fwd_flag,
    family = binomial(link = "logit"), data = muestra_10pct
  )
  print("===========================================")
  print("  Summary Modelo 1 (con muestra de 50%)")
  print(summary(mod_muestra_50pct))

  # Alternativa: usar speedglm para datasets grandes (mismo modelo)
  mod_speedglm <- speedglm(
    recibio_propina ~ trip_distance + passenger_count + payment_type +
      factor(pickup_hour) + RatecodeID + store_and_fwd_flag,
    family = binomial(link = "logit"), data = df_taxis
  )

  print("===========================================")
  print("  Summary Modelo 1 (con speedglm)")
  print(summary(mod_speedglm))


  print("===========================================")
  print("  Comparación (10% vs 50% vs speedglm) utilizando el criterio Akaike (AIC)")
  print(AIC(mod_muestra_10pct, mod_muestra_50pct, mod_speedglm))
}

situacion_3_modelo_2 <- function(df_taxis) {
  print("===========================================")
  print("  Modelo 2: Número de Pasajeros")

  # Obtener una muestra aleatoria reducida (20% de la cantidad total)
  df_taxis_reducido <- df_taxis %>% slice_sample(prop = 0.20)

  print("  Calculando Modelo...")
  mod2_poisson <- speedglm(passenger_count ~ trip_distance + factor(pickup_hour) + PULocationID + DOLocationID,
    family = poisson(link = "log"),
    data = df_taxis_reducido
  )
  print("===========================================")
  print(" Summary Modelo 2")
  print(summary(mod2_poisson))

  print("===========================================")
  print(" Equidispersion Modelo 2")
  # Comprobar equidispersión (ratio var/mean)
  mean_pc <- mean(df_taxis$passenger_count, na.rm = TRUE)
  var_pc <- var(df_taxis$passenger_count, na.rm = TRUE)
  dispersion_ratio <- var_pc / mean_pc
  cat("Media:", mean_pc, "Var:", var_pc, "Dispersion:", dispersion_ratio, "\n")

  print("===========================================")
  print(" Coeficientes Modelo 2")
  print(exp(coef(mod2_poisson)))
}

situacion_3_modelo_3 <- function(df_taxis) {
  df_tips <- na.omit(df_taxis %>% filter(tip_amount > 0))
  df_tips <- df_tips %>% slice_sample(prop = 0.20)

  # GLM Gamma
  print("===========================================")
  print(" Calcular Modelo 3")
  mod3_gamma <- speedglm(tip_amount ~ trip_distance + passenger_count + payment_type + factor(pickup_hour),
    family = Gamma(link = "log"),
    data = df_tips,
    maxit = 150
  )
  print("===========================================")
  print(" Summary Modelo 3")
  print(summary(mod3_gamma))

  print("===========================================")
  print(" Coeficientes Modelo 3")
  print(exp(coef(mod3_gamma)))
  print(exp(confint(mod3_gamma)))

  # Alternativa: probar link inverse
  print("===========================================")
  print(" Modelo 3 - Alternativa (inverse) - (Saldrá error, No converge)")
  mod3_gamma_inv <- speedglm(tip_amount ~ trip_distance + passenger_count + payment_type + factor(pickup_hour),
    family = Gamma(link = "inverse"),
    data = df_tips,
    maxit = 150
  )
  mod3_gamma_inv
}

situacion_2 <- function() {
  # 8 combinaciones (4 lagos × 2 tamaños), cada una con 5 tipos de alimento = 40 filas
  lago <- rep(c(
    "Hancock", "Hancock",
    "Ocklawaha", "Ocklawaha",
    "Trafford", "Trafford",
    "George", "George"
  ), each = 5)

  tamano <- rep(c("<=2.3", ">2.3"), times = 4, each = 5)
  alimento <- rep(c("Peces", "Invertebrados", "Reptiles", "Aves", "Otros"), times = 8)
  frecuencia <- c(
    23, 4, 2, 2, 8, # Hancock <=2.3
    7, 0, 1, 3, 5, # Hancock >2.3
    5, 11, 1, 0, 3, # Ocklawaha <=2.3
    13, 8, 6, 1, 0, # Ocklawaha >2.3
    5, 11, 2, 1, 5, # Trafford <=2.3
    8, 7, 6, 3, 5, # Trafford >2.3
    16, 19, 1, 2, 3, # George <=2.3
    17, 1, 0, 1, 3 # George >2.3
  )

  datos <- data.frame(lago, tamano, alimento, frecuencia)
  df <- datos[rep(1:nrow(datos), datos$frecuencia), 1:3]

  # Ajustar modelo multinomial (referencia: Peces)
  df$alimento <- relevel(factor(df$alimento), ref = "Peces")
  modelo <- multinom(alimento ~ lago + tamano, data = df)

  # Calcular Odds Ratios e IC95
  odds_ratios <- exp(coef(modelo))
  se <- summary(modelo)$standard.errors
  z <- 1.96

  print("===========================================")
  print("  Resultados")
  print(summary(modelo))


  z <- summary(modelo)$coefficients / summary(modelo)$standard.errors
  p_vals <- 2 * (1 - pnorm(abs(z)))


  # Tabla final
  tabla <- data.frame(
    Categoria = rep(rownames(odds_ratios), each = ncol(odds_ratios)),
    Variable = rep(colnames(odds_ratios), times = nrow(odds_ratios)),
    Odds_Ratios = round(c(t(odds_ratios)), 4),
    SE = round(c(t(se)), 4),
    P = round(c(t(p_vals)), 4)
  )
  print("===========================================")
  print("  Resultados Ordenados")
  print(tabla)
}

situacion_3 <- function(modelo = "modelo1") {
  df_taxis <- read_parquet("taxis_enero_2023.parquet")

  # Limpieza y variables derivadas
  # Convertir datetimes
  df_taxis$tpep_pickup_datetime <- ymd_hms(df_taxis$tpep_pickup_datetime)
  df_taxis$tpep_dropoff_datetime <- ymd_hms(df_taxis$tpep_dropoff_datetime)

  # Crea la variable binaria recibio_propina
  df_taxis$recibio_propina <- ifelse(df_taxis$tip_amount > 0, 1, 0)

  # Hora del día (0-23) y periodos
  df_taxis$pickup_hour <- hour(df_taxis$tpep_pickup_datetime)
  df_taxis$periodo <- cut(df_taxis$pickup_hour,
    breaks = c(-1, 5, 9, 16, 20, 23),
    labels = c("madrugada", "manana", "tarde", "noche", "noche2"),
    right = TRUE
  )

  # Asegura los tipos adecuados
  df_taxis$VendorID <- factor(df_taxis$VendorID)
  df_taxis$RatecodeID <- factor(df_taxis$RatecodeID)
  df_taxis$store_and_fwd_flag <- factor(df_taxis$store_and_fwd_flag)
  df_taxis$PULocationID <- factor(df_taxis$PULocationID)
  df_taxis$DOLocationID <- factor(df_taxis$DOLocationID)
  df_taxis$payment_type <- factor(df_taxis$payment_type)
  df_taxis$passenger_count <- as.integer(df_taxis$passenger_count)

  if (modelo == "modelo1") {
    situacion_3_modelo_1(df_taxis)
  }

  if (modelo == "modelo2") {
    situacion_3_modelo_2(df_taxis)
  }

  if (modelo == "modelo3") {
    situacion_3_modelo_3(df_taxis)
  }
}
