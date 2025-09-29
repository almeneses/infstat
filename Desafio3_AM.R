# Librerías necesarias
library(tidyr)
library(dplyr)
library(nnet)


situacion_2 <- function() {
  data <- data.frame(
    Lago = c("Hancock", "Hancock", "Ocklawaha", "Ocklawaha", "Trafford", "Trafford", "George", "George"),
    Tamano = c("<=2.3", ">2.3", "<=2.3", ">2.3", "<=2.3", ">2.3", "<=2.3", ">2.3"),
    Peces = c(23, 7, 5, 13, 5, 8, 16, 17),
    Invertebrados = c(4, 0, 11, 8, 11, 7, 19, 1),
    Reptiles = c(2, 1, 1, 6, 2, 6, 1, 0),
    Aves = c(2, 3, 0, 1, 1, 3, 2, 1),
    Otros = c(8, 5, 3, 0, 5, 5, 3, 3),
    stringsAsFactors = FALSE
  )

  # Pasar a formato "largo" (cada fila = una dieta por grupo)
  df_long <- data %>%
    pivot_longer(
      cols = c(Peces, Invertebrados, Reptiles, Aves, Otros),
      names_to = "food",
      values_to = "n"
    ) %>%
    uncount(n)

  # Reordenar factores (para fijar la referencia)
  df_long$food <- relevel(factor(df_long$food), ref = "Peces")
  df_long$Lago <- factor(df_long$Lago)
  df_long$Tamano <- factor(df_long$Tamano, levels = c("<=2.3", ">2.3"))

  mod_mult <- multinom(food ~ Lago + Tamano, data = df_long)

  summary(mod_mult)

  # p - values
  # z <- summary(mod_mult)$coefficients / summary(mod_mult)$standard.errors
  # pvals <- 2 * (1 - pnorm(abs(z)))
  # pvals

  # Odds Ratios
  print(exp(coef(mod_mult)))
}
